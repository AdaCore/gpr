------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;
with GNAT.String_Split;

with GNATCOLL.OS.Constants;

with GPR2.Unit.List;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
pragma Warnings (Off, "* is not referenced");
--  GPR2.Project.Source.Dependencies return a Part_Set but only has limited
--  visibility on it. So in order to be able to manipulate the returned object
--  we need to have full vilibility so need to add this with clause.
--  However we never reference the package explicitly, so the compiler will
--  complain that the using is not referenced.
--  So let's just kill the warning.
with GPR2.Project.Source.Part_Set;
pragma Warnings (On, "* is not referenced");
with GPR2.Project.Typ;
with GPR2.Project.Variable;
with GPR2.Project.View.Set;
with GPR2.Version;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;

with GPRtools;

package body GPRinstall.Install is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GNAT;

   use GPR2;
   use all type Unit.Library_Unit_Type;

   use type GNATCOLL.OS.OS_Type;

   package String_Vector renames GPR2.Containers.Value_Type_List;

   subtype Message_Digest is GNAT.MD5.Message_Digest;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

   Content : String_Vector.Vector;
   --  The content of the project, this is used when creating the project
   --  and is needed to ease the project section merging when installing
   --  multiple builds.

   Initial_Buffer_Size : constant := 100;
   --  Arbitrary value for the initial size of the buffer below

   Buffer : GNAT.OS_Lib.String_Access := new String (1 .. Initial_Buffer_Size);
   Buffer_Last : Natural := 0;

   Agg_Manifest : Text_IO.File_Type;
   --  Manifest file for main aggregate project

   Line_Manifest     : Text_IO.Count := 0;
   Line_Agg_Manifest : Text_IO.Count := 0;
   --  Keep lines when opening the manifest files. This is used by the rollback
   --  routine when an error occurs while copying the files.

   function Other_Part_Need_Body
     (Source : GPR2.Project.Source.Object;
      Index  : Unit_Index) return Boolean
   is
     (Source.Has_Other_Part (Index)
      and then Source.Other_Part
        (Index).Source.Is_Implementation_Required
          (Source.Other_Part (Index).Index));
   --  Returns True if Source has other part and this part need body

   procedure Double_Buffer;
   --  Double the size of the Buffer

   procedure Write_Eol;
   --  Append the content of the Buffer as a line to Content and empty the
   --  Buffer.

   procedure Write_Str (S : String);
   --  Append S to the buffer. Double the buffer if needed

   procedure Process
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object;
      Options : GPRinstall.Options.Object);
   --  Install the give project view

   Installed : GPR2.Project.View.Set.Object;
   --  Record already installed project

   -------------------
   -- Double_Buffer --
   -------------------

   procedure Double_Buffer is
      New_Buffer : constant GNAT.OS_Lib.String_Access :=
                     new String (1 .. Buffer'Last * 2);
   begin
      New_Buffer (1 .. Buffer_Last) := Buffer (1 .. Buffer_Last);
      OS_Lib.Free (Buffer);
      Buffer := New_Buffer;
   end Double_Buffer;

   -------------
   -- Process --
   -------------

   procedure Process
     (Tree    : GPR2.Project.Tree.Object;
      Project : GPR2.Project.View.Object;
      Options : GPRinstall.Options.Object)
   is
      use GPRtools;

      use type OS_Lib.String_Access;
      use type GPR2.Path_Name.Object;
      use type GPR2.Project.View.Object;

      package A renames GPR2.Project.Registry.Attribute;
      package P renames GPR2.Project.Registry.Pack;

      subtype Param is GPRinstall.Options.Param;

      Target_Name  : constant String := To_String (Options.Target);

      Objcopy_Exec : constant String :=
                       (if Target_Name = "all"
                        then "objcopy"
                        else Target_Name & "-objcopy");
      --  Name of objcopy executable, possible a cross one

      Strip_Exec   : constant String :=
                       (if Target_Name = "all"
                        then "strip"
                        else Target_Name & "-strip");
      --  Name of strip executable, possible a cross one

      Objcopy      : constant OS_Lib.String_Access :=
                       OS_Lib.Locate_Exec_On_Path (Objcopy_Exec);
      Strip        : constant OS_Lib.String_Access :=
                       OS_Lib.Locate_Exec_On_Path (Strip_Exec);

      Windows_Target : constant Boolean := Tree.Is_Windows_Target;

      --  Local values for the given project, these are initially set with the
      --  default values. It is updated using the Install package found in the
      --  project if any.

      Active          : Boolean := True;
      --  Whether installation is active or not (Install package's attribute)

      Side_Debug      : Boolean := Options.Side_Debug;
      --  Whether to extract debug symbols from executables and shared
      --  libraries. Default to global value.

      Prefix_Dir      : Param   := Options.Global_Prefix_Dir;
      Exec_Subdir     : Param   := Options.Global_Exec_Subdir;
      Lib_Subdir      : Param   := Options.Global_Lib_Subdir;
      ALI_Subdir      : Param   := Options.Global_ALI_Subdir;
      Link_Lib_Subdir : Param   := Options.Global_Link_Lib_Subdir;
      Sources_Subdir  : Param   := Options.Global_Sources_Subdir;
      Project_Subdir  : Param   := Options.Global_Project_Subdir;
      Install_Mode    : Param   := Options.Global_Install_Mode;
      Install_Name    : Param   := Options.Global_Install_Name;
      Install_Project : Boolean := not Options.No_GPR_Install;

      type Items is (Source, Object, Dependency, Library, Executable);

      Copy : array (Items) of Boolean := (others => False);
      --  What should be copied from a project, this depends on the actual
      --  project kind and the mode (usage, dev) set for the install.

      Man : Text_IO.File_Type;
      --  File where manifest for this project is kept

      --  Keeping track of artifacts to install

      type Artifacts_Data is record
         Destination, Filename : Unbounded_String;
         Required              : Boolean;
      end record;

      package Artifacts_Set is
        new Ada.Containers.Vectors (Positive, Artifacts_Data);

      Artifacts : Artifacts_Set.Vector;

      Excluded_Naming : GPR2.Containers.Name_Set;
      --  This set contains names of Ada unit to exclude from the generated
      --  package Naming. This is needed to avoid renaming for bodies which
      --  are not installed when the minimum installation (-m) is used. In
      --  this case there is two points to do:
      --
      --  1. the installed .ali must use the spec naming
      --
      --  2. the naming convention for the body must be excluded from the
      --     generated project.

      procedure Copy_File
        (From, To      : Path_Name.Object;
         File          : Filename_Optional := No_Filename;
         From_Ver      : Path_Name.Object  := Path_Name.Undefined;
         Sym_Link      : Boolean := False;
         Executable    : Boolean := False;
         Extract_Debug : Boolean := False)
        with Pre =>
          (if From.Is_Directory
           then not To.Is_Directory or else File /= No_Filename
           else To.Is_Directory or else File = No_Filename);
      --  Copy file From into To. If From and To are directories the full path
      --  name is using the File which must not be empty in this case.
      --  If Sym_Link is set a symbolic link is created.
      --  If Executable is set, the destination file exec attribute is set.
      --  When Extract_Debug is set to True the debug information for the
      --  executable is written in a side file.

      function Dir_Name (Suffix : Boolean := True) return Filename_Type;
      --  Returns the name of directory where project files are to be
      --  installed. This name is the name of the project. If Suffix is
      --  True then the build name is also returned.

      function Sources_Dir
        (Build_Name : Boolean := True) return Path_Name.Object;
      --  Returns the full pathname to the sources destination directory

      function Prefix_For_Dir (Name : String) return Path_Name.Object is
        (Path_Name.Create_Directory
           (Filename_Type (Name),
            (if OS_Lib.Is_Absolute_Path (Name)
             then No_Filename
             else Filename_Optional (-Prefix_Dir.V))));
      --  Returns directory as Path_Name.Object prefixed with Prefix_Dir.V.all
      --  if not absote.

      function Exec_Dir return Path_Name.Object;
      --  Returns the full pathname to the executable destination directory

      function Lib_Dir (Build_Name : Boolean := True) return Path_Name.Object;
      --  Returns the full pathname to the library destination directory

      function ALI_Dir (Build_Name : Boolean := True) return Path_Name.Object;
      --  Returns the full pathname to the library destination directory

      function Link_Lib_Dir return Path_Name.Object;
      --  Returns the full pathname to the lib symlib directory

      function Project_Dir return Path_Name.Object;
      --  Returns the full pathname to the project destination directory

      procedure Check_Install_Package;
      --  Check Project's install package and overwrite the default values of
      --  the corresponding variables above.

      procedure Copy_Files;
      --  Do the file copies for the project's sources, objects, library,
      --  executables.

      procedure Create_Project (Project : GPR2.Project.View.Object);
      --  Create install project for the given project

      procedure Add_To_Manifest
        (Pathname       : Path_Name.Object;
         Aggregate_Only : Boolean := False)
        with Pre => Options.Install_Manifest;
      --  Add filename to manifest

      function Has_Sources (Project : GPR2.Project.View.Object) return Boolean
        with Inline;
      --  Returns True if the project contains sources

      function Is_Install_Active
        (Project : GPR2.Project.View.Object) return Boolean;
      --  Returns True if the Project is active, that is there is no attribute
      --  Active set to False in the Install package.

      procedure Open_Check_Manifest
        (File         : out Text_IO.File_Type;
         Current_Line : out Text_IO.Count);
      --  Check that manifest file can be used

      procedure Rollback_Manifests;
      --  Rollback manifest files (for current project or/and aggregate one)

      function For_Dev return Boolean is (-Install_Mode.V = "dev");

      function Build_Subdir
        (Subdir     : Param;
         Build_Name : Boolean := True) return Path_Name.Object;
      --  Return a path-name for a subdir

      ---------------------
      -- Add_To_Manifest --
      ---------------------

      procedure Add_To_Manifest
        (Pathname       : Path_Name.Object;
         Aggregate_Only : Boolean := False) is
      begin
         if not Aggregate_Only and then not Is_Open (Man) then
            Open_Check_Manifest (Man, Line_Manifest);
         end if;

         --  Append entry into manifest

         declare
            function N (Str : String) return String
              is (OS_Lib.Normalize_Pathname (Str, Case_Sensitive => False));

            MD5  : constant String := String (Pathname.Content_MD5);
            File : constant String := String (Pathname.Simple_Name);
         begin
            if not Aggregate_Only and then Is_Open (Man) then
               declare
                  Man_Path : constant Path_Name.Object :=
                               Path_Name.Create_File
                                 (Filename_Type (N (Name (Man))),
                                  Path_Name.No_Resolution);
               begin
                  Put_Line
                    (Man,
                     MD5 & ' '
                     & String (Pathname.Relative_Path (From => Man_Path).Name)
                     & File);
               end;
            end if;

            if Is_Open (Agg_Manifest) then
               declare
                  Agg_Man_Path : constant Path_Name.Object :=
                                   Path_Name.Create_File
                                     (Filename_Type (N (Name (Agg_Manifest))),
                                      Path_Name.No_Resolution);
               begin
                  Put_Line
                    (Agg_Manifest,
                     MD5 & ' '
                     & String
                         (Pathname.Relative_Path (From => Agg_Man_Path).Name)
                     & File);
               end;
            end if;
         end;
      end Add_To_Manifest;

      -------------
      -- ALI_Dir --
      -------------

      function ALI_Dir
        (Build_Name : Boolean := True) return Path_Name.Object is
      begin
         return Build_Subdir (ALI_Subdir, Build_Name);
      end ALI_Dir;

      ------------------
      -- Build_Subdir --
      ------------------

      function Build_Subdir
        (Subdir     : Param;
         Build_Name : Boolean := True) return Path_Name.Object
      is
         Install_Name_Dir : constant Filename_Type :=
                              (if Install_Name.Default
                               then "."
                               else Filename_Type (-Install_Name.V));
      begin
         if OS_Lib.Is_Absolute_Path (-Subdir.V) then
            return Path_Name.Create_Directory
              (Install_Name_Dir, Filename_Optional (-Subdir.V));

         elsif not Subdir.Default or else not Build_Name then
            return Path_Name.Create_Directory
              (Install_Name_Dir,
               Filename_Type
                 (Path_Name.Create_Directory
                      (Filename_Type (-Subdir.V),
                       Filename_Optional (-Prefix_Dir.V)).Value));

         else
            return Path_Name.Create_Directory
              (Dir_Name,
               Filename_Type
                 (Path_Name.Create_Directory
                      (Install_Name_Dir,
                       Filename_Type
                         (Path_Name.Create_Directory
                            (Filename_Type (-Subdir.V),
                             Filename_Optional (-Prefix_Dir.V)).Value))
                  .Value));
         end if;
      end Build_Subdir;

      ---------------------------
      -- Check_Install_Package --
      ---------------------------

      procedure Check_Install_Package is

         procedure Replace
           (P         : in out Param;
            Val       : String;
            Is_Dir    : Boolean := True;
            Normalize : Boolean := False) with Inline;
         --  Set Var with Value, free previous pointer

         -------------
         -- Replace --
         -------------

         procedure Replace
           (P         : in out Param;
            Val       : String;
            Is_Dir    : Boolean := True;
            Normalize : Boolean := False) is
         begin
            if Val /= "" then
               P := (To_Unbounded_String
                       ((if Is_Dir
                        then (if Normalize
                              then OS_Lib.Normalize_Pathname (Val)
                              else Val)
                         else Val)),
                     Default => False);
            end if;
         end Replace;

      begin
         if Project.Has_Package (P.Install) then
            declare
               use Ada.Characters.Handling;
            begin
               for V of Project.Attributes (Pack => P.Install) loop
                  if V.Name.Id = A.Install.Prefix then
                     --  If Install.Prefix is a relative path, it is made
                     --  relative to the global prefix.

                     if OS_Lib.Is_Absolute_Path (V.Value.Text) then
                        if Options.Global_Prefix_Dir.Default then
                           Replace
                             (Prefix_Dir, V.Value.Text, Normalize => True);
                        end if;

                     else
                        Replace
                          (Prefix_Dir,
                           -Options.Global_Prefix_Dir.V & "/" &
                             V.Value.Text,
                           Normalize => True);
                     end if;

                  elsif  V.Name.Id = A.Install.Exec_Subdir
                    and then Options.Global_Exec_Subdir.Default
                  then
                     Replace (Exec_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.Lib_Subdir
                    and then Options.Global_Lib_Subdir.Default
                  then
                     Replace (Lib_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.ALI_Subdir
                    and then Options.Global_ALI_Subdir.Default
                  then
                     Replace (ALI_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.Link_Lib_Subdir
                    and then Options.Global_Link_Lib_Subdir.Default
                  then
                     Replace (Link_Lib_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.Sources_Subdir
                    and then Options.Global_Sources_Subdir.Default
                  then
                     Replace (Sources_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.Project_Subdir
                    and then Options.Global_Project_Subdir.Default
                  then
                     Replace (Project_Subdir, V.Value.Text);

                  elsif V.Name.Id = A.Install.Mode
                    and then Options.Global_Install_Mode.Default
                  then
                     Replace (Install_Mode, V.Value.Text);

                  elsif V.Name.Id = A.Install.Install_Name
                    and then Options.Global_Install_Name.Default
                  then
                     Replace
                       (Install_Name, V.Value.Text, Is_Dir => False);

                  elsif V.Name.Id = A.Install.Active then
                     declare
                        Val : constant String := To_Lower (V.Value.Text);
                     begin
                        if Val = "false" then
                           Active := False;
                        else
                           Active := True;
                        end if;
                     end;

                  elsif V.Name.Id = A.Install.Side_Debug then
                     declare
                        Val : constant String := To_Lower (V.Value.Text);
                     begin
                        if Val = "true" then
                           Side_Debug := True;
                        else
                           Side_Debug := False;
                        end if;
                     end;

                  elsif V.Name.Id = A.Install.Install_Project then
                     declare
                        Val : constant String := To_Lower (V.Value.Text);
                     begin
                        if Val = "false" then
                           Install_Project := False;
                        else
                           Install_Project := True;
                        end if;
                     end;

                  elsif V.Name.Id in
                    A.Install.Artifacts | A.Install.Required_Artifacts
                  then
                     declare
                        Destination : constant Unbounded_String :=
                                        (if V.Index.Text = ""
                                         then To_Unbounded_String (".")
                                         else To_Unbounded_String
                                           (V.Index.Text));
                     begin
                        for S of V.Values loop
                           Artifacts.Append
                             (Artifacts_Data'
                                (Destination,
                                 To_Unbounded_String (S.Text),
                                 Required =>
                                   (if V.Name.Id = A.Install.Artifacts
                                    then False else True)));
                        end loop;
                     end;
                  end if;
               end loop;
            end;
         end if;

         --  Now check if Lib_Subdir is set and not ALI_Subdir as in this case
         --  we want ALI_Subdir to be equal to Lib_Subdir.

         if not Lib_Subdir.Default
           and then ALI_Subdir.Default
         then
            ALI_Subdir := Lib_Subdir;
         end if;
      end Check_Install_Package;

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File
        (From, To      : Path_Name.Object;
         File          : Filename_Optional := No_Filename;
         From_Ver      : Path_Name.Object  := Path_Name.Undefined;
         Sym_Link      : Boolean := False;
         Executable    : Boolean := False;
         Extract_Debug : Boolean := False)
      is
         Src_Path      : constant Path_Name.Object :=
                           (if From.Is_Directory
                            then From.Compose
                                  (if File = No_Filename
                                   then To.Simple_Name
                                   else File)
                            else From);
         F             : constant String := String (Src_Path.Value);
         Dest_Path     : constant Path_Name.Object :=
                           (if To.Is_Directory
                            then To.Compose
                                   (if File = No_Filename
                                    then From.Simple_Name
                                    else File)
                            else To);
         T             : constant String := String (Dest_Path.Dir_Name);
         Dest_Filename : aliased String  := Dest_Path.Value;
      begin
         pragma Warnings (Off, "*can never be executed*");

         if Sym_Link and then Is_Windows_Host then
            raise GPRinstall_Error
              with "internal error: cannot use symbolic links on Windows";
         end if;

         pragma Warnings (On, "*can never be executed*");

         if not Sym_Link
           and then Directories.Exists (Dest_Filename)
           and then not Options.Force_Installations
           and then Src_Path.Content_MD5 /= Dest_Path.Content_MD5
         then
            raise GPRinstall_Error
              with "file " & String (File) & " exists, use -f to overwrite";
         end if;

         if Options.Dry_Run or else Options.Verbose then
            if Sym_Link then
               Put ("ln -s ");
            else
               Put ("cp ");
            end if;

            Put (F);
            Put (" ");
            Put (Dest_Filename);
            New_Line;
         end if;

         if not Options.Dry_Run then
            --  If file exists and is read-only, first remove it

            if not Sym_Link and then Directories.Exists (Dest_Filename) then
               if not OS_Lib.Is_Writable_File (Dest_Filename) then
                  OS_Lib.Set_Writable (Dest_Filename);
               end if;

               declare
                  Success : Boolean;
               begin
                  OS_Lib.Delete_File (Dest_Filename, Success);

                  if not Success then
                     raise GPRinstall_Error with "cannot overwrite "
                       & Dest_Filename & " check permissions";
                  end if;
               end;
            end if;

            if not Sym_Link and then not Src_Path.Exists then
               raise GPRinstall_Error with
                 "file " & F & " does not exist, build may not be complete";
            end if;

            if (not Sym_Link and then not Directories.Exists (T))
              or else (Sym_Link and then not Src_Path.Exists)
            then
               if Options.Create_Dest_Dir then
                  begin
                     if Sym_Link then
                        Directories.Create_Path (Src_Path.Dir_Name);
                     else
                        Directories.Create_Path (T);
                     end if;
                  exception
                     when Text_IO.Use_Error =>
                        --  Cannot create path, permission issue
                        raise GPRinstall_Error with
                          "cannot create destination directory "
                          & (if Sym_Link then Src_Path.Dir_Name else T)
                          & " check permissions";
                  end;

               else
                  raise GPRinstall_Error with
                    "target directory "
                    & T & " does not exist, use -p to create";
               end if;
            end if;

            --  Do copy

            if Sym_Link then
               Src_Path.Create_Sym_Link (To => Dest_Path);

               --  Add file to manifest

               if Options.Install_Manifest then
                  Add_To_Manifest (Src_Path);
               end if;

               if From_Ver.Is_Defined then
                  From_Ver.Create_Sym_Link (To => Dest_Path);

                  if Options.Install_Manifest then
                     Add_To_Manifest (From_Ver);
                  end if;
               end if;

            else
               begin
                  Ada.Directories.Copy_File
                    (Source_Name => F,
                     Target_Name => Dest_Filename,
                     Form        => "preserve=timestamps");
               exception
                  when Text_IO.Use_Error =>
                     raise GPRinstall_Error with
                       "cannot overwrite file " & Dest_Filename
                        & " check permissions.";
               end;

               if Executable then
                  declare
                     use OS_Lib;
                  begin
                     OS_Lib.Set_Executable
                       (Dest_Filename,
                        Mode => S_Owner + S_Group + S_Others);
                  end;

                  --  Furthermore, if we have an executable and we ask for
                  --  separate debug symbols we do it now.
                  --  The commands to run are:
                  --    $ objcopy --only-keep-debug <exec> <exec>.debug
                  --    $ strip <exec>
                  --    $ objcopy --add-gnu-debuglink=<exec>.debug <exec>

                  if Extract_Debug then
                     if Objcopy = null then
                        Put_Line
                          (Objcopy_Exec & " not found, "
                           & "cannot create side debug file for "
                           & Dest_Filename);

                     elsif Strip = null then
                        Put_Line
                          (Strip_Exec & " not found, "
                           & "cannot create side debug file for "
                           & Dest_Filename);

                     else
                        declare
                           Keep_Debug : aliased String :=
                                          "--only-keep-debug";
                           Dest_Debug : aliased String :=
                                          Dest_Filename & ".debug";
                           Link_Debug : aliased String :=
                                          "--add-gnu-debuglink=" & Dest_Debug;
                           Success    : Boolean;
                           Args       : OS_Lib.Argument_List (1 .. 3);
                        begin
                           --  1. copy the debug symbols:

                           Args (1) := Keep_Debug'Unchecked_Access;
                           Args (2) := Dest_Filename'Unchecked_Access;
                           Args (3) := Dest_Debug'Unchecked_Access;

                           OS_Lib.Spawn (Objcopy.all, Args, Success);

                           if Success then
                              --  Record the debug file in the manifest
                              if Options.Install_Manifest then
                                 Add_To_Manifest
                                   (Path_Name.Create_File
                                      (Filename_Type (Dest_Debug)));
                              end if;

                              --  2. strip original executable

                              Args (1) := Dest_Filename'Unchecked_Access;

                              OS_Lib.Spawn (Strip.all, Args (1 .. 1), Success);

                              if Success then
                                 --  2. link debug symbols file with original
                                 --  file.

                                 Args (1) := Link_Debug'Unchecked_Access;
                                 Args (2) := Dest_Filename'Unchecked_Access;

                                 OS_Lib.Spawn
                                   (Objcopy.all, Args (1 .. 2), Success);

                                 if not Success then
                                    Put_Line
                                      (Objcopy_Exec & " error, "
                                       & "cannot link debug symbol file with"
                                       & " original executable "
                                       & Dest_Filename);
                                 end if;

                              else
                                 Put_Line
                                   (Strip_Exec & " error, "
                                    & "cannot remove debug symbols from "
                                    & Dest_Filename);
                              end if;

                           else
                              Put_Line
                                (Objcopy_Exec & " error, "
                                 & "cannot create side debug file for "
                                 & Dest_Filename);
                           end if;
                        end;
                     end if;
                  end if;
               end if;

               --  Add file to manifest

               if Options.Install_Manifest then
                  Add_To_Manifest (Dest_Path);
               end if;
            end if;
         end if;
      end Copy_File;

      ----------------
      -- Copy_Files --
      ----------------

      procedure Copy_Files is

         procedure Copy_Project_Sources (Project : GPR2.Project.View.Object);
         --  Copy sources from the given project

         function Copy_Source
           (Source : GPR2.Project.Source.Object) return Boolean;
         --  Copy Source and returns either artefactes need to be copied too

         procedure Copy_Artifacts
           (Pathname    : Path_Name.Object;
            Destination : Path_Name.Object;
            Required    : Boolean);
         --  Copy items from the artifacts attribute

         Source_Copied : GPR2.Project.Source.Set.Object;

         --------------------
         -- Copy_Artifacts --
         --------------------

         procedure Copy_Artifacts
           (Pathname    : Path_Name.Object;
            Destination : Path_Name.Object;
            Required    : Boolean)
         is
            use Ada.Directories;

            procedure Copy_Entry (E : Directory_Entry_Type);
            --  Copy file pointed by E

            Something_Copied : Boolean := False;
            --  Keep track if something has been copied or not. If an artifact
            --  is coming from Required_Artifacts we must ensure that there is
            --  actually something copied if we have a directory or wildcards.

            ----------------
            -- Copy_Entry --
            ----------------

            procedure Copy_Entry (E : Directory_Entry_Type) is
               Fullname : constant String := Full_Name (E);
               Dest_Dir : constant Path_Name.Object :=
                            Path_Name.Create_Directory
                              (Filename_Type (Destination.Value),
                               Filename_Optional (-Prefix_Dir.V));
            begin
               if Kind (E) = Directory
                 and then Directories.Simple_Name (E) /= "."
                 and then Directories.Simple_Name (E) /= ".."
               then
                  Copy_Artifacts
                    (Path_Name.Create_File
                       ("*", Filename_Optional (Fullname)),
                     Path_Name.Compose
                       (Dest_Dir,
                        Filename_Type (Directories.Simple_Name (E)),
                        Directory => True),
                     Required);

               elsif Kind (E) = Ordinary_File then
                  Copy_File
                    (From       =>
                       Path_Name.Create_File (Filename_Type (Fullname)),
                     To         => Destination,
                     Executable => OS_Lib.Is_Executable_File (Fullname));

                  if Required then
                     Something_Copied := True;
                  end if;
               end if;
            end Copy_Entry;

         begin
            Ada.Directories.Search
              (Directory => Pathname.Dir_Name,
               Pattern   => String (Pathname.Simple_Name),
               Process   => Copy_Entry'Access);

            if Required and not Something_Copied then
               Rollback_Manifests;
               raise GPRinstall_Error with
                 "error: file does not exist '" & Pathname.Value & ''';
            end if;
         exception
            when Text_IO.Name_Error =>
               if Required then
                  Rollback_Manifests;
                  raise GPRinstall_Error with
                    "error: file does not exist '" & Pathname.Value & ''';
               else
                  Put_Line
                    ("warning: file does not exist '" & Pathname.Value & ''');
               end if;
         end Copy_Artifacts;

         --------------------------
         -- Copy_Project_Sources --
         --------------------------

         procedure Copy_Project_Sources (Project : GPR2.Project.View.Object) is

            use all type GPR2.Project.Standalone_Library_Kind;

            function Is_Ada
              (Source : GPR2.Project.Source.Object) return Boolean
            is (Source.Language = Ada_Language);
            --  Returns True if Source is an Ada source

            procedure Install_Project_Source
              (Source               : GPR2.Project.Source.Object;
               Is_Interface_Closure : Boolean := False);
            --  Install the project source and possibly the corresponding
            --  artifacts.

            procedure Copy_Interface_Closure
              (Source : GPR2.Project.Source.Object;
               Index  : GPR2.Unit_Index)
            with Pre => Source.Has_Units;
            --  Copy all sources and artifacts part of the close of Source

            ----------------------------
            -- Copy_Interface_Closure --
            ----------------------------

            procedure Copy_Interface_Closure
              (Source : GPR2.Project.Source.Object;
               Index  : GPR2.Unit_Index) is
            begin
               --  Note that we only install the interface from the same view
               --  to avoid installing the runtime file for example.

               for D of Source.Dependencies
                 (Index, Closure => True, Sorted => False)
               loop
                  if not Source_Copied.Contains (D.Source)
                    and then (D.Source.Kind (D.Index) in Unit.Spec_Kind
                              or else Other_Part_Need_Body (D.Source, D.Index))
                    and then Source.View = D.Source.View
                  then
                     Install_Project_Source (D.Source,
                                             Is_Interface_Closure => True);
                  end if;
               end loop;
            end Copy_Interface_Closure;

            ----------------------------
            -- Install_Project_Source --
            ----------------------------

            procedure Install_Project_Source
              (Source                : GPR2.Project.Source.Object;
               Is_Interface_Closure  : Boolean := False)
            is
               Atf     : GPR2.Project.Source.Artifact.Object;
               CUs     : GPR2.Unit.List.Object;
               Done    : Boolean := True;
               Has_Atf : Boolean := False;
               --  Has artefacts to install

            begin
               --  Skip sources that are removed/excluded and sources not
               --  part of the interface for standalone libraries.

               Atf := Source.Artifacts;

               if not Project.Is_Library
                 or else Project.Library_Standalone = No
                 or else Is_Interface_Closure
                 or else Source.Is_Interface
               then
                  if Source.Has_Units then
                     CUs := Source.Units;
                  end if;

                  if Options.All_Sources
                    or else Source.Kind in Unit.Spec_Kind
                    or else Other_Part_Need_Body (Source, No_Index)
                    or else Source.Is_Generic (No_Index)
                    or else
                      (Source.Kind = S_Separate
                       and then Source.Separate_From
                         (No_Index).Source.Is_Generic (No_Index))
                  then
                     Done := Copy_Source (Source);

                     --  This if a source is an interface of the project we
                     --  need to also install the full-closure for this source.

                     if Source.Is_Interface
                       and then Source.Has_Units
                       and then not Is_Interface_Closure
                     then
                        if Source.Has_Units then
                           for CU of CUs loop
                              Copy_Interface_Closure (Source, CU.Index);
                           end loop;
                        else
                           Copy_Interface_Closure (Source, No_Index);
                        end if;
                     end if;

                  elsif Source.Has_Naming_Exception then
                     --  When a naming exception is present for a body which
                     --  is not installed we must exclude the Naming from the
                     --  generated project.

                     for CU of CUs loop
                        Excluded_Naming.Include (CU.Name);
                     end loop;
                  end if;

                  --  Objects / Deps

                  for CU of CUs loop
                     if CU.Kind not in S_Spec | S_Separate then
                        Has_Atf := True;
                        exit;
                     end if;
                  end loop;

                  if Done
                    and then not Options.Sources_Only
                    and then Has_Atf
                  then
                     if Copy (Object) then
                        for CU of CUs loop
                           if CU.Kind not in S_Spec | S_Separate
                             and then Atf.Has_Object_Code (CU.Index)
                           then
                              Copy_File
                                (From => Atf.Object_Code (CU.Index),
                                 To   => Lib_Dir);
                           end if;
                        end loop;
                     end if;

                     --  Install Ada .ali files (name the .ali
                     --  against the spec file in case of minimal
                     --  installation).

                     if Copy (Dependency) then
                        declare
                           use GPR2.Project.Source.Artifact;
                           Proj : GPR2.Project.View.Object;
                           Satf : GPR2.Project.Source.Artifact.Object;
                        begin
                           if Options.All_Sources
                             or else not Source.Has_Naming_Exception
                             or else not Source.Has_Single_Unit
                             or else not Source.Has_Other_Part
                           then
                              Satf := Atf;
                           else
                              Satf :=
                                Source.Other_Part.Source.Artifacts
                                                          (Force_Spec => True);
                           end if;

                           if Project.Qualifier = K_Aggregate_Library then
                              Proj := Project;
                           else
                              Proj := Source.View;
                           end if;

                           if Is_Ada (Source) then
                              for CU of Source.Units loop
                                 if Source.Kind (CU.Index)
                                      not in S_Spec | S_Separate
                                   and then Atf.Has_Dependency (CU.Index)
                                 then
                                    Copy_File
                                      (From => Atf.Dependency (CU.Index),
                                       To   => (if Proj.Kind = K_Library
                                                then ALI_Dir
                                                else Lib_Dir),
                                       File => Satf.Dependency.Simple_Name);
                                 end if;
                              end loop;
                           end if;

                           if Atf.Has_Callgraph
                             and then Atf.Callgraph.Exists
                           then
                              Copy_File
                                (From => Atf.Callgraph,
                                 To   => (if Proj.Kind = K_Library
                                          then ALI_Dir
                                          else Lib_Dir),
                                 File => Satf.Callgraph.Simple_Name);
                           end if;

                           if Atf.Has_Coverage
                             and then Atf.Coverage.Exists
                           then
                              Copy_File
                                (From => Atf.Coverage,
                                 To   => (if Proj.Kind = K_Library
                                          then ALI_Dir
                                          else Lib_Dir),
                                 File => Satf.Coverage.Simple_Name);
                           end if;
                        end;
                     end if;
                  end if;
               end if;
            end Install_Project_Source;

         begin
            for Source of Project.Sources loop
               Install_Project_Source (Source);
            end loop;
         end Copy_Project_Sources;

         -----------------
         -- Copy_Source --
         -----------------

         function Copy_Source
           (Source : GPR2.Project.Source.Object) return Boolean
         is
            Position : GPR2.Project.Source.Set.Cursor;
            Inserted : Boolean := False;
         begin
            Source_Copied.Insert (Source, Position, Inserted);

            if not Inserted or else not Is_Install_Active (Source.View) then
               return False;

            elsif not Copy (Process.Source) then
               return Inserted;
            end if;

            declare
               Art : constant GPR2.Project.Source.Artifact.Object :=
                       Source.Artifacts;
            begin
               Copy_File
                 (From => (if Art.Preprocessed_Source.Exists
                           then Art.Preprocessed_Source
                           else Source.Path_Name),
                  To   => Sources_Dir,
                  File => Source.Path_Name.Simple_Name);
            end;

            return True;
         end Copy_Source;

      begin
         if Has_Sources (Project) then
            --  Install the project and the extended projects if any

            Copy_Project_Sources (Project);
         end if;

         --  Copy library

         if Copy (Library) and then not Options.Sources_Only then
            if not Project.Is_Static_Library
              and then Project.Has_Library_Version
              and then Project.Library_Name
                         /= Project.Library_Version_Filename.Name
            then
               if Windows_Target then
                  --  No support for version, do a simple copy

                  Copy_File
                    (From          => Project.Library_Directory,
                     To            => Lib_Dir,
                     File          => Project.Library_Filename.Name,
                     Executable    => True,
                     Extract_Debug => Side_Debug);

               elsif Is_Windows_Host then
                  --  On windows host, Library_Filename is generated,

                  Copy_File
                    (From          => Project.Library_Filename,
                     To            => Lib_Dir,
                     Executable    => True,
                     Extract_Debug => Side_Debug);

               else
                  Copy_File
                    (From          => Project.Library_Version_Filename,
                     To            => Lib_Dir,
                     Executable    => True,
                     Extract_Debug => Side_Debug);

                  Copy_File
                    (From     => Path_Name.Compose
                       (Lib_Dir,
                        Project.Library_Filename.Name),
                     To       => Lib_Dir,
                     File     => Project.Library_Version_Filename.Simple_Name,
                     From_Ver => Path_Name.Compose
                       (Lib_Dir,
                        Project.Library_Major_Version_Filename.Name),
                     Sym_Link => True);
               end if;

            else
               Copy_File
                 (From          => Project.Library_Directory,
                  To            => Lib_Dir,
                  File          => Project.Library_Filename.Name,
                  Executable    => not Project.Is_Static_Library,
                  Extract_Debug =>
                    Side_Debug and then not Project.Is_Static_Library);
            end if;

            --  On Windows copy the shared libraries into the bin directory
            --  for it to be found in the PATH when running executable. On non
            --  Windows platforms add a symlink into the lib directory.

            if not Project.Is_Static_Library
              and then not Options.No_Lib_Link
            then
               if Windows_Target then
                  if Lib_Dir /= Exec_Dir then
                     Copy_File
                       (From          => Lib_Dir,
                        To            => Exec_Dir,
                        File          => Project.Library_Filename.Name,
                        Executable    => True,
                        Extract_Debug => False);
                  end if;

               elsif Link_Lib_Dir /= Lib_Dir then
                  if Is_Windows_Host then
                     Copy_File
                       (From       => Lib_Dir,
                        To         => Link_Lib_Dir,
                        File       => Project.Library_Filename.Name,
                        Sym_Link   => False);
                  else
                     Copy_File
                       (From       => Link_Lib_Dir,
                        To         => Lib_Dir,
                        File       => Project.Library_Filename.Name,
                        Sym_Link   => True);
                  end if;

                  --  Copy also the versioned library if any

                  if not Is_Windows_Host and then Project.Has_Library_Version
                    and then
                      Project.Library_Filename.Name
                        /= Project.Library_Version_Filename.Name
                  then
                     Copy_File
                       (From       => Link_Lib_Dir,
                        To         => Lib_Dir,
                        File       => Project.Library_Version_Filename.Name,
                        From_Ver   => Path_Name.Compose
                          (Link_Lib_Dir,
                           Project.Library_Major_Version_Filename.Name),
                        Sym_Link   => True);
                  end if;
               end if;
            end if;
         end if;

         --  Copy executable(s)

         if Copy (Executable) and then not Options.Sources_Only then
            for Main of Project.Executables loop
               Copy_File
                 (From          => Main,
                  To            => Exec_Dir,
                  Executable    => True,
                  Extract_Debug => Side_Debug);
            end loop;
         end if;

         --  Copy artifacts

         for E of Artifacts loop
            declare
               Destination : constant Filename_Type :=
                               Filename_Type (To_String (E.Destination));
               Filename    : constant Filename_Type :=
                               Filename_Type (To_String (E.Filename));
            begin
               Copy_Artifacts
                 (Path_Name.Compose (Project.Dir_Name, Filename),
                  Path_Name.Create_Directory
                    (Destination,
                     Filename_Optional (-Prefix_Dir.V)),
                  E.Required);
            end;
         end loop;
      end Copy_Files;

      --------------------
      -- Create_Project --
      --------------------

      procedure Create_Project (Project : GPR2.Project.View.Object) is

         package Lang_Set is new Ada.Containers.Indefinite_Ordered_Sets
           (String,
            Strings.Less_Case_Insensitive, Strings.Equal_Case_Insensitive);

         Filename : constant String :=
                      Project_Dir.Dir_Name
                      & String (Project.Path_Name.Base_Name) & ".gpr";

         GPRinstall_Tag : constant String :=
                            "This project has been generated by GPRINSTALL";

         Line      : Unbounded_String;

         Languages : Lang_Set.Set;

         function "+"
           (Item : String) return Unbounded_String renames To_Unbounded_String;
         function "-"
           (Item : Unbounded_String) return String renames To_String;

         procedure Create_Packages;
         --  Create packages that are needed, currently Naming and part of
         --  Linker is generated for the installed project.

         procedure Create_Variables;
         --  Create global variables

         procedure Read_Project;
         --  Read project and set Content accordingly

         procedure With_External_Imports (Project : GPR2.Project.View.Object);
         --  Add all imports of externally built projects into install project
         --  imports.

         procedure Write_Project;
         --  Write content into project

         procedure Add_Empty_Line with Inline;

         function Naming_Case_Alternative
           (Project : GPR2.Project.View.Object) return String_Vector.Vector;
         --  Returns the naming case alternative for this project configuration

         function Linker_Case_Alternative
           (Proj : GPR2.Project.View.Object) return String_Vector.Vector;
         --  Returns the linker case alternative for this project configuration

         function Data_Attributes return String_Vector.Vector;
         --  Returns the attributes for the sources, objects and library

         function Get_Languages return Lang_Set.Set;
         --  Returns the list of languages

         function Get_Build_Line (Vars, Default : String) return String;
         --  Returns the build line for Var1 and possibly Var2 if not empty
         --  string. Default is the default build name.

         --------------------
         -- Add_Empty_Line --
         --------------------

         procedure Add_Empty_Line is
         begin
            if Content.Element (Content.Last_Index) /= "" then
               Content.Append ("");
            end if;
         end Add_Empty_Line;

         ---------------------
         -- Create_Packages --
         ---------------------

         procedure Create_Packages is

            procedure Create_Naming (Project : GPR2.Project.View.Object);
            --  Create the naming package

            procedure Create_Linker (Project : GPR2.Project.View.Object);
            --  Create the linker package if needed

            -------------------
            -- Create_Linker --
            -------------------

            procedure Create_Linker (Project : GPR2.Project.View.Object) is
            begin
               Content.Append ("   package Linker is");

               Content.Append ("      case BUILD is");
               --  Attribute Linker_Options only if set

               Content.Append_Vector (Linker_Case_Alternative (Project));

               Content.Append ("      end case;");
               Content.Append ("   end Linker;");
               Add_Empty_Line;
            end Create_Linker;

            -------------------
            -- Create_Naming --
            -------------------

            procedure Create_Naming (Project : GPR2.Project.View.Object) is
               Found : Boolean := False;
            begin
               Content.Append ("   package Naming is");

               for A of Project.Attributes (Pack          => P.Naming,
                                            With_Defaults => False,
                                            With_Config   => False)
               loop
                  if not A.Has_Index then
                     Content.Append ("      " & A.Image);
                     Found := True;
                  end if;
               end loop;

               if Found then
                  Content.Append ("");
               end if;

               Content.Append ("      case BUILD is");

               Content.Append_Vector (Naming_Case_Alternative (Project));

               Content.Append ("      end case;");
               Content.Append ("   end Naming;");
               Add_Empty_Line;
            end Create_Naming;

         begin
            Create_Naming (Project);
            Create_Linker (Project);
         end Create_Packages;

         ----------------------
         -- Create_Variables --
         ----------------------

         procedure Create_Variables is
            Max_Len : Natural := 0;

            --  List of output types to avoid duplicate
            T       : GPR2.Containers.Name_Set;

            procedure Create_Type (Typ : GPR2.Project.Typ.Object);
            --  Output type definition if not already created

            -----------------
            -- Create_Type --
            -----------------

            procedure Create_Type (Typ : GPR2.Project.Typ.Object) is
               T_Name : constant Name_Type := Typ.Name.Text;
            begin
               if not T.Contains (T_Name) then
                  Write_Str ("   " & Typ.Image);
                  Write_Eol;

                  T.Insert (T_Name);
               end if;
            end Create_Type;

            Var_Has_Type : Boolean := False;

         begin
            --  Output types if any

            if Project.Has_Types then
               for Typ of Project.Types loop
                  Create_Type (Typ);
               end loop;
            end if;

            if Project.Has_Variables then
               for Var of Project.Variables loop
                  --  Compute variable max length

                  Max_Len := Natural'Max (Max_Len, Var.Name.Text'Length);

                  --  Output types used in variable if any

                  if Var.Has_Type then
                     Create_Type (Var.Typ);
                     Var_Has_Type := True;
                  end if;
               end loop;

               if Var_Has_Type then
                  Write_Eol;
               end if;

               --  Finally output variables

               for Var of Project.Variables loop
                  Write_Str ("   " & Var.Image (Name_Len => Max_Len));
                  Write_Eol;
               end loop;
            end if;
         end Create_Variables;

         ---------------------
         -- Data_Attributes --
         ---------------------

         function Data_Attributes return String_Vector.Vector is

            procedure Gen_Dir_Name
              (P : Param; Line : in out Unbounded_String);
            --  Generate dir name

            ------------------
            -- Gen_Dir_Name --
            ------------------

            procedure Gen_Dir_Name
              (P : Param; Line : in out Unbounded_String) is
            begin
               if P.Default then
                  --  This is the default value, add Dir_Name
                  Line := Line & String (Dir_Name (Suffix => False));

                  --  Furthermore, if the build name is "default" do not output

                  if -Options.Build_Name /= "default" then
                     Line := Line & "." & (-Options.Build_Name);
                  end if;
               end if;
            end Gen_Dir_Name;

            V          : String_Vector.Vector;
            Line       : Unbounded_String;
            Attr       : GPR2.Project.Attribute.Object;
            Standalone : GPR2.Project.Standalone_Library_Kind;
            use type GPR2.Project.Standalone_Library_Kind;

         begin
            V.Append ("      when """ & (-Options.Build_Name) & """ =>");

            --  Project sources

            Line := +"         for Source_Dirs use (""";

            if Has_Sources (Project) then
               Line := Line
                 & String (Sources_Dir (Build_Name => False).Relative_Path
                           (From => Project_Dir).Name);

               Gen_Dir_Name (Sources_Subdir, Line);
            end if;

            Line := Line & """);";

            V.Append (-Line);

            --  Project objects and/or library

            if Project.Is_Library then
               Line := +"         for Library_Dir use """;
            else
               Line := +"         for Object_Dir use """;
            end if;

            Line := Line
              & String (Lib_Dir (Build_Name => False).Relative_Path
                        (From => Project_Dir).Name);

            Gen_Dir_Name (Lib_Subdir, Line);
            Line := Line & """;";

            V.Append (-Line);

            if Project.Is_Library then
               --  If ALI are in a different location, set the corresponding
               --  attribute.

               if Lib_Dir /= ALI_Dir then
                  Line := +"         for Library_ALI_Dir use """;

                  Line := Line
                    & String (ALI_Dir (Build_Name => False).Relative_Path
                              (From => Project_Dir).Name);

                  Gen_Dir_Name (ALI_Subdir, Line);
                  Line := Line & """;";
                  V.Append (-Line);
               end if;

               V.Append
                 ("         for Library_Kind use """
                  & String (Project.Library_Kind) & """;");

               Standalone := Project.Library_Standalone;

               if Standalone /= GPR2.Project.No then
                  if not Project.Is_Static_Library then
                     V.Append
                       ("         for Library_Standalone use """
                        & Characters.Handling.To_Lower (Standalone'Image)
                        & """;");
                  end if;

                  --  And then generates the interfaces

                  declare
                     First : Boolean := True;
                  begin
                     if Project.Check_Attribute
                       (A.Library_Interface, Result => Attr)
                     then
                        Line := +"         for Library_Interface use (";

                        for V of Attr.Values loop
                           if not First then
                              Append (Line, ", ");
                           end if;

                           Append (Line, Quote (V.Text));
                           First := False;
                        end loop;

                     elsif Project.Check_Attribute
                       (A.Interfaces, Result => Attr)
                     then
                        Line := +"         for Interfaces use (";

                        for V of Attr.Values loop
                           if not First then
                              Append (Line, ", ");
                           end if;

                           Append (Line, Quote (V.Text));
                           First := False;
                        end loop;

                     else
                        Line := +"         for library_Interfaces use (";

                        for Source of Project.Sources (Interface_Only => True)
                        loop
                           if Source.Has_Units then
                              for CU of Source.Units loop
                                 if CU.Kind in
                                   S_Spec | S_Spec_Only | S_Body_Only
                                 then
                                    if not First then
                                       Append (Line, ", ");
                                    end if;

                                    Append (Line, Quote (String (CU.Name)));
                                    First := False;
                                 end if;
                              end loop;
                           end if;
                        end loop;
                     end if;
                  end;

                  Append (Line, ");");
                  V.Append (-Line);
               end if;
            end if;

            return V;
         end Data_Attributes;

         --------------------
         -- Get_Build_Line --
         --------------------

         function Get_Build_Line (Vars, Default : String) return String is
            use Strings.Fixed;
            Variables : String_Split.Slice_Set;
            Line      : Unbounded_String;
         begin
            Line := +"   BUILD : BUILD_KIND := ";

            if not Options.No_Build_Var then
               String_Split.Create (Variables, Vars, ",");

               if Vars = "" then
                  --  No variable specified, use default value
                  Line := Line & "external(""";
                  Line := Line & Characters.Handling.To_Upper
                                   (String (Dir_Name (Suffix => False)));
                  Line := Line & "_BUILD"", ";

               else
                  for K in 1 .. String_Split.Slice_Count (Variables) loop
                     Line := Line & "external(""";
                     Line := Line & String_Split.Slice (Variables, K) & """, ";
                  end loop;
               end if;
            end if;

            Line := Line & '"' & Default & '"';

            if not Options.No_Build_Var then
               Line := Line
                 & (+(Natural (String_Split.Slice_Count (Variables)) * ')'));
            end if;

            Line := Line & ';';

            return -Line;
         end Get_Build_Line;

         -------------------
         -- Get_Languages --
         -------------------

         function Get_Languages return Lang_Set.Set is

            Langs : Lang_Set.Set;

            procedure For_Project (Project : GPR2.Project.View.Object);
            --  Add languages for the given project

            -----------------
            -- For_Project --
            -----------------

            procedure For_Project (Project : GPR2.Project.View.Object) is
               use GPR2.Project;

               package A renames GPR2.Project.Registry.Attribute;
               package P renames GPR2.Project.Registry.Pack;
               Attr : GPR2.Project.Attribute.Object;
            begin
               if Project.Has_Languages then
                  for Lang of Project.Languages loop
                     if Project.Tree.Has_Configuration then
                        declare
                           C : constant GPR2.Project.View.Object :=
                                 Project.Tree.Configuration.Corresponding_View;
                        begin
                           if C.Has_Package (P.Compiler)
                             and then C.Check_Attribute
                               (A.Compiler.Driver,
                                Attribute_Index.Create (Lang.Text),
                                Result => Attr)
                             and then Attr.Value.Text /= ""
                           then
                              Langs.Include (Lang.Text);
                           end if;
                        end;
                     end if;
                  end loop;
               end if;
            end For_Project;

         begin
            --  First adds language for the main project

            For_Project (Project);

            --  If we are dealing with an aggregate library, adds the languages
            --  from all aggregated projects.

            if Project.Qualifier = K_Aggregate_Library then
               for Agg of Project.Aggregated loop
                  For_Project (Agg);
               end loop;
            end if;

            return Langs;
         end Get_Languages;

         -----------------------------
         -- Linker_Case_Alternative --
         -----------------------------

         function Linker_Case_Alternative
           (Proj : GPR2.Project.View.Object) return String_Vector.Vector
         is
            use type Ada.Containers.Count_Type;

            procedure Linker_For (View : GPR2.Project.View.Object);
            --  Handle the linker options for this package

            procedure Append (Attribute : GPR2.Project.Attribute.Object);
            --  Add values if any

            procedure Opts_Append (Opt : String);
            --  Add Opt into Opts only if not added before

            procedure Append_Imported_External_Libraries
              (Project : GPR2.Project.View.Object);
            --  Add the externally built libraries without sources (referencing
            --  system libraries for example).

            Seen : GPR2.Containers.Value_Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            R    : String_Vector.Vector;
            Opts : String_Vector.Vector;

            ------------
            -- Append --
            ------------

            procedure Append (Attribute : GPR2.Project.Attribute.Object) is
            begin
               for V of Attribute.Values loop
                  if V.Text /= "" then
                     Opts_Append (V.Text);
                  end if;
               end loop;
            end Append;

            ----------------------------------------
            -- Append_Imported_External_Libraries --
            ----------------------------------------

            procedure Append_Imported_External_Libraries
              (Project : GPR2.Project.View.Object) is
            begin
               if Project.Has_Imports then
                  for L of Project.Imports (Recursive => True) loop
                     if L.Kind = K_Library
                       and then L.Is_Externally_Built
                       and then not L.Has_Sources
                     then
                        Opts_Append ("-L" & L.Library_Directory.Value);
                        Opts_Append ("-l" & String (L.Library_Name));
                     end if;
                  end loop;
               end if;
            end Append_Imported_External_Libraries;

            ----------------
            -- Linker_For --
            ----------------

            procedure Linker_For (View : GPR2.Project.View.Object) is
            begin
               if View.Has_Attribute (A.Linker.Linker_Options)
               then
                  Append (View.Attribute (A.Linker.Linker_Options));
               end if;
            end Linker_For;

            -----------------
            -- Opts_Append --
            -----------------

            procedure Opts_Append (Opt : String) is
               Position : GPR2.Containers.Value_Type_Set.Cursor;
               Inserted : Boolean;
            begin
               Seen.Insert (Opt, Position, Inserted);

               if Inserted then
                  Opts.Append (Opt);
               end if;
            end Opts_Append;

         begin
            R.Append ("         when """ & (-Options.Build_Name) & """ =>");

            Linker_For (Project);

            if Proj.Qualifier = K_Aggregate_Library then
               for Aggregated of Proj.Aggregated loop
                  Linker_For (Aggregated);
                  Append_Imported_External_Libraries (Aggregated);
               end loop;
            end if;

            Append_Imported_External_Libraries (Project);

            --  Append Library_Options to Opts list

            if Proj.Is_Library then
               declare
                  Library_Options : constant GPR2.Project.Attribute.Object :=
                                      Proj.Attribute (A.Library_Options);
               begin
                  if Library_Options.Is_Defined then
                     for Value of Library_Options.Values loop
                        Opts_Append (Value.Text);
                     end loop;
                  end if;
               end;
            end if;

            if Opts.Length = 0 then
               --  No linker alternative found, add null statement
               R.Append ("            null;");

            else
               declare
                  O_List : Unbounded_String;
               begin
                  for O of Opts loop
                     if O_List /= Null_Unbounded_String then
                        Append (O_List, ", ");
                     end if;

                     Append (O_List, '"' & O & '"');
                  end loop;

                  R.Append
                    ("            for Linker_Options use ("
                     & To_String (O_List) & ");");
               end;
            end if;

            return R;
         end Linker_Case_Alternative;

         -----------------------------
         -- Naming_Case_Alternative --
         -----------------------------

         function Naming_Case_Alternative
           (Project : GPR2.Project.View.Object) return String_Vector.Vector
         is
            procedure Naming_For (View : GPR2.Project.View.Object);
            --  Handle the naming scheme for this view

            Seen : GPR2.Containers.Name_Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            V    : String_Vector.Vector;
            --  Contains the final result returned

            function Is_Language_Active (Lang : String) return Boolean
              is (Languages.Contains ((Characters.Handling.To_Lower (Lang))));
            --  Returns True if Lang is active in the installed project

            ----------------
            -- Naming_For --
            ----------------

            procedure Naming_For (View : GPR2.Project.View.Object) is
               Found : Boolean := False;
            begin
               if View.Has_Package (P.Naming,
                                    With_Defaults => False,
                                    With_Config   => False)
               then
                  --  Check all associative attributes

                  for Att of View.Attributes (Pack          => P.Naming,
                                              With_Defaults => False,
                                              With_Config   => False) loop
                     if Att.Has_Index then
                        if (Att.Name.Id /= A.Naming.Body_N
                            or else not
                              Excluded_Naming.Contains
                                (Name_Type (Att.Index.Text)))
                          and then
                              ((Att.Name.Id not in A.Naming.Spec_Suffix |
                                                   A.Naming.Body_Suffix |
                                                   A.Naming.Separate_Suffix)
                             or else Is_Language_Active (Att.Index.Text))
                        then
                           declare
                              Decl : constant String := Att.Image;
                              Pos  : GPR2.Containers.Name_Type_Set.Cursor;
                              OK   : Boolean;
                           begin
                              Seen.Insert (Name_Type (Decl), Pos, OK);

                              if OK then
                                 V.Append ("            " & Decl);
                                 Found := True;
                              end if;
                           end;
                        end if;
                     end if;
                  end loop;
               end if;

               if not Found then
                  V.Append ("            null;");
               end if;
            end Naming_For;

         begin
            V.Append ("         when """ & (-Options.Build_Name) & """ =>");

            Naming_For (Project);

            if Project.Qualifier = K_Aggregate_Library then
               for Agg of Project.Aggregated loop
                  Naming_For (Agg);
               end loop;
            end if;

            return V;
         end Naming_Case_Alternative;

         ------------------
         -- Read_Project --
         ------------------

         procedure Read_Project is
            Max_Buffer : constant := 1_024;
            File       : File_Type;
            Buffer     : String (1 .. Max_Buffer);
            Last       : Natural;
         begin
            Open (File, In_File, Filename);

            while not End_Of_File (File) loop
               declare
                  L : Unbounded_String;
               begin
                  loop
                     Get_Line (File, Buffer, Last);
                     Append (L, Buffer (1 .. Last));

                     exit when Last < Max_Buffer or else End_Of_Line (File);
                  end loop;

                  Content.Append (To_String (L));
               end;
            end loop;

            Close (File);
         end Read_Project;

         ---------------------------
         -- With_External_Imports --
         ---------------------------

         procedure With_External_Imports
           (Project : GPR2.Project.View.Object) is
         begin
            for L of Project.Imports (Recursive => True) loop
               if L.Has_Sources and then L.Is_Externally_Built then
                  Content.Append
                    ("with """ & String (L.Path_Name.Base_Name) & """;");
               end if;
            end loop;
         end With_External_Imports;

         -------------------
         -- Write_Project --
         -------------------

         procedure Write_Project is
            F    : File_Access := Standard_Output;
            File : aliased File_Type;
         begin
            if not Options.Dry_Run then
               if not Project_Dir.Exists then
                  Directories.Create_Path (Project_Dir.Value);
               end if;

               Create (File, Out_File, Filename);
               F := File'Unchecked_Access;
            end if;

            for Line of Content loop
               Put_Line (F.all, Line);
            end loop;

            if not Options.Dry_Run then
               Close (File);
            end if;
         end Write_Project;

         type Section_Kind is (Top, Naming, Linker);

         Project_Exists  : constant Boolean := Directories.Exists (Filename);
         Current_Section : Section_Kind := Top;
         Pos             : String_Vector.Cursor;
         Generated       : Boolean := False;

      begin
         --  Set-up all languages used by the project tree

         Languages := Get_Languages;

         if Options.Dry_Run or else Options.Verbose then
            New_Line;
            Put ("Project ");
            Put (Filename);

            if Options.Dry_Run then
               Put_Line (" would be installed");
            else
               Put_Line (" installed");
            end if;

            New_Line;
         end if;

         --  If project exists, read it and check the generated status

         if Project_Exists then
            Read_Project;

            --  First check that this project has been generated by gprbuild,
            --  if not exit with an error as we cannot modify a project created
            --  manually and we do not want to overwrite it.

            Pos := Content.First;

            Check_Generated_Status : while String_Vector.Has_Element (Pos) loop
               if Strings.Fixed.Index
                 (String_Vector.Element (Pos), GPRinstall_Tag) /= 0
               then
                  Generated := True;
                  exit Check_Generated_Status;
               end if;

               String_Vector.Next (Pos);
            end loop Check_Generated_Status;

            if not Generated and then not Options.Force_Installations then
               raise GPRinstall_Error with
                 "non gprinstall project file "
                 & Filename & " exists, use -f to overwrite";
            end if;
         end if;

         if Project_Exists and then Generated then
            if not Has_Sources (Project) then
               --  Nothing else to do in this case
               return;
            end if;

            if Options.Verbose then
               Put_Line ("project file exists, merging new build");
            end if;

            --  Do merging for new build, we need to add an entry into the
            --  BUILD_KIND type and a corresponding case entry in the naming
            --  and Linker package. The variables need also to be updated.

            Parse_Content : while String_Vector.Has_Element (Pos) loop
               declare
                  use Ada.Strings;

                  procedure Insert_Move_After
                    (Pos : in out String_Vector.Cursor;
                     V   : String_Vector.Vector);
                  --  Insert V into Content before Pos and return Post to be
                  --  the item after the last inserted.

                  -----------------------
                  -- Insert_Move_After --
                  -----------------------

                  procedure Insert_Move_After
                    (Pos : in out String_Vector.Cursor;
                     V   : String_Vector.Vector)
                  is
                     C : constant Ada.Containers.Count_Type := V.Length;
                     P : constant String_Vector.Extended_Index :=
                           String_Vector.To_Index (Pos);
                  begin
                     String_Vector.Next (Pos);
                     Content.Insert_Vector (Pos, V);

                     Pos := Content.To_Cursor
                       (P + String_Vector.Extended_Index (C));
                  end Insert_Move_After;

                  BN   : constant String := -Options.Build_Name;
                  Line : constant String := String_Vector.Element (Pos);
                  P, L : Natural;
               begin
                  if Fixed.Index (Line, "type BUILD_KIND is (") /= 0 then
                     --  This is the "type BUILD_KIND" line, add new build name

                     --  First check if the current build name already exists

                     if Fixed.Index (Line, """" & BN & """") = 0 then
                        --  Get end of line

                        P := Strings.Fixed.Index (Line, ");");

                        if P = 0 then
                           raise GPRinstall_Error with
                             "cannot parse the BUILD_KIND line";

                        else
                           Content.Replace_Element
                             (Pos,
                              Line (Line'First .. P - 1)
                              & ", """ & BN & """);");
                        end if;
                     end if;

                  elsif Fixed.Index (Line, ":= external(") /= 0 then

                     --  This is the BUILD line, get build vars

                     declare
                        Default : Unbounded_String;
                     begin
                        --  Get default value

                        L := Fixed.Index
                          (Line, """", Going => Strings.Backward);
                        P := Fixed.Index
                          (Line (Line'First .. L - 1), """",
                           Going => Strings.Backward);

                        Default := +Line (P + 1 .. L - 1);

                        Content.Replace_Element
                          (Pos,
                           Get_Build_Line
                             (-Options.Build_Vars, -Default));
                     end;

                  elsif Fixed.Index (Line, "package Naming is") /= 0 then
                     Current_Section := Naming;

                  elsif Fixed.Index (Line, "package Linker is") /= 0 then
                     Current_Section := Linker;

                  elsif Fixed.Index (Line, "case BUILD is") /= 0 then

                     --  Add new case section for the new build name

                     case Current_Section is
                        when Naming =>
                           Insert_Move_After
                             (Pos, Naming_Case_Alternative (Project));

                        when Linker =>
                           Insert_Move_After
                             (Pos, Linker_Case_Alternative (Project));

                        when Top =>
                           --  For the Sources/Lib attributes
                           Insert_Move_After (Pos, Data_Attributes);
                     end case;

                  elsif Fixed.Index (Line, "when """ & BN & """ =>") /= 0 then
                     --  Found a when with the current build name, this is a
                     --  previous install overwritten by this one. Remove this
                     --  section. Note that this removes sections from all
                     --  packages Naming and Linker, and from project level
                     --  case alternative.

                     Count_And_Delete : declare

                        use type Ada.Containers.Count_Type;

                        function End_When (L : String) return Boolean;
                        --  Return True if L is the end of a when alternative

                        --------------
                        -- End_When --
                        --------------

                        function End_When (L : String) return Boolean is
                           P   : constant Natural :=
                                   Strings.Fixed.Index_Non_Blank (L);
                           Len : constant Natural := L'Last;
                        begin
                           return P > 0
                             and then
                               ((P + 4 <= Len
                                 and then L (P .. P + 4) = "when ")
                                or else
                                  (P + 8 <= Len
                                   and then L (P .. P + 8) = "end case;"));
                        end End_When;

                        I : constant String_Vector.Extended_Index :=
                              String_Vector.To_Index (Pos);
                        P : String_Vector.Extended_Index :=
                              String_Vector.To_Index (Pos);
                        N : Ada.Containers.Count_Type := 0;
                     begin
                        --  The number of line to delete are from Pos to the
                        --  first line starting with a "when".

                        loop
                           N := N + 1;
                           P := P + 1;

                           exit when End_When (Content.Element (P));
                        end loop;

                        Content.Delete (Pos, N);

                        --  Then reset Pos to I (previous Pos index)

                        Pos := Content.To_Cursor (I);
                     end Count_And_Delete;

                  else
                     Check_Vars : declare
                        Assign : constant Natural :=
                                   Fixed.Index (Line, " := ");
                     begin
                        --  Check if line is a variable definition, and if so
                        --  update the value.

                        if Assign > 0 then
                           for V of Project.Variables loop
                              declare
                                 Name : constant String :=
                                          String (V.Name.Text);
                              begin
                                 if Fixed.Index
                                   (Line, ' ' & Name & ' ') in 1 .. Assign - 1
                                 then
                                    Content.Replace_Element
                                      (Pos, "   " & V.Image);
                                 end if;
                              end;
                           end loop;
                        end if;
                     end Check_Vars;
                  end if;
               end;

               String_Vector.Next (Pos);
            end loop Parse_Content;

         else
            --  Project does not exist, or it exists, was not generated by
            --  gprinstall and -f used. In this case it will be overwritten by
            --  a generated project.

            Content.Clear;

            --  Tag project as generated by gprbuild

            Content.Append
              ("--  " & GPRinstall_Tag & ' ' & Version.Long_Value);
            Add_Empty_Line;

            if Project.Qualifier = K_Aggregate_Library then
               for V of Project.Aggregated loop
                  With_External_Imports (V);
               end loop;

               Add_Empty_Line;

            elsif Project.Has_Imports then
               --  Handle with clauses, generate a with clauses only for
               --  project bringing some visibility to sources. No need
               --  for doing this for aggregate projects.

               for L of Project.Imports loop
                  if L.Has_Sources and then Is_Install_Active (L) then
                     Content.Append
                       ("with """ & String (L.Path_Name.Base_Name) & """;");
                  end if;
               end loop;

               With_External_Imports (Project);

               Add_Empty_Line;
            end if;

            --  Project name

            if Project.Is_Library then
               Line := +"library ";
            else
               if Has_Sources (Project) then
                  Line := +"standard ";
               else
                  Line := +"abstract ";
               end if;
            end if;

            Line := Line & "project ";
            Line := Line & String (Project.Name);
            Line := Line & " is";
            Content.Append (-Line);

            if Has_Sources (Project) or else Project.Is_Library then
               --  BUILD variable

               Content.Append
                 ("   type BUILD_KIND is ("""
                  & (-Options.Build_Name) & """);");

               Line := +Get_Build_Line
                 (Vars    => -Options.Build_Vars,
                  Default => -Options.Build_Name);

               Content.Append (-Line);

               --  Add languages, for an aggregate library we want all unique
               --  languages from all aggregated libraries.

               if Has_Sources (Project) then
                  Add_Empty_Line;

                  declare
                     Lang  : Unbounded_String;
                     First : Boolean := True;
                  begin
                     for L of Languages loop
                        if not First then
                           Append (Lang, ", ");
                        end if;

                        Append (Lang, '"' & L & '"');
                        First := False;
                     end loop;

                     Content.Append
                       ("   for Languages use (" & To_String (Lang) & ");");
                  end;
               end if;

               --  Build_Suffix used to avoid .default as suffix

               Add_Empty_Line;

               Content.Append ("   case BUILD is");
               Content.Append_Vector (Data_Attributes);
               Content.Append ("   end case;");

               Add_Empty_Line;

               --  Library Name

               if Project.Is_Library then
                  Content.Append
                    ("   for Library_Name use """
                     & String (Project.Library_Name)
                     & """;");

                  --  Issue the Library_Version only if needed

                  if not Project.Is_Static_Library
                    and then Project.Has_Library_Version
                    and then
                      Project.Library_Filename.Name
                        /= Project.Library_Version_Filename.Name
                  then
                     Content.Append
                       ("   for Library_Version use """
                        & String (Project.Library_Version_Filename.Name)
                        & """;");
                  end if;
               end if;

               --  Packages

               if Has_Sources (Project) then
                  Add_Empty_Line;

                  Create_Packages;
               end if;

               --  Set as not installable

               Add_Empty_Line;

               Content.Append ("   package Install is");
               Content.Append ("      for Active use ""False"";");
               Content.Append ("   end Install;");

               --  Externally Built

               if not Options.Sources_Only then
                  Add_Empty_Line;
                  Content.Append ("   for Externally_Built use ""True"";");
               end if;

            else
               --  This is an abstract project

               Content.Append ("   for Source_Dirs use ();");
            end if;

            --  Variables

            Add_Empty_Line;

            Create_Variables;

            --  Close project

            Content.Append
              ("end " & String (Project.Name) & ";");
         end if;

         --  Write new project if needed

         Write_Project;

         if not Options.Dry_Run and then Options.Install_Manifest then
            --  Add project file to manifest

            Add_To_Manifest (Path_Name.Create_File (Filename_Type (Filename)));
         end if;
      end Create_Project;

      --------------
      -- Dir_Name --
      --------------

      function Dir_Name (Suffix : Boolean := True) return Filename_Type is

         function Get_Suffix return Filename_Optional;
         --  Returns a suffix if needed

         ----------------
         -- Get_Suffix --
         ----------------

         function Get_Suffix return Filename_Optional is
         begin
            --  .default is always omitted from the directory name

            if Suffix and then -Options.Build_Name /= "default" then
               return Filename_Type ('.' & (-Options.Build_Name));
            else
               return No_Filename;
            end if;
         end Get_Suffix;

      begin
         return Project.Path_Name.Base_Filename & Get_Suffix;
      end Dir_Name;

      --------------
      -- Exec_Dir --
      --------------

      function Exec_Dir return Path_Name.Object is
        (Prefix_For_Dir (-Exec_Subdir.V));

      -----------------
      -- Has_Sources --
      -----------------

      function Has_Sources
        (Project : GPR2.Project.View.Object) return Boolean is
      begin
         return Project.Has_Sources
           or else Project.Qualifier = K_Aggregate_Library;
      end Has_Sources;

      -----------------------
      -- Is_Install_Active --
      -----------------------

      function Is_Install_Active
        (Project : GPR2.Project.View.Object) return Boolean is
      begin
         if Project.Has_Package (P.Install) then
            for V of Project.Attributes (Pack => P.Install) loop
               if V.Name.Id = A.Install.Active then
                  return Characters.Handling.To_Lower
                           (V.Value.Text) /= "false";
               end if;
            end loop;
         end if;

         --  If not defined, the default is active

         return True;
      end Is_Install_Active;

      -------------
      -- Lib_Dir --
      -------------

      function Lib_Dir
        (Build_Name : Boolean := True) return Path_Name.Object is
      begin
         return Build_Subdir (Lib_Subdir, Build_Name);
      end Lib_Dir;

      ------------------
      -- Link_Lib_Dir --
      ------------------

      function Link_Lib_Dir return Path_Name.Object is
         (Prefix_For_Dir (-Link_Lib_Subdir.V));

      -------------------------
      -- Open_Check_Manifest --
      -------------------------

      procedure Open_Check_Manifest
        (File         : out Text_IO.File_Type;
         Current_Line : out Text_IO.Count)
      is
         Dir     : constant Path_Name.Object :=
                     Path_Name.Compose (Project_Dir, "manifests");
         M_File  : constant Path_Name.Object :=
                     Path_Name.Create_File
                       (Filename_Type (-Install_Name.V),
                        Filename_Optional (Dir.Value));
         Name    : constant String := String (M_File.Value);
         Prj_Sig : constant String := Project.Path_Name.Content_MD5;
         Buf     : String (1 .. 128);
         Last    : Natural;
      begin
         --  Check whether the manifest does not exist in this case

         if Directories.Exists (Name) then
            --  If this manifest is the same of the current aggregate
            --  one, do not try to reopen it.

            if not Is_Open (Agg_Manifest)
              or else
                OS_Lib.Normalize_Pathname
                  (Text_IO.Name (Agg_Manifest),
                   Case_Sensitive => False)
                /= OS_Lib.Normalize_Pathname (Name, Case_Sensitive => False)
            then
               Open (File, In_File, Name);

               if not End_Of_File (File) then
                  Get_Line (File, Buf, Last);

                  if Last >= Message_Digest'Length
                    and then
                      (Buf (1 .. 2) /= Sig_Line
                       or else Buf (3 .. Message_Digest'Last + 2) /= Prj_Sig)
                      and then Install_Name.Default
                      and then Install_Project
                  then
                     Put_Line
                       ("Project file "
                        & String (Project.Path_Name.Simple_Name)
                        &  " is different from the one currently installed.");
                     Put_Line
                       ("Either:");
                     Put_Line
                       ("   - uninstall first using --uninstall option");
                     Put_Line
                       ("   - install under another name, use --install-name");
                     Put_Line
                       ("   - force installation under the same name, "
                        & "use --install-name=" & (-Install_Name.V));
                     raise GPRinstall_Error_No_Message;
                  end if;
               end if;

               Reset (File, Append_File);

               Current_Line := Line (File);
            end if;

         else
            Directories.Create_Path (Dir.Value);
            Create (File, Out_File, Name);
            Current_Line := 1;

            Put_Line (File, Sig_Line & Prj_Sig);
         end if;

      exception
         when Text_IO.Use_Error =>
            raise GPRinstall_Error with
              "cannot open or create the manifest file "
              & (-Project_Subdir.V) & (-Install_Name.V)
              & ", check permissions on this location";
      end Open_Check_Manifest;

      -----------------
      -- Project_Dir --
      -----------------

      function Project_Dir return Path_Name.Object is
         (Prefix_For_Dir (-Project_Subdir.V));

      ------------------------
      -- Rollback_Manifests --
      ------------------------

      procedure Rollback_Manifests is

         Content : String_Vector.Vector;

         procedure Rollback_Manifest
           (File : in out Text_IO.File_Type; Line : Text_IO.Count);

         -----------------------
         -- Rollback_Manifest --
         -----------------------

         procedure Rollback_Manifest
           (File : in out Text_IO.File_Type; Line : Text_IO.Count)
         is
            use type Ada.Containers.Count_Type;
            Dir    : constant String :=
                       Directories.Containing_Directory (Name (File)) & DS;
            Buffer : String (1 .. 4_096);
            Last   : Natural;
         begin
            --  Set manifest file in Read mode

            Reset (File, Text_IO.In_File);

            while not End_Of_File (File) loop
               Get_Line (File, Buffer, Last);

               if Text_IO.Line (File) = 2
                 or else Text_IO.Line (File) < Line
               then
                  --  Record file to be kept in manifest
                  Content.Append (Buffer (1 .. Last));

               else
                  --  Delete file
                  declare
                     Filename : constant String :=
                                  Dir
                                  & Buffer
                                      (GNAT.MD5.Message_Digest'Length + 2
                                       .. Last);
                     Unused  : Boolean;
                  begin
                     OS_Lib.Delete_File (Filename, Unused);

                     Delete_Empty_Directory
                       (-Prefix_Dir.V,
                        Directories.Containing_Directory (Filename));
                  end;
               end if;
            end loop;

            --  There is nothing left in the manifest file (only the signature
            --  line), remove it, otherwise we create the new manifest file
            --  containing only the previous content.

            if Content.Length = 1 then
               declare
                  Manifest_Filename : constant String := Name (File);
               begin
                  Delete (File);

                  --  Delete manifest directories if empty

                  Delete_Empty_Directory
                    (-Prefix_Dir.V,
                     Directories.Containing_Directory (Manifest_Filename));
               end;

            else
               --  Set manifest file back to Write mode

               Reset (File, Text_IO.Out_File);

               for C of Content loop
                  Text_IO.Put_Line (File, C);
               end loop;

               Close (File);
            end if;
         end Rollback_Manifest;

      begin
         if Is_Open (Man) then
            Rollback_Manifest (Man, Line_Manifest);
         end if;

         if Is_Open (Agg_Manifest) then
            Rollback_Manifest (Agg_Manifest, Line_Agg_Manifest);
         end if;
      end Rollback_Manifests;

      -----------------
      -- Sources_Dir --
      -----------------

      function Sources_Dir
        (Build_Name : Boolean := True) return Path_Name.Object is
      begin
         return Build_Subdir (Sources_Subdir, Build_Name);
      end Sources_Dir;

      Is_Project_To_Install : Boolean;
      --  Whether the project is to be installed

   begin
      --  Empty Content

      Content.Delete_First (Count => Ada.Containers.Count_Type'Last);

      --  First look for the Install package and set up the local values
      --  accordingly.

      Check_Install_Package;

      --  The default install name is the name of the project without
      --  extension.

      if Install_Name.Default then
         Install_Name.V :=
           To_Unbounded_String (String (Project.Path_Name.Base_Name));
      end if;

      --  Skip non-active project, note that externally built project must be
      --  installed.

      Is_Project_To_Install := Active
        and then (Project.Has_Sources
                  or else Project.Has_Attribute (A.Main)
                  or else Project.Is_Externally_Built);

      --  If we have an aggregate project we just install separately all
      --  aggregated projects.

      if Project.Qualifier = K_Aggregate then
         --  If this is the main project and is an aggregate project, create
         --  the corresponding manifest.

         if Project = Tree.Root_Project
           and then Tree.Root_Project.Qualifier = K_Aggregate
           and then Options.Install_Manifest
         then
            Open_Check_Manifest (Agg_Manifest, Line_Agg_Manifest);
         end if;

         for Agg of Project.Aggregated loop
            Process (Tree, Agg, Options);
         end loop;

         --  Nothing more to do for an aggregate project

         return;
      end if;

      if not Installed.Contains (Project) then
         Installed.Insert (Project);

         if Options.Verbosity > Quiet then
            if Is_Project_To_Install then
               Put ("Install");
            elsif Options.Verbose then
               Put ("Skip");
            end if;

            if Is_Project_To_Install or else Options.Verbose then
               Put (" project " & String (Project.Name));
               if -Options.Build_Name /= "default" then
                  Put (" - " & (-Options.Build_Name));
               end if;
            end if;

            if not Is_Project_To_Install and then Options.Verbose then
               Put (" (not active)");
            end if;

            if Is_Project_To_Install or else Options.Verbose then
               New_Line;
            end if;
         end if;

         --  If this is not an active project, just return now

         if not Is_Project_To_Install then
            return;
         end if;

         --  What should be copied ?

         Copy :=
           (Source     => For_Dev,
            Object     => For_Dev
                            and then not Project.Has_Mains
                            and then Project.Qualifier /= K_Library
                            and then Project.Qualifier /= K_Aggregate_Library
                            and then Project.Kind /= K_Library,
            Dependency => For_Dev and then not Project.Has_Mains,
            Library    => Project.Is_Library
                            and then
                              (not Project.Is_Static_Library or else For_Dev),
            Executable => Project.Has_Mains);

         --  Copy all files from the project

         Copy_Files;

         --  A project file is only needed in developer mode

         if For_Dev and then Install_Project then
            Create_Project (Project);
         end if;

         --  Add manifest into the main aggregate project manifest

         if Is_Open (Man) then
            if Is_Open (Agg_Manifest) then
               declare
                  Man_Dir  : constant Path_Name.Object :=
                               Path_Name.Create_Directory
                                 ("manifests",
                                  Filename_Type (Project_Dir.Value));
                  Filename : constant Path_Name.Object :=
                               Path_Name.Create_File
                                 (Filename_Type
                                    (Directories.Simple_Name (Name (Man))),
                                  Filename_Type (Man_Dir.Value));
               begin
                  Close (Man);
                  Add_To_Manifest (Filename, Aggregate_Only => True);
               end;

            else
               Close (Man);
            end if;
         end if;

         --  Handle all projects recursively if needed

         if Options.Recursive and then Project.Has_Imports then
            for P of Project.Imports loop
               Process (Tree, P, Options);
            end loop;
         end if;
      end if;
   end Process;

   procedure Process
     (Tree    : GPR2.Project.Tree.Object;
      Options : GPRinstall.Options.Object) is
   begin
      Process (Tree, Tree.Root_Project, Options);
   end Process;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      Content.Append (New_Item => (Buffer (1 .. Buffer_Last)));
      Buffer_Last := 0;
   end Write_Eol;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : String) is
   begin
      while Buffer_Last + S'Length > Buffer'Last loop
         Double_Buffer;
      end loop;

      Buffer (Buffer_Last + 1 .. Buffer_Last + S'Length) := S;
      Buffer_Last := Buffer_Last + S'Length;
   end Write_Str;

end GPRinstall.Install;
