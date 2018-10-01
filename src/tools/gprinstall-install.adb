------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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
with Ada.Containers.Indefinite_Vectors;
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

with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Name_Values;
with GPR2.Project.Pack;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Variable;
with GPR2.Project.View.Set;
pragma Unreferenced (GPR2.Project.View.Set);
--  Needed as a Vew.Set is used
with GPR2.Project.Source.Set;
pragma Unreferenced (GPR2.Project.Source.Set);
--  Needed as a Vew.Set is used
with GPR2.Version;
with GPR2.Source;

package body Gprinstall.Install is

   use Ada;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GNAT;

   use GPR2;

   use type GNATCOLL.OS.OS_Type;

   package String_Vector is
     new Containers.Indefinite_Vectors (Positive, String);

   package Seen_Set is new Containers.Indefinite_Ordered_Sets (String);

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
      Options : Gprinstall.Options.Object);
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
      Options : Gprinstall.Options.Object)
   is
      use type OS_Lib.String_Access;
      use type GPR2.Path_Name.Object;
      use type GPR2.Project.View.Object;

      package A renames GPR2.Project.Registry.Attribute;
      package P renames GPR2.Project.Registry.Pack;

      subtype Param is Gprinstall.Options.Param;

      function Dup (P : Param) return Param renames Gprinstall.Options.Dup;

      Objcopy_Exec : constant String :=
                       (if Options.Target_Name = null
                        then "objcopy"
                        else Options.Target_Name.all & "-objcopy");
      --  Name of objcopy executable, possible a cross one

      Strip_Exec   : constant String :=
                       (if Options.Target_Name = null
                        then "strip"
                        else Options.Target_Name.all & "-strip");
      --  Name of strip executable, possible a cross one

      Objcopy      : constant OS_Lib.String_Access :=
                       OS_Lib.Locate_Exec_On_Path (Objcopy_Exec);
      Strip        : constant OS_Lib.String_Access :=
                       OS_Lib.Locate_Exec_On_Path (Strip_Exec);

      Windows_Target : constant Boolean :=
                         Tree.Has_Configuration
                             and then
                         Tree.Configuration.Corresponding_View.Has_Attributes
                           (A.Shared_Lib_Suffix)
                             and then
                         Tree.Configuration.Corresponding_View.Attribute
                           (A.Shared_Lib_Suffix).Value_Equal (".dll");
      --  ??? We may also check that the Tree target name constains mingw or
      --  windows.

      --  Local values for the given project, these are initially set with the
      --  default values. It is updated using the Install package found in the
      --  project if any.

      Active          : Boolean := True;
      --  Whether installation is active or not (Install package's attribute)

      Side_Debug      : Boolean := Options.Side_Debug;
      --  Whether to extract debug symbols from executables and shared
      --  libraries. Default to global value.

      Prefix_Dir      : Param   := Dup (Options.Global_Prefix_Dir);
      Exec_Subdir     : Param   := Dup (Options.Global_Exec_Subdir);
      Lib_Subdir      : Param   := Dup (Options.Global_Lib_Subdir);
      ALI_Subdir      : Param   := Dup (Options.Global_ALI_Subdir);
      Link_Lib_Subdir : Param   := Dup (Options.Global_Link_Lib_Subdir);
      Sources_Subdir  : Param   := Dup (Options.Global_Sources_Subdir);
      Project_Subdir  : Param   := Dup (Options.Global_Project_Subdir);
      Install_Mode    : Param   := Dup (Options.Global_Install_Mode);
      Install_Name    : Param   := Dup (Options.Global_Install_Name);
      Install_Project : Boolean := not Options.No_Project;

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
        new Containers.Vectors (Positive, Artifacts_Data);

      Artifacts : Artifacts_Set.Vector;

      Excluded_Naming : Seen_Set.Set;
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
        (From, To       : Path_Name.Object;
         File           : Name_Type;
         From_Ver       : Path_Name.Object  := Path_Name.Undefined;
         Sym_Link       : Boolean := False;
         Executable     : Boolean := False;
         Extract_Debug  : Boolean := False);
      --  Copy file From into To. If From is directory then the source full
      --  filename is composed from From and File parameters.
      --  If Sym_Link is set a symbolic link is created.
      --  If Executable is set, the destination file exec attribute is set.
      --  When Extract_Debug is set to True the debug information for the
      --  executable is written in a side file.

      function Dir_Name (Suffix : Boolean := True) return Name_Type;
      --  Returns the name of directory where project files are to be
      --  installed. This name is the name of the project. If Suffix is
      --  True then the build name is also returned.

      function Sources_Dir
        (Build_Name : Boolean := True) return Path_Name.Object;
      --  Returns the full pathname to the sources destination directory

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
         Aggregate_Only : Boolean := False);
      --  Add filename to manifest

      function Has_Sources (Project : GPR2.Project.View.Object) return Boolean
        with Inline;
      --  Returns True if the project contains sources

      function Bring_Sources
        (Project : GPR2.Project.View.Object) return Boolean;
      --  Returns True if Project gives visibility to some sources directly or
      --  indirectly via the with clauses.

      function Main_Binary (Source : Name_Type) return Name_Type;
      --  Give the source name found in the Main attribute, returns the actual
      --  binary as built by gprbuild. This routine looks into the Builder
      --  switches for a the Executable attribute.

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

      function For_Dev return Boolean is (Install_Mode.V.all = "dev");

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

            Man_Path : constant Path_Name.Object :=
                         Path_Name.Create_File
                           (Name_Type (N (Name (Man))));

            MD5  : constant String := String (Pathname.Content_MD5);
            File : constant String := String (Pathname.Simple_Name);
         begin
            if not Aggregate_Only and then Is_Open (Man) then
               Put_Line
                 (Man,
                  MD5 & ' '
                  & String (Pathname.Relative_Path (To => Man_Path).Name)
                  & File);
            end if;

            if Is_Open (Agg_Manifest) then
               declare
                  Agg_Man_Path : constant Path_Name.Object :=
                                   Path_Name.Create_File
                                     (Name_Type (N (Name (Agg_Manifest))));
               begin
                  Put_Line
                    (Agg_Manifest,
                     MD5 & ' '
                     & String
                         (Pathname.Relative_Path (To => Agg_Man_Path).Name)
                     & File);
               end;
            end if;
         end;
      end Add_To_Manifest;

      -------------
      -- ALI_Dir --
      -------------

      function ALI_Dir
        (Build_Name : Boolean := True) return Path_Name.Object
      is
         Install_Name_Dir : constant Name_Type :=
                              (if Install_Name.Default
                               then "."
                               else Name_Type (Install_Name.V.all));
      begin
         if OS_Lib.Is_Absolute_Path (ALI_Subdir.V.all) then
            return Path_Name.Create_Directory
              (Install_Name_Dir, Optional_Name_Type (ALI_Subdir.V.all));

         elsif not ALI_Subdir.Default or else not Build_Name then
            return Path_Name.Create_Directory
              (Install_Name_Dir,
               Optional_Name_Type (Prefix_Dir.V.all & ALI_Subdir.V.all));

         else
            return Path_Name.Create_Directory
              (Dir_Name,
               Optional_Name_Type
                 (Prefix_Dir.V.all & ALI_Subdir.V.all
                  & String (Install_Name_Dir)));
         end if;
      end ALI_Dir;

      -------------------
      -- Bring_Sources --
      -------------------

      function Bring_Sources
        (Project : GPR2.Project.View.Object) return Boolean is
      begin
         if Project.Has_Sources then
            return True;

         elsif Project.Has_Imports then
            for Import of Project.Imports loop
               if Import.Has_Sources then
                  return True;
               end if;
            end loop;
         end if;

         return False;
      end Bring_Sources;

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
               OS_Lib.Free (P.V);
               P := (new String'
                       ((if Is_Dir
                        then (if Normalize
                              then OS_Lib.Normalize_Pathname (Val)
                              else Val)
                         else Val)),
                     Default => False);
            end if;
         end Replace;

      begin
         if Project.Has_Packages (P.Install) then
            declare
               use Ada.Characters.Handling;

               Pck : constant GPR2.Project.Pack.Object :=
                       Project.Packages.Element (P.Install);
            begin
               for V of Pck.Attributes loop
                  if V.Name = A.Prefix then
                     --  If Install.Prefix is a relative path, it is made
                     --  relative to the global prefix.

                     if OS_Lib.Is_Absolute_Path (V.Value) then
                        if Options.Global_Prefix_Dir.Default then
                           Replace (Prefix_Dir, V.Value, Normalize => True);
                        end if;

                     else
                        Replace
                          (Prefix_Dir,
                           Options.Global_Prefix_Dir.V.all & V.Value,
                           Normalize => True);
                     end if;

                  elsif  V.Name = A.Exec_Subdir
                    and then Options.Global_Exec_Subdir.Default
                  then
                     Replace (Exec_Subdir, V.Value);

                  elsif V.Name = A.Lib_Subdir
                    and then Options.Global_Lib_Subdir.Default
                  then
                     Replace (Lib_Subdir, V.Value);

                  elsif V.Name = A.ALI_Subdir
                    and then Options.Global_ALI_Subdir.Default
                  then
                     Replace (ALI_Subdir, V.Value);

                  elsif V.Name = A.Link_Lib_Subdir
                    and then Options.Global_Link_Lib_Subdir.Default
                  then
                     Replace (Link_Lib_Subdir, V.Value);

                  elsif V.Name = A.Sources_Subdir
                    and then Options.Global_Sources_Subdir.Default
                  then
                     Replace (Sources_Subdir, V.Value);

                  elsif V.Name = A.Project_Subdir
                    and then Options.Global_Project_Subdir.Default
                  then
                     Replace (Project_Subdir, V.Value);

                  elsif V.Name = A.Mode
                    and then Options.Global_Install_Mode.Default
                  then
                     Replace (Install_Mode, V.Value);

                  elsif V.Name = A.Install_Name
                    and then Options.Global_Install_Name.Default
                  then
                     Replace
                       (Install_Name, V.Value, Is_Dir => False);

                  elsif V.Name = A.Active then
                     declare
                        Val : constant String := To_Lower (V.Value);
                     begin
                        if Val = "false" then
                           Active := False;
                        else
                           Active := True;
                        end if;
                     end;

                  elsif V.Name = A.Side_Debug then
                     declare
                        Val : constant String := To_Lower (V.Value);
                     begin
                        if Val = "true" then
                           Side_Debug := True;
                        else
                           Side_Debug := False;
                        end if;
                     end;

                  elsif V.Name = A.Install_Project then
                     declare
                        Val : constant String := To_Lower (V.Value);
                     begin
                        if Val = "false" then
                           Install_Project := False;
                        else
                           Install_Project := True;
                        end if;
                     end;

                  elsif V.Name = A.Artifacts
                    or else V.Name = A.Required_Artifacts
                  then
                     for S of V.Values loop
                        Artifacts.Append
                          (Artifacts_Data'
                             (To_Unbounded_String (V.Index),
                              To_Unbounded_String (S),
                              Required =>
                                (if V.Name = A.Artifacts
                                 then False else True)));
                     end loop;
                  end if;
               end loop;
            end;
         end if;

         --  Now check if Lib_Subdir is set and not ALI_Subdir as in this case
         --  we want ALI_Subdir to be equal to Lib_Subdir.

         if not Lib_Subdir.Default
           and then ALI_Subdir.Default
         then
            ALI_Subdir := Dup (Lib_Subdir);
         end if;
      end Check_Install_Package;

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File
        (From, To       : Path_Name.Object;
         File           : Name_Type;
         From_Ver       : Path_Name.Object  := Path_Name.Undefined;
         Sym_Link       : Boolean := False;
         Executable     : Boolean := False;
         Extract_Debug  : Boolean := False)
      is
         Src_Path      : constant Path_Name.Object :=
                           (if From.Is_Directory then From.Compose (File)
                            else From);
         F             : constant String := String (Src_Path.Value);
         T             : constant String := String (To.Value);
         Dest_Path     : constant Path_Name.Object :=
                           Path_Name.Compose (To, File);
         Dest_Filename : aliased String := Dest_Path.Value;
      begin
         pragma Warnings (Off, "*can never be executed*");

         if Sym_Link and then Is_Windows_Host then
            raise Constraint_Error
              with "internal error: cannot use symbolic links on Windows";
         end if;

         pragma Warnings (On, "*can never be executed*");

         if not Sym_Link
           and then Directories.Exists (Dest_Filename)
           and then not Options.Force_Installations
           and then Src_Path.Content_MD5 /= Dest_Path.Content_MD5
         then
            raise Constraint_Error
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
                     raise Constraint_Error with "cannot overwrite "
                       & Dest_Filename & " check permissions";
                  end if;
               end;
            end if;

            if not Sym_Link and then not Src_Path.Exists then
               raise Constraint_Error with
                 "file " & F & " does not exist, build may not be complete";
            end if;

            if (not Sym_Link and then not To.Exists)
              or else (Sym_Link and then not Src_Path.Exists)
            then
               if Options.Create_Dest_Dir then
                  begin
                     if Sym_Link then
                        Directories.Create_Path (Src_Path.Dir_Name);
                     else
                        Directories.Create_Path (To.Value);
                     end if;
                  exception
                     when Text_IO.Use_Error =>
                        --  Cannot create path, permission issue
                        raise Constraint_Error with
                          "cannot create destination directory "
                          & (if Sym_Link then Src_Path.Dir_Name else T)
                          & " check permissions";
                  end;

               else
                  raise Constraint_Error with
                    "target directory "
                    & T & " does not exist, use -p to create";
               end if;
            end if;

            --  Do copy

            if Sym_Link then
               Src_Path.Create_Sym_Link (To => Dest_Path);

               --  Add file to manifest

               Add_To_Manifest (Src_Path);

               if From_Ver /= Path_Name.Undefined then
                  From_Ver.Create_Sym_Link (To => Dest_Path);
                  Add_To_Manifest (From_Ver);
               end if;

            else
               begin
                  Ada.Directories.Copy_File
                    (Source_Name => F,
                     Target_Name => Dest_Filename,
                     Form        => "preserve=timestamps");
               exception
                  when Text_IO.Use_Error =>
                     raise Constraint_Error with
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
                              Add_To_Manifest
                                (Path_Name.Create_File
                                   (Name_Type (Dest_Debug)));

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

               Add_To_Manifest (Dest_Path);
            end if;
         end if;
      end Copy_File;

      ----------------
      -- Copy_Files --
      ----------------

      procedure Copy_Files is

         procedure Copy_Project_Sources (Project : GPR2.Project.View.Object);
         --  Copy sources from the given project

         procedure Copy_Source (Source : GPR2.Project.Source.Object);

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
                              (Name_Type (Destination.Value),
                               Optional_Name_Type (Prefix_Dir.V.all));
            begin
               if Kind (E) = Directory
                 and then Directories.Simple_Name (E) /= "."
                 and then Directories.Simple_Name (E) /= ".."
               then
                  Copy_Artifacts
                    (Path_Name.Create_File
                       ("*", Optional_Name_Type (Fullname)),
                     Path_Name.Compose
                       (Dest_Dir, Name_Type (Directories.Simple_Name (E))),
                     Required);

               elsif Kind (E) = Ordinary_File then
                  Copy_File
                    (From       =>
                       Path_Name.Create_File (Name_Type (Fullname)),
                     To         => Destination,
                     File       => Name_Type
                                     (Directories.Simple_Name (Fullname)),
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
               raise Constraint_Error with
                 "error: file does not exist '" & Pathname.Value & ''';
            end if;
         exception
            when Text_IO.Name_Error =>
               if Required then
                  Rollback_Manifests;
                  raise Constraint_Error with
                    "warning: file does not exist '" & Pathname.Value & ''';
               else
                  Put_Line
                    ("warning: file does not exist '" & Pathname.Value & ''');
               end if;
         end Copy_Artifacts;

         --------------------------
         -- Copy_Project_Sources --
         --------------------------

         procedure Copy_Project_Sources (Project : GPR2.Project.View.Object) is

            use type GPR2.Source.Object;
            use all type GPR2.Project.Standalone_Library_Kind;
            use all type GPR2.Source.Kind_Type;

            function Is_Ada
              (Source : GPR2.Project.Source.Object) return Boolean with Inline;
            --  Returns True if Source is an Ada source

            function Is_Part_Of_Aggregate_Lib
              (Aggregate_Lib_Project : GPR2.Project.View.Object;
               Source                : GPR2.Project.Source.Object)
               return Boolean;
            --  Returns True if Sid is part of the aggregate lib project. That
            --  is, Source project is one of the aggregated projects.

            ------------
            -- Is_Ada --
            ------------

            function Is_Ada
              (Source : GPR2.Project.Source.Object) return Boolean is
            begin
               return Source.Source.Language = "ada";
            end Is_Ada;

            ------------------------------
            -- Is_Part_Of_Aggregate_Lib --
            ------------------------------

            function Is_Part_Of_Aggregate_Lib
              (Aggregate_Lib_Project : GPR2.Project.View.Object;
               Source                : GPR2.Project.Source.Object)
               return Boolean is
            begin
               for V of Aggregate_Lib_Project.Aggregated loop
                  if V = Source.View then
                     return True;
                  end if;
               end loop;

               return False;
            end Is_Part_Of_Aggregate_Lib;

            Src : GPR2.Source.Object;
            Atf : GPR2.Project.Source.Artifact.Object;

         begin
            for Source of Project.Sources loop
               --  Skip sources that are removed/excluded and sources not
               --  part of the interface for standalone libraries.

               Src := Source.Source;
               Atf := Source.Artifacts;

               if (Project.Qualifier /= K_Aggregate_Library
                   or else (Is_Part_Of_Aggregate_Lib (Project, Source)
                            and then Is_Install_Active (Source.View)))
                 and then (Project.Kind /= K_Library
                           or else Project.Library_Standalone = No
                           or else Source.Is_Interface)
               then
                  if Options.All_Sources or else Source.Is_Interface then
                     Copy_Source (Source);

                  elsif Source.Has_Naming_Exception then
                     --  When a naming exception is present for a body which
                     --  is not installed we must exclude the Naming from the
                     --  generated project.
                     Excluded_Naming.Include (String (Src.Unit_Name));
                  end if;

                  --  Objects / Deps

                  if not Options.Sources_Only
                    and then
                      (not Source.Has_Other_Part
                       or else Src.Kind /= GPR2.Source.S_Spec)
                  then
                     if Copy (Object)
                       and then Src.Kind /= S_Separate
--                       and then Sid.Compilable = Yes
                     then
                        Copy_File
                          (From => Atf.Object_Code,
                           To   => Lib_Dir,
                           File => Atf.Object_Code.Simple_Name);
                     end if;

                     --  Only install Ada .ali files (always name the .ali
                     --  against the spec file).

                     if Copy (Dependency)
                       and then Src.Kind /= S_Separate
                       and then Is_Ada (Source)
                     then
                        declare
                           Proj : GPR2.Project.View.Object;
                           Ssrc : GPR2.Project.Source.Object;
                        begin
                           if not Source.Has_Other_Part
                             or else Source.Has_Naming_Exception
                             or else Options.All_Sources
                           then
                              Ssrc := Source;
                           else
                              Ssrc := Source.Other_Part;
                           end if;

                           if Project.Qualifier = K_Aggregate_Library then
                              Proj := Project;
                           else
                              Proj := Source.View;
                           end if;

                           Copy_File
                             (From => Atf.Dependency,
                              To   => (if Proj.Kind = K_Library
                                       then ALI_Dir
                                       else Lib_Dir),
                              File => Ssrc.Artifacts.Dependency.Simple_Name);
                        end;
                     end if;
                  end if;
               end if;
            end loop;
         end Copy_Project_Sources;

         -----------------
         -- Copy_Source --
         -----------------

         procedure Copy_Source (Source : GPR2.Project.Source.Object) is
         begin
            if Copy (Process.Source)
              and then Is_Install_Active (Source.View)
            then
               declare
                  Art : constant GPR2.Project.Source.Artifact.Object :=
                          Source.Artifacts;
               begin
                  if not Source_Copied.Contains (Source) then
                     Source_Copied.Insert (Source);

                     Copy_File
                       (From => (if Art.Preprocessed_Source.Exists
                                 then Art.Preprocessed_Source
                                 else Source.Source.Path_Name),
                        To   => Sources_Dir,
                        File => Source.Source.Path_Name.Simple_Name);
                  end if;
               end;
            end if;
         end Copy_Source;

      begin
         if Has_Sources (Project) then
            --  Install the project and the extended projects if any

            Copy_Project_Sources (Project);

            if Project.Has_Extended then
               Copy_Project_Sources (Project.Extended);
            end if;
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

               else
                  Copy_File
                    (From          => Project.Library_Version_Filename,
                     To            => Lib_Dir,
                     File          =>
                       Project.Library_Version_Filename.Simple_Name,
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

                  if Project.Has_Library_Version
                    and then
                      Project.Library_Filename
                        /= Project.Library_Version_Filename
                  then
                     if Is_Windows_Host then
                        Copy_File
                          (From       => Lib_Dir,
                           To         => Link_Lib_Dir,
                           File       => Project.Library_Version_Filename.Name,
                           From_Ver   => Path_Name.Compose
                             (Link_Lib_Dir,
                              Project.Library_Major_Version_Filename.Name),
                           Sym_Link   => False);
                     else
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
         end if;

         --  Copy executable(s)

         if Copy (Executable) and then not Options.Sources_Only then
            for Main of Project.Mains loop
               Copy_File
                 (From          => Project.Executable_Directory,
                  To            => Exec_Dir,
                  File          => Main_Binary (Main.Name),
                  Executable    => True,
                  Extract_Debug => Side_Debug);
            end loop;
         end if;

         --  Copy artifacts

         for E of Artifacts loop
            declare
               Destination : constant Name_Type :=
                               Name_Type (To_String (E.Destination));
               Filename    : constant Name_Type :=
                               Name_Type (To_String (E.Filename));
            begin
               Copy_Artifacts
                 (Path_Name.Compose (Project.Path_Name, Filename),
                  Path_Name.Create_Directory
                    (Destination,
                     Optional_Name_Type (Prefix_Dir.V.all)),
                  E.Required);
            end;
         end loop;
      end Copy_Files;

      --------------------
      -- Create_Project --
      --------------------

      procedure Create_Project (Project : GPR2.Project.View.Object) is

         package Lang_Set is new Containers.Indefinite_Ordered_Sets
           (String,
            Strings.Less_Case_Insensitive, Strings.Equal_Case_Insensitive);

         Filename : constant String :=
                      Project_Dir.Value
                      & String (Project.Path_Name.Base_Name) & ".gpr";

         Gprinstall_Tag : constant String :=
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

               Content.Append (Linker_Case_Alternative (Project));

               Content.Append ("      end case;");
               Content.Append ("   end Linker;");
               Add_Empty_Line;
            end Create_Linker;

            -------------------
            -- Create_Naming --
            -------------------

            procedure Create_Naming (Project : GPR2.Project.View.Object) is
            begin
               Content.Append ("   package Naming is");

               for A of Project.Naming_Package.Attributes loop
                  if not A.Has_Index then
                     Content.Append ("      " & A.Image);
                  end if;
               end loop;

               Content.Append ("      case BUILD is");

               Content.Append (Naming_Case_Alternative (Project));

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
         begin
            --  Output types if any

            if Project.Has_Types then
               for Typ of Project.Types loop
                  Write_Str (Typ.Image);
                  Write_Eol;
               end loop;
            end if;

            if Project.Has_Variables then
               --  Compute variable max length
               for Var of Project.Variables loop
                  Max_Len := Natural'Max (Max_Len, Var.Name'Length);
               end loop;

               --  Finally output variables

               for Var of Project.Variables loop
                  Write_Str (Var.Image (Max_Len));
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

                  if Options.Build_Name.all /= "default" then
                     Line := Line & "." & Options.Build_Name.all;
                  end if;
               end if;
            end Gen_Dir_Name;

            V    : String_Vector.Vector;
            Line : Unbounded_String;
         begin
            V.Append ("      when """ & Options.Build_Name.all & """ =>");

            --  Project sources

            Line := +"         for Source_Dirs use (""";

            if Has_Sources (Project) then
               Line := Line
                 & String (Sources_Dir (Build_Name => False).Relative_Path
                           (To => Project_Dir).Name);

               Gen_Dir_Name (Sources_Subdir, Line);
            end if;

            Line := Line & """);";

            V.Append (-Line);

            --  Project objects and/or library

            if Project.Kind = K_Library then
               Line := +"         for Library_Dir use """;
            else
               Line := +"         for Object_Dir use """;
            end if;

            Line := Line
              & String (Lib_Dir (Build_Name => False).Relative_Path
                        (To => Project_Dir).Name);

            Gen_Dir_Name (Lib_Subdir, Line);
            Line := Line & """;";

            V.Append (-Line);

            if Project.Kind = K_Library then
               --  If ALI are in a different location, set the corresponding
               --  attribute.

               if Lib_Dir /= ALI_Dir then
                  Line := +"         for Library_ALI_Dir use """;

                  Line := Line
                    & String (ALI_Dir (Build_Name => False).Relative_Path
                              (To => Project_Dir).Name);

                  Gen_Dir_Name (ALI_Subdir, Line);
                  Line := Line & """;";
                  V.Append (-Line);
               end if;

               Line := +"         for Library_Kind use """;
               Line := Line & String (Project.Library_Kind);
               Line := Line & """;";
               V.Append (-Line);

               if Project.Has_Attributes (A.Standalone_Library)
                 and then
                   Project.Attribute (A.Standalone_Library).Value /= "no"
               then
                  if not Project.Is_Static_Library then
                     Line := +"         for Library_Standalone use """;
                     Line := Line & Characters.Handling.To_Lower
                       (Project.Attribute (A.Standalone_Library).Value);
                     Line := Line & """;";
                     V.Append (-Line);
                  end if;

                  --  And then generates the interfaces

                  Line := +"         for Library_Interface use (";

                  declare
                     use all type GPR2.Project.View.Source_Kind;

                     First : Boolean := True;

                  begin
                     for Source
                       of Project.Sources (Filter => K_Interface_Only)
                     loop
                        if Source.Source.Has_Unit then
                           if not First then
                              Append (Line, ", ");
                           else
                              First := False;
                           end if;

                           Append (Line, """");
                           Append (Line, String (Source.Source.Unit_Name));
                           Append (Line, """");
                        end if;
                     end loop;
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
            use Ada.Characters.Handling;
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
                  Line := Line
                    & To_Upper (String (Dir_Name (Suffix => False)));
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
               package A renames GPR2.Project.Registry.Attribute;
               package P renames GPR2.Project.Registry.Pack;
            begin
               for Lang of Project.Languages loop
                  if Project.Tree.Has_Configuration then
                     declare
                        C : constant GPR2.Project.View.Object :=
                              Project.Tree.Configuration.Corresponding_View;
                     begin
                        if C.Has_Packages (P.Compiler) then
                           declare
                              Compiler : constant GPR2.Project.Pack.Object :=
                                           C.Packages.Element (P.Compiler);
                           begin
                              if Compiler.Has_Attributes (A.Driver, Lang)
                                and then
                                 Compiler.Attribute
                                   (A.Driver, Lang).Value /= ""
                              then
                                 Langs.Include (Lang);
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end loop;
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

            procedure Linker_For (Pck : GPR2.Project.Pack.Object);
            --  Handle the linker options for this package

            procedure Append (Attribute : GPR2.Project.Attribute.Object);
            --  Add values if any

            procedure Add_Library_Options (Proj : GPR2.Project.View.Object);
            --  For a library project, add the Library_Options

            Seen : Seen_Set.Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            R    : String_Vector.Vector;
            Opts : String_Vector.Vector;

            -------------------------
            -- Add_Library_Options --
            -------------------------

            procedure Add_Library_Options (Proj : GPR2.Project.View.Object) is
            begin
               if Proj.Kind = K_Library then
                  if Proj.Has_Attributes (A.Library_Options) then
                     Append (Proj.Attribute (A.Library_Options));
                  end if;
               end if;
            end Add_Library_Options;

            ------------
            -- Append --
            ------------

            procedure Append (Attribute : GPR2.Project.Attribute.Object) is
            begin
               for V of Attribute.Values loop
                  Opts.Append (V);
                  Seen.Include (V);
               end loop;
            end Append;

            ----------------
            -- Linker_For --
            ----------------

            procedure Linker_For (Pck : GPR2.Project.Pack.Object) is
            begin
               if Pck.Has_Attributes (A.Linker_Options) then
                  Append (Pck.Attributes.Element (A.Linker_Options));
               end if;
            end Linker_For;

         begin
            R.Append ("         when """ & Options.Build_Name.all & """ =>");

            if Project.Has_Packages (P.Linker) then
               Linker_For (Project.Packages.Element (P.Linker));
            end if;

            --  For libraries we want to add the library options here

            Add_Library_Options (Proj);

            if Proj.Qualifier = K_Aggregate_Library then
               for Aggregated of Proj.Aggregated loop
                  if Aggregated.Has_Packages (P.Linker) then
                     Linker_For (Aggregated.Packages.Element (P.Linker));
                  end if;

                  Add_Library_Options (Aggregated);
               end loop;
            end if;

            --  We also want to add the externally built libraries without
            --  sources (referencing system libraries for example).

            if Project.Has_Imports then
               for L of Project.Imports (Recursive => True) loop
                  if L.Kind = K_Library
                    and then L.Is_Externally_Built
                    and then not Bring_Sources (L)
                  then
                     Opts.Append ("-l" & String (L.Library_Name));
                  end if;
               end loop;
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
            procedure Naming_For (Pck : GPR2.Project.Pack.Object);
            --  Handle the naming scheme for this package

            Seen : Seen_Set.Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            V    : String_Vector.Vector;
            --  Contains the final result returned

            function Is_Language_Active (Lang : String) return Boolean
              is (Languages.Contains (Characters.Handling.To_Lower (Lang)));
            --  Returns True if Lang is active in the installed project

            ----------------
            -- Naming_For --
            ----------------

            procedure Naming_For (Pck : GPR2.Project.Pack.Object) is
            begin
               --  Check all associative attributes

               for Att of Pck.Attributes loop
                  if Att.Has_Index then
                     if (Att.Name /= A.Body_N
                         or else not Excluded_Naming.Contains (Att.Index))
                       and then
                         ((Att.Name /= A.Spec_Suffix
                           and then Att.Name /= A.Body_Suffix
                           and then Att.Name /= A.Separate_Suffix)
                          or else Is_Language_Active (Att.Index))
                     then
                        declare
                           Decl : constant String := Att.Image;
                        begin
                           if not Seen.Contains (Decl) then
                              V.Append ("            " & Decl);
                              Seen.Include (Decl);
                           end if;
                        end;
                     end if;
                  end if;
               end loop;
            end Naming_For;

         begin
            V.Append ("         when """ & Options.Build_Name.all & """ =>");

            Naming_For (Project.Naming_Package);

            if Project.Qualifier = K_Aggregate_Library then
               for Agg of Project.Aggregated loop
                  Naming_For (Agg.Naming_Package);
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
                 (String_Vector.Element (Pos), Gprinstall_Tag) /= 0
               then
                  Generated := True;
                  exit Check_Generated_Status;
               end if;

               String_Vector.Next (Pos);
            end loop Check_Generated_Status;

            if not Generated and then not Options.Force_Installations then
               raise Constraint_Error with
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
            --  and Linker package.

            Parse_Content : while String_Vector.Has_Element (Pos) loop
               declare
                  use Ada.Strings;

                  BN   : constant String := Options.Build_Name.all;
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
                           raise Constraint_Error with
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
                             ((if Options.Build_Vars = null
                              then ""
                              else Options.Build_Vars.all), -Default));
                     end;

                  elsif Fixed.Index (Line, "package Naming is") /= 0 then
                     Current_Section := Naming;

                  elsif Fixed.Index (Line, "package Linker is") /= 0 then
                     Current_Section := Linker;

                  elsif Fixed.Index (Line, "case BUILD is") /= 0 then

                     --  Add new case section for the new build name

                     case Current_Section is
                        when Naming =>
                           String_Vector.Next (Pos);
                           Content.Insert
                             (Pos, Naming_Case_Alternative (Project));

                        when Linker =>
                           String_Vector.Next (Pos);
                           Content.Insert
                             (Pos, Linker_Case_Alternative (Project));

                        when Top =>
                           --  For the Sources/Lib attributes
                           String_Vector.Next (Pos);
                           Content.Insert (Pos, Data_Attributes);
                     end case;

                  elsif Fixed.Index (Line, "when """ & BN & """ =>") /= 0 then
                     --  Found a when with the current build name, this is a
                     --  previous install overwritten by this one. Remove this
                     --  section. Note that this removes sections from all
                     --  packages Naming and Linker, and from project level
                     --  case alternative.

                     Count_And_Delete : declare

                        use type Containers.Count_Type;

                        function End_When (L : String) return Boolean;
                        --  Return True if L is the end of a when alternative

                        --------------
                        -- End_When --
                        --------------

                        function End_When (L : String) return Boolean is
                           P   : constant Natural :=
                                   Strings.Fixed.Index_Non_Blank (L);
                           Len : constant Natural := L'Length;
                        begin
                           return P > 0
                             and then
                               ((P + 4 <= Len
                                 and then L (P .. P + 4) = "when ")
                                or else
                                  (P + 8 <= Len
                                   and then L (P .. P + 8) = "end case;"));
                        end End_When;

                        N : Containers.Count_Type := 0;
                        P : String_Vector.Cursor := Pos;
                     begin
                        --  The number of line to delete are from Pos to the
                        --  first line starting with a "when".

                        loop
                           String_Vector.Next (P);
                           N := N + 1;

                           exit when End_When (String_Vector.Element (P));
                        end loop;

                        Content.Delete (Pos, N);
                     end Count_And_Delete;
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
              ("--  " & Gprinstall_Tag & ' ' & Version.Long_Value);
            Add_Empty_Line;

            if Project.Has_Imports then
               --  Handle with clauses, generate a with clauses only for
               --  project bringing some visibility to sources. No need
               --  for doing this for aggregate projects.

               if Project.Qualifier /= K_Aggregate_Library then
                  for L of Project.Imports loop
                     if L.Has_Sources and then Is_Install_Active (L) then
                        Content.Append
                          ("with """ & String (L.Path_Name.Base_Name) & """;");
                     end if;
                  end loop;
               end if;

               --  In all cases adds externally built projects

               for L of Project.Imports (Recursive => True) loop
                  if L.Has_Sources and then L.Is_Externally_Built then
                     Content.Append
                       ("with """ & String (L.Path_Name.Base_Name) & """;");
                  end if;
               end loop;

               Add_Empty_Line;
            end if;

            --  Project name

            if Project.Kind = K_Library then
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

            if Has_Sources (Project) or else Project.Kind = K_Library then
               --  BUILD variable

               Content.Append
                 ("   type BUILD_KIND is ("""
                  & Options.Build_Name.all & """);");

               Line := +Get_Build_Line
                 (Vars    =>
                    (if Options.Build_Vars = null
                     then ""
                     else Options.Build_Vars.all),
                  Default => Options.Build_Name.all);

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
               Content.Append (Data_Attributes);
               Content.Append ("   end case;");

               Add_Empty_Line;

               --  Library Name

               if Project.Kind = K_Library then
                  Content.Append
                    ("   for Library_Name use """
                     & String (Project.Library_Name)
                     & """;");

                  --  Issue the Library_Version only if needed

                  if not Project.Is_Static_Library
                    and then Project.Has_Library_Version
                    and then
                      Project.Library_Filename
                        /= Project.Library_Version_Filename
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

         if not Options.Dry_Run then
            --  Add project file to manifest

            Add_To_Manifest (Path_Name.Create_File (Name_Type (Filename)));
         end if;
      end Create_Project;

      --------------
      -- Dir_Name --
      --------------

      function Dir_Name (Suffix : Boolean := True) return Name_Type is

         function Get_Suffix return String;
         --  Returns a suffix if needed

         ----------------
         -- Get_Suffix --
         ----------------

         function Get_Suffix return String is
         begin
            --  .default is always omitted from the directory name

            if Suffix and then Options.Build_Name.all /= "default" then
               return '.' & Options.Build_Name.all;
            else
               return "";
            end if;
         end Get_Suffix;

      begin
         return Name_Type (String (Project.Path_Name.Base_Name) & Get_Suffix);
      end Dir_Name;

      --------------
      -- Exec_Dir --
      --------------

      function Exec_Dir return Path_Name.Object is
         E_Dir : constant Name_Type := Name_Type (Exec_Subdir.V.all);
      begin
         if OS_Lib.Is_Absolute_Path (Exec_Subdir.V.all) then
            return Path_Name.Create_Directory (E_Dir);
         else
            return Path_Name.Create_Directory
              (E_Dir, Optional_Name_Type (Prefix_Dir.V.all));
         end if;
      end Exec_Dir;

      -----------------
      -- Has_Sources --
      -----------------

      function Has_Sources
        (Project : GPR2.Project.View.Object) return Boolean
      is
         use type Containers.Count_Type;
      begin
         return (Project.Has_Attributes (A.Source_Dirs)
                 and then Project.Attribute (A.Source_Dirs).Count_Values > 0)
           or else Project.Qualifier = K_Aggregate_Library;
      end Has_Sources;

      -----------------------
      -- Is_Install_Active --
      -----------------------

      function Is_Install_Active
        (Project : GPR2.Project.View.Object) return Boolean is
      begin
         if Project.Has_Packages (P.Install) then
            for V of Project.Pack (P.Install).Attributes loop
               if V.Name = A.Active then
                  declare
                     Val : constant String :=
                             Characters.Handling.To_Lower (V.Value);
                  begin
                     if Val = "false" then
                        return False;
                     else
                        return True;
                     end if;
                  end;
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
        (Build_Name : Boolean := True) return Path_Name.Object
      is
         Install_Name_Dir : constant Name_Type :=
                              (if Install_Name.Default
                               then "."
                               else Name_Type (Install_Name.V.all));
      begin
         if OS_Lib.Is_Absolute_Path (Lib_Subdir.V.all) then
            return Path_Name.Create_Directory
              (Install_Name_Dir, Optional_Name_Type (Lib_Subdir.V.all));

         elsif not Lib_Subdir.Default or else not Build_Name then
            return Path_Name.Create_Directory
              (Install_Name_Dir,
               Optional_Name_Type (Prefix_Dir.V.all & Lib_Subdir.V.all));

         else
            return Path_Name.Create_Directory
              (Dir_Name,
               Optional_Name_Type
                 (Prefix_Dir.V.all & Lib_Subdir.V.all
                  & String (Install_Name_Dir)));
         end if;
      end Lib_Dir;

      ------------------
      -- Link_Lib_Dir --
      ------------------

      function Link_Lib_Dir return Path_Name.Object is
         L_Dir : constant Name_Type := Name_Type (Link_Lib_Subdir.V.all);
      begin
         if OS_Lib.Is_Absolute_Path (Link_Lib_Subdir.V.all) then
            return Path_Name.Create_Directory (L_Dir);
         else
            return Path_Name.Create_Directory
              (L_Dir, Optional_Name_Type (Prefix_Dir.V.all));
         end if;
      end Link_Lib_Dir;

      -----------------
      -- Main_Binary --
      -----------------

      function Main_Binary (Source : Name_Type) return Name_Type is

         use type GPR2.Project.Attribute.Object;

         function Get_Exec_Suffix return String;
         --  Return the target executable suffix

         ---------------------
         -- Get_Exec_Suffix --
         ---------------------

         function Get_Exec_Suffix return String is
         begin
            if Tree.Has_Configuration
              and then
                Tree.Configuration.Corresponding_View.Has_Attributes
                  (A.Executable_Suffix)
            then
               return Tree.Configuration.Corresponding_View.Attribute
                 (A.Executable_Suffix).Value;
            else
               return "";
            end if;
         end Get_Exec_Suffix;

         Attr : GPR2.Project.Attribute.Object;

      begin
         if Project.Has_Packages (P.Builder) then
            declare
               Pck : constant GPR2.Project.Pack.Object :=
                       Project.Packages.Element (P.Builder);
            begin
               if Pck.Has_Attributes (A.Executable, Value_Type (Source)) then
                  Attr := Pck.Attribute (A.Executable, Value_Type (Source));

               else
                  --  Not found but an extension is present, check without

                  declare
                     BN : constant Value_Type :=
                            Value_Type
                              (Directories.Base_Name (String (Source)));
                  begin
                     if Source /= Name_Type (BN)
                       and then Pck.Has_Attributes (A.Executable, BN)
                     then
                        Attr := Pck.Attribute (A.Executable, BN);
                     end if;
                  end;
               end if;
            end;
         end if;

         if Attr = GPR2.Project.Attribute.Undefined then
            return Name_Type
              (Directories.Base_Name (String (Source)) & Get_Exec_Suffix);
         else
            return Name_Type (Attr.Value & Get_Exec_Suffix);
         end if;
      end Main_Binary;

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
                       (Name_Type (Install_Name.V.all),
                        Optional_Name_Type (Dir.Value));
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
                       ("Project already installed, either:");
                     Put_Line
                       ("   - uninstall first using --uninstall option");
                     Put_Line
                       ("   - install under another name, use --install-name");
                     Put_Line
                       ("   - force installation under the same name, "
                        & "use --install-name=" & Install_Name.V.all);
                     raise Constraint_Error;
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
            raise Constraint_Error with
              "cannot open or create the manifest file "
              & Project_Subdir.V.all & Install_Name.V.all
              & ", check permissions on this location";
      end Open_Check_Manifest;

      -----------------
      -- Project_Dir --
      -----------------

      function Project_Dir return Path_Name.Object is
         P_Dir : constant Name_Type := Name_Type (Project_Subdir.V.all);
      begin
         if OS_Lib.Is_Absolute_Path (Project_Subdir.V.all) then
            return Path_Name.Create_Directory (P_Dir);
         else
            return Path_Name.Create_Directory
              (P_Dir, Optional_Name_Type (Prefix_Dir.V.all));
         end if;
      end Project_Dir;

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
                  begin
                     Ada.Directories.Delete_File (Filename);

                     Delete_Empty_Directory
                       (Prefix_Dir.V.all,
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
                    (Prefix_Dir.V.all,
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
        (Build_Name : Boolean := True) return Path_Name.Object
      is
         Install_Name_Dir : constant Name_Type :=
                              (if Install_Name.Default
                               then "."
                               else Name_Type (Install_Name.V.all));
      begin
         if OS_Lib.Is_Absolute_Path (Sources_Subdir.V.all) then
            return Path_Name.Create_Directory
              (Install_Name_Dir,
               Optional_Name_Type (Sources_Subdir.V.all));

         elsif not Sources_Subdir.Default or else not Build_Name then
            return Path_Name.Create_Directory
              (Install_Name_Dir,
               Optional_Name_Type (Prefix_Dir.V.all & Sources_Subdir.V.all));

         else
            return Path_Name.Create_Directory
              (Dir_Name,
               Optional_Name_Type (Prefix_Dir.V.all & Sources_Subdir.V.all
                 & String (Install_Name_Dir)));
         end if;
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
         Install_Name.V := new String'(String (Project.Path_Name.Base_Name));
      end if;

      --  Skip non active project and externally built ones

      Is_Project_To_Install := Active
        and then (Bring_Sources (Project)
                  or else Project.Is_Externally_Built);

      --  If we have an aggregate project we just install separately all
      --  aggregated projects.

      if Project.Qualifier = K_Aggregate then
         --  If this is the main project and is an aggregate project, create
         --  the corresponding manifest.

         if Project = Tree.Root_Project
           and then Tree.Root_Project.Qualifier = K_Aggregate
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

         if not Options.Quiet then
            if Is_Project_To_Install then
               Put ("Install");
            elsif Options.Verbose then
               Put ("Skip");
            end if;

            if Is_Project_To_Install or else Options.Verbose then
               Put (" project " & String (Project.Name));
               if Options.Build_Name.all /= "default" then
                  Put (" - " & Options.Build_Name.all);
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
            Library    => Project.Kind = K_Library
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
                                  Optional_Name_Type (Project_Dir.Value));
                  Filename : constant Path_Name.Object :=
                               Path_Name.Create_File
                                 (Name_Type
                                    (Directories.Simple_Name (Name (Man))),
                                  Optional_Name_Type (Man_Dir.Value));
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

      Gprinstall.Options.Free (Prefix_Dir);
      Gprinstall.Options.Free (Sources_Subdir);
      Gprinstall.Options.Free (Lib_Subdir);
      Gprinstall.Options.Free (Exec_Subdir);
      Gprinstall.Options.Free (Project_Subdir);
   end Process;

   procedure Process
     (Tree    : GPR2.Project.Tree.Object;
      Options : Gprinstall.Options.Object) is
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

end Gprinstall.Install;
