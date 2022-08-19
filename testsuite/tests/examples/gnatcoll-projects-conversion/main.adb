--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Strings;
with GNATCOLL.VFS;
with GPR2.Containers;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2_GNATCOLL_Projects;
with GPR2.Project.View;
with Pck1;
pragma Unreferenced (Pck1);
with GNATCOLL.Projects;
with Ada.Strings.Unbounded;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   Project_Tree : GNATCOLL.Projects.Project_Tree;

   procedure Print_Messages
     (Output_Warnings     : Boolean := True;
      Output_Informations : Boolean := False);

   ----------
   -- Load --
   ----------

   procedure Load (Project_Name      : GPR2.Filename_Type;
                   Subdirs           : GPR2.Optional_Name_Type := No_Name;
                   Language_Runtimes : GPR2.Containers.Lang_Value_Map :=
                     GPR2.Containers.Lang_Value_Maps.Empty_Map);

   procedure Load (Project_Name      : GPR2.Filename_Type;
                   Subdirs           : GPR2.Optional_Name_Type := No_Name;
                   Language_Runtimes : GPR2.Containers.Lang_Value_Map :=
                     GPR2.Containers.Lang_Value_Maps.Empty_Map)
   is
   begin
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename          => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context           => Context,
         Subdirs           => Subdirs,
         Language_Runtimes => Language_Runtimes);

   exception
      when Project_Error =>
         Print_Messages;
   end Load;

   --------------------
   -- Print_Messages --
   --------------------

   procedure Print_Messages
     (Output_Warnings     : Boolean := True;
      Output_Informations : Boolean := False)
   is
   begin
      if Tree.Has_Messages then
         GPR2_GNATCOLL_Projects.Output_Messages
           (Tree.Log_Messages.all, Output_Warnings, Output_Informations);
      end if;
   end Print_Messages;

   ------------------------
   -- Register_Attribute --
   ------------------------

   procedure Register_Attribute
     (Name                 : String;
      Pkg                  : String;
      Is_List              : Boolean;
      Indexed              : Boolean;
      Case_Sensitive_Index : Boolean);

   procedure Register_Attribute
     (Name                 : String;
      Pkg                  : String;
      Is_List              : Boolean;
      Indexed              : Boolean;
      Case_Sensitive_Index : Boolean)
   is
      Status : constant String :=
                 GPR2_GNATCOLL_Projects.Register_New_Attribute
                   (Name                 => (+(GPR2.Optional_Name_Type (Pkg)),
                                             +(GPR2.Optional_Name_Type (Name))),
                    Is_List              => Is_List,
                    Indexed              => Indexed,
                    Case_Sensitive_Index => Case_Sensitive_Index);
   begin
      if Status'Length > 0 then
         Ada.Text_IO.Put_Line (Status);
      end if;
   end Register_Attribute;

   -------------
   -- Strings --
   -------------

   function Strings (Strings : GNAT.Strings.String_List_Access) return String;

   function Strings (Strings : GNAT.Strings.String_List_Access) return String
   is
      First : Boolean := True;
      use GNAT.Strings;
      use Ada.Strings.Unbounded;
      US    : Unbounded_String;
   begin
      if Strings /= null then
         for S of Strings.all loop
            if First then
               First := False;
            else
               US := US & ";";
            end if;
            US := US & S.all;
         end loop;
      else
         US := US & "<null>";
      end if;
      return To_String (US);
   end Strings;

   ------------------------
   -- Test_Actifacts_Dir --
   ------------------------

   procedure Test_Artifacts_Dir;

   procedure Test_Artifacts_Dir is

      ------------------------
      -- Test_Actifacts_Dir --
      ------------------------

      procedure Test_Actifacts_Dir
        (Prefix : String; View   : GPR2.Project.View.Object);

      procedure Test_Actifacts_Dir
        (Prefix : String; View   : GPR2.Project.View.Object)
      is
         Dir : constant GNATCOLL.VFS.Filesystem_String :=
                 GPR2_GNATCOLL_Projects.Artifacts_Dir (View).Full_Name;
      begin
         Ada.Text_IO.Put_Line (Prefix & ":<" & String (Dir) & ">");
      end Test_Actifacts_Dir;

   begin
      Ada.Text_IO.Put_Line ("=== testing Artifacts_Dir ===");
      Load ("test.gpr");
      Test_Actifacts_Dir ("Undefined", GPR2.Project.View.Undefined);
      Test_Actifacts_Dir ("Root", Tree.Root_Project);
      Test_Actifacts_Dir ("Abstract",
                          Tree.View_For ("prj0", GPR2.Context.Root));
      Load ("test.gpr", "debug_subdirs");
      Test_Actifacts_Dir ("Root", Tree.Root_Project);
   end Test_Artifacts_Dir;

   --------------------
   -- Test_Attribute --
   --------------------

   procedure Test_Attribute
     (View         : GPR2.Project.View.Object;
      Pack         : String := "";
      Name         : String := "";
      Index        : String := "";
      Use_Extended : Boolean := False);

   procedure Test_Attribute
     (View         : GPR2.Project.View.Object;
      Pack         : String := "";
      Name         : String := "";
      Index        : String := "";
      Use_Extended : Boolean := False)
   is
      Value : constant String :=
                GPR2_GNATCOLL_Projects.Attribute_Value
                  (Project        => View,
                   Name           => (+(GPR2.Optional_Name_Type (Pack)),
                                      +(GPR2.Optional_Name_Type (Name))),
                   Index          => Index,
                   Default        => "test default value",
                   Use_Extended   => Use_Extended);

      Ref1 : constant String :=
               GNATCOLL.Projects.Attribute_Value
                 (Project      => Project_Tree.Root_Project,
                  Attribute    => GNATCOLL.Projects.Build (Pack, Name),
                  Index        => Index,
                  Default      => "test default value",
                  Use_Extended => Use_Extended);

      Values : constant String :=
                 Strings (GPR2_GNATCOLL_Projects.Attribute_Value
                          (Project        => View,
                           Name           => (+(GPR2.Optional_Name_Type (Pack)),
                                              +(GPR2.Optional_Name_Type (Name))),
                           Index          => Index,
                           Use_Extended   => Use_Extended));

      Ref2 : constant String :=
               Strings (GNATCOLL.Projects.Attribute_Value
                        (Project      => Project_Tree.Root_Project,
                         Attribute    => GNATCOLL.Projects.Build (Pack, Name),
                         Index        => Index,
                         Use_Extended => Use_Extended));

      ------------------
      -- Print_Prefix --
      ------------------

      procedure Print_Prefix;

      procedure Print_Prefix is
      begin
         if Pack'Length > 0 then
            Ada.Text_IO.Put ("attribute " & Pack & "'");
         end if;
         Ada.Text_IO.Put (Name);
         if Index'Length > 0 then
            Ada.Text_IO.Put (" (""" & Index & """)");
         end if;
         if Use_Extended then
            Ada.Text_IO.Put (" (Use_Extended)");
         end if;
      end Print_Prefix;

   begin
      if Value /= Ref1 then
         Print_Prefix;
         Ada.Text_IO.Put_Line
           ("=<actual:" & Value & ", expected:" & Ref1 & ">");
      end if;
      if Values /= Ref2 then
         Print_Prefix;
         Ada.Text_IO.Put_Line
           ("=(actual:(" & Values & "), expected:(" & Ref2 & "))");
      end if;
   end Test_Attribute;

   procedure Test_Attribute;

   procedure Test_Attribute is

      -------------------
      -- Test_Extended --
      -------------------

      procedure Test_Extended (Pack : String);

      procedure Test_Extended (Pack : String) is
      begin
         Test_Attribute (View  => Tree.Root_Project,
                         Pack  => Pack,
                         Name  => "Switches",
                         Index => "Ada");
         Test_Attribute (View         => Tree.Root_Project,
                         Pack         => Pack,
                         Name         => "Switches",
                         Index        => "Ada",
                         Use_Extended => True);
         Test_Attribute (View  => Tree.Root_Project,
                         Pack  => Pack,
                         Name  => "Default_Switches",
                         Index => "Ada");
         Test_Attribute (View         => Tree.Root_Project,
                         Pack         => Pack,
                         Name         => "Default_Switches",
                         Index        => "Ada",
                         Use_Extended => True);
      end Test_Extended;
   begin
      Ada.Text_IO.Put_Line ("--- testing Attribute_Value ---");
      Load ("files/root.gpr");

      declare
         use GNATCOLL.VFS;
      begin
         GNATCOLL.Projects.Load
           (Self              => Project_Tree,
            Root_Project_Path => Create (+"files/root.gpr"));
      end;

      Test_Attribute (View  => GPR2.Project.View.Undefined,
                      Pack  => "",
                      Name  => "Switches",
                      Index => "Ada");
      Test_Attribute (View  => GPR2.Project.View.Undefined,
                      Pack  => "Compiler",
                      Name  => "Switches",
                      Index => "Ada");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Object_Dir",
                      Index => "");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Default_Language",
                      Index => "");

      --  Check different kind of attributes (pkg,indexed,single)

      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "Install",
                      Name  => "Install_Project",
                      Index => "");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "Clean",
                      Name  => "Switches",
                      Index => "");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "Ide",
                      Name  => "Compiler_Command",
                      Index => "Ada");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "Ide",
                      Name  => "Default_Switches",
                      Index => "Ada");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Excluded_Source_Dirs",
                      Index => "");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Source_List_File",
                      Index => "");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Object_Generated",
                      Index => "Ada");
      Test_Attribute (View  => Tree.Root_Project,
                      Pack  => "",
                      Name  => "Roots",
                      Index => "Ada");

      Test_Extended ("Binder");
      Test_Extended ("Linker");
      Test_Extended ("Metrics");
      Test_Extended ("Check");
      Test_Extended ("GNATstub");
      Test_Extended ("Pretty_Printer");
      Test_Extended ("Finder");
      Test_Extended ("Cross_Reference");
      Test_Extended ("Eliminate");
      Test_Extended ("Not Defined Package");
   end Test_Attribute;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create;

   procedure Test_Create is

      -----------------
      -- Test_Create --
      -----------------

      procedure Test_Create
        (Name            : GNATCOLL.VFS.Filesystem_String;
         Project         : GPR2.Project.View.Object;
         Use_Source_Path : Boolean;
         Use_Object_Path : Boolean);

      procedure Test_Create
        (Name            : GNATCOLL.VFS.Filesystem_String;
         Project         : GPR2.Project.View.Object;
         Use_Source_Path : Boolean;
         Use_Object_Path : Boolean)
      is
         VF : constant GNATCOLL.VFS.Virtual_File :=
                GPR2_GNATCOLL_Projects.Create
                  (Self            => Tree,
                   Name            => Name,
                   Project         => Project,
                   Use_Source_Path => Use_Source_Path,
                   Use_Object_Path => Use_Object_Path);
         S  : constant GNATCOLL.VFS.Filesystem_String := VF.Full_Name;
      begin
         Put_Line ("<" & String (S) & ">");
      end Test_Create;

   begin
      Ada.Text_IO.Put_Line ("=== testing Create ===");
      Tree.Update_Sources (With_Runtime => True);
      Test_Create
        (Name            => GNATCOLL.VFS.Create
           (Full_Filename => "files/absolute.path",
            Normalize     => True).Full_Name,
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "unexisting.txt",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => True,
         Use_Object_Path => True);
      Test_Create
        (Name            => "test.gpr",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "prj0.gpr",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => True,
         Use_Object_Path => True);
      Test_Create
        (Name            => "test.gpr",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "prj0.gpr",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "pck1.ads",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "pck1.ads",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => True,
         Use_Object_Path => False);
      Test_Create
        (Name            => "main.adb",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => True,
         Use_Object_Path => False);
      Test_Create
        (Name            => "pck1.ads",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => True,
         Use_Object_Path => False);
      Test_Create
        (Name            => "pck1.o",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => True,
         Use_Object_Path => True);
      Load ("test.gpr");
      Test_Create
        (Name            => "pck1.o",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => False,
         Use_Object_Path => False);
      Test_Create
        (Name            => "pck1.o",
         Project         => GPR2.Project.View.Undefined,
         Use_Source_Path => True,
         Use_Object_Path => True);
      Test_Create
        (Name            => "main.o",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => False,
         Use_Object_Path => True);
      Test_Create
        (Name            => "pck1.o",
         Project         => Tree.View_For ("prj1", GPR2.Context.Root),
         Use_Source_Path => False,
         Use_Object_Path => True);
   end Test_Create;

   -------------------------------
   -- Test_File_Path_Conversion --
   -------------------------------

   procedure Test_File_Path_Conversion;

   procedure Test_File_Path_Conversion is
      Path : GPR2.Path_Name.Object;

      -------------------------------
      -- To_GNATCOLL_Projects_Test --
      -------------------------------

      procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object);

      procedure To_GNATCOLL_Projects_Test (Path : GPR2.Path_Name.Object) is
         VF : constant GNATCOLL.VFS.Filesystem_String :=
                GPR2_GNATCOLL_Projects.To_Filesystem_String
                  (GPR2_GNATCOLL_Projects.To_Pathname
                     (GPR2_GNATCOLL_Projects.To_Virtual_File (Path)));

         S  : constant GNATCOLL.VFS.Filesystem_String :=
                GPR2_GNATCOLL_Projects.To_Filesystem_String (Path);

      begin
         Ada.Text_IO.Put_Line ("<" & String (VF) & ">");
         Ada.Text_IO.Put_Line ("<" & String (S) & ">");
      end To_GNATCOLL_Projects_Test;

      ----------------------------
      -- To_GPR2_Path_Name_Test --
      ----------------------------

      procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File);

      procedure To_GPR2_Path_Name_Test (VF : GNATCOLL.VFS.Virtual_File) is
         Path : constant GPR2.Path_Name.Object :=
                  GPR2_GNATCOLL_Projects.To_Pathname (VF);
      begin
         Ada.Text_IO.Put ("<");
         if Path.Is_Defined then
            if Path.Has_Dir_Name then
               Ada.Text_IO.Put
                 (String (Path.Dir_Name) & String (Path.Simple_Name));
            else
               Ada.Text_IO.Put (String (Path.Simple_Name));
            end if;
         else
            Ada.Text_IO.Put ("undefined");
         end if;
         Ada.Text_IO.Put_Line (">");
      end To_GPR2_Path_Name_Test;

      ----------------------------
      -- To_GPR2_Path_Name_Test --
      ----------------------------

      procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String);

      procedure To_GPR2_Path_Name_Test (S : GNATCOLL.VFS.Filesystem_String) is
         Path : constant GPR2.Path_Name.Object :=
                  GPR2_GNATCOLL_Projects.To_Pathname (S);
      begin
         Ada.Text_IO.Put ("<");
         if Path.Is_Defined then
            if Path.Has_Dir_Name then
               Ada.Text_IO.Put
                 (String (Path.Dir_Name) & String (Path.Simple_Name));
            else
               Ada.Text_IO.Put (String (Path.Simple_Name));
            end if;
         else
            Ada.Text_IO.Put ("undefined");
         end if;
         Ada.Text_IO.Put_Line (">");
      end To_GPR2_Path_Name_Test;

   begin
      Ada.Text_IO.Put_Line ("=== testing GNATCOLL.VFS conversion ===");
      To_GNATCOLL_Projects_Test (Path);
      Path := GPR2.Path_Name.Create_File ("gpr2.path.no.res",
                                          GPR2.Path_Name.No_Resolution);
      To_GNATCOLL_Projects_Test (Path);
      Path := GPR2.Path_Name.Create_File ("gpr2.path");
      To_GNATCOLL_Projects_Test (Path);
      Ada.Text_IO.Put_Line ("=== testing GPR2.Path_Name conversion ===");
      To_GPR2_Path_Name_Test ("");
      To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create (""));
      To_GPR2_Path_Name_Test ("gpr2.path.no.res");
      To_GPR2_Path_Name_Test (GNATCOLL.VFS.Create ("gpr2.path.no.res"));
      To_GPR2_Path_Name_Test (GNATCOLL.VFS.Filesystem_String (Path.Value));
      To_GPR2_Path_Name_Test
        (GNATCOLL.VFS.Create (GNATCOLL.VFS.Filesystem_String (Path.Value)));
   end Test_File_Path_Conversion;

   ---------------
   -- Test_Name --
   ---------------

   procedure Test_Name;

   procedure Test_Name is
   begin
      Ada.Text_IO.Put_Line ("=== testing Name ===");
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Name (GPR2.Project.View.Undefined) &
           ">");
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Name
           (Tree.View_For ("prj0", GPR2.Context.Root)) & ">");
   end Test_Name;

   ---------------------
   -- Test_Object_Dir --
   ---------------------

   procedure Test_Object_Dir;

   procedure Test_Object_Dir is

      ---------------------
      -- Test_Object_Dir --
      ---------------------

      procedure Test_Object_Dir (View : GPR2.Project.View.Object);

      procedure Test_Object_Dir (View : GPR2.Project.View.Object) is
         VF  : constant GNATCOLL.VFS.Virtual_File :=
                 GPR2_GNATCOLL_Projects.Object_Dir (View);
         FSS : constant GNATCOLL.VFS.Filesystem_String := VF.Full_Name;
      begin
         Ada.Text_IO.Put_Line ("<" & String (FSS) & ">");
      end Test_Object_Dir;
   begin
      Ada.Text_IO.Put_Line ("=== testing Object_Dir ===");
      Test_Object_Dir (GPR2.Project.View.Undefined);
      Test_Object_Dir (Tree.Root_Project);
      Load (Project_Name      => "files/prj2.gpr",
            Subdirs           => "subdirs1");
      Test_Object_Dir (Tree.View_For ("prj1", GPR2.Context.Root));
   end Test_Object_Dir;

   ------------------
   -- Test_Runtime --
   ------------------

   procedure Test_Runtime;

   procedure Test_Runtime is
      Language_Runtimes : GPR2.Containers.Lang_Value_Map :=
                            GPR2.Containers.Lang_Value_Maps.Empty_Map;
   begin
      Ada.Text_IO.Put_Line ("=== testing Get_Runtime ===");
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Get_Runtime (Tree.Root_Project) & ">");
      Language_Runtimes.Insert (+GPR2.Optional_Name_Type'("ada"),
                                GPR2.Value_Type'("zfp"));
      Load (Project_Name      => "test.gpr",
            Language_Runtimes => Language_Runtimes);
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Get_Runtime
           (Tree.View_For ("prj0", GPR2.Context.Root)) & ">");
   end Test_Runtime;

   -----------------
   -- Test_Target --
   -----------------

   procedure Test_Target;

   procedure Test_Target is
   begin
      Ada.Text_IO.Put_Line ("=== testing Get_Target ===");
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Get_Target (Tree) & ">");
      Load ("files/prj2.gpr");
      Ada.Text_IO.Put_Line
        ("<" & GPR2_GNATCOLL_Projects.Get_Target (Tree) & ">");
   end Test_Target;

begin
   Test_File_Path_Conversion;

   Ada.Text_IO.Put_Line ("=== testing Register_New_Attribute ===");
   Register_Attribute ("GCP1", "", False, False, False);
   Register_Attribute ("GCPL", "GCP0", True, False, False);
   Register_Attribute ("GCPI", "GCP0", False, True, False);
   Register_Attribute ("GCPCSI", "GCP0", False, True, True);
   Register_Attribute ("Source_Dirs", "", False, False, False);

   Load ("files/prj2.gpr");

   Test_Name;
   Test_Runtime;
   Test_Create;
   Test_Target;
   Test_Object_Dir;
   Test_Artifacts_Dir;
   Test_Attribute;

end Main;
