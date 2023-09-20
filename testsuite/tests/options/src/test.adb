with Ada.Containers;
--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.IO; use GNAT.IO;

with GPR2;
with GPR2.Containers;
with GPR2.Context;
with GPR2.File_Readers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;
procedure test is

   Options : GPR2.Options.Object;

   procedure Print_Test_OK is
   begin
      Put_Line ("Test OK");
      Put_Line ("");
   end Print_Test_OK;

   procedure Add_Switch_Pre_Check is
   begin
      Options.Add_Switch (GPR2.Options.Unchecked_Shared_Lib_Imports);
   end Add_Switch_Pre_Check;

   procedure Base_Pre_Check is
      Base : GPR2.KB.Object := Options.Base;
      pragma Unreferenced (Base);
   begin
      null;
   end Base_Pre_Check;

   procedure Build_Path_Pre_Check is
      Build_Path : GPR2.Path_Name.Object := Options.Build_Path;
      pragma Unreferenced (Build_Path);
   begin
      null;
   end Build_Path_Pre_Check;

   procedure Check_Shared_Lib_Pre_Check is
      Check_Shared_Lib : Boolean := Options.Check_Shared_Lib;
      pragma Unreferenced (Check_Shared_Lib);
   begin
      null;
   end Check_Shared_Lib_Pre_Check;

   procedure Config_Project_Pre_Check is
      Config_Project : GPR2.Path_Name.Object := Options.Config_Project;
      pragma Unreferenced (Config_Project);
   begin
      null;
   end Config_Project_Pre_Check;

   procedure Config_Project_Has_Error_Pre_Check is
      Config_Project_Has_Error :Boolean := Options.Config_Project_Has_Error;
      pragma Unreferenced (Config_Project_Has_Error);
   begin
      null;
   end Config_Project_Has_Error_Pre_Check;

   procedure Config_Project_Log_Pre_Check is
      Config_Project_Log : GPR2.Log.Object := Options.Config_Project_Log;
      pragma Unreferenced (Config_Project_Log);
   begin
      null;
   end Config_Project_Log_Pre_Check;

   procedure Context_Pre_Check is
      Context : GPR2.Context.Object := Options.Context;
      pragma Unreferenced (Context);
   begin
      null;
   end Context_Pre_Check;

   procedure Filename_Pre_Check is
      Filename : GPR2.Path_Name.Object := Options.Filename;
      pragma Unreferenced (Filename);
   begin
      null;
   end Filename_Pre_Check;

   procedure Finalize_Pre_Check is
   begin
      Options.Finalize;
   end Finalize_Pre_Check;

   procedure Implicit_With_Pre_Check is
      Implicit_With : GPR2.Path_Name.Set.Object := Options.Implicit_With;
      pragma Unreferenced (Implicit_With);
   begin
      null;
   end Implicit_With_Pre_Check;

   procedure Load_Project_Pre_Check is
      Tree : GPR2.Project.Tree.Object;
      Loaded : Boolean := Options.Load_Project (Tree);
      pragma Unreferenced (Loaded);
   begin
      null;
   end Load_Project_Pre_Check;

   procedure On_Extra_Arg_Pre_Check is
      On_Extra_Arg : Boolean := Options.On_Extra_Arg ("extra-arg");
      pragma Unreferenced (On_Extra_Arg);
   begin
      null;
   end On_Extra_Arg_Pre_Check;

   procedure Project_File_Pre_Check is
      Project_File : GPR2.Path_Name.Object := Options.Project_File;
      pragma Unreferenced (Project_File);
   begin
      null;
   end Project_File_Pre_Check;

   procedure Project_Is_Defined_Pre_Check is
      Project_Is_Defined : Boolean := Options.Project_Is_Defined;
      pragma Unreferenced (Project_Is_Defined);
   begin
      null;
   end Project_Is_Defined_Pre_Check;

   procedure RTS_Map_Pre_Check is
      RTS_Map : GPR2.Containers.Lang_Value_Map := Options.RTS_Map;
      pragma Unreferenced (RTS_Map);
   begin
      null;
   end RTS_Map_Pre_Check;

   procedure Src_Subdirs_Unbounded_Pre_Check is
      Src_Subdirs : Ada.Strings.Unbounded.Unbounded_String := Options.Src_Subdirs;
      pragma Unreferenced (Src_Subdirs);
   begin
      null;
   end Src_Subdirs_Unbounded_Pre_Check;

   procedure Src_Subdirs_Pre_Check is
      Src_Subdirs : GPR2.Optional_Name_Type := Options.Src_Subdirs;
      pragma Unreferenced (Src_Subdirs);
   begin
      null;
   end Src_Subdirs_Pre_Check;

   procedure Subdirs_Unbounded_Pre_Check is
      Subdirs : Ada.Strings.Unbounded.Unbounded_String := Options.Subdirs;
      pragma Unreferenced (Subdirs);
   begin
      null;
   end Subdirs_Unbounded_Pre_Check;

   procedure Subdirs_Pre_Check is
      Subdirs : GPR2.Optional_Name_Type := Options.Subdirs;
      pragma Unreferenced (Subdirs);
   begin
      null;
   end Subdirs_Pre_Check;

   procedure Target_Pre_Check is
      Target : GPR2.Name_Type := Options.Target;
      pragma Unreferenced (Target);
   begin
      null;
   end Target_Pre_Check;

   procedure Pre_Check_Test (Test : access procedure; Name : String; Finalized : Boolean :=  False) is
   begin
      Put_Line ("Testing " & Name);
      Options := GPR2.Options.Empty_Options;
      if Finalized then
         Options.Add_Switch (GPR2.Options.No_Project);
         Options.Finalize;
      end if;

      begin
         Test.all;
      exception
         when E : others =>
            Put_Line (Name & " => " & Ada.Exceptions.Exception_Name (E));
            Print_Test_OK;
      end;
   end Pre_Check_Test;

   procedure Pre_Check_Tests is
   begin
      Pre_Check_Test (Add_Switch_Pre_Check'Access, "Add_Switch pre check", True);
      Pre_Check_Test (Base_Pre_Check'Access, "Base pre check", False);
      Pre_Check_Test (Build_Path_Pre_Check'Access, "Build_Path pre check", False);
      Pre_Check_Test (Check_Shared_Lib_Pre_Check'Access, "Check_Shared_Lib pre check", False);
      Pre_Check_Test (Config_Project_Pre_Check'Access, "Config_Project_Pre_Check pre check", False);
      Pre_Check_Test (Config_Project_Has_Error_Pre_Check'Access, "Config_Project_Has_Error pre check", False);
      Pre_Check_Test (Config_Project_Log_Pre_Check'Access, "Config_Project_Log pre check", False);
      Pre_Check_Test (Context_Pre_Check'Access, "Context pre check", False);
      Pre_Check_Test (Filename_Pre_Check'Access, "Filename pre check", False);
      Pre_Check_Test (Finalize_Pre_Check'Access, "Finalize pre check", True);
      Pre_Check_Test (Implicit_With_Pre_Check'Access, "Implicit_With pre check", False);
      Pre_Check_Test (Load_Project_Pre_Check'Access, "Load_Project pre check", False);
      Pre_Check_Test (On_Extra_Arg_Pre_Check'Access, "On_Extra_Arg pre check", True);
      Pre_Check_Test (Project_File_Pre_Check'Access, "Project_File pre check", False);
      Pre_Check_Test (Project_Is_Defined_Pre_Check'Access, "Project_Is_Defined pre check", False);
      Pre_Check_Test (RTS_Map_Pre_Check'Access, "RTS_Map pre check", False);
      Pre_Check_Test (Src_Subdirs_Unbounded_Pre_Check'Access, "Src_Subdirs_Unbounded pre check", False);
      Pre_Check_Test (Src_Subdirs_Pre_Check'Access, "Src_Subdirs pre check", False);
      Pre_Check_Test (Subdirs_Unbounded_Pre_Check'Access, "Subdirs_Unbounded pre check", False);
      Pre_Check_Test (Subdirs_Pre_Check'Access, "Subdirs pre check", False);
      Pre_Check_Test (Target_Pre_Check'Access, "Target pre check", False);
   end Pre_Check_Tests;

   procedure Test_Add_Switch_DB is
   begin
      Put_Line ("Testing Add_Switch --db test.gpr");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.DB, "test.gpr");
      Print_Test_OK;

      Put_Line ("Testing Add_Switch --db src");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.DB, "src");
      Print_Test_OK;

      Put_Line ("Testing Add_Switch --db unexisting");
      Options := GPR2.Options.Empty_Options;

      begin
         Options.Add_Switch (GPR2.Options.DB, "unexisting");
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;
   end Test_Add_Switch_DB;

   procedure Test_Add_Switch_P is
   begin
      Put_Line ("Testing Add_Switch -Pfirst -Psecond");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "first");

      begin
         Options.Add_Switch (GPR2.Options.P, "second");
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;

      Put_Line ("Testing Add_Switch extra.gpr -Ptest");
      Options := GPR2.Options.Empty_Options;
      Put_Line ("On_Extra_Arg (""extra.gpr"") => " & GPR2.Options.On_Extra_Arg (Options, "extra.gpr")'Image);

      begin
         Options.Add_Switch (GPR2.Options.P, "test");
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;
   end Test_Add_Switch_P;

   procedure Test_Add_Switch_X is
   begin
      Put_Line ("Testing Add_Switch -X name:value");
      Options := GPR2.Options.Empty_Options;

      begin
         Options.Add_Switch (GPR2.Options.X, "name:value");
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;
   end Test_Add_Switch_X;

   procedure Test_On_Extra_Arg is
      Name1     : constant String := "extra1.gpr";
      Name2     : constant String := "extra2.gpr";
      Other_Arg : constant String := "other-arg";
   begin
      Put_Line ("Testing On_Extra_Arg other-arg extra1.gpr other-arg extra2.gpr");

      Options := GPR2.Options.Empty_Options;
      Put_Line ("On_Extra_Arg (""" & Other_Arg & """) => " & GPR2.Options.On_Extra_Arg (Options, Other_Arg)'Image);
      Put_Line ("On_Extra_Arg (""" & Name1 & """) => " & GPR2.Options.On_Extra_Arg (Options, Name1)'Image);
      Put_Line ("On_Extra_Arg (""" & Other_Arg & """) => " & GPR2.Options.On_Extra_Arg (Options, Other_Arg)'Image);

      begin
         Put_Line ("On_Extra_Arg (""" & Name2 & """) => " & GPR2.Options.On_Extra_Arg (Options, Name2)'Image);
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;

      Put_Line ("Testing Add_Switch -Ptest extra.gpr");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "test");

      begin
         Put_Line ("On_Extra_Arg (""extra.gpr"") => " & GPR2.Options.On_Extra_Arg (Options, "extra.gpr")'Image);
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;
   end Test_On_Extra_Arg;

   procedure Test_Add_Switch_RTS_Map is
      use GPR2;

      RTS : GPR2.Containers.Lang_Value_Map;
      Other_Language : constant Language_Id := +"otherlanguage";
   begin
      Put_Line ("Testing --RTS=adaRuntime1 --RTS:otherLanguage=otherRuntime1");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.RTS, "adaRuntime1");
      Options.Add_Switch (GPR2.Options.RTS, "otherRuntime1", "otherLanguage");
      Options.Finalize;
      RTS := Options.RTS_Map;
      Put_Line ("Ada runtime: " & RTS.Element (GPR2.Ada_Language));
      Put_Line ("Other runtime: " & RTS.Element (Other_Language));
      Print_Test_OK;

      Put_Line ("Testing --RTS:ada=adaRuntime0 --RTS:ADA=adaRuntime2 --RTS:otherLanguage=otherRuntime1");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.RTS, "adaRuntime0", "ada");
      Options.Add_Switch (GPR2.Options.RTS, "adaRuntime2", "ADA");
      Options.Add_Switch (GPR2.Options.RTS, "otherRuntime0", "otherLanguage");
      Options.Add_Switch (GPR2.Options.RTS, "otherRuntime2", "OTHERLANGUAGE");
      Options.Finalize;
      RTS := Options.RTS_Map;
      Put_Line ("Ada runtime: " & RTS.Element (GPR2.Ada_Language));
      Put_Line ("Other language runtime: " & RTS.Element (Other_Language));
      Print_Test_OK;
   end Test_Add_Switch_RTS_Map;

   procedure Test_Values is

      procedure Output_Values (Allow_Implicit_Project : String := "")is
         use Ada.Containers;
      begin
         Put_Line ("Filename" & Allow_Implicit_Project & ":" & Options.Filename.String_Value);
         Put_Line ("Context.Length" & Allow_Implicit_Project & ":" & Options.Context.Length'Image);

         if Options.Implicit_With.Length > 0 then
            Put_Line ("Context'First " & String (Options.Context.First_Key) & "=" & String (Options.Context.First_Element));
         else
            Put_Line ("Context" & Allow_Implicit_Project & ": is empty");
         end if;

         if Options.Config_Project.Is_Defined then
            Put_Line ("Config_Project:" & String (Options.Config_Project.Name));
         else
            Put_Line ("Config_Project" & Allow_Implicit_Project & ": is not defined");
         end if;

         Put_Line ("Build_Path" & Allow_Implicit_Project & ":" & Options.Build_Path.String_Value);

         declare
            Subdirs     : constant GPR2.Optional_Name_Type := Options.Subdirs;
            Src_Subdirs : constant GPR2.Optional_Name_Type := Options.Src_Subdirs;
         begin
            Put_Line ("Subdirs" & Allow_Implicit_Project & ":" & String (Subdirs));
            Put_Line ("Subdirs (unbounded)" & Allow_Implicit_Project & ":" & Ada.Strings.Unbounded.To_String (Options.Subdirs));
            Put_Line ("Src_Subdirs" & Allow_Implicit_Project & ":" & String (Src_Subdirs));
            Put_Line ("Src_Subdirs (unbounded)" & Allow_Implicit_Project & ":" & Ada.Strings.Unbounded.To_String (Options.Src_Subdirs));
         end;
         Put_Line ("Check_Shared_Lib" & Allow_Implicit_Project & ":" & Options.Check_Shared_Lib'Image);

         if Options.Implicit_With.Length > 0 then
            Put_Line ("Implicit_With'First" & String (Options.Implicit_With.First_Element.Name));
         else
            Put_Line ("Implicit_With" & Allow_Implicit_Project & ": is empty");
         end if;

         Put_Line ("Target" & Allow_Implicit_Project & ":" & String (Options.Target));
         Put_Line ("RTS_Map.Length" & Allow_Implicit_Project & ":" & Options.RTS_Map.Length'Image);
         Put_Line ("Base.Is_Default_Db" & Allow_Implicit_Project & ":" & Options.Base.Is_Default_Db'Image);
         Put_Line ("Project_Is_Defined" & Allow_Implicit_Project & ":" & Options.Project_Is_Defined'Image);
      end Output_Values;

   begin
      Put_Line ("Testing default values (--no-project)");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Finalize (Allow_Implicit_Project => False);
      Output_Values;
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Finalize;
      Output_Values ("(Allow_Implicit_Project)");
      Print_Test_OK;

      Put_Line ("Testing values");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "./path-added");
      Options.Add_Switch (GPR2.Options.Autoconf, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Db_Minus);
      Options.Add_Switch (GPR2.Options.Implicit_With, "implicit.gpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "Src_Subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "Subdirs");
      Options.Add_Switch (GPR2.Options.Target, "arm-elf");
      Options.Add_Switch (GPR2.Options.Unchecked_Shared_Lib_Imports);
      Options.Add_Switch (GPR2.Options.X, "key=value");
      Options.Finalize;
      Output_Values;
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.Config, "config.cgpr");
      Options.Finalize;
      Put_Line ("Config_Project (--config):" & String (Options.Config_Project.Name));
      Print_Test_OK;
   end Test_Values;

   procedure Test_Finalize is

      procedure Print_Path (Name : String; Path_Name : GPR2.Path_Name.Object) is
      begin
         if Path_Name.Is_Defined then
            Put_Line (Name & ":" & String (Path_Name.Name));
         else
            Put_Line (Name & " is Undefined");
         end if;
      end Print_Path;

      procedure Print_Paths is
      begin
         Print_Path ("Filename", Options.Filename);
         Print_Path ("Build_Path", Options.Build_Path);
      end Print_Paths;

      procedure Test (Current_Directory : String; Allow_Implicit_Project : Boolean := True;Quiet : Boolean := False) is
      begin
         Ada.Directories.Set_Directory (Current_Directory);
         Put_Line ("Testing Finalize no arguments at " & Ada.Directories.Current_Directory
                  & " with Allow_Implicit_Project=" & Allow_Implicit_Project'Image );
         Options := GPR2.Options.Empty_Options;
         Options.Finalize (Allow_Implicit_Project, Quiet);
         Print_Paths;
         Print_Test_OK;
      end Test;

   begin
      Put_Line ("Testing Finalize --no-project -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.P, "test");

      begin
         Options.Finalize;
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;

      Put_Line ("Testing Finalize --root-dir=root  -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.Root_Dir, "root");
      Options.Add_Switch (GPR2.Options.P, "test");

      begin
         Options.Finalize;
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;

      Test (".");
      Test ("./new-current-directory/default");
      Test ("../two-gpr");
      Test ("../two-gpr", Quiet => True);
      Test ("../no-gpr");
      Test ("../..", Allow_Implicit_Project => False);

      Put_Line ("Testing Finalize -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Finalize;
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --relocate-build-tree=relocated -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Finalize;
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --no-project --relocate-build-tree=relocated");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Finalize;
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --no-project --relocate-build-tree=relocated --root-dir=.");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Add_Switch (GPR2.Options.Root_Dir, Ada.Directories.Current_Directory & ".");
      Options.Finalize;
      Print_Paths;
      Print_Test_OK;
   end Test_Finalize;


   procedure Test_Load_Project is
      Tree : GPR2.Project.Tree.Object;

      procedure Test (Name : String;
                      Tree             : in out GPR2.Project.Tree.Object;
                      Absent_Dir_Error : GPR2.Project.Tree.Error_Level :=
                        GPR2.Project.Tree.Warning;
                      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                        GPR2.File_Readers.No_File_Reader_Reference;
                      Quiet            : Boolean := False) is
         Loaded : Boolean;

      begin
         Put_Line ("Testing Load Project " & Name);
         Loaded := Options.Load_Project
                     (Tree             => Tree,
                      Absent_Dir_Error => Absent_Dir_Error,
                      File_Reader      => File_Reader,
                      Quiet            => Quiet);
         Put_Line ("Load_Project returned " & Loaded'Image);

         if Options.Config_Project_Has_Error then
            Options.Config_Project_Log.Output_Messages (Information => False,
                                                        Warning     => False);
         else
            Options.Config_Project_Log.Output_Messages;
         end if;

         if Tree.Log_Messages.Has_Error then
            Tree.Log_Messages.Output_Messages (Information => False,
                                               Warning     => False);
         else
            Tree.Log_Messages.Output_Messages (Information => False);
         end if;

         if not Loaded then
            Put_Line ("Target:" & String (Tree.Target));
            Put_Line ("Runtime(Ada):" & String (Tree.Runtime (GPR2.Ada_Language)));
            Put_Line ("Subdirs:" & String (Tree.Subdirs));

            if Tree.Has_Src_Subdirs then
               Put_Line ("Src_Subdirs:" & String (Tree.Src_Subdirs));
            end if;

            Put_Line ("Build_Path:" & String (Tree.Build_Path.Name));
            Put_Line ("Object_Directory:" & String (Tree.Root_Project.Object_Directory.Name));
         end if;
         Print_Test_OK;
      end Test;

   begin
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Autoconf, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "src_subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "subdirs");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Debug");
      Options.Finalize;
      Test ("-aP added-path -Pload-project/test --autoconf=other-autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug", Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.AP, "load-project");
      Options.Add_Switch (GPR2.Options.Root_Dir, ".");
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Add_Switch (GPR2.Options.Autoconf, "other-autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Debug");
      Options.Finalize;
      Test ("-aP added-path -aP load-project --root-dir=. --relocate-build-tree=relocated -Ptest --autoconf=autoconf.cgpr -XBUILD=Debug", Tree, Quiet => True);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "src_subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "subdirs");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Options.Finalize;
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug", Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Target, String (Tree.Target));
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Options.Finalize;
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug --target " & String (Tree.Target), Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Target, "unknown-target");
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "src_subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "subdirs");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Options.Finalize;
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug --target unknown-target", Tree);
   end Test_Load_Project;

begin
   Pre_Check_Tests;
   Test_Add_Switch_DB;
   Test_Add_Switch_P;
   Test_Add_Switch_X;
   Test_On_Extra_Arg;
   Test_Add_Switch_RTS_Map;
   Test_Values;
   Test_Finalize;
   Test_Load_Project;
end test;
