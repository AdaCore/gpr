with Ada.Directories;
with Ada.Exceptions;

with GNAT.IO; use GNAT.IO;

with GPR2;
with GPR2.Containers;
with GPR2.File_Readers;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure test is

   Options : GPR2.Options.Object;

   procedure Print_Test_OK is
   begin
      Put_Line ("Test OK");
      Put_Line ("");
   end Print_Test_OK;

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
      RTS := Options.RTS_Map;
      Put_Line ("Ada runtime: " & RTS.Element (GPR2.Ada_Language));
      Put_Line ("Other language runtime: " & RTS.Element (Other_Language));
      Print_Test_OK;
   end Test_Add_Switch_RTS_Map;

   procedure Test_Finalize is

      Tree : GPR2.Project.Tree.Object;
      Res  : Boolean;

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
         Print_Path ("Filename",
                     (if Tree.Root_Project.Is_Defined
                      then Tree.Root_Project.Path_Name
                      else GPR2.Path_Name.Undefined));
         Print_Path ("Build_Path", Options.Build_Path);
      end Print_Paths;

      procedure Test (Current_Directory : String; Allow_Implicit_Project : Boolean := True; Quiet : Boolean := False) is
      begin
         Ada.Directories.Set_Directory (Current_Directory);
         Put_Line ("Testing Finalize no arguments at " & Ada.Directories.Current_Directory
                   & " with Allow_Implicit_Project=" & Allow_Implicit_Project'Image );
         GPR2.Project.Tree.Verbosity :=
           (if Quiet then GPR2.Project.Tree.Quiet
            else GPR2.Project.Tree.Minimal);
         Options := GPR2.Options.Empty_Options;
         Res := Tree.Load
           (Options,
            Allow_Implicit_Project => Allow_Implicit_Project);
         Print_Paths;
         Print_Test_OK;
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " => " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end Test;

   begin
      Put_Line ("Testing Finalize --no-project -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.P, "test");

      begin
         Res := Tree.Load (Options);
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
         Res := Tree.Load (Options);
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

      Put_Line ("Testing with --src-subdirs being an absolute path");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "/tmp/subdir");
      Options.Add_Switch (GPR2.Options.P, "test");
      begin
         Res := Tree.Load (Options);
      exception
         when E : GPR2.Options.Usage_Error =>
            Put_Line (Ada.Exceptions.Exception_Name (E) & " = > " & Ada.Exceptions.Exception_Message (E));
            Print_Test_OK;
      end;

      GPR2.Project.Tree.Verbosity := GPR2.Project.Tree.Quiet;

      Put_Line ("Testing Finalize -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "test");
      Res := Tree.Load (Options);
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --relocate-build-tree=relocated -Ptest");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Res := Tree.Load (Options);
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --no-project --relocate-build-tree=relocated");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Res := Tree.Load (Options);
      Print_Paths;
      Print_Test_OK;

      Put_Line ("Testing Finalize --no-project --relocate-build-tree=relocated --root-dir=.");
      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.No_Project);
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Add_Switch (GPR2.Options.Root_Dir, Ada.Directories.Current_Directory & ".");
      Res := Tree.Load (Options);
      Print_Paths;
      Print_Test_OK;
   end Test_Finalize;


   procedure Test_Load_Project is
      Tree : GPR2.Project.Tree.Object;

      procedure Test
        (Name : String;
         Tree             : in out GPR2.Project.Tree.Object;
         Absent_Dir_Error : GPR2.Error_Level := GPR2.Warning;
         File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                              GPR2.File_Readers.No_File_Reader_Reference;
         Verbosity        : GPR2.Project.Tree.Verbosity_Level :=
                              GPR2.Project.Tree.Minimal)
      is
         Loaded : Boolean;

      begin
         GPR2.Project.Tree.Verbosity := Verbosity;

         Put_Line ("Testing Load Project " & Name);
         Loaded := Tree.Load
                     (Options,
                      Absent_Dir_Error => Absent_Dir_Error,
                      File_Reader      => File_Reader);
         Put_Line ("Load_Project returned " & Loaded'Image);

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
      Test ("-aP added-path -Pload-project/test --autoconf=other-autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug", Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.AP, "load-project");
      Options.Add_Switch (GPR2.Options.Root_Dir, ".");
      Options.Add_Switch (GPR2.Options.Relocate_Build_Tree, "relocated");
      Options.Add_Switch (GPR2.Options.P, "test");
      Options.Add_Switch (GPR2.Options.Autoconf, "other-autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Debug");
      Test ("-aP added-path -aP load-project --root-dir=. --relocate-build-tree=relocated -Ptest --autoconf=autoconf.cgpr -XBUILD=Debug", Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "src_subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "subdirs");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug", Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Target, String (Tree.Target));
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug --target " & String (Tree.Target), Tree);

      Options := GPR2.Options.Empty_Options;
      Options.Add_Switch (GPR2.Options.AP, "added-path");
      Options.Add_Switch (GPR2.Options.P, "load-project/test");
      Options.Add_Switch (GPR2.Options.Target, "unknown-target");
      Options.Add_Switch (GPR2.Options.Config, "autoconf.cgpr");
      Options.Add_Switch (GPR2.Options.Src_Subdirs, "src_subdirs");
      Options.Add_Switch (GPR2.Options.Subdirs, "subdirs");
      Options.Add_Switch (GPR2.Options.X, "BUILD=Release");
      Test ("-aP added-path -Pload-project/test --config=autoconf.cgpr --subdirs=subdirs --src_subdirs=srcsubdirs -XBUILD=Debug --target unknown-target", Tree);
   end Test_Load_Project;

begin
   Test_Add_Switch_DB;
   Test_Add_Switch_P;
   Test_Add_Switch_X;
   Test_On_Extra_Arg;
   Test_Add_Switch_RTS_Map;
   Test_Finalize;
   Test_Load_Project;
end test;
