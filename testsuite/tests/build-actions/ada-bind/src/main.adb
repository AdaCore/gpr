with Ada.Directories;
with Ada.Containers;

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;

with GNATCOLL.Utils;
with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

use GPR2, GPR2.Build;

with Test_Helper;

function Main return Integer is
   use type GPR2.Language_Id;
   use type Ada.Containers.Count_Type;
   package GBA renames GPR2.Build.Actions;

   DS : constant Character := GNAT.OS_Lib.Directory_Separator;
begin
   Test_Case_Binder_Action_Process : declare
      Tree    : GPR2.Project.Tree.Object;
      Action  : GBA.Ada_Bind.Object := GBA.Ada_Bind.Undefined;
      Count   : Natural;
      Obj_Dir : Virtual_File;
   begin
      Test_Helper.New_Test_Case ("Binder action process");
      Tree := Test_Helper.Load_Project ("tree/main.gpr");
      Obj_Dir := Tree.Root_Project.Object_Directory.Virtual_File;
      Make_Dir (Obj_Dir);
      Test_Helper.Assert (Is_Directory (Obj_Dir));

      --  We need to create the ALI files dynamically because the
      --  compiler date used to create the ALI is stored at its first line.
      --  If the runtime ALIs date differs, then an error is raised.

      Test_Helper.Create_Ali_File ("main");
      Test_Helper.Create_Ali_File ("pkg");
      Test_Helper.Create_Ali_File ("dep_two");

      Action := Test_Helper.Create_Binder_Action (Tree);

      declare
         Ret     : Integer;
         Process : Process_Handle;
      begin
         Action.Update_Command_Line (1);
         Process := Start
           (Args        => Action.Command_Line.Argument_List,
            Env         => Action.Command_Line.Environment_Variables,
            Cwd         => Action.Working_Directory.String_Value,
            Inherit_Env => True);
         Ret := Wait (Process);
         Test_Helper.Assert (Ret = 0, "Successful action");
      end;

      Test_Helper.Assert (Action.Generated_Spec.Path.Extension = ".ads",
                          "Spec has .ads extension");
      Test_Helper.Assert (Action.Generated_Body.Path.Extension = ".adb",
                          "Body has .adb extension");
      Test_Helper.Assert
        (Action.Generated_Body.Path.Base_Name =
           Action.Generated_Spec.Path.Base_Name,
         "Coherent base name");
      Test_Helper.Assert (Action.Generated_Body.Path.Base_Name = "b__main",
                          "Expected base name");

      Count := 0;
      for Input of Tree.Artifacts_Database.Inputs (Action.UID) loop
         Count := Count + 1;
         Test_Helper.Assert
           (Artifacts.Files.Object (Input).Path.Simple_Name in "main.ali" | "pkg.ali" | "dep_two.ali",
            "Artifact file is " & String (Artifacts.Files.Object (Input).Path.Simple_Name));
      end loop;

      Test_Helper.Assert (Count = 3, "Correct number of inputs");

      declare
         PB  : GPR2.Build.Actions.Post_Bind.Object := Action.Post_Bind;
      begin
         Test_Helper.Assert (PB.Is_Defined,
                             "Post-Bind action correctly created");
      end;
   end Test_Case_Binder_Action_Process;

   Test_Cases_Parse_Binder_Tool : declare
      Tree   : GPR2.Project.Tree.Object;
      Action : GBA.Ada_Bind.Object := GBA.Ada_Bind.Undefined;
   begin
      Test_Helper.New_Test_Case ("Parse gnatbind_prefix");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("gnatbind_prefix=bla");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/gnatbind_prefix.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);
      Action.Update_Command_Line (1);

      declare
         Args     : constant Argument_List := Action.Command_Line.Argument_List;
         Driver   : constant String :=
           GNAT.OS_Lib.Normalize_Pathname (Args.First_Element);
         Expected : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (String (Tree.Root_Project.Path_Name.Dir_Name
              & DS & "bin" & DS & "bla-gnatbind")
              & Test_Helper.Get_Executable_Suffix);
      begin
         Test_Helper.Assert
           (Driver = Expected,
            "Test binder tool with gnatbind_prefix=bla",
            Debug => Driver & " /= " & Expected);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) = 0,
            "Successfully launched process");
      end;

      Test_Helper.New_Test_Case ("Parse gnatbind_prefix with dash");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("gnatbind_prefix=bla-");
      --  end Binder;
      Tree   :=
        Test_Helper.Load_Project ("tree/gnatbind_prefix_with_dash.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);
      Action.Update_Command_Line (1);

      declare
         Args     : constant Argument_List := Action.Command_Line.Argument_List;
         Driver   : constant String :=
           GNAT.OS_Lib.Normalize_Pathname (Args.First_Element);
         Expected : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (String (Tree.Root_Project.Path_Name.Dir_Name
              & DS & "bin" & DS & "bla-gnatbind")
              & Test_Helper.Get_Executable_Suffix);
      begin
         Test_Helper.Assert
           (Driver = Expected,
            "Test binder tool with gnatbind_prefix=bla-",
            Debug => Driver & " /= " & Expected);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Action.Command_Line.Argument_List) = 0,
            "Successfully launched process");
      end;

      Test_Helper.New_Test_Case ("Parse ada_binder");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("ada_binder=bla-gnatbind");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/ada_binder.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);
      Action.Update_Command_Line (1);

      declare
         Args     : constant Argument_List := Action.Command_Line.Argument_List;
         Driver   : constant String :=
           GNAT.OS_Lib.Normalize_Pathname (Args.First_Element);
         Expected : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (String (Tree.Root_Project.Path_Name.Dir_Name
              & DS & "bin" & DS & "bla-gnatbind")
              & Test_Helper.Get_Executable_Suffix);
      begin
         Test_Helper.Assert
           (Driver = Expected,
            "Test binder tool with ada_binder=bla-gnatbind",
            Debug => Driver & " /= " & Expected);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) = 0,
            "Successfully launched process");
      end;

      Test_Helper.New_Test_Case ("Parse gnatbind_path");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("--gnatbind_path=" & Project'Project_Dir & "/bin/bla-gnatbind");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/gnatbind_path.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);
      Action.Update_Command_Line (1);

      declare
         Args     : constant Argument_List := Action.Command_Line.Argument_List;
         Driver   : constant String :=
           GNAT.OS_Lib.Normalize_Pathname (Args.First_Element);
         Expected : constant String :=
           GNAT.OS_Lib.Normalize_Pathname
             (String (Tree.Root_Project.Path_Name.Dir_Name
              & DS & "bin" & DS & "bla-gnatbind")
              & Test_Helper.Get_Executable_Suffix);
      begin
         Test_Helper.Assert
           (Driver = Expected,
            "Correctly found binder tool define in gnatbind_path=<path>/bla-gnatbind",
            Debug => Driver & "/=" & Expected);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) = 0,
            "Successfully launched process");
      end;

      Test_Helper.New_Test_Case ("Parse empty required switches");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/empty_required_switches.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);

      declare
         Args    : Argument_List;
         Expected_Tool_Path : constant String :=
                                GNAT.OS_Lib.Normalize_Pathname
                                  (String (Tree.Root_Project.Path_Name.Dir_Name
                                   & DS & "bin" & DS & "gnatbind")
                                   & Test_Helper.Get_Executable_Suffix);
      begin
         Action.Update_Command_Line (1);
         Args := Action.Command_Line.Argument_List;
         Test_Helper.Assert
           (Args.First_Element = Expected_Tool_Path,
            "Correctly defaulted to binder tool in Compiler.Driver install directory",
            Debug => Args.First_Element);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) = 0,
            "Successfully launched process");
      end;

      Test_Helper.New_Test_Case ("Fake gnatbind path");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("--gnatbind_path=" & Project'Project_Dir & "fake/fake-gnatbind");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/fake_gnatbind_path.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);

      declare
         Args               : Argument_List;
         Expected_Tool_Path : constant String := "fake-gnatbind" & (if GPR2.On_Windows then ".exe" else "");
         Expected_Error     : constant String :=
                                "cannot spawn process";
      begin
         Action.Update_Command_Line (1);
         Args := Action.Command_Line.Argument_List;
         Test_Helper.Assert
           (Ends_With (Args.First_Element, Expected_Tool_Path),
            "Unknown tool",
            Debug => Args.First_Element & ":" & Expected_Tool_Path);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) /= 0,
            "Process error expected due to a fake gnatbind being used");
      exception
         when E : GNATCOLL.OS.OS_Error =>
            Test_Helper.Assert
              (Starts_With (Ada.Exceptions.Exception_Message (E), Expected_Error),
               "Process error expected due to a fake gnatbind being used",
               Ada.Exceptions.Exception_Message (E));
      end;

      Test_Helper.New_Test_Case ("Fake gnatbind prefix");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("gnatbind_prefix=fake");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/fake_gnatbind_prefix.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);

      declare
         Args           : Argument_List;
         Expected_Tool  : constant String :=
                            "fake-gnatbind" & (if GPR2.On_Windows then ".exe" else "");
         Expected_Error : constant String :=
                            "cannot spawn process";
      begin
         Action.Update_Command_Line (1);
         Args := Action.Command_Line.Argument_List;
         Test_Helper.Assert
           (Args.First_Element = Expected_Tool,
            "Unknown tool",
            Debug => Args.First_Element);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) /= 0,
            "Process error expected due to a fake gnatbind being used");
      exception
         when E : GNATCOLL.OS.OS_Error =>
            Test_Helper.Assert
              (Starts_With (Ada.Exceptions.Exception_Message (E), Expected_Error),
               "Process error expected due to a fake gnatbind being used",
               Ada.Exceptions.Exception_Message (E));
      end;

      Test_Helper.New_Test_Case ("Fake ada binder");
      --  package Binder is
      --   	for Required_Switches ("Ada") use ("ada_binder=fake-gnatbind");
      --  end Binder;
      Tree   := Test_Helper.Load_Project ("tree/fake_ada_binder.gpr");
      Action := Test_Helper.Create_Binder_Action (Tree);

      declare
         Args           : Argument_List;
         Expected_Tool  : constant String := "fake-gnatbind" & (if GPR2.On_Windows then ".exe" else "");
         Expected_Error : constant String := "cannot spawn process";
      begin
         Action.Update_Command_Line (1);
         Args := Action.Command_Line.Argument_List;
         Test_Helper.Assert
           (Args.First_Element = Expected_Tool,
            "Unknown tool",
            Debug => Args.First_Element);
         Test_Helper.Assert
           (Test_Helper.Launch_Action (Args) /= 0,
            "Process error expected due to a fake gnatbind being used");
      exception
         when E : GNATCOLL.OS.OS_Error =>
            Test_Helper.Assert
              (Starts_With (Ada.Exceptions.Exception_Message (E), Expected_Error),
               "Process error expected due to a fake gnatbind being used",
               Ada.Exceptions.Exception_Message (E));
      end;
   end Test_Cases_Parse_Binder_Tool;

   return Test_Helper.Result;
end Main;
