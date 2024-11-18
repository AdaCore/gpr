with Ada.Text_IO;

with GPR2.Build.Artifacts.Files;
with GPR2.Options;

with GNAT.OS_Lib;

with Test_Helper;

package body Test_Helper is

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Debug     : String := "";
      Topic     : Assert_Topic := Check) is
   begin
      if Condition then
         if Message /= "" then
            Ada.Text_IO.Put
              ((
               if Topic = Setup then "[ Setup ] "
               elsif Topic = Check then "[ OK ] "
               else ""
              ));
            Ada.Text_IO.Put (Message);
            Ada.Text_IO.New_Line;
         end if;
      else
         Ada.Text_IO.Put
           ((
            if Topic = Setup then "[ Failed - Setup ] "
            elsif Topic = Check then "[ Failed ] "
            else ""
           ));
         if Message /= "" then
            Ada.Text_IO.Put (Message & " : " & Debug);
         end if;
         Ada.Text_IO.New_Line;
         Test_Result := TEST_KO;
      end if;
   end Assert;

   ---------------------
   -- Create_Ali_File --
   ---------------------

   procedure Create_Ali_File (Base_Name : String) is
      Args : Argument_List;
      Ret  : Integer;
      DS   : constant Character := GNAT.OS_Lib.Directory_Separator;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append ("tree" & DS & "src" & DS & Base_Name & ".adb");
      Args.Append ("-o");
      Args.Append ("tree" & DS & "obj" & DS & Base_Name & ".o");
      Assert
        (Launch_Action (Args) = 0, Base_Name & ".ali Created", Topic => Setup);
   end Create_Ali_File;

   --------------------------
   -- Create_Binder_Action --
   --------------------------

   function Create_Binder_Action
     (Tree   : GPR2.Project.Tree.Object) return GBA.Ada_Bind.Object
   is
      Action : GBA.Ada_Bind.Object;
   begin
      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            declare
               Ali : constant Build.Artifacts.Files.Object :=
                       Build.Artifacts.Files.Create
                         (Root.Object_Directory.Compose
                            (Main.Source.Base_Filename & ".ali"));
            begin
               Action.Initialize
                 ((Kind     => GBA.Ada_Bind.Ada_Main_Program,
                   Main_Ali => Ali),
                  Context   => Root);
               Assert
                 (not Tree.Artifacts_Database.Has_Action (Action.UID),
                  "New binder action", Topic => Setup);
               Assert
                 (Tree.Artifacts_Database.Add_Action (Action),
                  "Action inserted in Tree_DB", Topic => Setup);
               Tree.Artifacts_Database.Add_Input (Action.UID, Ali, True);
            end;
         end loop;
      end loop;

      return Action;
   end Create_Binder_Action;

   -------------------
   -- Launch_Action --
   -------------------

   function Launch_Action (Args : Argument_List) return Integer
   is
      Process : Process_Handle;
   begin
      Process := Start (Args => Args);
      return Wait (Process);
   end Launch_Action;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (Project : String) return GPR2.Project.Tree.Object
   is
      Tree : GPR2.Project.Tree.Object;
      Opt  : GPR2.Options.Object;
   begin
      Opt.Add_Switch (GPR2.Options.P, Project);

      Assert
        (Tree.Load
           (Opt, With_Runtime => True, Absent_Dir_Error => GPR2.No_Error),
         Project & " Loaded", Topic => Setup);

      Assert
        (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
         "Sources updated", Topic => Setup);

      return Tree;
   end Load_Project;

   -------------------
   -- New_Test_Case --
   -------------------

   procedure New_Test_Case (Message : String := "") is
   begin
      Ada.Text_IO.Put ("[ Test case" & Test_Cases'Img & " ]");
      if Message /= "" then
         Ada.Text_IO.Put (" - [ " & Message & " ]");
      end if;
      Ada.Text_IO.New_Line;
      Test_Cases := Test_Cases + 1;
   end New_Test_Case;

end Test_Helper;
