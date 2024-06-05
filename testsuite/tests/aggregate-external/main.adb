with Ada.Environment_Variables;
with Ada.Text_IO;

with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate (Information => False)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Test
     (Name       : String;
      Test_Value : String := "";
      Var_Value  : String := "")
   is
      Opt : Options.Object;
   begin
      Opt.Add_Switch (Options.P, "aggr.gpr");
      if Test_Value'Length > 0 then
         Opt.Add_Switch (Options.X, "TEST=" & Test_Value);
      end if;
      if Var_Value'Length > 0 then
         Opt.Add_Switch (Options.X, "VAR=" & Var_Value);
      end if;

      Ada.Text_IO.Put_Line (Name);
      Tree.Unload;
      if not Tree.Load (Opt) then
         Ada.Text_IO.Put_Line ("Could not load project");
      end if;
   end Test;

begin
   Test ("Test1");
   Test ("Test2", "2");
   Test ("Test3", "3");
   Test ("Test4", "4");
   Ada.Environment_Variables.Set ("VAR", "5");
   Test ("Test5", "5");
   Ada.Environment_Variables.Set ("VAR", "BAD");
   Test ("Test6", "6", "6");

end Main;
