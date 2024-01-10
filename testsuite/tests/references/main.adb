with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.Variable;

procedure Main is
   Tree : GPR2.Project.Tree.Object;

   procedure Test (Name : GPR2.Name_Type) is
      Variable : GPR2.Project.Variable.Object;
   begin
      if Tree.Root_Project.Has_Variables (Name) then
         Variable := Tree.Root_Project.Variable (Name);
         Ada.Text_IO.Put_Line (Tree.Root_Project.Variable (Name).Value.Text);
      else
         Ada.Text_IO.Put_Line ("Cannot test: " & String (Name));
      end if;
   end Test;
begin
   GPR2.Project.Tree.Load_Autoconf
     (Self              => Tree,
      Filename          => GPR2.Path_Name.Create_File ("references.gpr"),
      Context           => GPR2.Context.Empty);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("### type references ###");
   Test ("Test1");
   Test ("Test2");
   Test ("Test3");
   Test ("Test4");
   Test ("Test5");

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("### variable references ###");
   Test ("Test6");
   Test ("Test7");
   Test ("Test8");
   Test ("Test9");
   Test ("Test10");
   Test ("Test11");
   Test ("Test12");
   Test ("Test13");
   Test ("Test14");
   Test ("Test15");

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("### attributes references ###");
   Test ("Test16");
   Test ("Test17");
   Test ("Test18");
   Test ("Test19");
   Test ("Test20");
   Test ("Test21");
   Test ("Test22");
   Test ("Test23");
   Test ("Test24");

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("### package renames/extends references ###");
   Test ("Test25");
   Test ("Test26");
   Test ("Test27");
   Test ("Test28");

exception
   when GPR2.Project_Error =>
      Ada.Text_IO.Put_Line ("Cannot load references.gpr file");
      for C in Tree.Log_Messages.Iterate (Error => True) loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
      end loop;
end Main;