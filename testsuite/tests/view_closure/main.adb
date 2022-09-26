with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

   procedure Display (V : View.Object);
   procedure Display (V : View.Object) is
   begin
      Text_IO.Put_Line (Item => "   View : " & String (V.Name));
   end Display;

begin
   Project.Tree.Load (Prj, Create ("projects/demo.gpr"), Ctx);

   Text_IO.Put_Line (Item => "Iterate from Tree :");
   for V in Prj.Iterate loop
      declare
         Elt : View.Object := Tree.Element (V);
      begin
         Display (Elt);
      end;
   end loop;

   for V in Prj.Iterate loop
      declare
         Elt : View.Object := Tree.Element (V);
      begin
         Text_IO.Put_Line (Item => "Closure from " & String (Elt.Name));
         for V_Closure of Elt.Closure loop
            Display (V => V_Closure);
         end loop;
      end;
   end loop;

end Main;
