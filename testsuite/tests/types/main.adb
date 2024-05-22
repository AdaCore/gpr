with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
--  with GPR2.Project.Typ;

procedure Main is

   use Ada;
   use GPR2;

   procedure Load (Filename : String);

   ----------
   -- Load --
   ----------

   procedure Load (Filename : String) is
      Prj : Project.Tree.Object;
      Opt : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Filename);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Text_IO.Put_Line ("All good, no message.");

         for T of Prj.Root_Project.Types loop
            Text_IO.Put ("Type : " & String (T.Name.Text) & " -");

            for V of T.Values loop
               Text_IO.Put (' ' & V.Text);
            end loop;

            Text_IO.New_Line;
         end loop;
      end if;
   end Load;

begin
   Load ("demo.gpr");
   Load ("demo2.gpr");
   Load ("demo3.gpr");
   Load ("demo4-child.gpr");
end Main;
