with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   function Get_Aggregated
     (View : GPR2.Project.View.Object; Name : Simple_Name)
      return GPR2.Project.View.Object is
   begin
      for V of View.Aggregated (False) loop
         if V.Path_Name.Simple_Name = Name then
            return V;
         end if;
      end loop;
      return GPR2.Project.View.Undefined;
   end Get_Aggregated;

   procedure Test (View : GPR2.Project.View.Object) is
      A     : GPR2.Project.View.Object;
      B     : GPR2.Project.View.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & String (View.Path_Name.Simple_Name));
      A := Get_Aggregated (View, "a.gpr");
      B := Get_Aggregated (View, "b.gpr");
      Ada.Text_IO.Put_Line ("A.Object_Dir:" & A.Object_Directory.String_Value);
      Ada.Text_IO.Put_Line ("B.Object_Dir:" & B.Object_Directory.String_Value);
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

   procedure Load (Project_Name : String) is
      Opt  : GPR2.Options.Object;
      Dead : Boolean with Unreferenced;
   begin
      Ada.Text_IO.Put_Line ("loading " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (GPR2.Options.P, String (Project_Name));
      Dead := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Load;

begin
   Load ("files/aggl1.gpr");
   Test (Tree.Root_Project);
   Load ("files/aggl.gpr");
   Test (Get_Aggregated (Tree.Root_Project, "aggl1.gpr"));
end Main;
