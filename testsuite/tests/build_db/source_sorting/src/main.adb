with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;
   package Sets renames Build.Source.Sets;
   package UB renames Ada.Strings.Unbounded;

   Tree : Project.Tree.Object;
   Opt  : Options.Object;

   procedure Check (Sources : Sets.Object; Expected : String) is
      Actual : UB.Unbounded_String;
   begin
      for S of Sources loop
         UB.Append
           (Actual, To_Lower_Fast (String (S.Owning_View.Name)) & ":" &
            String (S.Path_Name.Simple_Name) & "|");
      end loop;
      if UB.To_String (Actual) /= Expected then
         raise Program_Error with "unexpected order: " & UB.To_String (Actual);
      end if;
   end Check;
begin
   Opt.Add_Switch (Options.P, "tree/root.gpr");
   if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      raise Program_Error with "load failed";
   end if;
   Tree.Update_Sources;

   Check
     (Tree.Root_Project.Visible_Sources (Ambiguous => True),
      (if File_Names_Case_Sensitive
       then "a:Alpha.c|b:Beta.c|a:Zeta.c|b:alpha.c|a:bee.c|"
       else "a:Alpha.c|b:alpha.c|a:bee.c|b:Beta.c|a:Zeta.c|"));
   Check
     (Tree.Root_Project.Visible_Sources,
      (if File_Names_Case_Sensitive
       then "a:Alpha.c|b:Beta.c|a:Zeta.c|b:alpha.c|a:bee.c|"
       else "a:Alpha.c|a:bee.c|b:Beta.c|a:Zeta.c|"));

   for C in Tree.Iterate loop
      declare
         V : constant Project.View.Object := Project.Tree.Element (C);
      begin
         if V.Name = "a" then
            Check
              (Sets.Create (V.View_Db, Sets.Sorted),
               (if File_Names_Case_Sensitive then "a:Alpha.c|a:Zeta.c|a:bee.c|"
                else "a:Alpha.c|a:bee.c|a:Zeta.c|"));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("OK");
end Main;
