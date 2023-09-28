with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.View_Ids;

procedure Main is
   use GPR2;
   use type GPR2.View_Ids.View_Id;

   Tree    : GPR2.Project.Tree.Object;
   Log     : GPR2.Log.Object;
   Options : GPR2.Options.Object;
   Result  : Boolean;
   First   : Boolean := True;
begin

   Options.Add_Switch (GPR2.Options.P, "tree/agg.gpr");
   Options.Finalize;
   Result := Options.Load_Project
     (Tree,
      With_Runtime     => True,
      Absent_Dir_Error => GPR2.Project.Tree.No_Error);

   if not Result then
      Tree.Log_Messages.Output_Messages (Information => False);
      return;
   end if;

   Tree.Update_Sources (Sources_Units_Dependencies, Log);
   Log.Output_Messages;

   for NS of Tree.Namespace_Root_Projects loop
      if not First then
         Ada.Text_IO.New_Line;
      end if;

      First := False;

      declare
         Title : constant String := "ROOT VIEW: " & String (NS.Name);
         Under : constant String (Title'Range) := (others => '-');
      begin
         Ada.Text_IO.Put_Line (Title);
         Ada.Text_IO.Put_Line (Under);
      end;

      for U of NS.Units loop
         Ada.Text_IO.Put_Line
           ("* " & String (U.Name) & ": " & String (U.Main_Part.Source.Relative_Path (NS.Dir_Name)));
         for Dep of U.Known_Dependencies loop
            Ada.Text_IO.Put_Line
              ("  - depends on " & String (Dep.Name));
         end loop;
      end loop;
   end loop;
end Main;
