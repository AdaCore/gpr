with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.View_Ids;

procedure Main is
   use GPR2;
   use type GPR2.View_Ids.View_Id;

   CWD : constant Path_Name.Object := Path_Name.Create_Directory (".");

   procedure Show (CU : Build.Compilation_Unit.Object) is
   begin
      Ada.Text_IO.Put_Line (" " & String (CU.Name));
      Ada.Text_IO.Put_Line (" - " & String (CU.Main_Part.Source.Relative_Path (CWD)));
   end Show;

   Tree    : GPR2.Project.Tree.Object;
   Options : GPR2.Options.Object;
   Result  : Boolean;
   First   : Boolean := True;
   Names   : GPR2.Containers.Filename_Set;
begin

   Options.Add_Switch (GPR2.Options.P, "tree/agg.gpr");

   if not Tree.Load (Options, True, No_Error) then
      return;
   end if;

   Tree.Update_Sources (Sources_Units);

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
         if not U.Owning_View.Is_Runtime then
            Ada.Text_IO.Put_Line
              ("* " & String (U.Name) & ": " &
                 String (U.Main_Part.Source.Relative_Path (NS.Dir_Name)));
            for Dep of U.Known_Dependencies loop
               Ada.Text_IO.Put_Line
                 ("  - depends on " & String (Dep));
            end loop;
         end if;
      end loop;
   end loop;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name));
   Tree.For_Each_Ada_Closure (Show'Access);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name));
   Tree.For_Each_Ada_Closure (Show'Access, Root_Project_Only => True);

   Tree.Unload;
   Options := GPR2.Options.Empty_Options;
   Options.Add_Switch (GPR2.Options.P, "tree/c.gpr");
      Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("tree/c.gpr");
   if Tree.Load (Options, Absent_Dir_Error => No_Error) then
      Tree.Update_Sources (Sources_Units);
      Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name));
      Tree.For_Each_Ada_Closure (Show'Access);
      Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name) & " (lookup p1)");
      Names.Insert ("p1");
      Tree.For_Each_Ada_Closure (Show'Access, Mains => Names);
      begin
         Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name) & " (lookup unknown)");
         Names.Clear;
         Names.Insert ("unknown");
         Tree.For_Each_Ada_Closure (Show'Access, Mains => Names);
      exception
         when E : GPR2.Options.Usage_Error =>
            Ada.Text_IO.Put_Line ("error: " & Ada.Exceptions.Exception_Message (E));
      end;
      Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name) & " (option -U)");
      Tree.For_Each_Ada_Closure (Show'Access, All_Sources => True);
      Ada.Text_IO.Put_Line ("TREE_CLOSURE: " & String (Tree.Root_Project.Name) & " (option -U, --no-subprojects)");
      Tree.For_Each_Ada_Closure (Show'Access, All_Sources => True, Root_Project_Only => True);
   end if;
end Main;
