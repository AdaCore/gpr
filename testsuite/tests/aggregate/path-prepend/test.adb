with Ada.Text_IO;        use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Path_Name;

procedure Test is

   procedure Show_Tree (Tree : GPR2.Project.Tree.Object);

   ---------------
   -- Show_Tree --
   ---------------

   procedure Show_Tree (Tree : GPR2.Project.Tree.Object)
   is
      use type GPR2.Project.View.Object;
   begin
      if Tree.Log_Messages.Has_Error then
         Put_Line (" loaded with errors");
      else
         for View of reverse Tree.Ordered_Views loop
            if View /= Tree.Runtime_Project then
               Put (" - " & String (View.Name));
               Put (" @ ");
               Put_Line
                 (String
                   (View.Dir_Name.Relative_Path (Tree.Root_Project.Dir_Name)));
            end if;
         end loop;
      end if;
   end Show_Tree;

   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;


begin
   Put_Line ("append.gpr, no project path from command line");

   Opt.Add_Switch (GPR2.Options.P, "./data/append.gpr");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Show_Tree (Tree);
   end if;
   Tree.Unload;

   Put_Line ("prepend.gpr, no project path from command line");
   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "./data/prepend.gpr");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Show_Tree (Tree);
   end if;
   Tree.Unload;

   --  Equivalent to using the -aP switch
   Put_Line ("append.gpr, 'dep' in search path");
   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "./data/append.gpr");
   Opt.Add_Switch (GPR2.Options.AP, "./data/dep");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Show_Tree (Tree);
   end if;
   Tree.Unload;

   Put_Line ("prepend.gpr, 'dep' in search path");
   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "./data/prepend.gpr");
   Opt.Add_Switch (GPR2.Options.AP, "./data/dep");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Show_Tree (Tree);
   end if;
   Tree.Unload;

   Put_Line ("prj.gpr, 'dep' in search path");
   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "./data/prj.gpr");
   Opt.Add_Switch (GPR2.Options.AP, "./data/dep");
   if Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      Show_Tree (Tree);
   end if;
   Tree.Unload;

end Test;
