with Ada.Text_IO;        use Ada.Text_IO;

with GPR2.Context;
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
   begin
      if Tree.Log_Messages.Has_Error then
         Put_Line (" loaded with errors");
      else
         for View of reverse Tree.Ordered_Views loop
            Put (" - " & String (View.Name));
            Put (" @ ");
            declare
               Rel_Path : GPR2.Path_Name.Object;
            begin
               Rel_Path :=
                 View.Dir_Name.Relative_Path (Tree.Root_Project.Dir_Name);
               Put_Line (String (Rel_Path.Name));
            end;
         end loop;
      end if;
   end Show_Tree;

   Ctx  : GPR2.Context.Object;
   Tree : GPR2.Project.Tree.Object;


begin
   Put_Line ("append.gpr, no project path from command line");
   GPR2.Project.Tree.Load_Autoconf
     (Tree,
      GPR2.Path_Name.Create_File ("./data/append.gpr"),
      Context => Ctx);
   Show_Tree (Tree);
   Tree.Unload;

   Put_Line ("prepend.gpr, no project path from command line");
   GPR2.Project.Tree.Load_Autoconf
     (Tree,
      GPR2.Path_Name.Create_File ("./data/prepend.gpr"),
      Context => Ctx);
   Show_Tree (Tree);
   Tree.Unload;

   --  Equivalent to using the -aP switch
   Put_Line ("append.gpr, 'dep' in search path");
   Tree.Register_Project_Search_Path
     (GPR2.Path_Name.Create_Directory ("./data/dep"));
   GPR2.Project.Tree.Load_Autoconf
     (Tree,
      GPR2.Path_Name.Create_File ("./data/append.gpr"),
      Context => Ctx);
   Show_Tree (Tree);
   Tree.Unload;

   Put_Line ("prepend.gpr, 'dep' in search path");
   Tree.Register_Project_Search_Path
     (GPR2.Path_Name.Create_Directory ("./data/dep"));
   GPR2.Project.Tree.Load_Autoconf
     (Tree,
      GPR2.Path_Name.Create_File ("./data/prepend.gpr"),
      Context => Ctx);
   Show_Tree (Tree);
   Tree.Unload;

   Put_Line ("prj.gpr, 'dep' in search path");
   Tree.Register_Project_Search_Path
     (GPR2.Path_Name.Create_Directory ("./data/dep"));
   GPR2.Project.Tree.Load_Autoconf
     (Tree,
      GPR2.Path_Name.Create_File ("./data/prj.gpr"),
      Context => Ctx);
   Show_Tree (Tree);
   Tree.Unload;

end Test;
