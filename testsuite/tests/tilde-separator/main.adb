with Ada.Text_IO;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Main is
   Ctx  : GPR2.Context.Object;
   Tree : GPR2.Project.Tree.Object;
   package Set is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   package Sort is new Set.Generic_Sorting;
   List : Set.List;

   procedure Print_And_Clean_List;
   procedure Print_And_Clean_List is
   begin
      Sort.Sort (List);
      for S of List loop
         Ada.Text_IO.Put_Line (To_String (S));
      end loop;
      List.Clear;
   end Print_And_Clean_List;

begin
   Tree.Load_Autoconf
     (GPR2.Path_Name.Create_File ("default.gpr"), Ctx);

   Tree.Update_Sources;

   for Source of Tree.Root_Project.Sources loop
      List.Append (To_Unbounded_String (String (Source.Path_Name.Name)));
   end loop;
   Print_And_Clean_List;

   for Unit of Tree.Root_Project.Units loop
      List.Append (To_Unbounded_String (String (Unit.Name)));
   end loop;
   Print_And_Clean_List;

end Main;
