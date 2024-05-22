
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use Ada.Exceptions;
   use Ada.Text_IO;
   use GPR2;
   use GPR2.Project;

   procedure Test
     (Project_Name : Filename_Type;
      Filter       : GPR2.Simple_Name := "");

   procedure Test_Tree
     (Project_Name  : Filename_Type;
      Filter        : GPR2.Simple_Name := "";
      No_Subproject : Boolean := False);

   ----------
   -- Test --
   ----------

   procedure Test
     (Project_Name : Filename_Type;
      Filter       : GPR2.Simple_Name := "")
   is

      procedure Display (V : View.Object);
      --  Display source closure for V

      -------------
      -- Display --
      -------------

      procedure Display (V : View.Object) is
      begin
         Text_IO.Put_Line
           ("========== For view "
            & String (V.Name)
            & (if Filter = "" then "" else " " & String (Filter)));

         for S of V.Sources_Closure (Source => Filter) loop
            Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
         end loop;

         Text_IO.New_Line;
      end Display;

      Prj : Tree.Object;
      Ctx : Context.Object;

   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);
      Display (Prj.Root_Project);
   exception
      when others =>
         Prj.Log_Messages.Output_Messages;
   end Test;

   ---------------
   -- Test_Tree --
   ---------------

   procedure Test_Tree
     (Project_Name  : Filename_Type;
      Filter        : GPR2.Simple_Name := "";
      No_Subproject : Boolean := False)
   is

      procedure Display (T : Tree.Object);
      --  Display source closure for V

      -------------
      -- Display --
      -------------

      procedure Display (T : Tree.Object) is
      begin
         Text_IO.Put_Line
           ("========== For tree "
            & String (T.Root_Project.Name)
            & (if Filter = "" then "" else " " & String (Filter))
            & " Subproject " & Boolean'Image (not No_Subproject));

         for S of T.Sources_Closure
           (Source => Filter, No_Subproject => No_Subproject)
         loop
            Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
         end loop;

         Text_IO.New_Line;
      end Display;

      Prj : Tree.Object;
      Ctx : Context.Object;

   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);
      Display (Prj);
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         Prj.Log_Messages.Output_Messages;
   end Test_Tree;

begin
   Test ("projects/std.gpr");
   Test ("projects/app1.gpr");
   Test ("projects/app2.gpr");
   Test ("projects/app2.gpr", "main1.adb");
   Test ("projects/app2.gpr", "mainfull.adb");
   Test ("projects/lib1.gpr");
   Test ("projects/lib1.gpr", "pck2.ads");
   Test ("projects/lib2.gpr", "pck2.ads");
   Test ("projects/lib3.gpr", "pck2.ads");
   Test ("projects/lib3.gpr");
   Test ("projects/lib3.gpr", "pck34.ads");
   Test ("projects/app3.gpr");
   Test ("projects/lib4.gpr");

   Test_Tree ("projects/app1.gpr", No_Subproject => True);
   Test_Tree ("projects/app2.gpr", No_Subproject => True);

   Test_Tree ("projects/lib4.gpr", No_Subproject => True);
   Test_Tree ("projects/lib4.gpr", No_Subproject => False);

   Test_Tree ("projects/std2.gpr", No_Subproject => True);
   Test_Tree ("projects/std2.gpr", No_Subproject => False);
end Main;
