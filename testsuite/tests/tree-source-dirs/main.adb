with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Source;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;

   procedure Test (File : String;
                   View_Path : GPR2.Path_Name.Object);

   procedure Test (File : String;
                   View_Path : GPR2.Path_Name.Object) is
      Project_Tree : Project.Tree.Object;
      Ctx          : constant Context.Object := Context.Empty;

      Paths  : GPR2.Path_Name.Set.Object;

      function "<" (L, R : GPR2.Path_Name.Object) return Boolean
      is (L.Value < R.Value);
      package Sort is new GPR2.Path_Name.Set.Set.Generic_Sorting;

      procedure Check (View : GPR2.Project.View.Object;
                       Externally_Built : Boolean);

      procedure Check (View : GPR2.Project.View.Object;
                       Externally_Built : Boolean) is
         Header : constant String :=
                    (if View.Is_Defined then
                        "testing " & File & " View=" &
                        String (View.Path_Name.Name) &
                        " Externally_Built= " & Externally_Built'Image

                     else
                        "testing " & File & " View=Root_Project" &
                        " Externally_Built= " & Externally_Built'Image);

         procedure Do_Action (Source : Project.Source.Object);

         procedure Do_Action (Source : Project.Source.Object) is
         begin
            Paths.Append (Source.Path_Name);
         end Do_Action;

         procedure Print_Paths;

         procedure Print_Paths is
         begin
            Sort.Sort (Paths);
            for Path of Paths loop
               Ada.Text_IO.Put_Line (Path.Value);
            end loop;
            Paths.Clear;
         end Print_Paths;

      begin
         Ada.Text_IO.Put_Line (Header);

         Paths := Project_Tree.Source_Directories
           (View             => View,
            Externally_Built => Externally_Built);

         Print_Paths;

         Ada.Text_IO.Put_Line
           (Header & " Language=All");

         Project_Tree.For_Each_Source
           (View             => View,
            Action           => Do_Action'Access,
            Externally_Built => Externally_Built);

         Print_Paths;

         Ada.Text_IO.Put_Line
           (Header & " Language=C");

         Project_Tree.For_Each_Source
           (View             => View,
            Action           => Do_Action'Access,
            Language         => GPR2.C_Language,
            Externally_Built => Externally_Built);

         Print_Paths;


      end Check;

      use GPR2.Log;

      View : GPR2.Project.View.Object;

      use GPR2.Path_Name;
   begin
      Ada.Text_IO.Put_Line ("loading " & File);

      Project_Tree.Load_Autoconf
        (Filename          => Project.Create (GPR2.Filename_Type (File)),
         Context           => Ctx);

      Check (View, False);

      if View_Path.Is_Defined then
         for V of Project_Tree.Ordered_Views loop
            if V.Path_Name.Simple_Name = View_Path.Simple_Name then
               View := V;
               exit;
            end if;
         end loop;
      end if;

      Check (View, True);

      Project_Tree.Unload;

   exception
      when Project_Error =>
         for M of Project_Tree.Log_Messages.all loop
            Ada.Text_IO.Put_Line (M.Format);
         end loop;
   end Test;

begin

   Test ("files/abstract1.gpr", GPR2.Path_Name.Undefined);
   Test ("files/aggregating.gpr",
         GPR2.Path_Name.Create_File ("files/aggregated.gpr"));
   Test ("files/agg_lib.gpr", GPR2.Path_Name.Undefined);

end Main;
