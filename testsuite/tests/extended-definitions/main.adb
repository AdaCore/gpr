with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Text_IO.Put
           ("A:   " & Image (A.Name.Id));
         Text_IO.Put (" ->");

         for V of A.Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;

      if Prj.Has_Variables then
         for V in Prj.Variables.Iterate loop
            Text_IO.Put ("V:   " & String (Key (V)));
            Text_IO.Put (" -> ");
            Text_IO.Put (Element (V).Value.Text);
            Text_IO.New_Line;
         end loop;
      end if;

      for Source of Prj.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit.Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put ("   Kind: "
                         & Source.Kind'Image);

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "extended-definitions";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Opt1, Opt2 : Options.Object;

begin
   Opt1.Add_Switch (Options.P, "prj1");
   Opt2.Add_Switch (Options.P, "prj2");

   if Prj1.Load (Opt1, Absent_Dir_Error => No_Error)
     and then Prj2.Load (Opt2, Absent_Dir_Error => No_Error)
   then
      Prj1.Update_Sources;
      Prj2.Update_Sources;

      Text_IO.Put_Line ("**************** Iterator Prj1");

      for P of Prj1 loop
         Display (P, Full => False);
      end loop;

      Text_IO.Put_Line ("**************** Iterator Prj2");

      for C in Prj2.Iterate
        (Kind => (I_Project | I_Imported | I_Recursive => True, others => False))
      loop
         Display (Project.Tree.Element (C), Full => False);
      end loop;
   end if;
end Main;
