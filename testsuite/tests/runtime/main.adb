with Ada.Text_IO;
with Ada.Strings.Fixed;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Display (Att : Project.Attribute.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
      K : Natural;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False,
                                  With_Config   => False)
         loop
            Text_IO.Put
              ("A:   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               declare
                  Val : constant GPR2.Value_Type := V.Text;
               begin
                  K := Strings.Fixed.Index (Val, "adainclude");
                  if K = 0 then
                     K := Strings.Fixed.Index (Val, "adalib");
                  end if;
                  if K = 0 then
                     Text_IO.Put (" " & Val (Val'First .. Val'Last));
                  else
                     Text_IO.Put (" <rtsdir>" & Val (K - 1 .. Val'Last));
                  end if;
               end;
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");

               declare
                  Vals : constant Containers.Source_Value_List :=
                           Element (V).Values;

                  procedure Output (Val : String) is
                  begin
                     K := Strings.Fixed.Index (Val, "adainclude");

                     if K = 0 then
                        Text_IO.Put (Val);
                     else
                        Text_IO.Put ("..." & Val (K - 1 .. Val'Last));
                     end if;
                  end Output;

               begin
                  for V of Vals loop
                     Output (V.Text);
                  end loop;
               end;

               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;

         for Pck of Prj.Packages (With_Defaults => False,
                                  With_Config   => False)
         Loop
            for A of Prj.Attributes (Pack => Pck,
                                     With_Defaults => False,
                                     With_Config   => False)
            loop
               Display (A);
            end loop;
         end loop;
      end if;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "adainclude";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I .. S'Last));
   end Output_Filename;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if Prj.Load (Opt, With_Runtime => True, Absent_Dir_Error => No_Error) then
      Prj.Update_Sources;

      Display (Prj.Root_Project);

      if Prj.Has_Configuration then
         Display (Prj.Configuration.Corresponding_View, Full => False);
      end if;

      if Prj.Has_Runtime_Project then
         Display (Prj.Runtime_Project, Full => True);

         for Source of Prj.Runtime_Project.Sources loop
            if Source.Path_Name.Base_Name = "memtrack" then
               Text_IO.Put_Line
                 ("!!! ERROR !!!: memtrack should not be listed as rts source");
            end if;

            if Strings.Fixed.Head
              (String (Source.Path_Name.Base_Name), 8) in
                "a-calend" | "a-strunb" | "a-tags  " | "a-contai"
                  | "a-string" | "a-strunb" | "a-tags  " | "a-uncdea" | "ada     "
            then
               declare
                  U : constant Optional_Name_Type :=
                        (if Source.Has_Units then Source.Unit.Name else "");
               begin
                  Output_Filename (Source.Path_Name.Value);

                  Text_IO.Set_Col (27);
                  Text_IO.Put
                    ("   Kind: " & Source.Kind'Image);
                  Text_IO.Put ("   unit: " & String (U));
                  Text_IO.New_Line;
               end;
            end if;
         end loop;
      end if;
   end if;
end Main;
