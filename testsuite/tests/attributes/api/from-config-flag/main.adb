with GPR2;              use GPR2;
with GPR2.Project.Tree; use GPR2.Project.Tree;
with GPR2.Options;      use GPR2.Options;
with Ada.Text_IO;
with GPR2.Project.Attribute;
with GPR2.Path_Name;    use GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

procedure Main is
   Tree       : GPR2.Project.Tree.Object;
   Opt        : GPR2.Options.Object;
   Main_Found : Integer := 0;
   Test_Found : Integer := 0;

   package PRA renames GPR2.Project.Registry.Attribute;
   use GPR2;
begin
   Opt.Add_Switch (Options.P, "simple");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      for Prj of Tree.Ordered_Views loop
         if Prj.Name = "simple" then
            Ada.Text_IO.Put_Line ("Project: " & String (Prj.Name));

            Ada.Text_IO.Put_Line ("Attributes WITH DEFAULT, WITH CONFIG:");

            for Att of
              Prj.Attributes (With_Defaults => True, With_Config => True)
            loop
               if Att.Name.Id = PRA.Inherit_Source_Path then
                  Ada.Text_IO.Put_Line
                    ("* "
                     & GPR2.Project.Attribute.Image (Att)
                     & " from_config="
                     & Att.Is_From_Config'Image);
               end if;
            end loop;

            Ada.Text_IO.Put_Line ("");
            Ada.Text_IO.Put_Line ("Attributes WITH DEFAULT, WITHOUT CONFIG:");

            for Att of
              Prj.Attributes (With_Defaults => True, With_Config => False)
            loop
               if Att.Name.Id = PRA.Inherit_Source_Path then
                  Ada.Text_IO.Put_Line
                    ("* "
                     & GPR2.Project.Attribute.Image (Att)
                     & " from_config="
                     & Att.Is_From_Config'Image);
               end if;
            end loop;
         end if;
      end loop;
   end if;
end Main;
