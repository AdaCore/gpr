with Ada.Environment_Variables;
with Ada.Text_IO;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.External;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Test (Project_Name : String) is
      procedure Check (Name : External_Name_Type) is
      begin
         if Tree.Configuration.Has_Externals
           and then Tree.Configuration.Corresponding_View.Context.Contains (Name)
         then
            Ada.Text_IO.Put_Line (String (Name) & " in externals");
         else
            Ada.Text_IO.Put_Line (String (Name) & " in externals");
         end if;
      end;
      procedure Print_Variable (Name : Name_Type) is
      begin
         if Tree.Root_Project.Variables.Contains (Name) then
            Ada.Text_IO.Put_Line
              (String (Name) & "="
               & Tree.Root_Project.Variable (Name).Value.Text);
         else
            Ada.Text_IO.Put_Line (String (Name) & " not found");
         end if;

      end Print_Variable;

      Opt : Options.Object;

   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Opt.Add_Switch (Options.P, Project_Name);

      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Check ("EXTERNAL_WITHOUT_DEFAULT");
         Check ("EXTERNAL_WITH_DEFAULT");
         Print_Variable ("Value1");
         Print_Variable ("Value2");
      end if;
   end Test;

begin
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
   Ada.Environment_Variables.Set ("EXTERNAL_WITHOUT_DEFAULT", "environment_value_1");
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
   Ada.Environment_Variables.Set ("EXTERNAL_WITH_DEFAULT", "environment_value_2");
   Test ("files/imported2");
   Test ("files/imported1");
   Test ("files/extending6");
   Test ("files/extending5");
   Test ("files/extending4");
   Test ("files/extending3");
   Test ("files/extending2");
   Test ("files/extending1");
   Test ("files/aggregate2");
   Test ("files/aggregate1");
end Main;
