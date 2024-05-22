with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;


procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   use GPR2;

begin
   Opt.Add_Switch (Options.P, "double_quote");

   if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      Ada.Text_IO.Put_Line
        ("Variable = " & Tree.Root_Project.Variable ("var").Value.Text);
      Ada.Text_IO.Put_Line (Tree.Root_Project.Variable ("var").Image);
      Ada.Text_IO.Put_Line
        ("Attribute = " &
           Tree.Root_Project.Attribute
           (Name  => GPR2.Project.Registry.Attribute.Compiler.Default_Switches,
            Index =>
              GPR2.Project.Attribute_Index.Create
                (GPR2.C_Language)).Values.First_Element.Text);
      Ada.Text_IO.Put_Line
        (Tree.Root_Project.Attribute
           (Name  => GPR2.Project.Registry.Attribute.Compiler.Default_Switches,
            Index =>
              GPR2.Project.Attribute_Index.Create (GPR2.C_Language)).Image);
   end if;
end Main;
