with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable;
with GPR2.Project.Typ;
with GPR2.Project.Pretty_Printer;
with GPR_Parser.Analysis;

procedure Main is
   Tree     : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Variable (Variable : GPR2.Project.Variable.Object) is
      use GPR2.Project.Registry.Attribute;
   begin
      Ada.Text_IO.Put(String (Variable.Name.Text) & ":" & Variable.Kind'Img & "=");
      if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
         Ada.Text_IO.Put_Line (Variable.Value.Text);
      else
         for V of Variable.Values loop
            Ada.Text_IO.Put (V.Text & ";");
         end loop;
         Ada.Text_IO.Put_Line ("");
      end if;
   end Print_Variable;

   procedure Load (Project_Name : String) is
      Opt : Options.Object;
      Res : Boolean;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (Options.P, Project_Name);
      Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
   end Load;

   procedure Test_Pretty_Printer (Project_Name : GPR2.Filename_Type) is
      Ctx  : constant GPR_Parser.Analysis.Analysis_Context :=
               GPR_Parser.Analysis.Create_Context;
      Path : constant GPR2.Path_Name.Object :=
               GPR2.Path_Name.Create_File
                 (GPR2.Project.Ensure_Extension (Project_Name));
      Unit : constant GPR_Parser.Analysis.Analysis_Unit :=
               GPR_Parser.Analysis.Get_From_File (Ctx, Path.String_Value);
      PP   : GPR2.Project.Pretty_Printer.Object;

   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      PP.Pretty_Print (Analysis_Unit => Unit);
      Ada.Text_IO.Put_Line (PP.Result);
   end Test_Pretty_Printer;

begin
   Load ("prj.gpr");
   Print_Variable (Tree.Root_Project.Variable (Name => "V1"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V2"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V3"));
   Print_Variable (Tree.Root_Project.Variable (Name => "V4"));
   declare
      Typ : GPR2.Project.Typ.Object :=
              Tree.Root_Project.Variable (Name => "V5").Typ;
   begin
      Ada.Text_IO.Put_Line (String (Typ.Values.First_Element.Text));
   end;

   --  check invalid ref is package renaming & extending
   Load ("bad1.gpr");
   Load ("bad2.gpr");
   Load ("bad3.gpr");
   Load ("bad4.gpr");

   --  check package renaming/extending and attribute/variable/type refs
   --  pretty pinting
   Test_Pretty_Printer ("prj.gpr");
   Test_Pretty_Printer ("build-settings.gpr");
end Main;
