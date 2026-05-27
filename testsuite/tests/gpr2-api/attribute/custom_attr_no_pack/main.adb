with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Project.Tree;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

procedure Main is
   package Attr renames GPR2.Project.Registry.Attribute;
   package Pack renames GPR2.Project.Registry.Pack;
   Ctx : GPR2.Context.Object;
   PT  : GPR2.Project.Tree.Object;

   Qualified_Name : GPR2.Q_Attribute_Id
     := (Pack => GPR2."+" ("Foo"),
         Attr => GPR2."+" ("Bar"));

   procedure Case_1 is
   begin
      Attr.Add
        (Name                 => Qualified_Name,
         Index_Type           => Attr.No_Index,
         Value                => Attr.Single,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Attr.Everywhere);
   exception
      when Ada.Assertions.Assertion_Error =>
         Put_Line (Item => "raised ADA.ASSERTIONS.ASSERTION_ERROR :"
                   & " failed precondition");
      when others =>
         Put_Line (Item => "Unexpected exception");
   end Case_1;

   procedure Case_2 is
   begin
      Pack.Add (Name     => Qualified_Name.Pack,
                Projects => Pack.Everywhere);
      Attr.Add
        (Name                 => Qualified_Name,
         Index_Type           => Attr.No_Index,
         Value                => Attr.Single,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Attr.Everywhere);
   end Case_2;
begin
   Put_Line (Item => "Case 1 - Adding a custom attribute without pack");
   Case_1;
   if not Pack.Exists (Name => GPR2."+" ("Foo"))
     and then not Attr.Exists (Q_Name => Qualified_Name)
   then
      Put_Line (Item => "Test OK !");
   else
      Put_Line (Item => "Test KO !");
   end if;

   Put_Line (Item => "Case 2 - Adding a custom attribute with pack");
   Case_2;
   if Pack.Exists (Name => GPR2."+" ("Foo"))
     and then Attr.Exists (Q_Name => Qualified_Name)
   then
      Put_Line (Item => "Test OK !");
   else
      Put_Line (Item => "Test KO !");
   end if;
end Main;
