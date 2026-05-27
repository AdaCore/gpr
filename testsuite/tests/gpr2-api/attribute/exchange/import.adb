with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.OS.Process;

with GPR2.Containers;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Exchange;
with GPR2.Project.Registry.Pack;

with Test;

procedure Import is

   Exported_Content : Ada.Strings.Unbounded.Unbounded_String;
   Imported_Content : Ada.Strings.Unbounded.Unbounded_String;
   Empty_List : GPR2.Containers.Package_Id_List;

   procedure Imported (Item : String) is
   begin
      Ada.Strings.Unbounded.Append (Imported_Content, Item);
   end Imported;

   use type Ada.Strings.Unbounded.Unbounded_String;
begin

   Ada.Text_IO.Put_Line ("********************");
   Ada.Text_IO.Put_Line ("* Exported_Registy *");
   Ada.Text_IO.Put_Line ("********************");

   Exported_Content := Test.Get_Exported_Registry_From_Tool ("export");
   GPR2.Project.Registry.Exchange.Import
     (Definitions => Exported_Content,
      Included    => Empty_List,
      Excluded    => GPR2.Project.Registry.Pack.Predefined_Packages);

   GPR2.Project.Registry.Exchange.Export
     (Included => GPR2.Project.Registry.Pack.All_Packages,
      Excluded => GPR2.Project.Registry.Pack.Predefined_Packages,
      Format   => GPR2.Project.Registry.Exchange.K_JSON,
      Output   => Imported'Access);

   if Exported_Content /= Imported_Content then
      Ada.Text_IO.Put_Line ("Import/Export failure !!!");
   end if;

   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Exported_Content));

   Exported_Content := Test.Get_Exported_Registry_From_Tool ("check_prove");
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Exported_Content));
   GPR2.Project.Registry.Exchange.Import
     (Definitions => Exported_Content,
      Included    => Empty_List,
      Excluded    => GPR2.Project.Registry.Pack.Predefined_Packages);

   Ada.Text_IO.Put_Line ("********************");
   Ada.Text_IO.Put_Line ("* Imported Registy *");
   Ada.Text_IO.Put_Line ("********************");

   GPR2.Project.Registry.Exchange.Export
     (Included => GPR2.Project.Registry.Pack.All_Packages,
      Excluded => GPR2.Project.Registry.Pack.Predefined_Packages,
      Format   => GPR2.Project.Registry.Exchange.K_JSON);

   declare
      Args_Vector : GNATCOLL.OS.Process.Argument_List;
      Output      : Ada.Strings.Unbounded.Unbounded_String;
      Dummy       : Integer;
      Definitions : GPR2.Project.Registry.Attribute.Def;

      package PRA renames GPR2.Project.Registry.Attribute;
      use type GPR2.Optional_Attribute_Id;
   begin
      Args_Vector.Append (Test.Locate_Exec_On_Path("export"));

      Output := GNATCOLL.OS.Process.Run
        (Args        => Args_Vector,
         Stdin       => GNATCOLL.OS.Process.FS.Null_FD,
         Stderr      => GNATCOLL.OS.Process.FS.To_Stdout,
         Status      => Dummy);

      GPR2.Project.Registry.Exchange.Import
        (Definitions => Output,
         Included    => GPR2.Containers.Package_Id_Type_List.Empty_Set,
         Excluded    => GPR2.Containers.Package_Id_Type_List.Empty_Set);


      Ada.Text_IO.Put_Line ("Test Inherit_From_Extended_Type support");
      for Inherited_Mode in PRA.Inherit_From_Extended_Type loop
         Definitions :=
           PRA.Get
             ((GPR2.Project_Level_Scope,
              +GPR2.Optional_Name_Type ("Attribute_" & Inherited_Mode'Img)));
           Ada.Text_IO.Put_Line
              (" expected:" & Inherited_Mode'Img & " actual:" &
                 Definitions.Inherit_From_Extended'Img);
      end loop;
   end;

end Import;
