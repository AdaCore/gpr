with Ada.Text_IO;

with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Source_Reference.Value;

procedure Main is
   Tree: GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Attributes (Name : Q_Attribute_Id) is
      Attributes : GPR2.Project.Attribute.Set.Object;
      Header     : constant String :=
                     (if Name.Pack = Project_Level_Scope
                      then Image (Name.Attr)
                      else Image (Name.Pack)
                      & "." & Image (Name.Attr));
   begin
      Attributes := Tree.Root_Project.Attributes
        (Name          => Name,
         With_Defaults => False,
         With_Config   => False);

      for A of Attributes loop
         declare
            Attribute : constant GPR2.Project.Attribute.Object := A;
            use GPR2.Project.Registry.Attribute;
         begin
            Ada.Text_IO.Put (Header);
            if Attribute.Has_Index then
               Ada.Text_IO.Put ("(" & Attribute.Index.Text & ")");
            end if;
            Ada.Text_IO.Put ("=");
            if Attribute.Kind = GPR2.Project.Registry.Attribute.Single then
               Ada.Text_IO.Put ("""");
               Ada.Text_IO.Put (String (Attribute.Value.Text));
               Ada.Text_IO.Put ("""");
            else
               declare
                  Separator : Boolean := False;
                  Value     : GPR2.Source_Reference.Value.Object;
               begin
                  Ada.Text_IO.Put ("(");
                  for V of Attribute.Values loop
                     Value := V;
                     if Separator then
                        Ada.Text_IO.Put (",");
                     end if;
                     Separator := True;
                     Ada.Text_IO.Put ("""");
                     Ada.Text_IO.Put (String (Value.Text));
                     Ada.Text_IO.Put ("""");
                  end loop;
                  Ada.Text_IO.Put (")");
               end;
            end if;
            Ada.Text_IO.Put_Line ("");
         end;

      end loop;
   end Print_Attributes;

   procedure Test (Project_Name : String) is
      Opt : Options.Object;
   begin
      Ada.Text_IO.Put_Line ("testing " & Project_Name);
      Tree.Unload;
      Opt.Add_Switch (Options.P, Project_Name);
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Print_Attributes ((+"Compiler", +"Default_Switches"));
      end if;
   end Test;

begin
   Test ("p3.gpr");
   Test ("p4.gpr");
end Main;
