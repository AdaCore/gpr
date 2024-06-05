with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Log;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;

procedure Main is

   use GPR2;

   procedure Test (Filename : String) is
      Tree    : GPR2.Project.Tree.Object;
      Opt     : Options.Object;

      procedure Print_Attributes (Name : Q_Attribute_Id) is
         Attributes : GPR2.Project.Attribute.Set.Object;
         use GPR2;
         Header     : String := (if Name.Pack = Project_Level_Scope
                                 then Image (Name.Attr)
                                 else Image (Name.Pack) & "."
                                 & Image (Name.Attr));
      begin
         Attributes := Tree.Root_Project.Attributes
           (Name          => Name,
            With_Defaults => False,
            With_Config   => False);

         for A of Attributes loop
            declare
               Attribute : GPR2.Project.Attribute.Object := A;
               use GPR2.Project.Registry.Attribute;
            begin
               Ada.Text_IO.Put (Header);
               if Attribute.Has_Index then
                  Ada.Text_IO.Put ( "(" & Attribute.Index.Text & ")");
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

   begin
      Opt.Add_Switch (Options.P, Filename);
      Ada.Text_IO.Put_Line (Filename);

      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Print_Attributes ((+"", +"Runtime"));
         Print_Attributes ((+"Compiler", +"Switches"));
      end if;
   end Test;

begin
   Test ("check1.gpr");
   Test ("check2a.gpr");
   Test ("check2b.gpr");
   Test ("check2c.gpr");
   Test ("check3a.gpr");
   Test ("check3b.gpr");
   Test ("check4a.gpr");
   Test ("check4b.gpr");
   Test ("check5a.gpr");
   Test ("check5b.gpr");
   Test ("check6.gpr");
end main;
