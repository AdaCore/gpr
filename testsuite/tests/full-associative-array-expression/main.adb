with Ada.Exceptions;
with Ada.Text_IO;

with GPR2.Context;
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

   procedure Test (Filename : GPR2.Filename_Type) is
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;

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
      begin
         Ada.Text_IO.Put_Line (String (Filename));
         Tree.Load_Autoconf
           (Filename  => GPR2.Path_Name.Create_File (Filename),
            Context   => Context);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
      end;
      Print_Attributes ((+"", +"Runtime"));
      Print_Attributes ((+"Compiler", +"Switches"));
      if Tree.Has_Messages  then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
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
