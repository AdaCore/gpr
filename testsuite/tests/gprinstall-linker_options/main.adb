------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack;
with GPR2.Project.Registry;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;

procedure Main is
   Tree1        : GPR2.Project.Tree.Object;
   Tree2        : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   Library_Options : GPR2.Containers.Source_Value_List;
   Linker   : GPR2.Project.Pack.Object;
   Linker_Options  : GPR2.Containers.Source_Value_List;

   procedure Print_Attributes
     (Tree : GPR2.Project.Tree.Object;
      Pack : GPR2.Optional_Package_Id;
      Name : GPR2.Optional_Attribute_Id) is
      Attributes : GPR2.Project.Attribute.Set.Object;
      use GPR2;
      Header     : String := (if Pack = No_Package
                              then Image (Name)
                              else Image (Pack) & "." & Image (Name));
   begin
      if Pack = No_Package then
         Attributes := Tree.Root_Project.Attributes (Name);
      elsif Tree.Root_Project.Has_Packages (Pack) then
         Attributes := Tree.Root_Project.Pack (Pack).Attributes (Name);
      end if;
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
   Tree1.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension ("prj/lib2"),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   Library_Options := Tree1.Root_Project.Attribute
     (Name  => GPR2.Project.Registry.Attribute.Library_Options).Values;
   Tree2.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension ("inst/share/gpr/lib2"),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   Linker := Tree2.Root_Project.Pack
     (Name           => GPR2.Project.Registry.Pack.Linker,
      Check_Extended => True);
   Linker_Options := Linker.Attribute
     (Name  => GPR2.Project.Registry.Attribute.Linker_Options).Values;
   for Library_Option of Library_Options loop
      declare
         Found : Boolean := False;
      begin
         for Linker_Option of Linker_Options loop
            if Linker_Option.Text = Library_Option.Text then
               Found := True;
               exit;
            end if;
         end loop;
         if Not Found then
            Print_Attributes
              (Tree => Tree1,
               Pack => GPR2.No_Package,
               Name => GPR2.Project.Registry.Attribute.Library_Options);
            Print_Attributes
              (Tree => Tree2,
               Pack => GPR2.Project.Registry.Pack.Linker,
               Name => GPR2.Project.Registry.Attribute.Linker_Options);
            return;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("OK");
end Main;
