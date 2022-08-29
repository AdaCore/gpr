--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Variable.Set;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;
   use GPR2.Project.Registry.Attribute;

   use all type GPR2.Project.Name_Values.Value_Kind;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;

      procedure Put_Attributes (Attrs : Attribute.Set.Object);

      --------------------
      -- Put_Attributes --
      --------------------

      procedure Put_Attributes (Attrs : Attribute.Set.Object) is
         Attr : Attribute.Object;
      begin
         for A in Attrs.Iterate loop
            Attr := Attribute.Set.Element (A);
            Text_IO.Put ("A:   " & String (Image (Attr.Name.Id.Attr)));

            if Attr.Has_Index then
               if Attr.Index.Is_Any_Index then
                  Text_IO.Put (" ()");
               else
                  Text_IO.Put (" [" & String (Attr.Index.Text)  & ']');
               end if;
            end if;

            Text_IO.Put (" " & (if Attr.Is_Default then '~' else '-') & ">");

            for V of Attribute.Set.Element (A).Values loop
               declare
                  Value : constant Value_Type := V.Text;
                  function No_Last_Slash (Dir : String) return String is
                    (if Dir'Length > 0 and then Dir (Dir'Last) in '\' | '/'
                     then Dir (Dir'First .. Dir'Last - 1) else Dir);
               begin
                  Text_IO.Put
                    (" "
                     & (if No_Last_Slash (Value)
                       = No_Last_Slash (Directories.Current_Directory)
                       then "{Current_Directory}" else Value));
               end;
            end loop;
            Text_IO.New_Line;
         end loop;
      end Put_Attributes;

   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         Put_Attributes (Prj.Attributes (With_Config => False));

         for A in Prj.Attributes.Filter (Object_Dir.Attr).Iterate loop
            Text_IO.Put
              ("A2:  " & Image (Attribute.Set.Element (A).Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of Attribute.Set.Element (A).Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;

         for A of Prj.Attributes.Filter (Object_Dir.Attr) loop
            Text_IO.Put_Line ("A3:  " & Image (A.Name.Id.Attr));
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" ->");

               if Element (V).Kind = Single then
                  Text_IO.Put (" " & Element (V).Value.Text);

               else
                  for Val of Element (V).Values loop
                     Text_IO.Put (" " & Val.Text);
                  end loop;
               end if;
               Text_IO.New_Line;
            end loop;
         end if;

         for P of Prj.Packages (With_Defaults => False,
	                        With_Config => False)
         loop
            Text_IO.Put_Line (Image (P));
            Put_Attributes (Prj.Attributes (Pack          => P,
                                            With_Defaults => False,
                                            With_Config   => False));
         end loop;
      end if;

   end Display;

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   Project.Tree.Load_Autoconf (Prj, Create ("demo.gpr"), Ctx);

   Display (Prj.Root_Project);
end Main;
