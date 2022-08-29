--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with GPR2.Unit;
with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use Ada.Exceptions;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Display (Att : Project.Attribute.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id.Attr));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
      K : Natural;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A of Prj.Attributes (With_Defaults => False,
                                  With_Config   => False)
         loop
            Text_IO.Put
              ("A:   " & Image (A.Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of A.Values loop
               declare
                  Val : constant GPR2.Value_Type := V.Text;
               begin
                  K := Strings.Fixed.Index (Val, "adainclude");
                  if K = 0 then
                     K := Strings.Fixed.Index (Val, "adalib");
                  end if;
                  if K = 0 then
                     Text_IO.Put (" " & Val (Val'First .. Val'Last));
                  else
                     Text_IO.Put (" ..." & Val (K - 1 .. Val'Last));
                  end if;
               end;
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");

               declare
                  Vals : constant Containers.Source_Value_List :=
                           Element (V).Values;

                  procedure Output (Val : String) is
                  begin
                     K := Strings.Fixed.Index (Val, "adainclude");

                     if K = 0 then
                        Text_IO.Put (Val);
                     else
                        Text_IO.Put ("..." & Val (K - 1 .. Val'Last));
                     end if;
                  end Output;

               begin
                  for V of Vals loop
                     Output (V.Text);
                  end loop;
               end;

               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;

         for Pck of Prj.Packages (With_Defaults => False,
                                  With_Config   => False)
         Loop
            Text_IO.Put_Line (" " & Image (Pck));

            for A of Prj.Attributes (Pack => Pck,
                                     With_Defaults => False,
                                     With_Config   => False)
            loop
               Display (A);
            end loop;
         end loop;
      end if;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "adainclude");
   begin
      Text_IO.Put (" > " & Filename (I .. Filename'Last));
   end Output_Filename;

   Gpr : constant Path_Name.Object := Create ("demo.gpr");
   Des : constant Configuration.Description :=
           Configuration.Create (Language => Ada_Language);
   KB  : GPR2.KB.Object := GPR2.KB.Create (GPR2.KB.Default_Flags);
   Cnf : constant Configuration.Object :=
           Configuration.Create
             (Configuration.Description_Set'(1 => Des), "all", Gpr,
              Base => KB);

   Prj : Project.Tree.Object;
   Ctx : Context.Object;

begin
   if Cnf.Has_Messages then
      for M of Cnf.Log_Messages loop
         declare
            F : constant String := M.Sloc.Filename;
            I : constant Natural := Strings.Fixed.Index (F, "runtime");
         begin
            Text_IO.Put_Line ("> " & F (I - 1 .. F'Last));
            Text_IO.Put_Line (M.Level'Img);
            Text_IO.Put_Line (M.Format);
         end;
      end loop;
   end if;

   Project.Tree.Load (Prj, Gpr, Ctx, Config => Cnf);

   Display (Prj.Root_Project);

   if Prj.Has_Configuration then
      Display (Prj.Configuration.Corresponding_View, Full => False);
   end if;

   if Prj.Has_Runtime_Project then
      Display (Prj.Runtime_Project, Full => True);

      for Source of Prj.Runtime_Project.Sources loop
         if Strings.Fixed.Head
           (String (Source.Path_Name.Base_Name), 8) in
           "a-calend" | "a-strunb" | "a-tags  " | "a-calend" | "a-contai"
             | "a-string" | "a-strunb" | "a-tags  " | "a-uncdea" | "ada     "
         then
            declare
               U : constant Optional_Name_Type :=
                     (if Source.Has_Units then Source.Unit_Name else "");
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Set_Col (27);
               Text_IO.Put
                 ("   Kind: " & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));
               Text_IO.Put ("   unit: " & String (U));
               Text_IO.New_Line;
            end;
         end if;
      end loop;
   end if;

exception
   when E : GPR2.Project_Error =>
      Text_Io.Put_Line (Exception_Information (E));

      if Prj.Has_Messages then
         Text_IO.Put_Line ("Messages found:");

         for M of Prj.Log_Messages.all loop
            declare
               F : constant String := M.Sloc.Filename;
               I : constant Natural := Strings.Fixed.Index (F, "runtime");
            begin
               Text_IO.Put_Line ("> " & F (I - 1 .. F'Last));
               Text_IO.Put_Line (M.Level'Img);
               Text_IO.Put_Line (M.Format);
            end;
         end loop;
      end if;
end Main;
