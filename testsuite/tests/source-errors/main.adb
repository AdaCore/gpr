--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit_Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put
              ("   Kind: "
               & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;

   exception
      when E : GPR2.Project_Error =>
         Text_IO.Put_Line
           ("BEFORE: Has unread Message: "
            & Prj.Log_Messages.Has_Element (Read => False)'Img);

         for C in Prj.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            declare
               M : constant Message.Object := Log.Element (C);
               F : constant String := M.Sloc.Filename;
               I : constant Natural := Strings.Fixed.Index
                     (F, "source-errors");
            begin
               Text_IO.Put_Line ("> " & F (I - 1 .. F'Last));
               Text_IO.Put_Line (M.Level'Img);

               if M.Sloc.Has_Source_Reference then
                  Text_IO.Put_Line (M.Sloc.Line'Img);
                  Text_IO.Put_Line (M.Sloc.Column'Img);
               end if;

               Text_IO.Put_Line (M.Message);
            end;
         end loop;

         --  Read also information message

         for C in Prj.Log_Messages.Iterate
           (True, True, True, True, True)
         loop
            null;
         end loop;

         Text_IO.Put_Line
           ("AFTER: Has unread Message: "
            & Prj.Log_Messages.Has_Element (Read => False)'Img);
         Text_IO.New_Line;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive := Strings.Fixed.Index (Filename, "source-errors");
   begin
      Text_IO.Put (" > " & Filename (I + 14 .. Filename'Last));
   end Output_Filename;

begin
   Check ("demo1.gpr");
   Check ("demo2.gpr");
   Check ("demo3.gpr");
   Check ("demo4.gpr");
   Check ("demo5.gpr");
end Main;
