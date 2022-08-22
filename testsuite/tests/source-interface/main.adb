--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Unit;
with GPR2.Context;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

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

      procedure List_Sources (View : Project.View.Object);

      ------------------
      -- List_Sources --
      ------------------

      procedure List_Sources (View : Project.View.Object) is
      begin
         Text_IO.New_Line;
         Text_IO.Put_Line ("---------- ALL");

         for Source of View.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit_Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Put (",language: " & Image (Source.Language));

               Text_IO.Put
                 (",Kind: "
                  & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

               if U /= "" then
                  Text_IO.Put (",unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;

         Text_IO.New_Line;
         Text_IO.Put_Line ("---------- INTERFACE ONLY");

         for Source of View.Sources (Interface_Only => True) loop
            declare
               U : constant Optional_Name_Type := Source.Unit_Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Put
                 (",Kind: "
                  & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

               if U /= "" then
                  Text_IO.Put (",unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;

         Text_IO.New_Line;
         Text_IO.Put_Line ("---------- COMPILABLE ONLY");

         for Source of View.Sources (Compilable_Only => True) loop
            declare
               U : constant Optional_Name_Type := Source.Unit_Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Put
                 (",Kind: "
                  & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

               if U /= "" then
                  Text_IO.Put (",unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end List_Sources;

      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;

      procedure Print_Messages (Info : Boolean) is
      begin
         if Prj.Log_Messages.Has_Element
           (Information => Info, Lint => False)
         then
            Text_IO.Put_Line ("Messages found:");

            for J in Prj.Log_Messages.Iterate (Information => Info) loop
               declare
                  M : constant Message.Object := Prj.Log_Messages.all (J);
               begin
                  Text_IO.Put_Line (M.Format);
               end;
            end loop;
         end if;
      end Print_Messages;

   begin
      Project.Tree.Load (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));
      Print_Messages (False);

      List_Sources (View);
      Print_Messages (False);
      Prj.Unload;
   exception
      when GPR2.Project_Error =>
         Print_Messages (True);
         Prj.Unload;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
   begin
      Text_IO.Put (" > " & Filename);
   end Output_Filename;

begin
    Check ("demo.gpr");
    Check ("demo2.gpr");
end Main;
