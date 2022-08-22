--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Source;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display_Source (Src : GPR2.Source.Object'Class);
   procedure Test_Prj (Fname : Filename_Type);

   procedure Display_Source (Src : GPR2.Source.Object'Class) is
   begin
      Text_IO.Put_Line (String (Src.Path_Name.Simple_Name) & ": " & Src.Kind'Image);
   end Display_Source;

   procedure Test_Prj (Fname : Filename_Type)
   is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;

   begin
      Text_IO.Put_Line ("GPR file: " & String (Fname));
      Project.Tree.Load (Prj, Create (Fname), Ctx);
      begin
         Prj.Update_Sources;
         for V of reverse Prj.Ordered_Views loop
            Text_IO.Put_Line (String (V.Name));
            for S of V.Sources loop
               Display_Source (S);
            end loop;
         end loop;
      exception
         when Project_Error =>
            Text_IO.Put_Line ("Messages found:");

            for C in Prj.Log_Messages.Iterate
              (False, True, True, True, True)
            loop
               declare
                  use Ada.Strings;
                  use Ada.Strings.Fixed;
                  DS  : Character renames GNAT.OS_Lib.Directory_Separator;
                  M   : constant Message.Object := Log.Element (C);
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                          Fixed.Index (Mes, DS & "source_files" & DS);
               begin
                  if L /= 0 then
                     Text_IO.Put_Line
                       (Replace_Slice
                          (Mes,
                           Fixed.Index
                             (Mes (1 .. L), """", Going => Backward) + 1,
                           L - 1,
                           "<path>"));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
      end;

      Prj.Unload;
   end Test_Prj;

begin
   Test_Prj ("data/prj.gpr");
   Test_Prj ("data/prj1.gpr");
   Ada.Environment_Variables.Set
     ("SOURCE_LIST_FILE_PATH",
      GPR2.Path_Name.Create_File("data/sources_absolute.lst").Value);
   Test_Prj ("data/prj1.gpr");
   Test_Prj ("data/prj2.gpr");
   Test_Prj ("data/prj3.gpr");
end Main;
