--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GPR2.Message;

package body GPR2.Project.Source_Files is

   ----------
   -- Read --
   ----------

   procedure Read
     (Tree     : not null access Project.Tree.Object;
      Filename : GPR2.Path_Name.Full_Name;
      Attr     : Source_Reference.Value.Object;
      Set      : in out Source_Set.Set)
   is
      use Ada.Strings;
      use type Ada.Strings.Maps.Character_Set;

      Skip_Set : constant Strings.Maps.Character_Set :=
                   Maps.Constants.Control_Set or Maps.To_Set (" ");
      F        : Text_IO.File_Type;
   begin
      Text_IO.Open (F, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (F) loop
         declare
            use GNATCOLL.Utils;

            Line     : constant String :=
                         Fixed.Trim
                           (Text_IO.Get_Line (F),
                            Skip_Set, Skip_Set);
            Position : Source_Set.Cursor;
            Inserted : Boolean;

         begin
            if Line /= "" and then not Starts_With (Line, "--") then
               if Has_Directory_Separator (Line) then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "file name cannot include directory information ("""
                        & Line & """)",
                        Attr));
               else
                  Set.Insert (Filename_Type (Line), Position, Inserted);
               end if;
            end if;
         end;
      end loop;

      Text_IO.Close (F);
   end Read;

end GPR2.Project.Source_Files;
