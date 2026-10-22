--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Source_Base.Ada_Parser;
with GPR2.Build.Unit_Info;
with GPR2.File_Readers;
with GPR2.Path_Name;
with Test_Reader;

procedure Main is
   use Ada.Command_Line;
   use GPR2;

   Ref : File_Readers.File_Reader_Reference :=
           File_Readers.No_File_Reader_Reference;
begin
   if Argument (1) /= "mapped" then
      Ref := Test_Reader.Create
        (Test_Reader.Failure_Kind'Value (Argument (1)));
   end if;

   for Attempt in 1 .. 3 loop
      declare
         Source : Build.Source_Base.Object'Class := Build.Source_Base.Create
           (Filename         => Path_Name.Create_File
                                  (Filename_Type (Argument (2))),
            Language         => Ada_Language,
            Kind             => S_Body,
            Timestamp        => Ada.Calendar.Clock,
            Tree_Db          => null,
            Naming_Exception => False,
            Source_Dir_Idx   => 1,
            From_Src_Subdirs => False);
         Success : Boolean;
      begin
         if Argument (3) = "parsed" then
            Source.Update_Unit
              (Build.Unit_Info.Create
                 (Unit_Name => "PREVIOUS", Index => No_Index,
                  Kind => S_Body, Parsed => True));
         end if;

         Build.Source_Base.Ada_Parser.Compute (Ref, Source, True, Success);
         if Success
           or else (Source.Has_Unit_At (No_Index)
                    and then Source.Unit.Is_Parsed)
         then
            raise Program_Error with "read failure left a parsed source";
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("unparsed");
end Main;
