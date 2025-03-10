------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                                  F M                                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 1995-2023, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it and/or modify it         --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
pragma Warnings (Off, """System.Parameters"" is An Internal GNAT Unit");
pragma Warnings (Off,
                 "use of This Unit is Non-Portable and Version-dependent");
with System.Parameters;
pragma Warnings (On, """System.Parameters"" is An Internal GNAT Unit");
pragma Warnings (On,
                 "use of This Unit is Non-Portable and Version-dependent");
package body FM is

   Init_Error : exception;

   procedure Read_Init_File;

   procedure Read_Init_File is
      F : File_Type;
      File_Ok : Boolean := True;
   begin

      --  Open the file if it is there otherwise, nothing needs to be done

      begin
         Open (F, In_File, "fm.ini");
      exception
         when others => File_Ok := False;
      end;

      --  If the file is available, get the number of workers

      if File_Ok then
         begin
            Ada.Integer_Text_IO.Get (F, NB_Workers);
         exception
            when others =>
               Put      ("fm.ini incorrectly formatted: it must contain");
               Put_Line ("1 or 2 integer values");
               Close (F);
               raise Init_Error;
         end;
      end if;

      --  If there is another integer value in the file, this is the
      --  default stack size. Read in a temp to avoid clobbering the
      --  default value in case of failure

      if File_Ok then
         declare
            Temp : Integer;
         begin
            Ada.Integer_Text_IO.Get (F, Temp);
            Worker_Stack_Size := Temp;
         exception
            when others => null;
         end;
         Close (F);
      end if;
   end Read_Init_File;

   --------------
   -- Nb_Tasks --
   --------------

   function Nb_Tasks return Fortran_Integer is
   begin
      return Fortran_Integer (NB_Workers);
   end Nb_Tasks;

   ----------------------
   -- Set_Waiting_Time --
   ----------------------

   procedure Set_Waiting_Time (T : Real) is
   begin
      Waiting_Time := Duration (T);
   end Set_Waiting_Time;

begin
   Worker_Stack_Size := Integer (System.Parameters.Default_Stack_Size);
   Read_Init_File;
   Ada.Integer_Text_IO.Put (NB_Workers);
   New_Line;
   Ada.Integer_Text_IO.Put (Worker_Stack_Size);
   New_Line;
end FM;
