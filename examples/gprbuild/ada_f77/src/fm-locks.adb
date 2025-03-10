------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                             F M  . L O C K S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $                             --
--                                                                          --
--           Copyright (C) 1995-1998 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

package body FM.Locks is
   protected body Lock_Manager is
      entry Barrier (for J in 1..NB_Sync) when Locks (J) = 0 is
      begin
         Available (J) := True;
      end;

     procedure Get_Synch (Synch : in out Integer) is
      begin
         for I in Locks'range loop
            if Available (I)  then
               Available (I) := False;
               Synch := I;
               return;
            end if;
         end loop;

         raise Program_Error;
      end Get_Synch;

      procedure Inc (Synch : Integer; Val : Integer) is
      begin
         Locks (Synch) := Locks (Synch) + Val;
      end Inc;

      entry Lock when not Global_Lock is
      begin
         Global_Lock := True;
      end Lock;

      procedure Unlock is
      begin
         Global_Lock := False;
      end Unlock;

      entry Wait (I : Integer) when True  is
      begin
         if Locks (I) > 0 then requeue Barrier (I); end if;
      end Wait;

   end Lock_Manager;

   procedure Inc (Synch : Integer; Val : Integer := 1) is
   begin
      Lock_Manager.Inc (Synch, Val);
   end Inc;

end FM.Locks;
