------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

--  Synchronize data to/from the slave. The usage is:
--
--    On one side:
--       1. call Send_Files for every slave to be synchronized
--       2. call Wait to wait for the synchronization to be terminated
--
--    On the other side:
--       1. call Receive_Files

with GPR2.Compilation.Protocol;
with GPR2.Containers;

package GPR2.Compilation.Sync is

   type Direction is (To_Slave, To_Master);

   procedure Send_Files
     (Channel           : Protocol.Communication_Channel;
      Root_Dir          : String;
      Excluded_Patterns : Containers.Value_List;
      Included_Patterns : Containers.Value_List;
      Mode              : Direction);
   --  Synchronize from the build master to the slave

   procedure Wait;
   --  Wait for all synchronization to be terminated

   function Receive_Files
     (Channel           : Protocol.Communication_Channel;
      Root_Dir          : String;
      Total_File        : out Natural;
      Total_Transferred : out Natural;
      Remote_Files      : out Containers.Value_Set;
      Is_Debug          : Boolean;
      Display           : access procedure (Message : String))
      return Protocol.Command_Kind;
   --  This routine must be used to receive the files that will be sent over
   --  by To_Slave. Total_File will be set with the total number of files
   --  checked and Total_Transferred the total number of files actually
   --  transferred (because of a time-stamp mismatch). The Root_Dir is the
   --  directory from where the files are to be written. Finally a Display
   --  routine can be passed to display messages during the transfer. Some
   --  messages are only displayed depending on Is_Debug status.

end GPR2.Compilation.Sync;
