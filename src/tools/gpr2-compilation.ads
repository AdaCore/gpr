------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

--  This is the root package for the compilation support. It handles the local
--  and distributed compilation modes.

with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GPR2.Time_Stamp;

package GPR2.Compilation is

   Default_Port : constant := 8484;

   Opts_Sep : constant Character := Ada.Characters.Latin_1.HT;

   --  Command options separator, that is the separator used for options to be
   --  passed to the executed command.

   --  A simple concurrent counter type

   protected type Shared_Counter (Default : Natural := 0) is

      function Count return Natural;
      --  Returns the current counter value

      procedure Increment;
      --  Increment by one

      procedure Decrement;
      --  Decrement by one

      procedure Reset;
      --  Reset counter to 0

      entry Wait_Non_Zero;
      --  Returns when the counter is above zero

   private
      Counter : Natural := Default;
   end Shared_Counter;

   type Shared_Counter_Access is access Shared_Counter;

   procedure Set_Env
     (Env   : String;
      Fail  : Boolean;
      Force : Boolean := False);
   --  Set environment given an Env variable containing a set of name=value
   --  separated with Opts_Sep.
   --
   --  name=value[<opts_sep>name=value]
   --
   --  If Fail is true the program will exit if the a format error is detected.
   --  If Force is set to True the environment will always be set otherwise it
   --  will be set only if not already set.

   --  The set of files for a given project (associated with a synchronization
   --  job).

   type File_Data is record
      Path_Name     : Ada.Strings.Unbounded.Unbounded_String;
      Timestamp     : Time_Stamp.Data;
      Is_Executable : Boolean;
   end record;

   package File_Data_Set is new Ada.Containers.Vectors (Positive, File_Data);

   --  Process's Id, shared between Slave and Process children

   type Remote_Id is mod 2 ** 64;
   --  Represent a remote process id, this number is unique across all slaves.
   --  Such number if created by the slaves using a slave id (unique number)
   --  and a compilation number. Bother numbers are 32bits value:
   --
   --     63                 32  31                     0
   --      |    [slave id]      |  [compilation number] |

   type Process_Kind is (Local, Remote);

   type Id (Kind : Process_Kind := Local) is record
      case Kind is
         when Local  => Pid   : GNAT.OS_Lib.Process_Id;
         when Remote => R_Pid : Remote_Id;
      end case;
   end record;

   Invalid_Process : constant Id := (Local, Pid => GNAT.OS_Lib.Invalid_Pid);

   function Image (Pid : Remote_Id) return String;
   --  Returns the string representation of Pid

   procedure Check_Local_Process (Process : Id);
   --  Check that a local process is valid. If not, fail with the errno and
   --  associated message.

private

   function To_Native_Directory_Separator
     (Pathname : String) return String with Inline;
   --  Returns Pathname with native directory separator

end GPR2.Compilation;
