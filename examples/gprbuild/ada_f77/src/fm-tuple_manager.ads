
------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      F M - T U P L E _ M A N A G E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                             --
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
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

--  This package specifies the management of a queue of jobs that can be
--  executed concurrently. The tuple space is implemented by means of a
--  protected queue that holds independent iterates. Worker tasks remove
--  work items from the queue and execute them independently.

with FM.Protected_Queue;
with System; use System;
with FM.Types; use FM.Types;
with FM.System_Dependant;

private package FM.Tuple_Manager is


   task type Worker is
      pragma Storage_Size (Worker_Stack_Size);
      pragma Task_Info (FM.System_Dependant.Thread_Setup);
   end;
   --  Worker tasks remove work items from the queue and call back the
   --  Fortran subprogram with the proper actuals, which have been captured
   --  in the interface.

   subtype Param_Range is Natural range 1 .. 10;
   type Params   is array (Param_Range range <>) of Fortran_Arg;
   type Booleans is array (Param_Range range <>) of Boolean;

   type Tuple (N : Param_Range := 1) is record
      Ptr     : Address;
      Synch   : Integer;
      Actuals : Params   (1 .. N);
      Saved   : Booleans (1 .. N);
   end record;
   --  This type describes the structure of a unit of work.

   package Work_Queue is new Protected_Queue (Nb_Jobs, Tuple);
   use Work_Queue;

   procedure Post (T : Tuple);
   --  Insert work item in protected queue. Called from interface routines.

end FM.Tuple_Manager;
