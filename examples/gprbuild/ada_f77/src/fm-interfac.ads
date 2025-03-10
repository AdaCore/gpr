------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         F M . I N T E R F A C E                          --
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

--  The subprograms in this package are to be called from Fortran code in order
--  to make use of multiple processors for parallel execution. The first group
--  of procedures are used to parallelize simple loops. The second group gives
--  the user control over the fragments to be executed in parallel, and also
--  provides explicit locking mechanisms that allow the Fortran user to define
--  critical sections and barrier synchronization.
--
with FM.Types; use FM.Types;
package FM.Interfac is

   -------------
   -- PARLOOP --
   -------------
   --  Fortran use:
   --       CALL PARLOOP1 (I1, IN, I, SUB, VAR1)
   --       CALL PARLOOP2 (I1, IN, I, SUB, VAR1, VAR2)
   --       CALL PARLOOP3 (I1, IN, I, SUB, VAR1, VAR2, VAR3)
   --       CALL PARLOOP4 (I1, IN, I, SUB, VAR1, VAR2, VAR3, VAR4)
   --       CALL PARLOOP5 (I1, IN, I, SUB, VAR1, VAR2, VAR3, VAR4, VAR5)
   --       CALL PARLOOP6 (I1, IN, I, SUB, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6)
   --  Execute in parallel, the loop equivalent to
   --        DO 10 I=I1,IN
   --           CALL SUB (VAR1 ... VARn)
   --   10   CONTINUE
   --
   --  This is semantically equivalent to using the following calls, as
   --  explained below:
   --        CALL INITSYNC (ISYNC)
   --        DO 10 I=I1,IN
   --           CALL POSTn (ISYNC, I, SUB, VAR1 ... VARn)
   --   10   CONTINUE
   --        CALL WAITSYNC (ISYNC)

   procedure Parloop1 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr1;
       Var1  : Fortran_Arg);
   pragma Export (Fortran, ParLoop1);

   procedure Parloop2 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr2;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg);
   pragma Export (Fortran, ParLoop2);

   procedure Parloop3 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr3;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg);
   pragma Export (Fortran, ParLoop3);

   procedure Parloop4 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr4;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg);
   pragma Export (Fortran, ParLoop4);

   procedure Parloop5 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr5;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg;
       Var5  : Fortran_Arg);
   pragma Export (Fortran, ParLoop5);

   procedure Parloop6 (
       From  : Fortran_Arg;
       To    : Fortran_Arg;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr6;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg;
       Var5  : Fortran_Arg;
       Var6  : Fortran_Arg);
   pragma Export (Fortran, ParLoop6);

   ---------------
   -- INITSYNC --
   ---------------
   --  Fortran use:
   --       CALL INITSYNC (ISYNC)
   --  where ISYNC is a integer variable that must not be changed by the
   --  fortran code and that represents this specific loop. The same
   --  variable must be used for the call to WAITSYNC. This procedure is
   --  called prior to using one of the POST routines.
   procedure Initsync (Synch : in out Integer);
   pragma Export (Fortran, Initsync);

   --------------
   -- WAITSYNC --
   --------------
   --  Fortran use:
   --       CALL WAITSYNC (ISYNC)
   --  where ISYNC is a integer variable that must not be changed by the
   --  fortran code and that represents this specific loop. It must match
   --  the variable used by a former call to INITSYNC. A call to this
   --  procedure will block execution of the program till all the POST calls
   --  are completed
   procedure Waitsync (Synch : Integer);
   pragma Export (Fortran, Waitsync);

   ----------
   -- LOCK --
   ----------
   --  Fortran use:
   --       CALL LOCK ()
   -- This procedure must be called before entering a critical section of code
   -- that cannot be executed in parallel.
   procedure Lock;
   pragma Export (Fortran, Lock);

   ------------
   -- UNLOCK --
   ------------
   --  Fortran use:
   --       CALL UNLOCK ()
   -- This procedure must be called after exiting a critical section of code
   -- that cannot be executed in parallel.
   procedure Unlock;
   pragma Export (Fortran, Unlock);

    ----------
    -- STOP --
    ----------
   --  Fortran use:
   --       CALL STOP ()
   --  This procedure should be called before ADAFINAL () to signify to the
   --  Ada subsystem that the workers can be shutdown.

    procedure Stop;
    pragma Export (Fortran, Stop);
   ----------------------
   --  POST procedures --
   ----------------------
   --  Fortran use:
   --       CALL POST1 (ISYNC, I, SUB, VAR1)
   --       CALL POST2 (ISYNC, I, SUB, VAR1, VAR2)
   --       CALL POST3 (ISYNC, I, SUB, VAR1, VAR2, VAR3)
   --       CALL POST4 (ISYNC, I, SUB, VAR1, VAR2, VAR3, VAR4)
   --       CALL POST5 (ISYNC, I, SUB, VAR1, VAR2, VAR3, VAR4, VAR5)
   --       CALL POST6 (ISYNC, I, SUB, VAR1, VAR2, VAR3, VAR4, VAR5, VAR6)
   --  Each procedure allows to put a procedure call in
   --  the queue of calls to be executed. Interfaces are given for
   --  procedures with 1 to 6 parameters. ISYNC is the parameter matching the
   --  previous call to INITSYNC. I is the loop index. SUB is the subprogram
   --  to call with paramaters VAR1, ..., VAR6

   procedure Post1
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr1;
       Var1  : Fortran_Arg);
    pragma Export (Fortran, Post1);

    procedure Post2
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr2;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg);
    pragma Export (Fortran, Post2);

    procedure Post3
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr3;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg);
    pragma Export (Fortran, Post3);

    procedure Post4
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr4;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg);
    pragma Export (Fortran, Post4);

    procedure Post5
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr5;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg;
       Var5  : Fortran_Arg);
    pragma Export (Fortran, Post5);

    procedure Post6
      (Synch : Integer;
       Index : Fortran_Arg;
       Subr  : Fortran_Subr6;
       Var1  : Fortran_Arg;
       Var2  : Fortran_Arg;
       Var3  : Fortran_Arg;
       Var4  : Fortran_Arg;
       Var5  : Fortran_Arg;
       Var6  : Fortran_Arg);
    pragma Export (Fortran, Post6);
end FM.Interfac;
