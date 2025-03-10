
------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         F M - I N T E R F A C E                          --
--                                                                          --
--                                 B o d y                                  --
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

with FM.Locks;  use  FM.Locks;
with FM.Tuple_Manager; use  FM.Tuple_Manager;

package body FM.Interfac is

   type Array_Worker is array (Natural range <>) of Worker;
   Workers : Array_Worker (1 .. Nb_Workers);

   Procedure Set_Var
     (V : in out Tuple;
      I : Param_Range;
      Index: Fortran_Arg;
      Var : Fortran_Arg)
   is
   begin
      if Var = Index then
         V.Actuals (I) := new Integer'(Var.all);
         V.Saved   (I) := True;
      else
         V.Actuals (I) := Var;
         V.Saved   (I) := False;
      end if;
   end Set_Var;

   -----------
   -- Post1 --
   -----------

   procedure Post1
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr1;
      Var1  : Fortran_Arg)
   is
     V : Tuple (1);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Tuple_Manager.Post (V);
   end Post1;

   -----------
   -- Post2 --
   -----------

   procedure Post2
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr2;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg)
   is
     V : Tuple (2);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Set_Var (V, 2, Index, Var2);
      Tuple_Manager.Post (V);
   end Post2;

   -----------
   -- Post3 --
   -----------

   procedure Post3
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr3;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg)
   is
     V : Tuple (3);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Set_Var (V, 2, Index, Var2);
      Set_Var (V, 3, Index, Var3);
      Tuple_Manager.Post (V);
   end Post3;

   -----------
   -- Post4 --
   -----------

   procedure Post4
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr4;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg)
   is
     V : Tuple (4);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Set_Var (V, 2, Index, Var2);
      Set_Var (V, 3, Index, Var3);
      Set_Var (V, 4, Index, Var4);
      Tuple_Manager.Post (V);
   end Post4;

   -----------
   -- Post5 --
   -----------

   procedure Post5
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr5;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg;
      Var5  : Fortran_Arg)
   is
     V : Tuple (5);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Set_Var (V, 2, Index, Var2);
      Set_Var (V, 3, Index, Var3);
      Set_Var (V, 4, Index, Var4);
      Set_Var (V, 5, Index, Var5);
      Tuple_Manager.Post (V);
   end Post5;

   -----------
   -- Post6 --
   -----------

   procedure Post6
     (Synch : Integer;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr6;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg;
      Var5  : Fortran_Arg;
      Var6  : Fortran_Arg)
   is
     V : Tuple (6);

   begin
      V.Ptr := To_Addr (Subr);
      V.Synch := Synch;
      Set_Var (V, 1, Index, Var1);
      Set_Var (V, 2, Index, Var2);
      Set_Var (V, 3, Index, Var3);
      Set_Var (V, 4, Index, Var4);
      Set_Var (V, 5, Index, Var5);
      Set_Var (V, 6, Index, Var6);
      Tuple_Manager.Post (V);
   end Post6;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Lock_Manager.Lock;
   end Lock;

   --------------
   -- Parloop1 --
   --------------

   procedure Parloop1
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr1;
      Var1  : Fortran_Arg)
   is
      Isync : Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post1 (Isync, Index, Subr, Var1);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop1;

   --------------
   -- Parloop2 --
   --------------

   procedure Parloop2
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr2;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg)
   is
      Isync : aliased Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post2 (Isync, Index, Subr, Var1, Var2);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop2;

   --------------
   -- Parloop3 --
   --------------

   procedure Parloop3
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr3;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg)
   is
      Isync : aliased Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post3 (Isync, Index, Subr, Var1, Var2, Var3);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop3;

   --------------
   -- Parloop4 --
   --------------

   procedure Parloop4
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr4;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg)
   is
      Isync : aliased Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post4 (Isync, Index, Subr, Var1, Var2, Var3, Var4);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop4;

   --------------
   -- Parloop5 --
   --------------

   procedure Parloop5
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr5;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg;
      Var5  : Fortran_Arg)
   is
      Isync : aliased Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post5 (Isync, Index, Subr, Var1, Var2, Var3, Var4, Var5);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop5;

   --------------
   -- Parloop6 --
   --------------

   procedure Parloop6
     (From  : Fortran_Arg;
      To    : Fortran_Arg;
      Index : Fortran_Arg;
      Subr  : Fortran_Subr6;
      Var1  : Fortran_Arg;
      Var2  : Fortran_Arg;
      Var3  : Fortran_Arg;
      Var4  : Fortran_Arg;
      Var5  : Fortran_Arg;
      Var6  : Fortran_Arg)
   is
      Isync : aliased Integer := 0;

   begin
      Initsync (Isync);
      Index.all := From.all;
      while Index.all <= To.all loop
         Post6 (Isync, Index, Subr, Var1, Var2, Var3, Var4, Var5, Var6);
         Index.all := Index.all + 1;
      end loop;
      Waitsync (Isync);
   end Parloop6;

   ---------------
   -- INITSYNC --
   ---------------

   procedure Initsync (Synch : in out Integer) is
   begin
      Lock_Manager.Get_Synch (Synch);
   end Initsync;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Stop_Called := True;
   end Stop;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Lock_Manager.Unlock;
   end Unlock;

   --------------
   -- Waitsync --
   --------------

   procedure Waitsync (Synch : Integer) is
   begin
      Lock_Manager.Wait (Synch);
   end Waitsync;
end FM.Interfac;
