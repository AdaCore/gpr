------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                             F M . T Y P E S                              --
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

--  This package provides the basic types used in interfacing Fortran
--  routines to Ada. We declare several access to procedures  with a
--  variables number of formals. Given Fortran passes all parameters by
--  address, the interface uses the type System.Address for all of them.
--  Thus the Ada side performs no type-checking at all, and it is up to
--  the Fortran programmer to insure that the proper interface procedure is
--  invoked.

with System; use System;
with Unchecked_Conversion;
with Unchecked_Deallocation;
package FM.types is

   type Fortran_Arg is access all integer;
   --  A fortran argument is passed by reference
   procedure Free is new Unchecked_Deallocation (Integer, Fortran_Arg);


   type Fortran_Subr1 is access procedure (Var1 : Fortran_Arg);
   pragma Convention (Fortran, Fortran_Subr1);
   function To_Addr is new Unchecked_Conversion (Fortran_Subr1, Address);
   function To_Ptr1 is new Unchecked_Conversion (Address, Fortran_Subr1);

    type Fortran_Subr2 is access procedure
      (Var1 : Fortran_Arg;
       Var2 : Fortran_Arg);
    pragma Convention (Fortran, Fortran_Subr2);
    function To_Addr is new Unchecked_Conversion (Fortran_Subr2, Address);
    function To_Ptr2 is new Unchecked_Conversion (Address, Fortran_Subr2);

    type Fortran_Subr3 is access procedure
      (Var1 : Fortran_Arg;
       Var2 : Fortran_Arg;
       Var3 : Fortran_Arg);
    pragma Convention (Fortran, Fortran_Subr3);
    function To_Addr is new Unchecked_Conversion (Fortran_Subr3, Address);
    function To_Ptr3 is new Unchecked_Conversion (Address, Fortran_Subr3);

    type Fortran_Subr4 is access procedure
      (Var1 : Fortran_Arg;
       Var2 : Fortran_Arg;
       Var3 : Fortran_Arg;
       Var4 : Fortran_Arg);
    pragma Convention (Fortran, Fortran_Subr4);
    function To_Addr is new Unchecked_Conversion (Fortran_Subr4, Address);
    function To_Ptr4 is new Unchecked_Conversion (Address, Fortran_Subr4);

    type Fortran_Subr5 is access procedure
      (Var1 : Fortran_Arg;
       Var2 : Fortran_Arg;
       Var3 : Fortran_Arg;
       Var4 : Fortran_Arg;
       Var5 : Fortran_Arg);
    pragma Convention (Fortran, Fortran_Subr5);
    function To_Addr is new Unchecked_Conversion (Fortran_Subr5, Address);
    function To_Ptr5 is new Unchecked_Conversion (Address, Fortran_Subr5);

    type Fortran_Subr6 is access procedure
      (Var1 : Fortran_Arg;
       Var2 : Fortran_Arg;
       Var3 : Fortran_Arg;
       Var4 : Fortran_Arg;
       Var5 : Fortran_Arg;
       Var6 : Fortran_Arg);
    pragma Convention (Fortran, Fortran_Subr6);
    function To_Addr is new Unchecked_Conversion (Fortran_Subr6, Address);
    function To_Ptr6 is new Unchecked_Conversion (Address, Fortran_Subr6);
end FM.Types;
