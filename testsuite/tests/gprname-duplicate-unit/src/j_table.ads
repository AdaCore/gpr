------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ T A B L E                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                              --
--                                                                          --
--          Copyright (C) 1998 Free Software Foundation, Inc.               --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This file is part of the Ada-to-JVM compiler.

--  JVM class file structures contain various tables. Typically you have:
--
--       struct some_jvm_class_file_struct {
--          ...
--          u2   data_count;
--          data data_table [data_count];
--       }
--
--  where data is some data type (note that this is not valid C).
--  To map the above struct in Ada we could make data_count a discriminant in
--  the corresponding Ada record. This is, at best, inconvenient since the
--  number of elements in data_table must be determined before we can allocate
--  the whole record.

--  The following generic allows a data table to be allocated and grown on the
--  fly without requiring explicit dynamic storage allocation. Furthermore,
--  contrary to the GNAT.Table abstraction, several conceptually distinct data
--  tables can be allocated.

--  This package allows the creation of fixed-size or expandable tables using
--  routines Allocate_Fixed_Table and Allocate_Expandable_Table. An expandable
--  table can be grown element by element using routine Add until it is frozen
--  using routine Freeze_Expandable_Table. After Freeze_Expandable_Table has
--  been invoked the expandable table becomes a fixed-size table.

--  While an expandable table is being constructed, no other fixed-size or
--  expandable table from the same generic instantiation can be allocated. If
--  you need to construct two expandable tables at the same time, do it on two
--  different instantiations of this generic.

with J_Types; use J_Types;

generic

   type Data is private;
   --  The component type of the table

   type Index is (<>);
   --  The index type of the table

   Default_Init : Boolean := True;
   --  When set, default initialize the elements of fixed size tables.
   --  The default initial value is obtained by declaring a local variable
   --  "Init_Val : Data;" and using Init_Val as the initial value for all
   --  the elements of the table.

package J_Table is

   pragma Elaborate_Body (J_Table);

   type Table is private;
   --  Table of Data elements. The first element is at Index'Pos (0).  Table
   --  objects are automatically initialized to the unallocated table.

   type Data_Array is array (Nat_32 range <>) of Data;
   --  Data type used to convert between Table and a regular array of Data

   function "=" (T1, T2 : Table) return Boolean;
   --  Always raises an exception. No table comparaison is allowed.

   procedure Allocate_Fixed_Table (T : in out Table; L : Nat_32);
   --  Allocates a fixed-size Table T of length L and initializa all its
   --  elements if Default_Init is set. An exception is raised if T has been
   --  allocated already or if an expandable table has been allocated but not
   --  frozen yet.

   procedure Allocate_Expandable_Table (T : in out Table);
   --  Allocates an expandable Table T of initial length 0.  An exception is
   --  raised if T has been allocated already or if an expandable table has
   --  been allocated but not frozen yet.

   function Allocated (T : Table) return Boolean;
   --  Returns True if Table T has been allocated, False otherwise

   procedure Add (T : in out Table; Val : Data);
   --  Adds Val to the end of expandable Table T and increase T's length by
   --  one. An exception is raised if T is not the current expandable Table.

   procedure Add (T : in out Table; A : Data_Array);
   --  Appends array A to the end of expandable Table T and increase T's length
   --  by A'Length. An exception is raised if T is not the current expandable
   --  Table.

   procedure Freeze_Expandable_Table (T : Table);
   --  Makes the current expandable Table T a fixed-size Table. An exception
   --  is raised if T is not the current expandable table. After calling
   --  Freeze_Expandable_Table (T), T is no loger an expandable table.

   function Building_Expandable_Table return Boolean;
   --  Returns True if an expandable table is being built.

   function Length (T : Table) return Nat_32;
   --  Returns the length of T, return zero if T has not been allocated yet

   function Last (T : Table) return Int_32;
   --  Returns Length (T) - 1

   --  The following routines allow to access the K-th element of a table T
   --  with either a typed index (generic formal Index) or with an integer.

   function Get (T : Table; K : Index)  return Data;
   function Get (T : Table; K : Nat_32) return Data;
   --  Return the K-th element in T. An exception is raised if T has not been
   --  allocated yet. If K >= Length (T) the Constraint_Error is raised.  The
   --  first version is equivalent to Get (T, Index'Val (K)) in the second.

   function Get (T : Table) return Data_Array;
   --  Returns the contents of T as array of Data objects

   procedure Put (T : Table; K : Index;  Val : Data);
   procedure Put (T : Table; K : Nat_32; Val : Data);
   --  Set Val to be the K-th element of T, otherwise same remarks as for Get

   procedure Put (T : Table; A : Data_Array);
   --  Sets the contents of T to be those of A. An exception is raised if
   --  A'Length /= Length (T).

   generic
      with procedure Visit (A : in out Data_Array) is <>;
   procedure Process (T : Table);
   --  Process converts table T into a Data_Array and passes is to procedure
   --  Visit for read/write processing. Process can be used to perform an
   --  efficient traversal of T's elements.  An exception is raised if T has
   --  not been allocated.  While Process is executing you should not create a
   --  new Table or add an element to a Table that is being expanded (by Table
   --  we intend a Table object coming from the same generic instatiation). It
   --  is of course permitted to manipulate a Table object belonging to another
   --  generic instantiation.

   ------------------------
   -- Table Deallocation --
   ------------------------

   --  After having allocated a bunch of tables with routines Allocate_*_Table
   --  above, it may be desirable to deallocate such tables and reclaim the
   --  storage used by these.  To achieve this, use routines Set_Water_Mark and
   --  Free_To_Next_Water_Mark, defined below, as follows:
   --
   --  1. Before allocating the tables call Set_Water_Mark.
   --
   --  2. To deallocate the tables allocated from the last call to
   --     Set_Water_Mark just call Free_To_Next_Water_Mark.
   --
   --  An exception is raised if either routine is called during the growth of
   --  an expandable table, before the expandable table is frozen. An exception
   --  is also raised if either routine is called while procedure Apply is
   --  being invoked.
   --
   --  All tables allocated after the call to Set_Water_Mark are deallocated
   --  after the call to Free_To_Next_Water_Mark, that is for any such table T,
   --  Allocated (T) returns False.

   procedure Set_Water_Mark;
   procedure Free_To_Next_Water_Mark;

private

   type Allocation_Id is new Nat_32;

   No_Index : constant Nat_32 := 0;

   type Table is record
      Index : Nat_32 := No_Index;
      --  Index into the global Data array giving the beginning of the Table

      Length : Nat_32 := 0;
      --  Number of elements in the Table

      Allocation_Nb : Allocation_Id := 0;
      --  Number of Table objects that have been allocated up to and including
      --  this one. Used to check if this table is currently allocated.
      --  Allocation_Nb is used to index array Is_Allocated.Table (see body of
      --  this package), whose role is to keep track of the currently allocated
      --  Table objects. An object T of type Table is allocated iff:
      --
      --      T.Allocation_Nb in 1 .. Is_Allocated.Last
      --  and
      --      Is_Allocated.Table (T.Allocation_Nb) = True;
   end record;

   pragma Inline (Add);
   pragma Inline (Allocated);
   pragma Inline (Building_Expandable_Table);
   pragma Inline (Get);
   pragma Inline (Last);
   pragma Inline (Length);
   pragma Inline (Process);
   pragma Inline (Put);

end J_Table;
