------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
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

package body GPR2.Project.Import.Set is

   type Iterator is new Import_Iterator.Forward_Iterator with record
     Root : not null access constant Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.P_S.Clear;
      Self.B_S.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type is
   begin
      return Constant_Reference_Type'
        (Import => Path_Name_Set.Constant_Reference
           (Self.P_S, Position.Current).Element);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Object; Path_Name : Path_Name_Type) return Boolean is
   begin
      return Self.P_S.Contains (Path_Name);
   end Contains;

   function Contains
     (Self : Object; Base_Name : Name_Type) return Boolean is
   begin
      return Self.B_S.Contains (Base_Name);
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Object; Path_Name : Path_Name_Type) is
   begin
      Self.P_S.Delete (Path_Name);
      Self.B_S.Delete (Base_Name (Path_Name));
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Project.Import.Object is
   begin
      return Path_Name_Set.Element (Position.Current);
   end Element;

   function Element
     (Self : Object; Path_Name : Path_Name_Type) return Import.Object is
   begin
      return Self.P_S (Path_Name);
   end Element;

   function Element
     (Self : Object; Base_Name : Name_Type) return Import.Object is
   begin
      return Self.B_S (Base_Name);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      return Cursor'(Current => Path_Name_Set.First (Iter.Root.P_S));
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Path_Name_Set.Has_Element (Position.Current);
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : in out Object; Import : Project.Import.Object) is
   begin
      Self.P_S.Insert (Import.Path_Name, Import);
      Self.B_S.Insert (Base_Name (Import.Path_Name), Import);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.P_S.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Import_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Root => Self'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Containers.Count_Type is
   begin
      return Self.P_S.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
   begin
      return Cursor'(Current => Path_Name_Set.Next (Position.Current));
   end Next;

end GPR2.Project.Import.Set;
