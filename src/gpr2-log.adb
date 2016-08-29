------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

package body GPR2.Log is

   use type Message.Level_Value;
   use type Message.Status_Type;

   type Iterator is new Log_Iterator.Forward_Iterator with record
     Log : Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Match_Filter
     (Message     : GPR2.Message.Object;
      Information : Boolean;
      Warning     : Boolean;
      Error       : Boolean;
      Read        : Boolean;
      Unread      : Boolean) return Boolean is
     (((Message.Level = GPR2.Message.Information and then Information)
       or else (Message.Level = GPR2.Message.Warning and then Warning)
       or else (Message.Level = GPR2.Message.Error and then Error))
      and then
        ((Message.Status = GPR2.Message.Read and then Read)
         or else (Message.Status = GPR2.Message.Unread and then Unread)));
   --  Returns True is the Message's Level match the information/warning/error
   --  values with the corresponding Read/Unread status.

   ------------
   -- Append --
   ------------

   procedure Append
     (Self    : in out Object;
      Message : GPR2.Message.Object) is
   begin
      Self.Store.Append (Message);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.Store.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      return Constant_Reference_Type'
        (Message =>
           Message_Set.Constant_Reference
             (Position.Store, Position.P).Element);
   end Constant_Reference;

   -----------
   -- Count --
   -----------

   function Count (Self : Object) return Containers.Count_Type is
   begin
      return Self.Store.Length;
   end Count;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Message.Object is
   begin
      return Position.Store (Position.P);
   end Element;

   function Element
     (Self     : Object;
      Position : Positive)
      return Message.Object is
   begin
      return Self.Store (Position);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      return Cursor'(Iter.Log.Store, Iter.Log.Store.First_Index);
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.P >= 1
        and then Position.P <= Natural (Position.Store.Length);
   end Has_Element;

   function Has_Element
     (Self        : Object;
      Information : Boolean := True;
      Warning     : Boolean := True;
      Error       : Boolean := True;
      Read        : Boolean := True;
      Unread      : Boolean := True) return Boolean is
   begin
      for M of Self.Store loop
         if Match_Filter (M, Information, Warning, Error, Read, Unread) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.Store.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self        : in out Object;
      Information : Boolean := True;
      Warning     : Boolean := True;
      Error       : Boolean := True;
      Read        : Boolean := True;
      Unread      : Boolean := True)
      return Log_Iterator.Forward_Iterator'Class
   is
      Iter : Iterator;
   begin
      --  Fill store with the matching elements and tag them as read

      for M of Self.Store loop
         if Match_Filter (M, Information, Warning, Error, Read, Unread) then
            Iter.Log.Store.Append (M);
            M.Set_Status (Message.Read);
         end if;
      end loop;

      return Iter;
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
   begin
      return Cursor'(Position.Store, Position.P + 1);
   end Next;

end GPR2.Log;
