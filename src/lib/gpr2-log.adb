------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

   type Iterator
     (Information : Boolean;
      Warning     : Boolean;
      Error       : Boolean;
      Read        : Boolean;
      Unread      : Boolean)
   is new Log_Iterator.Forward_Iterator with record
     Store : not null access Message_Set.Vector;
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

   procedure Set_Read (Position : Cursor)
     with Post => not Has_Element (Position)
                  or else Element (Position).Status = Message.Read;
   --  Set Element at Position as read

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Object; Message : GPR2.Message.Object) is
      Index : constant String := Message.Format;
   begin
      if not Self.Index.Contains (Index) then
         Self.Store.Append (Message);
         Self.Store (Self.Store.Last).Set_Status (GPR2.Message.Unread);
         Self.Index.Insert (Index);
      end if;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.Store.Clear;
      Self.Index.Clear;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : Message_Set.Constant_Reference_Type renames
              Message_Set.Constant_Reference (Self.Store, Position.P);
   begin
      return Constant_Reference_Type'
        (Message => Ref.Element.all'Unrestricted_Access,
         Ref     => Ref);
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
      return Position.Store.all (Position.P);
   end Element;

   function Element
     (Self     : Object;
      Position : Positive) return Message.Object is
   begin
      return Self.Store (Position);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
      Position : constant Cursor := (Iter.Store, Iter.Store.First_Index);
   begin
      if not Has_Element (Position)
        or else
          Match_Filter
            (Element (Position),
             Iter.Information, Iter.Warning, Iter.Error,
             Iter.Read, Iter.Unread)
      then
         Set_Read (Position);
         return Position;
      else
         return Next (Iter, Position);
      end if;
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
     (Self        : Object;
      Information : Boolean := True;
      Warning     : Boolean := True;
      Error       : Boolean := True;
      Read        : Boolean := True;
      Unread      : Boolean := True)
      return Log_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'
        (Information => Information,
         Warning     => Warning,
         Error       => Error,
         Read        => Read,
         Unread      => Unread,
         Store       => Self.Store'Unrestricted_Access);
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      New_Position : Cursor := Position;
   begin
      loop
         New_Position.P := New_Position.P + 1;

         exit when New_Position.P > Natural (Iter.Store.Length)
           or else
             Match_Filter
               (Element (New_Position),
                Iter.Information, Iter.Warning, Iter.Error,
                Iter.Read, Iter.Unread);
      end loop;

      Set_Read (New_Position);

      return New_Position;
   end Next;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type
   is
      Ref : Message_Set.Reference_Type renames
              Message_Set.Reference (Self.Store, Position.P);
   begin
      return Reference_Type'
        (Message => Ref.Element.all'Unrestricted_Access,
         Ref     => Ref);
   end Reference;

   --------------
   -- Set_Read --
   --------------

   procedure Set_Read (Position : Cursor) is
   begin
      if Has_Element (Position) then
         Position.Store.all (Position.P).Set_Status (Message.Read);
      end if;
   end Set_Read;

end GPR2.Log;
