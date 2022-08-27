--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Log is

   use type Message.Level_Value;
   use type Message.Status_Type;

   type Iterator
     (Information : Boolean;
      Warning     : Boolean;
      Error       : Boolean;
      Lint        : Boolean;
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
      Lint        : Boolean;
      Read        : Boolean;
      Unread      : Boolean) return Boolean is
     (((Message.Level = GPR2.Message.Information and then Information)
       or else (Message.Level = GPR2.Message.Warning and then Warning)
       or else (Message.Level = GPR2.Message.Error and then Error)
       or else (Message.Level = GPR2.Message.Lint and then Lint))
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
      Index    : constant String :=
                   Message.Format (Levels => (others => GPR2.Message.Short));
      Position : Containers.Value_Type_Set.Cursor;
      Inserted : Boolean;
   begin
      Self.Index.Insert (Index, Position, Inserted);

      if Inserted then
         Self.Store.Append (Message);
         Self.Store (Self.Store.Last).Set_Status (GPR2.Message.Unread);
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
             Iter.Information, Iter.Warning, Iter.Error, Iter.Lint,
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
      Lint        : Boolean := True;
      Read        : Boolean := True;
      Unread      : Boolean := True) return Boolean is
   begin
      for M of Self.Store loop
         if Match_Filter
           (M, Information, Warning, Error, Lint, Read, Unread)
         then
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
      Lint        : Boolean := False;
      Read        : Boolean := True;
      Unread      : Boolean := True)
      return Log_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'
        (Information => Information,
         Warning     => Warning,
         Error       => Error,
         Lint        => Lint,
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
                Iter.Information, Iter.Warning, Iter.Error, Iter.Lint,
                Iter.Read, Iter.Unread);
      end loop;

      Set_Read (New_Position);

      return New_Position;
   end Next;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages
     (Log            : GPR2.Log.Object;
      Information    : Boolean := True;
      Warning        : Boolean := True;
      Error          : Boolean := True;
      Lint           : Boolean := False;
      Full_Path_Name : Boolean := False;
      Output_Levels  : GPR2.Message.Level_Output :=
                         (GPR2.Message.Long,
                          GPR2.Message.Long,
                          GPR2.Message.Long,
                          GPR2.Message.Long)) is
   begin
      for C in Log.Iterate
        (Information => Information,
         Warning     => Warning,
         Error       => Error,
         Lint        => Lint,
         Read        => False,
         Unread      => True)
      loop
         Log (C).Output (Full_Path_Name, Output_Levels);
      end loop;
   end Output_Messages;

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
