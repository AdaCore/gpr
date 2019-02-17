------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body GPR2.Project.Attribute.Set is

   type Iterator is new Attribute_Iterator.Forward_Iterator with record
     Name  : Unbounded_String;
     Index : Unbounded_String;
     Set   : Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Is_Matching
     (Iter : Iterator'Class; Position : Cursor) return Boolean
     with Pre => Has_Element (Position);
   --  Returns True if the current Position is matching the Iterator

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.Attributes.Clear;
      Self.Length := 0;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      return Constant_Reference_Type'
        (Attribute =>
           Set_Attribute.Constant_Reference
             (Position.Set.all, Position.CA).Element);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Boolean
   is
      Position : constant Cursor := Self.Find (Name, Index);
   begin
      return Has_Element (Position);
   end Contains;

   function Contains
     (Self      : Object;
      Attribute : Project.Attribute.Object) return Boolean is
   begin
      return Self.Contains (Attribute.Name, Attribute.Index.Text);
   end Contains;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Attribute.Object is
   begin
      return Set_Attribute.Element (Position.CA);
   end Element;

   function Element
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Attribute.Object
   is
      Position : constant Cursor := Self.Find (Name, Index);
   begin
      if Set_Attribute.Has_Element (Position.CA) then
         return Element (Position);
      else
         return Project.Attribute.Undefined;
      end if;
   end Element;

   ------------
   -- Filter --
   ------------

   function Filter
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Object is
   begin
      if Name = No_Name and then Index = No_Value then
         return Self;

      else
         declare
            Result : Object;
         begin
            for C in Self.Iterate (Name, Index) loop
               Result.Insert (Element (C));
            end loop;

            return Result;
         end;
      end if;
   end Filter;

   ----------
   -- Find --
   ----------

   function Find
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Cursor
   is
      Result : Cursor :=
                 (CM  => Self.Attributes.Find (Name),
                  CA  => Set_Attribute.No_Element,
                  Set => null);
   begin
      if Set.Has_Element (Result.CM) then
         Result.Set := Self.Attributes.Constant_Reference (Result.CM).Element;

         --  If we have an attribute in the bucket let's check if the index
         --  is case sensitive or not.

         Result.CA := Result.Set.Find
           (if Index = No_Value
              or else Result.Set.Is_Empty
              or else Result.Set.First_Element.Index_Case_Sensitive
            then Index
            else Ada.Characters.Handling.To_Lower (Index));
      end if;

      return Result;
   end Find;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
      Position : Cursor :=
                   (Iter.Set.Attributes.First,
                    CA  => Set_Attribute.No_Element,
                    Set => null);
   begin
      if Set.Has_Element (Position.CM) then
         Position.Set :=
           Iter.Set.Attributes.Constant_Reference (Position.CM).Element;
         Position.CA := Position.Set.First;
      end if;

      if Has_Element (Position) and then not Is_Matching (Iter, Position) then
         return Next (Iter, Position);
      else
         return Position;
      end if;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Set /= null
        and then Set_Attribute.Has_Element (Position.CA);
   end Has_Element;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self : in out Object; Attribute : Project.Attribute.Object)
   is
      Position : constant Set.Cursor := Self.Attributes.Find (Attribute.Name);
      Present  : Boolean := False;
   begin
      if Set.Has_Element (Position) then
         declare
            A : Set_Attribute.Map := Set.Element (Position);
         begin
            Present := A.Contains (Attribute.Index.Text);
            A.Include  (Attribute.Case_Aware_Index, Attribute);
            Self.Attributes.Replace_Element (Position, A);
         end;

      else
         declare
            A : Set_Attribute.Map;
         begin
            Present := A.Contains (Attribute.Index.Text);
            A.Include (Attribute.Case_Aware_Index, Attribute);
            Self.Attributes.Insert (Attribute.Name, A);
         end;
      end if;

      if not Present then
         Self.Length := Self.Length + 1;
      end if;
   end Include;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Object; Attribute : Project.Attribute.Object)
   is
      Position : constant Set.Cursor := Self.Attributes.Find (Attribute.Name);
   begin
      if Set.Has_Element (Position) then
         declare
            A : Set_Attribute.Map := Set.Element (Position);
         begin
            A.Insert (Attribute.Case_Aware_Index, Attribute);
            Self.Attributes.Replace_Element (Position, A);
         end;

      else
         declare
            A : Set_Attribute.Map;
         begin
            A.Insert (Attribute.Case_Aware_Index, Attribute);
            Self.Attributes.Insert (Attribute.Name, A);
         end;
      end if;

      Self.Length := Self.Length + 1;
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
   begin
      return Self.Length = 0;
   end Is_Empty;

   -----------------
   -- Is_Matching --
   -----------------

   function Is_Matching
     (Iter : Iterator'Class; Position : Cursor) return Boolean
   is
      A     : constant Attribute.Object := Position.Set.all (Position.CA);
      Name  : constant Optional_Name_Type :=
                Optional_Name_Type (To_String (Iter.Name));
      Index : constant Value_Type := To_String (Iter.Index);
   begin
      return
        (Name = No_Name or else A.Name = Name_Type (Name))
        and then (Index = No_Value or else A.Index_Equal (Index));
   end Is_Matching;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value)
      return Attribute_Iterator.Forward_Iterator'Class is
   begin
      return It : Iterator do
         It.Set   := Self;
         It.Name  := To_Unbounded_String (String (Name));
         It.Index := To_Unbounded_String (String (Index));
      end return;
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Self : Object) return Containers.Count_Type is
   begin
      return Self.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is

      procedure Next (Position : in out Cursor)
        with Post => Position'Old /= Position;
      --  Move Position to next element

      ----------
      -- Next --
      ----------

      procedure Next (Position : in out Cursor) is
      begin
         Position.CA := Set_Attribute.Next (Position.CA);

         if not Set_Attribute.Has_Element (Position.CA) then
            Position.CM := Set.Next (Position.CM);

            if Set.Has_Element (Position.CM) then
               Position.Set :=
                 Iter.Set.Attributes.Constant_Reference (Position.CM).Element;
               Position.CA := Position.Set.First;

            else
               Position.Set := null;
            end if;
         end if;
      end Next;

      Result : Cursor := Position;
   begin
      loop
         Next (Result);
         exit when not Has_Element (Result) or else Is_Matching (Iter, Result);
      end loop;

      return Result;
   end Next;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      return Reference_Type'
        (Attribute =>
           Set_Attribute.Reference (Position.Set.all, Position.CA).Element);
   end Reference;

end GPR2.Project.Attribute.Set;
