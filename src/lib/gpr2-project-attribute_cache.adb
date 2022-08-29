--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;
with GNAT.Task_Lock;

package body GPR2.Project.Attribute_Cache is

   use type Project.Attribute_Index.Object;

   function Cache_Key
      (Name   : Q_Attribute_Id;
       Index  : Project.Attribute_Index.Object := Attribute_Index.Undefined;
       At_Pos : Unit_Index                     := No_Index)
      return String;
   --  Given the Get_Attribute parameters return a unique key.

   procedure Free is new Ada.Unchecked_Deallocation
      (Attribute_Cache_Maps.Map, Map_Access);

   procedure Free is new Ada.Unchecked_Deallocation
      (Inner_Object, Inner_Object_Access);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Cache : in out Object) is
   begin
      Cache.Inner := new Inner_Object'
        (Enabled               => Cache.Inner.Enabled,
         Table                 => new Attribute_Cache_Maps.Map'
                                    (Cache.Inner.Table.Copy),
         --  No need to keep the former cache table on copy
         Former_Table          => null,
         --  Extra capacity should be set to 0 as no new element is scheduled
         --  for addition in the cache.
         Needed_Extra_Capacity => 0);

      --  Ensure cache capacity is at least Min_Cache_Size
      if Cache.Inner.Table.Capacity < Min_Cache_Size then
         Cache.Inner.Table.Reserve_Capacity (Min_Cache_Size);
      end if;
   end Adjust;

   ---------------
   -- Cache_Key --
   ---------------

   function Cache_Key
      (Name   : Q_Attribute_Id;
       Index  : Project.Attribute_Index.Object := Attribute_Index.Undefined;
       At_Pos : Unit_Index                     := No_Index)
      return String
   is
   begin
      if Index /= Attribute_Index.Undefined then
         return Name.Attr'Img & ":" & Name.Pack'Img & ":" &
            Index.Value (Preserve_Case => Index.Is_Case_Sensitive) & ":" &
            At_Pos'Img;
      else
         return Name.Attr'Img & ":" & Name.Pack'Img;
      end if;
   end Cache_Key;

   -----------------
   -- Check_Cache --
   -----------------

   function Check_Cache
      (Self   : Object;
       Name   : Q_Attribute_Id;
       Index  : Project.Attribute_Index.Object := Attribute_Index.Undefined;
       At_Pos : Unit_Index                     := No_Index)
      return Cursor
   is
      Key : constant String := Cache_Key (Name, Index, At_Pos);
   begin
      return Attribute_Cache.Cursor
         (Attribute_Cache_Maps.Find (Self.Inner.Table.all, Key));
   end Check_Cache;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Self : Object) is
   begin
      Attribute_Cache_Maps.Clear (Self.Inner.Table.all);
   end Clear_Cache;

   -------------------
   -- Disable_Cache --
   -------------------

   procedure Disable_Cache (Self : Object) is
   begin
      Self.Inner.Enabled := False;
      Self.Clear_Cache;
   end Disable_Cache;

   -------------
   -- Element --
   -------------

   overriding function Element
      (C : Cursor) return GPR2.Project.Attribute.Object
   is
   begin
      return Attribute_Cache_Maps.Element (Attribute_Cache_Maps.Cursor (C));
   end Element;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Self : Object) is
   begin
      Self.Inner.Enabled := True;
   end Enable_Cache;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Cache : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Attribute_Cache_Maps.Map, Map_Access);

   begin
      if Cache.Inner /= null then
         if Cache.Inner.Table /= null then
            Free (Cache.Inner.Table);
         end if;

         if Cache.Inner.Former_Table /= null then
            Free (Cache.Inner.Former_Table);
         end if;

         Free (Cache.Inner);
      end if;

   end Finalize;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (C : Cursor) return Boolean is
   begin
      return Attribute_Cache_Maps.Has_Element
         (Attribute_Cache_Maps.Cursor (C));
   end Has_Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Cache : in out Object)
   is
   begin
      Cache.Inner := new Inner_Object'
        (Enabled               => True,
         Table                 => new Attribute_Cache_Maps.Map'
                                    (Attribute_Cache_Maps.Empty
                                       (Capacity => Min_Cache_Size)),
         Former_Table          => null,
         Needed_Extra_Capacity => 0);
   end Initialize;

   ---------------------------
   -- Schedule_Update_Cache --
   ---------------------------

   procedure Schedule_Update_Cache (Self : Object) is
   begin
      GNAT.Task_Lock.Lock;

      --  In the worst case we will need 1 more element in the cache
      Self.Inner.Needed_Extra_Capacity :=
         Self.Inner.Needed_Extra_Capacity + 1;

      if Self.Inner.Table.Length + Self.Inner.Needed_Extra_Capacity >
         Self.Inner.Table.Capacity
      then
         --  We have reached the maximum capacity. Increase the table size
         declare
            New_Table : constant Map_Access :=
               new Attribute_Cache_Maps.Map'(Self.Inner.Table.all.Copy);
            New_Capacity : Ada.Containers.Count_Type :=
               Self.Inner.Table.Capacity * 2;
         begin
            while New_Capacity <
               Self.Inner.Table.Length + Self.Inner.Needed_Extra_Capacity
            loop
               New_Capacity := New_Capacity * 2;
            end loop;

            New_Table.Reserve_Capacity (New_Capacity);

            --  Free the former table. We assume that sufficiently time has
            --  passed so that any concurrent query on the former table had
            --  time to finish. Making that assumption allows GPR2 to avoid
            --  need for lock on cache read.
            if Self.Inner.Former_Table /= null then
               Free (Self.Inner.Former_Table);
            end if;

            --  Switch access to tables
            Self.Inner.Former_Table := Self.Inner.Table;
            Self.Inner.Table := New_Table;
         end;
      end if;

      GNAT.Task_Lock.Unlock;
   end Schedule_Update_Cache;

   ------------------
   -- Update_Cache --
   ------------------

   procedure Update_Cache
      (Self   : Object;
       Name   : Q_Attribute_Id;
       Index  : Project.Attribute_Index.Object := Attribute_Index.Undefined;
       At_Pos : Unit_Index                     := No_Index;
       Attr   : GPR2.Project.Attribute.Object)
   is
      Key : constant String := Cache_Key (Name, Index, At_Pos);
   begin
      GNAT.Task_Lock.Lock;

      if Self.Inner.Enabled then
         Attribute_Cache_Maps.Include (Self.Inner.Table.all, Key, Attr);

         if Self.Inner.Needed_Extra_Capacity > 0 then
            Self.Inner.Needed_Extra_Capacity :=
              Self.Inner.Needed_Extra_Capacity - 1;
         end if;
      end if;

      GNAT.Task_Lock.Unlock;
   end Update_Cache;

end GPR2.Project.Attribute_Cache;
