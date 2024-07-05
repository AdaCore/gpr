--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source.Sets;
with GPR2.Build.Tree_Db;
pragma Warnings (Off);
--  needed for visibility reasons of the object, as it is limited withed
--  otherwise
with GPR2.Project.View.Set;
pragma Warnings (On);

package body GPR2.Build.View_Db is

   use GPR2.Build.View_Tables;

   function View_Base_For (Data : View_Tables.View_Data) return Object;
   --  Create an object from the View_Tables data

   function Ref (Inst : Object) return View_Tables.View_Data_Ref is
     (Inst.Get);
   --  Extracts a reference to view_tables data from a View_Base instance

   ----------------------
   -- Compilation_Unit --
   ----------------------

   function Compilation_Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object
   is
      Internal : constant View_Tables.View_Data_Ref := Ref (Self);
      Cursor   : constant Compilation_Unit_Maps.Cursor :=
                   Internal.CUs.Find (Name);
   begin
      if Compilation_Unit_Maps.Has_Element (Cursor) then
         return Compilation_Unit_Maps.Element (Cursor);
      else
         return Self.Compilation_Unit (Internal.Separates (Name));
      end if;
   end Compilation_Unit;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (Self                  : Object;
      With_Externally_Built : Boolean := False)
      return Build.Compilation_Unit.Maps.Map is
   begin
      return Result : Build.Compilation_Unit.Maps.Map do
         for C in Ref (Self).CUs.Iterate loop
            declare
               U : Build.Compilation_Unit.Object renames
                     Compilation_Unit_Maps.Element (C);
            begin
               if With_Externally_Built
                 or else not U.Owning_View.Is_Externally_Built
               then
                  Result.Insert (Compilation_Unit_Maps.Key (C),
                                 Compilation_Unit_Maps.Element (C));
               end if;
            end;
         end loop;
      end return;
   end Compilation_Units;

   --------------
   -- Own_Unit --
   --------------

   function Own_Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object
   is
      C     : Unit_Maps.Cursor;
      Db    : constant View_Tables.View_Data_Ref := Ref (Self);

   begin
      C := Db.Own_CUs.Find (Name);

      if Unit_Maps.Has_Element (C) then
         for Root of Db.Own_CUs.Reference (C) loop
            if Self.View.Namespace_Roots.Contains (Root) then
               return Root.Unit (Name);
            end if;
         end loop;
      end if;

      return Build.Compilation_Unit.Undefined;
   end Own_Unit;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self   : Object;
      Sorted : Boolean := False) return GPR2.Build.Source.Sets.Object
   is (Build.Source.Sets.Create
         (Self,
          (if Sorted then Build.Source.Sets.Sorted
           else Build.Source.Sets.Unsorted)));

   -------------------
   -- Source_Option --
   -------------------

   function Source_Option (Self : Object) return Optional_Source_Info_Option is
     (Self.Get.Tree_Db.Source_Option);

   -------------
   -- Tree_Db --
   -------------

   function Tree_Db (Self : Object) return access GPR2.Build.Tree_Db.Object is
     (Self.Get.Tree_Db);

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : Object;
      Messages : in out GPR2.Log.Object) is
   begin
      View_Tables.Refresh (Ref (Self), Messages);
   end Update;

   -------------------
   -- View_Base_For --
   -------------------

   function View_Base_For (Data : View_Tables.View_Data) return Object is
   begin
      return Result : View_Db.Object do
         Result.Set (Data);
      end return;
   end View_Base_For;

   function View_Base_For
     (Self : Object;
      View : Project.View.Object) return Object
   is (if Self.Get.View = View then Self
       else Self.Get.Tree_Db.View_Database (View));

   ---------------------
   -- Visible_Sources --
   ---------------------

   function Visible_Sources
     (Self : Object) return GPR2.Build.Source.Sets.Object
   is (Build.Source.Sets.Create (Self, Build.Source.Sets.Recurse));

begin
   View_Tables.View_Base_For := View_Base_For'Access;
   View_Tables.Get_Ref       := Ref'Access;
end GPR2.Build.View_Db;
