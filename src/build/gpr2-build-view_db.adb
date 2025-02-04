--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source.Sets;
with GPR2.Build.Tree_Db;
with GPR2.Project.View.Set;

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

   --------------------------------
   -- Excluded_Inherited_Sources --
   --------------------------------

   function Excluded_Inherited_Sources
     (Self : Object) return GPR2.Build.Source_Base.Vectors.Vector
   is
      Current : Object := Self;
      Todo    : GPR2.Project.View.Set.Object;
      Src     : GPR2.Build.Source_Base.Object;
   begin
      Todo.Include (Self.View);

      return Result : Build.Source_Base.Vectors.Vector do
         while not Todo.Is_Empty loop
            Current := Self.View_Base_For (Todo.First_Element);
            Todo.Delete_First;

            for Proxy of Ref (Current).Actually_Excluded loop
               if Proxy /= No_Proxy then
                  Src :=
                    Ref (Proxy.View.View_Db).Src_Infos.Element
                      (Proxy.Path_Name);
                  Result.Append (Src);
               end if;
            end loop;

            if Current.View.Is_Extending then
               for V of Current.View.Extended loop
                  Todo.Include (V);
               end loop;
            end if;
         end loop;
      end return;
   end Excluded_Inherited_Sources;

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

   ---------------
   -- Own_Units --
   ---------------

   function Own_Units (Self : Object) return Build.Compilation_Unit.Maps.Map is
      Db  : constant View_Tables.View_Data_Ref := Ref (Self);
      Res : Build.Compilation_Unit.Maps.Map;
   begin
      for C in Db.Own_CUs.Iterate loop
         declare
            Name : constant Name_Type := Unit_Maps.Key (C);
         begin
            Res.Insert (Name, Unit_Maps.Element (C).First_Element.Unit (Name));
         end;
      end loop;

      return Res;
   end Own_Units;

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
