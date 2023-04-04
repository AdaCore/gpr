--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  with GPR2.Build.Compilation_Input.Sets;
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

   function Ref (Inst : Object) return View_Tables.View_Data_Ref
   is (Inst.Get);
   --  Extracts a reference to view_tables data from a View_Base instance

   --  ------------------------
   --  -- Compilation_Inputs --
   --  ------------------------
   --
   --  function Compilation_Inputs
   --    (Self : Object) return Build.Compilation_Input.Sets.Object is
   --  begin
   --     return Build.Compilation_Input.Sets.Create (Self);
   --  end Compilation_Inputs;

   ----------------------
   -- Compilation_Unit --
   ----------------------

   function Compilation_Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object is
   begin
      return Ref (Self).CUs.Element (Name);
   end Compilation_Unit;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (Self : Object) return Build.Compilation_Unit.Maps.Map is
   begin
      return Result : Build.Compilation_Unit.Maps.Map do
         for C in Ref (Self).CUs.Iterate loop
            Result.Insert (Compilation_Unit_Maps.Key (C),
                           Compilation_Unit_Maps.Element (C));
         end loop;
      end return;
   end Compilation_Units;

   --------------------------
   -- Has_Compilation_Unit --
   --------------------------

   function Has_Compilation_Unit
     (Self : Object;
      Name : Name_Type) return Boolean
   is
   begin
      return Ref (Self).CUs.Contains (Name);
   end Has_Compilation_Unit;

   ----------------
   -- Has_Source --
   ----------------

   function Has_Source
     (Self     : Object;
      Basename : Simple_Name) return Boolean
   is
   begin
      return Ref (Self).Sources.Contains (Basename);
   end Has_Source;

   ------------
   -- Source --
   ------------

   function Source
     (Self     : Object;
      Basename : Simple_Name) return Build.Source.Object
   is
      Def : constant View_Data_Ref := Ref (Self);
      C   : constant Basename_Source_Maps.Cursor :=
              Def.Sources.Find (Basename);
   begin
      if not Basename_Source_Maps.Has_Element (C) then
         return Build.Source.Undefined;
      else
         declare
            Proxy : Source_Proxy renames Basename_Source_Maps.Element (C);

         begin
            if Proxy.View = Def.View then
               return Def.Src_Infos.Element (Proxy.Path_Name);
            else
               return Get_Data
                 (Def.Tree_Db, Proxy.View).Src_Infos.Element (Proxy.Path_Name);
            end if;
         end;
      end if;
   end Source;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self : Object;
      Sorted : Boolean := False) return GPR2.Build.Source.Sets.Object
   is (Build.Source.Sets.Create
         (Self,
          (if Sorted then Build.Source.Sets.Sorted
           else Build.Source.Sets.Unsorted)));

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
   is
   begin
      if Self.View = View then
         return Self;
      else
         return Self.Get.Tree_Db.View_Database (View);
      end if;
   end View_Base_For;

   --------------------
   -- Visible_Source --
   --------------------

   function Visible_Source
     (Self     : Object;
      Basename : Simple_Name) return Source_Context
   is
      Result : GPR2.Build.Source.Object;
   begin
      --  Look for the source in the view's closure (withed or limited withed
      --  views)

      for V of Self.View.Closure (Include_Self => True) loop
         declare
            Db : constant Object := Self.View_Base_For (V);
         begin
            Result := Db.Source (Basename);

            if Result.Is_Defined then
               return (V, Result);
            end if;
         end;
      end loop;

      return No_Context;
   end Visible_Source;

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
