--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Build.Source_Info.Sets;
with GPR2.Build.Tree_Db;

package body GPR2.Build.View_Db is

   use GPR2.Build.View_Tables;

   function View_Base_For (Data : View_Tables.View_Data) return Object;
   --  Create an object from the View_Tables data

   function Ref (Inst : Object) return View_Tables.View_Data_Ref
   is (Inst.Get);
   --  Extracts a reference to view_tables data from a View_Base instance

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (Self : Object) return Compilation_Unit.Maps.Map is
   begin
      return Result : Compilation_Unit.Maps.Map do
         for C in Ref (Self).CUs.Iterate loop
            Result.Insert (Compilation_Unit_Maps.Key (C),
                           Compilation_Unit_Maps.Element (C));
         end loop;
      end return;
   end Compilation_Units;

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
      Basename : Simple_Name) return Source_Info.Object
   is
      Def : constant View_Data_Ref := Ref (Self);
      C   : constant Basename_Source_Maps.Cursor :=
              Def.Sources.Find (Basename);
   begin
      if not Basename_Source_Maps.Has_Element (C) then
         return Source_Info.Undefined;
      else
         declare
            use type GPR2.Project.View.Object;

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
      Sorted : Boolean := False) return GPR2.Build.Source_Info.Sets.Object
   is (Source_Info.Sets.Create (Self, Sorted));

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Object) is
   begin
      View_Tables.Refresh (Ref (Self));
   end Update;

   -----------
   -- To_Db --
   -----------

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
      use type Project.View.Object;
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
      Basename : Simple_Name) return GPR2.Build.Source_Info.Object
   is
      Result : GPR2.Build.Source_Info.Object;
   begin
      Result := Self.Source (Basename);

      if Result.Is_Defined then
         return Result;
      end if;

      --  ??? Need access to the view's closure
      return Source_Info.Undefined;
   end Visible_Source;

begin

   View_Tables.View_Base_For := View_Base_For'Access;
   View_Tables.Get_Ref       := Ref'Access;

end GPR2.Build.View_Db;
