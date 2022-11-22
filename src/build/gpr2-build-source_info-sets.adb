--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, ".* is not referenced");
with GPR2.Project.View.Set;
pragma Warnings (On, ".* is not referenced");

with GPR2.Build.Tree_Db;

package body GPR2.Build.Source_Info.Sets is

   use type GPR2.Project.View.Object;

   type Source_Iterator (Sort : Boolean) is
     new Source_Iterators.Forward_Iterator
   with record
      Db    : Build.View_Db.Object;

      case Sort is
         when True =>
            Paths : Path_Name_Sets.Set;
         when False =>
            null;
      end case;
   end record;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref
     is (Get_Ref (Inst));

   function Tree_Db
     (Db : Build.View_Db.Object) return access GPR2.Build.Tree_Db.Object
   is (Get_Ref (Db).Tree_Db);

   overriding function First (Self : Source_Iterator) return Cursor;
   overriding function Next
     (Self     : Source_Iterator;
      Position : Cursor) return Cursor;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Constant_Reference_Type
   is
      Proxy : constant Source_Proxy :=
                Basename_Source_Maps.Element (Position.Current_Src);
      Db    : constant View_Data_Ref :=
                (if Proxy.View /= Position.Db.View
                 then Get_Data (Tree_Db (Position.Db), Proxy.View)
                 else -Self.Db);
      Ref   : constant Src_Info_Maps.Constant_Reference_Type :=
                Db.Src_Infos.Constant_Reference
                  (Proxy.Path_Name);
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   function Create (Db     : Build.View_Db.Object;
                    Sorted : Boolean := False) return Object
   is
   begin
      return (Db   => Db,
              Sort => Sorted);
   end Create;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Source_Info.Object is
      Proxy : constant Source_Proxy :=
                 Basename_Source_Maps.Element (Position.Current_Src);
      Db    : constant View_Data_Ref :=
                (if Proxy.View /= Position.Db.View
                 then Get_Data (Tree_Db (Position.Db), Proxy.View)
                 else -Position.Db);
   begin
      return Db.Src_Infos.Element (Proxy.Path_Name);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Self : Source_Iterator) return Cursor is
      Sources : Basename_Source_Maps.Map renames Get_Ref (Self.Db).Sources;
   begin
      if Self.Sort then
         if Self.Paths.Is_Empty then
            return (Db           => Self.Db,
                    Sort         => False,
                    Current_Src  => Basename_Source_Maps.No_Element,
                    Current_Path => Path_Name_Sets.No_Element);

         else
            return
              (Db           => Self.Db,
               Sort         => True,
               Current_Src  => Sources.Find
                                 (Self.Paths.First_Element.Simple_Name),
               Current_Path => Self.Paths.First);
         end if;

      else
         return (Db           => Self.Db,
                 Sort         => False,
                 Current_Src  => Sources.First,
                 Current_Path => Path_Name_Sets.No_Element);
      end if;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Basename_Source_Maps.Has_Element (Position.Current_Src);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean
   is
   begin
      return Get_Ref (Self.Db).Sources.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class
   is
   begin
      if Self.Sort then
         return Result : Source_Iterator (True) do
            Result.Db := Self.Db;

            for S of Get_Ref (Self.Db).Sources loop
               Result.Paths.Insert (S.Path_Name);
            end loop;
         end return;

      else
         return Source_Iterator'(Sort => False,
                                 Db   => Self.Db);
      end if;
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Source_Iterator;
      Position : Cursor) return Cursor
   is
      Result : Cursor := Position;
      use Basename_Source_Maps;
      use Path_Name_Sets;

   begin
      if not Self.Sort then
         Next (Result.Current_Src);

      else
         Next (Result.Current_Path);

         if Has_Element (Result.Current_Path) then
            Result.Current_Src :=
              Get_Ref (Self.Db).Sources.Find
                (Element (Result.Current_Path).Simple_Name);

         else
            Result.Current_Src := Basename_Source_Maps.No_Element;
         end if;
      end if;

      return Result;
   end Next;

end GPR2.Build.Source_Info.Sets;
