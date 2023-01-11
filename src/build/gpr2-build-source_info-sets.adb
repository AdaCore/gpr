--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Build.Tree_Db;

package body GPR2.Build.Source_Info.Sets is

   use type GPR2.Project.View.Object;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref
     is (Get_Ref (Inst));

   function Tree_Db
     (Db : Build.View_Db.Object) return access GPR2.Build.Tree_Db.Object
   is (Get_Ref (Db).Tree_Db);

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Constant_Reference_Type
   is
      Proxy : constant Source_Proxy :=
                (if Position.From_View_Db
                 then  Basename_Source_Maps.Element (Position.Current_Src)
                 else Path_Source_Maps.Element (Position.Current_Path));
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

   function Create
     (Db     : Build.View_Db.Object;
      Option : Source_Set_Option := Unsorted) return Object
   is
   begin
      return (Db, Option);
   end Create;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Source_Info.Object is
      Proxy : constant Source_Proxy :=
                (if Position.From_View_Db
                 then Basename_Source_Maps.Element (Position.Current_Src)
                 else Path_Source_Maps.Element (Position.Current_Path));
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
   begin
      if Self.From_View_Db then
         declare
            List : Basename_Source_Maps.Map renames Get_Ref (Self.Db).Sources;
         begin
            if List.Is_Empty then
               return No_Element;

            else
               return (From_View_Db => True,
                       Db           => Self.Db,
                       Current_Src  => List.First);
            end if;
         end;

      else
         if Self.Paths.Is_Empty then
            return No_Element;

         else
            return
              (From_View_Db => False,
               Db           => Self.Db,
               Current_Path => Self.Paths.First);
         end if;
      end if;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.From_View_Db then
         return Basename_Source_Maps.Has_Element (Position.Current_Src);
      else
         return Path_Source_Maps.Has_Element (Position.Current_Path);
      end if;
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class
   is
   begin
      case Self.Option is
         when Unsorted =>
            return Source_Iterator'(True, Self.Db);

         when Sorted =>
            return Iter : Source_Iterator (False) do
               Iter.Db := Self.Db;

               for C in Get_Ref (Self.Db).Sources.Iterate loop
                  Iter.Paths.Insert
                    (View_Tables.Basename_Source_Maps.Key (C),
                     View_Tables.Basename_Source_Maps.Element (C));
               end loop;
            end return;

         when Recurse =>
            declare
               Result : Source_Iterator (False);
            begin
               Result.Db := Self.Db;

               --  Add first the view's sources, unconditionally

               for C in Get_Ref (Self.Db).Sources.Iterate loop
                  Result.Paths.Insert
                    (View_Tables.Basename_Source_Maps.Key (C),
                     View_Tables.Basename_Source_Maps.Element (C));
               end loop;

               --  Then add the withed views sources, not overriding if
               --  there's a basename clash.

               for V of Get_Ref (Self.Db).View.Closure loop
                  if V.Kind in With_Object_Dir_Kind then
                     declare
                        Db   : constant View_Db.Object :=
                                 Get_Ref (Self.Db).Tree_Db.View_Database (V);
                        Pos  : Path_Source_Maps.Cursor;
                        Done : Boolean;
                     begin
                        for C in Get_Ref (Db).Sources.Iterate loop
                           Result.Paths.Insert
                             (Key       => Basename_Source_Maps.Key (C),
                              New_Item  => Basename_Source_Maps.Element (C),
                              Position  => Pos,
                              Inserted  => Done);

                           --  ??? In case of basename clash, we should issue a
                           --  linter warning...
                        end loop;
                     end;
                  end if;
               end loop;

               return Result;
            end;
      end case;
   end Iterate;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Source_Iterator;
      Position : Cursor) return Cursor
   is
      Result : Cursor := Position;

   begin
      if Self.From_View_Db then
         Basename_Source_Maps.Next (Result.Current_Src);

      else
         Path_Source_Maps.Next (Result.Current_Path);
      end if;

      return Result;
   end Next;

end GPR2.Build.Source_Info.Sets;
