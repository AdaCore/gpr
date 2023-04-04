--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Build.Tree_Db;
pragma Warnings (Off);
with GPR2.Project.View.Set;
pragma Warnings (On);

package body GPR2.Build.Source.Sets is

   use type GPR2.Project.View.Object;

   function Element (Self  : Object;
                     Proxy : Source_Proxy) return Source.Object;

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
      Option : Source_Set_Option := Unsorted;
      Filter : Filter_Function := null;
      F_Data : Filter_Data'Class := No_Data) return Object
   is
   begin
      return (Db, Option, Filter, Filter_Data_Holders.To_Holder (F_Data));
   end Create;

   -------------
   -- Element --
   -------------

   function Element (Self  : Object;
                     Proxy : Source_Proxy) return Source.Object
   is
      Db : constant View_Data_Ref :=
             (if Proxy.View /= Self.Db.View
              then -Self.Db.View_Base_For (Proxy.View)
              else -Self.Db);
   begin
      return Db.Src_Infos.Element (Proxy.Path_Name);
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Source.Object is
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
      Candidate : Cursor;
   begin
      if Self.From_View_Db then
         declare
            List : Basename_Source_Maps.Map renames
                     Get_Ref (Self.Db).Sources;
         begin
            if List.Is_Empty then
               return No_Element;
            end if;

            Candidate := (From_View_Db => True,
                          Db           => Self.Db,
                          Current_Src  => List.First);
            return Candidate;
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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
     (Get_Ref (Self.Db).Sources.Is_Empty);

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class
   is
      use View_Tables.Basename_Source_Maps;
      Opt : Source_Set_Option := Self.Option;

   begin
      if Opt = Unsorted and then Self.Filter /= null then
         Opt := Sorted;
      end if;

      case Opt is
         when Unsorted =>
            return Source_Iterator'(True, Self.Db, Self.Filter);

         when Sorted =>
            return Iter : Source_Iterator (False) do
               Iter.Db := Self.Db;

               for C in Get_Ref (Self.Db).Sources.Iterate loop
                  if Self.Filter = null
                    or else Self.Filter
                      (Self.Element (Element (C)),
                       Filter_Data_Holders.Element (Self.F_Data))
                  then
                     Iter.Paths.Insert (Key (C), Element (C));
                  end if;
               end loop;
            end return;

         when Recurse =>
            declare
               Result : Source_Iterator (False);
            begin
               Result.Db := Self.Db;

               --  Add the withed views sources, not overriding if
               --  there's a basename clash.

               for V of Get_Ref (Self.Db).View.Closure (Include_Self => True)
               loop
                  if V.Kind in With_Object_Dir_Kind then
                     declare
                        Db   : constant View_Db.Object :=
                                 Get_Ref (Self.Db).Tree_Db.View_Database (V);
                        Pos  : Path_Source_Maps.Cursor;
                        Done : Boolean;
                     begin
                        for C in Get_Ref (Db).Sources.Iterate loop
                           if Self.Filter = null
                             or else Self.Filter
                               (Self.Element (Element (C)),
                                Filter_Data_Holders.Element (Self.F_Data))
                           then
                              Result.Paths.Insert
                                (Key       => Basename_Source_Maps.Key (C),
                                 New_Item  => Basename_Source_Maps.Element (C),
                                 Position  => Pos,
                                 Inserted  => Done);
                           end if;
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

end GPR2.Build.Source.Sets;
