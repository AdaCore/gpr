--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Containers;
with GPR2.Path_Name;
pragma Warnings (Off);
with GPR2.Project.View.Vector;
pragma Warnings (On);

package body GPR2.Build.Source.Sets is

   function Element
     (Self  : Object;
      Proxy : Source_Proxy) return Source.Object;

   procedure Ensure_Visible (C : in out Cursor);

   function Fetch_Source_Context (Position : Cursor) return Source_Context;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref is
     (Get_Ref (Inst));

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return Source.Object
   is
      Src_Ctxt : constant Source_Context := Fetch_Source_Context (Position);
   begin
      if Position.From_View_Db then
         return View_Tables.Source (-Self.Db, Src_Ctxt.Proxy);
      else
         return View_Tables.Source (-Src_Ctxt.Owner, Src_Ctxt.Proxy);
      end if;
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   function Create
     (Db     : Build.View_Db.Object;
      Option : Source_Set_Option := Unsorted;
      Filter : Filter_Function := null;
      F_Data : Filter_Data'Class := No_Data) return Object is
   begin
      return (Db, Option, Filter, Filter_Data_Holders.To_Holder (F_Data));
   end Create;

   -------------
   -- Element --
   -------------

   function Element
     (Self  : Object;
      Proxy : Source_Proxy) return Source.Object
   is
   begin
      return View_Tables.Source (-Self.Db, Proxy);
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Source.Object is
      Src_Ctxt : constant Source_Context := Fetch_Source_Context (Position);
   begin
      return View_Tables.Source (-Src_Ctxt.Owner, Src_Ctxt.Proxy);
   end Element;

   --------------------
   -- Ensure_Visible --
   --------------------

   procedure Ensure_Visible (C : in out Cursor) is
   begin
      if not C.From_View_Db then
         return;
      end if;

      while Has_Element (C) and then not Element (C).Is_Visible loop
         Filename_Source_Maps.Next (C.Current_Src);
      end loop;
   end Ensure_Visible;

   --------------------------
   -- Fetch_Source_Context --
   --------------------------

   function Fetch_Source_Context (Position : Cursor) return Source_Context is
      Proxy    : constant Source_Proxy :=
                   (if Position.From_View_Db
                    then View_Tables.Filename_Source_Maps.Element
                      (Position.Current_Src)
                    else No_Proxy);
      Src_Ctxt : constant Source_Context :=
                   (if Position.From_View_Db
                    then (Proxy.Path_Len, Position.Db, Proxy)
                    else Path_Source_Maps.Element (Position.Current_Path));
   begin
      return Src_Ctxt;
   end Fetch_Source_Context;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Source_Iterator) is
   begin
      if not Self.From_View_Db then
         Self.Paths.Clear;
      end if;
   end Finalize;

   -----------
   -- First --
   -----------

   overriding function First (Self : Source_Iterator) return Cursor is
      Candidate : Cursor;
   begin
      if not Self.Db.Is_Defined then
         return No_Element;

      elsif Self.From_View_Db then
         declare
            Db : constant View_Data_Ref := Get_Ref (Self.Db);
         begin
            if Db.Sources.Is_Empty then
               return No_Element;
            end if;

            Candidate :=
              (From_View_Db   => True,
               Db             => Self.Db,
               Current_Src    => Db.Sources.First);
            Ensure_Visible (Candidate);
         end;

      else
         if Self.Paths.Is_Empty then
            return No_Element;

         else
            Candidate :=
              (From_View_Db => False,
               Db           => Self.Db,
               Current_Path => Self.Paths.First);
         end if;
      end if;

      return Candidate;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.From_View_Db then
         return Filename_Source_Maps.Has_Element (Position.Current_Src);
      else
         return Path_Source_Maps.Has_Element
           (Position.Current_Path);
      end if;
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Object) return Boolean is
     (Self = Empty_Set or else Get_Ref (Self.Db).Sources.Is_Empty);

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object) return Source_Iterators.Forward_Iterator'Class
   is
      use View_Tables.Filename_Source_Maps;
      Opt : Source_Set_Option := Self.Option;

   begin
      if Opt = Unsorted and then Self.Filter /= null then
         Opt := Sorted;
      end if;

      if Self = Empty_Set then
         return Source_Iterator'
           (Ada.Finalization.Controlled with
            From_View_Db => False,
            Db           => Build.View_Db.Undefined,
            others       => <>);
      end if;

      case Opt is
         when Unsorted =>
            return Source_Iterator'
              (Ada.Finalization.Controlled with
               From_View_Db => True,
               Db           => Self.Db,
               Filter       => Self.Filter);

         when Sorted =>
            return Iter : Source_Iterator (False) do
               Iter.Db := Self.Db;

               for C in Get_Ref (Self.Db).Sources.Iterate loop
                  declare
                     Proxy    : constant View_Tables.Source_Proxy :=
                                  Filename_Source_Maps.Element (C);
                     Src_Ctxt : constant Source_Context :=
                                  (Proxy.Path_Len, Self.Db, Proxy);
                     Src      : constant Build.Source.Object :=
                                  Self.Element (Element (C));
                  begin
                     if Src.Is_Visible
                       and then
                         (Self.Filter = null
                          or else Self.Filter
                            (Self.Db.View,
                             Src,
                             Filter_Data_Holders.Element (Self.F_Data)))
                     then
                        Iter.Paths.Include (Key (C), Src_Ctxt);
                     end if;
                  end;
               end loop;
            end return;

         when Recurse =>
            declare
               Result : Source_Iterator (False);
               Basenames : GPR2.Containers.Filename_Set;
            begin
               Result.Db := Self.Db;

               --  Add the withed views sources, not overriding if
               --  there's a basename clash.

               for V
                 of Get_Ref (Self.Db).View.Closure (True, True, True)
               loop
                  if V.Kind in With_Object_Dir_Kind
                    and then not V.Is_Extended
                  then
                     declare
                        Db    : constant View_Db.Object :=
                                  Get_Ref (Self.Db).Tree_Db.View_Database (V);
                     begin
                        for C in Get_Ref (Db).Sources.Iterate loop
                           --  Note: we cannot just use Self.Element (Proxy)
                           --  here since this would give us a source with a
                           --  visibility for Self.Db.View, so in case the
                           --  source is owned by a withed unit, such
                           --  visibility would be null (e.g. False). We need
                           --  to use the withed view context here and then
                           --  filter on the basename to check if it's visible.

                           declare
                              Proxy    : constant View_Tables.Source_Proxy :=
                                           Filename_Source_Maps.Element (C);
                              Src_Ctxt : constant Source_Context :=
                                           (Proxy.Path_Len, V.View_Db, Proxy);
                              C_Db     : constant View_Data_Ref :=
                                           (Get_Ref (V.View_Db));
                              Src      : constant GPR2.Build.Source.Object :=
                                           View_Tables.Source
                                             (C_Db, Src_Ctxt.Proxy);
                              C_BN     : Containers.Filename_Type_Set.Cursor;
                              Inserted : Boolean;

                           begin
                              if Src.Is_Visible
                                and then
                                  (Self.Filter = null
                                   or else Self.Filter
                                     (Self.Db.View,
                                      Src,
                                      Filter_Data_Holders.Element
                                        (Self.F_Data)))
                              then
                                 if Src.Is_Compilable then
                                    --  Need to check for simple name clashes
                                    Basenames.Insert
                                      (Src.Path_Name.Simple_Name,
                                       C_BN, Inserted);
                                 else
                                    Inserted := True;
                                 end if;

                                 if Inserted then
                                    Result.Paths.Include
                                      (Filename_Source_Maps.Key (C),
                                       Src_Ctxt);
                                 end if;
                              end if;
                           end;
                        end loop;
                     end;
                  end if;
               end loop;

               return Result;
            end;
      end case;
   end Iterate;

   ----------
   -- Less --
   ----------

   function Less (P1, P2 : Filename_Type) return Boolean is
      BN1 : constant Simple_Name := GPR2.Path_Name.Simple_Name (P1);
      BN2 : constant Simple_Name := GPR2.Path_Name.Simple_Name (P2);
   begin
      --  Prioritize the sort on the simple name, only use the full path when
      --  the simple names are identical: for developers (in particular in Ada)
      --  the simple name is the important information.
      return (if BN1 = BN2 then P1 < P2 else BN1 < BN2);
   end Less;

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
         Filename_Source_Maps.Next (Result.Current_Src);
         Ensure_Visible (Result);

      else
         Path_Source_Maps.Next (Result.Current_Path);
      end if;

      return Result;
   end Next;

end GPR2.Build.Source.Sets;
