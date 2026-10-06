--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View.Vector;

package body GPR2.Build.Source.Sets is

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
     (Db        : Build.View_Db.Object;
      Option    : Source_Set_Option := Unsorted;
      Filter    : Filter_Function := null;
      F_Data    : Filter_Data'Class := No_Data;
      Ambiguous : Boolean := False) return Object is
   begin
      return (Db, Option, Filter, Filter_Data_Holders.To_Holder (F_Data),
              Ambiguous);
   end Create;

   ----------------
   -- Create_Key --
   ----------------

   function Create_Key (Path : Filename_Type) return Source_Key is
      BN : constant String := String (GPR2.Path_Name.Simple_Name (Path));
   begin
      return (Path_Len => Path'Length,
              Name_Len => BN'Length,
              Path     => Path,
              Basename => (if File_Names_Case_Sensitive then BN
                           else To_Lower_Fast (BN)));
   end Create_Key;

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
     (Self : Object) return Source_Iterators.Forward_Iterator'Class is
   begin
      return Iterate (Self, Include_Runtime => True);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Object; Include_Runtime : Boolean)
      return Source_Iterators.Forward_Iterator'Class
   is
      use View_Tables.Filename_Source_Maps;
      Opt : Source_Set_Option := Self.Option;

      procedure Check_Source
        (Data          : View_Data_Ref;
         Proxy         : Source_Proxy;
         Is_Visible    : out Boolean;
         Is_Compilable : out Boolean);

      procedure Check_Source
        (Data          : View_Data_Ref;
         Proxy         : Source_Proxy;
         Is_Visible    : out Boolean;
         Is_Compilable : out Boolean) is
      begin
         View_Tables.Source_Status
           (Data, Proxy, Is_Visible, Is_Compilable);
         if Is_Visible and then Self.Filter /= null then
            Is_Visible := Self.Filter
              (Self.Db.View, View_Tables.Source (Data, Proxy),
               Filter_Data_Holders.Element (Self.F_Data));
         end if;
      end Check_Source;

   begin
      if Opt = Unsorted and then Self.Filter /= null then
         Opt := Sorted;
      end if;

      if Self = Empty_Set or else not Self.Db.Is_Defined then
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
                     Visible, Compilable : Boolean;
                  begin
                     Check_Source
                       (Get_Ref (Self.Db), Proxy, Visible, Compilable);
                     if Visible then
                        Iter.Paths.Include (Create_Key (Key (C)), Src_Ctxt);
                     end if;
                  end;
               end loop;
            end return;

         when Recurse =>
            declare
               Basenames : GPR2.Containers.Filename_Set;
               View      : constant GPR2.Project.View.Object :=
                             Get_Ref (Self.Db).View;
               Closure   : GPR2.Project.View.Vector.Object :=
                             View.Closure (True, True, True);
               C         : GPR2.Project.View.Vector.Vector.Cursor;
            begin
               return Result : Source_Iterator (False) do
                  Result.Db := Self.Db;

                  --  Add the withed views sources, not overriding if
                  --  there's a basename clash.

                  --  Make sure the runtime is last, since any project may
                  --  override runtime sources

                  if Include_Runtime
                    and then View.Tree.Has_Runtime_Project
                  then
                     C := Closure.Find (View.Tree.Runtime_Project);

                     if GPR2.Project.View.Vector.Vector.Has_Element (C) then
                        Closure.Delete (C);
                        Closure.Append (View.Tree.Runtime_Project);
                     end if;
                  end if;

                  for V of Closure loop
                     if V.Kind in With_Object_Dir_Kind
                       and then not V.Is_Extended
                       and then (Include_Runtime or else not V.Is_Runtime)
                     then
                        declare
                           Db : constant View_Db.Object :=
                                  Get_Ref (Self.Db).Tree_Db.View_Database (V);
                           C_Db : constant View_Data_Ref := Get_Ref (Db);
                        begin
                           for C in C_Db.Sources.Iterate loop
                              --  Visibility is checked in the owning view,
                              --  then basename clashes across the closure.

                              declare
                                 Proxy : constant View_Tables.Source_Proxy :=
                                              Filename_Source_Maps.Element (C);
                                 Src_Ctxt : constant Source_Context :=
                                   (Proxy.Path_Len, Db, Proxy);
                                 Visible, Compilable : Boolean;
                                 C_BN : Containers.Filename_Type_Set.Cursor;
                                 Inserted : Boolean;

                              begin
                                 Check_Source
                                   (C_Db, Proxy, Visible, Compilable);
                                 if Visible then
                                    if Compilable
                                      and then not Self.Ambiguous
                                    then
                                       --  Check for basename clashes.
                                       Basenames.Insert
                                         (GPR2.Path_Name.Simple_Name
                                            (Proxy.Path_Name),
                                          C_BN, Inserted);
                                    else
                                       Inserted := True;
                                    end if;

                                    if Inserted then
                                       Result.Paths.Include
                                         (Create_Key
                                            (Filename_Source_Maps.Key (C)),
                                          Src_Ctxt);
                                    end if;
                                 end if;
                              end;
                           end loop;
                        end;
                     end if;
                  end loop;

               end return;
            end;
      end case;
   end Iterate;

   ----------
   -- Less --
   ----------

   function Less (P1, P2 : Source_Key) return Boolean is
   begin
      return (if P1.Basename = P2.Basename
              then P1.Path < P2.Path else P1.Basename < P2.Basename);
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

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Source : Source_Base.Object))
   is
      use type GPR2.Project.View.Object;

      Ctxt : constant Source_Context := Fetch_Source_Context (Position);
      Data : constant View_Data_Ref := Get_Ref (Ctxt.Owner);
      Ref  : constant Src_Info_Maps.Constant_Reference_Type :=
               (if Ctxt.Proxy.View = Data.View
                then Data.Src_Infos.Constant_Reference (Ctxt.Proxy.Path_Name)
                else Get_Data (Data.Tree_Db, Ctxt.Proxy.View).Src_Infos.
                  Constant_Reference (Ctxt.Proxy.Path_Name));
   begin
      Process (Ref.Element.all);
   end Query_Element;

end GPR2.Build.Source.Sets;
