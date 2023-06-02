--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Source.Ada_Parser;
with GPR2.Build.Tree_Db;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Tree;

package body GPR2.Build.View_Tables is

   procedure Add_Unit_Part
     (NS_Db   : in out View_Data;
      CU        : Name_Type;
      Kind      : Unit_Kind;
      Sep_Name  : Optional_Name_Type;
      View_Db   : in out View_Data;
      Path      : Path_Name.Object;
      Index     : Unit_Index;
      Messages  : in out GPR2.Log.Object)
     with Pre => NS_Db.Is_Root
                   and then (Kind = S_Separate) = (Sep_Name'Length > 0);

   procedure Remove_Unit_Part
     (NS_Db    : in out View_Data;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : in out View_Data;
      Path     : Path_Name.Object;
      Index    : Unit_Index)
     with Pre => NS_Db.Is_Root
                   and then (Kind = S_Separate) = (Sep_Name'Length > 0);

   procedure Resolve_Visibility
     (Data     : in out View_Data;
      Cursor   : in out Basename_Source_List_Maps.Cursor;
      Messages : in out GPR2.Log.Object);

   procedure Add_Unit_Ownership
     (To   : in out View_Data;
      CU   : Name_Type;
      Root : View_Data);

   procedure Remove_Unit_Ownership
     (From : in out View_Data;
      CU   : Name_Type;
      Root : View_Data);

   package Update_Sources_List is
      procedure Process
        (Data          : in out View_Data;
         Stop_On_Error : Boolean;
         Messages      : in out GPR2.Log.Object);
      --  Update the list of sources
   end Update_Sources_List;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref
   is (Get_Ref (Inst));

   procedure Check_Separate
     (Root_Db : View_Tables.View_Data;
      File    : in out Source.Object)
     with Pre => Root_Db.Is_Root
                   and then File.Has_Single_Unit
                   and then File.Unit.Kind = S_Separate;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Data               : in out View_Data;
      View_Owner         : GPR2.Project.View.Object;
      Path               : GPR2.Path_Name.Object;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
   is
      use type GPR2.Project.View.Object;

      C_Overload : Basename_Source_List_Maps.Cursor;
      Done       : Boolean;
      Proxy      : constant Source_Proxy :=
                     (View      => View_Owner,
                      Path_Name => Path,
                      Inh_From  => Extended_View);
      Owner_Db   : constant View_Data :=
                     (if View_Owner = Data.View
                      then Data
                      else Get_Data (Data.Tree_Db, View_Owner));
      Src_Info   : constant Src_Info_Maps.Constant_Reference_Type :=
                     Owner_Db.Src_Infos.Constant_Reference (Path);

   begin
      Data.Overloaded_Srcs.Insert (Path.Simple_Name,
                                   Source_Proxy_Sets.Empty_Set,
                                   C_Overload,
                                   Done);
      Data.Overloaded_Srcs.Reference (C_Overload).Include (Proxy);

      if not Data.Langs_Usage.Contains (Src_Info.Language) then
         Data.Langs_Usage.Insert (Src_Info.Language, 1);
      else
         declare
            Val : constant Sources_By_Langs_Maps.Reference_Type :=
                    Data.Langs_Usage.Reference (Src_Info.Language);
         begin
            Val.Element.all := Val + 1;
         end;
      end if;

      if Resolve_Visibility then
         View_Tables.Resolve_Visibility (Data, C_Overload, Messages);
      end if;
   end Add_Source;

   ------------------------
   -- Add_Unit_Ownership --
   ------------------------

   procedure Add_Unit_Ownership
     (To   : in out View_Data;
      CU   : Name_Type;
      Root : View_Data)
   is
      C         : Unit_Maps.Cursor;
      New_Set   : Project.View.Set.Object;

   begin
      C := To.Own_CUs.Find (CU);

      if Unit_Maps.Has_Element (C) then
         To.Own_CUs.Reference (C).Insert (Root.View);
      else
         New_Set.Insert (Root.View);
         To.Own_CUs.Include (CU, New_Set);
      end if;

      if not To.View.Is_Extended then
         for NS of To.View.Aggregate_Libraries loop
            if NS.Namespace_Roots.Contains (Root.View) then
               Add_Unit_Ownership (Get_Data (To.Tree_Db, NS), CU, Root);
            end if;
         end loop;
      end if;
   end Add_Unit_Ownership;

   -------------------
   -- Add_Unit_Part --
   -------------------

   procedure Add_Unit_Part
     (NS_Db   : in out View_Data;
      CU        : Name_Type;
      Kind      : Unit_Kind;
      Sep_Name  : Optional_Name_Type;
      View_Db   : in out View_Data;
      Path      : Path_Name.Object;
      Index     : Unit_Index;
      Messages  : in out GPR2.Log.Object)
   is
      Cursor  : Compilation_Unit_Maps.Cursor;
      Done    : Boolean;
      Other   : Path_Name.Object;
      Success : Boolean := True;

   begin
      Cursor := NS_Db.CUs.Find (CU);

      if not Compilation_Unit_Maps.Has_Element (Cursor) then
         declare
            CU_Instance : Compilation_Unit.Object :=
                            Compilation_Unit.Create (CU, NS_Db.View);
         begin
            CU_Instance.Add (Kind, View_Db.View, Path, Index, Sep_Name, Done);
            NS_Db.CUs.Insert (CU, CU_Instance);

            if Kind /= S_Separate then
               Add_Unit_Ownership (View_Db, CU, NS_Db);
            end if;
         end;
      else
         declare
            CU_Instance : constant Compilation_Unit_Maps.Reference_Type :=
                            NS_Db.CUs.Reference (Cursor);
            Old_Owner   : constant Project.View.Object :=
                            CU_Instance.Owning_View;
            use type GPR2.Project.View.Object;
         begin
            CU_Instance.Add
              (Kind, View_Db.View, Path, Index, Sep_Name, Success);

            if Success and then Old_Owner /= CU_Instance.Owning_View then

               --  Owning view changed, let's apply this change
               if Old_Owner.Is_Defined then
                  declare
                     Old_Db : constant View_Data_Ref :=
                                Get_Data (NS_Db.Tree_Db, Old_Owner);
                  begin
                     Remove_Unit_Ownership (Old_Db, CU, NS_Db);
                  end;
               end if;

               if CU_Instance.Owning_View.Is_Defined then
                  Add_Unit_Ownership (View_Db, CU, NS_Db);
               end if;
            end if;

            if not Success then
               Other := CU_Instance.Get (Kind, Sep_Name).Source;

               Messages.Append
                 (Message.Create
                    (Level   => Message.Warning,
                     Message => "Duplicated " &
                       Image (Kind) & " for unit """ & String (CU) & """ in " &
                       String (Other.Value) & " and " & String (Path.Value),
                     Sloc    =>
                       Source_Reference.Create
                         (NS_Db.View.Path_Name.Value, 0, 0)));
            end if;
         end;
      end if;

      if Success and then Kind = S_Separate then
         declare
            Full_Name : constant Name_Type :=
                          GPR2."&" (GPR2."&" (CU, "."), Sep_Name);
         begin
            NS_Db.Separates.Include (Full_Name, CU);
         end;
      end if;
   end Add_Unit_Part;

   --------------------
   -- Check_Separate --
   --------------------

   procedure Check_Separate
     (Root_Db : View_Tables.View_Data;
      File    : in out Source.Object)
   is
      C : Name_Maps.Cursor;
   begin
      loop
         C := Root_Db.Separates.Find (File.Unit.Name);

         exit when not Name_Maps.Has_Element (C);

         --  The separate_from unit is in the separates map, this
         --  happens in case of separates of separates.
         --  We need to rebase on the actual compilation unit in this case.

         declare
            U            : constant Source.Unit_Part := File.Unit;
            New_Name     : constant Name_Type := Name_Maps.Element (C);
            --  New compilation unit name
            P_Simple     : constant Name_Type := U.Name
                                         (New_Name'Length + 2 .. U.Name_Len);
            --  Simple unit name of the separate parent
            New_Sep_Name : constant Name_Type :=
                             GPR2."&" (GPR2."&" (P_Simple, "."),
                                       U.Separate_Name);
         begin
            File.Update_Unit
              (Source.Create
                 (Unit_Name      => New_Name,
                  Index          => U.Index,
                  Kind           => U.Kind,
                  Kind_Ambiguous => False,
                  Separate_Name  => New_Sep_Name));
         end;
      end loop;
   end Check_Separate;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Db : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref
   is
   begin
      return -Db.View_Database (View);
   end Get_Data;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Data     : in out View_Data;
      Messages : in out GPR2.Log.Object) is
   begin
      Update_Sources_List.Process (Data, False, Messages);

      --  Disambiguate unit kind for Ada bodies

      if Data.Tree_Db.Source_Option >= Sources_Units
        and then not Data.View.Is_Extended
        and then Data.View.Kind in With_Object_Dir_Kind
      then
         --  Only look at "final" views: e.g. not inherited.
         --  That's because the compilation unit may not be complete until we
         --  reach the inheriting view that gathers together all the sources.

         for C_Proxy in Data.Sources.Iterate loop
            declare
               Proxy : constant Source_Proxy :=
                         Basename_Source_Maps.Element (C_Proxy);
               Db    : constant View_Data_Ref :=
                         Get_Data (Data.Tree_Db, Proxy.View);
               S_Ref : constant Src_Info_Maps.Reference_Type :=
                         Db.Src_Infos.Reference (Proxy.Path_Name);

            begin
               if S_Ref.Language = Ada_Language
                 and then not S_Ref.Has_Index
               then
                  declare
                     Unit : Source.Unit_Part := S_Ref.Unit;

                  begin
                     if Unit.Kind_Ambiguous
                       and then Unit.Kind = S_Body
                     then
                        --  If an ambiguous body is found, check if it has a
                        --  spec: it there is, then it's an actual body.

                        Root_Loop :
                        for Root_View of Data.View.Namespace_Roots loop
                           declare
                              Units : Compilation_Unit_Maps.Map renames
                                        Get_Data (Data.Tree_Db, Root_View).CUs;
                           begin
                              if Units (Unit.Name).Has_Part (S_Spec) then
                                 --  If it has a corresponding spec, then it's
                                 --  a body
                                 Unit.Kind_Ambiguous := False;
                                 S_Ref.Update_Unit (Unit);

                                 exit Root_Loop;
                              end if;
                           end;
                        end loop Root_Loop;
                     end if;

                     if Unit.Kind_Ambiguous then
                        --  If still ambiguous after the above path, we
                        --  have a body with no spec and dot_repl in the
                        --  filename: can be a child body-only, or a separate.
                        --  We need to parse the source to determine the
                        --  exact kind.
                        --  ??? Use the ali parser if any corresponding ali
                        --  file is present as it's faster than the Ada
                        --  parser.
                        Build.Source.Ada_Parser.Compute (S_Ref, False);

                        if S_Ref.Unit (No_Index).Name /= Unit.Name
                          or else S_Ref.Unit (No_Index).Kind /= Unit.Kind
                        then
                           for Root_View of Data.View.Namespace_Roots loop
                              declare
                                 Root_Db : constant View_Data_Ref :=
                                             Get_Data (Data.Tree_Db,
                                                       Root_View);
                              begin
                                 --  Remove old unit from the namespace root
                                 --  list.

                                 Remove_Unit_Part
                                   (Root_Db,
                                    Unit.Name,
                                    Unit.Kind,
                                    Unit.Separate_Name,
                                    Data,
                                    S_Ref.Path_Name,
                                    S_Ref.Unit.Index);

                                 --  And add the new one
                                 Add_Unit_Part
                                   (Root_Db,
                                    S_Ref.Unit.Name,
                                    S_Ref.Unit.Kind,
                                    S_Ref.Unit.Separate_Name,
                                    Data,
                                    S_Ref.Path_Name,
                                    S_Ref.Unit.Index,
                                    Messages);
                              end;
                           end loop;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end loop;

         --  Now check separates of separates

         for C_Proxy in Data.Sources.Iterate loop
            declare
               Proxy : constant Source_Proxy :=
                         Basename_Source_Maps.Element (C_Proxy);
               Db    : constant View_Data_Ref :=
                         Get_Data (Data.Tree_Db, Proxy.View);
               S_Ref : constant Src_Info_Maps.Reference_Type :=
                         Db.Src_Infos.Reference (Proxy.Path_Name);
            begin
               if S_Ref.Has_Units
                 and then S_Ref.Has_Single_Unit
                 and then S_Ref.Unit.Kind = S_Separate
               then
                  for Root of Data.View.Namespace_Roots loop
                     declare
                        Root_Db : constant View_Data_Ref :=
                                    Get_Data (Data.Tree_Db, Root);
                        Old     : constant Source.Unit_Part := S_Ref.Unit;
                     begin
                        Check_Separate (Root_Db, S_Ref);

                        if S_Ref.Unit.Name /= Old.Name then
                           Remove_Unit_Part
                             (Root_Db,
                              Old.Name,
                              Old.Kind,
                              Old.Separate_Name,
                              Data,
                              S_Ref.Path_Name,
                              S_Ref.Unit.Index);

                           --  And add the new one
                           Add_Unit_Part
                             (Root_Db,
                              S_Ref.Unit.Name,
                              S_Ref.Unit.Kind,
                              S_Ref.Unit.Separate_Name,
                              Data,
                              S_Ref.Path_Name,
                              S_Ref.Unit.Index,
                              Messages);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end if;
   end Refresh;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Data               : in out View_Data;
      View_Owner         : GPR2.Project.View.Object;
      Path               : GPR2.Path_Name.Object;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
   is
      use type GPR2.Project.View.Object;

      Basename : constant Simple_Name := Path.Simple_Name;
      C_Overload : Basename_Source_List_Maps.Cursor;
      Proxy    : constant Source_Proxy := (View      => View_Owner,
                                           Path_Name => Path,
                                           Inh_From  => Extended_View);
      Owner_Db   : constant View_Data :=
                     (if View_Owner = Data.View
                      then Data
                      else Get_Data (Data.Tree_Db, View_Owner));
      Src_Info   : constant Src_Info_Maps.Constant_Reference_Type :=
                     Owner_Db.Src_Infos.Constant_Reference (Path);

   begin
      C_Overload := Data.Overloaded_Srcs.Find (Basename);
      Data.Overloaded_Srcs.Reference (C_Overload).Delete (Proxy);

      declare
         Val : constant Sources_By_Langs_Maps.Reference_Type :=
                 Data.Langs_Usage.Reference (Src_Info.Language);
      begin
         pragma Assert (Val > 0);
         Val.Element.all := Val - 1;
      end;

      if Resolve_Visibility then
         View_Tables.Resolve_Visibility (Data, C_Overload, Messages);
      end if;
   end Remove_Source;

   ---------------------------
   -- Remove_Unit_Ownership --
   ---------------------------

   procedure Remove_Unit_Ownership
     (From : in out View_Data;
      CU   : Name_Type;
      Root : View_Data)
   is
      C : Unit_Maps.Cursor;
   begin
      C := From.Own_CUs.Find (CU);

      From.Own_CUs.Reference (C).Delete (Root.View);

      if Unit_Maps.Element (C).Is_Empty then
         From.Own_CUs.Delete (C);
      end if;

      if not From.View.Is_Extended then
         for NS of From.View.Aggregate_Libraries loop
            if NS.Namespace_Roots.Contains (Root.View) then
               Remove_Unit_Ownership (Get_Data (From.Tree_Db, NS), CU, Root);
            end if;
         end loop;
      end if;
   end Remove_Unit_Ownership;

   ----------------------
   -- Remove_Unit_Part --
   ----------------------

   procedure Remove_Unit_Part
     (NS_Db    : in out View_Data;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : in out View_Data;
      Path     : Path_Name.Object;
      Index    : Unit_Index)
   is
      Cursor    : Compilation_Unit_Maps.Cursor;
      Old_Owner : Project.View.Object;
   begin
      Cursor := NS_Db.CUs.Find (CU);

      if not Compilation_Unit_Maps.Has_Element (Cursor) then
         return;
      end if;

      declare
         use type GPR2.Project.View.Object;
         CU_Ref : constant Compilation_Unit_Maps.Reference_Type :=
                    NS_Db.CUs.Reference (Cursor);
      begin
         Old_Owner := CU_Ref.Owning_View;

         CU_Ref.Remove
           (Kind, View_Db.View, Path, Index, Sep_Name);

         if CU_Ref.Owning_View /= Old_Owner then
            if Old_Owner.Is_Defined then
               pragma Assert (Old_Owner = View_Db.View);
               Remove_Unit_Ownership (View_Db, CU, NS_Db);
            end if;

            if CU_Ref.Owning_View.Is_Defined then
               Add_Unit_Ownership
                 (Get_Data (NS_Db.Tree_Db, CU_Ref.Owning_View), CU, NS_Db);
            end if;
         end if;
      end;

      if Compilation_Unit_Maps.Element (Cursor).Is_Empty then
         NS_Db.CUs.Delete (Cursor);
      end if;

      if Kind = S_Separate then
         declare
            Full_Name : constant Name_Type :=
                          GPR2."&" (GPR2."&" (CU, "."), Sep_Name);
         begin
            NS_Db.Separates.Delete (Full_Name);
         end;
      end if;
   end Remove_Unit_Part;

   ------------------------
   -- Resolve_Visibility --
   ------------------------

   procedure Resolve_Visibility
     (Data     : in out View_Data;
      Cursor   : in out Basename_Source_List_Maps.Cursor;
      Messages : in out Log.Object)
   is
      use type Ada.Containers.Count_Type;
      use type Project.View.Object;
      use type Source_Reference.Value.Object;

      procedure Propagate_Visible_Source_Removal (Src : Source_Proxy);
      procedure Propagate_Visible_Source_Added (Src : Source_Proxy);

      ------------------------------------
      -- Propagate_Visible_Source_Added --
      ------------------------------------

      procedure Propagate_Visible_Source_Added (Src : Source_Proxy) is
         View_Db   : constant View_Data_Ref :=
                       Get_Data (Data.Tree_Db, Src.View);
         Src_Info  : constant Src_Info_Maps.Reference_Type :=
                       View_Db.Src_Infos.Reference (Src.Path_Name);

      begin
         if Data.View.Is_Extended then
            --  ??? Check the view's list of excluded sources before doing that
            declare
               Ext_Data : constant View_Data_Ref :=
                            Get_Data (Data.Tree_Db, Data.View.Extending);
            begin
               Add_Source
                 (Ext_Data,
                  Src.View,
                  Src.Path_Name,
                  Extended_View      => Data.View,
                  Resolve_Visibility => True,
                  Messages           => Messages);
            end;
         end if;

         if Src_Info.Has_Units
           and then not Data.View.Is_Extended
         then
            --  Update unit information. Note that we do that on "final"
            --  views only, so not if the view is extended or aggregated in
            --  a library, since that's the extending or aggregating lib
            --  that will have the full picture on what is visible or not.

            for U of Src_Info.Units loop
               for Root of Src.View.Namespace_Roots loop
                  Add_Unit_Part
                    (NS_Db    => Get_Data (Data.Tree_Db, Root),
                     CU       => U.Name,
                     Kind     => U.Kind,
                     Sep_Name => U.Separate_Name,
                     View_Db  => Data,
                     Path     => Src_Info.Path_Name,
                     Index    => U.Index,
                     Messages => Messages);
               end loop;
            end loop;
         end if;
      end Propagate_Visible_Source_Added;

      --------------------------------------
      -- Propagate_Visible_Source_Removal --
      --------------------------------------

      procedure Propagate_Visible_Source_Removal (Src : Source_Proxy) is
         View_Db  : constant View_Data_Ref :=
                      Get_Data (Data.Tree_Db, Src.View);
         Src_Info : constant Source.Object :=
                      View_Db.Src_Infos (Src.Path_Name);

      begin
         if Src_Info.Has_Units and then not Data.View.Is_Extended then
            for U of Src_Info.Units loop
               for Root of Src.View.Namespace_Roots loop
                  Remove_Unit_Part
                    (Get_Data (Data.Tree_Db, Root),
                     CU       => U.Name,
                     Kind     => U.Kind,
                     Sep_Name => U.Separate_Name,
                     View_Db  => Data,
                     Path     => Src_Info.Path_Name,
                     Index    => U.Index);
               end loop;
            end loop;
         end if;

         if Data.View.Is_Extended then
            Remove_Source
              (Get_Data (Data.Tree_Db, Data.View.Extending),
               Src.View,
               Src.Path_Name,
               Extended_View      => Data.View,
               Resolve_Visibility => True,
               Messages           => Messages);
         end if;
      end Propagate_Visible_Source_Removal;

      Basename  : constant Simple_Name :=
                    Basename_Source_List_Maps.Key (Cursor);
      Set       : constant Source_Proxy_Sets.Set :=
                    Basename_Source_List_Maps.Element (Cursor);
      Candidate : Source_Proxy;
      Current   : Source_Proxy;
      C_Src     : Basename_Source_Maps.Cursor :=
                    Data.Sources.Find (Basename);
      C_Info    : Src_Info_Maps.Cursor;
      C_Info2   : Src_Info_Maps.Cursor;
      SR1, SR2  : Source_Reference.Value.Object;
      Clashes   : GPR2.Containers.Source_Value_Set;

   begin
      if Set.Is_Empty then
         --  no source for the specified basenamne

         Candidate := No_Proxy;

      elsif Set.Length = 1 then
         --  Only one source in the set: just use it

         Candidate := Source_Proxy_Sets.Element (Set.First);

      else
         --  project extension case, or the same basename is found in
         --  different source dirs

         for C of Set loop
            if Candidate = No_Proxy then
               --  First value, consider it as a candidate

               Candidate := C;

               if Candidate.View = Data.View then
                  --  Own source, get Src_Info cursor
                  C_Info := Data.Src_Infos.Find (Candidate.Path_Name);
               end if;

            elsif C.View = Data.View
              and then Candidate.View /= Data.View
            then
               --  Candidate was inherited: own source overrides it
               Candidate := C;
               C_Info    := Data.Src_Infos.Find (Candidate.Path_Name);

            elsif Candidate.View = Data.View
              and then C.View /= Data.View
            then
               --  Candidate is owned by current view, so ignore inherited
               --  source
               null;

            elsif C.View = Data.View then
               --  Both candidates are owned by the view, check
               --  Source_Reference: the declaration order of the source
               --  directory in the Source_Dirs attribute gives the
               --  visibility priority

               C_Info2 := Data.Src_Infos.Find (C.Path_Name);

               SR1 := Src_Info_Maps.Element (C_Info).Source_Reference;
               SR2 := Src_Info_Maps.Element (C_Info2).Source_Reference;

               if SR1 = SR2 then
                  Clashes.Include (SR1);

               elsif SR2 < SR1 then
                  --  Source_Ref of C2 is declared before the one of
                  --  Candidate, so takes precedence.

                  Candidate := C;
                  C_Info := C_Info2;
                  Clashes.Clear;
               end if;

            else
               --  Remaining case: inheritance shows two candidate sources

               Messages.Append
                 (Message.Create
                    (Message.Error,
                     '"' & String (Basename) & '"' &
                       " is found in several extended projects",
                     Source_Reference.Create
                       (Data.View.Path_Name.Value, 0, 0)));
               declare
                  P1, P2 : GPR2.Path_Name.Object;
                  V1, V2 : GPR2.Path_Name.Object;
               begin
                  --  Use alphabetical sort to have consistent output.
                  --  This is in particular important when comparing test
                  --  output.

                  if C.Path_Name < Candidate.Path_Name then
                     P1 := C.Path_Name;
                     V1 := C.View.Path_Name;
                     P2 := Candidate.Path_Name;
                     V2 := Candidate.View.Path_Name;
                  else
                     P1 := Candidate.Path_Name;
                     V1 := Candidate.View.Path_Name;
                     P2 := C.Path_Name;
                     V2 := C.View.Path_Name;
                  end if;

                  Messages.Append
                    (Message.Create
                       (Message.Error,
                        P1.Value,
                        Source_Reference.Create (V1.Value, 0, 0),
                        Indent => 1));
                  Messages.Append
                    (Message.Create
                       (Message.Error,
                        P2.Value,
                        Source_Reference.Create (V2.Value, 0, 0),
                        Indent => 1));
               end;

               Candidate := No_Proxy;

               exit;

            end if;
         end loop;

         if not Clashes.Is_Empty then
            for SR of Clashes loop
               Messages.Append
                 (Message.Create
                    (Message.Error,
                     '"' & String (Basename) & '"' &
                       " is found multiple times for the same source" &
                       " directory",
                     SR));
            end loop;

            Candidate := No_Proxy;
         end if;
      end if;

      if Basename_Source_Maps.Has_Element (C_Src) then
         Current := Basename_Source_Maps.Element (C_Src);
      else
         Current := No_Proxy;
      end if;

      if Current /= Candidate then
         --  Remove current visible source
         if Current /= No_Proxy then
            Propagate_Visible_Source_Removal (Current);

            if Candidate = No_Proxy then
               Data.Sources.Delete (C_Src);
            end if;
         end if;

         if Candidate /= No_Proxy then
            if Current = No_Proxy then
               Data.Sources.Insert (Basename, Candidate);
            else
               Data.Sources.Replace_Element (C_Src, Candidate);
            end if;

            Propagate_Visible_Source_Added (Candidate);
         end if;
      end if;
   end Resolve_Visibility;

   package body Update_Sources_List is separate;

end GPR2.Build.View_Tables;
