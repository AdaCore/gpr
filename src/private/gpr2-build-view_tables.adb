--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Traces;

with GPR2.Build.Tree_Db;
with GPR2.Build.Unit_Info;
with GPR2.Message;
with GPR2.Source_Reference;

package body GPR2.Build.View_Tables is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.SOURCES_DB", GNATCOLL.Traces.Off);

   procedure Add_Unit_Part
     (NS_Db    : View_Data_Ref;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : View_Data_Ref;
      Path     : GPR2.Path_Name.Object;
      Src      : Source_Proxy;
      Index    : Unit_Index;
      Messages : in out GPR2.Log.Object)
     with Pre => NS_Db.Is_Root
                   and then (Kind = S_Separate) = (Sep_Name'Length > 0);

   procedure Remove_Unit_Part
     (NS_Db    : View_Data_Ref;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : View_Data_Ref;
      Path     : Path_Name.Object;
      Index    : Unit_Index)
     with Pre => NS_Db.Is_Root
                   and then (Kind = S_Separate) = (Sep_Name'Length > 0);

   procedure Remove_Source
     (Data       : View_Data_Ref;
      View_Owner : GPR2.Project.View.Object;
      Path       : Filename_Type);

   procedure Add_Unit_Ownership
     (To   : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref);

   procedure Remove_Unit_Ownership
     (From : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref);

   package Update_Sources_List is
      procedure Process
        (Data           : View_Data_Ref;
         Messages       : in out GPR2.Log.Object;
         Matching_Units : Boolean := True);
      --  Load and validate the view's own and inherited sources
   end Update_Sources_List;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref
     is (Get_Ref (Inst));

   procedure Check_Separate
     (Root_Db : View_Tables.View_Data_Ref;
      File    : in out Source_Base.Object)
     with Pre => Root_Db.Is_Root
                   and then File.Has_Single_Unit
                   and then File.Unit.Kind = S_Separate;

   function Is_Source_Visible
     (Data          : View_Data_Ref;
      Proxy         : Source_Proxy;
      Is_Compilable : Boolean) return Boolean;

   function Source
     (Data : View_Data_Ref;
      Pos  : Basename_Source_Maps.Cursor) return Build.Source.Object
     with Inline;

   ------------------------
   -- Add_Unit_Ownership --
   ------------------------

   procedure Add_Unit_Ownership
     (To   : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref)
   is
      C       : Unit_Maps.Cursor;
      New_Set : Project.View.Set.Object;

   begin
      if Traces.Is_Active then
         Traces.Trace
           ("Add_Unit_Ownership:" & ASCII.LF &
              " - To: " & To.View.Path_Name.String_Value & ASCII.LF &
              " - CU: " & String (CU) & ASCII.LF &
              " - Root: " & Root.View.Path_Name.String_Value);
      end if;
      C := To.Own_CUs.Find (CU);

      if Unit_Maps.Has_Element (C) then
         To.Own_CUs.Reference (C).Include (Root.View);
      else
         New_Set.Insert (Root.View);
         To.Own_CUs.Include (CU, New_Set);
      end if;

      if not To.View.Is_Extended then
         for NS of To.View.Aggregated_By loop
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
     (NS_Db    : View_Data_Ref;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : View_Data_Ref;
      Path     : GPR2.Path_Name.Object;
      Src      : Source_Proxy;
      Index    : Unit_Index;
      Messages : in out GPR2.Log.Object)
   is
      Cursor       : Compilation_Unit_Maps.Cursor;
      Success      : Boolean := True;
      Remove_Src   : Boolean := False;
      Remove_Other : Boolean := False;
      Replace      : Boolean := False;
      Error_Case   : Boolean := False;
      Other        : Compilation_Unit.Unit_Location;
      Old_Owner    : GPR2.Project.View.Object;

   begin
      if Traces.Is_Active then
         Traces.Trace
           ("Add unit part:" & ASCII.LF &
              "CU: " & String (CU) & ASCII.LF &
              "Kind: " & Kind'Image & ASCII.LF &
              (if Kind = S_Separate
               then "Separate: " & String (Sep_Name) & ASCII.LF
               else "") &
              "View: " & String (View_Db.View.Name) & ASCII.LF &
              "Path: " & Path.String_Value);
      end if;

      Cursor := NS_Db.CUs.Find (CU);

      if not Compilation_Unit_Maps.Has_Element (Cursor) then
         if Traces.Is_Active then
            pragma Annotate (Xcov, Exempt_On, "Logging code");
            Traces.Trace ("new compilation unit, create it");
            pragma Annotate (Xcov, Exempt_Off);
         end if;

         --  Compilation unit name is not known: new unit, so simply add it
         declare
            CU_Instance : Compilation_Unit.Object :=
                            Compilation_Unit.Create (CU, NS_Db.View);
         begin
            CU_Instance.Add
              (Kind, View_Db.View, Path, Index, Sep_Name, Success);
            NS_Db.CUs.Insert (CU, CU_Instance);

            if CU_Instance.Owning_View.Is_Defined then
               Add_Unit_Ownership (View_Db, CU, NS_Db);
            end if;
         end;

      else
         declare
            CU_Instance : constant Compilation_Unit_Maps.Reference_Type :=
                            NS_Db.CUs.Reference (Cursor);

         begin
            Old_Owner := CU_Instance.Owning_View;

            CU_Instance.Add
              (Kind, View_Db.View, Path, Index, Sep_Name, Success);

            if not Success and then not View_Db.View.Is_Runtime then
               Traces.Trace ("!! clashing unit part");

               --  Check for duplicated units

               --  Note: the runtime *has* duplicated unit to support
               --  system.memory, now our generated project to add it to the
               --  tree is a bit simple minded, so just kill the warning for
               --  the runtime view.

               Other := CU_Instance.Get (Kind, Sep_Name);

               if Other.View.Is_Runtime then
                  --  Special case for the runtime, where we allow some
                  --  runtime units to be overriden by project sources.
                  Traces.Trace ("... replacing a runtime unit");

                  CU_Instance.Remove
                    (Kind, Other.View, Other.Source, Other.Index, Sep_Name);
                  CU_Instance.Add
                    (Kind, View_Db.View, Path, Index, Sep_Name, Success, True);

                  NS_Db.Runtime_Override_Files.Include
                    (Filename_Type (Other.Source.Simple_Name));
                  if CU_Instance.Has_Part (S_Spec) then
                     NS_Db.Runtime_Override_Files.Include
                       (Filename_Type (CU_Instance.Spec.Source.Simple_Name));
                  end if;
                  if CU_Instance.Has_Part (S_Body) then
                     NS_Db.Runtime_Override_Files.Include
                       (Filename_Type
                          (CU_Instance.Main_Body.Source.Simple_Name));
                  end if;

               elsif Other.Source.Value = Path.Value then
                  --  Same source found by multiple projects
                  Messages.Append
                    (Message.Create
                       (Level   => Message.Critical_Error,
                        Message => "source file """ &
                          String (Path.Simple_Name) &
                          """ already part of project " &
                          String (CU_Instance.Get (Kind, Sep_Name).View.Name),
                        Sloc    => Source_Reference.Create
                          (View_Db.View.Path_Name.Value, 0, 0)));
                  Success := False;

               else
                  --  Clashing unit case: we need to check if project extension
                  --  is involved to hide the extended unit.

                  declare
                     Other_Loc : constant Source_Proxy :=
                                   Get_Data (NS_Db.Tree_Db, Other.View).Sources
                                   (Other.Source.Value);
                     use type GPR2.Project.View.Object;

                  begin
                     if View_Db.View = Other.View then
                        --  Both sources are reported for the same view, let's
                        --  see if one is inherited. This may happen if the
                        --  extending view has a naming exception for the unit.

                        if not Src.Inh_From.Is_Defined
                          and then not Other_Loc.Inh_From.Is_Defined
                        then
                           if View_Db.View.Visible_Source
                             (Path).From_Src_Subdirs
                             xor View_Db.View.Visible_Source
                             (Other.Source).From_Src_Subdirs
                           then
                              if View_Db.View.Visible_Source
                                (Path).From_Src_Subdirs
                              then
                                 --  The new source comes from --src-subdirs we
                                 --  need to replace the one which doesn't come
                                 --  from the --src-subdirs.

                                 Replace := True;
                              else
                                 --  The new source isn't from --source-subdirs
                                 --  we just need to remove the new source
                                 --  since the --src-subdirs one has priority.

                                 Remove_Src := True;
                              end if;

                           else
                              --  both sources are directly defined for the
                              --  view, so we raise an error about duplicated
                              --  units.

                              Traces.Trace
                                ("... error: both units are defined in the" &
                                   " current view");

                              Error_Case := True;
                           end if;

                        elsif Src.Inh_From.Is_Defined
                          and then not Other_Loc.Inh_From.Is_Defined
                        then
                           --  new source is inherited. previously analyzed
                           --  one is not, don't do anything apart of cleaning
                           --  up Src.

                           Traces.Trace
                             ("... new source is inherited, remove it");

                           Remove_Src := True;

                        elsif not Src.Inh_From.Is_Defined
                          and then Other_Loc.Inh_From.Is_Defined
                        then
                           --  previously analyzed source was inherited while
                           --  the new one is not: replace by the new source
                           --  in the unit.

                           Traces.Trace
                             ("... new source overloads old unit, replace it");

                           Replace := True;

                        elsif Src.View.Is_Extending (Other_Loc.View) then
                           --  the defining view for the new source is
                           --  extending the defining view for the old source:
                           --  replace the old one.

                           Traces.Trace
                             ("... new source overloads old unit, replace it");

                           Replace := True;

                        elsif Other_Loc.View.Is_Extending (Src.View) then
                           --  new source was overloaded by the old source, so
                           --  just remove its unit

                           Traces.Trace
                             ("... new source is inherited, remove it");

                           Remove_Src := True;

                        else
                           --  Both have been inherited but from different
                           --  unrelated views: there's a clash.

                           Traces.Trace
                             ("... units come from unrelated inherited" &
                                " views, error");

                           Error_Case := True;
                        end if;

                     elsif View_Db.View.Is_Extending (Other.View) then
                        --  Replace the unit in the compilation unit

                        Traces.Trace
                          ("... new source overloads old unit, " &
                             "replace it");

                        Replace := True;

                     elsif Other.View.Is_Extending (View_Db.View) then
                        Remove_Src := True;

                     else
                        Traces.Trace
                          ("??? we should never end up here !");

                        Error_Case := True;
                     end if;

                     if Error_Case then
                        --  Two sources in the closure declare the same unit
                        --  part, so issue a warning.

                        Messages.Append
                          (Message.Create
                             (Level   => Message.Warning,
                              Message => "duplicated " &
                                Image (Kind) & " for unit """ & String (CU) &
                                """ in " & Other.Source.String_Value &
                                " and " & Path.String_Value,
                              Sloc    =>
                                Source_Reference.Create
                                  (NS_Db.View.Path_Name.Value, 0, 0)));

                        --  Ignore the clashing sources
                        Remove_Src   := True;
                        Remove_Other := True;
                        Replace      := False;
                     else
                        Success := True;
                     end if;

                     if Replace then
                        CU_Instance.Remove
                          (Kind, Other.View, Other.Source, Other.Index,
                           Sep_Name);

                        CU_Instance.Add
                          (Kind, View_Db.View, Path, Index, Sep_Name, Success);

                        --  If the source is multi-unit, remove this unit from
                        --  it, else completely remove the source.

                        if Other.Index /= No_Index then
                           declare
                              use Src_Info_Maps;

                              Owning_Db : constant View_Data_Ref :=
                                            Get_Data (NS_Db.Tree_Db,
                                                      Other_Loc.View);
                              Other_Src : constant Reference_Type :=
                                            Owning_Db.Src_Infos.Reference
                                              (Other.Source.Value);
                           begin
                              Other_Src.Remove_Unit (Other.Index);

                              Remove_Other := Other_Src.Units.Is_Empty;
                           end;

                        else
                           Remove_Other := True;
                        end if;

                     elsif Remove_Src then
                        Remove_Src := False;

                        if Index /= No_Index then
                           declare
                              use Src_Info_Maps;

                              Owning_Db : constant View_Data_Ref :=
                                            Get_Data (NS_Db.Tree_Db, Src.View);
                              Ref       : constant Reference_Type :=
                                            Owning_Db.Src_Infos.Reference
                                              (Path.Value);
                           begin
                              Ref.Remove_Unit (Index);
                              Remove_Src := Ref.Units.Is_Empty;
                           end;

                        else
                           Remove_Src := True;
                        end if;
                     end if;
                  end;
               end if;
            end if;
         end;

         --  Note: we need to make sure we don't hold a reference on the
         --  compilation unit before actually removing sources, since this
         --  operation may need to modify the CU map and thus lead to a program
         --  error exception.

         if Remove_Src then
            if Traces.Is_Active then
               Traces.Trace
                 ("ignoring source '" &
                    String (Src.Path_Name));
            end if;

            pragma Annotate
              (Xcov, Exempt_On,
               "This looks like the case Src.Inh_From.Is_Defined may not " &
                 "be exercised currently considering the order used to load " &
                 "sources. However if the order change or the above analysis" &
                 "is incorrect, it is better to keep it as-is.");

            if Src.Inh_From.Is_Defined then
               --  Remove the source from the project that loaded it rather
               --  than just the current view. Allows fixing case where a
               --  source is overriden by its Ada unit but has a different
               --  basename than the overloading source.

               Remove_Source
                 (Get_Data (NS_Db.Tree_Db, Src.View),
                  Src.View,
                  Src.Path_Name);
               pragma Annotate (Xcov, Exempt_Off);
            else
               Remove_Source
                 (View_Db,
                  Src.View,
                  Src.Path_Name);
            end if;
         end if;

         if Remove_Other then
            declare
               Other_Db : constant View_Data_Ref :=
                 Get_Data (NS_Db.Tree_Db, Other.View);
            begin
               --  The source may already have been removed
               if Other_Db.Sources.Contains (Other.Source.Value)
               then
                  declare
                     Other_Loc : constant Source_Proxy :=
                       Other_Db.Sources (Other.Source.Value);
                  begin

                     if Traces.Is_Active then
                        Traces.Trace
                          ("ignoring source '" &
                             String (Other_Loc.Path_Name) &
                             "' as it is not used anymore");
                     end if;

                     if Other_Loc.Inh_From.Is_Defined then
                        --  Remove it from the defining view
                        Remove_Source
                          (Get_Data (NS_Db.Tree_Db, Other_Loc.View),
                           Other_Loc.View,
                           Other_Loc.Path_Name);
                     else
                        Remove_Source
                          (View_Db,
                           Other_Loc.View,
                           Other_Loc.Path_Name);
                     end if;
                  end;
               elsif Traces.Is_Active then
                  Traces.Trace
                    ("source '" & String (Other.Source.Value) &
                       "' already removed");
               end if;
            end;
         end if;

         if Success then
            declare
               CU_Instance : constant Compilation_Unit.Object :=
                               Compilation_Unit_Maps.Element (Cursor);
               use type GPR2.Project.View.Object;
            begin
               if Old_Owner /= CU_Instance.Owning_View then
                  --  Owning view changed, let's apply this change
                  if Old_Owner.Is_Defined then
                     if Traces.Is_Active then
                        pragma Annotate (Xcov, Exempt_On, "Logging code");
                        Traces.Trace ("changing unit ownership from " &
                                        String (Old_Owner.Name) & " to " &
                                        String (View_Db.View.Name));
                        pragma Annotate (Xcov, Exempt_Off);
                     end if;

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
            end;
         end if;
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
     (Root_Db : View_Tables.View_Data_Ref;
      File    : in out Source_Base.Object)
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
            U            : constant Unit_Info.Object := File.Unit;
            New_Name     : constant Name_Type := Name_Maps.Element (C);
            --  New compilation unit name
            P_Simple     : constant Name_Type := U.Name
                                        (New_Name'Length + 2 .. U.Name'Length);
            --  Simple unit name of the separate parent
            New_Sep_Name : constant Name_Type :=
                             GPR2."&" (GPR2."&" (P_Simple, "."),
                                       U.Separate_Name);
         begin
            File.Update_Unit
              (Unit_Info.Create
                 (Unit_Name      => New_Name,
                  Index          => U.Index,
                  Kind           => U.Kind,
                  Separate_Name  => New_Sep_Name));
         end;
      end loop;
   end Check_Separate;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Db : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref is
   begin
      return -Db.View_Database (View);
   end Get_Data;

   -----------------------
   -- Is_Source_Visible --
   -----------------------

   function Is_Source_Visible
     (Data          : View_Data_Ref;
      Proxy         : Source_Proxy;
      Is_Compilable : Boolean) return Boolean
   is
   begin
      if not Is_Compilable then
         return True;
      end if;

      declare
         C : constant Basename_Source_Maps.Cursor :=
               Data.Basenames.Find (Path_Name.Simple_Name (Proxy.Path_Name));
      begin
         return Basename_Source_Maps.Has_Element (C)
           and then Basename_Source_Maps.Element (C) = Proxy;
      end;
   end Is_Source_Visible;

   ------------------
   -- Load_Sources --
   ------------------

   procedure Load_Sources
     (Data           : View_Data_Ref;
      Messages       : in out GPR2.Log.Object;
      Matching_Units : Boolean := True) is
   begin
      if Data.View.Kind in With_Source_Dirs_Kind then
         Update_Sources_List.Process (Data, Messages, Matching_Units);
      end if;
   end Load_Sources;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Data       : View_Data_Ref;
      View_Owner : GPR2.Project.View.Object;
      Path       : Filename_Type)
   is
      Current_View : Project.View.Object := Data.View;
      Owner   : constant View_Data_Ref :=
                  Get_Data (Data.Tree_Db, View_Owner);
      Info    : constant Source_Base.Object := Owner.Src_Infos.Element (Path);
      Name    : constant Simple_Name := Path_Name.Simple_Name (Path);
   begin
      Data.Tree_Db.Invalidate_Source_Caches;
      loop
         declare
            Current : constant View_Data_Ref :=
                        Get_Data (Data.Tree_Db, Current_View);
         begin
            exit when not Current.Sources.Contains (Path);
            Current.Sources.Delete (Path);
            declare
               Count : constant Sources_By_Langs_Maps.Reference_Type :=
                         Current.Langs_Usage.Reference (Info.Language);
            begin
               Count.Element.all := Count - 1;
            end;

            exit when not Current.Basenames.Contains (Name)
              or else Current.Basenames.Element (Name).Path_Name /= Path;
            Current.Basenames.Delete (Name);

            if not Current.View.Is_Extended then
               if Info.Has_Units then
                  for U of Info.Units loop
                     for Root of View_Owner.Namespace_Roots loop
                        if Root.Kind in With_View_Db
                          and then U.Kind /= S_No_Body
                        then
                           Remove_Unit_Part
                             (Get_Data (Data.Tree_Db, Root),
                              U.Name, U.Kind, U.Separate_Name, Current,
                              Info.Path_Name, U.Index);
                        end if;
                     end loop;
                  end loop;
               end if;
               exit;
            end if;

            Current_View := Current.View.Extending;
         end;
      end loop;
   end Remove_Source;

   ---------------------------
   -- Remove_Unit_Ownership --
   ---------------------------

   procedure Remove_Unit_Ownership
     (From : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref)
   is
      C : Unit_Maps.Cursor := From.Own_CUs.Find (CU);
   begin
      From.Own_CUs.Reference (C).Delete (Root.View);

      if Unit_Maps.Element (C).Is_Empty then
         From.Own_CUs.Delete (C);
      end if;

      if not From.View.Is_Extended then
         for NS of From.View.Aggregated_By loop
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
     (NS_Db    : View_Data_Ref;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : View_Data_Ref;
      Path     : Path_Name.Object;
      Index    : Unit_Index)
   is
      Cursor    : Compilation_Unit_Maps.Cursor := NS_Db.CUs.Find (CU);
      Old_Owner : Project.View.Object;
   begin
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
            C         : Name_Maps.Cursor :=
                          NS_Db.Separates.Find (Full_Name);
         begin
            if Name_Maps.Has_Element (C) then
               NS_Db.Separates.Delete (C);
            end if;
         end;
      end if;
   end Remove_Unit_Part;

   -------------------
   -- Reset_Sources --
   -------------------

   procedure Reset_Sources (Data : View_Data_Ref) is
   begin
      Data.Visible_Src_Cache.Clear;
      Data.Runtime_Override_Units.Clear;
      Data.Interface_Closure.Clear;
      Data.Interface_Closure_Computed := False;
      Data.Sources.Clear;
      Data.Basenames.Clear;
      Data.Src_Infos.Clear;
      Data.Langs_Usage.Clear;
      Data.Actually_Excluded.Clear;
      Data.Own_CUs.Clear;
      if Data.Is_Root then
         Data.CUs.Clear;
         Data.Runtime_Override_Files.Clear;
         Data.Separates.Clear;
      end if;
   end Reset_Sources;

   -----------------------
   -- Resolve_Separates --
   -----------------------

   procedure Resolve_Separates
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object)
   is
      Sources : Basename_Source_Maps.Map;
   begin
      --  Disambiguate unit kind for Ada bodies

      if Data.Tree_Db.Source_Option >= Sources_Units
        and then not Data.View.Is_Extended
        and then Data.View.Kind in With_Object_Dir_Kind
      then
         --  Check separates of separates

         --  Conflict resolution may remove a visible source
         Sources := Data.Basenames;

         for Proxy of Sources loop
            declare
               Db    : constant View_Data_Ref :=
                         Get_Data (Data.Tree_Db, Proxy.View);
               S_Ref : constant Src_Info_Maps.Reference_Type :=
                         Db.Src_Infos.Reference (Proxy.Path_Name);
            begin
               if S_Ref.Has_Units
                 and then not S_Ref.Has_Index
                 and then S_Ref.Unit.Kind = S_Separate
               then
                  for Root of Data.View.Namespace_Roots loop
                     declare
                        Root_Db : constant View_Data_Ref :=
                                    Get_Data (Data.Tree_Db, Root);
                        Old     : constant Unit_Info.Object := S_Ref.Unit;
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
                              Proxy,
                              S_Ref.Unit.Index,
                              Messages);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end if;
   end Resolve_Separates;

   ------------
   -- Source --
   ------------

   function Source
     (Data : View_Data_Ref;
      Pos  : Basename_Source_Maps.Cursor) return Build.Source.Object
   is
      use type GPR2.Project.View.Object;

      Proxy    : constant Source_Proxy := Basename_Source_Maps.Element (Pos);
      Base_Src : constant Src_Info_Maps.Constant_Reference_Type :=
                   (if Proxy.View = Data.View
                    then Data.Src_Infos.Constant_Reference (Proxy.Path_Name)
                    else Get_Data (Data.Tree_Db, Proxy.View).Src_Infos.
                      Constant_Reference (Proxy.Path_Name));
   begin
      return Build.Source.Create
        (Base_Source    => Base_Src.Element.all,
         Defining_View  => Proxy.View,
         Owning_View    => Data.View,
         Inherited_From => Proxy.Inh_From,
         Is_Visible     => True);
   end Source;

   ------------
   -- Source --
   ------------

   function Source
     (Data  : View_Data_Ref;
      Proxy : Source_Proxy) return Build.Source.Object
   is
      use type GPR2.Project.View.Object;

      Base_Src      : constant Src_Info_Maps.Constant_Reference_Type :=
                        (if Proxy.View = Data.View
                         then Data.Src_Infos.Constant_Reference
                           (Proxy.Path_Name)
                         else Get_Data (Data.Tree_Db, Proxy.View).Src_Infos.
                           Constant_Reference (Proxy.Path_Name));
      Is_Compilable : constant Boolean :=
                        Data.View.Is_Compilable (Base_Src.Language);
      Is_Visible    : constant Boolean :=
                        Is_Source_Visible (Data, Proxy, Is_Compilable);

   begin
      return Build.Source.Create
        (Base_Source    => Base_Src.Element.all,
         Defining_View  => Proxy.View,
         Owning_View    => Data.View,
         Inherited_From => Proxy.Inh_From,
         Is_Visible     => Is_Visible);
   end Source;

   ------------
   -- Source --
   ------------

   function Source
     (Data     : View_Data_Ref;
      Basename : Simple_Name) return Build.Source.Object
   is
      C   : constant Basename_Source_Maps.Cursor :=
              Data.Basenames.Find (Basename);
   begin
      if not Basename_Source_Maps.Has_Element (C) then
         return Build.Source.Undefined;

      else
         return Source (Data, C);
      end if;
   end Source;

   -------------------
   -- Source_Status --
   -------------------

   procedure Source_Status
     (Data          : View_Data_Ref;
      Proxy         : Source_Proxy;
      Is_Visible    : out Boolean;
      Is_Compilable : out Boolean)
   is
      use type GPR2.Project.View.Object;

      function Language (Owner : View_Data_Ref) return Language_Id;

      function Language (Owner : View_Data_Ref) return Language_Id is
         Ref : constant Src_Info_Maps.Constant_Reference_Type :=
                 Owner.Src_Infos.Constant_Reference (Proxy.Path_Name);
      begin
         return Ref.Language;
      end Language;
   begin
      Is_Compilable := Data.View.Is_Compilable
        ((if Proxy.View = Data.View then Language (Data)
          else Language (Get_Data (Data.Tree_Db, Proxy.View))));
      Is_Visible := Is_Source_Visible (Data, Proxy, Is_Compilable);
   end Source_Status;

   -----------------------
   -- Unit_Dependencies --
   -----------------------

   function Unit_Dependencies
     (Data     : View_Data_Ref;
      Basename : Simple_Name;
      Index    : Unit_Index) return GPR2.Containers.Name_Set
   is
      C : constant Basename_Source_Maps.Cursor :=
            Data.Basenames.Find (Basename);

      use type GPR2.Project.View.Object;

   begin
      if not Basename_Source_Maps.Has_Element (C) then
         return GPR2.Containers.Empty_Name_Set;
      end if;

      declare
         Proxy : constant Source_Proxy := Basename_Source_Maps.Element (C);

         function Deps (Owner : View_Data_Ref) return GPR2.Containers.Name_Set;

         ----------
         -- Deps --
         ----------

         function Deps (Owner : View_Data_Ref) return GPR2.Containers.Name_Set
         is
            Src : Src_Info_Maps.Constant_Reference_Type renames
                    Owner.Src_Infos.Constant_Reference (Proxy.Path_Name);
         begin
            if not Src.Has_Units or else not Src.Has_Unit_At (Index) then
               return GPR2.Containers.Empty_Name_Set;
            end if;

            return Src.Unit_Dependencies (Index);
         end Deps;

      begin
         if Proxy.View = Data.View then
            return Deps (Data);
         else
            return Deps (Get_Data (Data.Tree_Db, Proxy.View));
         end if;
      end;
   end Unit_Dependencies;

   ------------------------------
   -- Update_Runtime_Overrides --
   ------------------------------

   procedure Update_Runtime_Overrides (Data : View_Data_Ref) is
   begin
      Data.Runtime_Override_Units.Clear;

      if Data.Tree_Db.Source_Option < Sources_Units
        or else Data.View.Namespace_Roots.Is_Empty
      then
         return;
      end if;

      declare
         Root : constant View_Data_Ref :=
                  Get_Data
                    (Data.Tree_Db, Data.View.Namespace_Roots.First_Element);
         Ambiguous : Boolean;
      begin
         for File of Root.Runtime_Override_Files loop
            declare
               Src : constant Build.Source.Object :=
                       Visible_Source (Data, File, Ambiguous);
               Units : Containers.Name_Set;
            begin
               if Src.Is_Defined and then Src.Has_Units then
                  for U of Src.Units loop
                     if U.Kind in S_Spec | S_Body
                       and then U.Name'Length > 0
                       and then Root.CUs.Contains (Name_Type (U.Name))
                       and then Root.CUs.Constant_Reference
                         (Name_Type (U.Name)).Overridden_From_Runtime
                     then
                        Units.Include (Name_Type (U.Name));
                     end if;
                  end loop;
               end if;

               if not Units.Is_Empty then
                  Data.Runtime_Override_Units.Insert (File, Units);
               end if;
            end;
         end loop;
      end;
   end Update_Runtime_Overrides;

   package body Update_Sources_List is separate;

   --------------------
   -- Visible_Source --
   --------------------

   function Visible_Source
     (Data      : View_Data_Ref;
      Basename  : Simple_Name;
      Ambiguous : out Boolean) return Build.Source.Object
   is
      Generation : constant Natural := Data.Tree_Db.Source_Generation;
      C          : Basename_Source_Maps.Cursor;
      Cached     : Visible_Source_Maps.Cursor;
      Candidate  : Build.Source.Object;

   begin
      --  First set the out value

      Ambiguous := False;

      --  Resolving a name walks the whole closure, and the same names are
      --  looked up repeatedly, so memoise until the sources change.

      if Data.Visible_Src_Cache_Gen /= Generation then
         Data.Visible_Src_Cache.Clear;
         Data.Visible_Src_Cache_Gen := Generation;
      else
         Cached := Data.Visible_Src_Cache.Find (Basename);

         if Visible_Source_Maps.Has_Element (Cached) then
            declare
               Ref : constant Visible_Source_Maps.Constant_Reference_Type :=
                       Data.Visible_Src_Cache.Constant_Reference (Cached);
            begin
               Ambiguous := Ref.Ambiguous;
               return Ref.Src;
            end;
         end if;
      end if;

      C := Data.Basenames.Find (Basename);

      --  The view may own a source with this basename itself. Don't return
      --  it right away though: it still needs to be checked against the
      --  view's closure (withed or limited withed views) below, since one
      --  of those may also have a homonym, which would make the source
      --  ambiguous.

      if Basename_Source_Maps.Has_Element (C) then
         Candidate := Source (Data, C);
      end if;

      for V of Data.Visible_Source_Closure loop
         if V.Kind in With_View_Db then
            declare
               V_Data : View_Data_Ref renames Get_Data (Data.Tree_Db, V);
            begin
               C := V_Data.Basenames.Find (Basename);

               if Basename_Source_Maps.Has_Element (C) then
                  if Candidate.Is_Defined then
                     --  Take care of the special case of runtime sources that
                     --  are allowed to be overriden by a project. In this case
                     --  the project source overrides the runtime source.

                     if Candidate.Owning_View.Is_Runtime then
                        Candidate := Source (V_Data, C);
                     elsif V.Is_Runtime then
                        null;
                     else
                        Ambiguous := True;
                        Data.Visible_Src_Cache.Include
                          (Basename, (Src => Candidate, Ambiguous => True));

                        return Candidate;
                     end if;

                  else
                     Candidate := Source (V_Data, C);
                  end if;
               end if;
            end;
         end if;
      end loop;

      Data.Visible_Src_Cache.Include
        (Basename, (Src => Candidate, Ambiguous => False));

      return Candidate;
   end Visible_Source;

   function Visible_Source
     (Data : View_Data_Ref;
      Path : Path_Name.Object) return Build.Source.Object
   is
      C      : Filename_Source_Maps.Cursor := Data.Sources.Find (Path.Value);
      Result : Build.Source.Object;
   begin
      if Filename_Source_Maps.Has_Element (C) then
         return Source (Data, Filename_Source_Maps.Element (C));
      end if;

      for V of Data.Visible_Source_Closure loop
         if V.Kind in With_View_Db then
            declare
               V_Data    : View_Data_Ref renames Get_Data (Data.Tree_Db, V);
            begin
               C := V_Data.Sources.Find (Path.Value);

               if Filename_Source_Maps.Has_Element (C) then
                  --  Ensure it's locally visible
                  Result := Source (V_Data, Filename_Source_Maps.Element (C));

                  if Result.Is_Visible then
                     return Result;
                  end if;
               end if;
            end;
         end if;
      end loop;

      return Build.Source.Undefined;
   end Visible_Source;

end GPR2.Build.View_Tables;
