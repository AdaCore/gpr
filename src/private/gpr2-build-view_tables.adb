--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with GNAT.OS_Lib;

with GNATCOLL.Traces;

with GPR2.Build.Tree_Db;
with GPR2.Build.Unit_Info;
with GPR2.Project.Attribute;
with GPR2.Message;
with GPR2.Source_Reference.Value;
with GPR2.Project.Registry.Attribute;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

package body GPR2.Build.View_Tables is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.SOURCES_DB", GNATCOLL.Traces.Off);

   package Natural_Sets is new Ada.Containers.Ordered_Sets (Natural);

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

   procedure Resolve_Visibility
     (Data     : View_Data_Ref;
      Cursor   : in out Basename_Source_List_Maps.Cursor;
      Messages : in out GPR2.Log.Object);

   procedure Add_Unit_Ownership
     (To   : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref);

   procedure Remove_Unit_Ownership
     (From : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref);

   procedure Read_Source_List
     (View      : Project.View.Object;
      Attr      : Project.Attribute.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object);
   --  Read from file defined in project attribute Attr_Name and insert each
   --  line into Set

   procedure Include_Simple_Filename
     (Set      : in out Source_Set.Set;
      Value    : Value_Type;
      Sloc     : Source_Reference.Object'Class;
      Messages : in out GPR2.Log.Object);

   package Update_Sources_List is
      procedure Process
        (Data          : View_Data_Ref;
         Stop_On_Error : Boolean;
         Messages      : in out GPR2.Log.Object);
      --  Update the list of sources
   end Update_Sources_List;

   function "-" (Inst : Build.View_Db.Object) return View_Data_Ref
     is (Get_Ref (Inst));

   procedure Check_Separate
     (Root_Db : View_Tables.View_Data_Ref;
      File    : in out Source_Base.Object)
     with Pre => Root_Db.Is_Root
                   and then File.Has_Single_Unit
                   and then File.Unit.Kind = S_Separate;

   function Source
     (Data : View_Data_Ref;
      Pos  : Basename_Source_Maps.Cursor) return Build.Source.Object
     with Inline;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Data               : View_Data_Ref;
      View_Owner         : GPR2.Project.View.Object;
      Path               : Filename_Type;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
   is
      C_Overload : Basename_Source_List_Maps.Cursor;
      Done       : Boolean;
      Proxy      : constant Source_Proxy :=
                     (Path_Len  => Path'Length,
                      View      => View_Owner,
                      Path_Name => Path,
                      Inh_From  => Extended_View);
      Owner_Db   : constant View_Data_Ref :=
                     Get_Data (Data.Tree_Db, View_Owner);
      Src_Info   : constant Src_Info_Maps.Constant_Reference_Type :=
                     Owner_Db.Src_Infos.Constant_Reference (Path);
      C_Lang     : Sources_By_Langs_Maps.Cursor;

   begin
      Data.Sources.Include (Path, Proxy);

      Data.Langs_Usage.Insert (Src_Info.Language, 1, C_Lang, Done);

      if not Done then
         declare
            Val : constant Sources_By_Langs_Maps.Reference_Type :=
                    Data.Langs_Usage.Reference (C_Lang);
         begin
            Val.Element.all := Val + 1;
         end;
      end if;

      Data.Overloaded_Srcs.Insert
        (Path_Name.Simple_Name (Path),
         Source_Proxy_Sets.Empty_Set,
         C_Overload,
         Done);
      Data.Overloaded_Srcs.Reference (C_Overload).Include (Proxy);

      if Resolve_Visibility then
         View_Tables.Resolve_Visibility (Data, C_Overload, Messages);
      end if;
   end Add_Source;

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
      Traces.Trace ("Add_Unit_Ownership:" & ASCII.LF &
                      " - To: " & To.View.Path_Name.String_Value & ASCII.LF &
                      " - CU: " & String (CU) & ASCII.LF &
                      " - Root: " & Root.View.Path_Name.String_Value);
      C := To.Own_CUs.Find (CU);

      if Unit_Maps.Has_Element (C) then
         To.Own_CUs.Reference (C).Include (Root.View);
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
            pragma Annotate (Xcov, Exempt_On);
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
                    (Kind, View_Db.View, Path, Index, Sep_Name, Success);

               elsif Other.Source.Value = Path.Value then
                  --  Same source found by multiple projects
                  Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
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
                           --  both sources are directly defined for the view,
                           --  so we raise an error about duplicated units.

                           Traces.Trace
                             ("... error: both units are defined in the" &
                                " current view");

                           Error_Case := True;

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

            Remove_Source (View_Db,
                           Src.View,
                           Src.Path_Name,
                           Src.Inh_From,
                           False,
                           Messages);
         end if;


         if Remove_Other then
            declare
               Other_Loc : constant Source_Proxy :=
                             Get_Data (NS_Db.Tree_Db, Other.View).Sources
                             (Other.Source.Value);
            begin
               if Traces.Is_Active then
                  Traces.Trace
                    ("ignoring source '" &
                       String (Other_Loc.Path_Name) &
                       "' as it is not used anymore");
               end if;

               Remove_Source (View_Db,
                              Other_Loc.View,
                              Other_Loc.Path_Name,
                              Other_Loc.Inh_From,
                              False,
                              Messages);
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
                        pragma Annotate (Xcov, Exempt_On);
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

   ------------------------
   -- Check_Source_Lists --
   ------------------------

   procedure Check_Source_Lists
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object)
   is
      package PRA renames Project.Registry.Attribute;

      Attr : Project.Attribute.Object;
      Exc_List : Source_Set.Set;

   begin
      Data.Excluded_Sources.Clear;
      Data.Listed_Sources.Clear;
      Data.No_Sources := False;

      --  If we have attribute Excluded_Source_List_File

      Attr := Data.View.Attribute (PRA.Excluded_Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr, Exc_List, Messages);
         if not Messages.Has_Error then
            for Src of Exc_List loop
               Data.Excluded_Sources.Include
                 (Src, Source_Reference.Object (Attr.Value));
            end loop;
         end if;
      end if;

      --  If we have attribute Excluded_Source_Files

      Attr := Data.View.Attribute (PRA.Excluded_Source_Files);

      if Attr.Is_Defined then
         for File of Attr.Values loop
            Exc_List.Clear;
            Include_Simple_Filename
              (Exc_List, File.Text, File, Messages);
            if not Exc_List.Is_Empty then
               Data.Excluded_Sources.Include
                 (Exc_List.First_Element, Source_Reference.Object (File));
            end if;
         end loop;
      end if;

      --  Remove naming exception sources from inactive case alternatives

      for File of Data.View.Skipped_Sources loop
         Exc_List.Clear;
         Include_Simple_Filename
           (Exc_List, File.Text, File, Messages);
         if not Exc_List.Is_Empty then
            Data.Excluded_Sources.Include
              (Exc_List.First_Element, Source_Reference.Undefined);
         end if;
      end loop;

      --  If we have attribute Source_List_File

      Attr := Data.View.Attribute (PRA.Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr, Data.Listed_Sources, Messages);
      end if;

      --  If we have attribute Source_Files

      Attr := Data.View.Attribute (PRA.Source_Files);

      if Attr.Is_Defined then
         if Attr.Values.Is_Empty then
            Data.No_Sources := True;
         else
            for File of Attr.Values loop
               Include_Simple_Filename
                 (Data.Listed_Sources, File.Text, File, Messages);
            end loop;
         end if;
      end if;
   end Check_Source_Lists;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Db : access GPR2.Build.Tree_Db.Object;
      View : GPR2.Project.View.Object) return View_Data_Ref is
   begin
      return -Db.View_Database (View);
   end Get_Data;

   -----------------------------
   -- Include_Simple_Filename --
   -----------------------------

   procedure Include_Simple_Filename
     (Set      : in out Source_Set.Set;
      Value    : Value_Type;
      Sloc     : Source_Reference.Object'Class;
      Messages : in out GPR2.Log.Object)
   is
      Position : Source_Set.Cursor;
      Inserted : Boolean;
   begin
      if Has_Directory_Separator (Value) then
         Messages.Append
           (Message.Create
              (Message.Error,
               "file name cannot include directory information (""" & Value
               & """)",
               Sloc));

      elsif Value'Length = 0 then
         Messages.Append
           (Message.Create
              (Message.Error,
               "file name cannot be empty",
               Sloc));
      else
         Set.Insert (Filename_Type (Value), Position, Inserted);
      end if;
   end Include_Simple_Filename;

   ----------------------
   -- Read_Source_List --
   ----------------------

   procedure Read_Source_List
     (View      : Project.View.Object;
      Attr      : Project.Attribute.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object)
   is
      use Ada.Strings;
      use type Ada.Strings.Maps.Character_Set;
      package PRA renames Project.Registry.Attribute;

      Filename : constant GPR2.Path_Name.Object :=
                   (if GNAT.OS_Lib.Is_Absolute_Path (Attr.Value.Text)
                    then Path_Name.Create_File
                      (Filename_Type (Attr.Value.Text))
                    else View.Dir_Name.Compose
                      (Filename_Type (Attr.Value.Text)));
      Skip_Set : constant Strings.Maps.Character_Set :=
                   Maps.Constants.Control_Set or Maps.To_Set (" ");
      F        : Text_IO.File_Type;

   begin
      if View.Kind not in K_Standard | K_Library then
         return;
      end if;

      if not Filename.Exists or else Filename.Is_Directory then
         Messages.Append
           (Message.Create
              (Message.Error,
               (if Attr.Name.Id = PRA.Excluded_Source_List_File
                then "excluded "
                else "") & "source list file " &
                  Filename.String_Value & " not found",
               Sloc => Attr));

         return;
      end if;

      Text_IO.Open (F, Text_IO.In_File, Filename.String_Value);

      while not Text_IO.End_Of_File (F) loop
         declare
            Line : constant String :=
                     Fixed.Trim (Text_IO.Get_Line (F), Skip_Set, Skip_Set);

         begin
            if Line /= ""
              and then not GNATCOLL.Utils.Starts_With (Line, "--")
            then
               Include_Simple_Filename
                 (Set, Line, Attr.Value, Messages);
            end if;
         end;
      end loop;

      Text_IO.Close (F);
   end Read_Source_List;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object)
   is
      Sources : Filename_Source_Maps.Map;
   begin
      Update_Sources_List.Process (Data, False, Messages);

      --  Disambiguate unit kind for Ada bodies

      if Data.Tree_Db.Source_Option >= Sources_Units
        and then not Data.View.Is_Extended
        and then Data.View.Kind in With_Object_Dir_Kind
      then
         --  Check separates of separates

         --  Work on a copy since this operation might change Data.Sources
         --  so we don't want to hold cursors on this container.
         Sources := Data.Sources;

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
   end Refresh;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Data               : View_Data_Ref;
      View_Owner         : GPR2.Project.View.Object;
      Path               : Filename_Type;
      Extended_View      : GPR2.Project.View.Object;
      Resolve_Visibility : Boolean := False;
      Messages           : in out GPR2.Log.Object)
   is
      use type GPR2.Project.View.Object;

      Basename   : constant Simple_Name :=
                     Path_Name.Simple_Name (Path);
      C_Overload : Basename_Source_List_Maps.Cursor;
      Proxy      : constant Source_Proxy := (Path_Len  => Path'Length,
                                             View      => View_Owner,
                                             Path_Name => Path,
                                             Inh_From  => Extended_View);
      Owner_Db   : constant View_Data :=
                     (if View_Owner = Data.View
                      then Data
                      else Get_Data (Data.Tree_Db, View_Owner));
      Src_Info   : constant Src_Info_Maps.Constant_Reference_Type :=
                     Owner_Db.Src_Infos.Constant_Reference (Path);

   begin
      if Data.Sources.Contains (Proxy.Path_Name) then
         Data.Sources.Delete (Proxy.Path_Name);

         C_Overload := Data.Overloaded_Srcs.Find (Basename);

         if Basename_Source_List_Maps.Has_Element (C_Overload)
           and then Data.Overloaded_Srcs.Reference (C_Overload).Contains
                       (Proxy)
         then
            Data.Overloaded_Srcs.Reference (C_Overload).Delete (Proxy);
         end if;

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
      else
         Traces.Trace
           ("!!! Attempt to remove """ &
              String (Proxy.Path_Name) &
              """ while key is not in the Sources map for project """ &
              String (Data.View.Name));
      end if;
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

   ------------------------
   -- Resolve_Visibility --
   ------------------------

   procedure Resolve_Visibility
     (Data     : View_Data_Ref;
      Cursor   : in out Basename_Source_List_Maps.Cursor;
      Messages : in out GPR2.Log.Object)
   is
      use type Ada.Containers.Count_Type;
      use type Project.View.Object;

      function Source_Info
        (Src : Source_Proxy) return Src_Info_Maps.Reference_Type
      is (Get_Data (Data.Tree_Db,
          Src.View).Src_Infos.Reference (Src.Path_Name));
      procedure Propagate_Visible_Source_Removal (Src : Source_Proxy);
      procedure Propagate_Visible_Source_Added (Src : Source_Proxy);

      ------------------------------------
      -- Propagate_Visible_Source_Added --
      ------------------------------------

      procedure Propagate_Visible_Source_Added (Src : Source_Proxy) is
         Src_Info : constant Src_Info_Maps.Reference_Type :=
                      Source_Info (Src);

      begin
         if Data.View.Is_Extended then
            declare
               Ext_Data : constant View_Data_Ref :=
                            Get_Data (Data.Tree_Db, Data.View.Extending);
               Name     : constant Simple_Name :=
                            GPR2.Path_Name.Simple_Name (Src.Path_Name);
            begin
               --  Check if the extending project excludes the source
               if Ext_Data.Excluded_Sources.Contains (Name) then
                  Ext_Data.Actually_Excluded.Include (Name, Src);

                  --  No further propagation, do not use the source as
                  --  unit part, so just return.

                  return;

               else
                  Add_Source
                    (Ext_Data,
                     Src.View,
                     Src.Path_Name,
                     Extended_View      => Data.View,
                     Resolve_Visibility => True,
                     Messages           => Messages);
               end if;
            end;

         elsif Src_Info.Has_Units then
            --  Update unit information. Note that we do that on "final"
            --  views only, so not if the view is extended or aggregated in
            --  a library, since that's the extending or aggregating lib
            --  that will have the full picture on what is visible or not.

            Traces.Trace
              ("visible source propagation: aggregating the units in the" &
                 " view db");

            for U of Src_Info.Units loop
               for Root of Src.View.Namespace_Roots loop
                  if Root.Kind in With_View_Db
                    and then U.Kind /= S_No_Body
                  then
                     Add_Unit_Part
                       (NS_Db    => Get_Data (Data.Tree_Db, Root),
                        CU       => U.Name,
                        Kind     => U.Kind,
                        Sep_Name => U.Separate_Name,
                        View_Db  => Data,
                        Path     => Src_Info.Path_Name,
                        Src      => Src,
                        Index    => U.Index,
                        Messages => Messages);
                  end if;
               end loop;
            end loop;
         end if;
      end Propagate_Visible_Source_Added;

      --------------------------------------
      -- Propagate_Visible_Source_Removal --
      --------------------------------------

      procedure Propagate_Visible_Source_Removal
        (Src : Source_Proxy)
      is
         Src_Info : constant Source_Base.Object :=
                      Source_Info (Src).Element.all;
      begin
         if Data.View.Is_Extended then
            declare
               Ext_Data : constant View_Data_Ref :=
                            Get_Data (Data.Tree_Db, Data.View.Extending);
               Name     : constant Simple_Name :=
                            GPR2.Path_Name.Simple_Name (Src.Path_Name);
            begin
               if Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On);
                  Traces.Trace
                    ("visible source removed propagation: extended project '"
                     & String (Data.View.Name)
                     & "' for source " & String (Name));
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               --  Check if the extending project excludes the source
               if Ext_Data.Excluded_Sources.Contains (Name) then
                  Ext_Data.Actually_Excluded.Delete (Name);
                  --  No further propagation, do not use the source as
                  --  unit part, so just return.
                  return;

               else
                  if Traces.Is_Active then
                     pragma Annotate (Xcov, Exempt_On);
                     Traces.Trace
                       ("visible source propagation: remove '" &
                          String (Name) &
                          "' from extending view '" &
                          String (Ext_Data.View.Name) &
                          "' and resolve visibility");
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

                  Remove_Source
                    (Ext_Data,
                     Src.View,
                     Src.Path_Name,
                     Extended_View      => Data.View,
                     Resolve_Visibility => True,
                     Messages           => Messages);
               end if;
            end;

         elsif Src_Info.Has_Units then
            Traces.Trace
              ("visible source propagation: removing the units from the" &
                 " view db");

            for U of Src_Info.Units loop
               for Root of Src.View.Namespace_Roots loop
                  if Root.Kind in With_View_Db then
                     Remove_Unit_Part
                       (Get_Data (Data.Tree_Db, Root),
                        CU       => U.Name,
                        Kind     => U.Kind,
                        Sep_Name => U.Separate_Name,
                        View_Db  => Data,
                        Path     => Src_Info.Path_Name,
                        Index    => U.Index);
                  end if;
               end loop;
            end loop;
         end if;
      end Propagate_Visible_Source_Removal;

      Basename  : constant Simple_Name :=
                    Basename_Source_List_Maps.Key (Cursor);
      Set       : constant Basename_Source_List_Maps.Constant_Reference_Type :=
                    Data.Overloaded_Srcs.Constant_Reference (Cursor);
      Candidate : access constant Source_Proxy;
      Current   : access constant Source_Proxy;
      C_Path    : Basename_Source_Maps.Cursor;
      C_Info    : Src_Info_Maps.Cursor;
      C_Info2   : Src_Info_Maps.Cursor;
      SR1, SR2  : Natural;
      Clashes   : Natural_Sets.Set;

   begin
      if Traces.Is_Active then
         pragma Annotate (Xcov, Exempt_On);
         Traces.Trace
           ("=== Resolve visibility for '" & String (Basename) & "' ===");
         pragma Annotate (Xcov, Exempt_Off);
      end if;

      if Set.Is_Empty then
         --  no source for the specified basenamne

         Traces.Trace ("no source for basename");

         Candidate := null;

      elsif Set.Length = 1 then
         --  Only one source in the set: just use it

         Traces.Trace ("1 source for basename");

         Candidate := Set.Constant_Reference (Set.First).Element;

      else
         --  project extension case, or the same basename is found in
         --  different source dirs

         Traces.Trace ("several sources for basename");

         for Curs in Set.Iterate loop
            declare
               C : constant Source_Proxy_Sets.Constant_Reference_Type :=
                     Set.Constant_Reference (Curs);
            begin
               if Candidate = null then
                  --  First value, consider it as a candidate

                  Candidate := C.Element;

                  if Candidate.View = Data.View then
                     --  Own source, get Src_Info cursor
                     C_Info := Data.Src_Infos.Find (Candidate.Path_Name);

                     if Traces.Is_Active then
                        pragma Annotate (Xcov, Exempt_On);
                        Traces.Trace
                          ("found first candidate owned by the view: " &
                             String (Candidate.Path_Name));
                        pragma Annotate (Xcov, Exempt_Off);
                     end if;
                  else
                     if Traces.Is_Active then
                        pragma Annotate (Xcov, Exempt_On);
                        Traces.Trace
                          ("found first candidate not owned by the view: " &
                             String (Candidate.Path_Name));
                        pragma Annotate (Xcov, Exempt_Off);
                     end if;
                  end if;

               elsif C.View = Data.View
                 and then Candidate.View /= Data.View
               then
                  --  Candidate was inherited: own source overrides it
                  Candidate := C.Element;
                  C_Info    := Data.Src_Infos.Find (Candidate.Path_Name);

                  if Traces.Is_Active then
                     pragma Annotate (Xcov, Exempt_On);
                     Traces.Trace
                       ("found new candidate owned by the view, overloading " &
                          "old candidate: " &
                          String (Candidate.Path_Name));
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

               elsif Candidate.View = Data.View
                 and then C.View /= Data.View
               then
                  --  Candidate is owned by current view, so ignore inherited
                  --  source
                  if Traces.Is_Active then
                     pragma Annotate (Xcov, Exempt_On);
                     Traces.Trace
                       ("ignoring " &
                          String (C.Element.Path_Name) & ", overloaded " &
                          "by current candidate: " &
                          String (Candidate.Path_Name));
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

               elsif C.View = Data.View then
                  --  Both candidates are owned by the view, check
                  --  Source_Reference: the declaration order of the source
                  --  directory in the Source_Dirs attribute gives the
                  --  visibility priority

                  if Traces.Is_Active then
                     pragma Annotate (Xcov, Exempt_On);
                     Traces.Trace
                       ("checking sources both owned by the view " &
                          String (C.Element.Path_Name) & " and " &
                          String (Candidate.Path_Name));
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

                  C_Info2 := Data.Src_Infos.Find (C.Element.Path_Name);

                  SR1 :=
                    Src_Info_Maps.Element (C_Info).Source_Dir_Value_Index;
                  SR2 :=
                    Src_Info_Maps.Element (C_Info2).Source_Dir_Value_Index;

                  if SR1 = SR2 then
                     Traces.Trace
                       ("the sources come from the same Source_Dir value");

                     Clashes.Include (SR1);

                  elsif SR2 < SR1 then
                     --  Source_Ref of C2 is declared before the one of
                     --  Candidate, so takes precedence (and clears any
                     --  clashing situation).

                     if Traces.Is_Active then
                        pragma Annotate (Xcov, Exempt_On);
                        Traces.Trace
                          ("priority for source_dir " &
                             String (C.Element.Path_Name));
                        pragma Annotate (Xcov, Exempt_Off);
                     end if;

                     Candidate := C.Element;
                     C_Info := C_Info2;
                     Clashes.Clear;
                  end if;

               elsif Candidate.View.Is_Compilable
                 (Source_Info (Candidate.all).Language)
               then
                  --  Remaining case: inheritance shows two candidate sources

                  --  If the source is compilable, then this generates an error
                  --  since we derive the object file from the source, so
                  --  cannot handle two sources with the same simple name.

                  Messages.Append
                    (Message.Create
                       (Message.Error,
                        '"' & String (Basename) & '"' &
                          " is found in several extended projects",
                        Source_Reference.Create
                          (Data.View.Path_Name.Value, 0, 0)));

                  --  Use alphabetical sort to have consistent output.
                  --  This is in particular important when comparing test
                  --  output.

                  declare
                     C_First : constant Boolean :=
                                 C.Path_Name < Candidate.Path_Name;
                     P1      : constant Filename_Type :=
                                 (if C_First
                                  then C.Path_Name
                                  else Candidate.Path_Name);
                     P2      : constant Filename_Type :=
                                 (if not C_First
                                  then C.Path_Name
                                  else Candidate.Path_Name);
                     V1, V2  : GPR2.Path_Name.Object;

                  begin
                     if C_First then
                        V1 := C.View.Path_Name;
                        V2 := Candidate.View.Path_Name;
                     else
                        V1 := Candidate.View.Path_Name;
                        V2 := C.View.Path_Name;
                     end if;

                     Messages.Append
                       (Message.Create
                          (Message.Error,
                           String (P1),
                           Source_Reference.Create (V1.Value, 0, 0),
                           Indent => 1));
                     Messages.Append
                       (Message.Create
                          (Message.Error,
                           String (P2),
                           Source_Reference.Create (V2.Value, 0, 0),
                           Indent => 1));
                  end;

                  Candidate := null;

                  exit;
               end if;
            end;
         end loop;

         if not Clashes.Is_Empty
           and then Candidate.View.Is_Compilable
             (Src_Info_Maps.Element (C_Info).Language)
         then
            for SR of Clashes loop
               Messages.Append
                 (Message.Create
                    (Message.Error,
                     '"' & String (Basename) & '"' &
                       " is found multiple times for the same source" &
                       " directory",
                     Data.View.Attribute
                       (Project.Registry.Attribute.Source_Dirs)));
            end loop;

            Candidate := null;
         end if;
      end if;

      C_Path := Data.Basenames.Find (Basename);

      if Basename_Source_Maps.Has_Element (C_Path) then
         Current := Data.Basenames.Constant_Reference (C_Path).Element;
      else
         Current := null;
      end if;

      if Current /= Candidate then
         --  Remove current visible source
         if Current /= null then
            if Traces.Is_Active then
               pragma Annotate (Xcov, Exempt_On);
               Traces.Trace
                 ("removing source " &
                    String (Current.Path_Name));
               pragma Annotate (Xcov, Exempt_Off);
            end if;

            Propagate_Visible_Source_Removal (Current.all);

            if Candidate = null then
               Data.Basenames.Delete (C_Path);
            end if;
         end if;

         if Candidate /= null then
            if Current = null then
               Data.Basenames.Insert (Basename, Candidate.all);
            else
               Data.Basenames.Replace_Element (C_Path, Candidate.all);
            end if;

            if Traces.Is_Active then
               pragma Annotate (Xcov, Exempt_On);
               Traces.Trace
                 ("declare visible " &
                    String (Candidate.Path_Name));
               pragma Annotate (Xcov, Exempt_Off);
            end if;

            Propagate_Visible_Source_Added (Candidate.all);
         end if;
      end if;
   end Resolve_Visibility;

   ------------
   -- Source --
   ------------

   function Source
     (Data : View_Data_Ref;
      Pos  : Basename_Source_Maps.Cursor) return Build.Source.Object
   is
      Proxy : constant Source_Proxy := Basename_Source_Maps.Element (Pos);

      use type GPR2.Project.View.Object;
   begin
      if Proxy.View = Data.View then
         return Build.Source.Create
           (Base_Source    => Data.Src_Infos.Element (Proxy.Path_Name),
            Defining_View  => Proxy.View,
            Owning_View    => Data.View,
            Inherited_From => Proxy.Inh_From,
            Is_Visible     => True);
      else
         return Build.Source.Create
           (Base_Source    => Get_Data
              (Data.Tree_Db,
               Proxy.View).Src_Infos.Element
              (Proxy.Path_Name),
            Defining_View  => Proxy.View,
            Owning_View    => Data.View,
            Inherited_From => Proxy.Inh_From,
            Is_Visible     => True);
      end if;
   end Source;

   ------------
   -- Source --
   ------------

   function Source
     (Data  : View_Data_Ref;
      Proxy : Source_Proxy) return Build.Source.Object
   is
      use type GPR2.Project.View.Object;

      BN            : constant Simple_Name :=
                        Path_Name.Simple_Name (Proxy.Path_Name);
      C             : constant Basename_Source_Maps.Cursor :=
                        Data.Basenames.Find (BN);
      Base_Src      : constant GPR2.Build.Source_Base.Object :=
                        (if Proxy.View = Data.View
                         then Data.Src_Infos.Element (Proxy.Path_Name)
                         else Get_Data
                           (Data.Tree_Db, Proxy.View).Src_Infos.Element
                             (Proxy.Path_Name));
      Is_Compilable : constant Boolean :=
                        Data.View.Is_Compilable (Base_Src.Language);
      Is_Visible    : constant Boolean :=
                        not Is_Compilable or else
                            (Basename_Source_Maps.Has_Element (C) and then
                             Basename_Source_Maps.Element (C) = Proxy);

   begin
      return Build.Source.Create
        (Base_Source    => Base_Src,
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

   package body Update_Sources_List is separate;

   --------------------
   -- Visible_Source --
   --------------------

   function Visible_Source
     (Data      : View_Data_Ref;
      Basename  : Simple_Name;
      Ambiguous : out Boolean) return Build.Source.Object
   is
      C          : Basename_Source_Maps.Cursor :=
                     Data.Basenames.Find (Basename);
      Candidate  : Build.Source.Object;

   begin
      --  First set the out value

      Ambiguous := False;

      --  Look for the source in the view's closure (withed or limited withed
      --  views)

      if Basename_Source_Maps.Has_Element (C) then
         return Source (Data, C);
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

                        return Candidate;
                     end if;

                  else
                     Candidate := Source (V_Data, C);
                  end if;
               end if;
            end;
         end if;
      end loop;

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
