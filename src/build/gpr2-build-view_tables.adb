--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with GNAT.OS_Lib;

with GPR2.Build.Source.Ada_Parser;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;

package body GPR2.Build.View_Tables is

   procedure Add_Unit_Part
     (NS_Db     : View_Data_Ref;
      CU        : Name_Type;
      Kind      : Unit_Kind;
      Sep_Name  : Optional_Name_Type;
      View_Db   : View_Data_Ref;
      Path      : Path_Name.Object;
      Index     : Unit_Index;
      Messages  : in out GPR2.Log.Object)
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
      File    : in out Source.Object)
     with Pre => Root_Db.Is_Root
                   and then File.Has_Single_Unit
                   and then File.Unit.Kind = S_Separate;

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
      --  function Get_Owner_Db return View_Data_Ref is
      --    (if View_Owner = Data.View
      --     then Data
      --     else Get_Data (Data.Tree_Db, View_Owner));

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

   begin
      Data.Overloaded_Srcs.Insert (Path_Name.Simple_Name (Path),
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
     (To   : View_Data_Ref;
      CU   : Name_Type;
      Root : View_Data_Ref)
   is
      C       : Unit_Maps.Cursor;
      New_Set : Project.View.Set.Object;

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
     (NS_Db    : View_Data_Ref;
      CU       : Name_Type;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type;
      View_Db  : View_Data_Ref;
      Path     : Path_Name.Object;
      Index    : Unit_Index;
      Messages : in out GPR2.Log.Object)
   is
      Cursor  : Compilation_Unit_Maps.Cursor;
      Done    : Boolean;
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
            Other       : Path_Name.Object;
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

               if Other.Value = Path.Value then
                  --  Same source found by multiple projects
                  Messages.Append
                    (Message.Create
                       (Level => Message.Error,
                        Message => "source file """ &
                          String (Path.Simple_Name) &
                          """ already part of project " &
                          String (CU_Instance.Get (Kind, Sep_Name).View.Name),
                        Sloc    => Source_Reference.Create
                          (View_Db.View.Path_Name.Value, 0, 0)));
               else
                  Messages.Append
                    (Message.Create
                       (Level   => Message.Warning,
                        Message => "duplicated " &
                          Image (Kind) & " for unit """ & String (CU) &
                          """ in " & String (Other.Value) & " and " &
                          String (Path.Value),
                        Sloc    =>
                          Source_Reference.Create
                            (NS_Db.View.Path_Name.Value, 0, 0)));
               end if;
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
     (Root_Db : View_Tables.View_Data_Ref;
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

   begin
      Data.Excluded_Sources.Clear;
      Data.Listed_Sources.Clear;

      --  If we have attribute Excluded_Source_List_File

      Attr := Data.View.Attribute (PRA.Excluded_Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr, Data.Excluded_Sources, Messages);
      end if;

      --  If we have attribute Excluded_Source_Files

      Attr := Data.View.Attribute (PRA.Excluded_Source_Files);

      if Attr.Is_Defined then
         for File of Attr.Values loop
            Include_Simple_Filename
              (Data.Excluded_Sources, File.Text, File, Messages);
         end loop;
      end if;

      --  Remove naming exception sources from inactive case alternatives

      for File of Data.View.Skipped_Sources loop
         Include_Simple_Filename
           (Data.Excluded_Sources, File.Text, File, Messages);
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
         for File of Attr.Values loop
            Include_Simple_Filename
              (Data.Listed_Sources, File.Text, File, Messages);
         end loop;
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
                 (Set, Line, Attr, Messages);
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
      Messages : in out GPR2.Log.Object) is
   begin
      Update_Sources_List.Process (Data, False, Messages);

      --  Disambiguate unit kind for Ada bodies

      if Data.Tree_Db.Source_Option >= Sources_Units
        and then not Data.View.Is_Extended
        and then Data.View.Kind in With_Object_Dir_Kind
      then
         --  Check separates of separates

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
         begin
            NS_Db.Separates.Delete (Full_Name);
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
      use type Source_Reference.Value.Object;

      procedure Propagate_Visible_Source_Removal (Src : Source_Proxy);
      procedure Propagate_Visible_Source_Added (Src : Source_Proxy);

      ------------------------------------
      -- Propagate_Visible_Source_Added --
      ------------------------------------

      procedure Propagate_Visible_Source_Added (Src : Source_Proxy) is
         View_Db  : constant View_Data_Ref :=
                      Get_Data (Data.Tree_Db, Src.View);
         Src_Info : constant Src_Info_Maps.Reference_Type :=
                      View_Db.Src_Infos.Reference (Src.Path_Name);
      begin
         if Data.View.Is_Extended then
            --  ??? Check the view's list of excluded sources before doing that
            declare
               Ext_Data : constant View_Data_Ref :=
                            Get_Data (Data.Tree_Db, Data.View.Extending);
            begin
               if not Ext_Data.Excluded_Sources.Contains
                 (GPR2.Path_Name.Simple_Name (Src.Path_Name))
               then
                  Add_Source
                    (Ext_Data,
                     Src.View,
                     Src.Path_Name,
                     Extended_View      => Data.View,
                     Resolve_Visibility => True,
                     Messages           => Messages);
               end if;
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
                  if U.Kind /= S_No_Body then
                     Add_Unit_Part
                       (NS_Db    => Get_Data (Data.Tree_Db, Root),
                        CU       => U.Name,
                        Kind     => U.Kind,
                        Sep_Name => U.Separate_Name,
                        View_Db  => Data,
                        Path     => Src_Info.Path_Name,
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
      Set       : constant Basename_Source_List_Maps.Constant_Reference_Type :=
                    Data.Overloaded_Srcs.Constant_Reference (Cursor);
      Candidate : access constant Source_Proxy;
      Current   : access constant Source_Proxy;
      C_Src     : Basename_Source_Maps.Cursor :=
                    Data.Sources.Find (Basename);
      C_Info    : Src_Info_Maps.Cursor;
      C_Info2   : Src_Info_Maps.Cursor;
      SR1, SR2  : Source_Reference.Value.Object;
      Clashes   : GPR2.Containers.Source_Value_Set;

   begin
      if Set.Is_Empty then
         --  no source for the specified basenamne

         Candidate := null;

      elsif Set.Length = 1 then
         --  Only one source in the set: just use it

         Candidate := Set.Constant_Reference (Set.First).Element;

      else
         --  project extension case, or the same basename is found in
         --  different source dirs

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
                  end if;

               elsif C.View = Data.View
                 and then Candidate.View /= Data.View
               then
                  --  Candidate was inherited: own source overrides it
                  Candidate := C.Element;
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

                     Candidate := C.Element;
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

            Candidate := null;
         end if;
      end if;

      if Basename_Source_Maps.Has_Element (C_Src) then
         Current := Data.Sources.Constant_Reference (C_Src).Element;
      else
         Current := null;
      end if;

      if Current /= Candidate then
         --  Remove current visible source
         if Current /= null then
            Propagate_Visible_Source_Removal (Current.all);

            if Candidate = null then
               Data.Sources.Delete (C_Src);
            end if;
         end if;

         if Candidate /= null then
            if Current = null then
               Data.Sources.Insert (Basename, Candidate.all);
            else
               Data.Sources.Replace_Element (C_Src, Candidate.all);
            end if;

            Propagate_Visible_Source_Added (Candidate.all);
         end if;
      end if;
   end Resolve_Visibility;

   package body Update_Sources_List is separate;

end GPR2.Build.View_Tables;
