--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;

with GNATCOLL.OS.FS;
with GNATCOLL.Traces;

with GPR2.Build.Makefile_Parser;
pragma Warnings (Off, ".* is not referenced");
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");
with GPR2.Build.Tree_Db;
with GPR2.Build.External_Options;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Source_Reference;
with GPR2.Tree_Internal;
with GPR2.View_Internal;
with GNAT.OS_Lib;

package body GPR2.Build.Actions.Compile is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.ACTIONS.COMPILE",
                 GNATCOLL.Traces.Off);


   function Lang_Img (Lang : Language_Id) return Filename_Type is
      (Filename_Type (Ada.Characters.Handling.To_Lower (GPR2.Image (Lang))));

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      procedure Add_Attr
        (Id           : Q_Attribute_Id;
         Idx          : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean);

      procedure Add_Options_With_Arg
        (Attr         : Project.Attribute.Object;
         Arg          : String;
         In_Signature : Boolean);

      procedure Add_Config_File;

      procedure Add_Include_Path;

      procedure Add_Mapping_File;

      Lang_Idx : constant PAI.Object := PAI.Create (Self.Lang);
      Src_Idx  : constant PAI.Object :=
                   PAI.Create
                     (Value_Type (Self.Src.Path_Name.Simple_Name),
                      GPR2.File_Names_Case_Sensitive,
                      Object'Class (Self).Src_Index);

      --------------
      -- Add_Attr --
      --------------

      procedure Add_Attr
        (Id           : Q_Attribute_Id;
         Idx          : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean)
      is
         Mode : constant Build.Command_Line.Signature_Mode :=
                  (if In_Signature
                   then GPR2.Build.Command_Line.In_Signature
                   else GPR2.Build.Command_Line.Ignore);
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute (Id, Idx);
      begin
         if not Attr.Is_Defined then
            return;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               if Val.Text'Length > 0 then
                  Cmd_Line.Add_Argument (Val.Text, Mode);
               end if;
            end loop;
         else
            Cmd_Line.Add_Argument (Attr.Value.Text, Mode);
         end if;
      end Add_Attr;

      ---------------------
      -- Add_Config_File --
      ---------------------

      procedure Add_Config_File
      is
         use GNATCOLL.OS.FS;

         procedure Check_Exceptions_For
           (View : GPR2.Project.View.Object;
            FD   : GNATCOLL.OS.FS.File_Descriptor);

         procedure Write_Exception
           (FD        : GNATCOLL.OS.FS.File_Descriptor;
            Is_Spec   : Boolean;
            Unit_Name : String;
            Src       : String;
            Index     : Unit_Index);

         procedure Write_Tmpl
           (FD      : GNATCOLL.OS.FS.File_Descriptor;
            Attr_Id : Q_Attribute_Id);

         Cfg_File_Opt : constant GPR2.Project.Attribute.Object :=
                          Self.View.Attribute
                            (PRA.Compiler.Config_File_Switches, Lang_Idx);
         Spec_Ext     : constant Project.Attribute.Object :=
                          Self.View.Attribute (PRA.Naming.Spec_Suffix,
                                               Lang_Idx);
         Body_Ext     : constant Project.Attribute.Object :=
                          Self.View.Attribute (PRA.Naming.Body_Suffix,
                                               Lang_Idx);
         Dot_Repl     : constant Project.Attribute.Object :=
                          Self.View.Attribute (PRA.Naming.Dot_Replacement);
         Casing       : constant Project.Attribute.Object :=
                          Self.View.Attribute (PRA.Naming.Casing);
         Done_Except  : Containers.Name_Set;

         --------------------------
         -- Check_Exceptions_For --
         --------------------------

         procedure Check_Exceptions_For
           (View : GPR2.Project.View.Object;
            FD   : GNATCOLL.OS.FS.File_Descriptor)
         is
         begin
            for Attr of View.Attributes (PRA.Naming.Spec) loop
               declare
                  Unit : constant String := Attr.Index.Value;
                  Src  : constant String := Attr.Value.Text;
                  Idx  : constant Unit_Index := (if Attr.Value.Has_At_Pos
                                                 then Attr.Value.At_Pos
                                                 else No_Index);
               begin
                  if not Done_Except.Contains (Name_Type (Unit & "%s")) then
                     Write_Exception
                       (FD        => FD,
                        Is_Spec   => True,
                        Unit_Name => Unit,
                        Src       => Src,
                        Index     => Idx);
                     Done_Except.Include (Name_Type (Unit & "%s"));
                  end if;
               end;
            end loop;

            for Attr of View.Attributes (PRA.Naming.Body_N) loop
               declare
                  Unit : constant String := Attr.Index.Value;
                  Src  : constant String := Attr.Value.Text;
                  Idx  : constant Unit_Index := (if Attr.Value.Has_At_Pos
                                                 then Attr.Value.At_Pos
                                                 else No_Index);
               begin
                  if not Done_Except.Contains (Name_Type (Unit & "%b")) then
                     Write_Exception
                       (FD        => FD,
                        Is_Spec   => False,
                        Unit_Name => Unit,
                        Src       => Src,
                        Index     => Idx);
                     Done_Except.Include (Name_Type (Unit & "%b"));
                  end if;
               end;
            end loop;
         end Check_Exceptions_For;

         ---------------------
         -- Write_Exception --
         ---------------------

         procedure Write_Exception
           (FD        : GNATCOLL.OS.FS.File_Descriptor;
            Is_Spec   : Boolean;
            Unit_Name : String;
            Src       : String;
            Index     : Unit_Index)
         is
            Attr : Project.Attribute.Object;
            Last : Natural;
         begin
            if Is_Spec then
               if Index = No_Index then
                  Attr :=
                    Self.View.Attribute
                      (PRA.Compiler.Config_Spec_File_Name, Lang_Idx);
               else
                  Attr :=
                    Self.View.Attribute
                      (PRA.Compiler.Config_Spec_File_Name_Index, Lang_Idx);
               end if;
            else
               if Index = No_Index then
                  Attr :=
                    Self.View.Attribute
                      (PRA.Compiler.Config_Body_File_Name, Lang_Idx);
               else
                  Attr :=
                    Self.View.Attribute
                      (PRA.Compiler.Config_Body_File_Name_Index, Lang_Idx);
               end if;
            end if;

            if not Attr.Is_Defined then
               return;
            end if;

            declare
               Cnt : constant String := Attr.Value.Text;
               Idx : constant String :=
                       (if Index = No_Index then ""
                        else Index'Image);
            begin

               Last := Cnt'First;

               for J in Cnt'First .. Cnt'Last - 1 loop
                  if Cnt (J) = '%' then
                     Write (FD, Cnt (Last .. J - 1));
                     Last := J + 2;

                     case Cnt (J + 1) is
                        when 'u' =>
                           Write (FD, Unit_Name);
                        when 'f' =>
                           Write (FD, Src);
                        when 'i' =>
                           Write (FD, Idx (Idx'First + 1 .. Idx'Last));
                        when others =>
                           Write (FD, "%" & Cnt (J + 1));
                     end case;
                  end if;
               end loop;

               Write (FD, Cnt (Last .. Cnt'Last) & ASCII.LF);
            end;
         end Write_Exception;

         ----------------
         -- Write_Tmpl --
         ----------------

         procedure Write_Tmpl
           (FD      : GNATCOLL.OS.FS.File_Descriptor;
            Attr_Id : Q_Attribute_Id)
         is
            Attr : constant Project.Attribute.Object :=
                     Self.View.Attribute (Attr_Id, Lang_Idx);
            Last     : Natural;
         begin
            if not Attr.Is_Defined then
               return;
            end if;

            declare
               Cnt : String renames Attr.Value.Text;
            begin
               Last := Cnt'First;

               for J in Cnt'First .. Cnt'Last - 1 loop
                  if Cnt (J) = '%' then
                     Write (FD, Cnt (Last .. J - 1));
                     Last := J + 2;

                     case Cnt (J + 1) is
                        when 'b' =>
                           Write (FD, Body_Ext.Value.Text);
                        when 's' =>
                           Write (FD, Spec_Ext.Value.Text);
                        when 'c' =>
                           Write (FD, Casing.Value.Text);
                        when 'd' =>
                           Write (FD, Dot_Repl.Value.Text);
                        when others =>
                           Write (FD, "%" & Cnt (J + 1));
                     end case;
                  end if;
               end loop;

               Write (FD, Cnt (Last .. Cnt'Last) & ASCII.LF);
            end;
         end Write_Tmpl;

      begin
         if not Cfg_File_Opt.Is_Defined then
            return;
         end if;

         declare
            File     : constant Tree_Db.Temp_File :=
                         Self.Get_Or_Create_Temp_File
                           (Lang_Img (Self.Lang) & "_config", Global);
         begin
            if File.FD /= Null_FD then
               Write_Tmpl (File.FD,
                           PRA.Compiler.Config_Spec_File_Name_Pattern);
               Write_Tmpl (File.FD,
                           PRA.Compiler.Config_Body_File_Name_Pattern);

               for V of Self.View.Closure (True, True, True) loop
                  Check_Exceptions_For (V, File.FD);
               end loop;

               Close (File.FD);
            end if;

            Add_Options_With_Arg
              (Cfg_File_Opt,
               String (GPR2.Path_Name.Simple_Name (File.Path)),
               False);
         end;
      end Add_Config_File;

      ----------------------
      -- Add_Include_Path --
      ----------------------

      procedure Add_Include_Path is
         Attr : GPR2.Project.Attribute.Object;

         function Inc_Path_File (Sw : String := "") return Filename_Type;
         --  Get or create a temporary include path file for the view

         function Quoted (S : String) return String;

         -------------------
         -- Inc_Path_File --
         -------------------

         function Inc_Path_File (Sw : String := "") return Filename_Type is
            use GNATCOLL.OS.FS;
            Tmp : constant Tree_Db.Temp_File :=
                    Self.Get_Or_Create_Temp_File
                      (Lang_Img (Self.Lang) & "_inc_path",
                       Actions.Global);

         begin
            if Tmp.FD /= Null_FD then
               for P of Self.View.Include_Path (Self.Lang) loop
                  Write (Tmp.FD, Sw & Quoted (P.String_Value) & ASCII.LF);
               end loop;

               Close (Tmp.FD);
            end if;

            return Tmp.Path;
         end Inc_Path_File;

         ------------
         -- Quoted --
         ------------

         function Quoted (S : String) return String is
            Res : Unbounded_String;
         begin
            for C of S loop
               if C = ' ' then
                  Append (Res, "\ ");
               elsif C = '\' then
                  Append (Res, "\\");
               else
                  Append (Res, C);
               end if;
            end loop;

            return -Res;
         end Quoted;

      begin
         Attr := Self.View.Attribute
           (PRA.Compiler.Include_Path_File, Lang_Idx);

         if Attr.Is_Defined then
            declare
               Inc_File  : constant Filename_Type := Inc_Path_File;
               Full_Path : constant GPR2.Path_Name.Object :=
                             Path_Name.Create_File (Inc_File);
            begin
               Cmd_Line.Add_Env_Variable
                 (String (Attr.Value.Text), Full_Path.String_Value);
            end;

            return;
         end if;

         Attr :=
           Self.View.Attribute
             (PRA.Compiler.Include_Switches_Via_Spec, Lang_Idx);

         if Attr.Is_Defined then
            --  need to create a temp file with the
            --  path, and then another temp file used as gcc spec in the form:
            --
            --  * cc1 :
            --  + @<tempfile>
            --
            --  Where 'cc1' is the first value in the list, and '-I' the second
            --  one.
            --
            --  Finally (and it's hardcoded in gpr1) add switch
            --  "-spec=<the spec temp file>

            declare
               Inc_File  : constant Filename_Type :=
                             Inc_Path_File (Attr.Values.Element (2).Text);
               Spec_File : constant Tree_Db.Temp_File :=
                             Self.Get_Or_Create_Temp_File
                               (Lang_Img (Self.Lang) & "_spec_inc_path",
                                Global);
               use GNATCOLL.OS.FS;

            begin
               if Spec_File.FD /= Null_FD then
                  Write (Spec_File.FD, "*" & Attr.Values.First_Element.Text &
                           ":" & ASCII.LF);
                  Write (Spec_File.FD, "+ @" & Quoted (String (Inc_File)) &
                           ASCII.LF);
                  Close (Spec_File.FD);
               end if;

               Cmd_Line.Add_Argument
                 ("-specs=" & String (Path_Name.Simple_Name (Spec_File.Path)),
                  Build.Command_Line.Ignore);
            end;

            return;
         end if;

         Attr := Self.View.Attribute (PRA.Compiler.Include_Switches, Lang_Idx);

         if Attr.Is_Defined then
            for Path of Self.View.Include_Path (Self.Lang) loop
               Add_Options_With_Arg
                 (Attr, Path.String_Value, False);
            end loop;
         end if;
      end Add_Include_Path;

      ----------------------
      -- Add_Mapping_File --
      ----------------------

      procedure Add_Mapping_File
      is
         use GNATCOLL.OS.FS;
         Attr         : constant Project.Attribute.Object :=
                          Self.View.Attribute
                            (PRA.Compiler.Mapping_File_Switches, Lang_Idx);
         Slot_Img_Raw : constant Filename_Type := Filename_Type (Slot'Image);
         Slot_Img     : constant Filename_Type :=
                          Slot_Img_Raw
                            (Slot_Img_Raw'First + 1 .. Slot_Img_Raw'Last);
      begin
         if not Attr.Is_Defined then
            --  Nothing to do
            return;
         end if;

         declare
            Map_File : constant Tree_Db.Temp_File :=
                         Self.Get_Or_Create_Temp_File
                           (Lang_Img (Self.Lang) & "_mapping_" & Slot_Img,
                            Global);
            S_Suffix : constant String :=
                         Self.View.Attribute
                           (PRA.Compiler.Mapping_Spec_Suffix,
                            Lang_Idx).Value.Text;
            B_Suffix : constant String :=
                         Self.View.Attribute
                           (PRA.Compiler.Mapping_Body_Suffix,
                            Lang_Idx).Value.Text;
            use Standard.Ada.Characters.Handling;
         begin
            if Map_File.FD /= Null_FD then
               for S of Self.View.Visible_Sources loop
                  if S.Language = Ada_Language then
                     for U of S.Units loop
                        if U.Kind /= S_No_Body then
                           declare
                              Key : constant String :=
                                      To_Lower (String (U.Full_Name)) &
                              (if U.Kind = S_Spec
                               then S_Suffix else B_Suffix);
                           begin
                              Write
                                (Map_File.FD,
                                 Key & ASCII.LF &
                                 String (S.Path_Name.Simple_Name) & ASCII.LF &
                                 S.Path_Name.String_Value & ASCII.LF);
                           end;
                        end if;
                     end loop;
                  end if;
               end loop;

               for S of Self.View.View_Db.Excluded_Inherited_Sources loop
                  if S.Language = Ada_Language then
                     for U of S.Units loop
                        if U.Kind /= S_No_Body then
                           declare
                              Key : constant String :=
                                      To_Lower (String (U.Full_Name)) &
                              (if U.Kind = S_Spec
                               then S_Suffix else B_Suffix);
                           begin
                              Write
                                (Map_File.FD,
                                 Key & ASCII.LF &
                                 String (S.Path_Name.Simple_Name) & ASCII.LF &
                                 "/" & ASCII.LF);
                           end;
                        end if;
                     end loop;
                  end if;
               end loop;

               Close (Map_File.FD);
            end if;

            Add_Options_With_Arg
              (Attr, String (Path_Name.Simple_Name (Map_File.Path)), False);
         end;
      end Add_Mapping_File;

      --------------------------
      -- Add_Options_With_Arg --
      --------------------------

      procedure Add_Options_With_Arg
        (Attr         : Project.Attribute.Object;
         Arg          : String;
         In_Signature : Boolean)
      is
         Mode : constant Build.Command_Line.Signature_Mode :=
                  (if In_Signature
                   then GPR2.Build.Command_Line.In_Signature
                   else GPR2.Build.Command_Line.Ignore);
      begin
         if not Attr.Is_Defined or else Attr.Values.Is_Empty then
            return;
         end if;

         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, Mode);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text & Arg, Mode);
      end Add_Options_With_Arg;

      Driver_Attr : constant GPR2.Project.Attribute.Object :=
                      Self.Ctxt.Attribute (PRA.Compiler.Driver, Lang_Idx);

      Tree : constant access GPR2.Tree_Internal.Object :=
        View_Internal.Get_RO (Self.Ctxt).Tree;
   begin
      if Tree.Languages_To_Compilers.Contains (Self.Lang) then
         declare
            Path : constant GPR2.Path_Name.Object :=
              GPR2.Path_Name.Create_File
                (Filename_Type
                   (Tree.Languages_To_Compilers.Element (Self.Lang)));
         begin
            if Path.Exists
              and then GNAT.OS_Lib.Is_Executable_File (Path.String_Value)
            then
               Cmd_Line.Set_Driver (Path);
            else
               declare
                  Found_Path : constant String :=
                    Locate_Exec_On_Path (String (Path.Simple_Name));
               begin
                  if Found_Path /= "" then
                     Cmd_Line.Set_Driver (Found_Path);
                  else
                     Self.Tree.Reporter.Report
                       (GPR2.Message.Create
                          (GPR2.Message.Error,
                           "compiler """
                           & Path.String_Value
                           & """"
                           & " not found, cannot compile """
                           & String (Self.Src.Path_Name.Simple_Name)
                           & '"',
                           GPR2.Source_Reference.Create
                             (Self.Ctxt.Path_Name.Value, 0, 0)));
                     return;
                  end if;
               end;
            end if;
         end;
      elsif Driver_Attr.Is_Defined then
         Cmd_Line.Set_Driver (Driver_Attr.Value.Text);
      else
         if not Self.Deactivated then
            Self.Tree.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "no compiler for language """ & Image (Self.Lang) &
                    """, cannot compile """ &
                    String (Self.Src.Path_Name.Simple_Name) & '"',
                  GPR2.Source_Reference.Create
                    (Self.Ctxt.Path_Name.Value, 0, 0)));
         end if;

         return;
      end if;

      Add_Attr (PRA.Compiler.Leading_Required_Switches, Lang_Idx, True, True);
      --  ??? need to filter out builder switches from command line
      --  Add_Attr (PRA.Builder.Switches, Lang_Idx, True);
      Add_Attr (PRA.Compiler.Required_Switches, Lang_Idx, True, True);
      Add_Attr (PRA.Compiler.Switches, Src_Idx, True, True);

      --  Add -cargs and -cargs:<lang>

      for Arg of Self.Tree.External_Options.Fetch
                   (External_Options.Compiler, Self.Lang)
      loop
         Cmd_Line.Add_Argument (Arg);
      end loop;

      --  Add -fPIC when compiling in the context of a shared or static-pic
      --  library.
      --
      --  ??? The aggregate library context is a bit weird here, since
      --  aggregated views that should be independent of their aggregating
      --  project also need to comply with the library kind of the aggregating
      --  view. We don't make virtual copies here of the views, and have no
      --  mechanism to differentiate objects from their original view and the
      --  ones modified because of the library_kind influence...

      declare
         type In_Lib_Kind is (Undefined, With_Fpic, No_Fpic);
         Use_Fpic : In_Lib_Kind := Undefined;
      begin
         if not Self.View.Is_Aggregated_In_Library then
            if Self.View.Is_Library
              and then Self.View.Library_Kind /= "static"
            then
               Use_Fpic := With_Fpic;
            else
               Use_Fpic := No_Fpic;
            end if;

         elsif Self.View.Is_Aggregated_In_Library then
            --  Try to comply with the aggregating library, but detect
            --  incoherences.

            for V of Self.View.Aggregate_Libraries loop
               if V.Library_Kind /= "static" then
                  if Use_Fpic = Undefined then
                     Use_Fpic := With_Fpic;
                  elsif Use_Fpic = No_Fpic then
                     --  Error ?
                     Use_Fpic := Undefined;
                  end if;
               else
                  if Use_Fpic = Undefined then
                     Use_Fpic := No_Fpic;
                  elsif Use_Fpic = With_Fpic then
                     --  Error ?
                     Use_Fpic := Undefined;
                  end if;
               end if;

               pragma Assert
                 (Use_Fpic /= Undefined,
                  "Aggregated view "
                  & String (Self.View.Name)
                  & " is used both in a static lib context and a shared"
                  & " library context");
            end loop;
         end if;

         if Use_Fpic = With_Fpic then
            Add_Attr (PRA.Compiler.Pic_Option, Lang_Idx, True, True);
         end if;
      end;

      declare
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute
                    (PRA.Compiler.Dependency_Switches, Lang_Idx);
      begin
         if Attr.Is_Defined then
            Add_Options_With_Arg
              (Attr,
               String (Object'Class (Self).Dep_File.Path.Simple_Name),
               True);
         end if;
      end;

      if not Signature_Only then
         Add_Include_Path;
         Add_Mapping_File;
         Add_Config_File;
      end if;

      Add_Attr (PRA.Compiler.Trailing_Required_Switches, Lang_Idx, True, True);

      declare
         Attr  : constant Project.Attribute.Object :=
                   Self.View.Attribute
                     (PRA.Compiler.Source_File_Switches, Lang_Idx);
         Index : constant Unit_Index := Object'Class (Self).Src_Index;
         Idx   : constant String :=
                   (if Index = No_Index then ""
                    else Index'Image);
      begin
         if Attr.Is_Defined then
            Add_Options_With_Arg
              (Attr,
               Self.Src.Path_Name.String_Value,
               True);
         else
            Cmd_Line.Add_Argument
              (Self.Src.Path_Name.String_Value, Build.Command_Line.Simple);
         end if;

         if Index /= No_Index then
            declare
               Sw : constant GPR2.Project.Attribute.Object :=
                      Self.View.Attribute (PRA.Compiler.Multi_Unit_Switches,
                                           Lang_Idx);
            begin
               pragma Assert
                 (Sw.Is_Defined,
                  "Compiler'Multi_Unit_Switches is not defined in the " &
                    "config project");

               Add_Options_With_Arg
                 (Sw, Idx (Idx'First + 1 .. Idx'Last), True);
            end;
         end if;
      end;

      if Self.Obj_File.Is_Defined then
         declare
            Sw : constant GPR2.Project.Attribute.Object :=
                   Self.View.Attribute (PRA.Compiler.Object_File_Switches,
                                        Lang_Idx);
         begin
            if Sw.Is_Defined then
               Add_Options_With_Arg
                 (Sw, String (Self.Obj_File.Path.Simple_Name), True);
            elsif Self.Lang = Ada_Language
              and then Object'Class (Self).Src_Index /= No_Index
            then
               --  For multi-unit sources, we need to specify the output object

               --  [eng/gpr/gpr-issues#446] TODO modify the KB to have a proper
               --  default here.
               Cmd_Line.Add_Argument ("-o");
               Cmd_Line.Add_Argument
                 (String (Self.Obj_File.Path.Simple_Name));
            end if;
         end;
      end if;

      if Self.Global_Config_File.Is_Defined
        or else Self.Local_Config_File.Is_Defined
      then
         declare
            Sw : constant GPR2.Project.Attribute.Object :=
                   Self.View.Attribute
                     (PRA.Compiler.Config_File_Switches, Lang_Idx);
         begin

            if Self.Global_Config_File.Is_Defined then
               Add_Options_With_Arg
                 (Sw, Self.Global_Config_File.String_Value, False);
            end if;

            if Self.Local_Config_File.Is_Defined then
               Add_Options_With_Arg
                 (Sw, Self.Local_Config_File.String_Value, False);
            end if;
         end;
      end if;

   exception
      when GNATCOLL.OS.OS_Error =>
         Self.Tree.Reporter.Report
           ("Problem accessing the object directory for project """ &
              String (Self.View.Name) & '"',
            To_Stderr => True,
            Level     => GPR2.Message.Important);
         raise Action_Error;
   end Compute_Command;

   ----------------------------
   -- Compute_Response_Files --
   ----------------------------

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      if not Signature_Only then
         declare
            use Build.Response_Files;

            Lang_Index : constant PAI.Object := PAI.Create (Self.Lang);
            A_RFF      : constant Project.Attribute.Object :=
                           Self.View.Attribute
                             (PRA.Compiler.Response_File_Format, Lang_Index);
            Format     : Response_File_Format := None;
         begin
            if A_RFF.Is_Defined then
               declare
                  LV : constant String :=
                         Ada.Characters.Handling.To_Lower (A_RFF.Value.Text);
               begin
                  if LV = "gnu" then
                     Format := GNU;
                  elsif LV = "object_list" then
                     Format := Object_List;
                  elsif LV = "gcc_gnu" then
                     Format := GCC_GNU;
                  elsif LV = "gcc_option_list" then
                     Format := GCC_Option_List;
                  elsif LV = "gcc_object_list" then
                     Format := GCC_Object_List;
                  end if;
               end;
            end if;

            if Format = GCC_GNU then
               declare
                  A_RFS  : constant Project.Attribute.Object :=
                             Self.View.Attribute
                               (PRA.Compiler.Response_File_Switches,
                                Lang_Index);
                  RFS    : constant Containers.Source_Value_List :=
                             (if A_RFS.Is_Defined
                              then A_RFS.Values
                              else Containers.Empty_Source_Value_List);
                  A_CLML : constant Project.Attribute.Object :=
                             Self.View.Attribute
                               (PRA.Compiler.Max_Command_Line_Length);
                  CLML   : constant Natural :=
                             (if A_CLML.Is_Defined
                              then Natural'Value (A_CLML.Value.Text)
                              else 0);
               begin
                  Self.Response_Files.Initialize (Format, Compiler, CLML, RFS);
               end;

               if Self.Response_Files.Length_Restriction (Cmd_Line) then
                  declare
                     Resp_File : constant Tree_Db.Temp_File :=
                                   Self.Get_Or_Create_Temp_File
                                     ("response_file", Local);
                  begin
                     Self.Response_Files.Register
                       (Resp_File.FD,
                        Resp_File.Path);
                  end;

                  Self.Response_Files.Create (Cmd_Line);
               end if;
            end if;
         end;
      end if;
   end Compute_Response_Files;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean)
   is
      use GPR2.Build.Signature;
   begin
      if Self.Dep_File.Is_Defined then
         declare
            Deps : constant GPR2.Containers.Filename_Set := Self.Dependencies;
         begin
            if Deps.Is_Empty then
               if Load_Mode then
                  Self.Tree.Reporter.Report
                    ("file """ & Self.Dep_File.Path.String_Value &
                       """ is missing or is wrongly formatted",
                     To_Stderr => True,
                     Level     => GPR2.Message.Important);
               end if;

               --  Dependency file parsing went wrong, at least put the direct
               --  source as an input.
               if not Self.Signature.Add_Input
                 (Artifacts.Files.Create (Self.Src.Path_Name))
                 and then Load_Mode
               then
                  return;
               end if;

               --  This actions should be done no matter what since the
               --  dependencies states are unknown.
               Self.Force := True;
               Self.Signature.Invalidate;
            end if;

            for Dep of Deps loop
               declare
                  Ambiguous : Boolean;
                  Src       : constant GPR2.Build.Source.Object :=
                                Self.Ctxt.Visible_Source
                                  (Path_Name.Simple_Name (Dep),
                                   Ambiguous);
                  Path      : GPR2.Path_Name.Object;
               begin
                  if not Ambiguous and then Src.Is_Defined then
                     Path := Src.Path_Name;
                  elsif (Self.Global_Config_File.Is_Defined
                         and then Dep = Self.Global_Config_File.Simple_Name)
                    or else
                      (Self.Local_Config_File.Is_Defined
                       and then Dep = Self.Local_Config_File.Simple_Name)
                  then
                     Traces.Trace
                       ("config file reported as dependency, " &
                          "ignoring : " & String (Dep));
                  else
                     Path :=
                       GPR2.Path_Name.Create_File
                         (Dep,
                          (if Self.Inh_From.Is_Defined
                           then Self.Inh_From.Object_Directory.Value
                           else Self.View.Object_Directory.Value));
                  end if;

                  if not Path.Exists then
                     Traces.Trace
                       ("Compute_Signature: cannot find dependency " &
                          String (Dep));

                     if Load_Mode then
                        Self.Signature.Clear;
                        return;
                     end if;

                  elsif not Self.Signature.Add_Input
                              (Artifacts.Files.Create (Path))
                    and then Load_Mode
                  then
                     return;
                  end if;
               end;
            end loop;
         end;

      else
         --  No dependency file, so just add the input source
         if not Self.Signature.Add_Input
                  (Artifacts.Files.Create (Self.Src.Path_Name))
           and then Load_Mode
         then
            return;
         end if;
      end if;

      if Self.Obj_File.Is_Defined then
         if Self.Dep_File.Is_Defined
           and then not Self.Signature.Add_Output (Self.Dep_File)
           and then Load_Mode
         then
            return;
         end if;

         if not Self.Signature.Add_Output (Self.Obj_File)
           and then Load_Mode
         then
            return;
         end if;

      elsif Load_Mode then
         --  if no object is produced, then force the re-generation of the
         --  compilation each time the action is called by clearing the
         --  checksums of the signature.
         Self.Signature.Invalidate;

         return;
      end if;

      if Self.Global_Config_File.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Global_Config_File))
        and then Load_Mode
      then
         return;
      end if;

      if Self.Local_Config_File.Is_Defined
        and then not Self.Signature.Add_Input
                       (Artifacts.Files.Create (Self.Local_Config_File))
        and then Load_Mode
      then
         return;
      end if;
   end Compute_Signature;

   ---------------------
   -- Dep_File_Suffix --
   ---------------------

   function Dep_File_Suffix (Self : Object) return Filename_Optional is
      Tree : constant access GPR2.Tree_Internal.Object :=
               View_Internal.Get_RO (Self.Ctxt).Tree;
   begin
      return Tree.Dependency_Suffix (Self.Lang);
   end Dep_File_Suffix;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self : Object) return Containers.Filename_Set
   is
      All_Deps : GPR2.Containers.Filename_Set;
   begin
      if not Self.Dep_File.Is_Defined then
         return Containers.Empty_Filename_Set;
      end if;

      if not GPR2.Build.Makefile_Parser.Dependencies
        (Self.Dep_File.Path, Self.Obj_File.Path, All_Deps)
      then
         Traces.Trace
           ("Failed to parse dependencies from the dependency "
            & "file " & Self.Dep_File.Path.String_Value);

         return Containers.Empty_Filename_Set;
      end if;

      return All_Deps;
   end Dependencies;

   --------------
   -- Extended --
   --------------

   overriding function Extended (Self : Object) return Object
   is
   begin
      return Result : Object do
         Result.Initialize
           (Self.Inh_From.Source (Self.Src.Path_Name.Simple_Name));
      end return;
   end Extended;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object;
      Src  : GPR2.Build.Source.Object)
   is
      View      : constant GPR2.Project.View.Object := Src.Owning_View;
      BN        : constant Simple_Name := Src.Path_Name.Base_Filename;
      O_Suff    : constant Simple_Name :=
                    Simple_Name
                      (Src.Owning_View.Attribute
                         (PRA.Compiler.Object_File_Suffix,
                          PAI.Create (Src.Language)).Value.Text);
      Lang_Idx  : constant GPR2.Project.Attribute_Index.Object :=
                    PAI.Create (Src.Language);
      Attr      : constant GPR2.Project.Attribute.Object :=
                    View.Attribute (PRA.Object_Generated, Lang_Idx);
      Cfg_Attr  : GPR2.Project.Attribute.Object;
      No_Obj    : constant Boolean :=
                    (View.Is_Library and then View.Is_Externally_Built)
                      or else (Attr.Is_Defined
                               and then Name_Type (Attr.Value.Text) = "false");
      Candidate : GPR2.Project.View.Object;
      Inh_Src   : GPR2.Build.Source.Object;
      Found     : Boolean := False;
      Local_Dep : GPR2.Path_Name.Object;
      Lkup_Dep  : GPR2.Path_Name.Object;
      Lkup_Obj  : GPR2.Path_Name.Object;
      Obj_Path  : GPR2.Path_Name.Object;
      Has_Dep   : Boolean;

   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Ctxt     := Src.Owning_View;
      Self.Src      := Src;
      Self.Lang     := Src.Language;

      Has_Dep := Self.Ctxt.Has_Attribute
        (PRA.Compiler.Dependency_Switches, Lang_Idx);

      declare
         --  We need Self.View and Self.Lang set before calling Dep_File_Suffix
         Dep_Suff  : constant Simple_Name := Self.Dep_File_Suffix;
      begin
         if not No_Obj then
            Obj_Path  := Self.View.Object_Directory.Compose (BN & O_Suff);

            if Has_Dep then
               Local_Dep := Self.View.Object_Directory.Compose (BN & Dep_Suff);
            end if;

            if not Self.Input.Is_Inherited
              or else (Obj_Path.Exists
                       and then (Local_Dep.Is_Defined
                                 and then Local_Dep.Exists))
            then
               Self.Obj_File := Artifacts.Object_File.Create (Obj_Path);
               if Has_Dep then
                  Self.Dep_File := Artifacts.Files.Create (Local_Dep);
               end if;
            else
               Candidate := Self.Input.Inherited_From;

               while not Found and then Candidate.Is_Defined loop
                  Lkup_Obj := Candidate.Object_Directory.Compose (BN & O_Suff);

                  if Lkup_Obj.Exists then
                     if Has_Dep then
                        Lkup_Dep :=
                          Candidate.Object_Directory.Compose (BN & Dep_Suff);

                        if Lkup_Dep.Exists then
                           Found := True;
                        end if;
                     else
                        Found := True;
                     end if;
                  else
                     Inh_Src :=
                       Candidate.Source (Self.Input.Path_Name.Simple_Name);

                     if Inh_Src.Is_Inherited then
                        Candidate := Inh_Src.Inherited_From;
                     else
                        Candidate := GPR2.Project.View.Undefined;
                     end if;
                  end if;
               end loop;

               --  If not found then set the value to the object generated by
               --  this action.

               if not Found then
                  Self.Obj_File := Artifacts.Object_File.Create (Obj_Path);

                  if Has_Dep then
                     Self.Dep_File := Artifacts.Files.Create (Local_Dep);
                  end if;

               else
                  Self.Obj_File := Artifacts.Object_File.Create (Lkup_Obj);
                  Self.Inh_From := Candidate;

                  if Has_Dep then
                     Self.Dep_File := Artifacts.Files.Create (Lkup_Dep);
                  end if;
               end if;
            end if;

         else
            Self.Obj_File := Artifacts.Object_File.Undefined;
            Self.Dep_File := Artifacts.Files.Undefined;
         end if;
      end;

      --  Check the configuration files

      Cfg_Attr := Self.View.Tree.Root_Project.Attribute
        (PRA.Builder.Global_Config_File, Lang_Idx);

      if Cfg_Attr.Is_Defined then
         --  Note: the Global/Local configuration pragmas attribute are
         --  expanded by the GPR parser to full names, so that they still
         --  reference the initial project dir when copied/renamed accross
         --  views.

         Self.Global_Config_File :=
           Path_Name.Create_File
             (Filename_Type (Cfg_Attr.Value.Text),
              Self.View.Tree.Root_Project.Dir_Name.Value);
      end if;

      Cfg_Attr := Self.View.Attribute
        (PRA.Compiler.Local_Config_File, Lang_Idx);

      if Cfg_Attr.Is_Defined then
         Self.Local_Config_File :=
           Path_Name.Create_File (Filename_Type (Cfg_Attr.Value.Text),
                                  Self.View.Dir_Name.Value);
      end if;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if Self.Obj_File.Is_Defined
        and then not Db.Add_Output (UID, Self.Obj_File)
      then
         return False;
      end if;

      if Self.Dep_File.Is_Defined
        and then not Db.Add_Output
          (UID, Self.Dep_File)
      then
         return False;
      end if;

      return True;
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding
   function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean
   is
      Result   : Boolean := True;
   begin
      --  If the .o and .d stored in this action were inherited, and we
      --  finally decided to compile, we need to now redirect to the new .o

      if Status = Success and then Self.Inh_From.Is_Defined then
         declare
            BN        : constant Simple_Name :=
                          Self.Src.Path_Name.Base_Filename;
            O_Suff    : constant Simple_Name :=
                          Simple_Name
                            (Self.Ctxt.Attribute
                               (PRA.Compiler.Object_File_Suffix,
                                PAI.Create (Self.Lang)).Value.Text);
            Dep_Suff  : constant Simple_Name := Self.Dep_File_Suffix;
            Local_O   : Artifacts.Object_File.Object;
            Local_Dep : Artifacts.Files.Object;

         begin
            Local_O := Artifacts.Object_File.Create
              (Self.View.Object_Directory.Compose (BN & O_Suff));
            Self.Tree.Replace_Artifact (Self.Obj_File, Local_O);
            Self.Obj_File := Local_O;

            --  Only replace the dep file artifact if we had a dep file to
            --  begin with.
            if Self.Dep_File.Is_Defined then
               Local_Dep := Artifacts.Files.Create
                 (Self.View.Object_Directory.Compose (BN & Dep_Suff));
               Self.Tree.Replace_Artifact (Self.Dep_File, Local_Dep);
               Self.Dep_File := Local_Dep;
            end if;

            Self.Inh_From := GPR2.Project.View.Undefined;
         end;
      end if;

      for Path of Self.Dependencies loop
         declare
            Src      : constant GPR2.Build.Source.Object :=
                         Self.Ctxt.Visible_Source
                           (Path_Name.Simple_Name (Path));
            Root_Dir : constant Path_Name.Object :=
                         Self.Ctxt.Tree.Root_Project.Dir_Name;
            use type GPR2.Project.View.Object;

         begin
            if Src.Is_Defined
              and then not Src.Has_Units
              and then Src.Kind = S_Spec
              and then Src.Owning_View /= Self.Ctxt
            then
               if Src.Owning_View.Has_Interfaces
                 and then not Src.Owning_View.Interface_Sources.Contains
                   (Src.Path_Name.Simple_Name)
               then
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        '"'
                        & String (Self.Src.Path_Name.Relative_Path (Root_Dir))
                        & """ cannot import """
                        & String (Src.Path_Name.Relative_Path (Root_Dir))
                        & """:" & ASCII.LF
                        & " it is not part of the interfaces of its "
                        & "project """ & String (Src.Owning_View.Name) & '"',
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));
                  Result := False;
               end if;

               if Self.Tree.Build_Options.No_Indirect_Imports
                 and then not Self.View.Imports.Contains (Src.Owning_View)
                 and then
                   not Self.View.Limited_Imports.Contains (Src.Owning_View)
               then
                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        '"'
                        & String (Self.Src.Path_Name.Relative_Path (Root_Dir))
                        & """ cannot import """
                        & String (Src.Path_Name.Relative_Path (Root_Dir))
                        & ":" & ASCII.LF
                        & " """ & String (Self.View.Name)
                        & """ does not directly import project """ &
                          String (Src.Owning_View.Name) & """",
                        GPR2.Source_Reference.Create
                          (Self.Ctxt.Path_Name.Value, 0, 0)));
                  Result := False;
               end if;
            end if;
         end;
      end loop;

      return Result;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Compile_Id :=
                 (Name_Len => Self.Src.Path_Name.Simple_Name'Length,
                  Lang     => Self.Lang,
                  Ctxt     => Self.Ctxt,
                  Src_Name => Self.Src.Path_Name.Simple_Name);
   begin
      return Result;
   end UID;
end GPR2.Build.Actions.Compile;
