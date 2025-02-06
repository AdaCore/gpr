--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;

with GNATCOLL.OS.FS;

pragma Warnings (Off, ".* is not referenced");
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");
with GPR2.Build.Tree_Db;
with GPR2.Build.External_Options;
with GPR2.Project.Attribute;

package body GPR2.Build.Actions.Compile is

   function Lang_Img (Lang : Language_Id) return Filename_Type is
      (Filename_Type (Ada.Characters.Handling.To_Lower (GPR2.Image (Lang))));

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
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
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute (Id, Idx);
      begin
         if not Attr.Is_Defined then
            return;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               if Val.Text'Length > 0 then
                  Cmd_Line.Add_Argument (Val.Text, In_Signature);
               end if;
            end loop;
         else
            Cmd_Line.Add_Argument (Attr.Value.Text, In_Signature);
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

               Write (FD, Cnt (Last .. Cnt'Last));
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

               Check_Exceptions_For (Self.View, File.FD);

               if Self.View.Is_Extending then
                  for V of Self.View.Extended loop
                     Check_Exceptions_For (V, File.FD);
                  end loop;
               end if;

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
                  False);
            end;

            return;
         end if;

         Attr := Self.View.Attribute (PRA.Compiler.Include_Switches, Lang_Idx);

         if Attr.Is_Defined then
            for Path of Self.View.Include_Path (Self.Lang) loop
               Add_Options_With_Arg
                 (Attr, Path.String_Value, True);
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
      begin
         if not Attr.Is_Defined or else Attr.Values.Is_Empty then
            return;
         end if;

         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, In_Signature);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text & Arg, In_Signature);
      end Add_Options_With_Arg;

      Driver_Attr : constant GPR2.Project.Attribute.Object :=
                      Self.Ctxt.Attribute (PRA.Compiler.Driver, Lang_Idx);

   begin
      if Driver_Attr.Is_Defined then
         Cmd_Line.Set_Driver
           (Driver_Attr.Value.Text);
      else
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
         Cmd_Line.Add_Argument (Arg, True);
      end loop;

      if Self.View.Is_Library
        and then Self.View.Library_Kind /= "static"
      then
         Add_Attr (PRA.Compiler.Pic_Option, Lang_Idx, True, True);
      end if;

      declare
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute
                    (PRA.Compiler.Dependency_Switches, Lang_Idx);
      begin
         if Attr.Is_Defined then
            Add_Options_With_Arg
              (Attr,
               String (Object'Class (Self).Dependency_File),
               True);
         end if;
      end;

      Add_Include_Path;
      Add_Mapping_File;
      Add_Config_File;

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
            Cmd_Line.Add_Argument (Self.Src.Path_Name, True);
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
            elsif Self.Lang = Ada_Language then
               --  [eng/gpr/gpr-issues#446] TODO modify the KB to have a proper
               --  default here.
               Cmd_Line.Add_Argument ("-o");
               Cmd_Line.Add_Argument
                 (String (Self.Obj_File.Path.Simple_Name));
            end if;
         end;
      end if;

   exception
      when GNATCOLL.OS.OS_Error =>
         Self.Tree.Reporter.Report
           ("Problem accessing the object directory for project """ &
              String (Self.View.Name) & '"');
         raise Action_Error;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : Object;
      Signature : in out GPR2.Build.Signature.Object)
   is
      use GPR2.Build.Signature;
      Art : Artifacts.Files.Object;
   begin
      if Self.Obj_File.Is_Defined then
         --  ??? Need to process deps units

         Art := Artifacts.Files.Create (Self.Input.Path_Name);
         Signature.Add_Artifact (Art);

         Art := Self.Obj_File;
         Signature.Add_Artifact (Art);
      else
         --  In case we have Object_Generated unset for the language, then
         --  we don't save the signature so that the action is replayed.
         Signature.Clear;
      end if;
   end Compute_Signature;

   --------------
   -- Extended --
   --------------

   overriding function Extended (Self : Object) return Object
   is
   begin
      return Result : Object do
         Result.Initialize
           (Self.Src.Inherited_From.Source (Self.Src.Path_Name.Simple_Name));
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
      Attr      : constant GPR2.Project.Attribute.Object :=
                    View.Attribute (PRA.Object_Generated,
                                    PAI.Create (Src.Language));
      No_Obj    : constant Boolean :=
                    (View.Is_Library and then View.Is_Externally_Built)
                      or else (Attr.Is_Defined
                               and then Name_Type (Attr.Value.Text) = "false");
      Candidate : GPR2.Project.View.Object;
      Inh_Src   : GPR2.Build.Source.Object;
      Found     : Boolean := False;
      Lkup_Obj  : GPR2.Path_Name.Object;
      Obj_Path  : GPR2.Path_Name.Object;

   begin
      Self.Ctxt     := Src.Owning_View;
      Self.Src      := Src;
      Self.Lang     := Src.Language;
      Self.Traces   := Create ("ACTION_COMPILE");

      if not No_Obj then
         Obj_Path := Self.View.Object_Directory.Compose (BN & O_Suff);

         if not Self.Input.Is_Inherited
           or else Obj_Path.Exists
         then
            Self.Obj_File := Artifacts.Files.Create (Obj_Path, Self.Ctxt);

         else
            Candidate := Self.Input.Inherited_From;

            while not Found and then Candidate.Is_Defined loop
               Lkup_Obj := Candidate.Object_Directory.Compose (BN & O_Suff);

               if Lkup_Obj.Exists then
                  Found := True;
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
               Self.Obj_File := Artifacts.Files.Create (Obj_Path, Self.Ctxt);
            else
               Self.Obj_File := Artifacts.Files.Create (Lkup_Obj, Self.Ctxt);
            end if;
         end if;

      else
         Self.Obj_File := Artifacts.Files.Undefined;
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

      if Object'Class (Self).Dependency_File'Length > 0
        and then not Db.Add_Output
          (UID,
           Artifacts.Files.Create
             (Self.Ctxt.Object_Directory.Compose
                (Object'Class (Self).Dependency_File),
              Self.Ctxt))
      then
         return False;
      end if;

      return True;
   end On_Tree_Insertion;

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
