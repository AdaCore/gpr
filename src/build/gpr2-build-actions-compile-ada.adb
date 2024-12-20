--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FSUtil;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Link;
with GPR2.Build.ALI_Parser;
with GPR2.Build.Tree_Db;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;

package body GPR2.Build.Actions.Compile.Ada is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;
   package Actions renames GPR2.Build.Actions;

   type Reference_Type (Element : not null access Object) is record
      Ref : Tree_Db.Action_Reference_Type (Element);
   end record
     with Implicit_Dereference => Element;

   function Reference
     (Self : Object;
      Id   : Action_Id'Class) return Reference_Type;

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name;

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type;

   -------------------------
   -- Artifacts_Base_Name --
   -------------------------

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name
   is
      Main : constant Compilation_Unit.Unit_Location := Unit.Main_Part;
      BN   : constant Simple_Name := Simple_Name (Main.Source.Base_Name);

   begin
      if Main.Index = No_Index then
         return BN;
      else
         declare
            Img : constant String := Main.Index'Image;
            Sep : constant String :=
              Get_Attr
                (Main.View, PRA.Compiler.Multi_Unit_Object_Separator,
                 Ada_Language, "~");
         begin
            return BN & Simple_Name (Sep & Img (Img'First + 1 .. Img'Last));
         end;
      end if;
   end Artifacts_Base_Name;

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      Attr : GPR2.Project.Attribute.Object;
   begin
      Compile.Object (Self).Compute_Command (Slot, Cmd_Line);

      if Self.Global_Config_Pragmas.Is_Defined
        or else Self.Local_Config_Pragmas.Is_Defined
      then
         Attr := Self.View.Attribute
           (PRA.Compiler.Config_File_Switches, PAI.Create (Ada_Language));
      end if;

      if Self.Global_Config_Pragmas.Is_Defined then
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, False);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text &
              Self.Global_Config_Pragmas.Path.String_Value,
            False);
      end if;

      if Self.Local_Config_Pragmas.Is_Defined then
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
            Cmd_Line.Add_Argument
              (Attr.Values.Element (J).Text, False);
         end loop;

         Cmd_Line.Add_Argument
           (Attr.Values.Last_Element.Text &
              Self.Local_Config_Pragmas.Path.String_Value,
            False);
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : Object;
      Signature : in out GPR2.Build.Signature.Object)
   is
      Art : Artifacts.Files.Object;

   begin
      for Dep of Self.Dependencies loop
         Art := Artifacts.Files.Create (Dep);
         Signature.Add_Artifact (Art);
      end loop;

      Signature.Add_Artifact (Self.Ali_File);
      Signature.Add_Artifact (Self.Obj_File);

      if Self.Global_Config_Pragmas.Is_Defined then
         Signature.Add_Artifact (Self.Global_Config_Pragmas);
      end if;

      if Self.Local_Config_Pragmas.Is_Defined then
         Signature.Add_Artifact (Self.Local_Config_Pragmas);
      end if;
   end Compute_Signature;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self     : Object;
      With_RTS : Boolean := True) return Containers.Filename_Set
   is
      All_Deps : GPR2.Containers.Filename_Set;
      Result   : GPR2.Containers.Filename_Set;
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      if not Self.Ali_File.Path.Exists then
         Trace
           (Self.Traces,
            "The ALI file for action " & UID.Image & " does not exist");

         return Containers.Empty_Filename_Set;
      end if;

      if not GPR2.Build.ALI_Parser.Dependencies
        (Self.Ali_File.Path, All_Deps)
      then
         Trace
           (Self.Traces, "Failed to parse dependencies from the ALI file " &
              Self.Ali_File.Path.String_Value);

         return Containers.Empty_Filename_Set;
      end if;

      for Dep_Src of All_Deps loop
         declare
            Source : constant GPR2.Build.Source.Object :=
                       Self.View.Visible_Source
                         (Path_Name.Simple_Name (Dep_Src));
         begin
            if Source.Is_Defined
              and then
                (With_RTS
                 or else not Source.Owning_View.Is_Runtime)
            then
               if Self.Traces.Is_Active then
                  Self.Traces.Trace
                    ("Add " & String (Source.Path_Name.Name) &
                       " to the action " & UID.Image & " dependencies");
               end if;

               Result.Include (Source.Path_Name.Value);
            end if;
         end;
      end loop;

      return Result;
   end Dependencies;

   --------------
   -- Get_Attr --
   --------------

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type
   is
      Attr : constant GPR2.Project.Attribute.Object :=
               V.Attribute (Name, PAI.Create (Idx));
   begin
      if Attr.Is_Defined then
         return Attr.Value.Text;
      else
         return Default;
      end if;
   end Get_Attr;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object)
   is
      View   : constant GPR2.Project.View.Object := Src.Owning_View;
      No_Obj : constant Boolean :=
                 (View.Is_Library and then View.Is_Externally_Built)
                   or else View.Is_Runtime;
      Attr   : GPR2.Project.Attribute.Object;

   begin

      Self.Ctxt     := Src.Owning_View;
      Self.Src_Name := Src.Main_Part.Source;
      Self.Lang     := Ada_Language;
      Self.CU       := Src;
      Self.Traces   := Create ("ACTION_ADA_COMPILE");

      declare
         BN        : constant Simple_Name := Artifacts_Base_Name (Src);
         O_Suff    : constant Simple_Name :=
                       Simple_Name
                         (Get_Attr
                            (Self.View, PRA.Compiler.Object_File_Suffix,
                             Ada_Language,
                             ".o"));
         Local_O   : GPR2.Path_Name.Object;
         Local_Ali : GPR2.Path_Name.Object;
         Lkup_O    : GPR2.Path_Name.Object;
         Lkup_Ali  : GPR2.Path_Name.Object;
         Check_Sig : Boolean := True;
      begin
         if not No_Obj then
            Local_O := Self.View.Object_Directory.Compose (BN & O_Suff);
         else
            Self.Obj_File := Artifacts.Files.Undefined;
         end if;

         if Self.View.Is_Library then
            Local_Ali := Self.View.Library_Ali_Directory.Compose (BN & ".ali");
         else
            Local_Ali := Self.View.Object_Directory.Compose (BN & ".ali");
         end if;

         if not Self.View.Is_Extending then
            --  Simple case: just use the local .o and .ali
            if not No_Obj then
               Self.Obj_File := Artifacts.Files.Create (Local_O);
            end if;

            Self.Ali_File := Artifacts.Files.Create (Local_Ali);

         else
            --  Lookup if the object file exists in the hierarchy

            Lkup_O := Lookup
              (Self.View,
               BN & O_Suff,
               In_Lib_Dir => False,
               Must_Exist => True);

            --  If not found, set the value to the object generated by the
            --  compilation.

            if not Lkup_O.Is_Defined then
               Self.Obj_File := Artifacts.Files.Create (Local_O);
               Check_Sig := False;
            else
               Self.Obj_File := Artifacts.Files.Create (Lkup_O);
            end if;

            Lkup_Ali := Lookup
              (Self.View,
               BN & ".ali",
               In_Lib_Dir => True,
               Must_Exist => True);

            if not Lkup_Ali.Is_Defined then
               Self.Ali_File := Artifacts.Files.Create (Local_Ali);
               Check_Sig := False;
            else
               Self.Ali_File := Artifacts.Files.Create (Lkup_Ali);
            end if;

            if Check_Sig and then not Self.Valid_Signature then
               --  Since we'll need to recompute, make sure we use any local
               --  .ali and .o here.
               Self.Obj_File := Artifacts.Files.Create (Local_O);
               Self.Ali_File := Artifacts.Files.Create (Local_Ali);
            end if;
         end if;
      end;

      --  Check the configuration pragmas files

      Attr := Self.View.Tree.Root_Project.Attribute
        (PRA.Builder.Global_Configuration_Pragmas);

      if Attr.Is_Defined then
         --  Note: the Global/Local configuration pragmas attribute are
         --  expanded by the GPR parser to full names, so that they still
         --  reference the initial project dir when copied/renamed accross
         --  views.

         Self.Global_Config_Pragmas :=
           Artifacts.Files.Create (Filename_Type (Attr.Value.Text));
      end if;

      Attr := Self.View.Attribute (PRA.Compiler.Local_Configuration_Pragmas);

      if Attr.Is_Defined then
         Self.Local_Config_Pragmas :=
           Artifacts.Files.Create (Filename_Type (Attr.Value.Text));
      end if;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID       : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Other_Ali : Artifacts.Files.Object;
   begin
      if Self.Obj_File.Is_Defined then
         if not Db.Add_Output (UID, Self.Obj_File) then
            return False;
         end if;
      end if;

      if not Db.Add_Output (UID, Self.Ali_File) then
         return False;
      end if;

      if Self.View.Kind = K_Library
        and then not Self.View.Is_Externally_Built
      then
         --  Also add the .ali file created in the obj directory so that
         --  gprclean has visibility over it

         Other_Ali := Artifacts.Files.Create
           (Self.View.Object_Directory.Compose
              (Self.Ali_File.Path.Simple_Name));

         if not Db.Add_Output (UID, Other_Ali) then
            return False;
         end if;
      end if;

      return True;
   end On_Tree_Insertion;

   -------------------------
   -- On_Tree_Propagation --
   -------------------------

   overriding function On_Tree_Propagation
     (Self : in out Object) return Boolean
   is
      Imports : GPR2.Containers.Name_Set;
      Binds   : Action_Id_Sets.Set;
      Links   : Action_Id_Sets.Set;

      function Can_Unit_Be_Imported
        (CU : GPR2.Build.Compilation_Unit.Object) return Boolean;
      --  Ensure that a unit coming from another project can be imported.
      --  It is the case, if the unit is part of the Interfaces or the
      --  Library_Interface attributes. Otherwise, this units is reserved for
      --  internal project implementation.

      --------------------------
      -- Can_Unit_Be_Imported --
      --------------------------

      function Can_Unit_Be_Imported
        (CU : GPR2.Build.Compilation_Unit.Object) return Boolean
      is
         use GPR2.Project.View;

         CU_View : constant GPR2.Project.View.Object := CU.Owning_View;
         Allowed : Boolean := True;
      begin
         --  There is no restriction of units visibility inside the same view

         if Self.View /= CU_View then
            if CU_View.Is_Library and then CU_View.Has_Library_Interface
              and then not CU_View.Interface_Units.Contains (CU.Name)
            then
               --  If the unit is not listed by the Library_Interface
               --  attribute, it can not be imported.

               Allowed := False;
            elsif CU_View.Has_Interfaces
              and then not CU_View.Interface_Sources.Contains
                             (CU.Spec.Source.Simple_Name)
            then
               --  Similarly, if the unit source is not listed by the
               --  Interfaces attribute, then it can not be imported.

               Allowed := False;
            end if;

            if not Allowed then
               Self.Tree.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Error,
                     "unit """
                     & String (Self.CU.Name)
                     & """ can not import unit """
                     & String (CU.Name)
                     & """: it is not part of the interfaces of the project "
                     & String (CU_View.Name),
                     GPR2.Source_Reference.Object
                       (GPR2.Source_Reference.Create
                          (Self.Src_Name.Value, 0, 0))));
            end if;

            if Allowed then
               if Self.Tree.Build_Options.No_Indirect_Imports
                 and then not Self.View.Imports.Contains (CU_View)
                 and then not Self.View.Limited_Imports.Contains (CU_View)
               then
                  Allowed := False;

                  Self.Tree.Reporter.Report
                    (GPR2.Message.Create
                       (GPR2.Message.Error,
                        "unit """ & String (Self.CU.Name) &
                          """ cannot import unit """ &
                          String (CU.Name) & ":" &
                          ASCII.LF &
                          """" & String (Self.View.Name) &
                          """ does not directly import project """ &
                          String (CU_View.Name) & """",
                        GPR2.Source_Reference.Create
                          (Self.Src_Name.Value, 0, 0)));
               end if;
            end if;
         end if;

         return Allowed;
      end Can_Unit_Be_Imported;

   begin
      for Successor of Self.Tree.Successors (Self.Ali_File) loop
         if Successor in Actions.Ada_Bind.Object'Class then
            Binds.Insert (Successor.UID);
         end if;
      end loop;

      if Self.Obj_File.Is_Defined then
         for Successor of Self.Tree.Successors (Self.Obj_File) loop
            if Successor in Actions.Link.Object'Class then
               Links.Insert (Successor.UID);
            end if;
         end loop;
      end if;

      if Binds.Is_Empty and then Links.Is_Empty then
         --  No more things to do here
         return True;
      end if;

      if Self.Ali_File.Path.Exists then
         Trace (Self.Traces,
                "Parse " & String (Self.Ali_File.Path.Simple_Name));

         if not GPR2.Build.ALI_Parser.Imports (Self.Ali_File.Path, Imports)
         then
            Imports := Self.CU.Known_Dependencies;
         end if;
      else
         Imports := Self.CU.Known_Dependencies;
      end if;

      for Imp of Imports loop
         Trace (Self.Traces, "Found import " & String (Imp));

         --  Lookup the corresponding unit

         declare
            CU       : constant Build.Compilation_Unit.Object :=
                         Self.View.Namespace_Roots.First_Element.Unit (Imp);
            Action   : Object;
            Continue : Boolean := True;

         begin
            if not CU.Is_Defined then
               Trace
                  (Self.Traces,
                    "Did not find a view containing unit " &
                     String (Imp));
               Continue := False;

               --  ??? if the CU comes from ALI file, we should fallback to
               --  getting the dependencies from the source
            end if;

            if Continue and then not Can_Unit_Be_Imported (CU) then
               Self.Signature.Invalidate;
               return False;
            end if;

            if Continue and then CU.Owning_View.Is_Runtime then
               --  Since we're linking with libgnarl/libgnat anyway, don't
               --  add runtime units there.
               Continue := False;
            end if;

            if Continue then
               declare
                  Id : constant Ada_Compile_Id := Create (CU);
               begin
                  if not Self.Tree.Has_Action (Id) then
                     Action.Initialize (CU);
                     if not Self.Tree.Add_Action (Action) then
                        return False;
                     end if;

                     Self.Closure.Include (Action.UID);

                     for Bind of Binds loop
                        Self.Tree.Add_Input
                          (Bind, Action.Ali_File, False);
                     end loop;

                     for Link of Links loop
                        if Action.Obj_File.Is_Defined then
                           Self.Tree.Add_Input (Link, Action.Obj_File, False);
                        end if;
                     end loop;
                  else
                     declare
                        Closure      : Action_Id_Sets.Set;
                        New_Actions  : Action_Id_Sets.Set;
                        Done_Actions : Action_Id_Sets.Set;

                     begin
                        --  The compilation is already existing, need to be
                        --  attached to the current link/bind phases, together
                        --  with all the objects it relies upon

                        New_Actions := Self.Reference (Id).Closure;
                        New_Actions.Include (Id);

                        while not New_Actions.Is_Empty loop
                           declare
                              Ref : constant Reference_Type :=
                                      Self.Reference
                                        (New_Actions.Last_Element);
                              Added : Boolean := False;
                           begin
                              New_Actions.Delete_Last;
                              Done_Actions.Include (Ref.UID);

                              for Bind of Binds loop
                                 Self.Tree.Add_Input
                                   (Bind, Ref.Ali_File, False);
                                 Added := True;
                              end loop;

                              for Link of Links loop
                                 if Ref.Obj_File.Is_Defined then
                                    Self.Tree.Add_Input
                                      (Link, Ref.Obj_File, False);
                                    Added := True;
                                 end if;
                              end loop;

                              if Added then
                                 Closure :=
                                   Ref.Closure.Difference (Done_Actions);
                                 New_Actions.Union (Closure);
                              end if;
                           end;
                        end loop;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      return True;
   end On_Tree_Propagation;

   ------------------
   -- Post_Command --
   ------------------

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean
   is
      use GPR2.Path_Name;

   begin
      if Status = Skipped then
         --  No need to post-process anything if the action was skipped
         return True;
      end if;

      --  Copy the ali file in the library dir if needed
      if Self.View.Is_Library
        and then not GNATCOLL.OS.FSUtil.Copy_File
          (Self.View.Object_Directory.Compose
             (Self.Ali_File.Path.Simple_Name).String_Value,
           Self.Ali_File.Path.String_Value)
      then
         Self.Tree.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "could not copy ali file " &
                 String (Self.Ali_File.Path.Simple_Name) &
                 " to the library directory",
               GPR2.Source_Reference.Object
                 (GPR2.Source_Reference.Create
                    (Self.Ali_File.Path.Value, 0, 0))));
         return False;
      end if;

      --  Update the tree with potential new imports from ALI

      return Self.On_Tree_Propagation;
   end Post_Command;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self : Object;
      Id   : Action_Id'Class) return Reference_Type
   is
      Ref : constant GPR2.Build.Tree_Db.Action_Reference_Type :=
              Self.Tree.Action_Id_To_Reference (Id);
   begin
      return (Element => Object (Ref.Element.all)'Unchecked_Access,
              Ref     => Ref);
   end Reference;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      (Ada.Create (Src => Self.CU));

end GPR2.Build.Actions.Compile.Ada;
