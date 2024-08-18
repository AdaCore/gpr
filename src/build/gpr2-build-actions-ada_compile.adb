--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Assertions;
with Ada.Characters.Handling;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Link;
with GPR2.Build.ALI_Parser;
pragma Warnings (Off, ".* is not referenced");
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.View.Set;

package body GPR2.Build.Actions.Ada_Compile is

   package PAI renames GPR2.Project.Attribute_Index;
   package Actions renames GPR2.Build.Actions;

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name;

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      In_Lib_Dir : Boolean;
      Must_Exist : Boolean)
      return GPR2.Path_Name.Object;
   --  Look for BN in V's hierarchy of object directories

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type;

   procedure Update_Deps_From_Ali (Self : in out Object);
   --  Parse the ALI file produced by the action to update dependencies

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

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict)
   is
      procedure Add_Attr
        (Id      : Q_Attribute_Id;
         Idx     : PAI.Object;
         Is_List : Boolean);

      procedure Add_Dependency_Options;

      procedure Add_Include_Path;

      procedure Add_Mapping_File;

      Ada_Idx : constant PAI.Object := PAI.Create (Ada_Language);
      Src_Idx : constant PAI.Object :=
                  PAI.Create
                    (Value_Type (Self.CU.Main_Part.Source.Simple_Name),
                     GPR2.File_Names_Case_Sensitive,
                     Self.CU.Main_Part.Index);

      --------------
      -- Add_Attr --
      --------------

      procedure Add_Attr
        (Id      : Q_Attribute_Id;
         Idx     : PAI.Object;
         Is_List : Boolean)
      is
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute (Id, Idx);
      begin
         if not Attr.Is_Defined then
            return;
         end if;

         if Is_List then
            for Val of Attr.Values loop
               Args.Append (Val.Text);
            end loop;
         else
            Args.Append (Attr.Value.Text);
         end if;
      end Add_Attr;

      ----------------------------
      -- Add_Dependency_Options --
      ----------------------------

      procedure Add_Dependency_Options is
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute
                    (PRA.Compiler.Dependency_Switches, Ada_Idx);
      begin
         if not Attr.Is_Defined then
            return;
         end if;

         for Val of Attr.Values loop
            if Val.Text'Length > 0 then
               Args.Append (Val.Text);
            else
               Args.Append (String (Self.CU.Dependency_File));
            end if;
         end loop;
      end Add_Dependency_Options;

      ----------------------
      -- Add_Include_Path --
      ----------------------

      procedure Add_Include_Path is
         Attr : GPR2.Project.Attribute.Object;

         function Inc_Path_File return Filename_Type;
         --  Get or create a temporary include path file for the view

         -------------------
         -- Inc_Path_File --
         -------------------

         function Inc_Path_File return Filename_Type is
            use GNATCOLL.OS.FS;
            Tmp : constant Tree_Db.Temp_File :=
                    Self.Get_Or_Create_Temp_File
                      ("ada_inc_path", Actions.Global);
         begin
            if Tmp.FD /= Null_FD then
               for P of Self.View.Include_Path (Ada_Language) loop
                  Write (Tmp.FD, P.String_Value & ASCII.LF);
               end loop;

               Close (Tmp.FD);
            end if;

            return Tmp.Path;
         end Inc_Path_File;

         use type Ada.Containers.Count_Type;
      begin
         Attr := Self.View.Attribute (PRA.Compiler.Include_Path_File, Ada_Idx);

         if Attr.Is_Defined then
            declare
               Inc_File  : constant Filename_Type := Inc_Path_File;
               Full_Path : constant GPR2.Path_Name.Object :=
                             Self.View.Object_Directory.Compose (Inc_File);
            begin
               Env.Include (String (Attr.Value.Text), Full_Path.String_Value);
            end;

            return;
         end if;

         Attr :=
           Self.View.Attribute
             (PRA.Compiler.Include_Switches_Via_Spec, Ada_Idx);

         if Attr.Is_Defined then
            --  need to create a temp file with the
            --  paht, and then another temp file used as gcc spec in the form:
            --
            --  * cc1 :
            --  + @-I <tempfile>
            --
            --  Where 'cc1' is the first value in the list, and '-I' the second
            --  one.
            --
            --  Finally (and it's hardcoded in gpr1) add switch
            --  "-spec=<the spec temp file>

            declare
               Inc_File  : constant Filename_Type := Inc_Path_File;
               Full_Path : constant GPR2.Path_Name.Object :=
                             Self.View.Object_Directory.Compose (Inc_File);
               Spec_File : constant Tree_Db.Temp_File :=
                             Self.Get_Or_Create_Temp_File
                               ("ada_spec_inc_path", Global);
               use GNATCOLL.OS.FS;

            begin
               if Spec_File.FD /= Null_FD then
                  Write (Spec_File.FD, "* " & Attr.Values.First_Element.Text &
                           " :" & ASCII.LF);
                  Write (Spec_File.FD, "+@-I " & Full_Path.String_Value &
                           ASCII.LF);
                  Close (Spec_File.FD);
               end if;

               Args.Append ("-spec=" & String (Spec_File.Path));
            end;

            return;
         end if;

         Attr := Self.View.Attribute (PRA.Compiler.Include_Switches, Ada_Idx);

         if Attr.Is_Defined then
            for Path of Self.View.Include_Path (Ada_Language) loop
               if Attr.Values.Length > 1 then
                  for J in
                    Attr.Values.First_Index .. Attr.Values.Last_Index - 1
                  loop
                     Args.Append (Attr.Values.Element (J).Text);
                  end loop;
               end if;

               Args.Append
                 (Attr.Values.Last_Element.Text & Path.String_Value);
            end loop;
         end if;

         raise Ada.Assertions.Assertion_Error with
           "Cannot determine ways to transmit include path to the toolchain";
      end Add_Include_Path;

      ----------------------
      -- Add_Mapping_File --
      ----------------------

      procedure Add_Mapping_File
      is
         use GNATCOLL.OS.FS;
         Attr     : constant Project.Attribute.Object :=
                      Self.View.Attribute
                        (PRA.Compiler.Mapping_File_Switches, Ada_Idx);
      begin
         if not Attr.Is_Defined then
            --  Nothing to do
            return;
         end if;

         declare
            Map_File : constant Tree_Db.Temp_File :=
                         Self.Get_Or_Create_Temp_File ("ada_mapping", Global);
            S_Suffix : constant String :=
                         Self.View.Attribute
                           (PRA.Compiler.Mapping_Spec_Suffix,
                            Ada_Idx).Value.Text;
            B_Suffix : constant String :=
                         Self.View.Attribute
                           (PRA.Compiler.Mapping_Body_Suffix,
                            Ada_Idx).Value.Text;
            use Ada.Characters.Handling;
         begin
            if Map_File.FD /= Null_FD then
               for S of Self.View.Visible_Sources loop
                  if S.Has_Naming_Exception
                    and then S.Language = Ada_Language
                  then
                     for U of S.Units loop
                        declare
                           Key : constant String :=
                                   To_Lower (String (U.Full_Name)) &
                                   (if U.Kind = S_Spec
                                    then S_Suffix else B_Suffix);
                        begin
                           Write (Map_File.FD, Key & ASCII.LF);
                           Write (Map_File.FD,
                                  String (S.Path_Name.Simple_Name) &
                                    ASCII.LF);
                           Write (Map_File.FD,
                                  S.Path_Name.String_Value & ASCII.LF);
                        end;
                     end loop;
                  end if;
               end loop;

               --  ??? Missing the list of excluded sources

               Close (Map_File.FD);
            end if;

            for I in Attr.Values.First_Index .. Attr.Values.Last_Index - 1 loop
               Args.Append (Attr.Values.Element (I).Text);
            end loop;

            Args.Append
              (Attr.Values.Last_Element.Text &
                 String (Map_File.Path));
         end;
      end Add_Mapping_File;

   begin
      Add_Attr (PRA.Compiler.Driver, Ada_Idx, False);
      Add_Attr (PRA.Compiler.Leading_Required_Switches, Ada_Idx, True);
      --  ??? need to filter out builder switches from command line
      --  Add_Attr (PRA.Builder.Switches, Ada_Idx, True);
      Add_Attr (PRA.Compiler.Switches, Src_Idx, True);
      --  ??? TODO: command line -cargs options
      --  ??? TODO: command line -cargs:ada options

      if Self.View.Is_Library
        and then Self.View.Library_Kind /= "static"
      then
         Add_Attr (PRA.Compiler.Pic_Option, Ada_Idx, True);
      end if;

      Add_Dependency_Options;
      Add_Include_Path;
      Add_Mapping_File;

      --  ??? Replace hard coded values
      declare
         Input : constant Path_Name.Object :=
                    Self.CU.Main_Part.Source;
         Rel   : constant Filename_Type :=
                   Input.Relative_Path (Self.Working_Directory);
      begin
         if Rel'Length < Input.Value'Length then
            Args.Append (String (Rel));
         else
            Args.Append (Input.String_Value);
         end if;
      end;

      Args.Append ("-o");
      Args.Append (String (Self.Object_File.Path.Simple_Name));
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;

      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Art : Artifacts.Files.Object;
   begin
      Self.Signature.Clear;

      for Dep of Self.Dependencies loop
         Art := Artifacts.Files.Create (Dep);
         Self.Signature.Add_Artifact (Art);
      end loop;

      Self.Signature.Add_Artifact (Self.Ali_File);
      Self.Signature.Add_Artifact (Self.Obj_File);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID));
   end Compute_Signature;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self : in out Object) return GPR2.Path_Name.Set.Object
   is
   begin
      if not Self.Is_Defined then
         raise Program_Error with "Ada_Compile action is undefined";
      end if;

      if Self.Tree = null then
         raise Program_Error
           with Object'Class (Self).UID.Image & " has not been included to a" &
           " tree database. Please call the `Add_Action` procedure before" &
           " querying dependencies";
      end if;

      --  A unit has at least a dependency towards its own sources, so an
      --  an empty dependencies set means that the ALI file has not
      --  been parsed.

      if Self.Deps.Is_Empty then
         Self.Update_Deps_From_Ali;
      end if;

      return Self.Deps;
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

   begin

      Self.CU     := Src;
      Self.Traces := Create ("ACTION_ADA_COMPILE");

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
   end Initialize;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      In_Lib_Dir : Boolean;
      Must_Exist : Boolean) return GPR2.Path_Name.Object
   is
      Todo      : GPR2.Project.View.Set.Object;
      Done      : GPR2.Project.View.Set.Object;
      Current   : GPR2.Project.View.Object := V;
      Candidate : GPR2.Path_Name.Object;

   begin
      loop
         if In_Lib_Dir and then Current.Is_Library then
            Candidate := Current.Library_Ali_Directory.Compose (BN);
            exit when not Must_Exist or else Candidate.Exists;
         end if;

         if Current.Kind in With_Object_Dir_Kind then
            Candidate := Current.Object_Directory.Compose (BN);
            exit when not Must_Exist or else Candidate.Exists;
         end if;

         if Current.Is_Extending then
            Todo.Union (Current.Extended);
            Todo.Difference (Done);
         end if;

         if Todo.Is_Empty then
            return GPR2.Path_Name.Undefined;
         else
            Done.Include (Current);
            Current := Todo.First_Element;
            Todo.Delete_First;
         end if;
      end loop;

      return Candidate;
   end Lookup;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      if Self.Obj_File.Is_Defined then
         Db.Add_Output (UID, Self.Obj_File, Messages);

         if Messages.Has_Error then
            return;
         end if;
      end if;

      Db.Add_Output (UID, Self.Ali_File, Messages);

      if Messages.Has_Error then
         return;
      end if;
   end On_Tree_Insertion;

   -------------------------
   -- On_Tree_Propagation --
   -------------------------

   overriding procedure On_Tree_Propagation
     (Self : in out Object)
   is
      Imports  : GPR2.Containers.Name_Set;
      Messages : GPR2.Log.Object;
      Binds    : Action_Id_Sets.Set;
      Links    : Action_Id_Sets.Set;

   begin
      --  Check the bind actions that depend on Self

      if Self.View.Is_Externally_Built then
         return;
      end if;

      for Successor of Self.Tree.Successors (Self.Ali_File) loop
         if Successor in Actions.Ada_Bind.Object'Class then
            Binds.Insert (Successor.UID);
         end if;
      end loop;

      for Successor of Self.Tree.Successors (Self.Obj_File) loop
         if Successor in Actions.Link.Object'Class then
            Links.Insert (Successor.UID);
         end if;
      end loop;

      if Binds.Is_Empty and then Links.Is_Empty then
         --  No more things to do here
         return;
      end if;

      if Self.Ali_File.Path.Exists then
         Trace (Self.Traces,
                "Parse " & String (Self.Ali_File.Path.Simple_Name));

         GPR2.Build.ALI_Parser.Imports (Self.Ali_File.Path, Imports, Messages);

         if Messages.Has_Error then
            Messages.Output_Messages
              (Information => False,
               Warning     => False);
            --  ??? Should return a status that tells the compilation went
            --  wrong

            return;
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
            Id       : Ada_Compile_Id;
            Action   : Object;
            Messages : GPR2.Log.Object;
            Continue : Boolean := True;

         begin
            if not CU.Is_Defined then
               Trace
                  (Self.Traces,
                    "Did not find a view containing unit " &
                     String (Imp));
               Continue := False;
            end if;

            if CU.Owning_View.Is_Runtime then
               --  Since we're linking with libgnarl/libgnat anyway, don't
               --  add runtime units there.
               --  ??? Need to be generalized for library projects
               Continue := False;
            end if;

            if Continue then
               Id := Create (CU);

               if not Self.Tree.Has_Action (Id) then
                  Action.Initialize (CU);
                  Self.Tree.Add_Action (Action, Messages);

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
                     Action := Object (Self.Tree.Action (Id));

                     --  The compilation is already existing, need to be
                     --  attached to the current link/bind phases, together
                     --  with all the objects it relies upon

                     New_Actions := Action.Closure;
                     New_Actions.Include (Action.UID);

                     while not New_Actions.Is_Empty loop
                        Action :=
                          Object (Self.Tree.Action (New_Actions.Last_Element));
                        New_Actions.Delete_Last;
                        Done_Actions.Include (Action.UID);

                        for Bind of Binds loop
                           Self.Tree.Add_Input (Bind, Action.Ali_File, False);
                        end loop;

                        for Link of Links loop
                           if Action.Obj_File.Is_Defined then
                              Self.Tree.Add_Input
                                (Link, Action.Obj_File, False);
                           end if;
                        end loop;

                        Closure := Action.Closure.Difference (Done_Actions);
                        New_Actions.Union (Closure);
                     end loop;
                  end;
               end if;

               Messages.Output_Messages;
            end if;
         end;
      end loop;
   end On_Tree_Propagation;

   ------------------
   -- Post_Command --
   ------------------

   overriding procedure Post_Command (Self : in out Object) is
      use GPR2.Path_Name;

   begin
      --  Copy the ali file in the library dir if needed
      if Self.View.Is_Library
        and then not GNATCOLL.OS.FSUtil.Copy_File
          (Self.View.Object_Directory.Compose
             (Self.Ali_File.Path.Simple_Name).String_Value,
           Self.Ali_File.Path.String_Value)
      then
         pragma Assert (False, "could not copy ali to lib");
         --  ??? Should return some erroneous status to stop execution
         return;
      end if;

      --  Update the signature of the action

      Self.Update_Deps_From_Ali;

      --  Update the tree with potential new imports from ALI

      Self.On_Tree_Propagation;
   end Post_Command;

   ----------
   -- Skip --
   ----------

   overriding function Skip (Self : Object) return Boolean is
   begin
      return Self.View.Is_Externally_Built
        or else not Self.View.Attribute
          (PRA.Compiler.Driver, PAI.Create (Ada_Language)).Is_Defined;
   end Skip;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      (Ada_Compile_Id'(Main => Self.CU.Main_Part));

   --------------------------
   -- Update_Deps_From_Ali --
   --------------------------

   procedure Update_Deps_From_Ali (Self : in out Object) is
      Deps_Src : GPR2.Containers.Filename_Set;
      Messages : GPR2.Log.Object;
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      pragma Assert
        (Self.Ali_File.Path.Exists,
         "ALI file for action " & UID.Image & " does not exist");

      GPR2.Build.ALI_Parser.Dependencies
        (Self.Ali_File.Path, Deps_Src, Messages);

      for Dep_Src of Deps_Src loop
         declare
            Source : constant GPR2.Build.Source.Object :=
                       Self.View.Visible_Source
                         (Path_Name.Simple_Name (Dep_Src));
         begin
            if Source.Is_Defined then
               Trace
                 (Self.Traces,
                  "Add " & String (Source.Path_Name.Name) &
                    " to the action " & UID.Image & " dependencies");
               if not Self.Deps.Contains (Source.Path_Name) then
                  Self.Deps.Append (Source.Path_Name);
               end if;
            end if;
         end;
      end loop;
   end Update_Deps_From_Ali;

end GPR2.Build.Actions.Ada_Compile;
