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
with GPR2.Project.View.Set;
with GPR2.Source_Reference;

package body GPR2.Build.Actions.Compile.Ada is

   package PAI renames GPR2.Project.Attribute_Index;
   package Actions renames GPR2.Build.Actions;

   function Artifacts_Base_Name
     (Unit : GPR2.Build.Compilation_Unit.Object) return Simple_Name;

   function Get_Attr
     (V       : GPR2.Project.View.Object;
      Name    : Q_Attribute_Id;
      Idx     : Language_Id;
      Default : Value_Type) return Value_Type;

   function Update_Deps_From_Ali (Self : in out Object) return Boolean;
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

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String)
   is
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

      Self.Signature.Add_Output (Stdout, Stderr);

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

      if Self.Deps.Is_Empty and then not Self.Update_Deps_From_Ali then
         raise Program_Error with
          "Failed to obtain dependencies from the ALI file produced by the " &
            "action " & Object'Class (Self).UID.Image;
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
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if Self.Obj_File.Is_Defined then
         if not Db.Add_Output (UID, Self.Obj_File) then
            return False;
         end if;
      end if;

      return Db.Add_Output (UID, Self.Ali_File);
   end On_Tree_Insertion;

   -------------------------
   -- On_Tree_Propagation --
   -------------------------

   overriding function On_Tree_Propagation
     (Self : in out Object) return Boolean
   is
      Imports  : GPR2.Containers.Name_Set;
      Binds    : Action_Id_Sets.Set;
      Links    : Action_Id_Sets.Set;

   begin
      --  Check the bind actions that depend on Self

      if Self.View.Is_Externally_Built then
         return True;
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
            end if;

            if CU.Owning_View.Is_Runtime then
               --  Since we're linking with libgnarl/libgnat anyway, don't
               --  add runtime units there.
               --  ??? Need to be generalized for library projects
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
                        Action := Object (Self.Tree.Action (Id));

                        --  The compilation is already existing, need to be
                        --  attached to the current link/bind phases, together
                        --  with all the objects it relies upon

                        New_Actions := Action.Closure;
                        New_Actions.Include (Action.UID);

                        while not New_Actions.Is_Empty loop
                           Action :=
                             Object
                               (Self.Tree.Action (New_Actions.Last_Element));
                           New_Actions.Delete_Last;
                           Done_Actions.Include (Action.UID);

                           for Bind of Binds loop
                              Self.Tree.Add_Input
                                (Bind, Action.Ali_File, False);
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
      --  Copy the ali file in the library dir if needed
      if Status /= Skipped
        and then Self.View.Is_Library
        and then not GNATCOLL.OS.FSUtil.Copy_File
          (Self.View.Object_Directory.Compose
             (Self.Ali_File.Path.Simple_Name).String_Value,
           Self.Ali_File.Path.String_Value,
           Preserve_Timestamps => False,
           Preserve_Permissions => False)
      then
         Self.Tree.Report
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

      --  Update the signature of the action

      if not Self.Update_Deps_From_Ali then
         Trace
           (Self.Traces,
            "Failed to obtain dependencies from the ALI file produced " &
              "by the action " & Object'Class (Self).UID.Image);

         return False;
      end if;

      --  Update the tree with potential new imports from ALI

      return Self.On_Tree_Propagation;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      (Ada.Create (Src => Self.CU));

   --------------------------
   -- Update_Deps_From_Ali --
   --------------------------

   function Update_Deps_From_Ali (Self : in out Object) return Boolean is
      Deps_Src : GPR2.Containers.Filename_Set;
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin

      if not Self.Ali_File.Path.Exists then
         Trace
           (Self.Traces,
            "The ALI file for action " & UID.Image & " does not exist");

         return False;
      end if;

      if not GPR2.Build.ALI_Parser.Dependencies (Self.Ali_File.Path, Deps_Src)
      then
         Trace
           (Self.Traces, "Failed to parse dependencies from the ALI file " &
            Self.Ali_File.Path.String_Value);

         return False;
      end if;

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

      return True;
   end Update_Deps_From_Ali;

end GPR2.Build.Actions.Compile.Ada;
