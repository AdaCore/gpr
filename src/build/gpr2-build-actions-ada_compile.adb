--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.ALI_Parser;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Source;
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
      pragma Unreferenced (Env);

      procedure Add_Attr
        (Id      : Q_Attribute_Id;
         Idx     : PAI.Object;
         Is_List : Boolean);

      procedure Add_Dependency_Options;

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

   begin
      Add_Attr (PRA.Compiler.Driver, Ada_Idx, False);
      Add_Attr (PRA.Compiler.Leading_Required_Switches, Ada_Idx, True);
      --  ??? TODO: add builder switches from command line
      Add_Attr (PRA.Builder.Switches, Ada_Idx, True);
      Add_Attr (PRA.Compiler.Switches, Src_Idx, True);
      --  ??? TODO: command line -cargs options
      --  ??? TODO: command line -cargs:ada options

      if Self.View.Is_Library
        and then Self.View.Library_Kind /= "static"
      then
         Add_Attr (PRA.Compiler.Pic_Option, Ada_Idx, True);
      end if;

      Add_Dependency_Options;

      --  ??? TODO: create include_path file once for each language of the view

      --  ??? Replace hard coded values
      Args.Append (Self.CU.Main_Part.Source.String_Value);
      Args.Append ("-o");
      Args.Append (String (Self.Object_File.Simple_Name));
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;

      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
      Art : Artifacts.Files.Object;
   begin
      for Dep of Self.Dependencies loop
         Art := Artifacts.Files.Create (Dep);
         Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);
      end loop;

      Art := Artifacts.Files.Create (Self.Ali_File);
      Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);

      Art := Artifacts.Files.Create (Self.Obj_File);
      Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);

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
   begin
      Self.CU     := Src;
      Self.Traces := Create ("ACTION_ADA_COMPILE");

      declare
         BN     : constant Simple_Name := Artifacts_Base_Name (Src);
         O_Suff : constant Simple_Name :=
           Simple_Name
             (Get_Attr
                (Self.View, PRA.Compiler.Object_File_Suffix, Ada_Language,
                 ".o"));
      begin

         --  Lookup if the object file exists in the hierarchy

         Self.Obj_File := Lookup (Self.View, BN & O_Suff, Must_Exist => True);

         --  If not found, set the value to the object generated by the
         --  compilation.

         if not Self.Obj_File.Is_Defined then
            Self.Obj_File :=
              Lookup (Self.View, BN & O_Suff, Must_Exist => False);
         end if;

         Self.Ali_File := Lookup (Self.View, BN & ".ali", Must_Exist => True);

         if not Self.Ali_File.Is_Defined then
            Self.Ali_File :=
              Lookup (Self.View, BN & ".ali", Must_Exist => False);
         end if;
      end;
   end Initialize;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      Must_Exist : Boolean) return GPR2.Path_Name.Object
   is
      Todo      : GPR2.Project.View.Set.Object;
      Done      : GPR2.Project.View.Set.Object;
      Current   : GPR2.Project.View.Object := V;
      Candidate : GPR2.Path_Name.Object;

   begin
      loop
         Candidate := Current.Object_Directory.Compose (BN);
         exit when not Must_Exist or else Candidate.Exists;

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
      Db.Add_Output (UID, Artifacts.Files.Create (Self.Obj_File), Messages);

      if Messages.Has_Error then
         return;
      end if;

      if Self.Ali_File.Is_Defined then
         Db.Add_Output (UID, Artifacts.Files.Create (Self.Ali_File), Messages);
      end if;

      if Messages.Has_Error then
         return;
      end if;
   end On_Tree_Insertion;

   ------------------
   -- Post_Command --
   ------------------

   overriding procedure Post_Command (Self : in out Object) is
      Bind_Action : Actions.Ada_Bind.Object;
      Found       : Boolean := False;
   begin
      Self.Update_Deps_From_Ali;

      for Successor of Self.Tree.Successors
         (Artifacts.Files.Create (Self.Ali_File))
      loop
         if Successor in Actions.Ada_Bind.Object'Class then
            Bind_Action := Actions.Ada_Bind.Object (Successor);
            Found       := True;

            exit;
         end if;
      end loop;

      if Found then
         Bind_Action.Parse_Ali (Self.Ali_File);
      end if;
   end Post_Command;

   ----------
   -- Skip --
   ----------

   overriding function Skip (Self : Object) return Boolean is
   begin
      return not Self.View.Attribute
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
      Deps_Src : GPR2.Build.ALI_Parser.Dep_Vectors.Vector;
      Messages : GPR2.Log.Object;
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;

   begin
      if not Self.Ali_File.Is_Defined or else not Self.Ali_File.Exists then
         --  ??? Use a custom exception
         raise Program_Error
           with "ALI file for action " & UID.Image & " does not exist";
      end if;

      GPR2.Build.ALI_Parser.Dependencies (Self.Ali_File, Deps_Src, Messages);

      for Dep_Src of Deps_Src loop
         declare
            Source : constant GPR2.Build.Source.Object :=
                       Self.View.Visible_Source (Simple_Name (Dep_Src));
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
