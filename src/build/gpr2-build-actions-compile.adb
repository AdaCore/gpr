--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.View.Set;

package body GPR2.Build.Actions.Compile is

   package PAI renames GPR2.Project.Attribute_Index;

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      Must_Exist : Boolean) return GPR2.Path_Name.Object;
   --  Look for the basename BN in V's hierarchy of object/lib directories

   -------------
   -- Command --
   -------------

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict)
   is
      pragma Unreferenced (Env);
   begin
      --  ??? Replace hard coded values

      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append
        (String
           (Self.Ctxt.Source
              (Simple_Name (To_String (Self.Src_Name))).Path_Name.Value));
      --  ??? Check that path_name has a value.
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;
      Art : Artifacts.Files.Object;
   begin
      Self.Signature.Clear;

      --  ??? Need to process deps units

      Art := Artifacts.Files.Create (Self.Input.Path_Name);
      Self.Signature.Add_Artifact (Art);

      Art := Artifacts.Files.Create (Self.Obj_File);
      Self.Signature.Add_Artifact (Art);

      Self.Signature.Store
        (Self.Tree.Db_Filename_Path (Object'Class (Self).UID));
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Object; Src : GPR2.Build.Source.Object)
   is
      Src_Name : constant Simple_Name :=
                   Src.Path_Name.Simple_Name;

      BN       : constant Simple_Name := Src.Path_Name.Base_Filename;
      O_Suff   : constant Simple_Name :=
                   Simple_Name
                     (Src.Owning_View.Attribute
                        (PRA.Compiler.Object_File_Suffix,
                         PAI.Create (Src.Language)).Value.Text);
   begin
      Self.Ctxt     := Src.Owning_View;
      Self.Src_Name := To_Unbounded_String (String (Src_Name));
      Self.Lang     := Src.Language;
      Self.Traces   := Create ("ACTION_COMPILE");

      Self.Obj_File := Lookup (Self.Ctxt, BN & O_Suff, True);

      if not Self.Obj_File.Is_Defined then
         Self.Obj_File := Self.Ctxt.Object_Directory.Compose (BN & O_Suff);
      end if;
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
     (Self     : in out Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
      UID      : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      Db.Add_Output (UID, Artifacts.Files.Create (Self.Obj_File), Messages);

      if Messages.Has_Error then
         return;
      end if;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Compile_Id :=
                 (Name_Len => Ada.Strings.Unbounded.Length (Self.Src_Name),
                  Lang     => Self.Lang,
                  Ctxt     => Self.Ctxt,
                  Src_Name => Filename_Optional (To_String (Self.Src_Name)));
   begin
      return Result;
   end UID;
end GPR2.Build.Actions.Compile;
