--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

limited with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Path_Name; use GPR2.Path_Name;

with Ada.Containers.Hashed_Sets;

with GNATCOLL.OS.Process;

package GPR2.Build.Actions.Ada_Bind is

   type Ada_Bind_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Object with private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   procedure Initialize
     (Self           : in out Object;
      Basename       : Simple_Name;
      Context        : GPR2.Project.View.Object;
      Has_Main       : Boolean;
      SAL_In_Closure : Boolean;
      Skip           : Boolean := False);
   --  Basename: will produce b__<basename>.ad[bs]
   --  Context:  the view responsible for the bind action
   --  Has_Main: whether the binder generates a main or not
   --  Skip:     when set, this will skip the bind/post-bind stages
   --            and will consider the action valid (e.g. the signature
   --            is not checked). Used with --no-sal-binding option.

   package Path_Name_Sets is
     new Ada.Containers.Hashed_Sets
       (GPR2.Path_Name.Object, Hash => GPR2.Path_Name.Hash,
        Equivalent_Elements => GPR2.Path_Name."=");

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

   function Linker_Options
     (Self : Object) return GNATCOLL.OS.Process.Argument_List;
   --  Get the linker options generated by the binder

   function Generated_Spec (Self : Object) return Artifacts.Files.Object;
   function Generated_Body (Self : Object) return Artifacts.Files.Object;
   function Post_Bind (Self : Object) return Actions.Post_Bind.Object;

private

   No_Binder_Found : exception;

   type Ada_Bind_Id (Name_Len : Natural) is new Actions.Action_Id
     with record
      Ctxt : GPR2.Project.View.Object;
      BN   : Filename_Type (1 .. Name_Len);
   end record;

   overriding function View (Self : Ada_Bind_Id) return Project.View.Object is
     (Self.Ctxt);

   overriding function Action_Class (Self : Ada_Bind_Id) return Value_Type is
     ("Bind");

   overriding function Language (Self : Ada_Bind_Id) return Language_Id is
     (Ada_Language);

   overriding function Action_Parameter (Self : Ada_Bind_Id) return Value_Type
   is (Value_Type (Self.BN));

   type Object is new Actions.Object with record
      Basename    : Unbounded_String;
      --  Basename used to generate b__<bn>,ads and b__<bn>.adb
      Output_Spec : Artifacts.Files.Object;
      --  The generated spec, as artifact
      Output_Body : Artifacts.Files.Object;
      --  The generated body, as artifact
      Ctxt        : GPR2.Project.View.Object;
      --  View referenced by the generated compilation unit
      Has_Main    : Boolean := True;
      --  Whether the binder generates a main entry point
      SAL_Closure : Boolean := False;
      --  Whether the main depends on standalone libraries, in which case the
      --  binder needs to handle potentially duplicated elaboration
      Linker_Opts : GNATCOLL.OS.Process.Argument_List;
      --  Linker options generated by the binder in the generated body
      Skip        : Boolean := False;
   end record;

   overriding function Valid_Signature (Self : Object) return Boolean is
     (Self.Skip or else GPR2.Build.Actions.Object (Self).Valid_Signature);

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status) return Boolean;

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   overriding function Extended (Self : Object) return Object is
     (raise Internal_Error with "This action is not extending");

   function Generated_Spec (Self : Object) return Artifacts.Files.Object is
      (Self.Output_Spec);

   function Generated_Body (Self : Object) return Artifacts.Files.Object is
      (Self.Output_Body);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Linker_Options
     (Self : Object) return GNATCOLL.OS.Process.Argument_List is
     (Self.Linker_Opts);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (Self.Ctxt.Object_Directory);

end GPR2.Build.Actions.Ada_Bind;
