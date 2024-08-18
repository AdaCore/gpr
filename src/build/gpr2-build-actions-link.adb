--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Attribute;
--  with GPR2.Project.Attribute_Index;

package body GPR2.Build.Actions.Link is

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Self : in out Object; Option : String) is
   begin
         Self.Static_Options.Append (Option);
   end Add_Option;

   -------------
   -- Command --
   -------------

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict)
   is
      pragma Unreferenced (Env);

      Objects     : Tree_Db.Artifact_Sets.Set;

   begin
      Objects := Self.Embedded_Objects;

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Object'Class :=
                     Object'Class (Self.Tree.Action (Lib));
         begin
            Objects.Difference (Link.Embedded_Objects);
         end;
      end loop;

      --  ??? Replace hard coded values
      if Self.Is_Library then
         Args.Append ("ar");
         Args.Append ("crs");
         Args.Append (String (Self.Output.Path.Simple_Name));
      else
         Args.Append ("gcc");
         Args.Append ("-o");
         Args.Append (Self.Output.Path.String_Value);
      end if;

      for Obj of Objects loop
         Args.Append (Artifacts.Files.Object'Class (Obj).Path.String_Value);
      end loop;

      if not Self.Is_Library then
         for Lib of Self.Library_Dependencies loop
            declare
               Link : constant Object'Class :=
                        Object'Class (Self.Tree.Action (Lib));
               Opt  : constant Project.Attribute.Object :=
                        Link.View.Attribute (PRA.Linker.Linker_Options);
            begin
               Args.Append (Link.Library.Path.String_Value);

               if Opt.Is_Defined then
                  for Val of Opt.Values loop
                     Args.Append (Val.Text);
                  end loop;
               end if;
            end;
         end loop;

         for Option of Self.Static_Options loop
            Args.Append (Option);
         end loop;

         declare
            Required_Switches : constant Project.Attribute.Object :=
                                  Self.Ctxt.Attribute
                                    (PRA.Linker.Required_Switches);
         begin
            if Required_Switches.Is_Defined then
               for Switch of Required_Switches.Values loop
                  Args.Append ((Switch.Text));
               end loop;
            end if;
         end;
      end if;

   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      Self.Signature.Clear;

      for Obj of Self.Embedded_Objects loop
         Self.Signature.Add_Artifact (Obj);
      end loop;

      if not Self.Is_Library then
         --  ??? TODO dynamic libraries also need their library dependencies
         for Lib of Self.Library_Dependencies loop
            Self.Signature.Add_Artifact (Object'Class (Self).Output);
         end loop;
      end if;

      Self.Signature.Add_Artifact (Self.Output);

      Self.Signature.Store (Self.Tree.Db_Filename_Path (UID));
   end Compute_Signature;

   ----------------------
   -- Embedded_Objects --
   ----------------------

   function Embedded_Objects
     (Self : Object) return Build.Tree_Db.Artifact_Sets.Set
   is
      Result : Tree_Db.Artifact_Sets.Set;
   begin
      for Input of Self.Tree.Inputs (Self.UID) loop
         --  Inputs are either objects or libraries. Libraries are represented
         --  by an Artifact.Library class.
         if Input not in Artifacts.Library.Object'Class then
            Result.Include (Input);
         end if;
      end loop;

      return Result;
   end Embedded_Objects;

   ----------------
   -- Initialize --
   ---------------

   procedure Initialize_Executable
     (Self       : in out Object;
      Executable : GPR2.Path_Name.Object;
      Context    : GPR2.Project.View.Object) is
   begin
      Self.Is_Library := False;
      Self.Executable := Artifacts.Files.Create (Executable);
      Self.Ctxt       := Context;
      Self.Traces     := Create ("ACTION_LINK");
   end Initialize_Executable;

   ------------------------
   -- Initialize_Library --
   ------------------------

   procedure Initialize_Library
     (Self    : in out Object;
      Context : GPR2.Project.View.Object) is
   begin
      Self.Is_Library := True;
      Self.Library    := Artifacts.Library.Create (Context.Library_Filename);
      Self.Ctxt       := Context;
      Self.Traces     := Create ("ACTION_LINK");
   end Initialize_Library;

   --------------------------
   -- Library_Dependencies --
   --------------------------

   function Library_Dependencies
     (Self : Object) return Actions.Action_Id_Sets.Set
   is
      Result : Action_Id_Sets.Set;
   begin
      for Input of Self.Tree.Inputs (Self.UID) loop
         if Input in Artifacts.Library.Object'Class then
            Result.Insert (Self.Tree.Predecessor (Input).UID);
         end if;
      end loop;

      return Result;
   end Library_Dependencies;

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
      Db.Add_Output
        (UID,
         Self.Output,
         Messages);

      if Messages.Has_Error then
         return;
      end if;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name := Self.Output.Path.Simple_Name;
      Result : constant Link_Id :=
                 (Name_Len  => BN'Length,
                  Is_Lib    => Self.Is_Library,
                  View      => Self.Ctxt,
                  Exec_Name => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link;
