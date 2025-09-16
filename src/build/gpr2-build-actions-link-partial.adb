--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.OS_Lib;

with GPR2.Project;
with GPR2.Project.Attribute;

package body GPR2.Build.Actions.Link.Partial is

   ----------------
   -- Add_Option --
   ----------------

   overriding procedure Add_Option (Self : in out Object; Option : String) is
   begin
      if Self.Is_Encapsulated
        and then GNAT.OS_Lib.Is_Regular_File (Option)
      then
         Self.Static_Options.Append (Option);
      end if;
   end Add_Option;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot);
      use type GPR2.Project.Standalone_Library_Kind;

      Objects : Tree_Db.Artifact_Sets.Set;
   begin
      Objects := Self.Embedded_Objects;

      --  Remove from this list of objects the ones that come from
      --  libraries.

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Actions.Link.Object'Class :=
                     Actions.Link.Object'Class (Self.Tree.Action (Lib));
         begin
            Objects.Difference (Link.Embedded_Objects);
         end;
      end loop;

      Add_Library_Partial_Linker_Attribute : declare
         Attr  : constant GPR2.Project.Attribute.Object :=
                   Self.View.Attribute (PRA.Library_Partial_Linker);
         First : Boolean := True;
         use type Ada.Containers.Count_Type;
      begin
         if Attr.Is_Defined then
            if Attr.Values.Length > 0
              and then Attr.Values.First_Element.Text /= ""
            then
               for Value of Attr.Values loop
                  if First then
                     Cmd_Line.Set_Driver (Locate_Exec_On_Path (Value.Text));
                     First := False;
                  else
                     Cmd_Line.Add_Argument (Value.Text);
                  end if;
               end loop;
            end if;
         end if;
      end Add_Library_Partial_Linker_Attribute;

      Cmd_Line.Add_Argument (Self.Partial_Object.Path);

      for Obj of Objects loop
         declare
            Arg : constant String :=
                    Artifacts.Files.Object'Class
                      (Obj).Path.String_Value;
         begin
            Cmd_Line.Add_Argument (Arg, Kind => Build.Command_Line.Obj);
         end;
      end loop;

      --  If the library is encapsulated
      if Self.Is_Encapsulated then

         --  Add all internal library dependencies to the partial link
         for Lib of Self.Library_Dependencies loop
            declare
               Link         : constant Actions.Link.Object'Class :=
                                Actions.Link.Object'Class
                                  (Self.Tree.Action (Lib));
               Lib_Artifact : constant GPR2.Path_Name.Object :=
                                Link.Output.Path;
            begin
               Cmd_Line.Add_Argument (Lib_Artifact.String_Value);
            end;
         end loop;

         --  Add all static options given by the binder (libgnat / libgnarl)
         for Opt of Self.Static_Options loop
            Cmd_Line.Add_Argument (Opt);
         end loop;
      end if;

      if not Signature_Only then
         if Self.View.Library_Standalone /= GPR2.Project.No then
            --  Don't issue warnings at this stage since they would be
            --  duplicated when the main link occurs.
            Self.Handle_Export_File
              (Cmd_Line, Signature_Only, No_Warning => True);
         end if;
      end if;

      if Self.Mapping_File /= Null_Unbounded_String then
         Self.Add_Mapping_File_To_Cmd_Line (Cmd_Line);
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      for Obj of Self.Embedded_Objects loop
         if not Self.Signature.Add_Input (Obj, Check_Checksums) then
            return;
         end if;
      end loop;

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Actions.Link.Object'Class :=
              Actions.Link.Object'Class (Self.Tree.Action (Lib));
         begin
            if not Self.Signature.Add_Input (Link.Output, Check_Checksums)
            then
               return;
            end if;
         end;
      end loop;

      if not Self.Signature.Add_Output (Self.Output, Check_Checksums)
      then
         return;
      end if;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Object;
      Context : GPR2.Project.View.Object)
   is
      use type GPR2.Project.Standalone_Library_Kind;
   begin
      --  Ensure the object wasn't previously initialized prior to this call
      Self := Undefined;

      Self.Partial_Object :=
        Artifacts.Object_File.Create
          (Context.Object_Directory.Compose
             (Filename_Type
                ("p__" & Context.Library_Name & ".o")));
      Self.Is_Encapsulated :=
        (Context.Library_Standalone = GPR2.Project.Encapsulated);
      Self.Initialize
        (Kind => Library, Context => Context, No_Rpath => True);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      if not Db.Add_Output (UID, Self.Output) then
         return False;
      end if;

      return True;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      BN     : constant Simple_Name     := Self.Output.Path.Simple_Name;
      Result : constant Partial_Link_Id :=
                 (Name_Len          => BN'Length,
                  Is_Static_Lib     => Self.Is_Library and then Self.Is_Static,
                  View              => Self.Ctxt,
                  Exec_Name         => BN);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link.Partial;
