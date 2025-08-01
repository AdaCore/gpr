--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.OS_Lib;

with GPR2.Build.Response_Files;
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

      --  Add the possible object files given in the Library_Options attribute
      declare
         Attr : constant GPR2.Project.Attribute.Object :=
                  Self.View.Attribute (PRA.Library_Options);
      begin
         if Attr.Is_Defined then
            for Val of Attr.Values loop
               declare
                  Path : constant Path_Name.Object :=
                           Path_Name.Create_File
                             (Filename_Type (Val.Text),
                              Self.View.Object_Directory.Value);
               begin
                  if Path.Exists then
                     Cmd_Line.Add_Argument
                       (Val.Text, Kind => Build.Command_Line.Obj);
                  end if;
               end;
            end loop;
         end if;
      end;

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

            A_RFF  : constant Project.Attribute.Object :=
                       Self.View.Attribute (PRA.Linker.Response_File_Format);
            A_RFS  : constant Project.Attribute.Object :=
                       Self.View.Attribute (PRA.Linker.Response_File_Switches);
            RFS    : constant Containers.Source_Value_List :=
                       (if A_RFS.Is_Defined
                        then A_RFS.Values
                        else Containers.Empty_Source_Value_List);
            A_CLML : constant Project.Attribute.Object :=
                       Self.View.Attribute
                         (PRA.Linker.Max_Command_Line_Length);
            CLML   : constant Natural :=
                       (if A_CLML.Is_Defined
                        then Natural'Value (A_CLML.Value.Text)
                        else 0);
            Format : Response_File_Format := None;
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

            Self.Response_Files.Initialize (Format, Linker, CLML, RFS);

            if Self.Response_Files.Length_Restriction (Cmd_Line) then
               declare
                  Needs_Formating : constant Boolean :=
                                      Format in GCC_Formatting_Required;
               begin
                  if Needs_Formating then
                     declare
                        Resp_File : constant Tree_Db.Temp_File :=
                                      Self.Get_Or_Create_Temp_File
                                        ("response_file", Local);
                     begin
                        Self.Response_Files.Register
                          (Resp_File.FD,
                           Resp_File.Path,
                           Secondary => True);
                     end;
                  end if;

                  declare
                     RF_Name   : constant Filename_Type :=
                                   (if Needs_Formating
                                    then "encapsulated_"
                                    else "") & "response_file";
                     Resp_File : constant Tree_Db.Temp_File :=
                                   Self.Get_Or_Create_Temp_File
                                     (RF_Name, Local);
                  begin
                     Self.Response_Files.Register
                       (Resp_File.FD,
                        Resp_File.Path);
                  end;
               end;

               Self.Response_Files.Create (Cmd_Line);
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
   begin
      for Obj of Self.Embedded_Objects loop
         if not Self.Signature.Add_Input (Obj) and then Load_Mode then
            return;
         end if;
      end loop;

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Actions.Link.Object'Class :=
                     Actions.Link.Object'Class (Self.Tree.Action (Lib));
         begin
            if not Self.Signature.Add_Input (Link.Output)
              and then Load_Mode
            then
               return;
            end if;
         end;
      end loop;

      if not Self.Signature.Add_Output (Self.Output) and then Load_Mode then
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
      --  Add all object files contained in Library_Options attribute if they
      --  actually exist.
      Self.Add_Objects_From_Attribute (PRA.Library_Options);

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
