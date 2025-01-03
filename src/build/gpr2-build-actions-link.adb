--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;

with GNATCOLL.Utils;

with GPR2.External_Options;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;

package body GPR2.Build.Actions.Link is

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Self : in out Object; Option : String) is
   begin
      Self.Static_Options.Append (Option);
   end Add_Option;

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding procedure Compute_Command
     (Self     : in out Object;
      Slot     : Positive;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      pragma Unreferenced (Slot);

      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean;
         Param        : String := "") return Boolean;

      --------------
      -- Add_Attr --
      --------------

      function Add_Attr
        (Id           : Q_Attribute_Id;
         Index        : PAI.Object;
         Is_List      : Boolean;
         In_Signature : Boolean;
         Param        : String := "") return Boolean
      is
         Attr : constant Project.Attribute.Object :=
                  Self.View.Attribute (Id, Index);
      begin
         if not Attr.Is_Defined then
            return False;
         end if;

         if Is_List then
            for Idx in Attr.Values.First_Index .. Attr.Values.Last_Index loop
               if Idx < Attr.Values.Last_Index then
                  Cmd_Line.Add_Argument
                    (Attr.Values.Element (Idx).Text, In_Signature);
               else
                  Cmd_Line.Add_Argument
                    (Attr.Values.Element (Idx).Text & Param, In_Signature);
               end if;
            end loop;
         else
            Cmd_Line.Add_Argument
              (Attr.Value.Text & Param, In_Signature);
         end if;

         return True;
      end Add_Attr;

      Objects    : Tree_Db.Artifact_Sets.Set;
      Status     : Boolean;
      Src_Idx    : constant PAI.Object :=
                     (if not Self.Is_Library
                      then PAI.Create
                        (String (Self.Main_Src.Path.Simple_Name),
                         Case_Sensitive => File_Names_Case_Sensitive,
                         At_Pos         => Self.Main_Src.Index)
                      else PAI.Undefined);


   begin
      Objects := Self.Embedded_Objects;

      --  Remove from this list of objects the ones that come from
      --  libraries.

      for Lib of Self.Library_Dependencies loop
         declare
            Link : constant Object'Class :=
                     Object'Class (Self.Tree.Action (Lib));
         begin
            Objects.Difference (Link.Embedded_Objects);
         end;
      end loop;

      --  ??? Replace hard coded values
      if Self.Is_Static_Library then
         declare
            Attr : constant GPR2.Project.Attribute.Object :=
                     Self.View.Attribute (PRA.Archive_Builder);
         begin
            pragma Assert (Attr.Is_Defined, "No archiver is defined");

            for Val of Attr.Values loop
               --  [eng/gpr/gpr-issues#446] Hack to speed up and ease the
               --  generation of archives :
               --  instead of using "ar cr" then use ranlib, we generate
               --  directly the symbol table by using "ar csr".

               if Val.Text = "cr" then
                  Cmd_Line.Add_Argument ("csr", True);
               else
                  Cmd_Line.Add_Argument (Val.Text, True);
               end if;
            end loop;

            Cmd_Line.Add_Argument (String (Self.Output.Path.Simple_Name));
         end;

      else
         Status := Add_Attr (PRA.Linker.Driver, PAI.Undefined, False, True);
         pragma Assert (Status, "No linker driver is defined");

         if Src_Idx.Is_Defined then
            Status :=
              Add_Attr (PRA.Linker.Leading_Switches, Src_Idx, True, True);
         end if;

         if Self.Is_Library then
            --  shared lib case, add the minimal options
            Status := Add_Attr
              (PRA.Shared_Library_Minimum_Switches, PAI.Undefined, True, True);
         end if;

         --  ??? This shouldn't be hardcoded
         Cmd_Line.Add_Argument ("-o", True);
         Cmd_Line.Add_Argument
           (String (Self.Output.Path.Simple_Name), True);
      end if;

      for Obj of Objects loop
         Cmd_Line.Add_Argument
           (Artifacts.Files.Object'Class (Obj).Path, True);
      end loop;

      if not Self.Is_Static_Library then
         for Lib of Self.Library_Dependencies loop
            declare
               Link : constant Object'Class :=
                        Object'Class (Self.Tree.Action (Lib));

            begin
               Cmd_Line.Add_Argument
                 (Link.Library.Path, True);
            end;
         end loop;

         for C of Self.View.Closure (True) loop
            declare
               Opt     : constant Project.Attribute.Object :=
                           C.Attribute (PRA.Linker.Linker_Options);
               Lib_Opt : constant Value_Type :=
                           Self.Tree.Linker_Lib_Dir_Option;
               use GNATCOLL.Utils;
               use type GPR2.Project.View.Object;
            begin
               if Opt.Is_Defined then
                  for Val of Opt.Values loop
                     declare
                        Arg  : constant Value_Type := Val.Text;
                        Path : Path_Name.Object;
                     begin
                        if Arg'Length >= Lib_Opt'Length
                          and then Starts_With (Arg, Lib_Opt)
                        then
                           --  Need to check that any -L<path> option has an
                           --  absolute dir.

                           Path := Path_Name.Create_Directory
                             (Filename_Type
                                (Arg (Arg'First + Lib_Opt'Length .. Arg'Last)),
                              C.Dir_Name.Value);
                           Cmd_Line.Add_Argument
                             (Lib_Opt & Path.String_Value);

                        elsif Arg (Arg'First) = '-' then
                           --  ??? How about case where linker switches don't
                           --  start with a dash?

                           if C /= Self.View then
                              --  For self.View, use non-switch parts of
                              --  the linker option only.

                              Cmd_Line.Add_Argument (Val.Text, True);
                           end if;

                        else
                           --  Check for relative paths and translate them
                           --  as absolute.

                           Cmd_Line.Add_Argument
                             (Path_Name.Create_File
                                (Filename_Type (Val.Text),
                                 C.Dir_Name.Value).String_Value, True);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end if;

      if Src_Idx.Is_Defined then
         --  Add switches for linking an executable
         Status :=
           Add_Attr (PRA.Linker.Required_Switches, PAI.Undefined, True, True);

         Status := Add_Attr (PRA.Linker.Switches, Src_Idx, True, True);

         if not Status then
            Status := Add_Attr
              (PRA.Linker.Default_Switches,
               PAI.Create
                 (Self.View.Source (Self.Main_Src.Path.Simple_Name).Language),
               True,
               True);
         end if;

         --  Add -largs

         for Arg
           of Self.Tree.External_Options.Fetch
             (GPR2.External_Options.Linker, GPR2.No_Language)
         loop
            Cmd_Line.Add_Argument (Arg);
         end loop;

         for Option of Self.Static_Options loop
            Cmd_Line.Add_Argument (Option);
         end loop;

         Status :=
           Add_Attr (PRA.Linker.Trailing_Switches, Src_Idx, True, True);
      end if;
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature
     (Self      : Object;
      Signature : in out GPR2.Build.Signature.Object)
   is
   begin
      for Obj of Self.Embedded_Objects loop
         Signature.Add_Artifact (Obj);
      end loop;

      if not Self.Is_Library then
         --  ??? TODO dynamic libraries also need their library dependencies
         for Lib of Self.Library_Dependencies loop
            Signature.Add_Artifact (Object'Class (Self).Output);
         end loop;
      end if;

      Signature.Add_Artifact (Self.Output);
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
      Src        : Artifacts.File_Part.Object;
      Context    : GPR2.Project.View.Object;
      Output     : Filename_Optional := "")
   is
      Exec : constant GPR2.Path_Name.Object :=
               (if Output'Length = 0
                then Context.Executable (Src.Path.Simple_Name, Src.Index)
                else Context.Executable_Directory.Compose (Output));
   begin
      Self.Is_Library := False;
      Self.Main_Src   := Src;
      Self.Executable := Artifacts.Files.Create (Exec);
      Self.Ctxt       := Context;
      Self.Traces     := Create ("ACTION_LINK");
   end Initialize_Executable;

   -------------------------------
   -- Initialize_Global_Archive --
   -------------------------------

   procedure Initialize_Global_Archive
     (Self    : in out Object;
      Context : GPR2.Project.View.Object)
   is
      Project_Name_Low : constant String :=
                           Ada.Characters.Handling.To_Lower
                             (String (Context.Name));
      Library_Filename : constant Simple_Name :=
                           "lib" & Filename_Type (Project_Name_Low) &
                           Context.Tree.Configuration.Archive_Suffix;
   begin
      Self.Ctxt       := Context;
      Self.Is_Library := True;
      Self.Is_Static  := True;
      Self.In_Obj     := True;
      Self.Library    := Artifacts.Library.Create
        (Context.Object_Directory.Compose (Library_Filename));
      Self.Traces     := Create ("ACTION_LINK");
   end Initialize_Global_Archive;

   ------------------------
   -- Initialize_Library --
   ------------------------

   procedure Initialize_Library
     (Self    : in out Object;
      Context : GPR2.Project.View.Object) is
   begin
      Self.Ctxt       := Context;
      Self.Is_Library := True;
      Self.Is_Static  := Context.Library_Kind in "static" | "static-pic";
      Self.Library    := Artifacts.Library.Create (Context.Library_Filename);
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

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin
      return Db.Add_Output (UID, Self.Output);
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
