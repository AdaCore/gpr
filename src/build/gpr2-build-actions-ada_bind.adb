--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Compile.Pre_Bind;
with GPR2.Build.Actions.Link;
with GPR2.Build.Actions.Ada_Compile.Post_Bind;
with GPR2.Build.ALI_Parser;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;

package body GPR2.Build.Actions.Ada_Bind is

   package GBA renames GPR2.Build.Actions;

   -------------
   -- Command --
   -------------

   overriding function Command (Self : Object)
     return GNATCOLL.OS.Process.Argument_List
   is
      Args : GNATCOLL.OS.Process.Argument_List;
   begin
      if Self.Unit.Has_Part (S_Body) then
         Args.Append ("gnatbind");

         Args.Append  (Self.Main_Ali.String_Value);

         Args.Append ("-o");
         --  Directories separator are not allowed. We must be in the correct
         --  directory and only use the source base name with extension.

         Args.Append (String (Self.Unit.Main_Body.Source.Simple_Name));
      end if;

      return Args;
   end Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;

      Part     : Compilation_Unit.Unit_Location;
   begin

      Self.Signature.Clear;
      for Kind in S_Spec .. S_Body loop
         if Self.Unit.Has_Part (Kind) then
            Part := Self.Unit.Get (Kind);
            declare
               Art : constant Artifacts.File_Part.Object :=
                       Artifacts.File_Part.Create (Part.Source, Part.Index);
            begin
               Self.Signature.Update_Artifact
                 (Art.UID, Art.Image, Art.Checksum);
            end;
         end if;
      end loop;

      for Ali of Self.Ali_Files loop
         declare
               Art : constant Artifacts.Files.Object :=
                       Artifacts.Files.Create (Ali);
         begin
            Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);
         end;
      end loop;

      Self.Signature.Store
        (Self.Tree.Db_Filename_Path (Object'Class (Self).UID));
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Object;
      Main_Ali : GPR2.Path_Name.Object;
      Context  : GPR2.Project.View.Object)
   is
      BN       : constant Simple_Name :=
                   "b__" & Simple_Name (Main_Ali.Base_Name);
      Success  : Boolean := False;
   begin
      Self.Ctxt := Context;
      Self.Main_Ali := Main_Ali;
      Self.Ali_Files.Insert (Main_Ali);
      Self.Traces := Create ("ACTION_ADA_BIND");

      Self.Unit := GPR2.Build.Compilation_Unit.Create
                       (Name    => "ADA_MAIN",
                        Context => Context);

      GPR2.Build.Compilation_Unit.Add
        (Self     => Self.Unit,
         Kind     => S_Body,
         View     => Context,
         Path     => Context.Object_Directory.Compose (BN & ".adb"),
         Success  => Success);

      GPR2.Build.Compilation_Unit.Add
        (Self     => Self.Unit,
         Kind     => S_Spec,
         View     => Context,
         Path     => Context.Object_Directory.Compose (BN & ".ads"),
         Success  => Success);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
      Part : Compilation_Unit.Unit_Location;
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin

      for Kind in S_Spec .. S_Body loop
         if Self.Unit.Has_Part (Kind) then
            Part := Self.Unit.Get (Kind);
            Db.Add_Output
              (UID,
               Artifacts.File_Part.Create (Part.Source, Part.Index),
               Messages);
         end if;
      end loop;

      if Messages.Has_Error then
         return;
      end if;

      for Ali_File of Self.Ali_Files loop
         if Ali_File.Is_Defined then
            Db.Add_Input
              (UID,
               Artifacts.Files.Create (Ali_File),
               True);
         end if;
      end loop;
   end On_Tree_Insertion;

   ---------------
   -- Parse_Ali --
   ---------------

   procedure Parse_Ali (Self : in out Object; Ali : GPR2.Path_Name.Object)
   is
      procedure Add_Object_File_To_Linker
        (Object_File : GPR2.Path_Name.Object);
      --  Find the link action in the bind successors, and add the
      --  object file as one if its inputs.

      function Import_View_Db (Imp : GPR2.Build.ALI_Parser.Import_Info)
        return GPR2.Build.View_Db.Object;
      --  Return the view database containing the imported unit

      procedure Add_Action_If_Not_Already_Done
        (Action : in out GBA.Ada_Compile.Pre_Bind.Object);
      --  Add the action to the current object tree database if it has not
      --  already be done.

      ------------------------------------
      -- Add_Action_If_Not_Already_Done --
      ------------------------------------

      procedure Add_Action_If_Not_Already_Done
        (Action : in out GBA.Ada_Compile.Pre_Bind.Object) is
         Messages : GPR2.Log.Object;
      begin
         if not GPR2.Build.Tree_Db.Has_Action (Self.Tree.all, Action.UID) then
            Trace
               (Self.Traces, "Add the action '" & Action.UID.Image &
                  " to the tree database.");
            GPR2.Build.Tree_Db.Add_Action (Self.Tree.all, Action, Messages);

            if Messages.Has_Error then
               Messages.Output_Messages (Information => False,
                                         Warning     => False);

               --  ??? Use custom exception

               raise Program_Error with
                 "Failed to add " & Action.UID.Image &
                 " to the tree database";
            end if;
         end if;
      end Add_Action_If_Not_Already_Done;

      -------------------------------
      -- Add_Object_File_To_Linker --
      -------------------------------

      procedure Add_Object_File_To_Linker (Object_File : GPR2.Path_Name.Object)
      is
         Output_Unit_Src :
           constant GPR2.Build.Compilation_Unit.Unit_Location :=
             Self.Unit.Main_Part;
      begin
         for Action of Self.Tree.Successors
                         (Artifacts.File_Part.Create
                            (Output_Unit_Src.Source, Output_Unit_Src.Index))
         loop
            if Action in GBA.Ada_Compile.Post_Bind.Object'Class then
               for Post_Bind_Succ of Self.Tree.Successors
                 (Artifacts.Files.Create
                    (GBA.Ada_Compile.Post_Bind.Object (Action).Object_File))
               loop
                  if Post_Bind_Succ in GBA.Link.Object'Class then
                     declare
                        Link :
                          constant GPR2.Build.Tree_Db.Action_Reference_Type :=
                            Self.Tree.Action_Id_To_Reference
                              (Id => Post_Bind_Succ.UID);
                     begin
                        Trace
                           (Self.Traces,
                            "Add the object file " &
                              String (Object_File.Simple_Name) &
                              " to the action " & Link.UID.Image);
                        GBA.Link.Object'Class
                          (Link.Element.all).Add_Object_File
                             (Object_File);

                        return;
                     end;
                  end if;
               end loop;
            end if;
         end loop;
      end Add_Object_File_To_Linker;

      --------------------
      -- Import_View_Db --
      --------------------

      function Import_View_Db (Imp : GPR2.Build.ALI_Parser.Import_Info)
        return GPR2.Build.View_Db.Object
      is
      begin
         for V_Db of Self.Tree.Views_Database loop
            if V_Db.View.Is_Runtime then
               if V_Db.Has_Source
                 (Simple_Name (To_String (Imp.Source)))
               then
                  --  We do not want to recompile the runtime

                  return V_Db;
               end if;
            elsif V_Db.View.Is_Namespace_Root then
               if V_Db.Source_Option >= Sources_Units and then
                  V_Db.Has_Compilation_Unit
                    (Name_Type (To_String (Imp.Unit_Name)))
               then
                  return V_Db;
               end if;
            end if;
         end loop;

         return GPR2.Build.View_Db.Undefined;
      end Import_View_Db;


      Messages : GPR2.Log.Object;
      Imports  : GPR2.Build.ALI_Parser.Import_Info_Vectors.Vector;
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin


      Trace (Self.Traces,
               "Parse " & String (Ali.Simple_Name));
      GPR2.Build.ALI_Parser.Imports
         (Ali, Imports, Messages);

      if Messages.Has_Error then
         Messages.Output_Messages (Information => False,
                                   Warning     => False);

         --  ??? Use a custom exception

         raise Program_Error with "Parsing error";
      end if;

      for Imp of Imports loop
         Trace (Self.Traces, "Found import " & To_String (Imp.ALI));

         declare
            Imp_View_Db : constant GPR2.Build.View_Db.Object :=
                            Import_View_Db (Imp);
            Import_Ali : GPR2.Path_Name.Object;
         begin

            if not Imp_View_Db.Is_Defined then
               Trace
                  (Self.Traces,
                    "Did not find a view containing unit " &
                     To_String (Imp.Unit_Name));
                     goto Next_Import;
            end if;

            if Imp_View_Db.View.Is_Runtime then

               --  We do not want to create a compile action for the runtime
               --  units.

               if Imp_View_Db.View.Is_Library then
                  Import_Ali :=
                    Imp_View_Db.View.Library_Ali_Directory.Compose
                      (Filename_Type (To_String (Imp.ALI)));
               else
                  Import_Ali :=
                    Imp_View_Db.View.Object_Directory.Compose
                     (Filename_Type (To_String (Imp.ALI)));
               end if;

               if not Import_Ali.Is_Defined or else not Import_Ali.Exists then
                  Trace
                    (Self.Traces, "Did not find the runtime ALI file " &
                     To_String (Imp.ALI));
                  goto Next_Import;
               end if;

            elsif Imp_View_Db.View.Is_Namespace_Root then
               declare
                  Action    : GBA.Ada_Compile.Pre_Bind.Object :=
                        GBA.Ada_Compile.Pre_Bind.Undefined;
               begin
                  Action.Initialize
                    (Imp_View_Db.Compilation_Unit
                       (Name_Type (To_String (Imp.Unit_Name))));

                  Add_Action_If_Not_Already_Done (Action);

                  if String (Action.Ali_File.Simple_Name) = To_String (Imp.ALI)
                  then
                     Import_Ali := Action.Ali_File;

                     --  If Bind action already contains the new import's ALI,
                     --  then another ALI previously parsed already contained
                     --  this import.

                     if Import_Ali.Is_Defined and then
                        not Self.Ali_Files.Contains (Import_Ali)
                     then
                        Add_Object_File_To_Linker (Action.Object_File);
                     end if;
                  else
                     --  ??? Use a custom exception

                     raise Program_Error with
                        "Parsed ALI file """ &
                        String (Action.Ali_File.Simple_Name) &
                        " "" and computed ALI file """ & To_String (Imp.ALI) &
                        """ do not match";
                  end if;
               end;
            end if;

            if Import_Ali.Is_Defined and then
               not Self.Ali_Files.Contains (Import_Ali)
            then
               Trace
                  (Self.Traces, "Add " &
                  String (Import_Ali.Simple_Name) &
                  " as an input of the action " & UID.Image);
               GPR2.Build.Tree_Db.Add_Input
                 (Self.Tree.all, UID,
                  Artifacts.Files.Create (Import_Ali), True);
               Self.Ali_Files.Insert (Import_Ali);
            end if;
         end;

         <<Next_Import>>
      end loop;
   end Parse_Ali;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Ada_Bind_Id :=
                 (Name_Len  => Self.Main_Ali.Base_Name'Length,
                  Ali_Name  => Self.Main_Ali.Base_Name,
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Ada_Bind;
