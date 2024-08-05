--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GNAT.OS_Lib;

package body GPR2.Build.Actions.Link is

   ---------------------
   -- Add_Object_File --
   ---------------------

   procedure Add_Object_File
     (Self : in out Object; Obj : GPR2.Path_Name.Object) is
      UID : constant Actions.Action_Id'Class := Object'Class (Self).UID;
   begin

      if not Obj.Is_Defined then

         --  ??? Use a custom exception

         raise Program_Error with
           "Object file to add to " & UID.Image &
           " does not exist";
      end if;

      if not Self.Object_Files.Contains (Obj) then
         Self.Object_Files.Append (Obj);
      end if;

      if Self.Tree /= null and then Self.Tree.Is_Defined then
         if Obj.Is_Defined then
            Trace (
              Self.Traces,
              "Add " & String (Obj.Simple_Name) & " as an input for " &
                UID.Image);

            Self.Tree.Add_Input
              (UID,
               Artifacts.Files.Create (Obj),
               True);
         end if;
      end if;
   end Add_Object_File;

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
     (Self : Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict)
   is
      pragma Unreferenced (Env);
      use GPR2.Project.Registry.Attribute;

      Ada_Run_Dir : constant GPR2.Project.Attribute.Object :=
                      Self.Ctxt.Attribute
                        (Name  => Runtime_Dir,
                         Index => GPR2.Project.Attribute_Index.Create
                           (Ada_Language));
   begin
      --  ??? Replace hard coded values
      Args.Append ("gcc");

      for Obj of Self.Object_Files loop
         Args.Append (Obj.String_Value);
      end loop;

      --  ??? We will need a better way to obtain the link options depending
      --  on enabled runtimes.

      if Ada_Run_Dir.Is_Defined then
         declare
            Run_Dir : constant String := String (Ada_Run_Dir.Value.Text);
         begin
            --  ??? We may want to check that the directory exists

            Args.Append
              ("-L" & Run_Dir & GNAT.OS_Lib.Directory_Separator & "adalib");

            Args.Append
              (Run_Dir & GNAT.OS_Lib.Directory_Separator & "adalib" &
               GNAT.OS_Lib.Directory_Separator & "libgnat.a");
         end;
      end if;

      for Option of Self.Static_Options loop
         Args.Append (Option);
      end loop;

      declare
         Required_Switches : constant Project.Attribute.Object :=
           Self.Ctxt.Attribute (PRA.Linker.Required_Switches);
      begin
         if Required_Switches.Is_Defined then
            for Switch of Required_Switches.Values loop
               Args.Append ((Switch.Text));
            end loop;
         end if;
      end;

      Args.Append ("-o");
      Args.Append (Self.Executable.String_Value);
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding procedure Compute_Signature (Self : in out Object) is
      use GPR2.Build.Signature;
      Art : Artifacts.Files.Object;
   begin
      Self.Signature.Clear;

      for Obj_File of Self.Object_Files loop
         Art := Artifacts.Files.Create (Obj_File);
         Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);
      end loop;

      Art := Artifacts.Files.Create (Self.Output_Executable);
      Self.Signature.Update_Artifact (Art.UID, Art.Image, Art.Checksum);

      Self.Signature.Store
        (Self.Tree.Db_Filename_Path (Object'Class (Self).UID));
   end Compute_Signature;

   ----------------
   -- Initialize --
   ---------------

   procedure Initialize
     (Self             : in out Object;
      Executable       : GPR2.Path_Name.Object;
      Main_Object_File : GPR2.Path_Name.Object;
      Context          : GPR2.Project.View.Object)
   is
   begin
      Self.Object_Files.Append (Main_Object_File);
      Self.Executable := Executable;
      Self.Ctxt := Context;
      Self.Traces := Create ("ACTION_LINK");
   end Initialize;

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
         Artifacts.File_Part.Create (Self.Executable),
         Messages);

      if Messages.Has_Error then
         return;
      end if;

      for Obj of Self.Object_Files loop
         if Obj.Is_Defined then
            Trace (
              Self.Traces,
              "Add " & String (Obj.Simple_Name) & " as an input for " &
               UID.Image);

            Db.Add_Input
              (UID,
               Artifacts.Files.Create (Obj),
               True);
         end if;
      end loop;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Link_Id :=
                 (Name_Len  => Self.Executable.Simple_Name'Length,
                  Exec_Name =>
                    Optional_Name_Type (Self.Executable.Simple_Name),
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Link;
