--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Command_Line;
with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.Response_Files is

   type Response_File_Format is
     (None, GNU, Object_List, GCC_GNU, GCC_Object_List, GCC_Option_List,
      GNU_Archiver);

   subtype GCC_Formatting_Required is Response_File_Format
     with Static_Predicate =>
       GCC_Formatting_Required in GCC_GNU | GCC_Object_List | GCC_Option_List;

   type Response_File_Kind is (Binder, Compiler, Linker, Unknown);

   type Object is tagged private;

   Undefined : constant Object;

   procedure Create
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   procedure Initialize
     (Self     : in out Object;
      Format   : Response_File_Format;
      Kind     : Response_File_Kind;
      Switches : Containers.Source_Value_List);

   procedure Register
     (Self      : in out Object;
      FD        : GNATCOLL.OS.FS.File_Descriptor;
      Path      : Filename_Type;
      Secondary : Boolean := False);

   function Has_Primary_Content (Self : Object) return Boolean;

   function Primary_Response_File_Content
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String;

   function Primary_Response_File (Self : Object) return Path_Name.Object;

   function Has_Secondary_Content (Self : Object) return Boolean;

   function Secondary_Response_File_Content
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String;

   function Secondary_Response_File (Self : Object) return Path_Name.Object;

private

   use GNATCOLL;
   package GOF renames GNATCOLL.OS.FS;

   use type GOF.File_Descriptor;

   type Object is tagged record
      Format                : Response_File_Format     := None;
      Kind                  : Response_File_Kind       := Unknown;

      Primary_FD            : OS.FS.File_Descriptor    := GOF.Invalid_FD;
      Primary_Path          : Path_Name.Object;
      Primary_Content       : Unbounded_String;

      Secondary_FD          : OS.FS.File_Descriptor    := GOF.Invalid_FD;
      Secondary_Path        : Path_Name.Object;
      Secondary_Content     : Unbounded_String;

      Resp_File_Switches    : GNATCOLL.OS.Process.Argument_List;
   end record;

   procedure Close (Self : in out Object);

   procedure Create_Linker
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   procedure Create_Compiler
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   procedure Create_Binder
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   function Has_Primary_Response_File (Self : Object) return Boolean;

   function Has_Secondary_Response_File (Self : Object) return Boolean;

   Undefined : constant Object := (others => <>);

   function Has_Primary_Response_File (Self : Object) return Boolean is
     (Self.Primary_FD /= GOF.Invalid_FD);

   function Has_Primary_Content (Self : Object) return Boolean is
     (Self.Primary_Path.Is_Defined);

   function Primary_Response_File_Content
     (Self : Object) return Unbounded_String is
     (Self.Primary_Content);

   function Primary_Response_File (Self : Object) return Path_Name.Object is
     (Self.Primary_Path);

   function Has_Secondary_Response_File (Self : Object) return Boolean is
     (Self.Secondary_FD /= GOF.Invalid_FD);

   function Has_Secondary_Content (Self : Object) return Boolean is
     (Self.Secondary_Path.Is_Defined);

   function Secondary_Response_File_Content
     (Self : Object) return Unbounded_String is
     (Self.Secondary_Content);

   function Secondary_Response_File (Self : Object) return Path_Name.Object is
     (Self.Secondary_Path);

end  GPR2.Build.Response_Files;
