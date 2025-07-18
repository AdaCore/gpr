--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Command_Line;
with GPR2.Containers;

package GPR2.Build.Response_Files is

   type Response_File_Format is
     (None, GNU, Object_List, GCC_GNU, GCC_Object_List, GCC_Option_List,
      GNU_Archiver);

   subtype GCC_Formatting_Required is Response_File_Format
     with Static_Predicate =>
       GCC_Formatting_Required in GCC_GNU | GCC_Object_List | GCC_Option_List;

   type Response_File_Kind is (Compiler, Linker, Unknown);

   type Object is tagged private;

   Undefined : constant Object;

   procedure Create
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   procedure Initialize
     (Self       : in out Object;
      Format     : Response_File_Format;
      Kind       : Response_File_Kind;
      Max_Length : Natural;
      Switches   : Containers.Source_Value_List);

   procedure Register
     (Self      : in out Object;
      FD        : GNATCOLL.OS.FS.File_Descriptor;
      Path      : Filename_Type;
      Secondary : Boolean := False);

   function Has_Primary_Content (Self : Object) return Boolean;

   function Primary_Response_File_Content
     (Self : Object) return GNATCOLL.OS.Process.Argument_List;

   function Has_Secondary_Content (Self : Object) return Boolean;

   function Secondary_Response_File_Content
     (Self : Object) return GNATCOLL.OS.Process.Argument_List;

private

   use GNATCOLL;
   package GOF renames GNATCOLL.OS.FS;

   use type GOF.File_Descriptor;

   type Object is tagged record
      Format                : Response_File_Format     := None;
      Kind                  : Response_File_Kind       := Unknown;
      Max_Cmd_Line_Length   : Natural                  := 0;

      Primary_FD            : OS.FS.File_Descriptor    := GOF.Invalid_FD;
      Primary_Path          : Unbounded_String;
      Primary_Content       : OS.Process.Argument_List;
      Has_Primary_Content   : Boolean                  := False;

      Secondary_FD          : OS.FS.File_Descriptor    := GOF.Invalid_FD;
      Secondary_Path        : Unbounded_String;
      Secondary_Content     : OS.Process.Argument_List;
      Has_Secondary_Content : Boolean                  := False;

      Resp_File_Switches    : Build.Command_Line.Args_Vector.Vector;
   end record;

   procedure Close (Self : in out Object);

   procedure Create_Linker
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   procedure Create_Compiler
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object);

   function Check_Length
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object) return Boolean;

   function Has_Primary_Response_File (Self : Object) return Boolean;

   function Has_Secondary_Response_File (Self : Object) return Boolean;

   Undefined : constant Object := (others => <>);

   function Has_Primary_Response_File (Self : Object) return Boolean is
     (Self.Primary_FD /= GOF.Invalid_FD);

   function Has_Primary_Content (Self : Object) return Boolean is
     (Self.Has_Primary_Content);

   function Primary_Response_File_Content
     (Self : Object) return OS.Process.Argument_List is
     (Self.Primary_Content);

   function Has_Secondary_Response_File (Self : Object) return Boolean is
     (Self.Secondary_FD /= GOF.Invalid_FD);

   function Has_Secondary_Content (Self : Object) return Boolean is
     (Self.Has_Secondary_Content);

   function Secondary_Response_File_Content
     (Self : Object) return OS.Process.Argument_List is
     (Self.Secondary_Content);

end  GPR2.Build.Response_Files;
