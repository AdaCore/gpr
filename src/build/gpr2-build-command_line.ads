--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;

with GNATCOLL.OS.Process;

with GPR2.Path_Name;

package GPR2.Build.Command_Line is

   type Object is tagged private;

   type Arg_Kind is (Driver, Obj, Other);

   function Create
     (Working_Dir : Path_Name.Object) return Object;

   procedure Set_Driver
     (Self : in out Object;
      Arg  : String);

   procedure Set_Driver
     (Self : in out Object;
      Arg  : Path_Name.Object);

   type Signature_Mode is
     (In_Signature,
      Ignore,
      Simple);
   --  The way the argument is handled in the signature:
   --  In_Signature: included as is
   --  Ignore: argument is not used to compute the signature
   --  Simple: used for files, only the simple name is used

   procedure Add_Argument
     (Self : in out Object;
      Arg  : String;
      Mode : Signature_Mode := In_Signature;
      Kind : Arg_Kind       := Other);

   procedure Add_Argument
     (Self : in out Object;
      Arg  : Path_Name.Object;
      Mode : Signature_Mode := In_Signature;
      Kind : Arg_Kind       := Other);

   procedure Add_Env_Variable
     (Self  : in out Object;
      Var   : String;
      Value : String);

   procedure Set_Response_File_Command
     (Self : in out Object;
      Args : GNATCOLL.OS.Process.Argument_List);
   --  Replaces the actual command without changing its signature. Used
   --  in particular to accomodate commands with response file.

   function Argument_List
     (Self : Object) return GNATCOLL.OS.Process.Argument_List;

   function Argument_List
     (Self : Object; Kind : Arg_Kind) return GNATCOLL.OS.Process.Argument_List;

   procedure Recompute_For_Response_File
     (Self          : in out Object;
      Clear_Other   : Boolean;
      Resp_File_Arg : String);
   --  Recompute the current command line with the Resp_File_Arg response file

   procedure Remove
     (Self  : in out Object;
      Index : Natural);
   --  Remove an argument from the Argument list at the specified index

   function Signature (Self : Object) return String;

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict;

   function Total_Length (Self : Object) return Natural;

   procedure Filter_Duplicate_Switches
     (Self   : in out Object;
      Prefix : String;
      Keep_Leftmost : Boolean := False);
   --  Used to filter out duplicate values in some circumstances.
   --  If Keep_Leftmost is false, only the last option is kept, else the first
   --  option is kept.

private

   package Mode_Vectors is new Ada.Containers.Vectors
     (Natural, Signature_Mode);

   package Kind_Vectors is new Ada.Containers.Vectors
     (Natural, Arg_Kind);

   type Object is tagged record
      Cmd_Line     : GNATCOLL.OS.Process.Argument_List;
      Raw_Cmd_Line : GNATCOLL.OS.Process.Argument_List;
      Env          : GNATCOLL.OS.Process.Environment_Dict;
      In_Signature : Mode_Vectors.Vector;
      Kind         : Kind_Vectors.Vector;
      Total_Length : Natural := 0;
      Cwd          : Path_Name.Object;
   end record;

   function Argument_List
     (Self : Object) return GNATCOLL.OS.Process.Argument_List
   is (Self.Cmd_Line);

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict
   is (Self.Env);

   function Total_Length (Self : Object) return Natural is
     (Self.Total_Length);

end GPR2.Build.Command_Line;
