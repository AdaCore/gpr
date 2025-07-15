--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with GNATCOLL.OS.Process;

with GPR2.Path_Name;

package GPR2.Build.Command_Line is

   type Object is tagged private;

   type Arg_Kind is (Driver, Obj, Other);

   type RF_Delimiter is (All_Args, First_Obj);

   package Args_Vector is new Ada.Containers.Indefinite_Vectors
     (Natural, String);

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
      Kind : Arg_Kind       := Other;
      Mode : Signature_Mode := In_Signature);

   procedure Add_Argument
     (Self : in out Object;
      Arg  : Path_Name.Object;
      Kind : Arg_Kind       := Other;
      Mode : Signature_Mode := In_Signature);

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
     (Self : Object; Kind : Arg_Kind) return Args_Vector.Vector;

   procedure Recompute_For_Response_File
     (Self          : in out Object;
      Clear_Other   : Boolean;
      Resp_File_Arg : String;
      Delimiter     : RF_Delimiter := First_Obj);
   --  Recompute the current command line with the Resp_File_Arg response file

   procedure Remove
     (Self  : in out Object;
      Index : Natural);
   --  Remove an argument from the Argument list at the specified index

   function Signature (Self : Object) return String;

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict;

   function Total_Length (Self : Object) return Natural;

   function Arg_Length (Self : Object) return Natural;

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

   type Args_By_Kind_Array is array (Arg_Kind) of Args_Vector.Vector;

   type Object is tagged record
      Cmd_Line     : GNATCOLL.OS.Process.Argument_List;
      Raw_Cmd_Line : GNATCOLL.OS.Process.Argument_List;
      Env          : GNATCOLL.OS.Process.Environment_Dict;
      In_Signature : Mode_Vectors.Vector;
      Args_By_Kind : Args_By_Kind_Array;
      Total_Length : Natural := 0;
      Arg_Length   : Natural := 0;
      Cwd          : Path_Name.Object;
   end record;

   function Argument_List
     (Self : Object) return GNATCOLL.OS.Process.Argument_List
   is (Self.Cmd_Line);

   function Argument_List
     (Self : Object; Kind : Arg_Kind) return Args_Vector.Vector
   is (Self.Args_By_Kind (Kind));

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict
   is (Self.Env);

   function Total_Length (Self : Object) return Natural is
     (Self.Total_Length);

   function Arg_Length (Self : Object) return Natural is
     (Self.Arg_Length);

end GPR2.Build.Command_Line;
