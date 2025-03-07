--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Vectors;

with GNATCOLL.OS.Process;

with GPR2.Path_Name;

package GPR2.Build.Command_Line is

   type Object is tagged private;

   function Create
     (Working_Dir : Path_Name.Object) return Object;

   procedure Set_Driver
     (Self : in out Object;
      Arg   : String);

   procedure Set_Driver
     (Self : in out Object;
      Arg  : Path_Name.Object);

   procedure Add_Argument
     (Self         : in out Object;
      Arg          : String;
      In_Signature : Boolean := True);

   procedure Add_Argument
     (Self         : in out Object;
      Arg          : Path_Name.Object;
      In_Signature : Boolean := True);

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

   function Signature (Self : Object) return String;

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict;

   function Total_Length (Self : Object) return Natural;

   procedure Filter_Duplicate_Switches
     (Self   : in out Object;
      Prefix : String);
   --  Used to filter out duplicate values in some circumstances. Only the last
   --  option is kept.

private

   package Bool_Vectors is new Ada.Containers.Vectors
     (Natural, Boolean);

   type Object is tagged record
      Cmd_Line     : GNATCOLL.OS.Process.Argument_List;
      Env          : GNATCOLL.OS.Process.Environment_Dict;
      In_Signature : Bool_Vectors.Vector;
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
