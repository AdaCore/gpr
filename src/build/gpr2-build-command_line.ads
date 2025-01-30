--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.Process;

with GPR2.Path_Name;
with GPR2.Utils.Hash;

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

   procedure Finalize (Self : in out Object);

   function Argument_List
     (Self : Object) return GNATCOLL.OS.Process.Argument_List;

   function Signature (Self : Object) return String;

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict;

   function Total_Length (Self : Object) return Natural;

   function Is_Finalized (Self : Object) return Boolean;

   function Checksum (Self : Object) return GPR2.Utils.Hash.Hash_Digest
     with Pre => Self.Is_Finalized;

private

   type Object is tagged record
      Cmd_Line     : GNATCOLL.OS.Process.Argument_List;
      Env          : GNATCOLL.OS.Process.Environment_Dict;
      Total_Length : Natural := 0;
      Signature    : Unbounded_String;
      Cwd          : Path_Name.Object;
      Checksum     : GPR2.Utils.Hash.Hash_Digest :=
                       GPR2.Utils.Hash.No_Digest;
      Finalized    : Boolean := False;
   end record;

   function Argument_List
     (Self : Object) return GNATCOLL.OS.Process.Argument_List
   is (Self.Cmd_Line);

   function Environment_Variables
     (Self : Object) return GNATCOLL.OS.Process.Environment_Dict
   is (Self.Env);

   function Checksum (Self : Object) return GPR2.Utils.Hash.Hash_Digest is
     (Self.Checksum);

   function Is_Finalized (Self : Object) return Boolean is
      (Self.Finalized);

   function Total_Length (Self : Object) return Natural is
     (Self.Total_Length);

   function Signature (Self : Object) return String is
     (-Self.Signature);

end GPR2.Build.Command_Line;
