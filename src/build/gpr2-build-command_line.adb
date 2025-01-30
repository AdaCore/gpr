--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Command_Line is

   use GPR2.Path_Name;

------------------
-- Add_Argument --
------------------

   procedure Add_Argument
     (Self         : in out Object;
      Arg          : String;
      In_Signature : Boolean := True) is
   begin
      Self.Finalized := False;
      Self.Cmd_Line.Append (Arg);
      Self.Total_Length := Self.Total_Length + 1 + Arg'Length;

      if In_Signature then
         if Length (Self.Signature) = 0 then
            Append (Self.Signature, Arg);
         else
            Append (Self.Signature, ' ');
            Append (Self.Signature, Arg);
         end if;
      end if;
   end Add_Argument;

   procedure Add_Argument
     (Self         : in out Object;
      Arg          : Path_Name.Object;
      In_Signature : Boolean := True)
   is
      Rel : constant Filename_Type := Arg.Relative_Path (Self.Cwd);
   begin
      if Rel'Length < Arg.Value'Length then
         Self.Add_Argument (String (Rel), In_Signature);
      else
         Self.Add_Argument (Arg.String_Value, In_Signature);
      end if;
   end Add_Argument;

   ----------------------
   -- Add_Env_Variable --
   ----------------------

   procedure Add_Env_Variable
     (Self  : in out Object;
      Var   : String;
      Value : String) is
   begin
      Self.Env.Include (Var, Value);
   end Add_Env_Variable;

   ------------
   -- Create --
   ------------

   function Create
     (Working_Dir : Path_Name.Object) return Object is
   begin
      return Result : Object do
         Result.Cwd := Working_Dir;
      end return;
   end Create;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Object) is
   begin
      if Self.Finalized then
         return;
      end if;

      Self.Finalized := True;
      Self.Checksum := GPR2.Utils.Hash.Hash_Content (-Self.Signature);
   end Finalize;

   ----------------
   -- Set_Driver --
   ----------------

   procedure Set_Driver
     (Self : in out Object;
      Arg  : String)
   is
      SName : constant Simple_Name :=
                Path_Name.Simple_Name (Filename_Optional (Arg));
   begin
      Self.Finalized := False;
      Self.Cmd_Line.Prepend (Arg);

      if Length (Self.Signature) = 0 then
         Self.Signature := +SName;
         Self.Total_Length := Arg'Length;
      else
         Self.Signature := +(SName & " ") & Self.Signature;
         Self.Total_Length := Arg'Length + 1;
      end if;
   end Set_Driver;

   procedure Set_Driver
     (Self : in out Object;
      Arg  : Path_Name.Object) is
   begin
      Self.Finalized := False;
      Self.Cmd_Line.Prepend (Arg.String_Value);

      if Length (Self.Signature) = 0 then
         Self.Signature := +Arg.Simple_Name;
         Self.Total_Length := Arg.String_Value'Length;
      else
         Self.Signature := +(Arg.Simple_Name & " ") & Self.Signature;
         Self.Total_Length := Arg.String_Value'Length + 1;
      end if;
   end Set_Driver;

   -------------------------------
   -- Set_Response_File_Command --
   -------------------------------

   procedure Set_Response_File_Command
     (Self : in out Object;
      Args : GNATCOLL.OS.Process.Argument_List) is
   begin
      Self.Cmd_Line := Args;
   end Set_Response_File_Command;

end GPR2.Build.Command_Line;
