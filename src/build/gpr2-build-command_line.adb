--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.OS_Lib;
with GNATCOLL.Utils;

with GPR2.Containers;

package body GPR2.Build.Command_Line is

   use GPR2.Path_Name;

------------------
-- Add_Argument --
------------------

   procedure Add_Argument
     (Self : in out Object;
      Arg  : String;
      Mode : Signature_Mode := In_Signature) is
   begin
      Self.Cmd_Line.Append (Arg);
      Self.Total_Length := Self.Total_Length + 1 + Arg'Length;
      Self.In_Signature.Append (Mode);
   end Add_Argument;

   procedure Add_Argument
     (Self : in out Object;
      Arg  : Path_Name.Object;
      Mode : Signature_Mode := In_Signature)
   is
      Rel : constant Filename_Type := Arg.Relative_Path (Self.Cwd);
   begin
      if Rel'Length < Arg.Value'Length then
         Self.Add_Argument (String (Rel), Mode);
      else
         Self.Add_Argument (Arg.String_Value, Mode);
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

   -------------------------------
   -- Filter_Duplicate_Switches --
   -------------------------------

   procedure Filter_Duplicate_Switches
     (Self   : in out Object;
      Prefix : String)
   is
      Seen     : GPR2.Containers.Value_Set;
      Inserted : Boolean;
      Pos      : GPR2.Containers.Value_Type_Set.Cursor;
   begin
      for J in reverse Self.Cmd_Line.First_Index ..
        Self.Cmd_Line.Last_Index
      loop
         if GNATCOLL.Utils.Starts_With (Self.Cmd_Line (J), Prefix) then
            Seen.Insert (Self.Cmd_Line (J), Pos, Inserted);

            if not Inserted then
               --  Remove the duplicated value
               Self.Remove (J);
            end if;
         end if;
      end loop;
   end Filter_Duplicate_Switches;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : in out Object;
      Index : Natural) is
   begin
      Self.Cmd_Line.Delete (Index);
      Self.In_Signature.Delete (Index);
   end Remove;

   ----------------
   -- Set_Driver --
   ----------------

   procedure Set_Driver
     (Self : in out Object;
      Arg  : String)
   is
      procedure Internal (Driver : String);

      procedure Internal (Driver : String) is
      begin
         if Self.Cmd_Line.Is_Empty then
            Self.Total_Length := Driver'Length;
         else
            Self.Total_Length := Self.Total_Length + Driver'Length + 1;
         end if;

         Self.Cmd_Line.Prepend (Driver);
         Self.In_Signature.Prepend (Simple);
      end Internal;
   begin
      if GNAT.OS_Lib.Is_Absolute_Path (Arg) then
         Internal (Arg);
      else
         declare
            Full : constant String := GPR2.Locate_Exec_On_Path (Arg);
         begin
            if Full'Length > 0 then
               Internal (Full);
            else
               Internal (Arg);
            end if;
         end;
      end if;
   end Set_Driver;

   procedure Set_Driver
     (Self : in out Object;
      Arg  : Path_Name.Object) is
   begin
      Self.Set_Driver (Arg.String_Value);
   end Set_Driver;

   -------------------------------
   -- Set_Response_File_Command --
   -------------------------------

   procedure Set_Response_File_Command
     (Self : in out Object;
      Args : GNATCOLL.OS.Process.Argument_List) is
   begin
      Self.Raw_Cmd_Line := Self.Cmd_Line;
      Self.Cmd_Line     := Args;
   end Set_Response_File_Command;

   ---------------
   -- Signature --
   ---------------

   function Signature (Self : Object) return String
   is
      procedure Append (Arg : String);

      Result : String (1 .. Self.Total_Length) := (others => ' ');
      Idx    : Natural := Result'First;

      ------------
      -- Append --
      ------------

      procedure Append (Arg : String) is
      begin
         if Idx > Result'First then
            --  add a space
            Idx := Idx + 1;
         end if;

         Result (Idx .. Idx + Arg'Length - 1) := Arg;
         Idx := Idx + Arg'Length;
      end Append;

      use type GNATCOLL.OS.Process.Argument_List;

      Internal_Cmd : constant GNATCOLL.OS.Process.Argument_List :=
                       (if not Self.Raw_Cmd_Line.Is_Empty
                        and then Self.Cmd_Line /= Self.Raw_Cmd_Line
                        then Self.Raw_Cmd_Line
                        else Self.Cmd_Line);
   begin
      for J in Internal_Cmd.First_Index .. Internal_Cmd.Last_Index loop
         case Self.In_Signature (J) is
            when Simple =>
               declare
                  Arg    : String renames Internal_Cmd (J);
                  Simple : constant Simple_Name :=
                             Path_Name.Simple_Name
                               (Filename_Optional (Arg));
               begin
                  Append (String (Simple));
               end;

            when In_Signature =>
               Append (Internal_Cmd (J));

            when Ignore =>
               null;
         end case;
      end loop;

      return Result (Result'First .. Idx - 1);
   end Signature;

end GPR2.Build.Command_Line;
