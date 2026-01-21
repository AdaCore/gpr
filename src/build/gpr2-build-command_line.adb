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
      Mode : Signature_Mode := In_Signature;
      Kind : Arg_Kind       := Other) is
   begin
      Self.Cmd_Line.Append (Arg);
      Self.Kind.Append (Kind);
      Self.In_Signature.Append (Mode);
      Self.Total_Length := Self.Total_Length + 1 + Arg'Length;
   end Add_Argument;

   procedure Add_Argument
     (Self : in out Object;
      Arg  : Path_Name.Object;
      Mode : Signature_Mode := In_Signature;
      Kind : Arg_Kind       := Other)
   is
      Rel : constant Filename_Type := Arg.Relative_Path (Self.Cwd);
   begin
      if Rel'Length < Arg.Value'Length then
         Self.Add_Argument (String (Rel), Mode, Kind);
      else
         Self.Add_Argument (Arg.String_Value, Mode, Kind);
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

   -------------------
   -- Argument_List --
   -------------------

   function Argument_List
     (Self : Object; Kind : Arg_Kind) return GNATCOLL.OS.Process.Argument_List
   is
   begin
      return Result : GNATCOLL.OS.Process.Argument_List do
         for J in Self.Cmd_Line.First_Index .. Self.Cmd_Line.Last_Index loop
            if Self.Kind (J) = Kind then
               Result.Append (Self.Cmd_Line (J));
            end if;
         end loop;
      end return;
   end Argument_List;

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
     (Self          : in out Object;
      Prefix        : String;
      Keep_Leftmost : Boolean := False)
   is
      function To_Be_Removed (Idx : Natural) return Boolean;

      Seen     : GPR2.Containers.Value_Set;

      -----------
      -- Check --
      -----------

      function To_Be_Removed (Idx : Natural) return Boolean is
         Inserted : Boolean;
         Pos      : GPR2.Containers.Value_Type_Set.Cursor;
      begin
         if GNATCOLL.Utils.Starts_With (Self.Cmd_Line (Idx), Prefix) then
            Seen.Insert (Self.Cmd_Line (Idx), Pos, Inserted);

            if not Inserted then
               --  Remove the duplicated value
               return True;
            end if;
         end if;

         return False;
      end To_Be_Removed;

      Idx : Natural;

   begin
      if Keep_Leftmost then
         Idx := Self.Cmd_Line.First_Index;

         while Idx <= Self.Cmd_Line.Last_Index loop
            if To_Be_Removed (Idx) then
               Self.Remove (Idx);
            else
               Idx := Idx + 1;
            end if;
         end loop;

      else
         for J in reverse Self.Cmd_Line.First_Index ..
           Self.Cmd_Line.Last_Index
         loop
            if To_Be_Removed (J) then
               Self.Remove (J);
            end if;
         end loop;
      end if;
   end Filter_Duplicate_Switches;

   ---------------------------------
   -- Recompute_For_Response_File --
   ---------------------------------

   procedure Recompute_For_Response_File
     (Self          : in out Object;
      Clear_Other   : Boolean;
      Resp_File_Arg : String)
   is
      New_Cmd_Line : GNATCOLL.OS.Process.Argument_List;
      Index        : Natural;
   begin
      --  All args are inside the response file, just put the driver on the
      --  new command line.
      New_Cmd_Line.Append (Self.Cmd_Line.First_Element);

      Index := Self.Cmd_Line.First_Index + 1;

      if not Clear_Other then
         --  Skip the indexes until we find the first object
         --  This is important since some tools depend on the order of the
         --  switches

         while Index <= Self.Cmd_Line.Last_Index loop
            exit when Self.Kind (Index) = Obj;
            New_Cmd_Line.Append (Self.Cmd_Line (Index));
            Index := Index + 1;
         end loop;
      end if;

      --  Add the response file as a new arg of the command line
      New_Cmd_Line.Append (Resp_File_Arg);

      --  Depending on the response file structure, options can be contained in
      --  the response file or needs to be on the command line.
      if not Clear_Other then
         while Index <= Self.Cmd_Line.Last_Index loop
            if Self.Kind (Index) = Other then
               New_Cmd_Line.Append (Self.Cmd_Line  (Index));
            end if;

            Index := Index + 1;
         end loop;
      end if;

      Self.Set_Response_File_Command (New_Cmd_Line);
   end Recompute_For_Response_File;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : in out Object;
      Index : Natural)
   is
      Arg : constant String := Self.Cmd_Line (Index);
   begin
      Self.Cmd_Line.Delete (Index);
      Self.In_Signature.Delete (Index);
      Self.Kind.Delete (Index);
      Self.Total_Length := Self.Total_Length - Arg'Length - 1;
   end Remove;

   ----------------
   -- Set_Driver --
   ----------------

   procedure Set_Driver
     (Self : in out Object;
      Arg  : String)
   is
      procedure Internal (Arg : String);

      procedure Internal (Arg : String) is
      begin
         if Self.Cmd_Line.Is_Empty then
            Self.Total_Length := Arg'Length;
         else
            Self.Total_Length := Self.Total_Length + Arg'Length + 1;
         end if;

         Self.Cmd_Line.Prepend (Arg);
         Self.In_Signature.Prepend (Simple);
         Self.Kind.Prepend (Driver);
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
