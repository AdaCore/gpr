--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.Formatted_String;

package body GPR2.Message is

   ------------
   -- Create --
   ------------

   function Create
     (Level      : Level_Value;
      Message    : String;
      Sloc       : Source_Reference.Object'Class := Source_Reference.Undefined;
      Indent     : Natural := 0;
      To_Stderr  : Boolean := False;
      User_Level : User_Level_Value := Regular) return Object is
   begin
      return Object'
        (Level, User_Level, Unread, To_Unbounded_String (Message),
         Source_Reference.Object (Sloc), Indent, To_Stderr);
   end Create;

   ------------
   -- Format --
   ------------

   function Format
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Level_Fmt      : Level_Format := Long) return String
   is
      use GNAT.Formatted_String;

      function Level_Image return String is
        (case Level_Fmt is
            when None =>
               "",
            when Short =>
               (case Self.Level is
                   when Error    => "E",
                   when Warning  => "W",
                   when Hint     => "I",
                   when Lint     => "L",
                   when End_User => ""),
           when Long =>
               (case Self.Level is
                   when Error    => "error",
                   when Warning  => "warning",
                   when Hint     => "hint",
                   when Lint     => "lint",
                   when End_User => ""));

      Indent   : constant String := (1 .. Self.Indent * 2 => ' ');

      Indented : constant String := Indent
                   & (if Level_Image /= "" and then Self.Indent = 0
                      then Level_Image & ": "
                      else "")
                   & To_String (Self.Message);

   begin

      if Self.Level = End_User or else not Self.Sloc.Is_Defined then
         return Indented;
      else
         declare
            Format : constant Formatted_String := +"%s: %s";
         begin
            return -(Format & Self.Sloc.Format (Full_Path_Name) & Indented);
         end;
      end if;
   end Format;

   -----------
   -- Level --
   -----------

   function Level (Self : Object) return Level_Value is
   begin
      return Self.Level;
   end Level;

   -------------
   -- Message --
   -------------

   function Message (Self : Object) return String is
   begin
      return To_String (Self.Message);
   end Message;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Self : in out Object; Status : Status_Type) is
   begin
      Self.Status := Status;
   end Set_Status;

   ----------
   -- Sloc --
   ----------

   function Sloc (Self : Object) return Source_Reference.Object is
   begin
      return Self.Sloc;
   end Sloc;

   ------------
   -- Status --
   ------------

   function Status (Self : Object) return Status_Type is
   begin
      return Self.Status;
   end Status;

   ----------------
   -- User_Level --
   ----------------

   function User_Level (Self : Object) return User_Level_Value is
   begin
      return Self.User_Level;
   end User_Level;

end GPR2.Message;
