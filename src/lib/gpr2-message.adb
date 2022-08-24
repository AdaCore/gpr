--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with GNAT.Formatted_String;

package body GPR2.Message is

   ------------
   -- Create --
   ------------

   function Create
     (Level   : Level_Value;
      Message : String;
      Sloc    : Source_Reference.Object'Class;
      Indent  : Natural := 0;
      Raw     : Boolean := False) return Object is
   begin
      return Object'
        (Level, Unread, To_Unbounded_String (Message),
         Source_Reference.Object (Sloc), Indent, Raw);
   end Create;

   ------------
   -- Format --
   ------------

   function Format
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Levels         : Level_Output := (Long, Long, Long, Long)) return String
   is
      use GNAT.Formatted_String;

      function Level_Image return String is
        (case Levels (Self.Level) is
            when None =>
               "",
            when Short =>
               (case Self.Level is
                   when Error       => "E",
                   when Warning     => "W",
                   when Information => "I",
                   when Lint        => "L"),
           when Long =>
               (case Self.Level is
                   when Error       => "error",
                   when Warning     => "warning",
                   when Information => "info",
                   when Lint        => "lint"));

      Filename : constant String :=
                   (if Full_Path_Name
                    then Self.Sloc.Filename
                    else Directories.Simple_Name (Self.Sloc.Filename));

      Indent   : constant String := (1 .. Self.Indent * 2 => ' ');

      Indented : constant String := Indent
                   & (if Self.Indent < 1
                      then Level_Image & ": "
                      else "")
                   & To_String (Self.Message);
      --  Need to distingush warnings from errors because they are both going
      --  to the error output.

   begin
      if Self.Raw then
         return Indent & To_String (Self.Message);

      elsif Self.Sloc.Has_Source_Reference then
         declare
            Format : constant Formatted_String := +"%s:%d:%02d: %s";
         begin
            return -(Format
                     & Filename & Self.Sloc.Line & Self.Sloc.Column
                     & Indented);
         end;

      else
         declare
            Format : constant Formatted_String := +"%s: %s";
         begin
            return -(Format & Filename & Indented);
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

   ------------
   -- Output --
   ------------

   procedure Output
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Levels         : Level_Output := (Long, Long, Long, Long))
   is
      use Ada.Text_IO;
   begin
      Put_Line
        ((case Self.Level is
            when Information | Lint => Current_Output,
            when Error | Warning    => Current_Error),
         Self.Format (Full_Path_Name, Levels));
   end Output;

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

end GPR2.Message;
