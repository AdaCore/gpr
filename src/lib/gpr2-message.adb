------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with GNAT.Formatted_String;

with GPR2.Path_Name;

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
     (Self : Object; Full_Path_Name : Boolean := False) return String
   is
      use Ada;
      use GNAT.Formatted_String;
      Filename : constant String :=
                   (if Full_Path_Name
                    then Self.Sloc.Filename
                    else Directories.Simple_Name (Self.Sloc.Filename));
      Indent   : constant String := (1 .. Self.Indent * 2 => ' ');
      Indented : constant String := Indent
                   & (if Self.Level = Warning then "warning: " else "")
                   & To_String (Self.Message);
      --  Need to distingush warnings from errors because they are both going
      --  to the error output.
   begin
      if Self.Raw then
         return Indent & To_String (Self.Message);

      elsif Self.Sloc.Has_Source_Reference then
         declare
            Format : constant Formatted_String := +"%s:%d:%d: %s";
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
