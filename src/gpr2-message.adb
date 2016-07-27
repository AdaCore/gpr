------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with GNAT.Formatted_String;

package body GPR2.Message is

   ------------
   -- Create --
   ------------

   function Create
     (Level   : Level_Value;
      Message : String;
      Sloc    : Source_Reference.Object := Source_Reference.Undefined)
      return Object is
   begin
      return Object'(Level, To_Unbounded_String (Message), Sloc);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Self : Object) return String is
      use Ada;
      use GNAT.Formatted_String;

      Filename : constant Full_Path_Name := Self.Sloc.Filename;
   begin
      if Self.Sloc.Has_Source_Reference then
         declare
            Format : constant Formatted_String := +"%s:%d:%d: %s";
         begin
            return -(Format
                     & Directories.Simple_Name (Filename)
                     & Self.Sloc.Line & Self.Sloc.Column
                     & To_String (Self.Message));
         end;

      else
         declare
            Format : constant Formatted_String := +"%s: %s";
         begin
            return -(Format
                     & Directories.Simple_Name (Filename)
                     & To_String (Self.Message));
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

   ----------
   -- Sloc --
   ----------

   function Sloc (Self : Object) return Source_Reference.Object is
   begin
      return Self.Sloc;
   end Sloc;

end GPR2.Message;
