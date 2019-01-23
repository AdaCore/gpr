------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2019, Free Software Foundation, Inc.          --
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

--  Handles log messages

with GPR2.Source_Reference;

private with Ada.Strings.Unbounded;

package GPR2.Message is

   type Level_Value is (Information, Warning, Error);

   type Status_Type is (Read, Unread);
   --  Read/Unread status for the messages. This is used for example by the Log
   --  to be able to retrieve only the new unread messages.

   type Object is tagged private;

   function Create
     (Level   : Level_Value;
      Message : String;
      Sloc    : Source_Reference.Object'Class;
      Indent  : Natural := 0) return Object
     with Pre  => Sloc.Is_Defined,
          Post => Create'Result.Status = Unread;
   --  Constructor for a log message

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Level (Self : Object) return Level_Value with Inline;
   --  Returns the message level associated with the message

   function Status (Self : Object) return Status_Type with Inline;
   --  Returns the status for the message

   function Message (Self : Object) return String with Inline;
   --  Returns the message itself

   function Sloc (Self : Object) return Source_Reference.Object with Inline;
   --  Returns the actual source reference associated with this message

   function Format
     (Self : Object; Full_Path_Name : Boolean := False) return String;
   --  Returns the message with a standard message as expected by compiler
   --  tools: <filename>:<line>:<col>: <message>
   --  <filename> format controlled by Full_Path_Name parameter. Default False
   --  is for simple file name, True is for full path name format.

   procedure Set_Status (Self : in out Object; Status : Status_Type)
     with Post => Self.Status = Status;
   --  Sets message as Read or Unread as specified by Status

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Level   : Level_Value := Warning;
      Status  : Status_Type := Read;
      Message : Unbounded_String := To_Unbounded_String ((1 => ASCII.NUL));
      Sloc    : Source_Reference.Object;
      Indent  : Natural := 0;
   end record
     with Dynamic_Predicate => Message /= Null_Unbounded_String;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Message;
