--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Handles log messages

with GPR2.Source_Reference;

package GPR2.Message is

   type Level_Value is (Information, Warning, Error, Lint);

   type Status_Type is (Read, Unread);
   --  Read/Unread status for the messages. This is used for example by the Log
   --  to be able to retrieve only the new unread messages.

   type Object is tagged private;

   function Create
     (Level   : Level_Value;
      Message : String;
      Sloc    : Source_Reference.Object'Class;
      Indent  : Natural := 0;
      Raw     : Boolean := False) return Object
     with Pre  => Sloc.Is_Defined,
          Post => Create'Result.Status = Unread;
   --  Constructor for a log message.
   --  Raw parameter mean that message should be displayed as is, without
   --  source reference.
   --  ??? Raw parameter should be removed when configuration will be done with
   --  libgprconfig instead of calling the gprconfig tool.

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Level (Self : Object) return Level_Value
     with Inline, Pre => Self.Is_Defined;
   --  Returns the message level associated with the message

   function Status (Self : Object) return Status_Type
     with Inline, Pre => Self.Is_Defined;
   --  Returns the status for the message

   function Message (Self : Object) return String
     with Inline, Pre => Self.Is_Defined;
   --  Returns the message itself

   function Sloc (Self : Object) return Source_Reference.Object
     with Inline, Pre => Self.Is_Defined;
   --  Returns the actual source reference associated with this message

   type Level_Format is (None, Short, Long);
   --  None  : no output of level information in message
   --  Short : short output of level, I, W, E
   --  Long  : long output of level (info, warning, error)

   type Level_Output is array (Level_Value) of Level_Format;
   --  Control all level output

   function Format
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Levels         : Level_Output := (Long, Long, Long, Long)) return String
     with Pre => Self.Is_Defined;
   --  Returns the message with a standard message as expected by compiler
   --  tools: <filename>:<line>:<col>: <message>
   --  <filename> format controlled by Full_Path_Name parameter. Default False
   --  is for simple file name, True is for full path name format.

   procedure Output
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Levels         : Level_Output := (Long, Long, Long, Long))
     with Pre => Self.Is_Defined;
   --  Outputs the message to console as expected by compiler
   --  tools: <filename>:<line>:<col>: <message>
   --  <filename> format controlled by Full_Path_Name parameter. Default False
   --  is for simple file name, True is for full path name format.
   --  Errors and Warnings messages are printed to standard error output,
   --  informational messages are printed to standard output.

   procedure Set_Status (Self : in out Object; Status : Status_Type)
     with Pre  => Self.Is_Defined,
          Post => Self.Status = Status;
   --  Sets message as Read or Unread as specified by Status

private

   type Object is tagged record
      Level   : Level_Value := Warning;
      Status  : Status_Type := Read;
      Message : Unbounded_String := To_Unbounded_String ((1 => ASCII.NUL));
      Sloc    : Source_Reference.Object;
      Indent  : Natural := 0;
      Raw     : Boolean := False;
   end record
     with Dynamic_Predicate => Message /= Null_Unbounded_String;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Message;
