--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Handles log messages

with GPR2.Source_Reference;

package GPR2.Message is

   type Level_Value is (Warning, Error, End_User, Hint, Lint);
   --  Severity levels of messages:
   --
   --  Warning and Error:
   --     Self-explanatory.
   --  End_User:
   --     Used for messages displayed directly to the user without
   --     formatting, such as "Creating object directory 'obj'" or
   --     "Using project file p.gpr".
   --  Hint:
   --     Provides useful, but non-essential, information to help the user
   --     better understand the situation or context.
   --  Lint:
   --     Indicates possible stylistic and structural improvements
   --     to the project file.

   type User_Level_Value is (Optional, Regular, Important);

   type Status_Type is (Read, Unread);
   --  Read/Unread status for the messages. This is used for example by the Log
   --  to be able to retrieve only the new unread messages.

   type Object is tagged private;

   function Create
     (Level      : Level_Value;
      Message    : String;
      Sloc       : Source_Reference.Object'Class := Source_Reference.Undefined;
      Indent     : Natural := 0;
      To_Stderr  : Boolean := False;
      User_Level : User_Level_Value := Regular) return Object
     with Post => Create'Result.Status = Unread;
   --  Constructor for a log message
   --  Level:     the severity level of the message.
   --  Message:   the actual message to display
   --  Sloc:      the source location related to the message if any
   --  Indent:    if non-zero, "Format" won't print the prefix and will indent
   --             the message. Used to split a message into two messages.
   --  To_Stderr: will force the message to stderr (if supported by the
   --             reporter that actually displays the message). Else only
   --             warnings and errors go to stderr.

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Level (Self : Object) return Level_Value
     with Inline, Pre => Self.Is_Defined;
   --  Returns the message level associated with the message

   procedure Change_Level
     (Self : in out Object;
      Level : Level_Value)
     with Pre => Self.Is_Defined;
   --  Changes the level of Self

   function User_Level (Self : Object) return User_Level_Value
     with Inline, Pre => Self.Is_Defined;
   --  Returns the message level associated with the user-level message

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

   function Format
     (Self           : Object;
      Full_Path_Name : Boolean := False;
      Level_Fmt      : Level_Format := Long) return String
     with Pre => Self.Is_Defined;
   --  Returns the message with a standard message as expected by compiler
   --  tools: <filename>:<line>:<col>: <message>
   --  <filename> format controlled by Full_Path_Name parameter. Default False
   --  is for simple file name, True is for full path name format.
   --  Formatting does not apply to End_User messages, as they are intended to
   --  be displayed as simple strings to the user.

   procedure Set_Status (Self : in out Object; Status : Status_Type)
     with Pre  => Self.Is_Defined,
          Post => Self.Status = Status;
   --  Sets message as Read or Unread as specified by Status

   function To_Stderr (Self : Object) return Boolean;

private

   type Object is tagged record
      Level      : Level_Value := Warning;
      User_Level : User_Level_Value := Regular;
      Status     : Status_Type := Read;
      Message    : Unbounded_String := To_Unbounded_String ((1 => ASCII.NUL));
      Sloc       : Source_Reference.Object;
      Indent     : Natural := 0;
      To_Stderr  : Boolean := False;
   end record
     with Dynamic_Predicate => Message /= Null_Unbounded_String;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function To_Stderr (Self : Object) return Boolean is
      (Self.To_Stderr);
end GPR2.Message;
