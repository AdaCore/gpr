--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

with Ada.Strings.Unbounded;

generic
   type Text_Type is new String;
package GPR2.Source_Reference.Text_Value is

   type Object is new GPR2.Source_Reference.Object with private;

   overriding function "<" (Left, Right : Object) return Boolean;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Text_Type) return Object'Class
     with Post => Create'Result.Is_Defined;

   function Create
     (Sloc : GPR2.Source_Reference.Object;
      Text : Text_Type) return Object'Class
     with Pre  => Sloc.Is_Defined,
          Post => Create'Result.Is_Defined;

   function Text (Self : Object) return Text_Type
     with Pre => Self.Is_Defined;
   --  Returns the message associated with the reference

   function Unchecked_Text
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String
     with Pre => Self.Is_Defined;
   --  Returns the message associated with the reference

private

   type Object is new GPR2.Source_Reference.Object with record
      Text : Unbounded_String;
   end record;

   overriding function "<" (Left, Right : Object) return Boolean is
     (if GPR2.Source_Reference.Object (Left) /=
          GPR2.Source_Reference.Object (Right)
      then GPR2.Source_Reference.Object (Left) <
          GPR2.Source_Reference.Object (Right)
      else Left.Text < Right.Text);

   Undefined : constant Object :=
                 (GPR2.Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Source_Reference.Text_Value;
