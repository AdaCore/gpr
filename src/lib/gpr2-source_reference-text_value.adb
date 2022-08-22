--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

package body GPR2.Source_Reference.Text_Value is

   ------------
   -- Create --
   ------------

   function Create
     (Sloc : GPR2.Source_Reference.Object;
      Text : Text_Type) return Object'Class is
   begin
      return Object'
        (Sloc with Text => To_Unbounded_String (String (Text)));
   end Create;

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural;
      Text         : Text_Type)
      return Object'Class is
   begin
      return Create
        (GPR2.Source_Reference.Object
           (GPR2.Source_Reference.Create (Filename, Line, Column)),
         Text);
   end Create;

   ----------
   -- Text --
   ----------

   function Text (Self : Object) return Text_Type is
   begin
      return Text_Type (To_String (Self.Text));
   end Text;

   --------------------
   -- Unchecked_Text --
   --------------------

   function Unchecked_Text (Self : Object) return Unbounded_String is
   begin
      return Self.Text;
   end Unchecked_Text;

end GPR2.Source_Reference.Text_Value;
