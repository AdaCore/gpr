--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Source_Reference is

   ------------
   -- Column --
   ------------

   function Column (Self : Object) return Positive is
   begin
      return Self.Column;
   end Column;

   ------------
   -- Create --
   ------------

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural) return Object'Class is
   begin
      return Object'(Line, Column, To_Unbounded_String (Filename));
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Path_Name.Full_Name is
   begin
      return To_String (Self.Filename);
   end Filename;

   ----------
   -- Line --
   ----------

   function Line (Self : Object) return Positive is
   begin
      return Self.Line;
   end Line;

end GPR2.Source_Reference;
