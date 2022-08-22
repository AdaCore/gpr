--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Import is

   ------------
   -- Create --
   ------------

   function Create
     (Path_Name  : GPR2.Path_Name.Object;
      Sloc       : Source_Reference.Object;
      Is_Limited : Boolean) return Object is
   begin
      return Object'(Sloc with Path_Name, Is_Limited);
   end Create;

   ----------------
   -- Is_Limited --
   ----------------

   function Is_Limited (Self : Object) return Boolean is
   begin
      return Self.Is_Limited;
   end Is_Limited;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Path_Name;
   end Path_Name;

end GPR2.Project.Import;
