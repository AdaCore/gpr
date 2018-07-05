------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2018, Free Software Foundation, Inc.            --
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

package body GPR2.Project.Source.Artifact is

   ------------
   -- Create --
   ------------

   function Create
     (Source     : GPR2.Project.Source.Object;
      Object     : Path_Name.Object;
      Dependency : Path_Name.Object := Path_Name.Undefined)
      return Artifact.Object is
   begin
      return Artifact.Object'
        (Source     => Source,
         Object     => Object,
         Dependency => Dependency);
   end Create;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Artifact.Object) return Path_Name.Object is
   begin
      return Self.Dependency;
   end Dependency;

   -----------------
   -- Object_Code --
   -----------------

   function Object_Code
     (Self : Artifact.Object) return Path_Name.Object is
   begin
      return Self.Object;
   end Object_Code;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Project.Source.Object is
   begin
      return Self.Source;
   end Source;

end GPR2.Project.Source.Artifact;
