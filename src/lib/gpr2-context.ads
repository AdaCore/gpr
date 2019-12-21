------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Containers;

with GNAT.MD5; use GNAT;

package GPR2.Context is

   use type GPR2.Containers.Count_Type;
   use type MD5.Binary_Message_Digest;

   package Key_Value is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, Value_Type);

   type Object is new Key_Value.Map with private;
   --  A parsing context containing the external values for a given project
   --  tree. This context corresponds to all values of external variables found
   --  in a project tree.

   Empty : constant Object;

   subtype Binary_Signature is MD5.Binary_Message_Digest;

   Default_Signature : constant Binary_Signature;
   --  The default signature, this is the one used for project having no
   --  external variable.

   function Signature
     (Self      : Object;
      Externals : Containers.Name_List) return Context.Binary_Signature
     with Post =>
       (if Externals.Length = 0
           or else (for all E of Externals => not Self.Contains (E))
        then Signature'Result = Default_Signature
        else Signature'Result /= Default_Signature);
   --  Computes and returns MD5 signature for the Externals given the context.
   --  This is used to check if a project's environment has been changed and
   --  if so the project is to be analyzed again. Note that if there is no
   --  Externals the project has no need to be analyzed again, in this case
   --  the Default_Signature is returned.

private

   type Object is new Key_Value.Map with null record;

   Empty : constant Object := (Key_Value.Empty_Map with null record);

   Default_Signature : constant Binary_Signature := (others => 0);

end GPR2.Context;
