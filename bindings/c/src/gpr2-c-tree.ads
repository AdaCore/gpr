------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

--  C Binding to GPR2 Project Trees API

with GPR2.C.JSON;

package GPR2.C.Tree is

   procedure Load
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_LOAD

   procedure Unload
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_UNLOAD

   procedure Log_Messages
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_LOG_MESSAGES

   procedure Invalidate_Source_List
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_INVALIDATE_SOURCE_LIST

   procedure Update_Source_List
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_UPDATE_SOURCE_LIST

   procedure Update_Source_Infos
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Implements TREE_UPDATE_SOURCE_INFOS

end GPR2.C.Tree;
