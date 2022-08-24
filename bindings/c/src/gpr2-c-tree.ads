--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
