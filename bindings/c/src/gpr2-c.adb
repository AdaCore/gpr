--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.C.JSON;
with GPR2.C.Tree;
with GPR2.C.View;
with GPR2.C.Source;

package body GPR2.C is

   type Binding_Map is array (C_Function range <>) of GPR2.C.JSON.Bind_Handler;

   Binding : constant Binding_Map :=
      (TREE_LOAD                    => Tree.Load'Access,
       TREE_UNLOAD                  => Tree.Unload'Access,
       TREE_LOG_MESSAGES            => Tree.Log_Messages'Access,
       TREE_INVALIDATE_SOURCE_LIST  => Tree.Invalidate_Source_List'Access,
       TREE_UPDATE_SOURCE_LIST      => Tree.Update_Source_List'Access,
       TREE_UPDATE_SOURCE_INFOS     => Tree.Update_Source_Infos'Access,
       VIEW_LOAD                    => View.Load'Access,
       VIEW_ATTRIBUTE               => View.Attribute'Access,
       VIEW_SOURCES                 => View.Sources'Access,
       VIEW_UNITS                   => View.Units'Access,
       SOURCE_DEPENDENCIES          => Source.Dependencies'Access,
       SOURCE_UPDATE_SOURCE_INFOS   => Source.Update_Source_Infos'Access);

   ----------------------
   -- GPR2_Free_Answer --
   ----------------------

   procedure GPR2_Free_Answer (Answer : C_Answer) is
      use Interfaces.C.Strings;
      Tmp : chars_ptr := chars_ptr (Answer);
   begin
      Free (Tmp);
   end GPR2_Free_Answer;

   ------------------
   -- GPR2_Request --
   ------------------

   function GPR2_Request
        (Fun : C_Function; Request : C_Request; Answer : out C_Answer)
        return C_Status
   is
   begin
      return GPR2.C.JSON.Bind (Request, Answer, Binding (Fun));
   end GPR2_Request;
end GPR2.C;
