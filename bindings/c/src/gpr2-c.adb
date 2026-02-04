--
--  Copyright (C) 2020-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON;
with GPR2.C.Tree;
with GPR2.C.View;

package body GPR2.C is

   type Binding_Map is array (C_Function range <>) of GPR2.C.JSON.Bind_Handler;

   Binding : constant Binding_Map :=
     (TREE_ADA_CLOSURE         => Tree.Ada_Closure'Access,
      TREE_ARTIFACTS_DIRECTORY => Tree.Artifacts_Directory'Access,
      TREE_CONTEXT             => Tree.Context'Access,
      TREE_DESTRUCTOR          => Tree.Destructor'Access,
      TREE_ITERATE             => Tree.Iterate'Access,
      TREE_LOAD                => Tree.Load'Access,
      TREE_LOG_MESSAGES        => Tree.Log_Messages'Access,
      TREE_ROOT_PROJECT        => Tree.Root_Project'Access,
      TREE_RUNTIME_PROJECT     => Tree.Runtime_Project'Access,
      TREE_SET_CONTEXT         => Tree.Set_Context'Access,
      TREE_TARGET              => Tree.Target'Access,
      TREE_UPDATE_SOURCES      => Tree.Update_Sources'Access,
      VIEW_CONSTRUCTOR         => View.Constructor'Access,
      VIEW_DESTRUCTOR          => View.Destructor'Access,
      VIEW_EXECUTABLES         => View.Executables'Access,
      VIEW_OBJECT_DIRECTORY    => View.Object_Directory'Access,
      VIEW_SOURCES             => View.Sources'Access,
      VIEW_VISIBLE_SOURCES     => View.Visible_Sources'Access);

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
