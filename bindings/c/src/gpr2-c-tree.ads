--
--  Copyright (C) 2020-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  C Binding to GPR2 Project Trees API

with GPR2.C.JSON.Objects;

package GPR2.C.Tree is

   pragma Elaborate_Body;

   procedure Ada_Source_Closure
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_ADA_CLOSURE

   procedure Artifacts_Directory
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_ARTIFACTS_DIRECTORY

   procedure Context
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_CONTEXT

   procedure Destructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_DESTRUCTOR

   procedure Iterate
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_ITERATE

   procedure Load
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_LOAD

   procedure Log_Messages
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_LOG_MESSAGES

   procedure Root_Project
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_ROOT_PROJECT

   procedure Runtime_Project
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_RUNTIME_PROJECT

   procedure Set_Context
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_SET_CONTEXT

   procedure Target
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_TARGET

   procedure Update_Sources
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements TREE_UPDATE_SOURCES

end GPR2.C.Tree;
