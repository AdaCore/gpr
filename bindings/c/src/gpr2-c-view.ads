--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Objects;

package GPR2.C.View is

   pragma Elaborate_Body;

   procedure Constructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements VIEW_DESTRUCTOR

   procedure Destructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements VIEW_DESTRUCTOR

   procedure Executables
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements VIEW_EXECUTABLES

   procedure Object_Directory
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements VIEW_OBJECT_DIRECTORY

   procedure Sources
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);
   --  Implements VIEW_SOURCES

end GPR2.C.View;
