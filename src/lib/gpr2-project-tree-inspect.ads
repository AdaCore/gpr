--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides subprogram to share code to convert project tree into
--  JSON between `gprinspect` utility and Ada Language Server.

with GNATCOLL.JSON; use GNATCOLL.JSON;

package GPR2.Project.Tree.Inspect is

   procedure Inspect_Project_JSON_Output
     (JSON_Res                  : GNATCOLL.JSON.JSON_Value;
      Tree                      : GPR2.Project.Tree.Object;
      All_Projects              : Boolean;
      Display_Everything        : Boolean := False;
      Display_Attributes        : Boolean := False;
      Display_Config_Attributes : Boolean := False;
      Display_Packages          : Boolean := False;
      Display_Variables         : Boolean := False)
   with Pre => JSON_Res.Kind = JSON_Object_Type;
   --  Inspect project and possibly recursively all imports

end GPR2.Project.Tree.Inspect;
