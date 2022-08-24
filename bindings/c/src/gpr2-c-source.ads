--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.C.JSON; use GPR2.C.JSON;

package GPR2.C.Source is

   procedure Update_Source_Infos (Request : JSON_Value; Result : JSON_Value);

   procedure Dependencies (Request : JSON_Value; Result : JSON_Value);

end GPR2.C.Source;
