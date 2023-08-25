--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build is

   type Unit_Kind is (S_Spec, S_Body, S_Separate, S_No_Body);
   --  Used to identify the kind of source or unit is manipulated:
   --  S_Spec: spec or header
   --  S_Body: unit body or implementation source
   --  S_Separate: unit separated from a body
   --  S_No_Body: a body containing the pragma No_Body

   subtype Valid_Unit_Kind is Unit_Kind range S_Spec .. S_Separate;

   function Image (Kind : Valid_Unit_Kind) return String;

private

   function Image (Kind : Valid_Unit_Kind) return String is
     (case Kind is
         when S_Spec => "spec",
         when S_Body => "body",
         when S_Separate => "separate");

end GPR2.Build;
