--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Gpr_Parser_Support.Adalog.Generic_Main_Support;
with Gpr_Parser_Support.Images;

package Gpr_Parser_Support.Adalog.Main_Support
is new Gpr_Parser_Support.Adalog.Generic_Main_Support
  (Integer, Gpr_Parser_Support.Images.Stripped_Image);
