--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

with GPR2.Source_Reference.Scalar_Value;

package GPR2.Source_Reference.Attribute is
  new GPR2.Source_Reference.Scalar_Value
    (Q_Optional_Attribute_Id, No_Attribute_Id);
