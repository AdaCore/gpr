--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package represents an entity source reference with an associated
--  message. This is mostly to report warnings/errors while parsing sources.

with GPR2.Source_Reference.Scalar_Value;

package GPR2.Source_Reference.Pack is
  new GPR2.Source_Reference.Scalar_Value (Package_Id, Project_Level_Scope);
