--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

function GPR2.Project.Parser.Create
  (Name      : Name_Type;
   File      : GPR2.Path_Name.Object;
   Qualifier : Project_Kind) return Object;
--  Simple constructor for a project object. This is designed to be used by
--  built-in project like the runtime one.
