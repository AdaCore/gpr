--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Environment;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;

package GPR2.Configuration_Internal is

   procedure Resolve_Runtime_Dir
     (Settings     : in out GPR2.Project.Configuration.Description;
      Project_Path : GPR2.Path_Name.Object;
      Environment  : GPR2.Environment.Object;
      Message      : out GPR2.Message.Object);

end GPR2.Configuration_Internal;
