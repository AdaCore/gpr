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
     (Descriptor   : in out GPR2.Project.Configuration.Description;
      Project_Path : GPR2.Path_Name.Object;
      Environment  : GPR2.Environment.Object;
      Message      : out GPR2.Message.Object);
   --  Resolve runtime path component of the configuration descriptor.
   --
   --  @param Descriptor
   --     Configuration descriptor to resolve runtime path.
   --  @param Project_Path
   --     Path name of the project file. When defined, it is used to resolve
   --     runtime and to report error.
   --  @param Environment
   --     Set of environment variables to obtain value of the GPR_PROJECT_PATH
   --  @param Message
   --     Set to error message when error is detected, otherwise it is set to
   --     Undefined.

end GPR2.Configuration_Internal;
