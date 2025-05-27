--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private package GPR2.Build.Jobserver_Protocol is

   type Object is interface;

   function Initialize (Param : String) return Object is abstract;

   function Is_Available
     (Self : Object) return Boolean is abstract;

   function Get_Token
     (Self  : Object;
      Token : out Character) return Boolean is abstract;

   procedure Release_Token
     (Self : Object;
      Token : Character) is abstract;

   procedure Finalize (Self : in out Object) is abstract;

end GPR2.Build.Jobserver_Protocol;
