--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private with GNATCOLL.OS.FS;

package GPR2.Build.Jobserver_Protocol.Pipe is

   type Object is new Jobserver_Protocol.Object with private;

   overriding function Initialize (Param : String) return Object;
   --  Initialize a named pipe protocol

   function Initialize (R, W : Integer) return Object;
   --  Initialize a simple pipe protocol

   overriding function Is_Available (Self : Object) return Boolean;

   overriding function Get_Token
     (Self  : Object;
      Token : out Character) return Boolean;

   overriding procedure Release_Token
     (Self  : Object;
      Token : Character);

   overriding procedure Finalize (Self : in out Object);

private

   type Object is new Jobserver_Protocol.Object with record
      Read_FD, Write_FD : GNATCOLL.OS.FS.File_Descriptor :=
                            GNATCOLL.OS.FS.Invalid_FD;
      --  File descriptor to read from and write to
   end record;

end GPR2.Build.Jobserver_Protocol.Pipe;
