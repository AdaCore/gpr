--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Compiler iterator

package GPR2.KB.Compiler_Iterator is

   type Object is abstract tagged null record;
   --  An iterator that searches for all known compilers in a list of
   --  directories. Whenever a new compiler is found, the Callback primitive
   --  operation is called.

   procedure Callback
     (Self              : in out Object;
      Base              : in out KB.Object;
      Comp              : KB.Compiler;
      Runtime_Specified : Boolean;
      From_Extra_Dir    : Boolean;
      Continue          : out Boolean) is abstract;
   --  Called whenever a new compiler is discovered.
   --  It might be discovered either in a path added through a --config
   --  parameter (in which case From_Extra_Dir is True), or in a path specified
   --  in the environment variable $PATH (in which case it is False). If the
   --  directory is both in Extra_Dirs and in $PATH, From_Extra_Dir is set to
   --  False.
   --  If Runtime_Specified is True, only filters with a specified runtime are
   --  applied.
   --
   --  On exit, Continue should be set to False if there is no need to discover
   --  further compilers (however there will be no possibility to restart the
   --  search at the same point later on).

   procedure Foreach_In_Path
     (Self       : in out Object'Class;
      Base       : in out KB.Object;
      On_Target  : Name_Type;
      Extra_Dirs : String := "");
   --  Find all compilers in "Extra_Dirs & $PATH".
   --  Extra_Dirs should typically be the list of directories coming from
   --  GPR2.Project.Configuration.Description parameters.
   --  The only filtering done is the target, for optimization purposes (no
   --  need to compute all info about the compiler if we know it will not be
   --  used anyway).

end GPR2.KB.Compiler_Iterator;
