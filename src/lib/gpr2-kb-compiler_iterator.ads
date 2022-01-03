------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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
