------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2016-2017, Free Software Foundation, Inc.         --
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

with GPR.Compilation.Slave;

with GPR2.Compilation.Protocol;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.Compilation.Slave is

   procedure Register_Remote_Slaves
     (Tree               : GPR2.Project.Tree.Object;
      Synchronize        : Boolean);
   --  Same as above. Synchronize is set to true if the project has to be
   --  synchronized to the remote slave.

   function Compute_Env
     (Tree : GPR2.Project.Tree.Object; Auto : Boolean) return String;
   --  Compute a slave environment based on the command line parameter and
   --  the project variables. We want the same slave environment for identical
   --  build. Data is a string that must be taken into account in the returned
   --  value.

   function Remote_Root_Directory
     (Project : GPR2.Project.View.Object) return String;
   --  Returns the root directory for the project taking into account the
   --  remote package Root_Dir attribute. That is, this is the root directory
   --  used for the synchronization for example.

   procedure Unregister_Remote_Slaves (From_Signal : Boolean := False)
     renames GPR.Compilation.Slave.Unregister_Remote_Slaves;

   function Channel (Host : String) return Protocol.Communication_Channel
     renames GPR.Compilation.Slave.Channel;

   procedure Record_Slaves (Option : String)
     renames GPR.Compilation.Slave.Record_Slaves;

end GPR2.Compilation.Slave;
