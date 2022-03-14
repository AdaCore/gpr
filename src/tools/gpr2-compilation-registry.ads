------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package keep records of all active slaves. The slaves are recorded
--  into Slaves_Data and used as compilation node when they are free.

with GPR2.Containers;
with GPR2.Compilation.Protocol;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GPRtools.Options;

package GPR2.Compilation.Registry is

   procedure Register_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Options     : GPRtools.Options.Base_Options'Class;
      Synchronize : Boolean);
   --  Initialize the remotes slaves. That is, the comunication channel between
   --  the recorded slaves and the remote hosts is setup. Synchronize is set to
   --  true if the project has to be synchronized to the remote slaves.

   procedure Unregister_Remote_Slaves
     (Tree        : GPR2.Project.Tree.Object;
      Options     : GPRtools.Options.Base_Options'Class;
      From_Signal : Boolean := False);
   --  Unregister all slaves, send them notification about the end of the
   --  current build. This routine must be called after the compilation phase
   --  and before the bind and link ones. It is safe to call this routine
   --  multiple times, the first call will do the clean-up, next calls are
   --  just no-op. From_Signal must be set when called from a signal, for
   --  example when calling this routine from the ctrl-c handler.

   function Compute_Env
     (Tree : GPR2.Project.Tree.Object; Auto : Boolean) return String;
   --  Compute a slave environment based on the command line parameter and
   --  the project variables. We want the same slave environment for identical
   --  build. Data is a string that must be taken into account in the returned
   --  value.

   function Get_Hosts return Containers.Name_List;
   --  Return all slave hosts as defined by the environment (variables
   --  GPR_SLAVES or GPR_SLAVES_FILE) or the empty string if no definition
   --  found.

   function Remote_Root_Directory
     (Project : GPR2.Project.View.Object) return String;
   --  Returns the root directory for the project taking into account the
   --  remote package Root_Dir attribute. That is, this is the root directory
   --  used for the synchronization for example.

   function Channel (Host : Name_Type) return Protocol.Communication_Channel;

   procedure Record_Slaves (Slaves : Containers.Name_List);
   --  Records all slaves

   procedure Clean_Up_Remote_Slaves
     (Project : GPR2.Project.View.Object;
      Options : GPRtools.Options.Base_Options'Class);
   --  Send a clean-up request to all remote slaves. The slaves are then asked
   --  to remove all the sources and build artifacts for the given project.

   function Get_Max_Processes return Natural;
   --  Returns the maximum number of processes supported by the compilation
   --  engine. This is the sum of the parallel local builds as specified by
   --  the -j option and all the sum of the processes supported by each slaves.

   function Run
     (Project  : GPR2.Project.View.Object;
      Language : Language_Id;
      Options  : Containers.Value_List;
      Obj_Name : Name_Type;
      Dep_Name : String := "";
      Env      : String := "") return Compilation.Id;
   --  Send a compilation job to one slave that has still some free slot. There
   --  is also free slot when this routine is called (gprbuild ensure this).

end GPR2.Compilation.Registry;
