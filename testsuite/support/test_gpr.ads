------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
with GPR2.Context;
with GPR2.Project.Tree;

package Test_GPR is

   procedure Load_With_No_Errors
      (Tree     : in out GPR2.Project.Tree.Object;
       Filename : String;
       Context  : GPR2.Context.Object := GPR2.Context.Empty);
   --  Load a project file and assert that no errors is found during loading.
   --  if an error is found during project tree load then GPR2.Project_Error
   --  is raised.
   --
   --  Filename: path to the project file to load
   --  Context: context to pass to the project file

   procedure Assert_Variable
      (Tree     : GPR2.Project.Tree.Object;
       View     : String;
       Variable : String;
       Value    : String;
       Aggregate_Context : Boolean := False);
   --  Check if variable Variable in project View of Tree has the right value.
   --  If the project has been loaded inside an aggregate set Aggregate_Context
   --  to True.
end Test_GPR;
