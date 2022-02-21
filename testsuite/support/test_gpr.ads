------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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
with GPR2.Project.View;
with GPR2.Containers;

package Test_GPR is

   procedure Load_With_No_Errors
      (Tree             : in out GPR2.Project.Tree.Object;
       Filename         : String;
       Context          : GPR2.Context.Object := GPR2.Context.Empty;
       Config_Filename  : String := "";
       Load_Source_List : Boolean := False);
   --  Load a project file and assert that no errors is found during loading.
   --  if an error is found during project tree load then GPR2.Project_Error
   --  is raised.
   --
   --  Filename: path to the project file to load
   --  Context: context to pass to the project file
   --  Config_Filename: Config file filename to load

   procedure Assert_Variable
      (View     : GPR2.Project.View.Object;
       Variable : String;
       Value    : String);
   --  Check if variable Variable in project View of Tree has the right value.
   --  If the project has been loaded inside an aggregate set Aggregate_Context
   --  to True.

   procedure Assert_Attribute
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index;
       Value                : String);

   procedure Assert_Attribute
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index;
       Value                : GPR2.Containers.Name_List);

   procedure Assert_Attribute_Not_Defined
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index);

   procedure Assert_Attribute_Error
      (View                 : GPR2.Project.View.Object;
       Name                 : String;
       Pkg                  : String  := "";
       Index                : String  := "";
       Index_Case_Sensitive : Boolean := True;
       At_Pos               : GPR2.Unit_Index := GPR2.No_Index);

end Test_GPR;
