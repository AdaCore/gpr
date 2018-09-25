------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2018, Free Software Foundation, Inc.              --
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

--  Pretty Printer for a GPR2 project, represented by its view object.

with GPR2.Project;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.Project.Pretty_Printer is

   type Write_Char_Ap is access procedure (C : Character);

   type Write_Eol_Ap  is access procedure;

   type Write_Str_Ap is access procedure (S : String);

   subtype Max_Length_Of_Line is Positive range 50 .. 255;

   type Trivia_Level is (None, Comments);

   procedure Pretty_Print
     (Project_View           : GPR2.Project.View.Object;
      With_Trivia            : Trivia_Level       := Comments;
      Initial_Indent         : Natural            := 0;
      Increment              : Positive           := 3;
      Max_Line_Length        : Max_Length_Of_Line := 80;
      Minimize_Empty_Lines   : Boolean            := False;
      Backward_Compatibility : Boolean            := False;
      W_Char                 : Write_Char_Ap      := null;
      W_Eol                  : Write_Eol_Ap       := null;
      W_Str                  : Write_Str_Ap       := null);
   --  Output a project file, using either the default output routines (stdout)
   --  or the ones specified by W_Char, W_Eol and W_Str.
   --
   --  With_Trivia should be used if we have comments and formatting that we
   --  would like to keep.
   --
   --  Initial_Indent is the initial indentation.
   --
   --  Increment is the number of spaces for each indentation level.
   --
   --  Max_Line_Length is the maximum line length in the project file
   --
   --  If Minimize_Empty_Lines is True, empty lines will be output only after
   --  the last with clause, after the line declaring the project name, after
   --  the last declarative item of the project and before each package
   --  declaration. Otherwise, more empty lines are output.
   --
   --  If Backward_Compatibility is True, then new attributes (Spec,
   --  Spec_Suffix, Body, Body_Suffix) will be replaced by obsolete ones
   --  (Specification, Specification_Suffix, Implementation,
   --  Implementation_Suffix).

private

   procedure wpr (Tree : GPR2.Project.Tree.Object);
   --  Wrapper for use from gdb: call Pretty_Print with default parameters

end GPR2.Project.Pretty_Printer;
