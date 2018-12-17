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

--  Pretty Printer for a GPR2 project

with Ada.Strings.Unbounded;

with GPR_Parser.Analysis;

with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.Project.Pretty_Printer is

   use Ada;

   use GPR_Parser;
   use GPR_Parser.Analysis;

   type Object is tagged private;

   subtype Max_Length_Of_Line is Positive range 50 .. 255;

   function Create
     (With_Comments          : Boolean            := True;
      Initial_Indent         : Natural            := 0;
      Increment              : Positive           := 3;
      Max_Line_Length        : Max_Length_Of_Line := 80;
      Minimize_Empty_Lines   : Boolean            := False;
      Backward_Compatibility : Boolean            := False) return Object;
   --  Creates a pretty-printer object.
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

   procedure Pretty_Print
     (Self            : in out Object;
      View            : Project.View.Object    := Project.View.Undefined;
      Analysis_Unit   : Analysis.Analysis_Unit := No_Analysis_Unit;
      Write_Character : access procedure (C : Character) := null;
      Write_String    : access procedure (S : String)    := null;
      Write_EOL       : access procedure                 := null)
     with Pre => (View.Is_Defined
                  or else Analysis_Unit /= No_Analysis_Unit)
                    and then
                 ((Write_Character = null
                   and then  Write_String = null
                   and then Write_EOL = null)
                  or else
                    (Write_Character /= null
                     and then  Write_String /= null
                     and then Write_EOL /= null));
   --  Pretty prints the project, either uses the specified Write_* procedures,
   --  or creates string representation in memory.

   function Result (Self : Object) return String;
   --  Returns the result of the pretty printer

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      With_Comments          : Boolean            := True;
      Initial_Indent         : Natural            := 0;
      Increment              : Positive           := 3;
      Max_Line_Length        : Max_Length_Of_Line := 80;
      Minimize_Empty_Lines   : Boolean            := False;
      Backward_Compatibility : Boolean            := False;
      Buffer                 : Unbounded_String;
   end record;

end GPR2.Project.Pretty_Printer;
