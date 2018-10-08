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

with GNAT.OS_Lib;

with GPR.Output;

with GPR_Parser.Analysis;

with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.Project.Pretty_Printer is

   use GNAT.OS_Lib;

   use GPR_Parser.Analysis;

   use GPR2.Project.View;

   type Write_Char_Ap is access procedure (C : Character);
   type Write_Str_Ap  is access procedure (S : String);
   type Write_Eol_Ap  is access procedure;

   --
   --  Default Write {Char,Eol,Str} procedures.
   --
   --  At the moment they are borrowed from GPR, with a non-object
   --  implementation which may cause problems if we instantiate multiple
   --  Pretty_Printer objects.
   --  We need to reimplement this in GPR2 in an OO fashion.
   --

   procedure Write_Char_Default (C : Character) renames GPR.Output.Write_Char;
   procedure Write_Str_Default (S : String) renames GPR.Output.Write_Str;
   procedure Write_Eol_Default renames GPR.Output.Write_Eol;

   subtype Max_Length_Of_Line is Positive range 50 .. 255;

   Write_Error : exception;
   AST_Error   : exception;

   procedure Pretty_Print
     (Project_View           : View.Object        := View.Undefined;
      Project_Analysis_Unit  : Analysis_Unit      := No_Analysis_Unit;
      With_Comments          : Boolean            := True;
      Initial_Indent         : Natural            := 0;
      Increment              : Positive           := 3;
      Max_Line_Length        : Max_Length_Of_Line := 80;
      Minimize_Empty_Lines   : Boolean            := False;
      Backward_Compatibility : Boolean            := False;
      Write_Char             : Write_Char_Ap      := Write_Char_Default'Access;
      Write_Eol              : Write_Eol_Ap       := Write_Eol_Default'Access;
      Write_Str              : Write_Str_Ap       := Write_Str_Default'Access;
      Out_File_Descriptor    : File_Descriptor    := Standout)
     with Pre => Project_View /= View.Undefined or else
                 Project_Analysis_Unit /= No_Analysis_Unit;
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

   procedure Special_Output_Proc (Buf : String);
   --  When using the default Write_* and a custom Out_File_Descriptor,
   --  we use this to force the underlying GPR utilities to write to
   --  Out_File_Descriptor instead of standard files.

   Out_FD : File_Descriptor;

end GPR2.Project.Pretty_Printer;
