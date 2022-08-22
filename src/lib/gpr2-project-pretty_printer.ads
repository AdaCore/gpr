--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Pretty Printer for a GPR2 project

with Gpr_Parser.Analysis;

with GPR2.Project.View;

package GPR2.Project.Pretty_Printer is

   use Gpr_Parser;
   use Gpr_Parser.Analysis;

   type Object is tagged private;

   subtype Line_Length is Positive range 50 .. 255;

   function Create
     (With_Comments          : Boolean     := True;
      Initial_Indent         : Natural     := 0;
      Increment              : Positive    := 3;
      Max_Line_Length        : Line_Length := 80;
      Minimize_Empty_Lines   : Boolean     := False;
      Backward_Compatibility : Boolean     := False) return Object;
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

   type Object is tagged record
      With_Comments          : Boolean           := True;
      Initial_Indent         : Natural           := 0;
      Increment              : Positive          := 3;
      Max_Line_Length        : Line_Length       := 80;
      Minimize_Empty_Lines   : Boolean           := False;
      Backward_Compatibility : Boolean           := False;
      Buffer                 : Unbounded_String;
   end record;

end GPR2.Project.Pretty_Printer;
