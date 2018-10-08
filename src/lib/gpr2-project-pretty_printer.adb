------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2018, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling;

with GPR_Parser.Common;

with GPR2;
with GPR2.Parser.Project;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;

package body GPR2.Project.Pretty_Printer is

   ------------------
   -- Pretty_Print --
   ------------------

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
   is
      pragma Unreferenced (With_Comments);

      use Ada.Characters.Handling;

      use GPR_Parser.Common;

      Last_Line_Is_Empty : Boolean := False;
      --  Used to avoid two consecutive empty lines

      No_Split_String_Lit : Boolean := False;
      --  Used to indicate that the next String_Literal to be printed may
      --  or may not be split across several lines.

      Column : Natural := 0;
      --  Column number of the last character in the line. Used to avoid
      --  outputting lines longer than Max_Line_Length.

      procedure Print (Node : GPR_Node'Class; Indent : Natural);
      --  The recursive printer

      procedure Write_Indentation (Indent : Natural);
      --  Output the indentation at the beginning of the line

      procedure Write_Attribute_Name (Name : Name_Type; Indent : Natural);
      --  Write an attribute name, taking into account the value of
      --  Backward_Compatibility.

      procedure Write_Empty_Line (Always : Boolean := False);
      --  Outputs an empty line, only if the previous line was not empty
      --  already and either Always is True or Minimize_Empty_Lines is False.

      procedure Write_Name
        (Name       : Name_Type;
         Indent     : Natural;
         Capitalize : Boolean := True);
      --  Output a name, with the GNAT casing by default

      procedure Write_Project_Filename (S : String; Indent : Natural);
      --  Output a project file name in one single string literal
      --  (non-splittable: why???)

      procedure Write_Str_Lit
        (S          : String;
         Indent     : Natural;
         Splittable : Boolean := False);
      --  Use this to write string litterals that may be split / concatenated
      --  with "&" operators to fit within max line length.

      procedure Write_Token
        (S          : String;
         Indent     : Natural;
         End_Line   : Boolean := False;
         Lower_Case : Boolean := True);
      --  Use this to write tokens that are not identifiers nor string
      --  literals. Lower case by default, since we expect keywords.
      --  End_Line is only used for this utility since we will always end
      --  statements with a token (";" or "is").

      -----------
      -- Print --
      -----------

      procedure Print (Node : GPR_Node'Class; Indent : Natural) is
      begin
         if Node = No_GPR_Node then
            return;
         end if;

         case Kind (Node) is
            when GPR_Compilation_Unit =>
               --  Top level node

               Print (F_Project (Node.As_Compilation_Unit), Indent);

            when GPR_Project =>
               --  Context & project nodes

               Print (F_Context_Clauses (Node.As_Project), Indent);
               Print (F_Project_Decl (Node.As_Project), Indent);

            when GPR_With_Decl_List =>
               --  The list of "with" declarations

               if Node.Children_Count > 0 then
                  for C of Node.Children loop
                     Print (C, Indent);
                  end loop;
                  Write_Empty_Line;
               end if;

            when GPR_With_Decl =>
               --  [limited] with <name>

               case Kind (F_Is_Limited (Node.As_With_Decl)) is
                  when GPR_Limited_Present =>
                     Write_Token ("limited ", Indent);
                  when others =>
                     null;
               end case;

               Write_Token ("with ", Indent);
               No_Split_String_Lit := True;
               Print (F_Path_Names (Node.As_With_Decl), Indent);
               No_Split_String_Lit := False;
               Write_Token (";", Indent, End_Line => True);

            when GPR_String_Literal_List =>
               --  List of string literals (e.g. from F_Path_Names)

               declare
                  Count : Natural := 0;
               begin
                  for Item of Node.Children loop
                     Count := Count + 1;
                     Write_Token (Item.String_Text, Indent);

                     if Count < Node.Children_Count then
                        Write_Token (", ", Indent);
                     end if;
                  end loop;
               end;

            when GPR_Project_Declaration =>
               --  [qualifier] project [extends] <name> is
               --     ..
               --  end <name>;

               if F_Qualifier (Node.As_Project_Declaration) /= No_GPR_Node then
                  Print (F_Qualifier (Node.As_Project_Declaration), Indent);
                  Write_Token (" ", Indent);
               end if;

               Write_Token ("project ", Indent);
               Print (F_Project_Name (Node.As_Project_Declaration), Indent);
               Write_Token (" ", Indent);

               if F_Extension (Node.As_Project_Declaration) /= No_GPR_Node then
                  Print (F_Extension (Node.As_Project_Declaration), Indent);
                  Write_Token (" ", Indent);
               end if;

               Write_Token ("is", Indent, End_Line => True);
               Write_Empty_Line;

               for C of F_Decls (Node.As_Project_Declaration).Children loop
                  Print (C, Indent + Increment);
               end loop;

               Write_Empty_Line;
               Write_Token ("end ", Indent);
               Print (F_End_Name (Node.As_Project_Declaration), Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Project_Qualifier =>
               Print (F_Qualifier (Node.As_Project_Qualifier), Indent);

            when GPR_Abstract_Present =>
               Write_Token ("abstract", Indent);

            when GPR_Qualifier_Names =>
               --  qualifier1 [qualifier2]

               Write_Token
                 (F_Qualifier_Id1 (Node.As_Qualifier_Names).String_Text,
                  Indent);

               if F_Qualifier_Id2 (Node.As_Qualifier_Names) /= No_GPR_Node then
                  Write_Token (" ", Indent);
                  Write_Token
                    (F_Qualifier_Id2 (Node.As_Qualifier_Names).String_Text,
                     Indent);
               end if;

            when GPR_Prefix =>
               --  prefix[.suffix] (e.g. parent/child projects)

               Write_Name
                 (Name_Type (String'(F_Prefix (Node.As_Prefix).String_Text)),
                  Indent);

               if F_Suffix (Node.As_Prefix) /= No_GPR_Node then
                  Write_Token (".", Indent);
                  Write_Name
                    (Name_Type
                       (String'(F_Suffix (Node.As_Prefix).String_Text)),
                     Indent);
               end if;

            when GPR_Identifier =>
               --  Any identifier in the project

               Write_Name (Name_Type (String'(Node.String_Text)), Indent);

            when GPR_Project_Extension =>
               --  extends [all "<extended_proj>"]

               Write_Token ("extends ", Indent);

               if Kind (F_Is_All (Node.As_Project_Extension)) =
                 GPR_All_Qualifier_Present
               then
                  Write_Token ("all ", Indent);
               end if;

               Write_Project_Filename
                 (F_Path_Name (Node.As_Project_Extension).String_Text, Indent);

            when GPR_Attribute_Decl =>
               --  Attribute declaration node

               Write_Token ("for ", Indent);
               Write_Attribute_Name
                 (Name_Type (String'(F_Attr_Name
                  (Node.As_Attribute_Decl).String_Text)),
                  Indent);

               if F_Attr_Index (Node.As_Attribute_Decl) /= No_GPR_Node then
                  Write_Token (" (", Indent);
                  No_Split_String_Lit := True;
                  Print (F_Attr_Index (Node.As_Attribute_Decl), Indent);
                  No_Split_String_Lit := False;
                  Write_Token (")", Indent);
               end if;

               Write_Token (" use ", Indent);
               Print (F_Expr (Node.As_Attribute_Decl), Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Others_Designator =>
               Write_Token ("others", Indent);

            when GPR_String_Literal =>
               --  GPR string literal "..."
               --  Depending on the current value of No_Split_String_Lit (set
               --  by a parent call to Print) we will enable string splitting
               --  to stick to the max line length. This is especially used
               --  for project paths which are non-splittable.

               Write_Str_Lit
                 (Node.String_Text,
                  Indent,
                  Splittable => not No_Split_String_Lit);

            when GPR_String_Literal_At =>
               --  Same as above, followed by the "at ..." construct used by
               --  some attributes (Body/Spec in package Naming).

               Write_Str_Lit
                 (F_Str_Lit (Node.As_String_Literal_At).String_Text,
                  Indent,
                  Splittable => not No_Split_String_Lit);

               if F_At_Lit (Node.As_String_Literal_At) /= No_GPR_Node then
                  Write_Token (" at ", Indent);
                  Write_Token
                    (F_At_Lit (Node.As_String_Literal_At).String_Text,
                     Indent);
               end if;

            when GPR_Term_List =>
               --  List of terms separated by "&", forming an expression

               declare
                  Count : Natural := 0;
               begin
                  for C of Node.Children loop
                     Count := Count + 1;
                     Print (C, Indent);

                     if Count < Node.Children_Count then
                        Write_Token (" & ", Indent);
                     end if;
                  end loop;
               end;

            when GPR_Expr_List =>
               --  List of expressions, enclosed with parenthesis and separated
               --  by commas.

               declare
                  Count : Natural := 0;
               begin
                  Write_Token ("(", Indent);

                  for C of F_Exprs (Node.As_Expr_List).Children loop
                     Count := Count + 1;
                     Print (C, Indent);

                     if Count < F_Exprs (Node.As_Expr_List).Children_Count then
                        Write_Token (", ", Indent);
                     end if;
                  end loop;

                  Write_Token (")", Indent);
               end;

            when GPR_Builtin_Function_Call =>
               --  Builtin call term, e.g. external ("VAR", "default")

               Write_Name
                 (Name_Type
                    (String'(F_Function_Name
                     (Node.As_Builtin_Function_Call).String_Text)),
                  Indent);
               Write_Token (" ", Indent);
               No_Split_String_Lit := True;
               Print (F_Parameters (Node.As_Builtin_Function_Call), Indent);
               No_Split_String_Lit := False;

            when GPR_Variable_Reference =>
               --  Variable reference term.
               --  In the current grammar, this also covers attribute
               --  reference. The format is A[.B[.C]]['Attr].
               --  This should be modified in the future to handle unlimited
               --  parent/child project hierarchies.

               Write_Name
                 (Name_Type
                    (String'(F_Variable_Name1
                     (Node.As_Variable_Reference).String_Text)),
                  Indent);

               if F_Variable_Name2
                 (Node.As_Variable_Reference) /= No_GPR_Node
               then
                  Write_Token (".", Indent);
                  Write_Name
                    (Name_Type
                       (String'(F_Variable_Name2
                        (Node.As_Variable_Reference).String_Text)),
                     Indent);

                  if F_Variable_Name3
                    (Node.As_Variable_Reference) /= No_GPR_Node
                  then
                     Write_Token (".", Indent);
                     Write_Name
                       (Name_Type
                          (String'(F_Variable_Name3
                           (Node.As_Variable_Reference).String_Text)),
                        Indent);
                  end if;
               end if;

               if F_Attribute_Ref
                 (Node.As_Variable_Reference) /= No_GPR_Node
               then
                  Write_Token ("'", Indent);
                  Print (F_Attribute_Ref (Node.As_Variable_Reference), Indent);
               end if;

            when GPR_Attribute_Reference =>
               --  Attribute reference node, possibly with an index.
               --  Write_Attribute_Name takes care of compatibility with former
               --  GPR versions for atttribute names (e.g. replace Spec with
               --  Specification).

               Write_Attribute_Name
                 (Name_Type (F_Attribute_Name
                  (Node.As_Attribute_Reference).String_Text),
                  Indent);

               if F_Attribute_Index
                 (Node.As_Attribute_Reference) /= No_GPR_Node
               then
                  Write_Token ("(", Indent);
                  No_Split_String_Lit := True;
                  Print
                    (F_Attribute_Index (Node.As_Attribute_Reference),
                     Indent);
                  No_Split_String_Lit := False;
                  Write_Token (")", Indent);
               end if;

            when GPR_Project_Reference =>
               --  Node for the Project'... construct

               Write_Token ("Project'", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Attr_Ref
                     (Node.As_Project_Reference).String_Text)),
                  Indent);

            when GPR_Variable_Decl =>
               --  <var>[ : <type>] := <value>;

               Write_Name
                 (Name_Type
                    (String'(F_Var_Name
                     (Node.As_Variable_Decl).String_Text)),
                  Indent);

               if F_Var_Type (Node.As_Variable_Decl) /= No_GPR_Node then
                  Write_Token (" : ", Indent);
                  Print (F_Var_Type (Node.As_Variable_Decl), Indent);
               end if;

               Write_Token (" := ", Indent);
               Print (F_Expr (Node.As_Variable_Decl), Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Type_Reference =>
               --  CF above

               Write_Name
                 (Name_Type
                    (String'(F_Var_Type_Name1
                     (Node.As_Type_Reference).String_Text)),
                  Indent);

               if F_Var_Type_Name2 (Node.As_Type_Reference) /= No_GPR_Node then
                  Write_Token
                    (F_Var_Type_Name2
                       (Node.As_Type_Reference).String_Text,
                     Indent);
               end if;

            when GPR_Package_Decl =>
               --  Package declaration node (either renaming, or with a spec)

               Write_Empty_Line;
               Write_Token ("package ", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Pkg_Name
                     (Node.As_Package_Decl).String_Text)),
                  Indent);
               Write_Token (" ", Indent);
               Print (F_Pkg_Spec (Node.As_Package_Decl), Indent);
               Write_Empty_Line;

            when GPR_Package_Renaming =>
               --  Case of a package renaming

               Write_Token ("renames ", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Prj_Name
                     (Node.As_Package_Renaming).String_Text)),
                  Indent);
               Write_Token (".", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Pkg_Name
                     (Node.As_Package_Renaming).String_Text)),
                  Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Package_Spec =>
               --  Case of a package spec (may be an extending package)

               if F_Extension (Node.As_Package_Spec) /= No_GPR_Node then
                  Print (F_Extension (Node.As_Package_Spec), Indent);
                  Write_Token (" ", Indent);
               end if;

               Write_Token ("is", Indent, End_Line => True);

               for C of F_Decls (Node.As_Package_Spec).Children loop
                  Print (C, Indent + Increment);
               end loop;

               Write_Token ("end ", Indent);
               Write_Name
                 (Name_Type (String'(F_End_Name
                  (Node.As_Package_Spec).String_Text)),
                  Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Package_Extension =>
               --  Package extension

               Write_Token ("extends ", Indent);
               Write_Name
                 (Name_Type (String'(F_Prj_Name
                  (Node.As_Package_Extension).String_Text)),
                  Indent);
               Write_Token (".", Indent);
               Write_Name
                 (Name_Type (String'(F_Pkg_Name
                  (Node.As_Package_Extension).String_Text)),
                  Indent);

            when GPR_Empty_Decl =>
               Write_Token ("null;", Indent, End_Line => True);

            when GPR_Typed_String_Decl =>
               --  Node for a typed string declaration

               Write_Token ("type ", Indent);
               Write_Name
                 (Name_Type (String'(F_Type_Id
                  (Node.As_Typed_String_Decl).String_Text)),
                  Indent);
               Write_Token (" is (", Indent);
               Print (F_String_Literals (Node.As_Typed_String_Decl), Indent);
               Write_Token (");", Indent, End_Line => True);

            when GPR_Case_Construction =>
               Write_Empty_Line;
               Write_Token ("case ", Indent);
               Print (F_Var_Ref (Node.As_Case_Construction), Indent);
               Write_Token (" is ", Indent, End_Line => True);

               for C of F_Items (Node.As_Case_Construction).Children loop
                  Print (C, Indent + Increment);
               end loop;

               Write_Token ("end case;", Indent, End_Line => True);
               Write_Empty_Line;

            when GPR_Case_Item =>
               Write_Token ("when ", Indent);
               Print (F_Choice (Node.As_Case_Item), Indent);
               Write_Token (" => ", Indent);

               for C of F_Decls (Node.As_Case_Item).Children loop
                  Print (C, Indent + Increment);
               end loop;

            when GPR_Choices =>
               --  Value_1 | ... | Value_N

               declare
                  Count : Natural := 0;
               begin
                  for C of Node.Children loop
                     Count := Count + 1;
                     Print (C, Indent);

                     if Count < Node.Children_Count then
                        Write_Token (" | ", Indent);
                     end if;
                  end loop;
               end;

            when others =>
               --  Should not happen

               raise AST_Error with "unexpected construct";
         end case;
      end Print;

      --------------------------
      -- Write_Attribute_Name --
      --------------------------

      procedure Write_Attribute_Name (Name : Name_Type; Indent : Natural) is
         use GPR2.Project.Registry.Attribute;
      begin
         if Backward_Compatibility then
            if Name = Spec then
               Write_Name (Specification, Indent);
            elsif Name = Spec_Suffix then
               Write_Name (Specification_Suffix, Indent);
            elsif Name = Body_N then
               Write_Name (Implementation, Indent);
            elsif Name = Body_Suffix then
               Write_Name (Implementation_Suffix, Indent);
            else
               Write_Name (Name, Indent);
            end if;

         else
            Write_Name (Name, Indent);
         end if;
      end Write_Attribute_Name;

      ----------------------
      -- Write_Empty_Line --
      ----------------------

      procedure Write_Empty_Line (Always : Boolean := False) is
      begin
         if (Always or else not Minimize_Empty_Lines)
           and then not Last_Line_Is_Empty
         then
            Write_Eol.all;
            Column := 0;
            Last_Line_Is_Empty := True;
         end if;
      end Write_Empty_Line;

      -----------------------
      -- Write_Indentation --
      -----------------------

      procedure Write_Indentation (Indent : Natural) is
      begin
         Last_Line_Is_Empty := False;
         Write_Str ((1 .. Indent => ' '));
         Column := Indent;
      end Write_Indentation;

      ----------------
      -- Write_Name --
      ----------------

      procedure Write_Name
        (Name       : Name_Type;
         Indent     : Natural;
         Capitalize : Boolean := True)
      is
         Capital     : Boolean := Capitalize;
         Name_Buffer : constant String := String (Name);
         Name_Len    : constant Natural := Name_Buffer'Length;

      begin
         Last_Line_Is_Empty := False;

         if not Capitalize then
            Write_Token (Name_Buffer, Indent);

         else
            if Column = 0 then
               Write_Indentation (Indent);
            end if;

            --  If the line would become too long, start a new line if it helps

            if Column + Name_Len > Max_Line_Length
              and then Column > Indent + Increment
            then
               Write_Eol.all;
               Write_Indentation (Indent + Increment);
            end if;

            --  Capitalize First letter and letters following a "_"

            for J in Name_Buffer'Range loop
               if Capital then
                  Write_Char (To_Upper (Name_Buffer (J)));
               else
                  Write_Char (Name_Buffer (J));
               end if;

               if Capitalize then
                  Capital :=
                    Name_Buffer (J) = '_'
                    or else Is_Digit (Name_Buffer (J));
               end if;
            end loop;

            Column := Column + Name_Len;
         end if;
      end Write_Name;

      ----------------------------
      -- Write_Project_Filename --
      ----------------------------

      procedure Write_Project_Filename (S : String; Indent : Natural) is
      begin
         Last_Line_Is_Empty := False;
         Write_Token (S, Indent);
      end Write_Project_Filename;

      -------------------
      -- Write_Str_Lit --
      -------------------

      procedure Write_Str_Lit
        (S          : String;
         Indent     : Natural;
         Splittable : Boolean := False) is
      begin
         Last_Line_Is_Empty := False;

         if Column = 0 then
            Write_Indentation (Indent);
         end if;

         --  If the line would become too long, start a new line if it helps

         if Column + S'Length > Max_Line_Length
           and then Column > Indent + Increment
         then
            Write_Eol.all;
            Write_Indentation (Indent + Increment);
         end if;

         if not Splittable then
            Write_Str (S);
            Column := Column + S'Length;

         else
            for J in S'Range loop
               Write_Char (S (J));
               Column := Column + 1;

               --  If the string does not fit on one line, cut it in parts and
               --  concatenate.

               if J + 3 < S'Last and then Column >= Max_Line_Length - 3 then
                  Write_Str (""" &");
                  Write_Eol.all;
                  Write_Indentation (Indent + Increment);
                  Write_Char ('"');
                  Column := Column + 1;
               end if;
            end loop;
         end if;
      end Write_Str_Lit;

      -----------------
      -- Write_Token --
      -----------------

      procedure Write_Token
        (S          : String;
         Indent     : Natural;
         End_Line   : Boolean := False;
         Lower_Case : Boolean := True)
      is
         Formatted : constant String :=
                       (if Lower_Case then To_Lower (S) else S);

      begin
         --  TODO: split according to some characters (dots, spaces... spaces
         --        shouldn't be repeated on the new line)

         Last_Line_Is_Empty := False;

         if Column = 0 then
            Write_Indentation (Indent);
         end if;

         --  If the line would become too long, start a new line if it helps

         if Column + Formatted'Length > Max_Line_Length
           and then Column > Indent + Increment
         then
            Write_Eol.all;
            Write_Indentation (Indent + Increment);
         end if;

         Write_Str (Formatted);
         Column := Column + Formatted'Length;

         if End_Line then
            Write_Eol.all;
            Column := 0;
         end if;
      end Write_Token;

      Unit : Analysis_Unit := Project_Analysis_Unit;

   begin
      if Out_File_Descriptor /= Standout then
         Out_FD := Out_File_Descriptor;
         GPR.Output.Set_Special_Output (Special_Output_Proc'Access);
      end if;

      if Unit = No_Analysis_Unit then
         Unit := GPR2.Project.Definition.Get (Project_View).Trees.Project.Unit;
      end if;

      Print (Root (Unit), Initial_Indent);
   end Pretty_Print;

   -------------------------
   -- Special_Output_Proc --
   -------------------------

   procedure Special_Output_Proc (Buf : String) is
   begin
      if Write (Out_FD, Buf'Address, Buf'Length) /= Buf'Length
      then
         raise Write_Error with "write failed";
      end if;
   end Special_Output_Proc;

end GPR2.Project.Pretty_Printer;
