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

with GPR.Output;

with GPR_Parser.Analysis;
with GPR_Parser.Common;

with GPR2;
with GPR2.Parser.Project;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;

package body GPR2.Project.Pretty_Printer is

   use Ada.Characters.Handling;

   use GPR.Output;

   use GPR_Parser.Analysis;

   procedure Write_Char_Default (C : Character);
   procedure Write_Str_Default (S : String);
   procedure Write_Eol_Default;

   ------------------
   -- Pretty_Print --
   ------------------

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
      W_Str                  : Write_Str_Ap       := null)
   is
      pragma Unreferenced (With_Trivia);

      use GPR_Parser.Common;

      procedure Print (Node : GPR_Node'Class; Indent : Natural);
      --  The recursive printer

      --  Some printing utils  --

      procedure Start_Line (Indent : Natural);
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
      --  Outputs a name, with the GNAT casing by default

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
        (S        : String;
         Indent   : Natural;
         End_Line : Boolean := False);
      --  Use this to write tokens that are not identifiers nor string literals

      Write_Char : Write_Char_Ap := Write_Char_Default'Access;
      Write_Eol  : Write_Eol_Ap := Write_Eol_Default'Access;
      Write_Str  : Write_Str_Ap := Write_Str_Default'Access;
      --  These three access to procedure values are used for the output

      Last_Line_Is_Empty : Boolean := False;
      --  Used to avoid two consecutive empty lines

      No_Split_String_Lit : Boolean := False;
      --  Used to indicate that the next String_Literal no

      Column : Natural := 0;
      --  Column number of the last character in the line. Used to avoid
      --  outputting lines longer than Max_Line_Length.

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
               --  context & project nodes

               Print (F_Context_Clauses (Node.As_Project), Indent);
               Write_Empty_Line (Always => True);
               Print (F_Project_Decl (Node.As_Project), Indent);

            when GPR_With_Decl_List =>
               if Node.Children_Count > 0 then
                  for C of Node.Children loop
                     Print (C, Indent);
                  end loop;
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

               for C of F_Decls (Node.As_Project_Declaration).Children loop
                  Print (C, Indent + Increment);
               end loop;

               Write_Token ("end ", Indent);
               Print (F_End_Name (Node.As_Project_Declaration), Indent);
               Write_Token (";", Indent, End_Line => True);

            when GPR_Project_Qualifier =>
               Print (F_Qualifier (Node.As_Project_Qualifier), Indent);

            when GPR_Abstract_Present =>
               Write_Token ("abstract", Indent);

            when GPR_Qualifier_Names =>
               Write_Token
                 (F_Qualifier_Id1 (Node.As_Qualifier_Names).String_Text,
                  Indent);

               if F_Qualifier_Id2 (Node.As_Qualifier_Names) /= No_GPR_Node then
                  Write_Token (" ", Indent);
                  Write_Token (F_Qualifier_Id2
                               (Node.As_Qualifier_Names).String_Text,
                               Indent);
               end if;

            when GPR_Prefix =>
               Write_Name
                 (Name_Type (String'(F_Prefix (Node.As_Prefix).String_Text)),
                  Indent);

               if F_Suffix (Node.As_Prefix) /= No_GPR_Node then
                  Write_Token (".", Indent);
                  Write_Name (Name_Type (String'
                                (F_Suffix (Node.As_Prefix).String_Text)),
                              Indent);
               end if;

            when GPR_Project_Extension =>
               Write_Token ("extends ", Indent);

               if Kind (F_Is_All (Node.As_Project_Extension)) =
                 GPR_All_Qualifier_Present
               then
                  Write_Token ("all ", Indent);
               end if;

               Write_Project_Filename
                 (F_Path_Name (Node.As_Project_Extension).String_Text, Indent);

            when GPR_Attribute_Decl =>
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
               Write_Str_Lit
                 (Node.String_Text,
                  Indent,
                  Splittable => not No_Split_String_Lit);

            when GPR_String_Literal_At =>
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
               Write_Token ("Project'", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Attr_Ref
                      (Node.As_Project_Reference).String_Text)),
                  Indent);

            when GPR_Variable_Decl =>
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
               Write_Empty_Line (Always => True);
               Write_Token ("package ", Indent);
               Write_Name
                 (Name_Type
                    (String'(F_Pkg_Name
                      (Node.As_Package_Decl).String_Text)),
                  Indent);
               Write_Token (" ", Indent);
               Print (F_Pkg_Spec (Node.As_Package_Decl), Indent);
               Write_Empty_Line (Always => True);

            when GPR_Package_Renaming =>
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
               Write_Token ("type ", Indent);
               Write_Name
                 (Name_Type (String'(F_Type_Id
                   (Node.As_Typed_String_Decl).String_Text)),
                  Indent);
               Write_Token (" is (", Indent);
               Print (F_String_Literals (Node.As_Typed_String_Decl), Indent);
               Write_Token (");", Indent, End_Line => True);

            when GPR_Case_Construction =>
               Write_Empty_Line (Always => True);
               Write_Token ("case ", Indent);
               Print (F_Var_Ref (Node.As_Case_Construction), Indent);
               Write_Token (" is ", Indent, End_Line => True);

               for C of F_Items (Node.As_Case_Construction).Children loop
                  Print (C, Indent + Increment);
               end loop;

               Write_Token ("end case;", Indent, End_Line => True);
               Write_Empty_Line (Always => True);

            when GPR_Case_Item =>
               Write_Token ("when ", Indent);
               Print (F_Choice (Node.As_Case_Item), Indent);
               Write_Token (" => ", Indent);

               for C of F_Decls (Node.As_Case_Item).Children loop
                  Print (C, Indent + Increment);
               end loop;

            when GPR_Choices =>
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
               null;
         end case;
      end Print;

      ----------------
      -- Start_Line --
      ----------------

      procedure Start_Line (Indent : Natural) is
      begin
         Write_Str ((1 .. Indent => ' '));
         Column := Indent;
      end Start_Line;

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
         if not Capitalize then
            Write_Token (Name_Buffer, Indent);

         else
            if Column = 0 then
               Start_Line (Indent);
            end if;

            --  If the line would become too long, start a new line if it helps

            if Column + Name_Len > Max_Line_Length
              and then Column > Indent + Increment
            then
               Write_Eol.all;
               Start_Line (Indent + Increment);
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

      -------------------------
      -- Output_Project_File --
      -------------------------

      procedure Write_Project_Filename (S : String; Indent : Natural) is
      begin
         Write_Token (S, Indent);
      end Write_Project_Filename;

      -------------------
      -- Output_String --
      -------------------

      procedure Write_Str_Lit
        (S          : String;
         Indent     : Natural;
         Splittable : Boolean := False) is
      begin
         if Column = 0 then
            Start_Line (Indent);
         end if;

         --  If the line would become too long, start a new line if it helps

         if Column + S'Length > Max_Line_Length
           and then Column > Indent + Increment
         then
            Write_Eol.all;
            Start_Line (Indent + Increment);
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
                  Start_Line (Indent + Increment);
                  Write_Char ('"');
                  Column := Column + 1;
               end if;
            end loop;
         end if;
      end Write_Str_Lit;

      ------------------
      -- Write_String --
      ------------------

      procedure Write_Token
        (S        : String;
         Indent   : Natural;
         End_Line : Boolean := False) is
      begin
         --  TODO: split according to some characters (dots, spaces... spaces
         --        shouldn't be repeated on the new line)

         if Column = 0 then
            Start_Line (Indent);
         end if;

         --  If the line would become too long, start a new line if it helps

         if Column + S'Length > Max_Line_Length
           and then Column > Indent + Increment
         then
            Write_Eol.all;
            Start_Line (Indent + Increment);
         end if;

         Write_Str (S);
         Column := Column + S'Length;

         if End_Line then
            Last_Line_Is_Empty := False;
            Write_Eol.all;
            Column := 0;
         end if;
      end Write_Token;

      Unit : constant Analysis_Unit := GPR2.Project.Definition.Get
        (Project_View).Trees.Project.Unit;

   begin
      if W_Char = null then
         Write_Char := Write_Char_Default'Access;
      else
         Write_Char := W_Char;
      end if;

      if W_Eol = null then
         Write_Eol := Write_Eol_Default'Access;
      else
         Write_Eol := W_Eol;
      end if;

      if W_Str = null then
         Write_Str := Write_Str_Default'Access;
      else
         Write_Str := W_Str;
      end if;

      Print (Root (Unit), Initial_Indent);
   end Pretty_Print;

   ---------
   -- wpr --
   ---------

   procedure wpr (Tree : GPR2.Project.Tree.Object) is
   begin
      Pretty_Print (Tree.Root_Project);
   end wpr;

   ------------------------
   -- Write_Char_Default --
   ------------------------

   procedure Write_Char_Default (C : Character) is
   begin
      Write_Char (C);
   end Write_Char_Default;

   -----------------------
   -- Write_Eol_Default --
   -----------------------

   procedure Write_Eol_Default is
   begin
      Write_Eol;
   end Write_Eol_Default;

   -----------------------
   -- Write_Str_Default --
   -----------------------

   procedure Write_Str_Default (S : String) is
   begin
      Write_Str (S);
   end Write_Str_Default;

end GPR2.Project.Pretty_Printer;
