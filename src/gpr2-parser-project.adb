------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with Ada.Characters.Conversions;

with Langkit_Support.Tokens;

with GPR_Parser;
with GPR_Parser.AST;       use GPR_Parser.AST;
with GPR_Parser.AST.Types; use GPR_Parser.AST.Types;

with GPR2.Parser.Registry;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;
with GPR2.Project.View;

package body GPR2.Parser.Project is

   use type Ada.Containers.Count_Type;
   use type Context.Binary_Signature;

   --  Some helpers routines for the parser

   function Get_Name_Type
     (Node : not null access Single_Tok_Node_Type'Class) return Name_Type;
   --  Returns the Name for the given node

   function Present (Node : access GPR_Node_Type'Class) return Boolean is
     (Node /= null);
   --  Returns True if the Node is present (not null)

   ---------------
   -- Externals --
   ---------------

   function Externals (Self : Object) return Containers.Name_List is
   begin
      return Self.Externals;
   end Externals;

   -------------------
   -- Get_Name_Type --
   -------------------

   function Get_Name_Type
     (Node : not null access Single_Tok_Node_Type'Class) return Name_Type
   is
      use Ada.Characters.Conversions;
      V      : constant Wide_Wide_String :=
                 F_Tok (Single_Tok_Node (Node)).Text.all;
      Offset : Natural := 0;
   begin
      if V (V'First) = '"' and then V (V'Last) = '"' then
         Offset := 1;
      end if;
      return To_String (V (V'First + Offset .. V'Last - Offset));
   end Get_Name_Type;

   -------------------
   -- Has_Externals --
   -------------------

   function Has_Externals (Self : Object) return Boolean is
   begin
      return Self.Externals.Length > 0;
   end Has_Externals;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
   begin
      return Self.Imports.Length > 0;
   end Has_Imports;

   -------------
   -- Imports --
   -------------

   function Imports (Self : Object) return Containers.Path_Name_List is
   begin
      return Self.Imports;
   end Imports;

   ----------
   -- Load --
   ----------

   function Load (Filename : Path_Name_Type) return Object is

      use GPR_Parser;
      use Langkit_Support;
      use Langkit_Support.Tokens;
      use GPR_Parser;

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object;
      --  Analyse project, record all externals variables and imports

      -------------------
      -- Parse_Stage_1 --
      -------------------

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object is

         Project : Object;
         --  The project being constructed

         function Parser (Node : GPR_Node) return Visit_Status;
         --  Actual parser callabck for the project

         ------------
         -- Parser --
         ------------

         function Parser (Node : GPR_Node) return Visit_Status is
            Status : constant Visit_Status := Into;

            procedure Parse_Project_Declaration (N : Project_Declaration);
            --  Parse a project declaration and set the qualifier if present

            procedure Parse_External_Reference (N : External_Reference);
            --  A the name of the external into the Externals list

            procedure Parse_With_Decl (N : With_Decl);
            --  Add the name of the withed project into the Imports list

            ------------------------------
            -- Parse_External_Reference --
            ------------------------------

            procedure Parse_External_Reference (N : External_Reference) is
               Var : constant String_Literal := F_String_Lit (N);
            begin
               Project.Externals.Append (Get_Name_Type (Var));
            end Parse_External_Reference;

            -------------------------------
            -- Parse_Project_Declaration --
            -------------------------------

            procedure Parse_Project_Declaration (N : Project_Declaration) is
               Name : constant Expr := F_Project_Name (N);
               Qual : constant Types.Project_Qualifier := F_Qualifier (N);
            begin
               --  If we have an explicit qualifier parse it now. If not the
               --  kind of project will be determined later during a second
               --  pass.

               if Present (Qual) then
                  case AST.Kind (F_Qualifier (Qual)) is
                     when Abstract_Present_Kind =>
                        Project.Qualifier := K_Abstract;

                     when Qualifier_Names_Kind =>
                        declare
                           Names : constant Qualifier_Names :=
                                     Qualifier_Names (F_Qualifier (Qual));
                           Name_1 : constant Identifier :=
                                      F_Qualifier_Id1 (Names);
                           Str_1  : constant String :=
                                      (if Name_1 = null
                                       then ""
                                       else Get_Name_Type
                                         (Single_Tok_Node (Name_1)));
                           Name_2 : constant Identifier :=
                                      F_Qualifier_Id2 (Names);
                           Str_2  : constant String :=
                                      (if Name_2 = null
                                       then ""
                                       else Get_Name_Type
                                         (Single_Tok_Node (Name_2)));
                        begin
                           if Str_1 = "library" and then Str_2 = "" then
                              Project.Qualifier := K_Library;

                           elsif Str_1 = "aggregate" and then Str_2 = "" then
                              Project.Qualifier := K_Aggregate;

                           elsif Str_1 = "aggregate"
                             and then Str_2 = "library"
                           then
                              Project.Qualifier := K_Aggregate_Library;

                           elsif Str_1 = "configuration"
                             and then Str_2 = ""
                           then
                              Project.Qualifier := K_Configuration;

                           else
                              --  ?? an error
                              null;
                           end if;
                        end;

                     when others =>
                        --  ?? an error
                        null;
                  end case;
               end if;

               Project.Name :=
                 To_Unbounded_String (Get_Name_Type (Single_Tok_Node (Name)));
            end Parse_Project_Declaration;

            ---------------------
            -- Parse_With_Decl --
            ---------------------

            procedure Parse_With_Decl (N : With_Decl) is
               Path_Names : constant List_String_Literal := F_Path_Names (N);
               Num_Childs : constant Natural := Child_Count (N);
               Cur_Child  : GPR_Node;
            begin
               for J in 1 .. Num_Childs loop
                  Cur_Child := Child (GPR_Node (Path_Names), J - 1);

                  if Cur_Child /= null then
                     declare
                        Path : constant Path_Name_Type :=
                                 Create
                                   (Get_Name_Type
                                      (String_Literal (Cur_Child)));
                     begin
                        Project.Imports.Append (Path);
                     end;
                  end if;
               end loop;
            end Parse_With_Decl;

         begin
            case AST.Kind (Node) is
               when Project_Declaration_Kind =>
                  Parse_Project_Declaration (Project_Declaration (Node));

               when External_Reference_Kind =>
                  Parse_External_Reference (External_Reference (Node));

               when With_Decl_Kind =>
                  Parse_With_Decl (With_Decl (Node));

               when others =>
                  null;
            end case;

            return Status;
         end Parser;

      begin
         Traverse (Root (Unit), Parser'Access);

         return Project;
      end Parse_Stage_1;

      Context : constant Analysis_Context := Create ("UTF8");
      Unit    : Analysis_Unit;
      Project : Object;

   begin
      if Registry.Exists (Filename) then
         return Registry.Get (Filename);

      else
         Unit := Get_From_File (Context, Value (Filename));

         if Root (Unit) = null then
            --  A syntax error in the project trigger this, where is the
            --  context to display information about the malformed project?
            return Undefined;
         end if;

         --  Do the first stage parsing. We just need the external references
         --  and the project dependencies. This is the minimum to be able to
         --  create the project tree and setup the project context.

         Project := Parse_Stage_1 (Unit);

         --  Then record langkit tree data with project. Those data will be
         --  used for later parsing when creating view of projects with a
         --  full context.

         Project.File    := Filename;
         Project.Unit    := Unit;
         Project.Context := Context;

         --  Finaly register this project into the registry

         Registry.Register (Filename, Project);

         return Project;
      end if;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return Name_Type (To_String (Self.Name));
   end Name;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self      : in out Object;
      Tree      : GPR2.Project.Tree.Object;
      Attrs     : in out GPR2.Project.Attribute.Set.Object;
      Vars      : in out GPR2.Project.Variable.Set.Object;
      Packs     : in out GPR2.Project.Pack.Set.Object;
      Signature : in out Context.Binary_Signature)
   is

      Ctx : constant Context.Object := Tree.Context;
      --  The full project's tree context

      function Parser (Node : GPR_Node) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Variable_Values
        (Node : Variable_Reference) return Containers.Value_List;
      --  Parse and return the value for the given variable reference

      function Get_Attribute_Ref
        (Project : Name_Type; Node : Attribute_Reference)
         return Containers.Value_List;

      function Get_Variable_Ref
        (Project : Name_Type; Node : Identifier)
         return Containers.Value_List;

      function Get_Term_List
        (Node : Term_List) return Containers.Value_List
        with Pre => Node /= null;
      --  Parse a list of value or a single value as found in an attribute

      --  The parsing status for case statement (possibly nested)

      Case_Values : Containers.Value_List;
      --  The case-values to match against the case-item. Each time a case
      --  statement is enterred the value for the case is prepended into this
      --  vector. The first value is then removed when exiting from the case
      --  statement.

      Is_Open     : Boolean := True;
      --  Is_Open is a parsing barrier, it is True when parsing can be
      --  conducted and False otherwise. Is_Open is set to False when enterring
      --  a case construct. It is then set to True/False depending on the case
      --  When Is_Open is False no parsing should be done, that is all node
      --  should be ignored except the Case_Item ones.

      In_Pack     : Boolean := False;
      Pack_Attrs  : GPR2.Project.Attribute.Set.Object;

      -----------------------
      -- Get_Attribute_Ref --
      -----------------------

      function Get_Attribute_Ref
        (Project : Name_Type; Node : Attribute_Reference)
         return Containers.Value_List
      is
         Name : constant Name_Type :=
                  Get_Name_Type (Single_Tok_Node (F_Attribute_Name (Node)));
         View : constant GPR2.Project.View.Object :=
                  GPR2.Project.Tree.View_For (Tree, Project, Ctx);
      begin
         if View.Has_Attributes (Name) then
            return View.Attributes (Name).First_Element.Values;

         else
            return Containers.Value_Type_List.Empty_Vector;
         end if;
      end Get_Attribute_Ref;

      -------------------
      -- Get_Term_List --
      -------------------

      function Get_Term_List
        (Node : Term_List) return Containers.Value_List
      is
         use Langkit_Support;
         use Langkit_Support.Tokens;
         use GPR_Parser;

         Result : Containers.Value_List;
         --  The list of values returned by Get_Term_List

         function Parser (Node : GPR_Node) return Visit_Status;

         ------------
         -- Parser --
         ------------

         function Parser (Node : GPR_Node) return Visit_Status is
            Status : Visit_Status := Into;

            procedure Handle_String (Node : String_Literal)
              with Pre  => Present (Node),
                   Post => Result.Length'Old + 1 = Result.Length;
            --  A simple static string

            procedure Handle_Variable (Node : Variable_Reference)
              with Pre  => Present (Node),
                   Post => Result.Length'Old < Result.Length;
            --  A variable

            procedure Handle_External_Variable (Node : External_Reference)
              with Pre  => Present (Node),
                   Post => Result.Length'Old < Result.Length;
            --  An external variable

            ------------------------------
            -- Handle_External_Variable --
            ------------------------------

            procedure Handle_External_Variable (Node : External_Reference) is
               Str  : constant String_Literal := F_String_Lit (Node);
               Name : constant Name_Type :=
                        Unquote
                          (Name_Type (Image (F_Tok (Single_Tok_Node (Str)))));
               Expr  : constant Term_List := F_Expr (Node);
            begin
               if Ctx.Contains (Name) then
                  --  External in the context, use this value
                  Result.Append (Ctx (Name));

               elsif Present (Expr) then
                  --  External not in the context but has a default value
                  declare
                     Default : constant Containers.Value_List :=
                                 Get_Term_List (Expr);
                  begin
                     Result.Append (Unquote (Default.First_Element));
                  end;

               else
                  --  Not in the context and no default value
                  Result.Append ("");
               end if;

               --  Skip all child nodes, we do not want to parse a second time
               --  the string_literal.

               Status := Over;
            end Handle_External_Variable;

            -------------------
            -- Handle_String --
            -------------------

            procedure Handle_String (Node : String_Literal) is
            begin
               Result.Append
                 (Unquote
                    (Value_Type (Image (F_Tok (Single_Tok_Node (Node))))));
            end Handle_String;

            ---------------------
            -- Handle_Variable --
            ---------------------

            procedure Handle_Variable (Node : Variable_Reference) is
               Values : constant Containers.Value_List :=
                          Get_Variable_Values (Node);
            begin
               if Values.Length = 0 then
                  Result.Append ("");

               else
                  Result.Append (Values);
               end if;
            end Handle_Variable;

         begin
            case Kind (Node) is
               when String_Literal_Kind =>
                  Handle_String (String_Literal (Node));

               when Variable_Reference_Kind =>
                  Handle_Variable (Variable_Reference (Node));

               when External_Reference_Kind =>
                  Handle_External_Variable (External_Reference (Node));

               when others =>
                  null;
            end case;

            return Status;
         end Parser;

      begin
         Traverse (GPR_Node (Node), Parser'Access);
         return Result;
      end Get_Term_List;

      ----------------------
      -- Get_Variable_Ref --
      ----------------------

      function Get_Variable_Ref
        (Project : Name_Type; Node : Identifier)
         return Containers.Value_List
      is
         Name : constant Name_Type := Get_Name_Type (Node);
         View : constant GPR2.Project.View.Object :=
                  GPR2.Project.Tree.View_For (Tree, Project, Ctx);
      begin
         if View.Has_Variables (Name) then
            return View.Variables (Name).First_Element.Values;
         else
            return Containers.Value_Type_List.Empty_Vector;
         end if;
      end Get_Variable_Ref;

      -------------------------
      -- Get_Variable_Values --
      -------------------------

      function Get_Variable_Values
        (Node : Variable_Reference) return Containers.Value_List
      is
         use Langkit_Support.Tokens;
         Name_1  : constant Identifier := F_Variable_Name1 (Node);
         Name_2  : constant Identifier := F_Variable_Name2 (Node);
         Att_Ref : constant Attribute_Reference := F_Attribute_Ref (Node);
         Name    : constant Name_Type :=
                     Image (F_Tok (Single_Tok_Node (Name_1)));
      begin
         if Present (Att_Ref) then
            return Get_Attribute_Ref (Name, Att_Ref);

         elsif Present (Name_2) then
            return Get_Variable_Ref (Name, Name_2);

         elsif Vars.Contains (Name) then
            return Vars (Name).Values;

         else
            raise Constraint_Error with "variable " & Name & " does not exist";
         end if;
      end Get_Variable_Values;

      ------------
      -- Parser --
      ------------

      function Parser (Node : GPR_Node) return Visit_Status is

         use GPR_Parser;

         Status : Visit_Status := Into;

         procedure Parse_Attribute_Decl_Kind (Node : Attribute_Decl)
           with Pre => Is_Open;
         --  Parse attribute declaration and append it into Attrs set

         procedure Parse_Variable_Decl_Kind (Node : Variable_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Package_Decl_Kind (Node : Package_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Case_Construction (Node : Case_Construction)
           with Pre  => Is_Open,
                Post => Case_Values.Length'Old = Case_Values.Length;
         --  Parse a case construction, during a case construction parsing the
         --  Is_Open flag may be set to False and True. Set Is_Open comments.

         procedure Parse_Case_Item (Node : Case_Item)
           with Pre => Case_Values.Length > 0;
         --  Set Is_Open to True or False depending on the item

         procedure Visit_Child (Child : GPR_Node);
         --  Recursive call to the Parser if the Child is not null

         -------------------------------
         -- Parse_Attribute_Decl_Kind --
         -------------------------------

         procedure Parse_Attribute_Decl_Kind (Node : Attribute_Decl) is
            use Langkit_Support.Tokens;
            Name   : constant GPR_Node := F_Attr_Name (Node);
            Index  : constant GPR_Node := F_Attr_Index (Node);
            Expr   : constant Term_List := F_Expr (Node);
            Values : constant Containers.Value_List :=
                       Get_Term_List (Expr);
            A      : GPR2.Project.Attribute.Object;
         begin
            --  Name is either a string or an external

            if Present (Index) then
               if Values.Length = 1 then
                  A := GPR2.Project.Attribute.Create
                    (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                     Index =>
                       Get_Name_Type (F_Str_Lit (String_Literal_At (Index))),
                     Value => Values.First_Element);
               else
                  A := GPR2.Project.Attribute.Create
                    (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                     Index =>
                       Get_Name_Type (F_Str_Lit (String_Literal_At (Index))),
                     Values => Values);
               end if;

            else
               if Values.Length = 1 then
                  A := GPR2.Project.Attribute.Create
                    (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                     Value => Values.First_Element);
               else
                  A := GPR2.Project.Attribute.Create
                    (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                     Values => Values);
               end if;
            end if;

            if In_Pack then
               Pack_Attrs.Insert (A.Name, A);
            else
               Attrs.Insert (A.Name, A);
            end if;
         end Parse_Attribute_Decl_Kind;

         -----------------------------
         -- Parse_Case_Construction --
         -----------------------------

         procedure Parse_Case_Construction (Node : Case_Construction) is
            Var   : constant Variable_Reference := F_Var_Ref (Node);
            Value : constant Value_Type :=
                      Get_Variable_Values (Var).First_Element;
         begin
            Case_Values.Prepend (Value);
            --  Set status to close for now, this will be open when a
            --  when_clause will match the value pushed just above on
            --  the vector.

            Is_Open := False;

            declare
               Childs : constant List_Case_Item := F_Items (Node);
            begin
               for C in 0 .. Child_Count (Childs) loop
                  Visit_Child (Child (GPR_Node (Childs), C));
               end loop;
            end;

            --  Then remove the case value

            Case_Values.Delete_First;

            --  Skip all nodes for this construct

            Status := Over;

            Is_Open := True;
         end Parse_Case_Construction;

         ---------------------
         -- Parse_Case_Item --
         ---------------------

         procedure Parse_Case_Item (Node : Case_Item) is
            use GPR_Parser.AST.Types;

            function Parser (Node : GPR_Node) return Visit_Status;

            Is_Case_Item_Matches : Boolean := False;

            ------------
            -- Parser --
            ------------

            function Parser (Node : GPR_Node) return Visit_Status is
               Status : constant Visit_Status := Into;

               procedure Handle_String   (Node : String_Literal);

               -------------------
               -- Handle_String --
               -------------------

               procedure Handle_String (Node : String_Literal) is
                  use Langkit_Support.Tokens;
                  Value : constant Name_Type :=
                            Unquote
                              (Name_Type
                                 (Image (F_Tok (Single_Tok_Node (Node)))));
               begin
                  Is_Case_Item_Matches :=
                    Is_Case_Item_Matches
                    or else (Value = Case_Values.First_Element);
               end Handle_String;

            begin
               case Kind (Node) is
                  when String_Literal_Kind =>
                     Handle_String (String_Literal (Node));

                  when others =>
                     null;
               end case;

               return Status;
            end Parser;

            Choices : constant List_GPR_Node := F_Choice (Node);

         begin
            Traverse (GPR_Node (Choices), Parser'Access);
            Is_Open := Is_Case_Item_Matches;
         end Parse_Case_Item;

         -----------------------------
         -- Parse_Package_Decl_Kind --
         -----------------------------

         procedure Parse_Package_Decl_Kind (Node : Package_Decl) is
            Name : constant Identifier := F_Pkg_Name (Node);
         begin
            --  First clear the package attributes container

            Pack_Attrs.Clear;

            In_Pack := True;

            Visit_Child (F_Pkg_Spec (Node));

            In_Pack := False;

            --  Insert the package definition into the final result

            declare
               P_Name : constant Name_Type :=
                          Get_Name_Type (Single_Tok_Node (Name));
            begin
               Packs.Insert
                 (P_Name, GPR2.Project.Pack.Create (P_Name, Pack_Attrs));
            end;

            --  Skip all nodes for this construct

            Status := Over;
         end Parse_Package_Decl_Kind;

         ------------------------------
         -- Parse_Variable_Decl_Kind --
         ------------------------------

         procedure Parse_Variable_Decl_Kind (Node : Variable_Decl) is
            Name   : constant Identifier := F_Var_Name (Node);
            Expr   : constant Term_List := F_Expr (Node);
            Values : constant Containers.Value_List :=
                       Get_Term_List (Expr);
            V_Type : constant Types.Expr := F_Var_Type (Node);
            V      : GPR2.Project.Variable.Object;
         begin
            if V_Type /= null then
               --  The Type_Name
               --  ?? check the type
               null;
            end if;

            if Values.Length = 1 then
               V := GPR2.Project.Variable.Create
                 (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                  Value => Values.First_Element);
            else
               V := GPR2.Project.Variable.Create
                 (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                  Values => Values);
            end if;

            Vars.Include (V.Name, V);
         end Parse_Variable_Decl_Kind;

         -----------------
         -- Visit_Child --
         -----------------

         procedure Visit_Child (Child : GPR_Node) is
         begin
            if Present (Child) then
               Status :=
                 Traverse
                   (Node  => Child,
                    Visit => Parser'Access);
            end if;
         end Visit_Child;

      begin
         if Is_Open then
            --  Handle all kind of nodes when the parsing is open

            case AST.Kind (Node) is
               when Attribute_Decl_Kind =>
                  Parse_Attribute_Decl_Kind (Attribute_Decl (Node));

               when Variable_Decl_Kind =>
                  Parse_Variable_Decl_Kind (Variable_Decl (Node));

               when Package_Decl_Kind =>
                  Parse_Package_Decl_Kind (Package_Decl (Node));

               when Case_Construction_Kind =>
                  Parse_Case_Construction (Case_Construction (Node));

               when Case_Item_Kind =>
                  Parse_Case_Item (Case_Item (Node));

               when others =>
                  null;
            end case;

         else
            --  We are on a closed parsing mode, only handle case alternatives

            case AST.Kind (Node) is
               when Case_Item_Kind =>
                  Parse_Case_Item (Case_Item (Node));

               when others =>
                  null;
            end case;
         end if;

         return Status;
      end Parser;

      New_Signature : constant Context.Binary_Signature :=
                        Context.Signature (Ctx, Self.Externals);

   begin
      --  ?? we probably want to always parse and then check that Attrs, Vars
      --  or Packs has changed. Indeed, a project which depends on another
      --  project and referencing a variable or attribute whose value depends
      --  on an external is not taken into account right now.

      if Signature /= New_Signature then
         --  Clear all current values for this project

         Attrs.Clear;
         Vars.Clear;
         Packs.Clear;

         --  Re-Analyze the project given the new context

         Traverse (Root (Self.Unit), Parser'Access);

         --  Record new signature

         Signature := New_Signature;
      end if;
   end Parse;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return Path_Name_Type is
   begin
      return Self.File;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Self.Qualifier;
   end Qualifier;

end GPR2.Parser.Project;
