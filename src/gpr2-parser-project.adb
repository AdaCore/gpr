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
with Ada.Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;

with GPR_Parser;
with GPR_Parser.AST;       use GPR_Parser.AST;
with GPR_Parser.AST.Types; use GPR_Parser.AST.Types;

with GPR2.Message;
with GPR2.Parser.Registry;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;
with GPR2.Project.View;
with GPR2.Source_Reference;

package body GPR2.Parser.Project is

   use Ada;
   use type Ada.Containers.Count_Type;

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
                 Data (F_Tok (Single_Tok_Node (Node))).Text.all;
      Offset : Natural := 0;
   begin
      if V (V'First) = '"' and then V (V'Last) = '"' then
         Offset := 1;
      end if;
      return Name_Type (To_String (V (V'First + Offset .. V'Last - Offset)));
   end Get_Name_Type;

   -------------------
   -- Has_Externals --
   -------------------

   function Has_Externals (Self : Object) return Boolean is
   begin
      return not Self.Externals.Is_Empty;
   end Has_Externals;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
   begin
      return not Self.Imports.Is_Empty;
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

   function Load
     (Filename : Path_Name_Type;
      Messages : out Log.Object) return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;
      use GPR_Parser;
      use GPR_Parser;

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object;
      --  Analyse project, record all externals variables and imports

      -------------------
      -- Parse_Stage_1 --
      -------------------

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object is

         Project : Object;
         --  The project being constructed

         function Parser
           (Node : access GPR_Node_Type'Class) return Visit_Status;
         --  Actual parser callabck for the project

         ------------
         -- Parser --
         ------------

         function Parser
           (Node : access GPR_Node_Type'Class) return Visit_Status
         is
            Status : constant Visit_Status := Into;

            procedure Parse_Project_Declaration
              (N : not null Project_Declaration);
            --  Parse a project declaration and set the qualifier if present

            procedure Parse_External_Reference
              (N : not null External_Reference);
            --  A the name of the external into the Externals list

            procedure Parse_With_Decl (N : not null With_Decl);
            --  Add the name of the withed project into the Imports list

            ------------------------------
            -- Parse_External_Reference --
            ------------------------------

            procedure Parse_External_Reference
              (N : not null External_Reference)
            is
               Var : constant not null String_Literal := F_String_Lit (N);
            begin
               Project.Externals.Append (Get_Name_Type (Var));
            end Parse_External_Reference;

            -------------------------------
            -- Parse_Project_Declaration --
            -------------------------------

            procedure Parse_Project_Declaration
              (N : not null Project_Declaration)
            is
               Name : constant not null Expr := F_Project_Name (N);
               Qual : constant Types.Project_Qualifier :=
                        F_Qualifier (N);
            begin
               --  If we have an explicit qualifier parse it now. If not the
               --  kind of project will be determined later during a second
               --  pass.

               if Present (Qual) then
                  case AST.Kind (F_Qualifier (Qual)) is
                     when GPR_Abstract_Present =>
                        Project.Qualifier := K_Abstract;

                     when GPR_Qualifier_Names =>
                        declare
                           Names : constant not null Qualifier_Names :=
                                     Qualifier_Names (F_Qualifier (Qual));
                           Name_1 : constant Identifier :=
                                      F_Qualifier_Id1 (Names);
                           Str_1  : constant String :=
                                      (if Name_1 = null
                                       then ""
                                       else Characters.Handling.To_Lower
                                         (String
                                            (Get_Name_Type
                                               (Single_Tok_Node (Name_1)))));
                           Name_2 : constant Identifier :=
                                      F_Qualifier_Id2 (Names);
                           Str_2  : constant String :=
                                      (if Name_2 = null
                                       then ""
                                       else Characters.Handling.To_Lower
                                         (String
                                            (Get_Name_Type
                                               (Single_Tok_Node (Name_2)))));
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
                 To_Unbounded_String
                   (String (Get_Name_Type (Single_Tok_Node (Name))));
            end Parse_Project_Declaration;

            ---------------------
            -- Parse_With_Decl --
            ---------------------

            procedure Parse_With_Decl (N : not null With_Decl) is
               Path_Names : constant not null List_String_Literal :=
                              F_Path_Names (N);
               Num_Childs : constant Natural := Child_Count (N);
               Cur_Child  : GPR_Node;
            begin
               for J in 1 .. Num_Childs loop
                  Cur_Child := Child (GPR_Node (Path_Names), J);

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
               when GPR_Project_Declaration =>
                  Parse_Project_Declaration (Project_Declaration (Node));

               when GPR_External_Reference =>
                  Parse_External_Reference (External_Reference (Node));

               when GPR_With_Decl =>
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

         if Root (Unit) = null or else Has_Diagnostics (Unit) then

            if Has_Diagnostics (Unit) then
               for D of Diagnostics (Unit) loop
                  declare
                     Sloc : constant Source_Reference.Object'Class :=
                              Source_Reference.Create
                                (Filename => Value (Filename),
                                 Line     =>
                                   Natural (D.Sloc_Range.Start_Line),
                                 Column   =>
                                   Natural (D.Sloc_Range.Start_Column));
                  begin
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    => Source_Reference.Object (Sloc),
                           Message =>
                             To_String (To_Wide_Wide_String (D.Message))));
                  end;
               end loop;
            end if;

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
     (Self    : in out Object;
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Packs   : in out GPR2.Project.Pack.Set.Object)
   is

      function Parser
        (Node : access GPR_Node_Type'Class) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Variable_Values
        (Node : not null Variable_Reference) return Containers.Value_List;
      --  Parse and return the value for the given variable reference

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : not null Attribute_Reference;
         Pack    : Optional_Name_Type := "")
         return Containers.Value_List;

      function Get_Variable_Ref
        (Project : Name_Type; Node : not null Identifier)
         return Containers.Value_List;

      function Get_Term_List
        (Node   : not null Term_List;
         Single : out Boolean) return Containers.Value_List;
      --  Parse a list of value or a single value as found in an attribute.
      --  Single is set to True if we have a single value. It is false if we
      --  have parsed an expression list. In this later case it does not mean
      --  that we are retuning multiple values, just that the expression is a
      --  list surrounded by parentheses.

      procedure Record_Attribute
        (Set : in out GPR2.Project.Attribute.Set.Object;
         A   : GPR2.Project.Attribute.Object);
      --  Record an attribute into the given set. At the same time we increment
      --  the Empty_Attribute_Count if this attribute has an empty value. This
      --  is used to check whether we need to reparse the tree.

      function Stop_Iteration return Boolean;
      --  Returns true if a new parsing of the tree is needed. This is because
      --  an attribute can have a forward reference to another attribute into
      --  the same package.

      --  Global variables used to keep state during the parsing. While
      --  visiting child nodes we may need to record status (when in a package
      --  or a case construct for example). This parsing state is then used
      --  to apply different check or recording.

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

      --  Package orientated state, when parsing is in a package In_Pack is
      --  set and Pack_Name contains the name of the package and all parsed
      --  attributes are recorded into Pack_Attrs set.

      In_Pack              : Boolean := False;
      Pack_Name            : Unbounded_String;
      Pack_Attrs           : GPR2.Project.Attribute.Set.Object;
      Att_Defined          : Boolean := True;
      Is_Project_Reference : Boolean := False;
      --  Is_Project_Reference is True when using: Project'<attribute>

      Undefined_Attribute_Count          : Natural := 0;
      Previous_Undefined_Attribute_Count : Natural := 0;

      -----------------------
      -- Get_Attribute_Ref --
      -----------------------

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : not null Attribute_Reference;
         Pack    : Optional_Name_Type := "") return Containers.Value_List
      is
         use type GPR2.Project.View.Object;

         Name   : constant Name_Type :=
                    Get_Name_Type
                      (Single_Tok_Node (F_Attribute_Name (Node)));
         I_Node : constant GPR_Node := F_Attribute_Index (Node);
         Index  : constant Value_Type :=
                    (if Present (I_Node)
                     then Value_Type (Get_Name_Type (Single_Tok_Node (I_Node)))
                     else "");
         View   : constant GPR2.Project.View.Object :=
                    GPR2.Project.Tree.View_For (Tree, Project, Context);

         Result : Containers.Value_List :=
                    Containers.Value_Type_List.Empty_Vector;

      begin
         if Project = Name_Type (To_String (Self.Name))
           or else Is_Project_Reference
         then
            --  An attribute referencing a value in the current project

            --  Record only if fully defined (add a boolean to control this)
            --  and stop parsing when the number of undefined attribute is
            --  stable.

            if Pack = "" then
               if Attrs.Contains (Name, Index) then
                  Result := Attrs.Element (Name, Index).Values;
               else
                  Att_Defined := False;
               end if;

            elsif Pack_Name /= Null_Unbounded_String
              and then Name_Type (To_String (Pack_Name)) = Name_Type (Pack)
            then
               --  This is the current parsed package, look into Pack_Attrs

               if Pack_Attrs.Contains (Name, Index) then
                  Result := Pack_Attrs.Element (Name, Index).Values;
               else
                  Att_Defined := False;
               end if;

            elsif Packs.Contains (Name_Type (Pack)) then
               --  Or in another package in the same project
               if Packs (Name_Type (Pack)).Has_Attributes (Name, Index) then
                  Result := Packs.Element
                    (Name_Type (Pack)).Attributes.Element (Name, Index).Values;
               else
                  Att_Defined := False;
               end if;
            end if;

         else
            if View /= GPR2.Project.View.Undefined then
               if Pack = "" then
                  if View.Has_Attributes (Name) then
                     Result := View.Attributes.Element (Name).Values;
                  else
                     Att_Defined := False;
                  end if;

               else
                  if View.Has_Packages (Pack) then
                     declare
                        P : constant GPR2.Project.Pack.Object :=
                              View.Packages.Element (Name_Type (Pack));
                     begin
                        if P.Has_Attributes (Name) then
                           Result := P.Attributes.Element (Name).Values;
                        end if;
                     end;

                  else
                     Att_Defined := False;
                  end if;
               end if;
            end if;
         end if;

         return Result;
      end Get_Attribute_Ref;

      -------------------
      -- Get_Term_List --
      -------------------

      function Get_Term_List
        (Node   : not null Term_List;
         Single : out Boolean) return Containers.Value_List
      is
         use GPR_Parser;

         Result : Containers.Value_List;
         --  The list of values returned by Get_Term_List

         function Parser
           (Node : access GPR_Node_Type'Class) return Visit_Status;

         procedure Record_Value (Value : Value_Type)
           with Post => Result.Length'Old =
                         (if Result.Length'Old > 0 and then Single
                          then Result.Length else Result.Length - 1);
         --  Record Value into Result, either add it as a new value in the list
         --  (Single = False) or append the value to the current one.

         procedure Record_Values (Values : Containers.Value_List);
         --  Same as above but for multiple values

         ------------
         -- Parser --
         ------------

         function Parser
           (Node : access GPR_Node_Type'Class) return Visit_Status
         is
            Status : Visit_Status := Into;

            procedure Handle_String (Node : not null String_Literal)
              with Pre  => Present (Node);
            --  A simple static string

            procedure Handle_Variable (Node : not null Variable_Reference)
              with Pre => Present (Node);
            --  A variable

            procedure Handle_External_Variable
              (Node : not null External_Reference)
              with Pre  => Present (Node);
            --  An external variable

            procedure Handle_Attribute_Reference
              (Node : not null Attribute_Reference)
              with Pre  => Present (Node);
            --  An attribute reference for ProjectReference node only. The
            --  other cases are handled in other parts.

            --------------------------------
            -- Handle_Attribute_Reference --
            --------------------------------

            procedure Handle_Attribute_Reference
              (Node : not null Attribute_Reference) is
            begin
               if Is_Project_Reference then
                  Record_Values (Get_Attribute_Ref ("project", Node));
               end if;
               Status := Over;
            end Handle_Attribute_Reference;

            ------------------------------
            -- Handle_External_Variable --
            ------------------------------

            procedure Handle_External_Variable
              (Node : not null External_Reference)
            is
               Str  : constant not null String_Literal := F_String_Lit (Node);
               Name : constant Name_Type :=
                        Name_Type
                          (Unquote
                             (Value_Type
                                (Image (F_Tok (Single_Tok_Node (Str))))));
               Expr : constant Term_List := F_Expr (Node);
            begin
               if Context.Contains (Name) then
                  --  External in the context, use this value
                  Record_Value (Context (Name));

               elsif Present (Expr) then
                  --  External not in the context but has a default value
                  declare
                     Single  : Boolean;
                     Default : constant Containers.Value_List :=
                                 Get_Term_List (Expr, Single);
                  begin
                     Record_Value (Unquote (Default.First_Element));
                  end;

               else
                  --  Not in the context and no default value
                  Record_Value ("");
               end if;

               --  Skip all child nodes, we do not want to parse a second time
               --  the string_literal.

               Status := Over;
            end Handle_External_Variable;

            -------------------
            -- Handle_String --
            -------------------

            procedure Handle_String (Node : not null String_Literal) is
            begin
               Record_Value
                 (Unquote
                    (Value_Type (Image (F_Tok (Single_Tok_Node (Node))))));
            end Handle_String;

            ---------------------
            -- Handle_Variable --
            ---------------------

            procedure Handle_Variable (Node : not null Variable_Reference) is
               Values : constant Containers.Value_List :=
                          Get_Variable_Values (Node);
            begin
               if Values.Is_Empty then
                  Record_Value ("");

               else
                  --  We are adding a variable value to the current non empty
                  --  list, or the variable contains multiple values, so this
                  --  is not a single valued attribute.

                  if Result.Length > 0 or else Values.Length > 1 then
                     Single := False;
                  end if;

                  Record_Values (Values);
               end if;

               Status := Over;
            end Handle_Variable;

         begin
            case Kind (Node) is
               when GPR_Expr_List =>
                  --  We are opening not a single element but an expression
                  --  list.
                  Single := False;

               when GPR_String_Literal =>
                  Handle_String (String_Literal (Node));

               when GPR_Variable_Reference =>
                  Handle_Variable (Variable_Reference (Node));

               when GPR_External_Reference =>
                  Handle_External_Variable (External_Reference (Node));

               when GPR_Project_Reference =>
                  Is_Project_Reference := True;

               when GPR_Attribute_Reference =>
                  Handle_Attribute_Reference (Attribute_Reference (Node));

               when others =>
                  null;
            end case;

            return Status;
         end Parser;

         ------------------
         -- Record_Value --
         ------------------

         procedure Record_Value (Value : Value_Type) is
         begin
            if Single and then Result.Length > 0 then
               declare
                  New_Value : constant Value_Type :=
                                Result (Result.Last_Index) & Value;
               begin
                  Result.Replace_Element (Result.Last_Index, New_Value);
               end;

            else
               Result.Append (Value);
            end if;
         end Record_Value;

         -------------------
         -- Record_Values --
         -------------------

         procedure Record_Values (Values : Containers.Value_List) is
         begin
            for V of Values loop
               Record_Value (V);
            end loop;
         end Record_Values;

      begin
         Single := True;
         Is_Project_Reference := False;

         Traverse (GPR_Node (Node), Parser'Access);

         Is_Project_Reference := False;
         return Result;
      end Get_Term_List;

      ----------------------
      -- Get_Variable_Ref --
      ----------------------

      function Get_Variable_Ref
        (Project : Name_Type; Node : not null Identifier)
         return Containers.Value_List
      is
         Name : constant Name_Type :=
                  Name_Type (Get_Name_Type (Node));
         View : constant GPR2.Project.View.Object :=
                  GPR2.Project.Tree.View_For (Tree, Project, Context);
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
        (Node : not null Variable_Reference) return Containers.Value_List
      is
         Name_1  : constant not null Identifier := F_Variable_Name1 (Node);
         Name_2  : constant Identifier := F_Variable_Name2 (Node);
         Att_Ref : constant Attribute_Reference := F_Attribute_Ref (Node);
         Name    : constant Name_Type :=
                     Name_Type (Image (F_Tok (Single_Tok_Node (Name_1))));
      begin
         if Present (Att_Ref) then
            if Present (Name_2) then
               --  This is a project/package reference:
               --    <project>.<package>'<attribute>
               return Get_Attribute_Ref
                 (Project => Name,
                  Pack    => Optional_Name_Type
                               (Image (F_Tok (Single_Tok_Node (Name_2)))),
                  Node    => Att_Ref);
            else
               --  If a single name it can be either a project or a package

               if Containers.Contains (Self.Imports, Name) then
                  --  This is a project reference: <project>'<attribute>
                  return Get_Attribute_Ref
                    (Project => Name,
                     Pack    => "",
                     Node    => Att_Ref);
               else
                  --  This is a package reference: <package>'<attribute>
                  return Get_Attribute_Ref
                    (Project => Name_Type (To_String (Self.Name)),
                     Pack    => Name,
                     Node    => Att_Ref);
               end if;
            end if;

         elsif Present (Name_2) then
            return Get_Variable_Ref (Name, Name_2);

         elsif Vars.Contains (Name) then
            return Vars (Name).Values;

         else
            raise Constraint_Error
              with "variable " & String (Name) & " does not exist";
         end if;
      end Get_Variable_Values;

      ------------
      -- Parser --
      ------------

      function Parser
        (Node : access GPR_Node_Type'Class) return Visit_Status
      is

         use GPR_Parser;

         Status : Visit_Status := Into;

         procedure Parse_Attribute_Decl_Kind (Node : not null Attribute_Decl)
           with Pre => Is_Open;
         --  Parse attribute declaration and append it into Attrs set

         procedure Parse_Variable_Decl_Kind (Node : not null Variable_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Package_Decl_Kind (Node : not null Package_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Case_Construction (Node : not null Case_Construction)
           with Pre  => Is_Open,
                Post => Case_Values.Length'Old = Case_Values.Length;
         --  Parse a case construction, during a case construction parsing the
         --  Is_Open flag may be set to False and True. Set Is_Open comments.

         procedure Parse_Case_Item (Node : not null Case_Item)
           with Pre => not Case_Values.Is_Empty;
         --  Set Is_Open to True or False depending on the item

         procedure Visit_Child (Child : GPR_Node);
         --  Recursive call to the Parser if the Child is not null

         function Get_Source_Reference
           (Slr : Langkit_Support.Slocs.Source_Location_Range)
            return Source_Reference.Object is
           (Source_Reference.Create
              (Value (Self.File),
               Positive (Slr.Start_Line),
               Positive (Slr.Start_Column)));

         -------------------------------
         -- Parse_Attribute_Decl_Kind --
         -------------------------------

         procedure Parse_Attribute_Decl_Kind
           (Node : not null Attribute_Decl)
         is
            Sloc  : constant Source_Reference.Object :=
                      Get_Source_Reference (Sloc_Range (GPR_Node (Node)));
            Name  : constant not null GPR_Node := F_Attr_Name (Node);
            Index : constant GPR_Node := F_Attr_Index (Node);
            I_Str : constant Value_Type :=
                      (if Present (Index)
                       then Value_Type
                              (Get_Name_Type
                                (F_Str_Lit (String_Literal_At (Index))))
                       else "");
            Expr  : constant not null Term_List := F_Expr (Node);
            N_Str : constant Name_Type :=
                      (if Kind (Name) = GPR_External_Name
                       then "external"
                       else Get_Name_Type (Single_Tok_Node (Name)));
         begin
            declare
               Single : Boolean;
               Values : constant Containers.Value_List :=
                          Get_Term_List (Expr, Single);
               A      : GPR2.Project.Attribute.Object;
            begin
               --  Name is either a string or an external

               if Present (Index) then
                  if Single then
                     A := GPR2.Project.Attribute.Create
                       (Name  => N_Str,
                        Index => I_Str,
                        Value => Values.First_Element,
                        Sloc  => Sloc);

                  else
                     A := GPR2.Project.Attribute.Create
                       (Name   => N_Str,
                        Index  => I_Str,
                        Values => Values,
                        Sloc   => Sloc);
                  end if;

               else
                  if Single then
                     A := GPR2.Project.Attribute.Create
                       (Name  => N_Str,
                        Value => Values.First_Element,
                        Sloc  => Sloc);
                  else
                     A := GPR2.Project.Attribute.Create
                       (Name   => N_Str,
                        Values => Values,
                        Sloc   => Sloc);
                  end if;
               end if;

               --  Record attribute with proper casing definition if found

               declare
                  package A_Reg renames GPR2.Project.Registry.Attribute;

                  Q_Name : constant A_Reg.Qualified_Name :=
                             A_Reg.Create
                               (A.Name,
                                Optional_Name_Type (To_String (Pack_Name)));
               begin
                  if A_Reg.Exists (Q_Name) then
                     declare
                        Def : constant A_Reg.Def := A_Reg.Get (Q_Name);
                     begin
                        A.Set_Case
                          (Def.Index_Case_Sensitive,
                           Def.Value_Case_Sensitive);
                     end;
                  end if;

                  if In_Pack then
                     Record_Attribute (Pack_Attrs, A);
                  else
                     Record_Attribute (Attrs, A);
                  end if;
               end;
            end;
         end Parse_Attribute_Decl_Kind;

         -----------------------------
         -- Parse_Case_Construction --
         -----------------------------

         procedure Parse_Case_Construction
           (Node : not null Case_Construction)
         is
            Var   : constant not null Variable_Reference := F_Var_Ref (Node);
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
               for C in 1 .. Child_Count (Childs) loop
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

         procedure Parse_Case_Item (Node : not null Case_Item) is
            use GPR_Parser.AST.Types;

            function Parser
              (Node : access GPR_Node_Type'Class) return Visit_Status;

            Is_Case_Item_Matches : Boolean := False;

            ------------
            -- Parser --
            ------------

            function Parser
              (Node : access GPR_Node_Type'Class) return Visit_Status
            is
               Status : constant Visit_Status := Into;

               procedure Handle_String   (Node : not null String_Literal);

               -------------------
               -- Handle_String --
               -------------------

               procedure Handle_String (Node : not null String_Literal) is
                  Value : constant Value_Type :=
                            Unquote (Image (F_Tok (Single_Tok_Node (Node))));
               begin
                  Is_Case_Item_Matches :=
                    Is_Case_Item_Matches
                    or else (Value = Case_Values.First_Element);
               end Handle_String;

            begin
               case Kind (Node) is
                  when GPR_String_Literal =>
                     Handle_String (String_Literal (Node));

                  when GPR_Others_Designator =>
                     Is_Case_Item_Matches := True;

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

         procedure Parse_Package_Decl_Kind (Node : not null Package_Decl) is
            Sloc   : constant Source_Reference.Object :=
                       Get_Source_Reference (Sloc_Range (GPR_Node (Node)));
            Name   : constant not null Identifier := F_Pkg_Name (Node);
            P_Name : constant Name_Type :=
                       Get_Name_Type (Single_Tok_Node (Name));
         begin
            --  First clear the package attributes container or restore the
            --  previous values (stage 2) if it exists.

            if Packs.Contains (P_Name) then
               Pack_Attrs := Packs.Element (P_Name).Attributes;
            else
               Pack_Attrs.Clear;
            end if;

            --  Entering a package, set the state and parse the corresponding
            --  children.

            In_Pack := True;
            Pack_Name := To_Unbounded_String (String (P_Name));

            Visit_Child (F_Pkg_Spec (Node));

            In_Pack := False;
            Pack_Name := Null_Unbounded_String;

            --  Insert the package definition into the final result

            Packs.Include
              (Name_Type (P_Name),
               GPR2.Project.Pack.Create (P_Name, Pack_Attrs, Sloc));

            --  Skip all nodes for this construct

            Status := Over;
         end Parse_Package_Decl_Kind;

         ------------------------------
         -- Parse_Variable_Decl_Kind --
         ------------------------------

         procedure Parse_Variable_Decl_Kind (Node : not null Variable_Decl) is
            Sloc   : constant Source_Reference.Object :=
                       Get_Source_Reference (Sloc_Range (GPR_Node (Node)));
            Name   : constant not null Identifier := F_Var_Name (Node);
            Expr   : constant not null Term_List := F_Expr (Node);
            Single : Boolean;
            Values : constant Containers.Value_List :=
                       Get_Term_List (Expr, Single);
            V_Type : constant Types.Expr := F_Var_Type (Node);
            V      : GPR2.Project.Variable.Object;
         begin
            if V_Type /= null then
               --  The Type_Name
               --  ?? check the type
               null;
            end if;

            if Single then
               V := GPR2.Project.Variable.Create
                 (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                  Value => Values.First_Element,
                  Sloc  => Sloc);
            else
               V := GPR2.Project.Variable.Create
                 (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                  Values => Values,
                  Sloc   => Sloc);
            end if;

            Vars.Include (Name_Type (V.Name), V);
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
         Att_Defined := True;

         if Is_Open then
            --  Handle all kind of nodes when the parsing is open

            case AST.Kind (Node) is
               when GPR_Attribute_Decl =>
                  Parse_Attribute_Decl_Kind (Attribute_Decl (Node));

               when GPR_Variable_Decl =>
                  Parse_Variable_Decl_Kind (Variable_Decl (Node));

               when GPR_Package_Decl =>
                  Parse_Package_Decl_Kind (Package_Decl (Node));

               when GPR_Case_Construction =>
                  Parse_Case_Construction (Case_Construction (Node));

               when GPR_Case_Item =>
                  Parse_Case_Item (Case_Item (Node));

               when others =>
                  null;
            end case;

         else
            --  We are on a closed parsing mode, only handle case alternatives

            case AST.Kind (Node) is
               when GPR_Case_Item =>
                  Parse_Case_Item (Case_Item (Node));

               when others =>
                  null;
            end case;
         end if;

         return Status;
      end Parser;

      ----------------------
      -- Record_Attribute --
      ----------------------

      procedure Record_Attribute
        (Set : in out GPR2.Project.Attribute.Set.Object;
         A   : GPR2.Project.Attribute.Object)
      is
         use type Containers.Value_List;
      begin
         if Att_Defined then
            Set.Include (A);
         else
            Undefined_Attribute_Count := Undefined_Attribute_Count + 1;
         end if;
      end Record_Attribute;

      --------------------
      -- Stop_Iteration --
      --------------------

      function Stop_Iteration return Boolean is
         Result : constant Boolean :=
                    Previous_Undefined_Attribute_Count
                      = Undefined_Attribute_Count;
      begin
         Previous_Undefined_Attribute_Count := Undefined_Attribute_Count;
         Undefined_Attribute_Count := 0;
         return Result;
      end Stop_Iteration;

   begin
      Attrs.Clear;
      Vars.Clear;
      Packs.Clear;

      --  Re-Analyze the project given the new definitions (variables or
      --  attributes).

      loop
         Traverse (Root (Self.Unit), Parser'Access);
         exit when Stop_Iteration;
      end loop;
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
