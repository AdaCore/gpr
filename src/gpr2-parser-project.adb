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
with GPR2.Project.Variable;

package body GPR2.Parser.Project is

   use type Ada.Containers.Count_Type;
   use type MD5.Binary_Message_Digest;

   --  Some helpers routines for the parser

   function Get_Name_Type
     (Node : not null access Single_Tok_Node_Type'Class) return Name_Type;
   --  Returns the Name for the given node

   function Present (Node : access GPR_Node_Type'Class) return Boolean is
     (Node /= null);
   --  Returns True if the Node is present (not null)

   function Context_Signature
     (Ctx       : Context.Object;
      Externals : Containers.Name_List) return MD5.Binary_Message_Digest
     with Post =>
       (if Externals.Length = 0
        then Context_Signature'Result = Default_Signature
        else Context_Signature'Result /= Default_Signature);
   --  Compute and returns a MD5 signature for the Externals given the context.
   --  This is used to check if a project's environment has been changed and
   --  if so the project is to be analysed again. Note that if there is no
   --  Externals the project has no need to be analysed again, in this case
   --  the Default_Signature is returned.

   -----------------------
   -- Context_Signature --
   -----------------------

   function Context_Signature
     (Ctx       : Context.Object;
      Externals : Containers.Name_List) return MD5.Binary_Message_Digest
   is
      Result : MD5.Binary_Message_Digest := Default_Signature;
      C      : MD5.Context;
      Found  : Boolean := False;
   begin
      for E of Externals loop
         if Ctx.Contains (E) then
            MD5.Update (C, Ctx (E));
         end if;
         Found := True;
      end loop;

      if Found then
         Result := MD5.Digest (C);
      end if;

      return Result;
   end Context_Signature;

   ---------------
   -- Externals --
   ---------------

   function Externals (Project : Object) return Containers.Name_List is
   begin
      return Project.Externals;
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

   function Has_Externals (Project : Object) return Boolean is
   begin
      return Project.Externals.Length > 0;
   end Has_Externals;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Project : Object) return Boolean is
   begin
      return Project.Imports.Length > 0;
   end Has_Imports;

   -------------
   -- Imports --
   -------------

   function Imports (Project : Object) return Containers.Path_Name_List is
   begin
      return Project.Imports;
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
                        Project.Qualifier := Q_Abstract;

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
                              Project.Qualifier := Q_Library;

                           elsif Str_1 = "aggregate" and then Str_2 = "" then
                              Project.Qualifier := Q_Aggregate;

                           elsif Str_1 = "aggregate"
                             and then Str_2 = "library"
                           then
                              Project.Qualifier := Q_Aggregate_Library;

                           elsif Str_1 = "configuration"
                             and then Str_2 = ""
                           then
                              Project.Qualifier := Q_Configuration;

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

         Project.File      := Filename;
         Project.Unit      := Unit;
         Project.Context   := Context;
         Project.Signature := (1 => 1, others => 0);
         pragma Assert
           (Project.Signature /= Default_Signature,
            "project signature must be different that default_signature");
         --  Initialized with a signature which is not Default_Signature. This
         --  is needed to ensure that the analyze will be done in stage-1.

         --  Finaly register this project into the registry

         Registry.Register (Filename, Project);

         return Project;
      end if;
   end Load;

   ----------
   -- Name --
   ----------

   function Name (Project : Object) return Name_Type is
   begin
      return Name_Type (To_String (Project.Name));
   end Name;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project : in out Object;
      Ctx     : Context.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Changed : not null access procedure (Project : Object))
   is

      function Parser (Node : GPR_Node) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Term_List
        (Node : Term_List) return Containers.Value_List
        with Pre => Node /= null;
      --  Parse a list of value or a single value as found in an attribute

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

            procedure Handle_String   (Node : String_Literal)
              with Pre  => Present (Node),
                   Post => Result.Length'Old + 1 = Result.Length;
            --  A simple static string

            procedure Handle_Variable (Node : Variable_Reference)
              with Pre  => Present (Node),
                   Post => Result.Length'Old + 1 = Result.Length;
            --  A variable

            procedure Handle_External_Variable (Node : External_Reference)
              with Pre  => Present (Node),
                   Post => Result.Length'Old + 1 = Result.Length;
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
               Name_1 : constant Identifier := F_Variable_Name1 (Node);
               Name   : constant Name_Type :=
                          Image (F_Tok (Single_Tok_Node (Name_1)));
            begin
               if Vars.Contains (Name) then
                  Result.Append (String (Vars (Name).Values.First_Element));
               else
                  --  ?? This is an undefined variable reference
                  Result.Append ("@" & Name);
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

      ------------
      -- Parser --
      ------------

      function Parser (Node : GPR_Node) return Visit_Status is

         use GPR_Parser;

         Status : constant Visit_Status := Into;

         procedure Parse_Attribute_Decl_Kind (Node : Attribute_Decl);

         procedure Parse_Variable_Decl_Kind (Node : Variable_Decl);

         -------------------------------
         -- Parse_Attribute_Decl_Kind --
         -------------------------------

         procedure Parse_Attribute_Decl_Kind (Node : Attribute_Decl) is
            Name   : constant GPR_Node := F_Attr_Name (Node);
            Index  : constant GPR_Node := F_Attr_Index (Node);
            Expr   : constant Term_List := F_Expr (Node);
            Values : constant Containers.Value_List :=
                       Get_Term_List (Expr);
            A      : GPR2.Project.Attribute.Object;
         begin
            --  Name is either a string or an external

            if Values.Length = 1 then
               A := GPR2.Project.Attribute.Create
                 (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                  Value => Values.First_Element);
            else
               A := GPR2.Project.Attribute.Create
                 (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                  Values => Values);
            end if;

            Attrs.Insert (A.Name, A);

            if Present (Index) then
               --  ?? this is an attribute with an index
               null;
            end if;
         end Parse_Attribute_Decl_Kind;

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

      begin
         case AST.Kind (Node) is
            when Attribute_Decl_Kind =>
               Parse_Attribute_Decl_Kind (Attribute_Decl (Node));

            when Variable_Decl_Kind =>
               Parse_Variable_Decl_Kind (Variable_Decl (Node));

            when others =>
               null;
         end case;

         return Status;
      end Parser;

      New_Signature : constant MD5.Binary_Message_Digest :=
                        Context_Signature (Ctx, Project.Externals);

   begin
      if Project.Signature /= New_Signature then
         --  Clear all current values for this project

         Attrs.Clear;
         Vars.Clear;

         --  Re-Analyze the project given the new context

         Traverse (Root (Project.Unit), Parser'Access);

         --  Record new signature

         Project.Signature := New_Signature;

         --  Signal project change only if we have a non default signature.
         --  That is if there is at least some external used otherwise the
         --  project is stable and won't change.

         if Project.Signature /= Default_Signature then
            Changed (Project);
         end if;
      end if;
   end Parse;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Project : Object) return Path_Name_Type is
   begin
      return Project.File;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Project : Object) return Project_Qualifier is
   begin
      return Project.Qualifier;
   end Qualifier;

end GPR2.Parser.Project;
