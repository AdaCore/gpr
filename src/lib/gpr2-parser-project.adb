------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with GPR2.Builtin;
with GPR2.Message;
with GPR2.Parser.Registry;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

with GPR_Parser.Common;

package body GPR2.Parser.Project is

   use GPR_Parser.Common;
   use Langkit_Support.Text;
   use type Ada.Containers.Count_Type;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   function Is_Builtin_Project_Name (Name : Name_Type) return Boolean is
     (To_Lower (Name) in "project" | "config" | "runtime");

   --  Some helpers routines for the parser

   function Get_Value_Type
     (Node : Single_Tok_Node'Class) return Value_Type;
   --  Returns the Value_Type for the given node

   function Get_Name_Type
     (Node : Single_Tok_Node'Class) return Name_Type
   is
     (Name_Type (Get_Value_Type (Node)));
   --  Returns the Name for the given node

   function Present (Node : GPR_Node'Class) return Boolean is
     (not Node.Is_Null);
   --  Returns True if the Node is present (not null)

   function Get_Source_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Slr       : Langkit_Support.Slocs.Source_Location_Range)
      return Source_Reference.Object
   is
     (Source_Reference.Object
        (Source_Reference.Create
           (Path_Name.Value,
            Positive (Slr.Start_Line),
            Positive (Slr.Start_Column))));

   function Get_Source_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Node      : GPR_Node'Class) return Source_Reference.Object
   is
     (Get_Source_Reference (Path_Name, Sloc_Range (Node)));

   function Get_Value_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Slr       : Langkit_Support.Slocs.Source_Location_Range;
      Value     : Value_Type;
      At_Num    : Natural := 0) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
       (Source_Reference.Value.Create
         (Get_Source_Reference (Path_Name, Slr), Value, At_Num)));

   function Get_Value_Reference
     (Value  : Value_Type;
      Sloc   : Source_Reference.Object;
      At_Num : Natural := 0) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
       (Source_Reference.Value.Create (Sloc, Value, At_Num)));

   function Get_Identifier_Reference
     (Path_Name  : GPR2.Path_Name.Object;
      Slr        : Langkit_Support.Slocs.Source_Location_Range;
      Identifier : Name_Type)
      return Source_Reference.Identifier.Object
   is
     (Source_Reference.Identifier.Object
       (Source_Reference.Identifier.Create
          (Get_Source_Reference (Path_Name, Slr), Identifier)));

   function Get_Raw_Path
     (Node : Single_Tok_Node'Class) return GPR2.Path_Name.Object
   is
     (GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Get_Name_Type (Node)),
         GPR2.Path_Name.No_Resolution));
   --  Creates project Path_Name.Object not checked for location

   function Get_String_Literal
     (N     : GPR_Node'Class;
      Error : out Boolean) return Value_Type;
   --  Returns the first string literal found under this node. This is an
   --  helper routine to get strings out of built-in parameters for example.
   --  Set Error to True if the node was not a simple string-literal.

   --------------
   -- Extended --
   --------------

   function Extended (Self : Object) return GPR2.Project.Import.Object is
   begin
      return Self.Extended;
   end Extended;

   ---------------
   -- Externals --
   ---------------

   function Externals (Self : Object) return Containers.Name_List is
   begin
      return Self.Externals;
   end Externals;

   ------------------------
   -- Get_String_Literal --
   ------------------------

   function Get_String_Literal
     (N     : GPR_Node'Class;
      Error : out Boolean) return Value_Type
   is
      function Parser (Node : GPR_Node'Class) return Visit_Status;
      --  Parser for the string-literal tree

      Result : Unbounded_String;

      ------------
      -- Parser --
      ------------

      function Parser (Node : GPR_Node'Class) return Visit_Status is

         Status : Visit_Status := Into;

         procedure Handle_String (Node : String_Literal)
           with Pre  => Present (Node), Inline;
         --  A simple static string

         -------------------
         -- Handle_String --
         -------------------

         procedure Handle_String (Node : String_Literal) is
         begin
            Result := To_Unbounded_String
              (Unquote (Value_Type (To_UTF8 (Node.Text))));
         end Handle_String;

      begin
         case Kind (Node) is
            when GPR_String_Literal =>
               Handle_String (Node.As_String_Literal);

            when GPR_String_Literal_At | GPR_Base_List =>
               null;

            when others =>
               --  Everything else is an error
               Error := True;
               Status := Over;
         end case;

         return Status;
      end Parser;

   begin
      Error := False;
      Traverse (N, Parser'Access);

      return Value_Type (To_String (Result));
   end Get_String_Literal;

   --------------------
   -- Get_Value_Type --
   --------------------

   function Get_Value_Type
     (Node : Single_Tok_Node'Class) return Value_Type
   is
      use Ada.Characters.Conversions;
      V      : constant Wide_Wide_String := Text (Node);
      Offset : Natural := 0;
   begin
      if V (V'First) = '"' and then V (V'Last) = '"' then
         Offset := 1;
      end if;
      return To_String (V (V'First + Offset .. V'Last - Offset));
   end Get_Value_Type;

   ------------------
   -- Has_Extended --
   ------------------

   function Has_Extended (Self : Object) return Boolean is
   begin
      return Self.Extended.Is_Defined;
   end Has_Extended;

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

   function Imports (Self : Object) return GPR2.Project.Import.Set.Object is
   begin
      return Self.Imports;
   end Imports;

   ----------------------
   -- Is_Extending_All --
   ----------------------

   function Is_Extending_All (Self : Object) return Boolean is
   begin
      return Self.Is_All;
   end Is_Extending_All;

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

   function Parse
     (Filename      : GPR2.Path_Name.Object;
      Implicit_With : Containers.Name_Set;
      Messages      : out Log.Object) return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object;
      --  Analyze project, record all externals variables and imports

      -------------------
      -- Parse_Stage_1 --
      -------------------

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object is

         Project : Object;
         --  The project being constructed

         function Parser (Node : GPR_Node'Class) return Visit_Status;
         --  Actual parser callabck for the project

         ------------
         -- Parser --
         ------------

         function Parser (Node : GPR_Node'Class) return Visit_Status is

            Status : Visit_Status := Into;

            procedure Parse_Project_Declaration (N : Project_Declaration);
            --  Parse a project declaration and set the qualifier if present

            procedure Parse_Builtin (N : Builtin_Function_Call);
            --  Put the name of the external into the Externals list

            procedure Parse_With_Decl (N : With_Decl);
            --  Add the name of the withed project into the Imports list

            procedure Parse_Typed_String_Decl (N : Typed_String_Decl);
            --  A typed string declaration

            -------------------
            -- Parse_Builtin --
            -------------------

            procedure Parse_Builtin (N : Builtin_Function_Call) is

               procedure Parse_External_Reference
                 (N : Builtin_Function_Call);
               --  Put the name of the external into the Externals list

               procedure Parse_External_As_List_Reference
                 (N : Builtin_Function_Call);
               --  Put the name of the external into the Externals list

               procedure Parse_Split_Reference (N : Builtin_Function_Call);
               --  Check that split parameters has the proper type

               --------------------------------------
               -- Parse_External_As_List_Reference --
               --------------------------------------

               procedure Parse_External_As_List_Reference
                 (N : Builtin_Function_Call)
               is
                  Parameters : constant Expr_List := F_Parameters (N);
                  Exprs      : constant Term_List_List := F_Exprs (Parameters);
               begin
                  --  Note that this routine is only validating the syntax
                  --  of the external_as_list built-in. It does not add the
                  --  variable referenced by the built-in as dependencies
                  --  as an external_as_list result cannot be used in a
                  --  case statement.

                  if Exprs.Is_Null then
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    => Get_Source_Reference (Filename, N),
                           Message =>
                             "missing parameters for external_as_list"
                             & " built-in"));

                  else
                     --  We have External_As_List ("VAR", "SEP"), check the
                     --  variable name.

                     declare
                        Var_Node : constant Term_List :=
                                     Exprs.Child (1).As_Term_List;
                        Error    : Boolean;
                        Var      : constant Value_Type :=
                                     Get_String_Literal (Var_Node, Error);
                     begin
                        if Error then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Var_Node),
                                 Message =>
                                   "external_as_list first parameter must be "
                                   & "a simple string"));

                        elsif Var = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Var_Node),
                                 Message =>
                                   "external_as_list variable name must not "
                                   & "be empty"));
                        end if;
                     end;

                     --  Check that the second parameter exists and is a string

                     if Child (Exprs, 2).Is_Null then
                        Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Filename, Exprs),
                              Message =>
                                "external_as_list requires a second "
                                & "parameter"));
                     else
                        declare
                           Sep_Node : constant Term_List :=
                                        Child (Exprs, 2).As_Term_List;
                           Error    : Boolean;
                           Sep      : constant Value_Type :=
                                        Get_String_Literal (Sep_Node, Error);
                        begin
                           if Error then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sep_Node),
                                    Message =>
                                      "external_as_list second parameter must "
                                      & "be a simple string"));

                           elsif Sep = "" then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sep_Node),
                                    Message =>
                                      "external_as_list separator must not "
                                      & "be empty"));
                           end if;
                        end;
                     end if;
                  end if;
               end Parse_External_As_List_Reference;

               ------------------------------
               -- Parse_External_Reference --
               ------------------------------

               procedure Parse_External_Reference
                 (N : Builtin_Function_Call)
               is
                  Parameters : constant Expr_List := F_Parameters (N);
                  Exprs      : constant Term_List_List := F_Exprs (Parameters);
               begin
                  if Exprs.Is_Null then
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    => Get_Source_Reference (Filename, N),
                           Message =>
                             "missing parameter for external built-in"));

                  else
                     --  We have External ("VAR" [, "VALUE"]), get the
                     --  variable name.

                     declare
                        Var_Node : constant Term_List :=
                                     Child (Exprs, 1).As_Term_List;
                        Error    : Boolean;
                        Var      : constant Value_Type :=
                                     Get_String_Literal (Var_Node, Error);
                     begin
                        if Error then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Var_Node),
                                 Message =>
                                   "external first parameter must be a "
                                   & "simple string"));

                        elsif Var = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Var_Node),
                                 Message =>
                                   "external variable name must not be "
                                   & "empty"));

                        else
                           Project.Externals.Append
                             (Optional_Name_Type (Var));
                        end if;
                     end;
                  end if;
               end Parse_External_Reference;

               ---------------------------
               -- Parse_Split_Reference --
               ---------------------------

               procedure Parse_Split_Reference (N : Builtin_Function_Call) is
                  Parameters : constant Expr_List := F_Parameters (N);
                  Exprs      : constant Term_List_List := F_Exprs (Parameters);
               begin
                  --  Note that this routine is only validating the syntax
                  --  of the split built-in.

                  if Exprs.Is_Null then
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    => Get_Source_Reference (Filename, N),
                           Message =>
                             "missing parameters for split built-in"));

                  else
                     --  We have Split ("STR", "SEP"), check that STR and
                     --  SEP are actually simple strings.

                     declare
                        Str_Node : constant Term_List :=
                                     Child (Exprs, 1).As_Term_List;
                        Error    : Boolean;
                        Str      : constant Value_Type :=
                                     Get_String_Literal (Str_Node, Error);
                     begin
                        if Error then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Str_Node),
                                 Message =>
                                   "split first parameter must be "
                                   & "a simple string"));

                        elsif Str = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference (Filename, Str_Node),
                                 Message =>
                                   "split first parameter must not "
                                   & "be empty"));
                        end if;
                     end;

                     --  Check that the second parameter exists and is a string

                     if Child (Exprs, 2).Is_Null then
                        Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Filename, Exprs),
                              Message => "split requires a second parameter"));
                     else
                        declare
                           Sep_Node : constant Term_List :=
                                         Child (Exprs, 2).As_Term_List;
                           Error    : Boolean;
                           Sep      : constant Value_Type :=
                                        Get_String_Literal (Sep_Node, Error);
                        begin
                           if Error then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sep_Node),
                                    Message =>
                                      "split separator parameter must "
                                      & "be a simple string"));

                           elsif Sep = "" then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sep_Node),
                                    Message =>
                                      "split separator parameter must not "
                                      & "be empty"));
                           end if;
                        end;
                     end if;
                  end if;
               end Parse_Split_Reference;

               Function_Name : constant Name_Type :=
                                 Get_Name_Type (F_Function_Name (N));
            begin
               if Function_Name = "external" then
                  Parse_External_Reference (N);

               elsif Function_Name = "external_as_list" then
                  Parse_External_As_List_Reference (N);

               elsif Function_Name = "split" then
                  Parse_Split_Reference (N);

               else
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message =>
                          "unknown built-in '"
                        & String (Function_Name) & "'"));
               end if;
            end Parse_Builtin;

            -------------------------------
            -- Parse_Project_Declaration --
            -------------------------------

            procedure Parse_Project_Declaration (N : Project_Declaration) is
               Qual : constant Project_Qualifier := F_Qualifier (N);
               Ext  : constant Project_Extension := F_Extension (N);
            begin
               Project.Name := To_Unbounded_String
                 (To_UTF8 (F_Project_Name (N).Text));

               if Name (Project)
                 /= Name_Type (To_UTF8 (F_End_Name (N).Text))
               then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, F_End_Name (N)),
                        Message =>
                          "'end " & String (Name (Project)) & "' expected"));
               end if;

               --  If we have an explicit qualifier parse it now. If not the
               --  kind of project will be determined later during a second
               --  pass.

               if Present (Qual) then
                  case Kind (F_Qualifier (Qual)) is
                     when GPR_Abstract_Present =>
                        Project.Qualifier := K_Abstract;

                     when GPR_Qualifier_Names =>
                        declare
                           Not_Present : constant Name_Type := "@";

                           Names : constant Qualifier_Names :=
                                     Qual.F_Qualifier.As_Qualifier_Names;
                           Name_1 : constant Identifier :=
                                      F_Qualifier_Id1 (Names);
                           Str_1  : constant Name_Type :=
                                      (if Name_1.Is_Null
                                       then Not_Present
                                       else Get_Name_Type
                                              (Name_1.As_Single_Tok_Node));
                           Name_2 : constant Identifier :=
                                      F_Qualifier_Id2 (Names);
                           Str_2  : constant Name_Type :=
                                      (if Name_2.Is_Null
                                       then Not_Present
                                       else Get_Name_Type
                                              (Name_2.As_Single_Tok_Node));
                        begin
                           if Str_1 = "library"
                             and then Str_2 = Not_Present
                           then
                              Project.Qualifier := K_Library;

                           elsif Str_1 = "aggregate"
                             and then Str_2 = Not_Present
                           then
                              Project.Qualifier := K_Aggregate;

                           elsif Str_1 = "aggregate"
                             and then Str_2 = "library"
                           then
                              Project.Qualifier := K_Aggregate_Library;

                           elsif Str_1 = "configuration"
                             and then Str_2 = Not_Present
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

               --  Check if we have an extends declaration

               if Present (Ext) then
                  Project.Extended :=
                    GPR2.Project.Import.Create
                      (Get_Raw_Path (F_Path_Name (Ext)),
                       Get_Source_Reference (Filename, Ext),
                       Is_Limited => False);
                  Project.Is_All := F_Is_All (Ext);
               end if;
            end Parse_Project_Declaration;

            -----------------------------
            -- Parse_Typed_String_Decl --
            -----------------------------

            procedure Parse_Typed_String_Decl (N : Typed_String_Decl) is
               Name       : constant Name_Type :=
                              Get_Name_Type (F_Type_Id (N));
               Values     : constant String_Literal_List :=
                              F_String_Literals (N);
               Num_Childs : constant Natural := Children_Count (Values);
               Cur_Child  : GPR_Node;
               Set        : Containers.Value_Set;
               List       : Containers.Source_Value_List;
            begin
               if Project.Types.Contains (Name) then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, F_Type_Id (N)),
                        Message =>
                          "type " & String (Name) & " already defined"));

               else
                  for J in 1 .. Num_Childs loop
                     Cur_Child := Child (GPR_Node (Values), J);

                     if not Cur_Child.Is_Null then
                        declare
                           Value : constant Value_Type :=
                                     Get_Value_Type
                                       (Cur_Child.As_String_Literal);
                        begin
                           if Set.Contains (Value) then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Cur_Child),
                                    Message =>
                                      String (Name)
                                      & " has duplicate value '"
                                      & String (Value) & '''));
                           else
                              Set.Insert (Value);
                              List.Append
                                (Get_Value_Reference
                                   (Filename, Sloc_Range (Cur_Child), Value));
                           end if;
                        end;
                     end if;
                  end loop;

                  Project.Types.Insert
                    (Name,
                     GPR2.Project.Typ.Create
                       (Get_Identifier_Reference
                          (Filename, Sloc_Range (F_Type_Id (N)), Name), List));
               end if;
            end Parse_Typed_String_Decl;

            ---------------------
            -- Parse_With_Decl --
            ---------------------

            procedure Parse_With_Decl (N : With_Decl) is
               Path_Names : constant String_Literal_List :=
                              F_Path_Names (N);
               Num_Childs : constant Natural := Children_Count (N);
               Cur_Child  : GPR_Node;
            begin
               for J in 1 .. Num_Childs loop
                  Cur_Child := Child (GPR_Node (Path_Names), J);

                  if not Cur_Child.Is_Null then
                     declare
                        use type GPR2.Path_Name.Object;

                        Path : constant GPR2.Path_Name.Object :=
                                 Get_Raw_Path (Cur_Child.As_String_Literal);
                     begin
                        if Project.Imports.Contains (Path) then
                           declare
                              Prev : constant GPR2.Project.Import.Object :=
                                       Project.Imports.Element (Path);
                           begin
                              if Prev.Path_Name = Path then
                                 Messages.Append
                                   (GPR2.Message.Create
                                      (Level   => Message.Warning,
                                       Message => "duplicate with clause '"
                                       & String (Path.Base_Name) & ''',
                                       Sloc    => Get_Source_Reference
                                         (Filename, Cur_Child)));

                              else
                                 Messages.Append
                                   (GPR2.Message.Create
                                      (Level   => Message.Warning,
                                       Message => "duplicate project name '"
                                       & String (Path.Base_Name) & ''',
                                       Sloc    => Get_Source_Reference
                                         (Filename, Cur_Child)));
                                 Messages.Append
                                   (GPR2.Message.Create
                                      (Level   => Message.Warning,
                                       Message => "already in '"
                                       & String (Prev.Path_Name.Name)
                                       & ''',
                                       Sloc    => Get_Source_Reference
                                         (Filename, Cur_Child)));
                              end if;
                           end;

                        else
                           Project.Imports.Insert
                             (GPR2.Project.Import.Create
                                (Path,
                                 Get_Source_Reference (Filename, Cur_Child),
                                 F_Is_Limited (N)));
                        end if;
                     end;
                  end if;
               end loop;
            end Parse_With_Decl;

         begin
            case Kind (Node) is
               when GPR_Project_Declaration =>
                  Parse_Project_Declaration (Node.As_Project_Declaration);

               when GPR_Builtin_Function_Call =>
                  Parse_Builtin (Node.As_Builtin_Function_Call);
                  Status := Over;

               when GPR_With_Decl =>
                  Parse_With_Decl (Node.As_With_Decl);
                  Status := Over;

               when GPR_Typed_String_Decl =>
                  Parse_Typed_String_Decl (Node.As_Typed_String_Decl);
                  Status := Over;

               when others =>
                  null;
            end case;

            return Status;
         end Parser;

      begin
         Traverse (Root (Unit), Parser'Access);

         --  Import --implicit-with options

         for W of Implicit_With loop
            declare
               use GPR2.Path_Name;
               PN : constant GPR2.Path_Name.Object :=
                      Create_File (W, No_Resolution);
            begin
               if PN /= Filename
                 and then not Project.Imports.Contains (PN)
               then
                  Project.Imports.Insert
                    (GPR2.Project.Import.Create
                       (PN,
                        Source_Reference.Object
                          (Source_Reference.Create (Filename.Value, 0, 0)),
                        Is_Limited => True));
               end if;
            end;
         end loop;

         return Project;
      end Parse_Stage_1;

      Context : constant Analysis_Context := Create_Context ("UTF-8");
      Unit    : Analysis_Unit;
      Project : Object;

      Empty_Project : constant String :=
                        "standard project Default is end Default;";

   begin
      if Registry.Check_Project (Filename, Project) then
         return Project;

      else
         if not Filename.Exists then
            Messages.Append
              (GPR2.Message.Create
                 (Level   => Message.Error,
                  Message => "project file '" & Filename.Value & "' not found",
                  Sloc    => Source_Reference.Create (Filename.Value, 0, 0)));
            return Undefined;
         end if;

         if Filename.Is_Directory then
            Unit := Get_From_Buffer
                      (Context, Filename.Value, Buffer => Empty_Project);
         else
            Unit := Get_From_File (Context, Filename.Value);
         end if;

         if Root (Unit).Is_Null or else Has_Diagnostics (Unit) then
            if Has_Diagnostics (Unit) then
               for D of Diagnostics (Unit) loop
                  declare
                     Sloc : constant Source_Reference.Object'Class :=
                              Source_Reference.Create
                                (Filename => Filename.Value,
                                 Line     =>
                                   Natural (D.Sloc_Range.Start_Line),
                                 Column   =>
                                   Natural (D.Sloc_Range.Start_Column));
                  begin
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    => Sloc,
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

         --  If this is a configuration project, then we register it under the
         --  "config" name as this is what is expected on this implementation.
         --  That is, referencing the configuration is done using
         --  Config'Archive_Suffix for example.

         if Project.Qualifier = K_Configuration then
            Project.Name := To_Unbounded_String ("Config");
         end if;

         --  Finaly register this project into the registry

         Registry.Register (Filename, Project);

         return Project;
      end if;
   end Parse;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.File;
   end Path_Name;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out Object;
      Tree    : GPR2.Project.Tree.Object;
      Context : GPR2.Context.Object;
      View    : GPR2.Project.View.Object;
      Attrs   : in out GPR2.Project.Attribute.Set.Object;
      Vars    : in out GPR2.Project.Variable.Set.Object;
      Packs   : in out GPR2.Project.Pack.Set.Object;
      Types   : in out GPR2.Project.Typ.Set.Object)
   is

      type Item_Values is record
         Values : Containers.Source_Value_List;
         Single : Boolean := False;
      end record
        with Dynamic_Predicate =>
          (if Item_Values.Single then Item_Values.Values.Length = 1);

      function To_Set
        (Values : Containers.Source_Value_List) return Containers.Value_Set;
      --  Create a set for fast searchiing from a list of values

      Empty_Item_Values : constant Item_Values :=
                            (Single => False, Values => <>);

      function Parser (Node : GPR_Node'Class) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values;
      --  Parse and return the value for the given variable reference

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Optional_Name_Type := "") return Item_Values;
      --  Return the value for an attribute reference in the given project and
      --  possibly the given package.

      function Get_Variable_Ref
        (Project : Name_Type;
         Node    : Identifier;
         Pack    : Identifier := No_Identifier) return Item_Values;
      --  Return the value for a variable reference in the given project

      function Is_Limited_Import
        (Self : Object; Project : Name_Type) return Boolean;
      --  Returns True if the given project exists and is made visible through
      --  a limited immport clause.

      function Get_Term_List (Node : Term_List) return Item_Values;
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

      function Has_Error return Boolean is
        (Tree.Log_Messages.Has_Error);

      --  Global variables used to keep state during the parsing. While
      --  visiting child nodes we may need to record status (when in a package
      --  or a case construct for example). This parsing state is then used
      --  to apply different check or recording.

      --  The parsing status for case statement (possibly nested)

      Case_Values : Containers.Value_List;
      --  The case-values to match against the case-item. Each time a case
      --  statement is enterred the value for the case is prepended into this
      --  vector. The first value is then removed when exiting from the case
      --  statement. This is to support nested case statements.

      Is_Open     : Boolean := True;
      --  Is_Open is a parsing barrier, it is True when parsing can be
      --  conducted and False otherwise. Is_Open is set to False when enterring
      --  a case construct. It is then set to True/False depending on the case
      --  When Is_Open is False no parsing should be done, that is all node
      --  should be ignored except the Case_Item ones.

      --  Package orientated state, when parsing is in a package In_Pack is
      --  set and Pack_Name contains the name of the package and all parsed
      --  attributes are recorded into Pack_Attrs set.
      --
      --  Att_Name and Idx_Name are the name for the currently parsed attribute
      --  declaration. This is used to check for recursive definition.
      --     for Att (Idx) use Att (Idx) & ("some", "other", "value");

      In_Pack              : Boolean := False;
      Pack_Name            : Unbounded_String;
      Pack_Attrs           : GPR2.Project.Attribute.Set.Object;
      Pack_Vars            : GPR2.Project.Variable.Set.Object;
      Is_Project_Reference : Boolean := False;
      --  Is_Project_Reference is True when using: Project'<attribute>

      Undefined_Attribute_Count          : Natural := 0;
      Previous_Undefined_Attribute_Count : Natural := 0;

      -----------------------
      -- Get_Attribute_Ref --
      -----------------------

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Optional_Name_Type := "") return Item_Values
      is
         use type PRA.Value_Kind;

         Sloc   : constant Source_Reference.Object :=
                    Get_Source_Reference (Self.File, Node);
         Name   : constant Name_Type :=
                    Get_Name_Type
                      (Single_Tok_Node (F_Attribute_Name (Node)));
         I_Node : constant GPR_Node := F_Attribute_Index (Node);
         Index  : constant PAI.Object :=
                    (if Present (I_Node)
                     then PAI.Create
                            (Get_Value_Type (I_Node.As_Single_Tok_Node))
                     else PAI.Undefined);
         View   : constant GPR2.Project.View.Object :=
                    Process.View.View_For (Project);

         Attr   : GPR2.Project.Attribute.Object;

      begin
         --  We do not want to have a reference to a limited import, we do not
         --  check when a special project reference is found Project'Name or
         --  Config'Name.

         if not Is_Builtin_Project_Name (Project)
           and then Is_Limited_Import (Self, Project)
         then
            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  "cannot have a reference to a limited project",
                  Get_Source_Reference (Self.File, Node)));

            return Empty_Item_Values;
         end if;

         --  For a project/attribute reference we need to check the attribute
         --  definition to know wether the result is multi-valued or not.

         if not PRA.Exists (PRA.Create (Name, Pack)) then
            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  "attribute """ & String (Name) & """ is not defined",
                  Get_Source_Reference (Self.File, Node)));

            return Empty_Item_Values;
         end if;

         --  If the attribute is not found or not yet resolved we need
         --  to ensure that the Values list respect the post
         --  condition. That is, a Single result must contain a single
         --  element.

         if Project = Name_Type (To_String (Self.Name))
           or else Is_Project_Reference
         then
            --  An attribute referencing a value in the current project

            --  Record only if fully defined (add a boolean to control this)
            --  and stop parsing when the number of undefined attribute is
            --  stable.

            if Pack = "" then
               Attr := Attrs.Element (Name, Index);

               if Attr.Is_Defined then
                  --  Found, no need other conditions

                  null;

               --  Special cases for some built-in references

               elsif not Index.Is_Defined then
                  if Name_Type (To_Lower (Name))
                    in PRA.Target | PRA.Canonical_Target
                  then
                     --  Project'Target

                     return R : Item_Values do
                        R.Single := True;
                        R.Values.Append
                          (Get_Value_Reference (To_Lower (Tree.Target), Sloc));
                     end return;
                  end if;

               elsif Index.Is_Defined
                 and then Name = PRA.Runtime
               then
                  --  Project'Runtime ("<lang>")

                  return R : Item_Values do
                     R.Single := True;
                     R.Values.Append
                       (Get_Value_Reference
                          (To_Lower
                             (Tree.Runtime (Optional_Name_Type (Index.Text))),
                           Sloc));
                  end return;
               end if;

            elsif Pack_Name /= Null_Unbounded_String
              and then Name_Type (To_String (Pack_Name)) = Name_Type (Pack)
            then
               --  This is the current parsed package, look into Pack_Attrs

               Attr := Pack_Attrs.Element (Name, Index);

            elsif Packs.Contains (Name_Type (Pack)) then
               --  Or in another package in the same project

               Attr := Packs.Element
                 (Name_Type (Pack)).Attributes.Element (Name, Index);
            end if;

         elsif View.Is_Defined then
            if Pack = "" then
               if not View.Check_Attribute (Name, Index, Result => Attr) then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "attribute """ & String (Name)
                        & """ is not defined in project """ & String (Project)
                        & '"',
                        Get_Source_Reference (Self.File, Node)));
               end if;

            elsif View.Has_Packages (Pack)
              and then not View.Pack (Pack).Check_Attribute
                             (Name, Index, Result => Attr)
            then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (Name)
                     & """ is not defined in project """
                     & String (Project) & """ package """ & String (Pack)
                     & '"',
                     Get_Source_Reference (Self.File, Node)));
            end if;

         elsif Project /= "Config" then
            --  Config project can be undefined at this stage

            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  "Project """ & String (Project) & """ not found",
                  Get_Source_Reference (Self.File, Node)));
         end if;

         return (if Attr.Is_Defined
                 then (Attr.Values, Attr.Kind = PRA.Single)
                 else Empty_Item_Values);
      end Get_Attribute_Ref;

      -------------------
      -- Get_Term_List --
      -------------------

      function Get_Term_List (Node : Term_List) return Item_Values is

         Result : Item_Values;
         --  The list of values returned by Get_Term_List

         New_Item : Boolean := True;

         function Parser (Node : GPR_Node'Class) return Visit_Status;

         procedure Record_Value (Value : Source_Reference.Value.Object)
           with Post => Result.Values.Length'Old <= Result.Values.Length;
         --  Record Value into Result, either add it as a new value in the list
         --  (Single = False) or append the value to the current one.

         procedure Record_Values (Values : Item_Values);
         --  Same as above but for multiple values

         ------------
         -- Parser --
         ------------

         function Parser (Node : GPR_Node'Class) return Visit_Status is
            Status : Visit_Status := Into;

            procedure Handle_String (Node : String_Literal)
              with Pre => Present (Node);
            --  A simple static string

            procedure Handle_String_At (Node : String_Literal_At)
              with Pre => Present (Node);
            --  A simple static string with "at" number.
            --  The number is retrieved and used later in Parse_Attribute_Decl.

            procedure Handle_Variable (Node : Variable_Reference)
              with Pre => Present (Node);
            --  A variable

            procedure Handle_Builtin (Node : Builtin_Function_Call)
              with Pre => Present (Node);
            --  A built-in

            procedure Handle_Attribute_Reference (Node : Attribute_Reference)
              with Pre  => Present (Node);
            --  An attribute reference for ProjectReference node only. The
            --  other cases are handled in other parts.

            --------------------------------
            -- Handle_Attribute_Reference --
            --------------------------------

            procedure Handle_Attribute_Reference
              (Node : Attribute_Reference) is
            begin
               if Is_Project_Reference then
                  Record_Values (Get_Attribute_Ref ("project", Node));
               end if;
               Status := Over;
            end Handle_Attribute_Reference;

            --------------------
            -- Handle_Builtin --
            --------------------

            procedure Handle_Builtin (Node : Builtin_Function_Call) is

               procedure Handle_External_Variable
                 (Node : Builtin_Function_Call);
               --  An external variable : External ("VAR"[, "VALUE"])

               procedure Handle_External_As_List_Variable
                 (Node : Builtin_Function_Call);
               --  An external_as_list variable :
               --    External_As_List ("VAR", "SEP")

               procedure Handle_Split (Node : Builtin_Function_Call);
               --  Handle the Split built-in : Split ("STR1", "SEP")

               --------------------------------------
               -- Handle_External_As_List_Variable --
               --------------------------------------

               procedure Handle_External_As_List_Variable
                 (Node : Builtin_Function_Call)
               is
                  Parameters : constant Term_List_List :=
                                 F_Exprs (F_Parameters (Node));
                  Error      : Boolean with Unreferenced;
                  Var        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 1), Error));
                  Sep        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 2), Error));
               begin
                  for V of Builtin.External_As_List (Context, Var, Sep) loop
                     New_Item := True;
                     Record_Value
                       (Get_Value_Reference
                          (V, Get_Source_Reference (Self.File, Parameters)));
                  end loop;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;
               end Handle_External_As_List_Variable;

               ------------------------------
               -- Handle_External_Variable --
               ------------------------------

               procedure Handle_External_Variable
                 (Node : Builtin_Function_Call)
               is
                  use Ada.Exceptions;

                  Parameters : constant Term_List_List :=
                                 F_Exprs (F_Parameters (Node));
                  Error      : Boolean;
                  Var        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 1), Error));
                  Value_Node : constant Term_List :=
                                 Child (Parameters, 2).As_Term_List;
               begin
                  if Present (Value_Node) then
                     --  External not in the context but has a default value
                     declare
                        Values : constant Item_Values :=
                                   Get_Term_List (Value_Node);
                     begin
                        if Values.Single then
                           Record_Value
                             (Builtin.External
                                (Context,
                                 Var, Values.Values.First_Element));

                        else
                           Tree.Log_Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference
                                     (Self.File, Parameters),
                                 Message =>
                                   "external default parameter must be a "
                                   & "simple string"));
                        end if;
                     end;

                  else
                     Record_Value
                       (Builtin.External
                          (Context, Var,
                           Sloc => Get_Source_Reference
                                     (Self.File, Parameters)));
                  end if;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;

               exception
                  when E : Project_Error =>
                     Tree.Log_Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    =>
                              Get_Source_Reference (Self.File, Parameters),
                           Message => Exception_Message (E)));
                     Record_Value
                       (Get_Value_Reference
                          ("", Get_Source_Reference (Self.File, Parameters)));
                     Status := Over;
               end Handle_External_Variable;

               ------------------
               -- Handle_Split --
               ------------------

               procedure Handle_Split (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Exprs (F_Parameters (Node));
                  Error      : Boolean with Unreferenced;
                  Str        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 1), Error));
                  Sep        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 2), Error));
               begin
                  for V of Builtin.Split (Str, Sep) loop
                     New_Item := True;
                     Record_Value
                       (Get_Value_Reference
                          (V, Get_Source_Reference (Self.File, Parameters)));
                  end loop;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;
               end Handle_Split;

               Function_Name : constant Name_Type :=
                                 Get_Name_Type (F_Function_Name (Node));
            begin
               if Function_Name = "external" then
                  Handle_External_Variable (Node);

               elsif Function_Name = "external_as_list" then
                  Result.Single := False;
                  Handle_External_As_List_Variable (Node);

               elsif Function_Name = "split" then
                  Result.Single := False;
                  Handle_Split (Node);
               end if;
            end Handle_Builtin;

            -------------------
            -- Handle_String --
            -------------------

            procedure Handle_String (Node : String_Literal) is
            begin
               Record_Value
                 (Get_Value_Reference
                    (Unquote (Value_Type (To_UTF8 (Node.Text))),
                     Get_Source_Reference (Self.File, Node)));
            end Handle_String;

            ----------------------
            -- Handle_String_At --
            ----------------------

            procedure Handle_String_At (Node : String_Literal_At) is
               At_Lit : constant Num_Literal := Node.F_At_Lit;
            begin
               Record_Value
                 (Get_Value_Reference
                    (Unquote (Value_Type (To_UTF8 (Node.F_Str_Lit.Text))),
                     Get_Source_Reference (Self.File, Sloc_Range (Node)),
                     At_Num => (if At_Lit = No_GPR_Node then 0
                                else Positive'Wide_Wide_Value (At_Lit.Text))));
               Status := Over;
               --  Stop here to avoid parsing into the String_Literal child
            end Handle_String_At;

            ---------------------
            -- Handle_Variable --
            ---------------------

            procedure Handle_Variable (Node : Variable_Reference) is
               Values : constant Item_Values := Get_Variable_Values (Node);
            begin
               Record_Values (Values);
               Status := Over;
            end Handle_Variable;

         begin
            case Kind (Node) is
               when GPR_Expr_List =>
                  --  We are opening not a single element but an expression
                  --  list.
                  Result.Single := False;

               when GPR_Term_List =>
                  --  A new value is found
                  New_Item := True;

               when GPR_String_Literal =>
                  Handle_String (Node.As_String_Literal);

               when GPR_String_Literal_At =>
                  Handle_String_At (Node.As_String_Literal_At);

               when GPR_Variable_Reference =>
                  Handle_Variable (Node.As_Variable_Reference);

               when GPR_Builtin_Function_Call =>
                  Handle_Builtin (Node.As_Builtin_Function_Call);

               when GPR_Project_Reference =>
                  Is_Project_Reference := True;

               when GPR_Attribute_Reference =>
                  Handle_Attribute_Reference (Node.As_Attribute_Reference);

               when others =>
                  null;
            end case;

            return Status;
         end Parser;

         ------------------
         -- Record_Value --
         ------------------

         procedure Record_Value (Value : Source_Reference.Value.Object) is
         begin
            if New_Item then
               Result.Values.Append (Value);
               New_Item := False;

            else
               declare
                  Last      : constant Containers.Extended_Index :=
                                Result.Values.Last_Index;
                  Old_Value : constant Source_Reference.Value.Object :=
                                Result.Values (Last);
                  New_Value : constant Value_Type :=
                                Old_Value.Text & Value.Text;
               begin
                  Result.Values.Replace_Element
                    (Last,
                     Get_Value_Reference
                       (New_Value,
                        Source_Reference.Object (Value)));
               end;
            end if;
         end Record_Value;

         -------------------
         -- Record_Values --
         -------------------

         procedure Record_Values (Values : Item_Values) is
         begin
            for V of Values.Values loop
               New_Item := New_Item or else Values.Single = False;
               Record_Value (V);
            end loop;

            --  If we add a list, then the final value must be a list

            if not Values.Single then
               Result.Single := False;
            end if;
         end Record_Values;

      begin
         Result.Single := True;
         Is_Project_Reference := False;

         Traverse (GPR_Node (Node), Parser'Access);

         Is_Project_Reference := False;

         if Result.Values.Is_Empty then
            return Empty_Item_Values;
         else
            return Result;
         end if;
      end Get_Term_List;

      ----------------------
      -- Get_Variable_Ref --
      ----------------------

      function Get_Variable_Ref
        (Project : Name_Type;
         Node    : Identifier;
         Pack    : Identifier := No_Identifier) return Item_Values
      is
         use type PRA.Value_Kind;

         function Get_Pack_Var
           (Pack : GPR2.Project.Pack.Object;
            Name : Name_Type) return Item_Values with Inline;
         --  Returns the variable value Pack.Name

         ------------------
         -- Get_Pack_Var --
         ------------------

         function Get_Pack_Var
           (Pack : GPR2.Project.Pack.Object;
            Name : Name_Type) return Item_Values is
         begin
            if Pack.Has_Variables (Name) then
               declare
                  V : constant GPR2.Project.Variable.Object :=
                        Pack.Variable (Name);
               begin
                  return (V.Values, V.Kind = PRA.Single);
               end;

            else
               return Empty_Item_Values;
            end if;
         end Get_Pack_Var;

         Name : constant Name_Type := Get_Name_Type (Node);
         View : constant GPR2.Project.View.Object :=
                  Process.View.View_For (Project);

         Result : Item_Values := Empty_Item_Values;

      begin
         if not View.Is_Defined then
            --  Some maybe Project is actually a local package

            if Packs.Contains (Project) then
               Result := Get_Pack_Var (Packs.Element (Project), Name);

            elsif not Is_Builtin_Project_Name (Project) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "project " & String (Project) & " is undefined",
                     Get_Source_Reference (Self.File, Node)));
            end if;

         else
            if Present (Pack) then
               --  reference is : Project.Pack.Var_Name
               Check_Pack : declare
                  P_Name : constant Name_Type := Get_Name_Type (Pack);
               begin
                  if View.Has_Packages (P_Name) then
                     Result := Get_Pack_Var
                       (View.Packages.Element (P_Name), Name);
                  end if;
               end Check_Pack;

            elsif View.Has_Variables (Name) then
               --  reference is : Project.Var_Name
               declare
                  V : constant GPR2.Project.Variable.Object :=
                        View.Variable (Name);
               begin
                  Result := (V.Values, V.Kind = PRA.Single);
               end;
            end if;
         end if;

         return Result;
      end Get_Variable_Ref;

      -------------------------
      -- Get_Variable_Values --
      -------------------------

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values
      is
         use type PRA.Value_Kind;

         Name_1  : constant Identifier := F_Variable_Name1 (Node);
         Name_2  : constant Identifier := F_Variable_Name2 (Node);
         Name_3  : constant Identifier := F_Variable_Name3 (Node);
         Att_Ref : constant Attribute_Reference := F_Attribute_Ref (Node);
         Name    : constant Simple_Name := Simple_Name (To_UTF8 (Name_1.Text));
      begin
         if Present (Att_Ref) then
            if Present (Name_2) then
               --  This is a project/package reference:
               --    <project>.<package>'<attribute>
               return Get_Attribute_Ref
                 (Project => Name,
                  Pack    => Optional_Name_Type (To_UTF8 (Name_2.Text)),
                  Node    => Att_Ref);

            else
               --  If a single name it can be either a project or a package

               if Self.Imports.Contains (Name)
                 or else (Self.Extended.Is_Defined
                            and then
                          Optional_Name_Type
                            (Self.Extended.Path_Name.Base_Name) = Name)
                 or else Is_Builtin_Project_Name (Name)
               then
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

         elsif Present (Name_3) then
            --  Project.Pack.Name
            return Get_Variable_Ref (Name, Name_3, Name_2);

         elsif Present (Name_2) then
            --  Project.Name or Package.Name
            return Get_Variable_Ref (Name, Name_2);

         elsif In_Pack and then Pack_Vars.Contains (Name) then
            --  Name (being defined into the current package)
            return
              (Pack_Vars (Name).Values,
               Pack_Vars (Name).Kind = GPR2.Project.Registry.Attribute.Single);

         elsif Vars.Contains (Name) then
            --  Name (being defined in current project)
            return
              (Vars (Name).Values,
               Vars (Name).Kind = GPR2.Project.Registry.Attribute.Single);

         else
            declare
               Result : Item_Values := Empty_Item_Values;
            begin
               if Self.Extended.Is_Defined then
                  declare
                     View : constant GPR2.Project.View.Object :=
                              Process.View.View_For
                                (Name_Type
                                   (Self.Extended.Path_Name.Base_Name));
                  begin
                     if View.Is_Defined
                       and then View.Has_Variables (Name)
                     then
                        declare
                           V : constant GPR2.Project.Variable.Object :=
                                 View.Variable (Name);
                        begin
                           Result :=
                             (Values => V.Values,
                              Single => V.Count_Values = 1);
                        end;
                     end if;
                  end;
               end if;

               if Result = Empty_Item_Values then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Self.File, Node),
                        Message =>
                          "variable '" & String (Name) & "' is undefined"));
               end if;

               return Result;
            end;
         end if;
      end Get_Variable_Values;

      -----------------------
      -- Is_Limited_Import --
      -----------------------

      function Is_Limited_Import
        (Self : Object; Project : Name_Type) return Boolean is
      begin
         return Self.Imports.Contains (Project)
           and then Self.Imports.Element (Project).Is_Limited;
      end Is_Limited_Import;

      ------------
      -- Parser --
      ------------

      function Parser (Node : GPR_Node'Class) return Visit_Status is
         Status : Visit_Status := Into;

         procedure Parse_Attribute_Decl (Node : Attribute_Decl)
           with Pre => Is_Open;
         --  Parse attribute declaration and append it into Attrs set

         procedure Parse_Variable_Decl (Node : Variable_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Package_Decl (Node : Package_Decl)
           with Pre => Is_Open;
         --  Parse variable declaration and append it into the Vars set

         procedure Parse_Package_Renaming (Node : Package_Renaming)
           with Pre => Is_Open;
         --  Parse a package renaming

         procedure Parse_Package_Extension (Node : Package_Extension)
           with Pre => Is_Open;
         --  Parse a package extension

         procedure Parse_Case_Construction (Node : Case_Construction)
           with Pre  => Is_Open,
                Post => Case_Values.Length'Old = Case_Values.Length;
         --  Parse a case construction, during a case construction parsing the
         --  Is_Open flag may be set to False and True. Set Is_Open comments.

         procedure Parse_Case_Item (Node : Case_Item)
           with Pre => not Case_Values.Is_Empty;
         --  Set Is_Open to True or False depending on the item

         procedure Visit_Child (Child : GPR_Node);
         --  Recursive call to the Parser if the Child is not null

         --------------------------
         -- Parse_Attribute_Decl --
         --------------------------

         procedure Parse_Attribute_Decl (Node : Attribute_Decl) is
            Name  : constant Identifier := F_Attr_Name (Node);
            Index : constant GPR_Node := F_Attr_Index (Node);
            Expr  : constant Term_List := F_Expr (Node);
            N_Str : constant Name_Type :=
                      Get_Name_Type (Name.As_Single_Tok_Node);
         begin
            declare
               Q_Name   : constant PRA.Qualified_Name :=
                            PRA.Create
                              (N_Str,
                               Optional_Name_Type (To_String (Pack_Name)));

               Values    : constant Item_Values := Get_Term_List (Expr);
               A         : GPR2.Project.Attribute.Object;
               Is_Valid  : Boolean := True;
               --  Set to False if the attribute definition is invalid

               function Create_Index return PAI.Object;
               --  Create index with "at" part if exists

               ------------------
               -- Create_Index --
               ------------------

               function Create_Index return PAI.Object is
                  Str_Lit : String_Literal_At;
                  At_Lit  : Num_Literal;
               begin
                  if Index.Kind = GPR_Others_Designator then
                     return PAI.Create
                       (Get_Value_Reference
                          (Self.Path_Name, Sloc_Range (Index), "others"),
                        Is_Others      => True,
                        Case_Sensitive => False);

                  else
                     Str_Lit := Index.As_String_Literal_At;
                     At_Lit  := Str_Lit.F_At_Lit;

                     return PAI.Create
                       (Get_Value_Reference
                          (Self.Path_Name, Sloc_Range (Index),
                           Get_Value_Type (Str_Lit.F_Str_Lit),
                           At_Num => -- Ati),
                             (if At_Lit = No_GPR_Node
                              then 0
                              else Positive'Wide_Wide_Value (At_Lit.Text))),
                        Is_Others      => False,
                        Case_Sensitive => False);
                  end if;
               end Create_Index;

               I_Sloc : constant PAI.Object :=
                          (if Present (Index)
                           then Create_Index
                           else PAI.Undefined);

            begin
               if Values.Single then
                  pragma Assert (Expr.Children_Count >= 1);

                  A := GPR2.Project.Attribute.Create
                    (Name            => Get_Identifier_Reference
                                          (Self.Path_Name,
                                           Sloc_Range (Name),
                                           N_Str),
                     Index           => I_Sloc,
                     Value           => Values.Values.First_Element);

               else
                  A := GPR2.Project.Attribute.Create
                    (Name            => Get_Identifier_Reference
                                          (Self.Path_Name,
                                           Sloc_Range (Name),
                                           N_Str),
                     Index           => I_Sloc,
                     Values          => Values.Values);
               end if;

               --  Record attribute with proper casing definition if found

               if PRA.Exists (Q_Name) then
                  declare
                     Def : constant PRA.Def := PRA.Get (Q_Name);

                     function Sloc return Source_Reference.Object is
                       (Get_Source_Reference (Self.File, Node));
                     --  Use function instead of constant because Sloc need
                     --  only in case of warning or error logging and no more
                     --  than once.

                  begin
                     if (Values.Single
                         and then Values.Values.First_Element.Text = "")
                       or else (not Values.Single
                                and then Values.Values.Length = 0)
                     then
                        case Def.Empty_Value is
                           when PRA.Allow =>
                              null;

                           when PRA.Ignore =>
                              Tree.Log_Messages.Append
                                (Message.Create
                                   (Level   => Message.Warning,
                                    Sloc    => Sloc,
                                    Message => "Empty attribute "
                                    & PRA.Image (Q_Name) & " ignored"));
                              Is_Valid := False;

                           when PRA.Error =>
                              Tree.Log_Messages.Append
                                (Message.Create
                                   (Level   => Message.Error,
                                    Sloc    => Sloc,
                                    Message => "Attribute "
                                               & PRA.Image (Q_Name)
                                               & " can't be empty"));
                        end case;
                     end if;

                     A.Set_Case
                       (Def.Index_Case_Sensitive,
                        Def.Value_Case_Sensitive);
                  end;
               end if;

               if Is_Valid then
                  if In_Pack then
                     Record_Attribute (Pack_Attrs, A);
                  else
                     Record_Attribute (Attrs, A);
                  end if;
               end if;
            end;
         end Parse_Attribute_Decl;

         -----------------------------
         -- Parse_Case_Construction --
         -----------------------------

         procedure Parse_Case_Construction (Node : Case_Construction) is
            Var   : constant Variable_Reference := F_Var_Ref (Node);
            Value : constant Containers.Source_Value_List :=
                      Get_Variable_Values (Var).Values;
         begin
            if Value.Length = 1 then
               Case_Values.Prepend (Value.First_Element.Text);

               --  Set status to close for now, this will be open when a
               --  when_clause will match the value pushed just above on
               --  the vector.

               Is_Open := False;

               declare
                  Childs : constant Case_Item_List := F_Items (Node);
               begin
                  Check_Case_Item : for C in 1 .. Children_Count (Childs) loop
                     Visit_Child (Child (GPR_Node (Childs), C));

                     --  Exit this look as soon as a case item has matched.
                     --  We do not want an other clause to match if an open
                     --  case-item has already been found and handled.
                     exit Check_Case_Item when Is_Open;
                  end loop Check_Case_Item;
               end;

               --  Then remove the case value

               Case_Values.Delete_First;

               --  Skip all nodes for this construct

               Status := Over;

               Is_Open := True;

            elsif Has_Error then
               null;

            else
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Get_Source_Reference (Self.File, Node),
                     Message => "variable '"
                       & Get_Value_Type (F_Variable_Name1 (Var))
                       & "' must be a simple value"));
            end if;
         end Parse_Case_Construction;

         ---------------------
         -- Parse_Case_Item --
         ---------------------

         procedure Parse_Case_Item (Node : Case_Item) is

            function Parser (Node : GPR_Node'Class) return Visit_Status;

            Is_Case_Item_Matches : Boolean := False;

            ------------
            -- Parser --
            ------------

            function Parser (Node : GPR_Node'Class) return Visit_Status is
               Status : constant Visit_Status := Into;

               procedure Handle_String   (Node : String_Literal);

               -------------------
               -- Handle_String --
               -------------------

               procedure Handle_String (Node : String_Literal) is
                  Value : constant Value_Type := Unquote (To_UTF8 (Node.Text));
               begin
                  Is_Case_Item_Matches :=
                    Is_Case_Item_Matches
                    or else (Value = Case_Values.First_Element);
               end Handle_String;

            begin
               case Kind (Node) is
                  when GPR_String_Literal =>
                     Handle_String (Node.As_String_Literal);

                  when GPR_Others_Designator =>
                     Is_Case_Item_Matches := True;

                  when others =>
                     null;
               end case;

               return Status;
            end Parser;

            Choices_Node : constant Choices := F_Choice (Node);

         begin
            Traverse (Choices_Node, Parser'Access);
            Is_Open := Is_Case_Item_Matches;
         end Parse_Case_Item;

         ------------------------
         -- Parse_Package_Decl --
         ------------------------

         procedure Parse_Package_Decl (Node : Package_Decl) is
            Name   : constant Identifier := F_Pkg_Name (Node);
            P_Name : constant Name_Type :=
                       Get_Name_Type (Name.As_Single_Tok_Node);
         begin
            --  First clear the package attributes container or restore the
            --  previous values (stage 2) if it exists.

            if Packs.Contains (P_Name) then
               Pack_Attrs := Packs.Element (P_Name).Attributes;
               Pack_Vars := Packs.Element (P_Name).Variables;
            else
               Pack_Attrs.Clear;
               Pack_Vars.Clear;
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
               GPR2.Project.Pack.Create
                 (Source_Reference.Identifier.Object
                    (Source_Reference.Identifier.Create
                       (Get_Source_Reference (Self.File, Node), P_Name)),
                  Pack_Attrs, Pack_Vars));

            --  Skip all nodes for this construct

            Status := Over;
         end Parse_Package_Decl;

         -----------------------------
         -- Parse_Package_Extension --
         -----------------------------

         procedure Parse_Package_Extension (Node : Package_Extension) is
            Sloc    : constant Source_Reference.Object :=
                        Get_Source_Reference (Self.File, Node);
            Prj     : constant Identifier := F_Prj_Name (Node);
            Project : constant Name_Type :=
                        Get_Name_Type (Single_Tok_Node (Prj));
            Name    : constant Identifier := F_Pkg_Name (Node);
            P_Name  : constant Name_Type :=
                        Get_Name_Type (Single_Tok_Node (Name));

            View    : constant GPR2.Project.View.Object :=
                        Process.View.View_For (Project);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Attrs.Clear;
            Pack_Vars.Clear;

            --  Check if the Project.Package reference exists

            if Is_Limited_Import (Self, Project) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "cannot have a reference to a limited project"));

            elsif not View.Is_Defined then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "project '" & String (Project) & "' is undefined"));

            elsif not View.Packages.Contains (P_Name) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "package '" & String (Project) & '.' & String (P_Name)
                       & "' is undefined"));

            else
               --  Then just copy the attributes into the current package

               Pack_Attrs := View.Packages.Element (P_Name).Attributes;
               Pack_Vars := View.Packages.Element (P_Name).Variables;
            end if;

            Status := Over;
         end Parse_Package_Extension;

         ----------------------------
         -- Parse_Package_Renaming --
         ----------------------------

         procedure Parse_Package_Renaming (Node : Package_Renaming) is
            Sloc    : constant Source_Reference.Object :=
                        Get_Source_Reference (Self.File, Node);
            Prj     : constant Identifier := F_Prj_Name (Node);
            Project : constant Name_Type :=
                        Get_Name_Type (Single_Tok_Node (Prj));
            Name    : constant Identifier := F_Pkg_Name (Node);
            P_Name  : constant Name_Type :=
                        Get_Name_Type (Single_Tok_Node (Name));

            View    : constant GPR2.Project.View.Object :=
                        Process.View.View_For (Project);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Attrs.Clear;
            Pack_Vars.Clear;

            --  Check if the Project.Package reference exists

            if Is_Limited_Import (Self, Project) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "cannot have a reference to a limited project"));

            elsif not View.Is_Defined then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "project '" & String (Project) & "' is undefined"));

            elsif not View.Packages.Contains (P_Name) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "package '" & String (Project) & '.' & String (P_Name)
                       & "' is undefined"));

            else
               --  Then just copy the attributes into the current package

               Pack_Attrs := View.Packages.Element (P_Name).Attributes;
               Pack_Vars := View.Packages.Element (P_Name).Variables;
            end if;

            Status := Over;
         end Parse_Package_Renaming;

         -------------------------
         -- Parse_Variable_Decl --
         -------------------------

         procedure Parse_Variable_Decl (Node : Variable_Decl) is

            function Search_Paths return GPR2.Path_Name.Set.Object is
              (GPR2.Project.Search_Paths
                 (Self.File, Tree.Project_Search_Paths));

            function Sloc return Source_Reference.Object is
              (Get_Source_Reference (Self.File, Node));
            --  Use function instead of constant because Sloc need only in case
            --  of error logging and no more than once.

            Name     : constant Identifier := F_Var_Name (Node);
            Expr     : constant Term_List := F_Expr (Node);
            Values   : constant Item_Values := Get_Term_List (Expr);
            V_Type   : constant Type_Reference := F_Var_Type (Node);
            V        : GPR2.Project.Variable.Object;
            Type_Def : GPR2.Project.Typ.Object;
         begin
            if not V_Type.Is_Null then
               declare
                  Type_N1  : constant Identifier :=
                               F_Var_Type_Name1 (V_Type);
                  Type_N2  : constant Identifier :=
                               F_Var_Type_Name2 (V_Type);
                  T_Name   : constant Name_Type :=
                               Get_Name_Type
                                 (if Type_N2.Is_Null
                                  then Single_Tok_Node (Type_N1)
                                  else Single_Tok_Node (Type_N2));
               begin
                  if not Type_N2.Is_Null then
                     --  We have a project prefix for the type name
                     declare
                        Project : constant Name_Type :=
                                    Get_Name_Type (Single_Tok_Node (Type_N1));
                     begin
                        if Self.Imports.Contains (Project) then
                           declare
                              Import : constant GPR2.Project.Import.Object :=
                                         Self.Imports.Element (Project);
                              Path   : constant GPR2.Path_Name.Object :=
                                         GPR2.Project.Create
                                           (Import.Path_Name.Name,
                                            Search_Paths);
                              Prj    : constant GPR2.Parser.Project.Object :=
                                         Registry.Get (Path);
                           begin
                              if Prj.Types.Contains (T_Name) then
                                 Type_Def := Prj.Types (T_Name);
                              end if;
                           end;
                        end if;
                     end;
                  end if;

                  if not Type_Def.Is_Defined
                    or else Type_Def.Count_Values = 0
                  then
                     if Self.Types.Contains (T_Name) then
                        Type_Def := Self.Types (T_Name);

                     elsif Self.Has_Extended then
                        declare
                           Path     : constant GPR2.Path_Name.Object :=
                                        GPR2.Project.Create
                                          (Self.Extended.Path_Name.Name,
                                           Search_Paths);
                           Extended : constant GPR2.Parser.Project.Object :=
                                        Registry.Get (Path);
                        begin
                           if Extended.Types.Contains (T_Name) then
                              Type_Def := Extended.Types (T_Name);
                           end if;
                        end;
                     end if;
                  end if;

                  --  Check that the type has been defined

                  if Type_Def.Is_Defined
                    and then Type_Def.Count_Values /= 0
                  then
                     --  Check that we have a single value

                     if Values.Single then
                        --  Check that the value is part of the type

                        declare
                           Value : constant Value_Type :=
                                     Values.Values.First_Element.Text;
                        begin
                           if Value /= ""
                             and then
                               not To_Set (Type_Def.Values).Contains (Value)
                           then
                              Tree.Log_Messages.Append
                                (Message.Create
                                   (Level   => Message.Error,
                                    Sloc    => Sloc,
                                    Message => "value '" & Value
                                    & "' is illegal for typed string '"
                                    & Get_Value_Type (Single_Tok_Node (Name))
                                    & '''));
                           end if;
                        end;

                     else
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Level   => Message.Error,
                              Sloc    => Sloc,
                              Message =>
                                "expression for '"
                                & Get_Value_Type (Single_Tok_Node (Name))
                                & "' must be a single string"));
                     end if;

                  else
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level   => Message.Error,
                           Sloc    => Get_Source_Reference (Self.File, V_Type),
                           Message =>
                             "unknown string type '" & String (T_Name) & '''));
                  end if;
               end;
            end if;

            if Values.Single then
               V := GPR2.Project.Variable.Create
                 (Name  =>
                    Get_Identifier_Reference
                      (Self.File,
                       Sloc_Range (Name),
                       Get_Name_Type (Single_Tok_Node (Name))),
                  Value => Values.Values.First_Element,
                  Typ   => Type_Def);
            else
               V := GPR2.Project.Variable.Create
                 (Name   =>
                    Get_Identifier_Reference
                      (Self.File,
                       Sloc_Range (Name),
                       Get_Name_Type (Single_Tok_Node (Name))),
                  Values => Values.Values,
                  Typ    => Type_Def);
            end if;

            if In_Pack then
               Pack_Vars.Include (V.Name.Text, V);
            else
               Vars.Include (V.Name.Text, V);
            end if;
         end Parse_Variable_Decl;

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

            case Kind (Node) is
               when GPR_Attribute_Decl =>
                  Parse_Attribute_Decl (Node.As_Attribute_Decl);

               when GPR_Variable_Decl =>
                  Parse_Variable_Decl (Node.As_Variable_Decl);

               when GPR_Package_Decl =>
                  Parse_Package_Decl (Node.As_Package_Decl);

               when GPR_Package_Renaming =>
                  Parse_Package_Renaming (Node.As_Package_Renaming);

               when GPR_Package_Extension =>
                  Parse_Package_Extension (Node.As_Package_Extension);

               when GPR_Case_Construction =>
                  Parse_Case_Construction (Node.As_Case_Construction);

               when GPR_Case_Item =>
                  Parse_Case_Item (Node.As_Case_Item);

               when others =>
                  null;
            end case;

         else
            --  We are on a closed parsing mode, only handle case alternatives

            case Kind (Node) is
               when GPR_Case_Item =>
                  Parse_Case_Item (Node.As_Case_Item);

               when others =>
                  null;
            end case;
         end if;

         if Has_Error then
            Status := Stop;
         end if;

         return Status;
      end Parser;

      ----------------------
      -- Record_Attribute --
      ----------------------

      procedure Record_Attribute
        (Set : in out GPR2.Project.Attribute.Set.Object;
         A   : GPR2.Project.Attribute.Object) is
      begin
         if A.Is_Defined then
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

      ------------
      -- To_Set --
      ------------

      function To_Set
        (Values : Containers.Source_Value_List) return Containers.Value_Set is
      begin
         return Set : Containers.Value_Set do
            for V of Values loop
               Set.Insert (V.Text);
            end loop;
         end return;
      end To_Set;

   begin
      Attrs.Clear;
      Vars.Clear;
      Packs.Clear;

      --  Insert intrinsic attributes Name and Project_Dir

      declare
         use Characters.Handling;
         Sloc : constant Source_Reference.Object :=
                  Source_Reference.Object
                    (Source_Reference.Create (Self.File.Value, 0, 0));

         function Create_Name
           (Name : Name_Type) return Source_Reference.Identifier.Object
         is
           (Source_Reference.Identifier.Object
              (Source_Reference.Identifier.Create (Sloc, Name)));

      begin
         Attrs.Insert
           (GPR2.Project.Attribute.Create
              (Name    => Create_Name (PRA.Name),
               Value   => Get_Value_Reference
                            (To_Lower (To_String (Self.Name)), Sloc),
               Default => True));

         Attrs.Insert
           (GPR2.Project.Attribute.Create
              (Name    => Create_Name (PRA.Project_Dir),
               Value   => Get_Value_Reference (Self.File.Dir_Name, Sloc),
               Default => True));
      end;

      Types := Self.Types;

      --  Re-Analyze the project given the new definitions (variables or
      --  attributes).

      loop
         Traverse (Root (Self.Unit), Parser'Access);
         exit when Stop_Iteration;
      end loop;
   end Process;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Self.Qualifier;
   end Qualifier;

   ----------
   -- Unit --
   ----------

   function Unit (Self : Object) return Analysis_Unit is
   begin
      return Self.Unit;
   end Unit;

end GPR2.Parser.Project;
