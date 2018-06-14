------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;

with GPR2.Builtin;
with GPR2.Message;
with GPR2.Parser.Registry;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Variable;
with GPR2.Source_Reference;

package body GPR2.Parser.Project is

   use Ada;
   use type Ada.Containers.Count_Type;

   --  Some helpers routines for the parser

   function Get_Name_Type
     (Node : Single_Tok_Node'Class) return Name_Type;
   --  Returns the Name for the given node

   function Present (Node : GPR_Node'Class) return Boolean is
     (not Node.Is_Null);
   --  Returns True if the Node is present (not null)

   function Get_Source_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Slr       : Langkit_Support.Slocs.Source_Location_Range)
      return Source_Reference.Object is
     (Source_Reference.Object
        (Source_Reference.Create
           (Path_Name.Value,
            Positive (Slr.Start_Line),
            Positive (Slr.Start_Column))));

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

   -------------------
   -- Get_Name_Type --
   -------------------

   function Get_Name_Type
     (Node : Single_Tok_Node'Class) return Name_Type
   is
      use Ada.Characters.Conversions;
      V      : constant Wide_Wide_String := Text (Node);
      Offset : Natural := 0;
   begin
      if V (V'First) = '"' and then V (V'Last) = '"' then
         Offset := 1;
      end if;
      return Name_Type (To_String (V (V'First + Offset .. V'Last - Offset)));
   end Get_Name_Type;

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
              (Unquote (Value_Type (String'(Text (Node)))));
         end Handle_String;

      begin
         case Kind (Node) is
            when GPR_String_Literal =>
               Handle_String (Node.As_String_Literal);

            when GPR_Term_List | GPR_String_Literal_At | GPR_Base_List =>
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

   ------------------
   -- Has_Extended --
   ------------------

   function Has_Extended (Self : Object) return Boolean is
      use type GPR2.Project.Import.Object;
   begin
      return Self.Extended /= GPR2.Project.Import.Undefined;
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

   ---------------------
   -- Is_Extended_All --
   ---------------------

   function Is_Extended_All (Self : Object) return Boolean is
   begin
      return Self.Is_All;
   end Is_Extended_All;

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
     (Filename : GPR2.Path_Name.Object;
      Messages : out Log.Object) return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      function Parse_Stage_1 (Unit : Analysis_Unit) return Object;
      --  Analyse project, record all externals variables and imports

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
                           Sloc    =>
                             Get_Source_Reference
                               (Filename, Sloc_Range (N)),
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
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Var_Node)),
                                 Message =>
                                   "external_as_list first parameter must be "
                                 & "a simple string"));

                        elsif Var = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Var_Node)),
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
                                Get_Source_Reference
                                  (Filename, Sloc_Range (Exprs)),
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
                                        (Filename, Sloc_Range (Sep_Node)),
                                    Message =>
                                      "external_as_list second parameter must "
                                    & "be a simple string"));

                           elsif Sep = "" then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sloc_Range (Sep_Node)),
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
                           Sloc    =>
                             Get_Source_Reference
                               (Filename, Sloc_Range (N)),
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
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Var_Node)),
                                 Message =>
                                   "external first parameter must be a "
                                 & "simple string"));

                        elsif Var = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Var_Node)),
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
                           Sloc    =>
                             Get_Source_Reference
                               (Filename, Sloc_Range (N)),
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
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Str_Node)),
                                 Message =>
                                   "split first parameter must be "
                                 & "a simple string"));

                        elsif Str = "" then
                           Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference
                                     (Filename, Sloc_Range (Str_Node)),
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
                                Get_Source_Reference
                                  (Filename, Sloc_Range (Exprs)),
                              Message =>
                                "split requires a second parameter"));
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
                                        (Filename, Sloc_Range (Sep_Node)),
                                    Message =>
                                      "split separator parameter must "
                                    & "be a simple string"));

                           elsif Sep = "" then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sloc_Range (Sep_Node)),
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
               end if;
            end Parse_Builtin;

            -------------------------------
            -- Parse_Project_Declaration --
            -------------------------------

            procedure Parse_Project_Declaration (N : Project_Declaration) is
               Name : constant Expr := F_Project_Name (N);
               Qual : constant Project_Qualifier := F_Qualifier (N);
               Ext  : constant Project_Extension := F_Extension (N);
            begin
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
                  declare
                     Path_Name : constant GPR2.Path_Name.Object :=
                                   GPR2.Project.Create
                                     (Get_Name_Type
                                        (F_Path_Name (Ext)));
                  begin
                     Project.Extended :=
                       GPR2.Project.Import.Create
                         (Path_Name,
                          Get_Source_Reference
                            (Filename, Sloc_Range (Ext)),
                          Is_Limited => False);
                     Project.Is_All := F_Is_All (Ext);
                  end;
               end if;

               Project.Name :=
                 To_Unbounded_String
                   (String (Get_Name_Type (Name.As_Single_Tok_Node)));
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
            begin
               if Project.Types.Contains (Name) then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference
                            (Filename, Sloc_Range (F_Type_Id (N))),
                        Message =>
                          "type " & String (Name) & " already defined"));

               else
                  for J in 1 .. Num_Childs loop
                     Cur_Child := Child (GPR_Node (Values), J);

                     if not Cur_Child.Is_Null then
                        declare
                           Value : constant Value_Type :=
                                     Value_Type
                                       (Get_Name_Type
                                          (Cur_Child.As_String_Literal));
                        begin
                           if Set.Contains (Value) then
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference
                                        (Filename, Sloc_Range (Cur_Child)),
                                    Message =>
                                      String (Name)
                                    & " has duplicate value '"
                                    & String (Value) & '''));
                           else
                              Set.Insert (Value);
                           end if;
                        end;
                     end if;
                  end loop;

                  Project.Types.Insert (Name, Set);
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
                                 GPR2.Project.Create
                                   (Get_Name_Type
                                      (Cur_Child.As_String_Literal));
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
                                         (Filename,
                                          Sloc_Range (Cur_Child))));

                              else
                                 Messages.Append
                                   (GPR2.Message.Create
                                      (Level   => Message.Warning,
                                       Message => "duplicate project name '"
                                       & String (Path.Base_Name) & ''',
                                       Sloc    => Get_Source_Reference
                                         (Filename,
                                          Sloc_Range (Cur_Child))));
                                 Messages.Append
                                   (GPR2.Message.Create
                                      (Level   => Message.Warning,
                                       Message => "already in '"
                                       & String (Prev.Path_Name.Name)
                                       & ''',
                                       Sloc    => Get_Source_Reference
                                         (Filename,
                                          Sloc_Range (Cur_Child))));
                              end if;
                           end;

                        else
                           Project.Imports.Insert
                             (GPR2.Project.Import.Create
                                (Path,
                                 Get_Source_Reference
                                   (Filename, Sloc_Range (Cur_Child)),
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

         return Project;
      end Parse_Stage_1;

      Context : constant Analysis_Context := Create ("UTF-8");
      Unit    : Analysis_Unit;
      Project : Object;

   begin
      if Registry.Exists (Filename) then
         declare
            P : constant Parser.Project.Object := Registry.Get (Filename);
         begin
            Registry.Register (Filename, P);
            Inc_Ref (P.Unit);
            Inc_Ref (P.Context);
            return P;
         end;

      else
         if not Directories.Exists (Filename.Value) then
            Messages.Append
              (GPR2.Message.Create
                 (Level   => Message.Error,
                  Message => "project file not found",
                  Sloc    => Source_Reference.Object
                               (Source_Reference.Create
                                 (Filename.Value, 1, 1))));
            return Undefined;
         end if;

         Unit := Get_From_File (Context, Filename.Value);

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

         Inc_Ref (Project.Unit);
         Inc_Ref (Project.Context);

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
      Packs   : in out GPR2.Project.Pack.Set.Object)
   is

      type Item_Values is record
         Values : Containers.Value_List;
         Single : Boolean := False;
      end record
        with Dynamic_Predicate =>
          (if Item_Values.Single then Item_Values.Values.Length = 1);

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
        (Tree.Log_Messages.Has_Element
           (Error       => True,
            Information => False,
            Warning     => False,
            Read        => False,
            Unread      => True));

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
      --
      --  Att_Name and Idx_Name are the name for the currently parsed attribute
      --  declaration. This is used to check for recursive definition.
      --     for Att (Idx) use Att (Idx) & ("some", "other", "value");

      In_Pack              : Boolean := False;
      Pack_Name            : Unbounded_String;
      Att_Name             : Unbounded_String;
      Idx_Name             : Unbounded_String;
      Pack_Attrs           : GPR2.Project.Attribute.Set.Object;
      Pack_Vars            : GPR2.Project.Variable.Set.Object;
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
         Node    : Attribute_Reference;
         Pack    : Optional_Name_Type := "") return Item_Values
      is
         use type GPR2.Project.Attribute.Object;
         use type GPR2.Project.Registry.Attribute.Value_Kind;
         use type GPR2.Project.View.Object;

         Name   : constant Name_Type :=
                    Get_Name_Type
                      (Single_Tok_Node (F_Attribute_Name (Node)));
         I_Node : constant GPR_Node := F_Attribute_Index (Node);
         Index  : constant Value_Type :=
                    (if Present (I_Node)
                     then Value_Type
                       (Get_Name_Type (I_Node.As_Single_Tok_Node))
                     else "");
         View   : constant GPR2.Project.View.Object :=
                    Process.View.View_For (Project);

         function Is_Self return Boolean is
           (Optional_Name_Type (To_String (Pack_Name)) = Pack
            and then Name_Type (To_String (Att_Name)) = Name
            and then Value_Type (To_String (Idx_Name)) = Index);
         --  Returns True if the current reference corresponds to the currently
         --  defined attribute (self/recursive reference).

         Attr   : GPR2.Project.Attribute.Object :=
                    GPR2.Project.Attribute.Undefined;
         Result : Item_Values;

      begin
         --  We do not want to have a reference to a limited import, we do not
         --  check when a special project reference is found Project'Name or
         --  Config'Name.

         if Project /= "project"
           and then Project /= "config"
           and then Project /= "runtime"
           and then Is_Limited_Import (Self, Project)
         then
            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  "cannot have a reference to a limited project",
                  Get_Source_Reference (Self.File, Sloc_Range (Node))));

            return Empty_Item_Values;
         end if;

         --  For a project/attribute reference we need to check the attribute
         --  definition to know wether the result is multi-valued or not.

         declare
            use GPR2.Project.Registry;

            Attrib : constant Attribute.Qualified_Name :=
                       Attribute.Create (Name, Pack);
         begin
            if Attribute.Exists (Attrib) then
               Result.Single :=
                 Attribute.Get (Attrib).Value = Attribute.Single;

            else
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute '" & String (Name) & "' is not defined",
                     Get_Source_Reference
                       (Self.File,
                        Sloc_Range (Node))));

               return Empty_Item_Values;
            end if;
         end;

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
               if Attrs.Contains (Name, Index) then
                  Attr := Attrs.Element (Name, Index);

               --  Special cases for some built-in references

               elsif Index = "" then
                  if Name = GPR2.Project.Registry.Attribute.Target then
                     --  Project'Target

                     return R : Item_Values do
                        R.Single := True;
                        R.Values.Append
                          (Characters.Handling.To_Lower
                             (String (Tree.Target)));
                     end return;

                  elsif Name = GPR2.Project.Registry.Attribute.Project_Dir then
                     --  Project'Project_Dir

                     return R : Item_Values do
                        R.Single := True;
                        R.Values.Append
                          (Characters.Handling.To_Lower
                             (String (Self.File.Name)));
                     end return;

                  elsif Name = GPR2.Project.Registry.Attribute.Name then
                     --  Project'Name

                     return R : Item_Values do
                        R.Single := True;
                        R.Values.Append
                          (Characters.Handling.To_Lower
                             (To_String (Self.Name)));
                     end return;
                  end if;

               elsif Index /= ""
                 and then Name = GPR2.Project.Registry.Attribute.Runtime
               then
                  --  Project'Runtime ("<lang>")

                  return R : Item_Values do
                     R.Single := True;
                     R.Values.Append
                       (Characters.Handling.To_Lower
                          (String (Tree.Runtime
                            (Optional_Name_Type (Index)))));
                  end return;

               elsif Is_Self then
                  null;

               else
                  Att_Defined := False;
               end if;

            elsif Pack_Name /= Null_Unbounded_String
              and then Name_Type (To_String (Pack_Name)) = Name_Type (Pack)
            then
               --  This is the current parsed package, look into Pack_Attrs

               if Pack_Attrs.Contains (Name, Index) then
                  Attr := Pack_Attrs.Element (Name, Index);

               elsif Is_Self then
                  --  A self reference, if the attribute is not found (check
                  --  above) it means that there is no previous definition
                  --  for this attribute. The current value is then the empty
                  --  string but the attribute is defined.
                  null;

               else
                  Att_Defined := False;
               end if;

            elsif Packs.Contains (Name_Type (Pack)) then
               --  Or in another package in the same project
               if Packs (Name_Type (Pack)).Has_Attributes (Name, Index) then
                  Attr := Packs.Element
                    (Name_Type (Pack)).Attributes.Element (Name, Index);
               else
                  Att_Defined := False;
               end if;
            end if;

         else
            if View /= GPR2.Project.View.Undefined then
               if Pack = "" then
                  if View.Has_Attributes (Name, Index) then
                     Attr := View.Attributes.Element (Name, Index);
                  else
                     Att_Defined := False;
                  end if;

               else
                  if View.Has_Packages (Pack) then
                     declare
                        P : constant GPR2.Project.Pack.Object :=
                              View.Packages.Element (Name_Type (Pack));
                     begin
                        if P.Has_Attributes (Name, Index) then
                           Attr := P.Attributes.Element (Name, Index);

                        else
                           Att_Defined := False;
                        end if;
                     end;

                  else
                     Att_Defined := False;
                  end if;
               end if;
            end if;
         end if;

         if Attr = GPR2.Project.Attribute.Undefined then
            Result.Single := False;

         else
            Result :=
              (Attr.Values,
               Attr.Kind = GPR2.Project.Registry.Attribute.Single);
         end if;

         return Result;
      end Get_Attribute_Ref;

      -------------------
      -- Get_Term_List --
      -------------------

      function Get_Term_List (Node : Term_List) return Item_Values is

         Result : Item_Values;
         --  The list of values returned by Get_Term_List

         New_Item : Boolean := True;

         function Parser (Node : GPR_Node'Class) return Visit_Status;

         procedure Record_Value (Value : Value_Type)
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
              with Pre  => Present (Node);
            --  A simple static string

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
                     Record_Value (V);
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
                                (Context, Var, Values.Values.First_Element));

                        else
                           Tree.Log_Messages.Append
                             (GPR2.Message.Create
                                (Level   => Message.Error,
                                 Sloc    =>
                                   Get_Source_Reference
                                     (Self.File, Sloc_Range (Parameters)),
                                 Message =>
                                   "external default parameter must be a "
                                 & "simple string"));
                        end if;
                     end;

                  else
                     Record_Value (Builtin.External (Context, Var));
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
                              Get_Source_Reference
                                (Self.File, Sloc_Range (Parameters)),
                           Message => Exception_Message (E)));
                     Record_Value ("");
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
                     Record_Value (V);
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
                 (Unquote (Value_Type (String'(Text (Node)))));
            end Handle_String;

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

               when GPR_GPR_Node_List =>
                  --  A new value is found
                  New_Item := True;

               when GPR_String_Literal =>
                  Handle_String (Node.As_String_Literal);

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

         procedure Record_Value (Value : Value_Type) is
         begin
            if New_Item then
               Result.Values.Append (Value);
               New_Item := False;

            else
               declare
                  Last      : constant Containers.Extended_Index :=
                                Result.Values.Last_Index;
                  New_Value : constant Value_Type :=
                                Result.Values (Last) & Value;
               begin
                  Result.Values.Replace_Element (Last, New_Value);
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
         use type GPR2.Project.Registry.Attribute.Value_Kind;
         use type GPR2.Project.View.Object;

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
                  return (V.Values,
                          V.Kind = GPR2.Project.Registry.Attribute.Single);
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
         if View = GPR2.Project.View.Undefined then
            --  Some maybe Project is actually a local package

            declare
               P_Name : Name_Type renames Project;
            begin
               if Packs.Contains (P_Name) then
                  Result := Get_Pack_Var (Packs.Element (P_Name), Name);

               elsif Project /= "config" and then Project /= "runtime" then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "project " & String (Project) & " is undefined",
                        Get_Source_Reference (Self.File, Sloc_Range (Node))));
               end if;
            end;

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
                  Result :=
                    (V.Values,
                     V.Kind = GPR2.Project.Registry.Attribute.Single);
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
         use type GPR2.Project.Import.Object;
         use type GPR2.Project.Registry.Attribute.Value_Kind;

         Sloc    : constant Source_Reference.Object :=
                     Get_Source_Reference
                       (Self.File,
                        Sloc_Range (GPR_Node (Node)));

         Name_1  : constant Identifier := F_Variable_Name1 (Node);
         Name_2  : constant Identifier := F_Variable_Name2 (Node);
         Name_3  : constant Identifier := F_Variable_Name3 (Node);
         Att_Ref : constant Attribute_Reference := F_Attribute_Ref (Node);
         Name    : constant Name_Type :=
                     Name_Type (String'(Text (Name_1)));
      begin
         if Present (Att_Ref) then
            if Present (Name_2) then
               --  This is a project/package reference:
               --    <project>.<package>'<attribute>
               return Get_Attribute_Ref
                 (Project => Name,
                  Pack    => Optional_Name_Type
                               (String'(Text (Name_2))),
                  Node    => Att_Ref);

            else
               --  If a single name it can be either a project or a package

               if Self.Imports.Contains (Name)
                 or else (Self.Extended /= GPR2.Project.Import.Undefined
                            and then
                          Optional_Name_Type
                            (Self.Extended.Path_Name.Base_Name) = Name)
                 or else Name = "config"
                 or else Name = "runtime"
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
               if Self.Extended /= GPR2.Project.Import.Undefined then
                  declare
                     use type GPR2.Project.View.Object;

                     View : constant GPR2.Project.View.Object :=
                              Process.View.View_For
                                (Self.Extended.Path_Name.Base_Name);
                  begin
                     if View /= GPR2.Project.View.Undefined
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
                        Sloc    => Sloc,
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
            Sloc  : constant Source_Reference.Object :=
                      Get_Source_Reference
                        (Self.File,
                         Sloc_Range (GPR_Node (Node)));
            Name  : constant Identifier := F_Attr_Name (Node);
            Index : constant GPR_Node := F_Attr_Index (Node);
            I_Str : constant Value_Type :=
                      (if Present (Index)
                       then
                          (if Kind (Index) = GPR_Others_Designator
                           then "others"
                           else Value_Type (Get_Name_Type
                             (Index.As_String_Literal_At.F_Str_Lit)))
                       else "");
            Expr  : constant Term_List := F_Expr (Node);
            N_Str : constant Name_Type :=
                      Get_Name_Type (Name.As_Single_Tok_Node);
         begin
            Att_Name := To_Unbounded_String (String (N_Str));
            Idx_Name := To_Unbounded_String (String (I_Str));

            declare
               Values : constant Item_Values := Get_Term_List (Expr);
               A      : GPR2.Project.Attribute.Object;
            begin
               if Present (Index) then
                  if Values.Single then
                     A := GPR2.Project.Attribute.Create
                       (Name  => N_Str,
                        Index => I_Str,
                        Value => Values.Values.First_Element,
                        Sloc  => Sloc);

                  else
                     A := GPR2.Project.Attribute.Create
                       (Name   => N_Str,
                        Index  => I_Str,
                        Values => Values.Values,
                        Sloc   => Sloc);
                  end if;

               else
                  if Values.Single then
                     A := GPR2.Project.Attribute.Create
                       (Name  => N_Str,
                        Value => Values.Values.First_Element,
                        Sloc  => Sloc);
                  else
                     A := GPR2.Project.Attribute.Create
                       (Name   => N_Str,
                        Values => Values.Values,
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

            Att_Name := Null_Unbounded_String;
            Idx_Name := Null_Unbounded_String;
         end Parse_Attribute_Decl;

         -----------------------------
         -- Parse_Case_Construction --
         -----------------------------

         procedure Parse_Case_Construction (Node : Case_Construction) is
            Var   : constant Variable_Reference := F_Var_Ref (Node);
            Value : constant Containers.Value_List :=
                      Get_Variable_Values (Var).Values;
         begin
            if Value.Length = 1 then
               Case_Values.Prepend (Value.First_Element);

               --  Set status to close for now, this will be open when a
               --  when_clause will match the value pushed just above on
               --  the vector.

               Is_Open := False;

               declare
                  Childs : constant Case_Item_List := F_Items (Node);
               begin
                  for C in 1 .. Children_Count (Childs) loop
                     Visit_Child (Child (GPR_Node (Childs), C));
                  end loop;
               end;

               --  Then remove the case value

               Case_Values.Delete_First;

               --  Skip all nodes for this construct

               Status := Over;

               Is_Open := True;

            elsif Has_Error then
               null;

            else
               declare
                  Sloc  : constant Source_Reference.Object :=
                            Get_Source_Reference
                              (Self.File,
                               Sloc_Range (GPR_Node (Node)));
                  Name  : constant Identifier :=
                            F_Variable_Name1 (Var);
               begin
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Sloc,
                        Message =>
                          "variable '"
                          & String (Get_Name_Type (Single_Tok_Node (Name)))
                          & "' must be a simple value"));
               end;
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
                  Value : constant Value_Type := Unquote (Text (Node));
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

            Choices : constant GPR_Node_List := F_Choice (Node);

         begin
            Traverse (GPR_Node (Choices), Parser'Access);
            Is_Open := Is_Case_Item_Matches;
         end Parse_Case_Item;

         ------------------------
         -- Parse_Package_Decl --
         ------------------------

         procedure Parse_Package_Decl (Node : Package_Decl) is
            Sloc   : constant Source_Reference.Object :=
                       Get_Source_Reference
                         (Self.File, Sloc_Range (Node.As_GPR_Node));
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
               GPR2.Project.Pack.Create (P_Name, Pack_Attrs, Pack_Vars, Sloc));

            --  Skip all nodes for this construct

            Status := Over;
         end Parse_Package_Decl;

         -----------------------------
         -- Parse_Package_Extension --
         -----------------------------

         procedure Parse_Package_Extension (Node : Package_Extension) is
            use type GPR2.Project.View.Object;

            Sloc    : constant Source_Reference.Object :=
                        Get_Source_Reference
                          (Self.File, Sloc_Range (GPR_Node (Node)));
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

            if View = GPR2.Project.View.Undefined then
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
            use type GPR2.Project.View.Object;

            Sloc    : constant Source_Reference.Object :=
                        Get_Source_Reference
                          (Self.File, Sloc_Range (GPR_Node (Node)));
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

            if View = GPR2.Project.View.Undefined then
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

            Sloc    : constant Source_Reference.Object :=
                        Get_Source_Reference
                          (Self.File, Sloc_Range (GPR_Node (Node)));
            Name    : constant Identifier := F_Var_Name (Node);
            Expr    : constant Term_List := F_Expr (Node);
            Values  : constant Item_Values := Get_Term_List (Expr);
            V_Type  : constant Type_Reference := F_Var_Type (Node);
            V       : GPR2.Project.Variable.Object;
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
                  Type_Def : Containers.Value_Set;
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
                                           (Name_Type (Import.Path_Name.Value),
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

                  if Type_Def.Is_Empty then
                     if Self.Types.Contains (T_Name) then
                        Type_Def := Self.Types (T_Name);

                     elsif Self.Has_Extended then
                        declare
                           Path     : constant GPR2.Path_Name.Object :=
                                        GPR2.Project.Create
                                          (Name_Type
                                             (Self.Extended.Path_Name.Value),
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

                  if not Type_Def.Is_Empty then
                     --  Check that we have a single value

                     if Values.Single then
                        --  Check that the value is part of the type

                        if not Type_Def.Contains
                          (Values.Values.First_Element)
                        then
                           Tree.Log_Messages.Append
                             (Message.Create
                                (Level   => Message.Error,
                                 Sloc    => Sloc,
                                 Message =>
                                   "value '"
                                   & String (Values.Values.First_Element)
                                   & "' is illegal for typed string '"
                                   & String
                                       (Get_Name_Type
                                         (Single_Tok_Node (Name))) & '''));
                        end if;

                     else
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Level   => Message.Error,
                              Sloc    => Sloc,
                              Message =>
                                "expression for '"
                                & String
                                    (Get_Name_Type (Single_Tok_Node (Name)))
                                & "' must be a single string"));
                     end if;

                  else
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level   => Message.Error,
                           Sloc    =>
                             Get_Source_Reference
                               (Self.File, Sloc_Range (GPR_Node (V_Type))),
                           Message =>
                             "unknown string type '" & String (T_Name) & '''));
                  end if;

               end;
            end if;

            if Values.Single then
               V := GPR2.Project.Variable.Create
                 (Name  => Get_Name_Type (Single_Tok_Node (Name)),
                  Value => Values.Values.First_Element,
                  Sloc  => Sloc);
            else
               V := GPR2.Project.Variable.Create
                 (Name   => Get_Name_Type (Single_Tok_Node (Name)),
                  Values => Values.Values,
                  Sloc   => Sloc);
            end if;

            if In_Pack then
               Pack_Vars.Include (V.Name, V);
            else
               Vars.Include (V.Name, V);
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
         Att_Defined := True;

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
   end Process;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Self.Qualifier;
   end Qualifier;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object) is
   begin
      Dec_Ref (Self.Unit);
      Dec_Ref (Self.Context);
   end Unload;

end GPR2.Parser.Project;
