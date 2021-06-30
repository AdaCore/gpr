------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with GPR2.Builtin;
with GPR2.Message;
with GPR2.Parser.Registry;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.Variable;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

with GPR_Parser.Common;

package body GPR2.Parser.Project is

   use GPR_Parser.Common;
   use Langkit_Support.Text;
   use type Ada.Containers.Count_Type;

   package PA renames GPR2.Project.Attribute;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;
   package PAI renames GPR2.Project.Attribute_Index;
   package ASU renames Ada.Strings.Unbounded;

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

   function Get_Name_Type
     (Node  : GPR_Node'Class;
      First : Positive := 1;
      Last  : Positive;
      Sep   : String := ".") return Name_Type
       with Pre => Last >= First and then Children_Count (Node) >= Last;
   --  Returns the Name for the given children of given node

   function Get_Filename
     (Node : Single_Tok_Node'Class) return Filename_Type
   is
     (Filename_Type (Get_Value_Type (Node)));
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
      At_Pos    : Natural := 0) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
        (Source_Reference.Value.Create
             (Get_Source_Reference (Path_Name, Slr), Value, At_Pos)));

   function Get_Value_Reference
     (Value        : Value_Type;
      Sloc         : Source_Reference.Object;
      At_Pos       : Natural := 0;
      From_Default : Boolean := False) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
        (Source_Reference.Value.Create (Sloc, Value, At_Pos, From_Default)));

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
        (GPR2.Project.Ensure_Extension (Get_Filename (Node)),
         GPR2.Path_Name.No_Resolution));
   --  Creates project Path_Name.Object not checked for location

   function Get_String_Literal
     (N     : GPR_Node'Class;
      Error : out Boolean) return Value_Type;
   --  Returns the first string literal found under this node. This is an
   --  helper routine to get strings out of built-in parameters for example.
   --  Set Error to True if the node was not a simple string-literal.

   function Parse_Stage_1
     (Unit          : Analysis_Unit;
      Filename      : GPR2.Path_Name.Object;
      Implicit_With : GPR2.Path_Name.Set.Object;
      Messages      : out Log.Object) return Object;
   --  Analyzes the project, recording all external references and imports

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
     (Node  : GPR_Node'Class;
      First : Positive := 1;
      Last  : Positive;
      Sep   : String := ".") return Name_Type
   is
      Name : ASU.Unbounded_String :=
               ASU.To_Unbounded_String
                 (Ada.Characters.Conversions.To_String
                    (Text (Child (Node, First))));
   begin
      for C in First + 1 .. Last loop
         ASU.Append (Name, Sep);
         ASU.Append
           (Name,
            Ada.Characters.Conversions.To_String (Text (Child (Node, C))));
      end loop;
      return Name_Type (ASU.To_String (Name));
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
     (Contents        : Ada.Strings.Unbounded.Unbounded_String;
      Messages        : out Log.Object;
      Pseudo_Filename : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined)
      return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      Filename : constant GPR2.Path_Name.Object :=
                   (if Pseudo_Filename.Is_Defined then Pseudo_Filename
                    else GPR2.Path_Name.Create_File
                      ("/string_input/default.gpr"));
      Context  : constant Analysis_Context := Create_Context ("UTF-8");
      Unit     : Analysis_Unit;
      Project  : Object;
   begin
      if Contents = Null_Unbounded_String then
         Messages.Append
           (GPR2.Message.Create
              (Level   => Message.Error,
               Message => "project string is empty",
               Sloc    => Source_Reference.Create (Filename.Value, 0, 0)));

         return Undefined;
      end if;

      Unit := Get_From_Buffer
        (Context, Filename.Value, Buffer => To_String (Contents));

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

      Project := Parse_Stage_1
        (Unit, Filename, GPR2.Path_Name.Set.Empty_Set, Messages);

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

      return Project;

   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename      : GPR2.Path_Name.Object;
      Implicit_With : GPR2.Path_Name.Set.Object;
      Messages      : in out Log.Object) return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

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

         Project := Parse_Stage_1 (Unit, Filename, Implicit_With, Messages);

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

         --  Finally register this project into the registry

         if not Messages.Has_Error then
            Registry.Register (Filename, Project);
         end if;

         return Project;
      end if;
   end Parse;

   -------------------
   -- Parse_Stage_1 --
   -------------------

   function Parse_Stage_1
     (Unit          : Analysis_Unit;
      Filename      : GPR2.Path_Name.Object;
      Implicit_With : GPR2.Path_Name.Set.Object;
      Messages      : out Log.Object) return Object
   is
      use type GPR2.Path_Name.Object;

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
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
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
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
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
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               --  Note that this routine is only validating the syntax
               --  of the split built-in.

               if Exprs.Is_Null then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message => "missing parameters for split built-in"));

               --  Check that the second parameter exists

               elsif Child (Exprs, 2).Is_Null then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, Exprs),
                        Message => "split requires a second parameter"));
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
               Project.Expl_Qual := True;
               Project.Qualifier :=
                 (case Kind (Qual) is
                     when GPR_Project_Qualifier_Abstract
                       => K_Abstract,
                     when GPR_Project_Qualifier_Standard
                       => K_Standard,
                     when GPR_Project_Qualifier_Library
                       => K_Library,
                     when GPR_Project_Qualifier_Aggregate
                       => K_Aggregate,
                     when GPR_Project_Qualifier_Aggregate_Library
                       => K_Aggregate_Library,
                     when GPR_Project_Qualifier_Configuration
                       => K_Configuration,
                     when others
                       => raise Program_Error with "Unreachable");
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
                     Path : constant GPR2.Path_Name.Object :=
                              Get_Raw_Path (Cur_Child.As_String_Literal);
                     CI : constant GPR2.Project.Import.Set.Cursor :=
                            Project.Imports.Find (Path);
                  begin
                     if GPR2.Project.Import.Set.Has_Element (CI) then
                        declare
                           Prev : constant GPR2.Project.Import.Object :=
                                    GPR2.Project.Import.Set.Element (CI);
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

      for PN of Implicit_With loop
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
      end loop;

      return Project;
   end Parse_Stage_1;

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
     (Self          : in out Object;
      Tree          : GPR2.Project.Tree.Object;
      Context       : GPR2.Context.Object;
      View          : GPR2.Project.View.Object;
      Attrs         : in out GPR2.Project.Attribute.Set.Object;
      Vars          : in out GPR2.Project.Variable.Set.Object;
      Packs         : in out GPR2.Project.Pack.Set.Object;
      Types         : in out GPR2.Project.Typ.Set.Object;
      Pre_Conf_Mode : Boolean := False)
   is

      type Indexed_Values is record
         Index  : GPR2.Project.Attribute_Index.Object;
         Values : Containers.Source_Value_List;
         Single : Boolean := False;
      end record
        with Dynamic_Predicate =>
          not PAI."=" (Indexed_Values.Index, PAI.Undefined)
            and then (if Indexed_Values.Single
                        then Indexed_Values.Values.Length = 1);

      package Indexed_Item_Values_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Indexed_Values);

      type Indexed_Item_Values is record
         Filled         : Boolean := False;
         Attribute_Pack : Unbounded_String;
         Attribute_Name : Unbounded_String;
         Values         : Indexed_Item_Values_Vectors.Vector;
      end record;

      type Item_Values is record
         Values         : Containers.Source_Value_List;
         Single         : Boolean := False;
         Indexed_Values : Indexed_Item_Values;
      end record
        with Dynamic_Predicate =>
          (if Item_Values.Single then Item_Values.Values.Length = 1);
      --  Indexed_Values is filled only in Get_Attribute_Ref when attribute
      --  allows index and index is not provided in the reference.

      function To_Set
        (Values : Containers.Source_Value_List) return Containers.Value_Set;
      --  Create a set for fast searchiing from a list of values

      Unfilled_Indexed_Values : constant Indexed_Item_Values :=
                                  (Filled         => False,
                                   Attribute_Pack => +"",
                                   Attribute_Name => +"",
                                   Values         =>
                                     Indexed_Item_Values_Vectors.Empty_Vector);

      Empty_Item_Values : constant Item_Values :=
                            (Single         => False,
                             Values         => <>,
                             Indexed_Values => Unfilled_Indexed_Values);

      function Missing_Project_Error_Level return Message.Level_Value is
        (if Pre_Conf_Mode then Message.Warning else Message.Error);
      --  Returns expected level for missing import messages

      function Ensure_Source_Loc
        (Values : Containers.Source_Value_List;
         Sloc   : Source_Reference.Object)
            return Containers.Source_Value_List;
      --  Ensure the values have the proper Source_Loc

      function Parser (Node : GPR_Node'Class) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values;
      --  Parse and return the value for the given variable reference

      function Get_Attribute_Index
        (Node : Attribute_Reference;
         Pack : Optional_Name_Type := No_Name) return PAI.Object;
      --  Get the attribute index, if any, or PAI.Undefined

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Optional_Name_Type := No_Name) return Item_Values;
      --  Return the value for an attribute reference in the given project and
      --  possibly the given package.

      function Get_Variable_Ref
        (Variable   : Name_Type;
         Source_Ref : Source_Reference.Object;
         Project    : Optional_Name_Type := No_Name;
         Pack       : Optional_Name_Type := No_Name;
         From_View  : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
        return Item_Values;
      --  Return the value for a variable reference in the given project
      --
      --  Variable:   the variable name to retrieve
      --  Source_Ref: the location of the variable reference
      --  Project:    the project name in which to look for the variable
      --              if No_Name it lookup is done in From_View
      --  Pack:       the package in which which to look for the variable. If
      --              No_Name it assumes the variable is declared at toplevel
      --  From_View:  the reference view from where to start the search. If
      --              set to Undefined search starts from the currently
      --              processed view.

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
        (Set  : in out PA.Set.Object;
         A    : PA.Object;
         Sloc : Source_Reference.Object);
      --  Record an attribute into the given set. At the same time we increment
      --  the Empty_Attribute_Count if this attribute has an empty value. This
      --  is used to check whether we need to reparse the tree.

      function Stop_Iteration return Boolean;
      --  Returns true if a new parsing of the tree is needed. This is because
      --  an attribute can have a forward reference to another attribute into
      --  the same package.

      function Has_Error return Boolean is
        (Tree.Log_Messages.Has_Error);

      function Is_Switches_Index_Case_Sensitive
        (Value : Value_Type) return Boolean;
      --  Check whether the switches index is case sensitive or not. This is
      --  needed as the Switches index can have language (non case-sensitive)
      --  and filename which can be case sensitive depending on the OS.

      --  Global variables used to keep state during the parsing. While
      --  visiting child nodes we may need to record status (when in a package
      --  or a case construct for example). This parsing state is then used
      --  to apply different check or recording.

      --  The parsing status for case statement (possibly nested)

      Actual : Containers.Filename_Set;
      --  Naming exception source filenames from active case alternatives

      Case_Values : Containers.Value_List;
      --  The case-values to match against the case-item. Each time a case
      --  statement is enterred the value for the case is prepended into this
      --  vector. The first value is then removed when exiting from the case
      --  statement. This is to support nested case statements.
      --  First character in each element mean is the case-item is open or
      --  closed. Other characters contain case value.

      function Is_Open return Boolean is
        (Case_Values.Is_Empty or else Case_Values.Last_Element (1) = '+');
      --  Is_Open is a parsing barrier, it is True when whole parsing can be
      --  conducted and False otherwise. When it is False the naming exceptions
      --  source filenames collected into Object.Skip_Src container to ignore
      --  at the Update_Sources stage. When it is True, the entire parsing
      --  processes and naming exception source filenames collected into Actual
      --  to remove it from Object.Skip_Src at the end of parsing.

      In_Pack    : Boolean := False;
      Pack_Name  : Unbounded_String;
      Pack_Attrs : PA.Set.Object;
      Pack_Vars  : GPR2.Project.Variable.Set.Object;
      --  Package orientated state, when parsing is in a package In_Pack is
      --  set and Pack_Name contains the name of the package and all parsed
      --  attributes are recorded into Pack_Attrs set and all
      --  parsed variables are recorded into Pack_Vars.

      Undefined_Attribute_Count          : Natural := 0;
      Previous_Undefined_Attribute_Count : Natural := 0;

      -----------------------
      -- Ensure_Source_Loc --
      -----------------------

      function Ensure_Source_Loc
        (Values : Containers.Source_Value_List;
         Sloc   : Source_Reference.Object)
            return Containers.Source_Value_List
      is
         New_List : Containers.Source_Value_List;
      begin
         for V of Values loop
            New_List.Append
              (Source_Reference.Value.Object
                 (Source_Reference.Value.Create
                      (Sloc         => Sloc,
                       Text         => V.Text,
                       At_Pos       => (if V.Has_At_Pos then V.At_Pos else 0),
                       From_Default => V.Is_From_Default)));
         end loop;

         return New_List;
      end Ensure_Source_Loc;

      -------------------------
      -- Get_Attribute_Index --
      -------------------------

      function Get_Attribute_Index
        (Node : Attribute_Reference;
         Pack : Optional_Name_Type := No_Name) return PAI.Object
      is
         Name           : constant Name_Type :=
                            Get_Name_Type
                              (Single_Tok_Node (F_Attribute_Name (Node)));
         I_Node         : constant GPR_Node := F_Attribute_Index (Node);
         Q_Name         : constant PRA.Qualified_Name :=
                            PRA.Create (Name, Pack);
         Case_Sensitive : Boolean;

      begin
         if not Present (I_Node) then
            return PAI.Undefined;
         end if;

         if I_Node.Kind in GPR_Others_Designator_Range then
            return PAI.I_Others;
         end if;

         if Name = PRA.Switches then
            Case_Sensitive := Is_Switches_Index_Case_Sensitive
              (Get_Value_Type (I_Node.As_Single_Tok_Node));
         elsif PRA.Exists (Q_Name) then
            Case_Sensitive := PRA.Get (Q_Name).Index_Case_Sensitive;
         else
            Case_Sensitive := True;
         end if;

         return PAI.Create
           (Get_Value_Type (I_Node.As_Single_Tok_Node),
            Case_Sensitive);
      end Get_Attribute_Index;

      -----------------------
      -- Get_Attribute_Ref --
      -----------------------

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Optional_Name_Type := No_Name) return Item_Values
      is
         use type PRA.Value_Kind;
         use PAI;

         Sloc   : constant Source_Reference.Object :=
                    Get_Source_Reference (Self.File, Node);
         Name   : constant Name_Type :=
                    Get_Name_Type
                      (Single_Tok_Node (F_Attribute_Name (Node)));
         Q_Name : constant PRA.Qualified_Name :=
                    PRA.Create (Name, Pack);

         Index  : constant PAI.Object := Get_Attribute_Index (Node, Pack);
         View   : constant GPR2.Project.View.Object :=
                    Process.View.View_For (Project);

         Attr   : PA.Object;

         Indexed_Values : Indexed_Item_Values := Unfilled_Indexed_Values;

         function Default_Value
           (Attribute_Name : Name_Type;
            Attrs          : PA.Set.Object) return PA.Object;
         --  Returns default value using Attrs for referenced default.

         procedure Fill_Indexed_Values (Attrs : PA.Set.Object);
         --  fill Indexed_Values if Index is undefined and Q_Name allows Index

         -------------------
         -- Default_Value --
         -------------------

         function Default_Value
           (Attribute_Name : Name_Type;
            Attrs          : PA.Set.Object) return PA.Object
         is

            Result : PA.Object;
            --  Return value.

            Q_Name : constant PRA.Qualified_Name :=
                       PRA.Create (Attribute_Name, Pack);
            --  Requested attribute qualified name

            procedure Fill_Result (Def : PRA.Def);
            --  Fill Result with default value extracted from 'Def'

            package SR renames Source_Reference;

            Project_SRef : constant SR.Object :=
                             SR.Object
                               (SR.Create (Self.Path_Name.Value, 0, 0));
            --  Source reference used when creating attribute's index/object.

            function Attr_Id return SR.Identifier.Object is
              (SR.Identifier.Object
                 (SR.Identifier.Create (Project_SRef, Attribute_Name)));
            --  Name used when creating attribute object.

            -----------------
            -- Fill_Result --
            -----------------

            procedure Fill_Result (Def : PRA.Def) is
               use type PRA.Index_Kind;
               package VSR renames Containers.Name_Value_Map_Package;

               function Create_Index
                 (Name : Value_Type) return PAI.Object is
                 (if Def.Index = PRA.No
                  then PAI.Undefined
                  else PAI.Create
                         (SR.Value.Object
                            (SR.Value.Create
                               (Project_SRef, Name)), False, False));
               --  Index created from attribute definition

               function Create_Attribute
                 (Value : SR.Value.Object) return PA.Object;
               --  Create attribute object with Attr_Id name, Kind extracted
               --  from definition and containing 'Value'

               ----------------------
               -- Create_Attribute --
               ----------------------

               function Create_Attribute
                 (Value : SR.Value.Object) return PA.Object
               is
                  Attr : PA.Object;
               begin
                  if Def.Value = PRA.List then
                     Attr := PA.Create
                       (Name   => Attr_Id,
                        Index  => Index,
                        Values => Containers.Source_Value_Type_List.To_Vector
                                    (Value, 1));
                  else
                     Attr := PA.Create (Attr_Id, Index, Value);
                  end if;

                  Attr.Set_Case
                    (Index_Is_Case_Sensitive => Def.Index_Case_Sensitive,
                     Value_Is_Case_Sensitive => Def.Value_Case_Sensitive);

                  return Attr;
               end Create_Attribute;

            begin
               if Def.Has_Default_In (Self.Qualifier) then
                  if Def.Default_Is_Reference then
                     declare
                        Ref_Name : constant Name_Type :=
                                     Name_Type (Def.Default.First_Element);
                        --  reference attribute's name

                        package PAS renames PA.Set;

                        CS       : constant PAS.Cursor :=
                                     Attrs.Find (Ref_Name, Index);
                        --  get value in 'Attrs' map.
                     begin
                        if PAS.Has_Element (CS) then
                           --  return renamed referenced value
                           Result := PAS.Element (CS).Rename (Attr_Id);
                        else
                           --  return referenced attribute default value
                           Result := Default_Value
                             (Attribute_Name  => Ref_Name,
                              Attrs           => Attrs);
                        end if;
                     end;
                  elsif not Def.Default.Is_Empty then
                     --  look for default definition for the requested index
                     for D in Def.Default.Iterate loop
                        if Index = Create_Index
                          (Value_Type (VSR.Key (D)))
                        then
                           --  At correct element, return it.
                           Result := Create_Attribute
                             (SR.Value.Object
                                (SR.Value.Create
                                     (Project_SRef, VSR.Element (D))));
                        end if;
                     end loop;
                  end if;
               else
                  Result := PA.Undefined;
               end if;
            end Fill_Result;
         begin
            if PRA.Exists (Q_Name) then
               Fill_Result (PRA.Get (Q_Name));
            else
               Result := PA.Undefined;
            end if;
            return Result;
         end Default_Value;

         -------------------------
         -- Fill_Indexed_Values --
         -------------------------

         procedure Fill_Indexed_Values
           (Attrs : PA.Set.Object)
         is
            use Indexed_Item_Values_Vectors;
            use PRA;
         begin
            if Index = PAI.Undefined
              and then PRA.Get (Q_Name).Index /= PRA.No
            then
               Indexed_Values.Filled := True;
               Indexed_Values.Attribute_Pack := +String (Pack);
               Indexed_Values.Attribute_Name := +String (Name);

               for Attribute of Attrs.Filter (Name => Name) loop
                  Indexed_Values.Values.Append
                    ((Index => (if Attribute.Is_Defined
                                then Attribute.Index
                                else PAI.Undefined),
                      Values => Attribute.Values,
                      Single => Attribute.Kind = PRA.Single), 1);
               end loop;
            end if;
         end Fill_Indexed_Values;

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

         if not PRA.Exists (Q_Name) then
            if not In_Pack
              or else PRP.Exists (Name_Type (To_String (Pack_Name)))
            then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & PRA.Image (Q_Name) & """ is not defined",
                     Get_Source_Reference (Self.File, Node)));
            end if;

            return Empty_Item_Values;
         end if;

         --  If the attribute is not found or not yet resolved we need
         --  to ensure that the Values list respect the post
         --  condition. That is, a Single result must contain a single
         --  element.

         if Project = Name_Type (To_String (Self.Name))
           or else Project = "Project"
         then
            --  An attribute referencing a value in the current project

            --  Record only if fully defined (add a boolean to control this)
            --  and stop parsing when the number of undefined attribute is
            --  stable.

            if Pack = No_Name then
               Fill_Indexed_Values (Attrs);
               Attr := Attrs.Element (Name, Index);

               --  Attributes used to configure the toolchain are handled
               --  specially: they are global to the project tree, and are just
               --  considered default values when creating the configuration
               --  project.

               --  Because of that, we need to implement a freezing mechanism
               --  for such attributes: once read, they can't be modified.
               --
               --  The freeze mechanism is used as follows:
               --  when the attribute is accessed, we first ensure that an
               --  attribute object is properly created in the Attrs list of
               --  the view, with the proper value from the tree, and we
               --  mark this attribute as frozen.
               --
               --  Then in the calls to Record_Attribute below, if a
               --  previous attribute already exists and is frozen, then an
               --  error is raised.

               if Attr.Is_Defined then
                  if PRA.Get (Q_Name).Is_Toolchain_Config
                    and then not Attr.Is_Frozen
                  then
                     Attr.Freeze;
                     Attrs.Include (Attr);
                  end if;

               elsif Name = PRA.Target then
                  --  Project'Target case
                  Attr := PA.Create
                    (Get_Identifier_Reference
                       (Self.Path_Name, Sloc_Range (Node), Name),
                     Value   => Get_Value_Reference
                       (Value_Not_Empty (Tree.Target), Sloc),
                     Default => True,
                     Frozen  => True);
                  Attrs.Include (Attr);

               elsif Name = PRA.Canonical_Target then
                  --  Project'Target case
                  Attr := PA.Create
                    (Get_Identifier_Reference
                       (Self.Path_Name, Sloc_Range (Node), Name),
                     Value   => Get_Value_Reference
                       (Value_Not_Empty (Tree.Target (Canonical => True)),
                        Sloc),
                     Default => True,
                     Frozen  => True);
                  Attrs.Include (Attr);

               elsif Name = PRA.Runtime and then Index /= PAI.Undefined then
                  --  Project'Runtime (<lang>)
                  Attr := PA.Create
                    (Get_Identifier_Reference
                       (Self.Path_Name, Sloc_Range (Node), Name),
                     Index   => Index,
                     Value   => Get_Value_Reference
                       (Value_Type (Tree.Runtime (Name_Type (Index.Text))),
                        Sloc),
                     Default => True,
                     Frozen  => True);
                  Attrs.Include (Attr);
               else
                  Attr := Default_Value (Name, Attrs);
               end if;

            elsif Pack_Name /= Null_Unbounded_String
              and then Name_Type (To_String (Pack_Name)) = Name_Type (Pack)
            then
               --  This is the current parsed package, look into Pack_Attrs

               Fill_Indexed_Values (Pack_Attrs);
               Attr := Pack_Attrs.Element (Name, Index);

               if not Attr.Is_Defined then
                  Attr := Default_Value (Name, Pack_Attrs);
               end if;

            elsif Packs.Contains (Name_Type (Pack)) then
               --  Or in another package in the same project

               Fill_Indexed_Values
                 (Packs.Element (Name_Type (Pack)).Attributes);
               Attr := Packs.Element
                 (Name_Type (Pack)).Attributes.Element (Name, Index);

               if not Attr.Is_Defined then
                  Attr := Default_Value
                    (Name, Packs.Element (Name_Type (Pack)).Attributes);
               end if;

            else
               Fill_Indexed_Values (GPR2.Project.Attribute.Set.Empty_Set);
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "package """ & String (Pack)
                     & """ not declared in project """
                     & String (Project) & '"',
                     Get_Source_Reference (Self.File, Node)));
            end if;

         elsif View.Is_Defined then
            if Pack = No_Name then
               Fill_Indexed_Values (View.Attributes);
               if not View.Check_Attribute (Name, Index, Result => Attr) then
                  --  Special case for Target & Runtime, that always default
                  --  to the configuration value
                  if Name = PRA.Target then
                     --  Project'Target case
                     Attr := PA.Create
                       (Get_Identifier_Reference
                          (Self.Path_Name, Sloc_Range (Node), Name),
                        Value   => Get_Value_Reference
                          (Value_Not_Empty (Tree.Target), Sloc,
                           From_Default => True),
                        Default => True,
                        Frozen  => True);

                  elsif Name = PRA.Canonical_Target then
                     --  Project'Target case
                     Attr := PA.Create
                       (Get_Identifier_Reference
                          (Self.Path_Name, Sloc_Range (Node), Name),
                        Value   => Get_Value_Reference
                          (Value_Not_Empty (Tree.Target (Canonical => True)),
                           Sloc),
                        Default => True,
                        Frozen  => True);

                  elsif Name = PRA.Runtime
                    and then Index /= PAI.Undefined
                  then
                     --  Project'Runtime (<lang>)
                     Attr := PA.Create
                       (Get_Identifier_Reference
                          (Self.Path_Name, Sloc_Range (Node), Name),
                        Index   => Index,
                        Value   => Get_Value_Reference
                          (Value_Type (Tree.Runtime (Name_Type (Index.Text))),
                           Sloc),
                        Default => True,
                        Frozen  => True);

                  elsif Indexed_Values.Filled then
                     if Indexed_Values.Values.Is_Empty then
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Message.Error,
                              "associative array value not found",
                              Get_Source_Reference (Self.File, Node)));
                     end if;

                  else
                     Attr := Default_Value (Name, View.Attributes);
                  end if;
               end if;

            elsif View.Has_Packages (Pack) then
               Fill_Indexed_Values (View.Pack (Pack).Attributes);

               if not View.Pack (Pack).Check_Attribute
                 (Name, Index, Result => Attr)
               then
                  if Indexed_Values.Filled then
                     if Indexed_Values.Values.Is_Empty then
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Message.Error,
                              "associative array value not found",
                              Get_Source_Reference (Self.File, Node)));
                     end if;

                  else
                     Attr := Default_Value (Name, View.Pack (Pack).Attributes);
                  end if;
               end if;

            else
               Fill_Indexed_Values (PA.Set.Empty_Set);
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Error,
                     "package """ & String (Pack)
                     & """ not declared in project """
                     & String (Project) & '"',
                     Get_Source_Reference (Self.File, Node)));
            end if;

         elsif Project /= "Config" then
            --  Config project can be undefined at this stage

            Tree.Log_Messages.Append
              (Message.Create
                 (Missing_Project_Error_Level,
                  "Project """ & String (Project) & """ not found",
                  Get_Source_Reference (Self.File, Node)));
         end if;

         return Result : Item_Values do
            Result.Indexed_Values := Indexed_Values;
            if Attr.Is_Defined then
               Result.Values := Ensure_Source_Loc (Attr.Values, Sloc);
               Result.Single := Attr.Kind = PRA.Single;
            else
               if PRA.Exists (Q_Name) then
                  Result.Single := PRA.Get (Q_Name).Value = PRA.Single;
               else
                  Result.Single := False;
               end if;
               if Result.Single then
                  Result.Values :=
                    GPR2.Containers.Source_Value_Type_List.To_Vector
                      (New_Item => GPR2.Source_Reference.Value.Object
                         (GPR2.Source_Reference.Value.Create
                            (Sloc, GPR2.No_Value)),
                       Length   => 1);
               end if;
            end if;
         end return;
      end Get_Attribute_Ref;

      -------------------
      -- Get_Term_List --
      -------------------

      function Get_Term_List (Node : Term_List) return Item_Values is

         Result : Item_Values;
         --  The list of values returned by Get_Term_List

         New_Item : Boolean := True;

         Force_Append : Boolean := False;
         --  When True new value are always added to list

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
                                 F_Terms (F_Parameters (Node));
                  Error      : Boolean with Unreferenced;
                  Var        : constant Name_Type :=
                                 Name_Type
                                   (Get_String_Literal
                                      (Child (Parameters, 1), Error));
                  Sep        : constant Value_Type :=
                                 Get_String_Literal
                                   (Child (Parameters, 2), Error);
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
                                 F_Terms (F_Parameters (Node));
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
                                 F_Terms (F_Parameters (Node));

                  Str : constant Item_Values :=
                          Get_Term_List (Child (Parameters, 1).As_Term_List);
                  Sep : constant Item_Values :=
                          Get_Term_List (Child (Parameters, 2).As_Term_List);
               begin
                  if not Str.Single then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Get_Source_Reference
                                      (Self.File, Child (Parameters, 1)),
                           Message => "Split first parameter must be a"
                                    & " string"));

                  elsif not Sep.Single then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Get_Source_Reference
                                      (Self.File, Child (Parameters, 2)),
                           Message => "Split separator parameter must be a"
                                    & " string"));

                  else
                     declare
                        Item  : constant Value_Type :=
                                  Str.Values.First_Element.Text;
                        Delim : constant Value_Type :=
                                  Sep.Values.First_Element.Text;
                     begin
                        if Delim = "" then
                           Tree.Log_Messages.Append
                             (Message.Create
                                (Level => Message.Error,
                                 Sloc  => Source_Reference.Object
                                            (Sep.Values.First_Element),
                                 Message => "Split separator parameter must"
                                          & " not be empty"));

                        elsif Item /= "" then
                           for V of Builtin.Split (Item, Delim) loop
                              New_Item := True;
                              Record_Value
                                (Get_Value_Reference
                                   (V,
                                    Source_Reference.Object
                                      (Str.Values.First_Element)));
                           end loop;
                        end if;
                     end;
                  end if;

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
                     At_Pos => (if At_Lit = No_GPR_Node then 0
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

            function Terms_Parser
              (Node : GPR_Node'Class) return Visit_Status;
            --  Parser for the terms tree

            ------------------
            -- Terms_Parser --
            ------------------

            function Terms_Parser
              (Node : GPR_Node'Class) return Visit_Status
            is
            begin
               case Kind (Node) is
                  when GPR_Terms =>
                     null;

                  when others =>
                     return Parser (Node);
               end case;

               return Into;
            end Terms_Parser;

         begin
            case Kind (Node) is
               when GPR_Terms =>
                  if Result.Values.Length /= 0 and then Result.Single then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Message.Error,
                           "literal string list cannot appear in a string",
                           Get_Source_Reference (Self.File, Node)));
                  end if;

                  --  We are opening not a single element but an expression
                  --  list.
                  Result.Single := False;

                  --  Handle '&' found in ("A" & "B", "C") as value extension
                  Force_Append := False;

                  --  Parse Terms tree
                  Traverse (GPR_Node (Node), Terms_Parser'Access);

                  --  Handle '&' found in () & "A as values list append

                  Force_Append := True;

                  Status := Over;

               when GPR_Term_List =>
                  --  A new value parsing is starting
                  New_Item := True;

               when GPR_String_Literal =>
                  Handle_String (Node.As_String_Literal);

               when GPR_String_Literal_At =>
                  Handle_String_At (Node.As_String_Literal_At);

               when GPR_Variable_Reference =>
                  Handle_Variable (Node.As_Variable_Reference);

               when GPR_Builtin_Function_Call =>
                  Handle_Builtin (Node.As_Builtin_Function_Call);

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
            if New_Item or else Force_Append then
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
            Result.Indexed_Values := Values.Indexed_Values;
            for V of Values.Values loop
               New_Item := New_Item or else Values.Single = False;
               Record_Value (V);
            end loop;

            --  If we add a list, then the final value must be a list

            if not Values.Single and then Result.Single then
               Result.Single := False;

               --  When parsing a list '&' should be used as append to list
               --  ("a") & "b" => ("a", "b")

               Force_Append := True;
            end if;
         end Record_Values;

      begin
         Result.Single := True;

         Traverse (GPR_Node (Node), Parser'Access);

         if Result.Values.Is_Empty
           and then Result.Indexed_Values = Unfilled_Indexed_Values
         then
            return Empty_Item_Values;
         else
            return Result;
         end if;
      end Get_Term_List;

      ----------------------
      -- Get_Variable_Ref --
      ----------------------

      function Get_Variable_Ref
        (Variable   : Name_Type;
         Source_Ref : Source_Reference.Object;
         Project    : Optional_Name_Type := No_Name;
         Pack       : Optional_Name_Type := No_Name;
         From_View  : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
        return Item_Values
      is
         use type PRA.Value_Kind;

         procedure Error (Msg : String := "") with Inline;
         --  Emit an error message that starts with
         --  "variable VARIABLE undefined". If Msg is not the empty string then
         --  append "(MSG)".

         function Get_Pack_Var
           (Pack : GPR2.Project.Pack.Object;
            Name : Name_Type) return Item_Values with Inline;
         --  Returns the variable value Pack.Name. If not found an error added

         -----------
         -- Error --
         -----------

         procedure Error (Msg : String := "") is
         begin
            if Msg'Length = 0 then
               Tree.Log_Messages.Append
                  (Message.Create
                     (Message.Error,
                      "variable '" & String (Variable) & "' is undefined",
                      Source_Ref));
            else
               Tree.Log_Messages.Append
                  (Message.Create
                     (Message.Error,
                      Msg,
                      Source_Ref));
            end if;
         end Error;

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
                  return (Values         => Ensure_Source_Loc (V.Values,
                                                               Source_Ref),
                          Single         => V.Kind = PRA.Single,
                          Indexed_Values => Unfilled_Indexed_Values);
               end;
            else
               Error;
               return Empty_Item_Values;
            end if;
         end Get_Pack_Var;

      begin
         if Project = No_Name and then not From_View.Is_Defined then
            --  Working from the current view that is processed. In that case
            --  use Packs and Vars variables as the view has not been updated
            --  yet.

            if Pack = No_Name then
               --  Look first if the variable is declared explicitely in the
               --  project itself otherwise iterate on the extended project
               --  chain.
               if Vars.Contains (Variable) then
                  --  ??? Source ref is plain ignored here
                  return (Values         =>
                            Ensure_Source_Loc (Vars (Variable).Values,
                                               Source_Ref),
                          Single         => Vars (Variable).Kind = PRA.Single,
                          Indexed_Values => Unfilled_Indexed_Values);
               elsif View.Is_Extending then
                  return Get_Variable_Ref (Variable   => Variable,
                                           From_View  => View.Extended_Root,
                                           Source_Ref => Source_Ref);
               else
                  Error;
               end if;
            else

               if In_Pack and then Pack = Name_Type (To_String (Pack_Name))
               then
                  --  If in the package currently processed use Pack_Vars to
                  --  find the value.
                  if Pack_Vars.Contains (Variable) then
                     return (Values =>
                               Ensure_Source_Loc (Pack_Vars (Variable).Values,
                                                  Source_Ref),
                             Single => Pack_Vars (Variable).Kind = PRA.Single,
                             Indexed_Values => Unfilled_Indexed_Values);
                  else
                     Error;
                  end if;
               else
                  --  Otherwise search into the already parsed packages
                  if Packs.Contains (Pack) then
                     return Get_Pack_Var (Packs.Element (Pack), Variable);
                  else
                     Error ("project or package " &
                              String (Pack) & " is undefined");
                  end if;
               end if;
            end if;

         elsif Project /= No_Name then
            --  We have a reference to subproject, resolve it and recurse
            declare
               Var_View : constant GPR2.Project.View.Object :=
                  (if From_View.Is_Defined then From_View.View_For (Project)
                   else View.View_For (Project));
            begin
               if not Var_View.Is_Defined then
                  if To_Lower (Project) = "project" then
                     --  If no project called project is defined then assume
                     --  project is the current project.
                     return Get_Variable_Ref (Variable   => Variable,
                                              Pack       => Pack,
                                              From_View  => From_View,
                                              Source_Ref => Source_Ref);
                  else
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Missing_Project_Error_Level,
                           "project " & String (Project) & " is undefined",
                           Source_Ref));
                  end if;
               else
                  return Get_Variable_Ref (Variable   => Variable,
                                           Pack       => Pack,
                                           From_View  => Var_View,
                                           Source_Ref => Source_Ref);
               end if;
            end;
         else
            --  From_View contains the variable we are looking at
            if Pack = No_Name then
               if From_View.Has_Variables (Variable) then
                  declare
                     V : constant GPR2.Project.Variable.Object :=
                        From_View.Variable (Variable);
                  begin
                     return (Values         => Ensure_Source_Loc (V.Values,
                                                                  Source_Ref),
                             Single         => V.Kind = PRA.Single,
                             Indexed_Values => Unfilled_Indexed_Values);
                  end;
               elsif From_View.Is_Extending then
                  return Get_Variable_Ref
                    (Variable   => Variable,
                     From_View  => From_View.Extended_Root,
                     Source_Ref => Source_Ref);
               else
                  Error;
               end if;

            else
               if From_View.Has_Packages (Pack) then
                  return Get_Pack_Var (From_View.Packages.Element (Pack),
                                       Variable);
               else
                  Error ("package " & String (Pack) & " is undefined");
               end if;
            end if;
         end if;

         return Empty_Item_Values;
      end Get_Variable_Ref;

      -------------------------
      -- Get_Variable_Values --
      -------------------------

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values
      is
         --  A reference a to variable values has the following format:
         --  prj_name[.pack_name[.var_name]]['Attribute]
         Var_Name   : constant Identifier_List := F_Variable_Name (Node);
         Att_Ref    : constant Attribute_Reference := F_Attribute_Ref (Node);
         Source_Ref : constant Source_Reference.Object :=
                        Get_Source_Reference (Self.File, Node);

         function Project_Name_Length
           (List : Identifier_List) return Natural;
         --  Returns the last Index in list of the project name part. can be
         --  0 if List not starting with a project name

         -------------------------
         -- Project_Name_Length --
         -------------------------

         function Project_Name_Length
           (List : Identifier_List) return Natural
         is
            Last : constant Natural :=
                     (if Present (Att_Ref)
                      then Children_Count (List)
                      else Children_Count (List) - 1);
            --  if not attribute reference last segment is variable name.

            function Is_Valid_Project_Name (Name : Name_Type) return Boolean is
              (Process.View.View_For (Name).Is_Defined
               or else Self.Imports.Contains (Name)
               or else (Self.Extended.Is_Defined
                        and then Self.Extended.Path_Name.Base_Name = Name)
               or else Is_Builtin_Project_Name (Name)
               or else Name_Type (To_String (Self.Name)) = Name);

         begin
            if Last >= 1
              and then Is_Valid_Project_Name (Get_Name_Type (List, 1, Last))
            then
               return Last;
            elsif Last >= 2
              and then Is_Valid_Project_Name
                (Get_Name_Type (List, 1, Last - 1))
            then
               return Last - 1;
            end if;
            return 0;
         end Project_Name_Length;

         Var_Name_Length : constant Positive :=  Children_Count (Var_Name);
         --  number of segment of variable name. cannot be 0 as var_name list
         --  empty are not allowed in gpr_parser language.

         Prj_Name_Length : constant Natural := Project_Name_Length (Var_Name);
         --  number of segment of project name part

      begin
         if Present (Att_Ref) then
            --  This is a reference to an attribute
            --  supported formats are prj'attr, pack'attr or prj.pack'attr
            --  prj can be a child project (root.child)
            return Get_Attribute_Ref
              (Project => (if Prj_Name_Length = 0
                           then (if Var_Name_Length = 1
                             then Name_Type (To_String (Self.Name))
                             else Get_Name_Type (Var_Name, 1,
                               Var_Name_Length - 1))
                           else Get_Name_Type (Var_Name, 1, Prj_Name_Length)),
               Pack    =>
                 (if Prj_Name_Length = Var_Name_Length
                  then No_Name
                  else Get_Name_Type
                    (Var_Name, Var_Name_Length, Var_Name_Length)),
               Node    => Att_Ref);
         else
            --  This is a reference to a variable
            declare
               Variable : constant Name_Type :=
                            Get_Name_Type
                               (Var_Name, Var_Name_Length, Var_Name_Length);
            begin
               if Var_Name_Length < 2 then
                  --  A 1 word variable can only refer to a variable declared
                  --  implicitely (in case of extends) or explicitely in the
                  --  current project itself.
                  if In_Pack and then Pack_Vars.Contains (Variable) then
                     --  If we are in the context of a package we don't need
                     --  the package prefix to refer to variables explicitely
                     --  declared in the package.
                     return Get_Variable_Ref
                       (Pack       => Name_Type (To_String (Pack_Name)),
                        Variable   => Variable,
                        Source_Ref => Source_Ref);
                  else
                     --  This is a reference to a variable in the current
                     --  project scope
                     return Get_Variable_Ref (Variable   => Variable,
                                              Source_Ref => Source_Ref);
                  end if;
               elsif Prj_Name_Length > 0
                 and then Prj_Name_Length + 1 = Var_Name_Length
               then
                  --  it is a <project_name>.<variable_name>
                  return Get_Variable_Ref
                    (Project    =>
                       Get_Name_Type (Var_Name, 1, Var_Name_Length - 1),
                     Variable   => Variable,
                     Source_Ref => Source_Ref);
               elsif Prj_Name_Length = 0 and then Var_Name_Length = 2 then
                  --  it is a <package_name>.<variable_name>
                  return Get_Variable_Ref
                    (Pack       =>
                       Get_Name_Type (Var_Name, 1, Var_Name_Length - 1),
                     Variable   => Variable,
                     Source_Ref => Source_Ref);
               else
                  --  it is a <project_name>.<package_name>.<variable_name>
                  return Get_Variable_Ref
                    (Project    =>
                       Get_Name_Type (Var_Name, 1, Var_Name_Length - 2),
                     Pack       =>
                       Get_Name_Type
                         (Var_Name, Var_Name_Length - 1, Var_Name_Length - 1),
                     Variable   => Variable,
                     Source_Ref => Source_Ref);
               end if;
            end;
         end if;
      end Get_Variable_Values;

      -----------------------
      -- Is_Limited_Import --
      -----------------------

      function Is_Limited_Import
        (Self : Object; Project : Name_Type) return Boolean
      is
         package PIS renames GPR2.Project.Import.Set;
         Position : constant PIS.Cursor := Self.Imports.Find (Project);
      begin
         return PIS.Has_Element (Position)
           and then PIS.Element (Position).Is_Limited;
      end Is_Limited_Import;

      --------------------------------------
      -- Is_Switches_Index_Case_Sensitive --
      --------------------------------------

      function Is_Switches_Index_Case_Sensitive
        (Value : Value_Type) return Boolean
      is
         function Is_Language
           (Value : Value_Type) return Boolean;
         --  Return true if Value is defined in language
         --  attribute.

         -----------------
         -- Is_Language --
         -----------------

         function Is_Language
           (Value : Value_Type) return Boolean
         is
            use GPR2.Project;
         begin
            if Attrs.Has_Languages then
               declare
                  Lang : constant Attribute.Object :=
                           Attrs.Languages;
               begin
                  for V of Lang.Values loop
                     if Strings.Equal_Case_Insensitive (V.Text, Value) then
                        return True;
                     end if;
                  end loop;

                  return False;
               end;

            else
               return False;
            end if;
         end Is_Language;

         Q_Name               : constant PRA.Qualified_Name :=
                                  PRA.Create ("switches", "compiler");
         Def                  : constant PRA.Def := PRA.Get (Q_Name);
         Index_Case_Sensitive : Boolean := Def.Index_Case_Sensitive;

      begin
         --  Check for source filename by looking for an
         --  extenssion separator or if the index is defined as
         --  a language. If Language is not defined the project
         --  tree will use the default languages and none of
         --  them have a dot in their name.

         if Strings.Fixed.Index (Value, ".") = 0
           or else Is_Language (Value)
         then
            --  No extension found, this is a language which is
            --  inconditionally non case-sensitive.

            Index_Case_Sensitive := False;
         end if;

         return Index_Case_Sensitive;
      end Is_Switches_Index_Case_Sensitive;

      ------------
      -- Parser --
      ------------

      function Parser (Node : GPR_Node'Class) return Visit_Status is
         Status : Visit_Status := Into;

         procedure Parse_Attribute_Decl (Node : Attribute_Decl);
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

            function Create_Index return PAI.Object;
            --  Create index with "at" part if exists

            procedure Create_And_Register_Attribute
              (Index  : PAI.Object;
               Values : Containers.Source_Value_List;
               Single : Boolean);
            --  Create attribute and register it if needed

            Q_Name : constant PRA.Qualified_Name :=
                       PRA.Create
                         (N_Str, Optional_Name_Type (To_String (Pack_Name)));

            Values   : constant Item_Values := Get_Term_List (Expr);
            A        : PA.Object;
            Is_Valid : Boolean := True;
            --  Set to False if the attribute definition is invalid

            Id : constant Source_Reference.Identifier.Object :=
                   Get_Identifier_Reference
                     (Self.Path_Name, Sloc_Range (Name), N_Str);
            --  The attribute name & sloc

            Sloc : constant Source_Reference.Object :=
                     Get_Source_Reference (Self.File, Node);

            use PAI;
            use PRA;

            Is_Name_Exception : constant Boolean :=
                                  Name_Type (To_Lower (N_Str)) in
                                    Spec | Specification | Body_N
                                    | Implementation;

            -----------------------------------
            -- Create_And_Register_Attribute --
            -----------------------------------

            procedure Create_And_Register_Attribute
              (Index  : PAI.Object;
               Values : Containers.Source_Value_List;
               Single : Boolean)
            is
               Position : Containers.Filename_Source_Reference_Package.Cursor;
               Inserted : Boolean;
            begin
               if Single then
                  pragma Assert (Expr.Children_Count >= 1);

                  A := PA.Create
                    (Name  => Id,
                     Index => Index,
                     Value => Values.First_Element);

               else
                  A := PA.Create
                    (Name   => Id,
                     Index  => Index,
                     Values => Values);
               end if;

               --  Record attribute with proper casing definition if found

               if PRA.Exists (Q_Name) then
                  declare
                     Def : constant PRA.Def := PRA.Get (Q_Name);

                  begin
                     if (Single and then Values.First_Element.Text = "")
                       or else (not Single and then Values.Length = 0)
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
                                    & PRA.Image (Q_Name)
                                    & " ignored"));
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

                     --  We need to special case the Switches attribute
                     --  which may have an index with a language or a source
                     --  filename. On case-sensitive system like Linux this
                     --  means that we need to have the language handled
                     --  without case-sensitivity but the source must be
                     --  handled with case taken into account.

                     Case_Switches_Index : declare
                        Index_Case_Sensitive : Boolean :=
                                                 Def.Index_Case_Sensitive;
                     begin
                        --  Check for source filename by looking for an
                        --  extenssion separator or if the index is defined
                        --  as a language. If Language is not defined
                        --  the project tree will use the default languages
                        --  and none of them have a dot in their name.

                        if Index.Is_Defined
                          and then not Index.Is_Others
                          and then A.Name.Text = PRA.Switches
                        then
                           --  No extension found, this is a language which
                           --  is inconditionally non case-sensitive.

                           Index_Case_Sensitive :=
                             Is_Switches_Index_Case_Sensitive (Index.Value);
                        end if;

                        A.Set_Case
                          (Index_Case_Sensitive,
                           Def.Value_Case_Sensitive);
                     end Case_Switches_Index;
                  end;
               end if;

               if Is_Valid then
                  if Is_Open then
                     if In_Pack then
                        Record_Attribute (Pack_Attrs, A, Sloc);

                        if Is_Name_Exception then
                           Actual.Include (Filename_Type (A.Value.Text));
                        end if;

                     else
                        Record_Attribute (Attrs, A, Sloc);
                     end if;

                  elsif Is_Name_Exception then
                     Self.Skip_Src.Insert
                       (Filename_Type (A.Value.Text), A.Value,
                        Position, Inserted);
                  end if;
               end if;
            end Create_And_Register_Attribute;

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
                        At_Pos => -- Ati),
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
            if not I_Sloc.Is_Defined
              and then PRA.Exists (Q_Name)
              and then PRA.Get (Q_Name).Index /= PRA.No
            then
               if not Values.Indexed_Values.Filled then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Sloc,
                        Message => "full associative array expression " &
                          "requires simple attribute reference"));

               elsif not Ada.Strings.Unbounded.Equal_Case_Insensitive
                 (Values.Indexed_Values.Attribute_Pack, Pack_Name)
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Sloc,
                        Message => "not the same package as " &
                          To_String (Pack_Name)));

               elsif not Ada.Strings.Equal_Case_Insensitive
                 (To_String (Values.Indexed_Values.Attribute_Name),
                  String (N_Str))
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Sloc,
                        Message => "full associative array expression " &
                          "must reference the same attribute """ &
                          String (N_Str) & '"'));

               else
                  for V of Values.Indexed_Values.Values loop
                     Create_And_Register_Attribute
                       (Index  => V.Index,
                        Values => V.Values,
                        Single => V.Single);
                  end loop;
               end if;

            else
               Create_And_Register_Attribute
                 (Index  => I_Sloc,
                  Values => Values.Values,
                  Single => Values.Single);
            end if;
         end Parse_Attribute_Decl;

         -----------------------------
         -- Parse_Case_Construction --
         -----------------------------

         procedure Parse_Case_Construction (Node : Case_Construction) is
            Var     : constant Variable_Reference := F_Var_Ref (Node);
            Value   : constant Containers.Source_Value_List :=
                        Get_Variable_Values (Var).Values;
            Att_Ref : constant Attribute_Reference := F_Attribute_Ref (Var);

         begin
            if Present (Att_Ref) then
               --  Can't have attribute references as value in case statements
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Get_Source_Reference (Self.File, Att_Ref),
                     Message => "attribute reference not allowed here"));

            elsif Value.Length = 1 then
               Case_Values.Append ('-' & Value.First_Element.Text);

               --  Set status to close for now, this will be open when a
               --  when_clause will match the value pushed just above on
               --  the vector.

               declare
                  Childs : constant Case_Item_List := F_Items (Node);
               begin
                  for C in 1 .. Children_Count (Childs) loop
                     Visit_Child (Child (Childs, C));
                  end loop;
               end;

               --  Then remove the case value

               Case_Values.Delete_Last;

               --  Skip all nodes for this construct

               Status := Over;

            elsif not Has_Error then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Missing_Project_Error_Level,
                     Sloc    => Get_Source_Reference (Self.File, Node),
                     Message => "variable '"
                     & String (Get_Name_Type (F_Variable_Name (Var), 1, 1))
                     & "' must be a simple value"));
               if Pre_Conf_Mode then
                  Status := Over;
               end if;
            end if;
         end Parse_Case_Construction;

         ---------------------
         -- Parse_Case_Item --
         ---------------------

         procedure Parse_Case_Item (Node : Case_Item) is

            function Parser (Node : GPR_Node'Class) return Visit_Status;

            Case_Value   : constant String := Case_Values.Last_Element;
            Is_That_Case : Boolean := False;

            ------------
            -- Parser --
            ------------

            function Parser (Node : GPR_Node'Class) return Visit_Status is
            begin
               case Kind (Node) is
                  when GPR_String_Literal =>
                     Is_That_Case :=
                       Unquote (To_UTF8 (Node.Text))
                         = Case_Value (2 .. Case_Value'Last);

                  when GPR_Others_Designator =>
                     Is_That_Case := True;

                  when others =>
                     return Into;
               end case;

               return (if Is_That_Case then Stop else Over);
            end Parser;

         begin
            case Case_Value (1) is
               when '-' =>
                  Traverse (F_Choice (Node), Parser'Access);

                  if Is_That_Case then
                     Case_Values (Case_Values.Last) (1) := '+';
                  end if;

               when '+' =>
                  Case_Values (Case_Values.Last) (1) := '^';

               when others =>
                  null;
            end case;
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
            Values     : constant Identifier_List := F_Extended_Name (Node);
            Num_Childs : constant Positive := Children_Count (Values);
            Project    : constant Name_Type :=
                           (if Num_Childs > 1
                            then Get_Name_Type
                              (Values, Last => Num_Childs - 1)
                            else "?");
            P_Name     : constant Name_Type :=
                           Get_Name_Type (Values, Num_Childs, Num_Childs);

            View       : constant GPR2.Project.View.Object :=
                           (if Num_Childs > 1
                            then Process.View.View_For (Project)
                            else GPR2.Project.View.Undefined);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Attrs.Clear;
            Pack_Vars.Clear;

            --  Check if the Project.Package reference exists

            if Num_Childs = 1 then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "project_name.package_name reference is required"));
            elsif Is_Limited_Import (Self, Project) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "cannot have a reference to a limited project"));

            elsif not View.Is_Defined then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Missing_Project_Error_Level,
                     Sloc    => Sloc,
                     Message =>
                       "project '" & String (Project) & "' is undefined"));

            elsif not View.Has_Packages (P_Name) then
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
            Sloc       : constant Source_Reference.Object :=
                           Get_Source_Reference (Self.File, Node);
            Values     : constant Identifier_List := F_Renamed_Name (Node);
            Num_Childs : constant Positive := Children_Count (Values);
            Project    : constant Name_Type :=
                           (if Num_Childs > 1
                            then Get_Name_Type
                              (Values, Last => Num_Childs - 1)
                            else "?");
            P_Name     : constant Name_Type :=
                           Get_Name_Type (Values, Num_Childs, Num_Childs);

            View       : constant GPR2.Project.View.Object :=
                           (if Num_Childs > 1
                            then Process.View.View_For (Project)
                            else GPR2.Project.View.Undefined);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Attrs.Clear;
            Pack_Vars.Clear;

            --  Check if the Project.Package reference exists

            if Num_Childs = 1 then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "project_name.package_name reference is required"));
            elsif Is_Limited_Import (Self, Project) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "cannot have a reference to a limited project"));

            elsif not View.Is_Defined then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Missing_Project_Error_Level,
                     Sloc    => Sloc,
                     Message =>
                       "project '" & String (Project) & "' is undefined"));

            elsif not View.Has_Packages (P_Name) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Warning,
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

            Name       : constant Identifier := F_Var_Name (Node);
            Expr       : constant Term_List := F_Expr (Node);
            Values     : constant Item_Values := Get_Term_List (Expr);
            V_Type     : constant Type_Reference := F_Var_Type (Node);
            V          : GPR2.Project.Variable.Object;
            Type_Def   : GPR2.Project.Typ.Object;
         begin
            if not V_Type.Is_Null then
               declare
                  package PTS renames GPR2.Project.Typ.Set.Set;
                  CT : PTS.Cursor;

                  Type_N     : constant Identifier_List :=
                                 F_Var_Type_Name (V_Type);
                  Num_Childs : constant Positive := Children_Count (Type_N);
                  T_Name     : constant Name_Type :=
                                 Get_Name_Type
                                   (Type_N, Num_Childs, Num_Childs);

                  procedure Get_Type_Def_From
                    (Imp : GPR2.Project.Import.Object);
                  --  Try to find type definition from Imp by name T_Name and
                  --  store it to Type_Def if found.

                  -----------------------
                  -- Get_Type_Def_From --
                  -----------------------

                  procedure Get_Type_Def_From
                    (Imp : GPR2.Project.Import.Object)
                  is
                     Path : constant GPR2.Path_Name.Object :=
                              GPR2.Project.Create
                                (Imp.Path_Name.Name, Search_Paths);
                     Types : GPR2.Project.Typ.Set.Object;
                  begin
                     if Path.Exists then
                        Types := Registry.Get (Path).Types;
                        CT := Types.Find (T_Name);

                        if PTS.Has_Element (CT) then
                           Type_Def := PTS.Element (CT);
                        end if;
                     end if;
                  end Get_Type_Def_From;

               begin
                  if Num_Childs > 1 then
                     --  We have a project prefix for the type name

                     declare
                        package PIS renames GPR2.Project.Import.Set;
                        Position : constant PIS.Cursor :=
                                     Self.Imports.Find
                                       (Get_Name_Type
                                          (Type_N, 1, Num_Childs - 1, "-"));
                     begin
                        if PIS.Has_Element (Position) then
                           Get_Type_Def_From (PIS.Element (Position));
                        end if;
                     end;
                  end if;

                  if not Type_Def.Is_Defined
                    or else Type_Def.Count_Values = 0
                  then
                     CT := Self.Types.Find (T_Name);

                     if PTS.Has_Element (CT) then
                        Type_Def := PTS.Element (CT);

                     elsif Self.Has_Extended then
                        Get_Type_Def_From (Self.Extended);
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
               Status := Traverse (Node => Child, Visit => Parser'Access);
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
            --  and Spec and Body attributes

            case Kind (Node) is
               when GPR_Case_Item =>
                  Parse_Case_Item (Node.As_Case_Item);

               when GPR_Attribute_Decl =>
                  Parse_Attribute_Decl (Node.As_Attribute_Decl);

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
        (Set  : in out PA.Set.Object;
         A    : PA.Object;
         Sloc : Source_Reference.Object)
      is
         Include : Boolean := True;
      begin
         if not A.Is_Defined then
            Undefined_Attribute_Count := Undefined_Attribute_Count + 1;
            Include := False;
         end if;

         if Include and then Set.Contains (A) then
            declare
               Old : constant PA.Object := Set.Element
                 (A.Name.Text,
                  (if A.Has_Index then A.Index
                   else GPR2.Project.Attribute_Index.Undefined));
            begin
               if Old.Is_Frozen then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level => Message.Error,
                        Sloc  => Sloc,
                        Message => "Cannot set attribute """ &
                          String (A.Name.Text) &
                          """ after it is referenced."));
                  Include := False;
               end if;
            end;
         end if;

         if Include then
            Set.Include (A);
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
           (PA.Create
              (Name    => Create_Name (PRA.Name),
               Value   => Get_Value_Reference
                            (To_Lower (To_String (Self.Name)), Sloc),
               Default => True));

         Attrs.Insert
           (PA.Create
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

      for F of Actual loop
         Self.Skip_Src.Exclude (F);
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
