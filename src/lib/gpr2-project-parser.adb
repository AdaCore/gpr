--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Wide_Unbounded;

with GNAT.Regpat;

with Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;
with Gpr_Parser.Common;

with GPR2.Builtin;
with GPR2.KB;
with GPR2.Message;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Definition;
with GPR2.Project.Pack;
with GPR2.Project.Parser.Registry;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Pack;
with GPR2.Source_Reference.Value;

package body GPR2.Project.Parser is

   use Ada.Exceptions;

   use Gpr_Parser.Common;
   use Gpr_Parser_Support.Text;

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
     (Node  : Gpr_Node'Class;
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

   function Present (Node : Gpr_Node'Class) return Boolean is
     (not Node.Is_Null);
   --  Returns True if the Node is present (not null)

   function Get_Source_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Slr       : Gpr_Parser_Support.Slocs.Source_Location_Range)
      return Source_Reference.Object
   is
     (Source_Reference.Object
        (Source_Reference.Create
             (Path_Name.Value,
              Positive (Slr.Start_Line),
              Positive (Slr.Start_Column))));

   function Get_Source_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Node      : Gpr_Node'Class) return Source_Reference.Object
   is
     (Get_Source_Reference (Path_Name, Sloc_Range (Node)));

   function Get_Value_Reference
     (Path_Name : GPR2.Path_Name.Object;
      Slr       : Gpr_Parser_Support.Slocs.Source_Location_Range;
      Value     : Value_Type;
      At_Pos    : Unit_Index := No_Index) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
        (Source_Reference.Value.Create
             (Get_Source_Reference (Path_Name, Slr), Value, At_Pos)));

   function Get_Value_Reference
     (Value        : Value_Type;
      Sloc         : Source_Reference.Object;
      At_Pos       : Unit_Index := No_Index;
      From_Default : Boolean := False) return Source_Reference.Value.Object
   is
     (Source_Reference.Value.Object
        (Source_Reference.Value.Create (Sloc, Value, At_Pos, From_Default)));

   function Get_Identifier_Reference
     (Path_Name  : GPR2.Path_Name.Object;
      Slr        : Gpr_Parser_Support.Slocs.Source_Location_Range;
      Identifier : Name_Type)
      return Source_Reference.Identifier.Object
   is
     (Source_Reference.Identifier.Object
        (Source_Reference.Identifier.Create
             (Get_Source_Reference (Path_Name, Slr), Identifier)));

   function Get_Attribute_Reference
     (Path_Name  : GPR2.Path_Name.Object;
      Slr        : Gpr_Parser_Support.Slocs.Source_Location_Range;
      Identifier : Q_Attribute_Id)
      return Source_Reference.Attribute.Object
   is
     (Source_Reference.Attribute.Object
        (Source_Reference.Attribute.Create
             (Get_Source_Reference (Path_Name, Slr), Identifier)));

   function Get_Raw_Path
     (Node : Single_Tok_Node'Class) return GPR2.Path_Name.Object
   is
     (GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Get_Filename (Node)),
         GPR2.Path_Name.No_Resolution));
   --  Creates project Path_Name.Object not checked for location

   function Get_String_Literal
     (N     : Gpr_Node'Class;
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

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache
     (Filename : GPR2.Path_Name.Object) is
   begin
      if Registry.Exists (Filename) then
         Registry.Unregister (Filename);
      end if;
   end Clear_Cache;

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
     (Node  : Gpr_Node'Class;
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
     (N     : Gpr_Node'Class;
      Error : out Boolean) return Value_Type
   is
      function Parser (Node : Gpr_Node'Class) return Visit_Status;
      --  Parser for the string-literal tree

      Result : Unbounded_String;

      ------------
      -- Parser --
      ------------

      function Parser (Node : Gpr_Node'Class) return Visit_Status is

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
            when Gpr_String_Literal =>
               Handle_String (Node.As_String_Literal);

            when Gpr_String_Literal_At | Gpr_Base_List =>
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
     (Contents        : Unbounded_String;
      Messages        : out Log.Object;
      Pseudo_Filename : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined)
      return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      Filename : constant GPR2.Path_Name.Object :=
                   (if Pseudo_Filename.Is_Defined
                    then Pseudo_Filename
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
      Messages      : in out Log.Object;
      File_Reader   : Gpr_Parser_Support.File_Readers.File_Reader_Reference :=
                        Gpr_Parser_Support.File_Readers.
                          No_File_Reader_Reference)
      return Object
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Unbounded;

      Context : constant Analysis_Context :=
                  Create_Context (Charset     => "UTF-8",
                                  File_Reader => File_Reader);
      Unit    : Analysis_Unit;
      Project : Object;

   begin
      if Registry.Check_Project (Filename, Project) then
         return Project;

      else
         if not Filename.Exists then
            Messages.Append
              (GPR2.Message.Create
                 (Level   => Message.Error,
                  Message => "project file """ & Filename.Value &
                             """ not found",
                  Sloc    => Source_Reference.Create (Filename.Value, 0, 0)));
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

      function Parser (Node : Gpr_Node'Class) return Visit_Status;
      --  Actual parser callabck for the project

      ------------
      -- Parser --
      ------------

      function Parser (Node : Gpr_Node'Class) return Visit_Status is

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

            procedure Parse_External_Reference (N : Builtin_Function_Call);
            --  Put the name of the external into the Externals list

            procedure Parse_External_As_List_Reference
              (N : Builtin_Function_Call);
            --  Put the name of the external into the Externals list

            procedure Parse_Split_Reference (N : Builtin_Function_Call);
            --  Check that split parameters has the proper type

            procedure Parse_Match_Reference (N : Builtin_Function_Call);
            --  Check that split parameters has the proper type

            procedure Parse_One_Parameter_Reference
              (N    : Builtin_Function_Call;
               Name : Name_Type);
            --  Check that lower/upper parameters has the proper type

            procedure Parse_Two_Parameter_Reference
              (N    : Builtin_Function_Call;
               Name : Name_Type);
            --  Check that default/alternative parameters has the proper type

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

               if Exprs.Is_Null or else Exprs.Children_Count = 0 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message =>
                          "missing parameters for external_as_list"
                        & " built-in"));

               elsif Exprs.Children_Count < 2 then
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    =>
                             Get_Source_Reference (Filename, Exprs),
                           Message =>
                             "external_as_list requires two "
                           & "parameters"));

               elsif Exprs.Children_Count > 2 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference
                            (Filename, Exprs),
                        Message =>
                          "external_as_list accepts only two parameters"));

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
            end Parse_External_As_List_Reference;

            ------------------------------
            -- Parse_External_Reference --
            ------------------------------

            procedure Parse_External_Reference (N : Builtin_Function_Call) is
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               if Exprs.Is_Null or else Exprs.Children_Count = 0 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message =>
                          "missing parameter for external built-in"));

               elsif Exprs.Children_Count > 2 then
                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
                           Sloc    =>
                             Get_Source_Reference (Filename, Exprs),
                           Message =>
                             "external built-in accepts at most two "
                             & "parameters."));

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
                        Project.Externals.Append (Optional_Name_Type (Var));

                        declare
                           Node : Gpr_Node := Exprs.Child (2);
                        begin
                           if not Node.Is_Null then
                              Node := Node.Child (1);

                              if not Node.Is_Null
                                and then Node.Kind = Gpr_Builtin_Function_Call
                              then
                                 Parse_Builtin (Node.As_Builtin_Function_Call);
                              end if;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end Parse_External_Reference;

            ---------------------------
            -- Parse_Match_Reference --
            ---------------------------

            procedure Parse_Match_Reference (N : Builtin_Function_Call) is
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               --  Note that this routine is only validating the syntax
               --  of the split built-in.

               if Exprs.Is_Null or else Exprs.Children_Count = 0 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message => "missing parameters for match built-in"));

               --  Check that the second parameter exists

               elsif Exprs.Children_Count < 2 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, Exprs),
                        Message => "match requires a second parameter"));

               --  Check that we don't have more than two parameters

               elsif Exprs.Children_Count > 3 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, Exprs),
                        Message =>
                          "match accepts a maximum of three parameters"));
               end if;
            end Parse_Match_Reference;

            -----------------------------------
            -- Parse_One_Parameter_Reference --
            -----------------------------------

            procedure Parse_One_Parameter_Reference
              (N    : Builtin_Function_Call;
               Name : Name_Type)
            is
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               --  Note that this routine is only validating the syntax
               --  of the split built-in.

               if Exprs.Is_Null or else Exprs.Children_Count = 0 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message => "missing parameters for "
                                    & String (Name) & "  built-in"));

               --  Check that we don't have more than two parameters

               elsif Exprs.Children_Count > 1 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, Exprs),
                        Message =>
                          String (Name) & " accepts only one parameter"));
               end if;
            end Parse_One_Parameter_Reference;

            ---------------------------
            -- Parse_Split_Reference --
            ---------------------------

            procedure Parse_Split_Reference (N : Builtin_Function_Call) is
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               --  Note that this routine is only validating the syntax
               --  of the split built-in.

               if Exprs.Is_Null or else Exprs.Children_Count = 0 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message => "missing parameters for split built-in"));

               --  Check that the second parameter exists

               elsif Exprs.Children_Count = 1 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, Exprs),
                        Message => "split requires a second parameter"));

               --  Check that we don't have more than two parameters

               elsif Exprs.Children_Count > 2 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, Exprs),
                        Message =>
                          "split accepts only two parameters"));
               end if;
            end Parse_Split_Reference;

            -----------------------------------
            -- Parse_Two_Parameter_Reference --
            -----------------------------------

            procedure Parse_Two_Parameter_Reference
              (N    : Builtin_Function_Call;
               Name : Name_Type)
            is
               Exprs : constant Term_List_List := F_Terms (F_Parameters (N));
            begin
               --  Note that this routine is only validating the syntax
               --  of the split built-in.

               if Exprs.Is_Null or else Exprs.Children_Count < 2 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Filename, N),
                        Message => "missing parameters for "
                                    & String (Name) & "  built-in"));

               --  Check that we don't have more than two parameters

               elsif Exprs.Children_Count > 2 then
                  Messages.Append
                    (GPR2.Message.Create
                       (Level   => Message.Error,
                        Sloc    =>
                          Get_Source_Reference (Filename, Exprs),
                        Message =>
                          String (Name) & " accepts only two parameters"));
               end if;
            end Parse_Two_Parameter_Reference;

            Function_Name : constant Name_Type :=
                              Get_Name_Type (F_Function_Name (N));
         begin
            if Function_Name = "external" then
               Parse_External_Reference (N);

            elsif Function_Name = "external_as_list" then
               Parse_External_As_List_Reference (N);

            elsif Function_Name = "split" then
               Parse_Split_Reference (N);

            elsif Function_Name = "lower" then
               Parse_One_Parameter_Reference (N, "lower");

            elsif Function_Name = "upper" then
               Parse_One_Parameter_Reference (N, "upper");

            elsif Function_Name = "match" then
               Parse_Match_Reference (N);

            elsif Function_Name = "default" then
               Parse_Two_Parameter_Reference (N, "default");

            elsif Function_Name = "alternative" then
               Parse_Two_Parameter_Reference (N, "alternative");

            elsif Function_Name = "item_at" then
               Parse_Two_Parameter_Reference (N, "item_at");

            elsif Function_Name = "filter_out" then
               Parse_Two_Parameter_Reference (N, "filter_out");

            elsif Function_Name = "remove_prefix" then
               Parse_Two_Parameter_Reference (N, "remove_prefix");

            elsif Function_Name = "remove_suffix" then
               Parse_Two_Parameter_Reference (N, "remove_suffix");

            else
               Messages.Append
                 (GPR2.Message.Create
                    (Level   => Message.Error,
                     Sloc    => Get_Source_Reference (Filename, N),
                     Message =>
                       "unknown built-in """
                     & String (Function_Name) & '"'));
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

            --  Check that project name is consistent with the end declaration

            if Name (Project) /= Name_Type (To_UTF8 (F_End_Name (N).Text)) then
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
                     when Gpr_Project_Qualifier_Abstract
                       => K_Abstract,
                     when Gpr_Project_Qualifier_Standard
                       => K_Standard,
                     when Gpr_Project_Qualifier_Library
                       => K_Library,
                     when Gpr_Project_Qualifier_Aggregate
                       => K_Aggregate,
                     when Gpr_Project_Qualifier_Aggregate_Library
                       => K_Aggregate_Library,
                     when Gpr_Project_Qualifier_Configuration
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
            Cur_Child  : Gpr_Node;
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
                       "type """ & String (Name) & """ already defined"));

            else
               for J in 1 .. Num_Childs loop
                  Cur_Child := Child (Gpr_Node (Values), J);

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
                                   '"' & String (Name)
                                 & """ has duplicate value """
                                 & String (Value) & '"'));
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
            Cur_Child  : Gpr_Node;
         begin
            for J in 1 .. Num_Childs loop
               Cur_Child := Child (Gpr_Node (Path_Names), J);

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
                                    Message => "duplicate with clause """
                                    & String (Path.Base_Name) & '"',
                                    Sloc    => Get_Source_Reference
                                      (Filename, Cur_Child)));

                           else
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Warning,
                                    Message => "duplicate project name """
                                    & String (Path.Base_Name) & '"',
                                    Sloc    => Get_Source_Reference
                                      (Filename, Cur_Child)));
                              Messages.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Warning,
                                    Message => "already in """
                                    & String (Prev.Path_Name.Name)
                                    & '"',
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
            when Gpr_Project_Declaration =>
               Parse_Project_Declaration (Node.As_Project_Declaration);

            when Gpr_Builtin_Function_Call =>
               Parse_Builtin (Node.As_Builtin_Function_Call);
               Status := Over;

            when Gpr_With_Decl =>
               Parse_With_Decl (Node.As_With_Decl);
               Status := Over;

            when Gpr_Typed_String_Decl =>
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
      Pre_Conf_Mode : Boolean := False;
      Ext_Conf_Mode : Boolean := False)
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
         Attribute_Name : Q_Optional_Attribute_Id := No_Attribute_Id;
         Values         : Indexed_Item_Values_Vectors.Vector;
      end record;

      type Item_Values is record
         Values         : Containers.Source_Value_List;
         Single         : Boolean := False;
         Indexed_Values : Indexed_Item_Values;
      end record
        with Dynamic_Predicate =>
          (if Item_Values.Single then Item_Values.Values.Length <= 1);
      --  Indexed_Values is filled only in Get_Attribute_Ref when attribute
      --  allows index and index is not provided in the reference.

      function To_Set
        (Values : Containers.Source_Value_List) return Containers.Value_Set;
      --  Create a set for fast searchiing from a list of values

      Unfilled_Indexed_Values : constant Indexed_Item_Values := (others => <>);

      Empty_Item_Values : constant Item_Values := (others => <>);
      No_Values         : constant Item_Values := (Single => True,
                                                   others => <>);

      function Missing_Project_Error_Level return Message.Level_Value is
        (if Pre_Conf_Mode then Message.Warning else Message.Error);
      --  Returns expected level for missing import messages

      function Ensure_Source_Loc
        (Values : Containers.Source_Value_List;
         Sloc   : Source_Reference.Object)
            return Containers.Source_Value_List;
      --  Ensure the values have the proper Source_Loc

      function Parser (Node : Gpr_Node'Class) return Visit_Status;
      --  Actual parser callabck for the project

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values;
      --  Parse and return the values for the given variable/attribute
      --  reference.

      function Get_Attribute_Index
        (Node : Attribute_Reference;
         Pack : Package_Id := Project_Level_Scope) return PAI.Object;
      --  Get the attribute index, if any, or PAI.Undefined

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Package_Id := Project_Level_Scope) return Item_Values;
      --  Return the value for an attribute reference in the given project and
      --  possibly the given package.

      function Get_Variable_Ref
        (Variable   : Name_Type;
         Source_Ref : Source_Reference.Object;
         Project    : Optional_Name_Type := No_Name;
         Pack       : Package_Id := Project_Level_Scope;
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
        (Set : in out PA.Set.Object;
         A   : PA.Object);
      --  Record an attribute into the given set. At the same time we increment
      --  the Empty_Attribute_Count if this attribute has an empty value. This
      --  is used to check whether we need to reparse the tree.

      function Has_Error return Boolean is
        (Tree.Log_Messages.Has_Error);

      View_Def    : GPR2.Project.Definition.Ref renames
                      Definition.Get (View);
      Attrs       : GPR2.Project.Attribute.Set.Object renames View_Def.Attrs;
      Vars        : GPR2.Project.Variable.Set.Object  renames View_Def.Vars;
      Packs       : GPR2.Project.Pack.Set.Map         renames View_Def.Packs;
      Types       : GPR2.Project.Typ.Set.Object       renames View_Def.Types;
      --  Easy access to the view's attributes, variables, packs and type
      --  definitions.
      --  Side note: unfortunately, for convenience we need to have View as
      --  an "in" parameter, while we obviously need to modify it. Working
      --  with "in out" would prevent usage of this function with return
      --  values (such as calling
      --  Parser.Process (Configuration.Corresponding_View)).
      --  View being just a ref, we can copy the ref and then use it to get
      --  the rw definition.

      Actual      : Containers.Filename_Set;
      --  Naming exception source filenames from active case alternatives

      Case_Values : Containers.Value_List;
      --  The case-values to match against the case-item. Each time a case
      --  statement is enterred the value for the case is prepended into this
      --  vector. The first value is then removed when exiting from the case
      --  statement. This is to support nested case statements.
      --  First character in each element mean is the case-item is open or
      --  closed. Other characters contain case value.

      In_Pack     : Boolean := False;
      Pack_Name   : Package_Id := Project_Level_Scope;
      Pack_Ref    : access GPR2.Project.Pack.Object;
      --  Package orientated state, when parsing is in a package In_Pack is
      --  set and Pack_Name contains the name of the package and Pack_Ref
      --  will point to the view's package object.

      Non_Fatal_Error : GPR2.Log.Object;
      --  Store non fatal errors that we record while parsing. This avoids
      --  stoping the parsing at the first error.

      function Is_Open return Boolean is
        (Case_Values.Is_Empty
         or else (for all CV of Case_Values => CV (1) = '+'));
      --  Is_Open is a parsing barrier, it is True when whole parsing can be
      --  conducted and False otherwise. When it is False the naming exceptions
      --  source filenames collected into Object.Skip_Src container to ignore
      --  at the Update_Sources stage. When it is True, the entire parsing
      --  processes and naming exception source filenames collected into Actual
      --  to remove it from Object.Skip_Src at the end of parsing.

      -----------------------
      -- Ensure_Source_Loc --
      -----------------------

      function Ensure_Source_Loc
        (Values : Containers.Source_Value_List;
         Sloc   : Source_Reference.Object) return Containers.Source_Value_List
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
         Pack : Package_Id := Project_Level_Scope) return PAI.Object
      is
         Name   : constant Attribute_Id :=
                    +Get_Name_Type (Single_Tok_Node (F_Attribute_Name (Node)));
         I_Node : constant Gpr_Node := F_Attribute_Index (Node);
         Q_Name : constant Q_Attribute_Id := (Pack, Name);
      begin
         if not Present (I_Node) then
            return PAI.Undefined;
         end if;

         if I_Node.Kind in Gpr_Others_Designator_Range then
            return PAI.I_Others;
         end if;

         declare
            Index : constant Value_Type :=
                      Get_Value_Type (I_Node.As_Single_Tok_Node);
         begin
            return
              PAI.Create
                 (Index,
                  Case_Sensitive =>
                    (if PRA.Exists (Q_Name)
                     then PRA.Is_Case_Sensitive (Index,
                                                 PRA.Get (Q_Name).Index_Type)
                     else True));
         end;
      end Get_Attribute_Index;

      -----------------------
      -- Get_Attribute_Ref --
      -----------------------

      function Get_Attribute_Ref
        (Project : Name_Type;
         Node    : Attribute_Reference;
         Pack    : Package_Id := Project_Level_Scope) return Item_Values
      is
         use type GPR2.Project.View.Object;
         use type PRA.Index_Value_Type;
         use type PRA.Value_Kind;
         use PAI;

         Sloc         : constant Source_Reference.Object :=
                          Get_Source_Reference (Self.File, Node);
         Name         : constant Attribute_Id :=
                          +Get_Name_Type
                            (Single_Tok_Node (F_Attribute_Name (Node)));
         Q_Name       : constant Q_Attribute_Id := (Pack, Name);
         Def          : constant PRA.Def := (if PRA.Exists (Q_Name)
                                             then PRA.Get (Q_Name)
                                             else PRA.Def'(others => <>));

         Index        : constant PAI.Object :=
                          Get_Attribute_Index (Node, Pack);
         Project_View : constant GPR2.Project.View.Object :=
                          (if Project = "Project" or else Project = View.Name
                           then View
                           else Process.View.View_For (Project));

         Attr         : PA.Object;

         Indexed_Values : Indexed_Item_Values := Unfilled_Indexed_Values;

         procedure Fill_Indexed_Values
           (View : GPR2.Project.View.Object;
            Pack : Package_Id);
         --  fill Indexed_Values if Index is undefined and Q_Name allows Index

         -------------------------
         -- Fill_Indexed_Values --
         -------------------------

         procedure Fill_Indexed_Values
           (View : GPR2.Project.View.Object;
            Pack : Package_Id)
         is
            Q_Name : constant Q_Attribute_Id := (Pack, Name);

            use Indexed_Item_Values_Vectors;
            use PRA;
         begin
            if Index = PAI.Undefined
              and then  Def.Index_Type /= PRA.No_Index
            then
               Indexed_Values.Filled := True;
               Indexed_Values.Attribute_Name := Q_Name;

               if View.Is_Defined then
                  for Attribute of View.Attributes (Q_Name) loop
                     Indexed_Values.Values.Append
                       ((Index  => Attribute.Index,
                         Values => Attribute.Values,
                         Single => Attribute.Kind = PRA.Single), 1);
                  end loop;
               end if;
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

            return No_Values;
         end if;

         --  For a project/attribute reference we need to check the attribute
         --  definition to know wether the result is multi-valued or not.

         if not PRA.Exists (Q_Name) then
            if not In_Pack
              or else PRP.Exists (Pack_Name)
            then
               --  Ignore case where we know nothing about the currently parsed
               --  package.

               --  Non defined package name
               if not PRP.Exists (Q_Name.Pack) then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "unknown package or project """ &
                          Image (Q_Name.Pack) & '"',
                        Get_Source_Reference (Self.File, Node)));

               else
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "undefined attribute """ & Image (Q_Name) &
                          '"',
                        Get_Source_Reference (Self.File, Node)));
               end if;
            end if;

            return No_Values;
         end if;

         if Index.Is_Defined and then Def.Index_Type = PRA.No_Index then
            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  "attribute """ & Image (Q_Name) & """ cannot have index",
                  Get_Source_Reference (Self.File, Node)));

            return No_Values;
         end if;

         --  If the attribute is not found or not yet resolved we need
         --  to ensure that the Values list respect the post
         --  condition. That is, a Single result must contain a single
         --  element.

         if Project_View.Is_Defined then
            Attr := Project_View.Attribute
                      (Name  => (Pack, Name), Index => Index);
            Fill_Indexed_Values (Project_View, Pack);

            --  Some toplevel attribute specific processing:
            if Pack = Project_Level_Scope then
               if Attr.Is_Defined then
                  if Project_View = View
                    and then Def.Is_Toolchain_Config
                    and then not Attr.Is_Frozen
                  then
                     Attr.Freeze;
                     Attrs.Include (Attr);
                  end if;

               elsif Indexed_Values.Filled
                 and then not Indexed_Values.Values.Is_Empty
               then
                  --  Full associative array values filled, no default to
                  --  create.
                  null;

                  --  Special case for built-in Canonical_Target and for
                  --  Runtime, that is at the minimum empty
               elsif Name = PRA.Canonical_Target.Attr then
                  --  Project'Target case
                  Attr := PA.Create
                    (Get_Attribute_Reference
                       (Self.Path_Name, Sloc_Range (Node),
                        (Project_Level_Scope, Name)),
                     Value   => Get_Value_Reference
                       (Value_Not_Empty (Tree.Target (Canonical => True)),
                        Sloc),
                     Default => True,
                     Frozen  => True);

               elsif Name = PRA.Runtime.Attr then
                  if Index /= PAI.Undefined then
                     --  Project'Runtime (<lang>)
                     Attr := PA.Create
                       (Get_Attribute_Reference
                          (Self.Path_Name,
                           Sloc_Range (Node),
                           (Project_Level_Scope, Name)),
                        Index   => Index,
                        Value   => Get_Value_Reference ("", Sloc),
                        Default => True,
                        Frozen  => True);
                  else
                     Indexed_Values.Attribute_Name :=
                       (Project_Level_Scope, Name);
                     Indexed_Values.Filled         := True;
                  end if;
               end if;
            end if;

            if not Attr.Is_Defined
              and then not Indexed_Values.Filled
              and then Def.Value /= PRA.Single
            then
               --  When no default is defined, lists are created empty.
               --  This allows the common pattern:
               --  My_List := ("new", "values") & Project'My_List
               Attr := GPR2.Project.Attribute.Create
                 (Source_Reference.Attribute.Object
                    (Source_Reference.Attribute.Create
                       (Source_Reference.Builtin,
                          (Project_Level_Scope, Name))),
                  Index   => Index,
                  Values  => Containers.Source_Value_Type_List.Empty_Vector,
                  Default => True);
            end if;

            if not Attr.Is_Defined then
               if Pack /= Project_Level_Scope
                 and then not Project_View.Has_Package (Pack)
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "package """ & Image (Pack)
                        & """ not declared in project """
                        & String (Project) & '"',
                        Get_Source_Reference (Self.File, Node)));

               elsif not Indexed_Values.Filled
                 or else Indexed_Values.Values.Is_Empty
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Error,
                        "undefined attribute """ &
                        (if Pack = Project_Level_Scope then ""
                         else Image (Pack) & "'") &
                          Image (Name) &
                          (if Index /= PAI.Undefined
                           then " (""" & Index.Text & """)"
                           else "") &
                          '"',
                        Get_Source_Reference (Self.File, Node)));
               end if;
            end if;

         elsif Project /= "Config" then
            --  Config project can be undefined at this stage

            Tree.Log_Messages.Append
              (Message.Create
                 (Missing_Project_Error_Level,
                  "undefined project or package """ & String (Project) &
                    '"',
                  Get_Source_Reference (Self.File, Node)));
         end if;

         return Result : Item_Values do
            Result.Indexed_Values := Indexed_Values;

            if Attr.Is_Defined then
               Result.Values := Ensure_Source_Loc (Attr.Values, Sloc);
               Result.Single := Attr.Kind = PRA.Single;
            else
               Result.Single := PRA.Get (Q_Name).Value = PRA.Single;

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

         function Parser (Node : Gpr_Node'Class) return Visit_Status;

         procedure Record_Value (Value : Source_Reference.Value.Object)
           with Post => Result.Values.Length'Old <= Result.Values.Length;
         --  Record Value into Result, either add it as a new value in the list
         --  (Single = False) or append the value to the current one.

         procedure Record_Values (Values : Item_Values);
         --  Same as above but for multiple values

         ------------
         -- Parser --
         ------------

         function Parser (Node : Gpr_Node'Class) return Visit_Status is
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

               procedure Handle_Item_At (Node : Builtin_Function_Call);
               --  Handle the Item_At build-in : Item_At (List, Index)

               procedure Handle_Filter_Out (Node : Builtin_Function_Call);
               --  Handle the Filter_Out build-in : Filter_Out (List, "REGEX")

               generic
                  with function Transform
                    (Value : Value_Type) return Value_Type;
               procedure Handle_Generic1 (Node : Builtin_Function_Call);
               --  A generic procedure call Transform for the single value or
               --  for each values in a list.

               generic
                  Name : String;
                  with function Transform_V
                    (Value1, Value2 : Value_Type) return Value_Type;
                  with function Transform_L
                    (List1, List2 : Containers.Source_Value_List)
                     return Containers.Source_Value_List;
               procedure Handle_Generic2 (Node : Builtin_Function_Call);
               --  A generic procedure call Transform for the single value or
               --  a list.

               generic
                  Name : String;
                  with function Transform
                    (Value1, Pattern : Value_Type) return Value_Type;
               procedure Handle_Generic2_LV (Node : Builtin_Function_Call);
               --  A generic procedure call Transform for the single value or
               --  for each values in a list.

               procedure Handle_Match (Node : Builtin_Function_Call);
               --  Handle the Match built-in :
               --    Match ("STR", "PATTERN"[, "REPL"])

               --------------------------------------
               -- Handle_External_As_List_Variable --
               --------------------------------------

               procedure Handle_External_As_List_Variable
                 (Node : Builtin_Function_Call)
               is
                  function Get_Parameter (Index : Positive) return Value_Type;
                  --  Returns parameter by Index

                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));

                  -------------------
                  -- Get_Parameter --
                  -------------------

                  function Get_Parameter
                    (Index : Positive) return Value_Type
                  is
                     Ignore : Boolean;
                  begin
                     return Get_String_Literal
                       (Child (Parameters, Index), Error => Ignore);
                  end Get_Parameter;

                  Var : constant Name_Type  := Name_Type (Get_Parameter (1));
                  Sep : constant Value_Type := Get_Parameter (2);

               begin
                  Result.Single := False;

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
                     if not Ext_Conf_Mode then
                        Tree.Log_Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Parameters),
                              Message => Exception_Message (E)));
                     end if;

                     Record_Value
                       (Get_Value_Reference
                          ("", Get_Source_Reference (Self.File, Parameters)));
                     Status := Over;
               end Handle_External_Variable;

               -----------------------
               -- Handle_Filter_Out --
               -----------------------

               procedure Handle_Filter_Out (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));
                  P1_Node    : constant Term_List :=
                                 Child (Parameters, 1).As_Term_List;
                  P2_Node    : constant Term_List :=
                                 Child (Parameters, 2).As_Term_List;
               begin
                  declare
                     P1 : constant Item_Values := Get_Term_List (P1_Node);
                     P2 : constant Item_Values := Get_Term_List (P2_Node);
                  begin
                     if P1.Single then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "first parameter of Filter_Out"
                                & " built-in must be a list"));
                     end if;

                     --  Check that 2nd parameter is a simple value

                     if not P2.Single then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "second parameter of Filter_Out"
                                & " built-in must be a value"));

                     else
                        declare
                           use GNAT;

                           Pattern : constant Value_Type :=
                                       P2.Values.First_Element.Text;

                           Regex   : constant Regexp.Regexp :=
                                       Regexp.Compile (Pattern);

                           L       : constant Containers.Source_Value_List :=
                                       Builtin.Filter_Out (P1.Values, Regex);
                        begin
                           for V of L loop
                              New_Item := True;

                              Record_Value
                                (Get_Value_Reference
                                   (V.Text,
                                 Get_Source_Reference
                                   (Self.File, Parameters)));
                           end loop;

                           Result.Single := False;
                        end;
                     end if;
                  end;

                  Status := Over;
               end Handle_Filter_Out;

               ---------------------
               -- Handle_Generic1 --
               ---------------------

               procedure Handle_Generic1 (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));
                  Value_Node : constant Term_List :=
                                 Child (Parameters, 1).As_Term_List;
               begin
                  declare
                     Values : constant Item_Values :=
                                Get_Term_List (Value_Node);
                  begin
                     if Values.Single then
                        Record_Value
                          (Get_Value_Reference
                             (Transform (Values.Values.First_Element.Text),
                              Get_Source_Reference
                                (Self.File, Parameters)));

                     else
                        for V of Values.Values loop
                           New_Item := True;

                           Record_Value
                             (Get_Value_Reference
                                (Transform (V.Text),
                                 Get_Source_Reference
                                   (Self.File, Parameters)));
                        end loop;

                        Result.Single := False;
                     end if;
                  end;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;
               end Handle_Generic1;

               ---------------------
               -- Handle_Generic2 --
               ---------------------

               procedure Handle_Generic2 (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));
                  P1_Node    : constant Term_List :=
                                 Child (Parameters, 1).As_Term_List;
                  P2_Node    : constant Term_List :=
                                 Child (Parameters, 2).As_Term_List;
               begin
                  declare
                     P1 : constant Item_Values := Get_Term_List (P1_Node);
                     P2 : constant Item_Values := Get_Term_List (P2_Node);
                  begin
                     if P1.Single xor P2.Single then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "parameters of " & Name
                                & " built-in must be of the same type"));
                     end if;

                     if P1.Single then
                        Record_Value
                          (Get_Value_Reference
                             (Transform_V
                                  (P1.Values.First_Element.Text,
                                   P2.Values.First_Element.Text),
                              Get_Source_Reference
                                (Self.File, Parameters)));

                     else
                        declare
                           L : constant Containers.Source_Value_List :=
                                 Transform_L (P1.Values, P2.Values);
                        begin
                           for V of L loop
                              New_Item := True;

                              Record_Value
                                (Get_Value_Reference
                                   (V.Text,
                                 Get_Source_Reference
                                   (Self.File, Parameters)));
                           end loop;
                        end;

                        Result.Single := False;
                     end if;
                  end;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;
               end Handle_Generic2;

               ------------------------
               -- Handle_Generic2_LV --
               ------------------------

               procedure Handle_Generic2_LV (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));
                  P1_Node    : constant Term_List :=
                                 Child (Parameters, 1).As_Term_List;
                  P2_Node    : constant Term_List :=
                                 Child (Parameters, 2).As_Term_List;
               begin
                  declare
                     P1 : constant Item_Values := Get_Term_List (P1_Node);
                     P2 : constant Item_Values := Get_Term_List (P2_Node);
                  begin
                     if not P2.Single then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "second parameters of " & Name
                                & " built-in must be a simple value"));
                     end if;

                     if P1.Single then
                        Record_Value
                          (Get_Value_Reference
                             (Transform
                                  (P1.Values.First_Element.Text,
                                   P2.Values.First_Element.Text),
                              Get_Source_Reference
                                (Self.File, Parameters)));

                     else
                        for V of P1.Values loop
                           declare
                              R : constant String :=
                                    Transform
                                      (V.Text, P2.Values.First_Element.Text);
                           begin
                              New_Item := True;

                              --  The result is empty, remove from the list

                              if R /= "" then
                                 Record_Value
                                   (Get_Value_Reference
                                      (Transform
                                           (V.Text,
                                            P2.Values.First_Element.Text),
                                       Get_Source_Reference
                                         (Self.File, Parameters)));
                              end if;
                           end;
                        end loop;

                        Result.Single := False;
                     end if;
                  end;

                  --  Skip all child nodes, we do not want to parse a second
                  --  time the string_literal.

                  Status := Over;
               end Handle_Generic2_LV;

               --------------------
               -- Handle_Item_At --
               --------------------

               procedure Handle_Item_At (Node : Builtin_Function_Call) is
                  use type Strings.Maps.Character_Set;

                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));
                  P1_Node    : constant Term_List :=
                                 Child (Parameters, 1).As_Term_List;
                  P2_Node    : constant Term_List :=
                                 Child (Parameters, 2).As_Term_List;
               begin
                  declare
                     P1 : constant Item_Values := Get_Term_List (P1_Node);
                     P2 : constant Item_Values := Get_Term_List (P2_Node);
                  begin
                     if P1.Single then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "first parameter of Index_At"
                                & " built-in must be a list"));
                     end if;

                     --  Check that 2nd parameter is a simple number

                     if not P2.Single
                       or else
                         Strings.Fixed.Index
                           (P2.Values.First_Element.Text,
                            Strings.Maps.Constants.Decimal_Digit_Set
                              or Strings.Maps.To_Set ("-"),
                            Strings.Outside) /= 0
                     then
                        Non_Fatal_Error.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Node),
                              Message =>
                                "second parameter of Index_At"
                                & " built-in must be a number"));

                     else
                        declare
                           Index : constant Integer :=
                                     Integer'Value
                                       (P2.Values.First_Element.Text);
                        begin
                           if abs (Index) > Positive (P1.Values.Length)
                             or else Index = 0
                           then
                              Non_Fatal_Error.Append
                                (GPR2.Message.Create
                                   (Level   => Message.Error,
                                    Sloc    =>
                                      Get_Source_Reference (Self.File, Node),
                                    Message =>
                                      "second parameter of Index_At"
                                      & " built-in out of bound"));
                           else
                              Record_Value
                                (Get_Value_Reference
                                   (Builtin.Item_At (P1.Values, Index),
                                    Source_Reference.Object
                                      (P1.Values.First_Element)));
                           end if;
                        end;
                     end if;
                  end;

                  Status := Over;
               end Handle_Item_At;

               ------------------
               -- Handle_Match --
               ------------------

               procedure Handle_Match (Node : Builtin_Function_Call) is
                  Parameters : constant Term_List_List :=
                                 F_Terms (F_Parameters (Node));

                  Str : constant Item_Values :=
                          Get_Term_List (Child (Parameters, 1).As_Term_List);
                  Pat : constant Item_Values :=
                          Get_Term_List (Child (Parameters, 2).As_Term_List);
                  Rep : constant Item_Values :=
                          (if Parameters.Children_Count = 3
                           then Get_Term_List
                                  (Child (Parameters, 3).As_Term_List)
                           else Empty_Item_Values);
               begin
                  if not Pat.Single then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Get_Source_Reference
                                      (Self.File, Child (Parameters, 2)),
                           Message => "Match pattern parameter must be a"
                                    & " string"));

                  elsif Rep /= Empty_Item_Values and then not Rep.Single then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Get_Source_Reference
                                      (Self.File, Child (Parameters, 2)),
                           Message => "Match replacement parameter must be a"
                                    & " string"));

                  else
                     declare
                        use GNAT;

                        Pattern : constant Value_Type :=
                                    Pat.Values.First_Element.Text;

                        Regex   : constant Regpat.Pattern_Matcher :=
                                    Regpat.Compile (Pattern);

                        Repl    : constant Value_Type :=
                                    (if Rep = Empty_Item_Values
                                     then ""
                                     else Rep.Values.First_Element.Text);
                     begin
                        if Str.Single then
                           declare
                              R : constant String :=
                                    Builtin.Match
                                      (Str.Values.First_Element.Text,
                                       Pattern, Regex, Repl);
                           begin
                              if R = "" then
                                 --  No match, result is an empty value
                                 Record_Value
                                   (Get_Value_Reference
                                      ("",
                                       Get_Source_Reference
                                         (Self.File, Parameters)));
                              else
                                 Record_Value
                                   (Get_Value_Reference
                                      (R,
                                       Source_Reference.Object
                                         (Str.Values.First_Element)));
                              end if;
                           end;

                        else
                           --  First parameter is a list, do the match on all
                           --  list items, if no match remove from the list.

                           for V of Str.Values loop
                              declare
                                 R : constant String :=
                                       Builtin.Match
                                         (V.Text, Pattern, Regex, Repl);
                              begin
                                 New_Item := True;

                                 if R /= "" then
                                    Record_Value
                                      (Get_Value_Reference
                                         (R,
                                          Source_Reference.Object
                                            (Str.Values.First_Element)));
                                 end if;
                              end;
                           end loop;

                           Result.Single := False;
                        end if;
                     end;
                  end if;

                  Status := Over;

               exception
                  when E : GNAT.Regpat.Expression_Error =>
                     if not Ext_Conf_Mode then
                        Tree.Log_Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Sloc    =>
                                Get_Source_Reference (Self.File, Parameters),
                              Message => Exception_Message (E)));
                     end if;

                     Record_Value
                       (Get_Value_Reference
                          ("", Get_Source_Reference (Self.File, Parameters)));
                     Status := Over;
               end Handle_Match;

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

               procedure Handle_Upper is
                 new Handle_Generic1 (Transform => Builtin.Upper);
               --  Handle the Upper built-in : Upper ("STR") or Upper (VAR)

               procedure Handle_Lower is
                 new Handle_Generic1 (Transform => Builtin.Lower);
               --  Handle the Lower built-in : Lower ("STR") or Lower (VAR)

               procedure Handle_Default is new Handle_Generic2
                 ("Default",
                  Transform_V => Builtin.Default,
                  Transform_L => Builtin.Default);
               --  Handle the Default built-in : Default ("STR", "def")

               procedure Handle_Alternative is new Handle_Generic2
                 ("Alternative",
                  Transform_V => Builtin.Alternative,
                  Transform_L => Builtin.Alternative);
               --  Handle the Alternative built-in :
               --    Alternative ("STR", "def")

               procedure Handle_Remove_Prefix is new Handle_Generic2_LV
                 ("Remove_Prefix",
                  Transform => Builtin.Remove_Prefix);
               --  Handle the Remove_Prefix built-in :
               --    Remove_Prefix ("STR", "def")

               procedure Handle_Remove_Suffix is new Handle_Generic2_LV
                 ("Remove_Suffix",
                  Transform => Builtin.Remove_Suffix);
               --  Handle the Remove_Prefix built-in :
               --    Remove_Suffix ("STR", "def")

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

               elsif Function_Name = "lower" then
                  Handle_Lower (Node);

               elsif Function_Name = "upper" then
                  Handle_Upper (Node);

               elsif Function_Name = "match" then
                  Handle_Match (Node);

               elsif Function_Name = "default" then
                  Handle_Default (Node);

               elsif Function_Name = "alternative" then
                  Handle_Alternative (Node);

               elsif Function_Name = "item_at" then
                  Handle_Item_At (Node);

               elsif Function_Name = "filter_out" then
                  Handle_Filter_Out (Node);

               elsif Function_Name = "remove_prefix" then
                  Handle_Remove_Prefix (Node);

               elsif Function_Name = "remove_suffix" then
                  Handle_Remove_Suffix (Node);
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
                     At_Pos =>
                       (if At_Lit = No_Gpr_Node then 0
                        else Unit_Index'Wide_Wide_Value (At_Lit.Text))));
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
              (Node : Gpr_Node'Class) return Visit_Status;
            --  Parser for the terms tree

            ------------------
            -- Terms_Parser --
            ------------------

            function Terms_Parser
              (Node : Gpr_Node'Class) return Visit_Status
            is
            begin
               case Kind (Node) is
                  when Gpr_Terms =>
                     null;

                  when others =>
                     return Parser (Node);
               end case;

               return Into;
            end Terms_Parser;

         begin
            case Kind (Node) is
               when Gpr_Terms =>
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
                  Traverse (Gpr_Node (Node), Terms_Parser'Access);

                  --  Handle '&' found in () & "A as values list append

                  Force_Append := True;

                  Status := Over;

               when Gpr_Term_List =>
                  --  A new value parsing is starting
                  New_Item := True;

               when Gpr_String_Literal =>
                  Handle_String (Node.As_String_Literal);

               when Gpr_String_Literal_At =>
                  Handle_String_At (Node.As_String_Literal_At);

               when Gpr_Variable_Reference =>
                  Handle_Variable (Node.As_Variable_Reference);

               when Gpr_Builtin_Function_Call =>
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
               New_Item := New_Item or else not Values.Single;
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

         Traverse (Gpr_Node (Node), Parser'Access);

         return Result;
      end Get_Term_List;

      ----------------------
      -- Get_Variable_Ref --
      ----------------------

      function Get_Variable_Ref
        (Variable   : Name_Type;
         Source_Ref : Source_Reference.Object;
         Project    : Optional_Name_Type := No_Name;
         Pack       : Package_Id := Project_Level_Scope;
         From_View  : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
        return Item_Values
      is
         use type PRA.Value_Kind;

         procedure Error (Msg : String := "") with Inline;
         --  Emit an error message that starts with
         --  "variable VARIABLE undefined". If Msg is not the empty string then
         --  append "(MSG)".

         function Get_Pack_Var
           (View : GPR2.Project.View.Object;
            Pack : Package_Id;
            Name : Name_Type) return Item_Values with Inline;
         --  Returns the variable value Pack.Name. If not found an error added

         function Try_Visible_In
           (View : GPR2.Project.View.Object) return Item_Values;
         --  Try to find variable either in extended view or in parent one

         -----------
         -- Error --
         -----------

         procedure Error (Msg : String := "") is
         begin
            Tree.Log_Messages.Append
              (Message.Create
                 (Message.Error,
                  (if Msg /= "" then Msg
                   else "undefined variable """ &
                        (if Project = No_Name then ""
                         else String (Project) & ".") &
                        (if Pack = Project_Level_Scope then ""
                         else Image (Pack) & ".") &
                        String (Variable) & '"'),
                  Source_Ref));
         end Error;

         ------------------
         -- Get_Pack_Var --
         ------------------

         function Get_Pack_Var
           (View : GPR2.Project.View.Object;
            Pack : Package_Id;
            Name : Name_Type) return Item_Values is
         begin
            if View.Has_Variables (Pack, Name) then
               declare
                  V : constant GPR2.Project.Variable.Object :=
                        View.Variable (Pack, Name);
               begin
                  return (Values => Ensure_Source_Loc (V.Values, Source_Ref),
                          Single => V.Kind = PRA.Single,
                          others => <>);
               end;
            else
               Error;
               return No_Values;
            end if;
         end Get_Pack_Var;

         --------------------
         -- Try_Visible_In --
         --------------------

         function Try_Visible_In
           (View : GPR2.Project.View.Object) return Item_Values
         is
            Result : Item_Values;
            Parent : GPR2.Project.View.Object;
         begin
            if View.Is_Extending then
               Result := Get_Variable_Ref
                 (Variable   => Variable,
                  From_View  => View.Extended_Root,
                  Source_Ref => Source_Ref);

               if Result /= No_Values then
                  return Result;
               end if;
            end if;

            if View.Check_Parent (Parent) then
               Result := Get_Variable_Ref
                 (Variable   => Variable,
                  From_View  => Parent,
                  Source_Ref => Source_Ref);

               if Result /= No_Values then
                  return Result;
               end if;
            end if;

            Error;

            return No_Values;
         end Try_Visible_In;

      begin
         if Project /= No_Name then
            --  We have a reference to subproject, resolve it and recurse

            declare
               Var_View : constant GPR2.Project.View.Object :=
                  (if From_View.Is_Defined then From_View.View_For (Project)
                   else View.View_For (Project));
            begin
               if Var_View.Is_Defined then
                  return Get_Variable_Ref
                           (Variable   => Variable,
                            Pack       => Pack,
                            From_View  => Var_View,
                            Source_Ref => Source_Ref);

               elsif To_Lower (Project) = "project" then
                  --  If no project called project is defined then assume
                  --  project is the current project.

                  return Get_Variable_Ref
                           (Variable   => Variable,
                            Pack       => Pack,
                            From_View  => From_View,
                            Source_Ref => Source_Ref);
               else
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Missing_Project_Error_Level,
                        "undefined project " & String (Project) & '"',
                        Source_Ref));
               end if;
            end;

         elsif not From_View.Is_Defined then
            --  Working from the current view that is processed. In that case
            --  use Packs and Vars variables as the view has not been updated
            --  yet.

            if Pack = Project_Level_Scope then
               --  Look first if the variable is declared explicitely in the
               --  project itself otherwise iterate on the extended project
               --  chain.

               if Vars.Contains (Variable) then
                  return (Values =>
                            Ensure_Source_Loc
                              (Vars (Variable).Values, Source_Ref),
                          Single => Vars (Variable).Kind = PRA.Single,
                          others => <>);

               else
                  return Try_Visible_In (View);
               end if;

            else
               if In_Pack and then Pack = Pack_Name  then
                  --  If in the package currently processed use Pack_Vars to
                  --  find the value.

                  if Pack_Ref.Vars.Contains (Variable) then
                     return
                       (Values =>
                          Ensure_Source_Loc
                            (Pack_Ref.Vars (Variable).Values, Source_Ref),
                        Single => Pack_Ref.Vars (Variable).Kind = PRA.Single,
                        others => <>);
                  else
                     Error;
                  end if;

               else
                  --  Otherwise search into the already parsed packages

                  if View.Has_Package (Pack) then
                     return Get_Pack_Var (View, Pack, Variable);
                  else
                     Error
                       ("undefined project or package """ & Image (Pack)
                        & '"');
                  end if;
               end if;
            end if;

         else
            --  From_View contains the variable we are looking at

            if Pack = Project_Level_Scope then
               if From_View.Has_Variables (Variable) then
                  declare
                     V : constant GPR2.Project.Variable.Object :=
                           From_View.Variable (Variable);
                  begin
                     return (Values =>
                               Ensure_Source_Loc (V.Values, Source_Ref),
                             Single => V.Kind = PRA.Single,
                             others => <>);
                  end;

               else
                  return Try_Visible_In (From_View);
               end if;

            elsif From_View.Has_Package (Pack) then
               return Get_Pack_Var (From_View, Pack, Variable);
            else
               Error ("undefined package """ & Image (Pack) & '"');
            end if;
         end if;

         return No_Values;
      end Get_Variable_Ref;

      -------------------------
      -- Get_Variable_Values --
      -------------------------

      function Get_Variable_Values
        (Node : Variable_Reference) return Item_Values
      is
         --  A reference to variable/attribute values has the following format:
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
                     Children_Count (List) -
                     (if Present (Att_Ref) then 0 else 1);
            --  if not attribute reference last segment is variable name

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
         --  Number of segment of variable name. cannot be 0 as var_name list
         --  empty are not allowed in gpr_parser language.

         Prj_Name_Length : constant Natural := Project_Name_Length (Var_Name);
         --  Number of segment of project name part

      begin
         if Present (Att_Ref) then
            --  This is a reference to an attribute
            --  supported formats are prj'attr, pack'attr or prj.pack'attr
            --  prj can be a child project (root.child)

            return Get_Attribute_Ref
              (Project => (if Prj_Name_Length = 0
                           then (if Var_Name_Length = 1
                                 then Name_Type (To_String (Self.Name))
                                 else Get_Name_Type
                                        (Var_Name, 1, Var_Name_Length - 1))
                           else Get_Name_Type (Var_Name, 1, Prj_Name_Length)),
               Pack    =>
                 (if Prj_Name_Length = Var_Name_Length
                  then Project_Level_Scope
                  else +Get_Name_Type
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
                  --  implicitly (in case of extends or child) or explicitly
                  --  in the current project itself.

                  if In_Pack and then Pack_Ref.Vars.Contains (Variable) then
                     --  If we are in the context of a package we don't need
                     --  the package prefix to refer to variables explicitely
                     --  declared in the package.

                     return Get_Variable_Ref
                       (Pack       => Pack_Name,
                        Variable   => Variable,
                        Source_Ref => Source_Ref);

                  else
                     --  This is a reference to a variable in the current
                     --  project scope

                     return Get_Variable_Ref
                              (Variable => Variable, Source_Ref => Source_Ref);
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
                       +Get_Name_Type (Var_Name, 1, Var_Name_Length - 1),
                     Variable   => Variable,
                     Source_Ref => Source_Ref);

               else
                  --  it is a <project_name>.<package_name>.<variable_name>

                  return Get_Variable_Ref
                    (Project    =>
                       Get_Name_Type (Var_Name, 1, Var_Name_Length - 2),
                     Pack       =>
                       +Get_Name_Type
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

      ------------
      -- Parser --
      ------------

      function Parser (Node : Gpr_Node'Class) return Visit_Status is
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
           with Post => Case_Values.Length'Old = Case_Values.Length;
         --  Parse a case construction, during a case construction parsing the
         --  Is_Open flag may be set to False and True. Set Is_Open comments.

         procedure Parse_Case_Item (Node : Case_Item)
           with Pre => not Case_Values.Is_Empty;
         --  Set Is_Open to True or False depending on the item

         procedure Visit_Child (Child : Gpr_Node);
         --  Recursive call to the Parser if the Child is not null

         --------------------------
         -- Parse_Attribute_Decl --
         --------------------------

         procedure Parse_Attribute_Decl (Node : Attribute_Decl) is
            Name  : constant Identifier := F_Attr_Name (Node);
            Index : constant Gpr_Node := F_Attr_Index (Node);
            Expr  : constant Term_List := F_Expr (Node);
            N_Str : constant Name_Type :=
                      Get_Name_Type (Name.As_Single_Tok_Node);
            N_Id  : constant Attribute_Id := +N_Str;

            function Create_Index return PAI.Object;
            --  Create index with "at" part if exists

            procedure Create_And_Register_Attribute
              (Index  : PAI.Object;
               Values : Containers.Source_Value_List;
               Single : Boolean);
            --  Create attribute and register it if needed

            Q_Name : constant Q_Attribute_Id := (Pack_Name, N_Id);

            Values   : constant Item_Values := Get_Term_List (Expr);
            A        : PA.Object;
            --  Set to False if the attribute definition is invalid

            Id : constant Source_Reference.Attribute.Object :=
                   Get_Attribute_Reference (Self.Path_Name,
                                            Sloc_Range (Name),
                                            Q_Name);
            --  The attribute name & sloc

            Sloc : constant Source_Reference.Object :=
                     Get_Source_Reference (Self.File, Node);

            use PAI;
            use PRA;

            Is_Name_Exception : constant Boolean :=
                                  N_Id in
                                    Naming.Spec.Attr |
                                    Naming.Specification.Attr |
                                    Naming.Body_N.Attr |
                                    Naming.Implementation.Attr;

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
                     if Def.Builtin then
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Level => Message.Error,
                              Sloc  => Sloc,
                              Message => "builtin attribute """ &
                                         Image (Q_Name) &
                                         """ is read-only"));
                     end if;

                     A.Set_Case
                       (Value_Is_Case_Sensitive => Def.Value_Case_Sensitive);
                  end;
               end if;

               if Is_Open then
                  if View_Def.Is_Root
                    and then View_Def.Kind /= K_Configuration
                    and then A.Name.Id = PRA.Target
                    and then Tree.Has_Configuration
                    and then A.Value.Text /= "all"
                  then
                     --  Check if defined target in the project is the
                     --  same as configuration. Else issue a warning.

                     declare
                        C_View : Project.View.Object renames
                                   Tree.Configuration.Corresponding_View;
                        T_Conf : constant Name_Type :=
                                   Name_Type
                                     (C_View.Attribute
                                        (PRA.Target).Value.Text);
                        T_Attr : constant Name_Type :=
                                   Name_Type (A.Value.Text);
                        Base   : GPR2.KB.Object := Tree.Get_KB;

                     begin
                        if not Base.Is_Defined then
                           Base := GPR2.KB.Create_Default
                             (GPR2.KB.Targetset_Only_Flags);
                        end if;

                        if Base.Normalized_Target (T_Conf) /=
                          Base.Normalized_Target (T_Attr)
                        then
                           Tree.Log_Messages.Append
                             (Message.Create
                                (Level   => Message.Warning,
                                 Sloc    => Sloc,
                                 Message => "target attribute '"
                                            & String (T_Attr)
                                            & "' not used, overriden by the "
                                            & "configuration's target: "
                                            & String (T_Conf)));
                        end if;
                     end;
                  end if;

                  declare
                     Alias : constant Q_Optional_Attribute_Id :=
                               PRA.Alias (Q_Name);
                     A2    : constant GPR2.Project.Attribute.Object :=
                               (if Alias.Attr /= No_Attribute
                                then A.Get_Alias (Alias)
                                else Project.Attribute.Undefined);
                  begin
                     if In_Pack then
                        Record_Attribute (Pack_Ref.Attrs, A);

                        if A2.Is_Defined
                          and then Pack_Ref.Attrs.Contains (A2)
                        then
                           --  Need to update the value
                           Record_Attribute (Pack_Ref.Attrs, A2);
                        end if;

                        if Is_Name_Exception then
                           Actual.Include (Filename_Type (A.Value.Text));
                        end if;

                     else
                        Record_Attribute (Attrs, A);

                        if A2.Is_Defined and then Attrs.Contains (A2) then
                           --  Need to update the value
                           Record_Attribute (Attrs, A2);
                        end if;
                     end if;
                  end;

               elsif Is_Name_Exception then
                  Self.Skip_Src.Insert
                    (Filename_Type (A.Value.Text), A.Value,
                     Position, Inserted);
               end if;
            end Create_And_Register_Attribute;

            ------------------
            -- Create_Index --
            ------------------

            function Create_Index return PAI.Object is
               Str_Lit : String_Literal_At;
               At_Lit  : Num_Literal;
            begin
               if Index.Kind = Gpr_Others_Designator then
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
                          (if At_Lit = No_Gpr_Node
                           then 0
                           else Unit_Index'Wide_Wide_Value
                             (At_Lit.Text))),
                     Is_Others      => False,
                     Case_Sensitive => False);
               end if;
            end Create_Index;

            I_Sloc : PAI.Object :=
                       (if Present (Index)
                        then Create_Index
                        else PAI.Undefined);

         begin
            if not I_Sloc.Is_Defined
             and then PRA.Exists (Q_Name)
             and then PRA.Get (Q_Name).Index_Type /= PRA.No_Index
            then
               if not Values.Indexed_Values.Filled then
                  Tree.Log_Messages.Append
                   (Message.Create
                      (Level   => Message.Error,
                       Sloc    => Sloc,
                       Message => "full associative array expression " &
                         "requires simple attribute reference"));

               elsif Values.Indexed_Values.Attribute_Name.Pack /= Pack_Name
               then
                  Tree.Log_Messages.Append
                   (Message.Create
                      (Level   => Message.Error,
                       Sloc    => Sloc,
                       Message => "not the same package as " &
                         Image (Pack_Name)));

               elsif Values.Indexed_Values.Attribute_Name.Attr /= N_Id then
                  Tree.Log_Messages.Append
                   (Message.Create
                      (Level   => Message.Error,
                       Sloc    => Sloc,
                       Message => "full associative array expression " &
                         "must reference the same attribute """ &
                         Image (N_Id) & '"'));

               else
                  for V of Values.Indexed_Values.Values loop
                     Create_And_Register_Attribute
                      (Index  => V.Index,
                       Values => V.Values,
                       Single => V.Single);
                  end loop;
               end if;

            elsif Values /= No_Values then
               if I_Sloc.Is_Defined and then PRA.Exists (Q_Name) then
                  I_Sloc.Set_Case
                    (PRA.Is_Case_Sensitive
                       (I_Sloc.Value, PRA.Get (Q_Name).Index_Type));
               end if;

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
                     Message => "variable """
                     & String (Get_Name_Type (F_Variable_Name (Var), 1, 1))
                     & """ must be a simple value"));
               if Pre_Conf_Mode then
                  Status := Over;
               end if;
            end if;
         end Parse_Case_Construction;

         ---------------------
         -- Parse_Case_Item --
         ---------------------

         procedure Parse_Case_Item (Node : Case_Item) is

            function Parser (Node : Gpr_Node'Class) return Visit_Status;

            Case_Value   : constant String := Case_Values.Last_Element;
            Is_That_Case : Boolean := False;

            ------------
            -- Parser --
            ------------

            function Parser (Node : Gpr_Node'Class) return Visit_Status is
            begin
               case Kind (Node) is
                  when Gpr_String_Literal =>
                     Is_That_Case :=
                       Unquote (To_UTF8 (Node.Text))
                         = Case_Value (2 .. Case_Value'Last);

                  when Gpr_Others_Designator =>
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
            P_Name : constant Package_Id :=
                       +Get_Name_Type (Name.As_Single_Tok_Node);

         begin
            --  Entering a package, set the state and parse the corresponding
            --  children.

            In_Pack := True;
            Pack_Name := P_Name;

            --  Make sure the package exists in view, and make Pack_Ref point
            --  to it.
            Packs.Include
              (P_Name,
               GPR2.Project.Pack.Object'
                 (Source_Reference.Pack.Object
                    (Source_Reference.Pack.Create
                       (Get_Source_Reference (Self.File, Node), P_Name)) with
                  PA.Set.Empty_Set, Project.Variable.Set.Empty_Set));
            Pack_Ref := Packs.Reference (P_Name).Element;

            Visit_Child (F_Pkg_Spec (Node));

            In_Pack   := False;
            Pack_Name := Project_Level_Scope;
            Pack_Ref  := null;

            --  Skip all nodes for this construct

            Status := Over;
         end Parse_Package_Decl;

         -----------------------------
         -- Parse_Package_Extension --
         -----------------------------

         procedure Parse_Package_Extension (Node : Package_Extension) is
            Sloc       : constant Source_Reference.Object :=
                           Get_Source_Reference (Self.File, Node);
            Values     : constant Identifier_List := F_Extended_Name (Node);
            Num_Childs : constant Positive := Children_Count (Values);
            Project    : constant Name_Type :=
                           (if Num_Childs > 1
                            then Get_Name_Type
                              (Values, Last => Num_Childs - 1)
                            else "?");
            P_Name     : constant Package_Id :=
                           +Get_Name_Type (Values, Num_Childs, Num_Childs);

            View       : constant GPR2.Project.View.Object :=
                           (if Num_Childs > 1
                            then Process.View.View_For (Project)
                            else GPR2.Project.View.Undefined);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Ref.Attrs.Clear;
            Pack_Ref.Vars.Clear;

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
                       "undefined project """ & String (Project) & '"'));

            elsif not View.Has_Package (P_Name) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Sloc    => Sloc,
                     Message =>
                       "undefined package """ &
                       String (Project) & '.' & Image (P_Name)
                       & '"'));

            else
               --  Then just copy the attributes into the current package

               Pack_Ref.Attrs := View.Raw_Attributes (P_Name);
               Pack_Ref.Vars  := View.Variables (Pack => P_Name);
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
            P_Name     : constant Package_Id :=
                           +Get_Name_Type (Values, Num_Childs, Num_Childs);

            View       : constant GPR2.Project.View.Object :=
                           (if Num_Childs > 1
                            then Process.View.View_For (Project)
                            else GPR2.Project.View.Undefined);
         begin
            --  Clear any previous value. This node is parsed as a child
            --  process of Parse_Package_Decl routine above.

            Pack_Ref.Attrs.Clear;
            Pack_Ref.Vars.Clear;

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
                       "undefined project """ & String (Project) & '"'));

            elsif not View.Has_Package (P_Name) then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level   => Message.Warning,
                     Sloc    => Sloc,
                     Message =>
                       "undefined package """ &
                       String (Project) & '.' & Image (P_Name) & '"'));

            else
               --  Then just copy the attributes into the current package

               Pack_Ref.Attrs := View.Raw_Attributes (P_Name);
               Pack_Ref.Vars  := View.Variables (Pack => P_Name);
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

                     --  Type definition from "parent" project

                     if not Type_Def.Is_Defined
                       and then Self.Has_Imports
                       and then Count (Self.Name, ".") > 0
                     then
                        declare
                           Prj_Id       : constant String := -Self.Name;
                           Dot_Position : Natural := Prj_Id'First;
                           I_Cursor     : GPR2.Project.Import.Set.Cursor;
                        begin
                           loop
                              for J in Dot_Position .. Prj_Id'Last loop
                                 Dot_Position := J;
                                 exit when Prj_Id (J) = '.';
                              end loop;

                              exit when Dot_Position = Prj_Id'Last;

                              I_Cursor := Self.Imports.Find
                                (Name_Type (Prj_Id (1 .. Dot_Position - 1)));
                              if GPR2.Project.Import.Set.Has_Element
                                                          (I_Cursor)
                              then
                                 Get_Type_Def_From
                                   (GPR2.Project.Import.Set.Element
                                      (I_Cursor));
                              end if;

                              exit when Type_Def.Is_Defined;
                           end loop;
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
                                    Message => "value """ & Value
                                    & """ is illegal for typed string """
                                    & Get_Value_Type (Single_Tok_Node (Name))
                                    & '"'));
                           end if;
                        end;

                     else
                        Tree.Log_Messages.Append
                          (Message.Create
                             (Level   => Message.Error,
                              Sloc    => Sloc,
                              Message =>
                                "expression for """
                                & Get_Value_Type (Single_Tok_Node (Name))
                                & """ must be a single string"));
                     end if;

                  else
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level   => Message.Error,
                           Sloc    => Get_Source_Reference (Self.File, V_Type),
                           Message =>
                             "unknown string type """ & String (T_Name) &
                             '"'));
                  end if;
               end;
            end if;

            if Values = No_Values then
               --  Do not report failure of evaluating the left-hand side if
               --  errors have already been reported: failure to get the actual
               --  value(s) is most certainly a direct consequence of the
               --  previous error.
               --
               --  Detecting such error without other explicit error is not
               --  expected, so this is just a safe guard, not expected to be
               --  covered by tests.

               if not Tree.Log_Messages.Has_Error
                 and then Non_Fatal_Error.Is_Empty
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Get_Source_Reference (Self.File, Name),
                        Message =>
                          "internal error evaluating the value for """ &
                          Get_Value_Type (Single_Tok_Node (Name)) &
                          '"'));
               end if;

               return;

            elsif Values.Single then
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
               Pack_Ref.Vars.Include (V.Name.Text, V);
            else
               Vars.Include (V.Name.Text, V);
            end if;
         end Parse_Variable_Decl;

         -----------------
         -- Visit_Child --
         -----------------

         procedure Visit_Child (Child : Gpr_Node) is
         begin
            if Present (Child) then
               Status := Traverse (Node => Child, Visit => Parser'Access);
            end if;
         end Visit_Child;

      begin
         if Is_Open then
            --  Handle all kind of nodes when the parsing is open

            case Kind (Node) is
               when Gpr_Attribute_Decl =>
                  Parse_Attribute_Decl (Node.As_Attribute_Decl);

               when Gpr_Variable_Decl =>
                  Parse_Variable_Decl (Node.As_Variable_Decl);

               when Gpr_Package_Decl =>
                  Parse_Package_Decl (Node.As_Package_Decl);

               when Gpr_Package_Renaming =>
                  Parse_Package_Renaming (Node.As_Package_Renaming);

               when Gpr_Package_Extension =>
                  Parse_Package_Extension (Node.As_Package_Extension);

               when Gpr_Case_Construction =>
                  Parse_Case_Construction (Node.As_Case_Construction);

               when Gpr_Case_Item =>
                  Parse_Case_Item (Node.As_Case_Item);

               when others =>
                  null;
            end case;

         else
            --  We are on a closed parsing mode, only handle case alternatives
            --  and Spec and Body attributes

            case Kind (Node) is
               when Gpr_Case_Construction =>
                  Parse_Case_Construction (Node.As_Case_Construction);

               when Gpr_Case_Item =>
                  Parse_Case_Item (Node.As_Case_Item);

               when Gpr_Attribute_Decl =>
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
         A    : PA.Object)
      is
         use type PRA.Value_Kind;
         use type PRA.Empty_Value_Status;

         Include : Boolean := True;
         Q_Name  : constant Q_Attribute_Id := A.Name.Id;
         Def     : PRA.Def;

      begin
         --  Check that a definition exists

         if not PRA.Exists (Q_Name) then
            if Q_Name.Pack = Project_Level_Scope
              or else PRP.Attributes_Are_Checked (Q_Name.Pack)
            then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Level => Message.Error,
                     Sloc  => Source_Reference.Object (A),
                     Message => "unrecognized attribute """ &
                                Image (Q_Name) & """"));
            end if;

            Include := False;

         --  Malformed attribute values can be side-effects of another
         --  error (such as missing variable). So only perform the next
         --  checks if there's no critical error.

         elsif not Tree.Log_Messages.Has_Error then

            --  Check value kind

            Def := PRA.Get (Q_Name);

            if Def.Value /= A.Kind then
               if Def.Value = PRA.Single then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level => Message.Error,
                        Sloc  => Source_Reference.Object (A),
                        Message => "attribute """ & Image (Q_Name) &
                                   """ expects a single value"));
               else
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level => Message.Error,
                        Sloc  => Source_Reference.Object (A),
                        Message => "attribute """ & Image (Q_Name) &
                                   """ expects a list of values"));
               end if;

               Include := False;

            elsif Def.Value = PRA.Single
              and then Def.Empty_Value in PRA.Error | PRA.Ignore
              and then Length (A.Value.Unchecked_Text) = 0
            then
               if Def.Empty_Value = PRA.Error then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => Source_Reference.Object (A.Value),
                        Message => "attribute """ & Image (Q_Name)
                                   & """ cannot be empty"));
               else
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Warning,
                        Sloc    => Source_Reference.Object (A.Value),
                        Message => "empty attribute """ & Image (Q_Name)
                                   & """ ignored"));
               end if;

               Include := False;
            end if;

            --  Check the attribute index

            case Def.Index_Type is
               when PRA.No_Index =>
                  if A.Has_Index then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Source_Reference.Object (A.Index),
                           Message => "attribute """ & Image (Q_Name) &
                                      """ does not expect an index"));
                     Include := False;
                  end if;

               when others =>
                  if not A.Has_Index then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Source_Reference.Object (A),
                           Message => "attribute """ & Image (Q_Name) &
                                      """ expects an index"));
                     Include := False;

                  elsif A.Index.Is_Others
                    and then not Def.Index_Optional
                  then
                     Tree.Log_Messages.Append
                       (Message.Create
                          (Level => Message.Error,
                           Sloc  => Source_Reference.Object (A),
                           Message => "'others' index not allowed with """ &
                                      Image (Q_Name) & """"));
                     Include := False;
                  end if;
            end case;
         end if;

         if Set.Contains (A) then
            declare
               Old : constant PA.Object := Set.Element (A.Name.Id.Attr,
                                                        A.Index);
            begin
               if Old.Is_Frozen then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level => Message.Error,
                        Sloc  => Source_Reference.Object (A),
                        Message => "cannot set configuration attribute """ &
                          Image (A.Name.Id) &
                          """ after it was referenced"));
                  Include := False;
               end if;
            end;
         end if;

         if Include then
            Set.Include (A);
         end if;
      end Record_Attribute;

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

      Is_Parsed_Project : constant Boolean := Self.Unit /= No_Analysis_Unit;

   begin
      if Is_Parsed_Project then
         Attrs.Clear;
         Vars.Clear;
         Packs.Clear;
      end if;

      --  Insert intrinsic attributes Name and Project_Dir

      declare
         use Characters.Handling;
         Sloc : constant Source_Reference.Object :=
                  Source_Reference.Object
                    (Source_Reference.Create (Self.File.Value, 0, 0));

         function Create_Attr
           (Name : Q_Attribute_Id) return Source_Reference.Attribute.Object
         is
           (Source_Reference.Attribute.Object
              (Source_Reference.Attribute.Create (Sloc, Name)));

      begin
         Attrs.Insert
           (PA.Create
              (Name    => Create_Attr (PRA.Name),
               Value   => Get_Value_Reference
                            (To_Lower (To_String (Self.Name)), Sloc),
               Default => True));

         Attrs.Insert
           (PA.Create
              (Name    => Create_Attr (PRA.Project_Dir),
               Value   => Get_Value_Reference (Self.File.Dir_Name, Sloc),
               Default => True));
      end;

      Types := Self.Types;

      if Is_Parsed_Project then
         Definition.Get (View).Disable_Cache;
         Traverse (Root (Self.Unit), Parser'Access);
         Definition.Get (View).Enable_Cache;
      end if;

      --  Fill possible non-fatal errors into the tree now

      for M of Non_Fatal_Error loop
         Tree.Log_Messages.Append (M);
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

end GPR2.Project.Parser;
