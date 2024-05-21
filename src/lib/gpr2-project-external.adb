--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Conversions;
with Ada.Containers;

with Gpr_Parser.Common;
with Gpr_Parser.Analysis;
with Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Text;

with GPR2.Message;
with GPR2.View_Internal;
with GPR2.Project.External.Set;
with GPR2.Project_Parser;
with GPR2.Tree_Internal;

package body GPR2.Project.External is

   use Gpr_Parser.Common;
   use Gpr_Parser.Analysis;
   use Gpr_Parser_Support.Text;
   use Gpr_Parser_Support.Slocs;

   package PP renames GPR2.Project_Parser;

   -----------------------
   -- Externals --
   -----------------------

   function Externals
     (Tree      : GPR2.Project.Tree.Object;
      Root_Only : Boolean := False)
      return External_Arr
   is
      procedure Assign_Type_To_External
         (Ext             : GPR2.Project.External.Set.Set.Reference_Type;
          Typ             : Identifier_List;
          Assignment_Sloc : GPR2.Source_Reference.Object;
          Parser          : PP.Object);
      --  Assign type to external. If assigned type may be conflicting with
      --  current external types, then the external is tagged as conflicting.
      --  All type assignments are tracked so their position in source code
      --  can be printed in case of conflicts.

      procedure Parse_Simple_Typed_Externals
        (Parser    : PP.Object;
         Externals : in out GPR2.Project.External.Set.Object);
      --  Parse typed externals, and update them in "Externals"

      procedure Populate_Externals
         (Exts            : in out GPR2.Project.External.Set.Object;
         Parsed_Externals : PP.Externals_Map;
         Parser           : PP.Object);
      --  Populate externals with explicit typed externals and untyped
      --  externals parsed during the semantic parsing stage of
      --  GPR2.Project.Parser parser.

      procedure Log_Warn_For_Conflicting_Ext (Ext : Object)
        with Pre => Ext.Is_Conflicting;
      --  Add warning messages to the logs explaining that the specified
      --  typed external is conflicting, and lists all the types assigned to
      --  it.

      -----------------------------
      -- Assign_Type_To_External --
      -----------------------------

      procedure Assign_Type_To_External
         (Ext             : GPR2.Project.External.Set.Set.Reference_Type;
          Typ             : Identifier_List;
          Assignment_Sloc : GPR2.Source_Reference.Object;
          Parser          : PP.Object)
      is
         Type_Def : constant GPR2.Project.Typ.Object :=
                      Parser.Type_Definition_From
                        (Tree_Internal.Get (Tree).all, Typ);
      begin
         --  Type_Def can not be undefined at this stage.
         --  Otherwise, the previous parsing stages would
         --  have failed.

         if Ext.Is_Typed then

            --  If external is already tagged as typed, union of
            --  all assigned types values are used as external
            --  possible values. If values differ between types,
            --  the external is tagged as conflicting.

            for T of Type_Def.Values loop
               declare
                  T_Unbounded : constant Unbounded_String :=
                                  To_Unbounded_String (String (T.Text));
               begin
                  if not Ext.Possible_Values.Contains (T_Unbounded) then
                     Ext.Conflicting := True;
                  end if;

                  Ext.Possible_Values.Include (T_Unbounded);
               end;
            end loop;

         else
            Ext.Typed := True;

            for T of Type_Def.Values loop
               Ext.Possible_Values.Include
                 (To_Unbounded_String (String (T.Text)));
            end loop;
         end if;

         --  Add the type assignement source location
         --  to the external. It is useful if the external
         --  is conflicting. In this case, a warning is raised,
         --  with all these references, so the user can easily
         --  know which type cause the ambiguity.

         Ext.Types_Assignments.Append
           ((Typ             => Type_Def,
             Assignment_Sloc => Assignment_Sloc));
      end Assign_Type_To_External;

      ----------------------------------
      -- Log_Warn_For_Conflicting_Ext --
      ----------------------------------

      procedure Log_Warn_For_Conflicting_Ext (Ext : Object) is
         Msg               : Unbounded_String;
         First_Assign_Sloc : constant GPR2.Source_Reference.Object :=
                               Ext.Types_Assignments.
                                 First_Element.Assignment_Sloc;

      begin
         Msg := "set of values for " & Ext.Name & " is conflicting";

         Tree.Log_Messages.Append
           (Message.Create
              (Level   => Message.Warning,
               Sloc    => First_Assign_Sloc,
               Message => To_String (Msg)));

         for Type_Assign of Ext.Types_Assignments loop
            Msg := To_Unbounded_String
              ("  type " & String (Type_Assign.Typ.Name.Text)
               & " is defined at " & Type_Assign.Typ.Format
               & " and used at " & Type_Assign.Assignment_Sloc.Format);

            Tree.Log_Messages.Append
              (Message.Create
                 (Level   => Message.Warning,
                  Sloc    => First_Assign_Sloc,
                  Message => To_String (Msg)));
         end loop;
      end Log_Warn_For_Conflicting_Ext;


      ----------------------------------
      -- Parse_Simple_Typed_Externals --
      ----------------------------------

      procedure Parse_Simple_Typed_Externals
        (Parser    : PP.Object;
         Externals : in out GPR2.Project.External.Set.Object)
      is
         function Internal (Node : Gpr_Node'Class) return Visit_Status;
         --  Internal function provided to Traverse to parse each node

         --------------
         -- Internal --
         --------------

         function Internal (Node : Gpr_Node'Class) return Visit_Status is
            Status : Visit_Status := Into;

            procedure Parse_Variable_Decl (Node : Variable_Decl);
            --  Parse variable declaration. If the variable value is a simple
            --  typed external, then it is added to Typed_Exts_Map.

            -------------------------
            -- Parse_Variable_Decl --
            -------------------------

            procedure Parse_Variable_Decl (Node : Variable_Decl) is

               Expr   : constant Term_List      := F_Expr (Node);
               V_Type : constant Type_Reference := F_Var_Type (Node);

               function Get_String_Literal
                 (N : Gpr_Node'Class; Error : out Boolean) return Value_Type;
               --  Return the string literal of the Node N

               function Get_Value_Type
                 (Node : Single_Tok_Node'Class) return Value_Type;
               --  Return value type of a node. Does not remove double quotes

               procedure Parse_External
                 (BFC : Builtin_Function_Call; Type_Node : Identifier_List);
               --  Fill Typed_Exts_Map with simple typed externals. If an
               --  external was already in this list before the parsing and
               --  becomes untyped, then it is removed from the map.

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

                  function Parser
                    (Node : Gpr_Node'Class) return Visit_Status
                  is
                     function Handle_String
                       (Node : String_Literal) return Unbounded_String;
                     --  A simple static string

                     -------------------
                     -- Handle_String --
                     -------------------

                     function Handle_String
                       (Node : String_Literal) return Unbounded_String is
                     begin
                        return
                          To_Unbounded_String
                            (Unquote (Value_Type (To_UTF8 (Node.Text))));
                     end Handle_String;

                  begin
                     --  Only Gpr_String_Literal, Gpr_String_Literal_At
                     --  and Gpr_Base_List nodes can appear here. Other
                     --  cases have already been detected as an error
                     --  during the first stage parsing.

                     if Kind (Node) = Gpr_String_Literal then
                        Result := Handle_String (Node.As_String_Literal);
                     end if;

                     return Into;
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

                  V : constant Wide_Wide_String := Text (Node);

               begin
                  return To_String (V (V'First .. V'Last));
               end Get_Value_Type;

               --------------------
               -- Parse_External --
               --------------------

               procedure Parse_External
                 (BFC : Builtin_Function_Call; Type_Node : Identifier_List)
               is
                  function External_Variable_Name
                    (Node  : Builtin_Function_Call;
                     Error : out Boolean) return Value_Type;
                  --  Returns the external variable name. It can be the first,
                  --  or the second field parameter, depending on
                  --  if the type is present.

                  ----------------------------
                  -- External_Variable_Name --
                  ----------------------------

                  function External_Variable_Name
                    (Node  : Builtin_Function_Call;
                     Error : out Boolean) return Value_Type
                  is
                     Exprs : constant Term_List_List :=
                               F_Terms (F_Parameters (Node));

                     function External_Explicit_Type return Identifier_List;
                     --  Returns the external type if present.
                     --  The type is an optional first field parameter.

                     ----------------------------
                     -- External_Explicit_Type --
                     ----------------------------

                     function External_Explicit_Type return Identifier_List is
                        Exprs : constant Term_List_List :=
                                  F_Terms (F_Parameters (Node));
                     begin
                        pragma Assert (Exprs.Children_Count > 1);

                        --  HACK: We do not parse the type as a type reference,
                        --  but a variable reference. The parser can not
                        --  distinguish between a type and a variable in
                        --  in its current state, as both begin similarly.

                        declare
                           Typ : constant Term_List :=
                                   Exprs.First_Child.As_Term_List;
                        begin
                           if Kind (Typ.First_Child)
                             = Gpr_Variable_Reference
                           then
                              return As_Variable_Reference
                                (Typ.First_Child).F_Variable_Name;
                           else
                              return No_Identifier_List;
                           end if;
                        end;
                     end External_Explicit_Type;

                     Var       : Term_List;
                     Var_Index : Natural;

                  begin
                     --  Type is optional, and is the first external parameter

                     if External_Explicit_Type = No_Identifier_List then
                        Var_Index := 1;
                     else
                        Var_Index := 2;
                     end if;

                     Var := Exprs.Child (Var_Index).As_Term_List;

                     return Get_String_Literal (Var, Error);
                  end External_Variable_Name;

                  Parameters         : constant Term_List_List     :=
                                         F_Terms (F_Parameters (BFC));
                  Error              : Boolean;
                  Ext_Name           : constant External_Name_Type :=
                                         External_Name_Type
                                           (External_Variable_Name
                                              (BFC, Error));
                  Default_Value_Node : constant Term_List          :=
                                         Last_Child (Parameters).As_Term_List;

               begin
                  Status := Over;

                  --  Avoid cases like:
                  --  Var : My_Type := external ("EXT", "default") & "value".

                  if Parent (BFC).Children_Count > 1 then
                     return;
                  end if;

                  declare
                     Ext      : constant GPR2.Project.External.
                                  Set.Set.Reference_Type :=
                                    Externals.Reference (Ext_Name);
                     Type_Def : constant GPR2.Project.Typ.Object :=
                                  Parser.Type_Definition_From
                                    (Tree_Internal.Get (Tree).all, Type_Node);

                  begin
                     --  Type_Def can not be undefined at this stage.
                     --  Otherwise, the previous parsing stages would
                     --  have failed.

                     if Ext.Is_Typed then
                        --  If external is already tagged as typed, union of
                        --  all assigned types values are used as external
                        --  possible values. If values differ between types,
                        --  the external is tagged as conflicting.

                        for T of Type_Def.Values loop
                           declare
                              T_Unbounded : constant Unbounded_String :=
                                              To_Unbounded_String
                                                (String (T.Text));
                           begin
                              if not Ext.Possible_Values.
                                 Contains (T_Unbounded)
                              then
                                 Ext.Conflicting := True;
                              end if;

                              Ext.Possible_Values.Include (T_Unbounded);
                           end;
                        end loop;

                     else
                        Ext.Typed := True;

                        for T of Type_Def.Values loop
                           Ext.Possible_Values.Include
                             (To_Unbounded_String (String (T.Text)));
                        end loop;
                     end if;

                     --  Add the type assignement source location
                     --  to the external. It is useful if the external
                     --  is conflicting. In this case, a warning is raised,
                     --  with all these references, so the user can easily
                     --  know which type cause the ambiguity.

                     declare
                        Slr : constant Source_Location_Range :=
                                Sloc_Range (Parameters);

                     begin
                        Ext.Types_Assignments.Append
                          ((Typ              => Type_Def,
                            Assignment_Sloc  => Source_Reference.Object
                              (Source_Reference.Create
                                (Parser.Path_Name.Value,
                                 Positive (Slr.Start_Line),
                                 Positive (Slr.Start_Column)))));
                     end;
                  end;

                  --  Process recursively externals as default value.
                  --  Example:
                  --    Var : My_Type := External ("EXT1",
                  --                  External (EXT2, "default"));
                  --  EXT1 and EXT2 are typed externals.
                  --
                  --    My_Type := External ("EXT1",
                  --            External (EXT2, "default") & "suffix");
                  --  EXT1 is typed, while EXT2 is untyped.

                  if First_Child (Default_Value_Node).Kind =
                    Gpr_Builtin_Function_Call
                  then
                     declare
                        Default_Val_BFC : constant Builtin_Function_Call :=
                                            First_Child (Default_Value_Node)
                                            .As_Builtin_Function_Call;
                        Function_Name   : constant Name_Type             :=
                                            Name_Type
                                              (Get_Value_Type
                                                 (F_Function_Name (BFC)));
                     begin

                        --  Reuse the same type as for the previously
                        --  parsed external.

                        if Function_Name = "external" then
                           Parse_External (Default_Val_BFC, Type_Node);
                        end if;
                     end;
                  end if;
               end Parse_External;

            begin
               --  Only typed externals are parsed. Untyped externals
               --  will be the externals whose names are only in the
               --  list obtained from the first stage parsing
               --  (Parse_Stage_1).

               if not V_Type.Is_Null then
                  if First_Child (Expr).Kind = Gpr_Builtin_Function_Call then
                     declare
                        BFC           : constant Builtin_Function_Call :=
                                          First_Child (Expr).
                                            As_Builtin_Function_Call;
                        Function_Name : constant Name_Type             :=
                                          Name_Type
                                            (Get_Value_Type
                                               (F_Function_Name (BFC)));

                     begin
                        --  Only simple cases like
                        --  - external (EXT, "default variable");
                        --  or
                        --  - external (EXT, external (...));
                        --   are processed.

                        if Function_Name = "external" then
                           declare
                              Type_Node : constant Identifier_List :=
                                            F_Var_Type_Name (V_Type);
                           begin
                              Parse_External (BFC, Type_Node);
                           end;
                        end if;
                     end;

                  else
                     Status := Over;
                  end if;
               end if;
            end Parse_Variable_Decl;

         begin
            if Kind (Node) = Gpr_Variable_Decl then
               Parse_Variable_Decl (Node.As_Variable_Decl);
            end if;

            return Status;
         end Internal;

      begin
         --  Do not try to parse projects that have been created via the
         --  View_Builder API.

         if Parser.Unit /= No_Analysis_Unit then
            Traverse (Root (Parser.Unit), Internal'Access);
         end if;
      end Parse_Simple_Typed_Externals;

      ------------------------
      -- Populate_Externals --
      ------------------------

      procedure Populate_Externals
        (Exts             : in out GPR2.Project.External.Set.Object;
         Parsed_Externals : PP.Externals_Map;
         Parser           : PP.Object) is
      begin
         for C in Parsed_Externals.Iterate loop
            declare
               Name : constant External_Name_Type :=
                        PP.Externals_Map_Package.Key (C);
            begin
               if not Exts.Contains (Name) then
                  Exts.Include
                    (Name,
                      (Name              =>
                           To_Unbounded_String (String (Name)),
                       Typed             => False,
                       Conflicting       => False,
                       Possible_Values   => <>,
                       Types_Assignments => <>));
               end if;

               for Parsed_Ext of Parsed_Externals (C) loop
                  --  Assign type for explicitly typed externals

                  if Parsed_Ext.Type_Node /= No_Identifier_List then
                     Assign_Type_To_External
                       (Exts.Reference (Name),
                        Parsed_Ext.Type_Node,
                        Parsed_Ext.Source_Ref, Parser);
                  end if;
               end loop;
            end;

         end loop;
      end Populate_Externals;

      Def       : constant View_Internal.Ref :=
                    View_Internal.Get (Tree.Root_Project);
      Externals : GPR2.Project.External.Set.Object;

   begin
      if Root_Only then
         Populate_Externals
           (Externals,
            Def.Trees.Project.Externals,
            Def.Trees.Project);
         Parse_Simple_Typed_Externals (Def.Trees.Project, Externals);

      else
         for V of Tree.Ordered_Views loop
            declare
               Parser      : constant PP.Object :=
                               View_Internal.Get_RO (V).Trees.Project;
               Parsed_Exts : constant PP.Externals_Map := Parser.Externals;

            begin
               Populate_Externals (Externals, Parsed_Exts, Parser);
               Parse_Simple_Typed_Externals (Parser, Externals);
            end;
         end loop;
      end if;

      declare
         Exts  : External_Arr (1 .. Integer (Externals.Length));
         Index : Positive := 1;
      begin
         for Ext of Externals loop
            if Ext.Is_Conflicting then
               Log_Warn_For_Conflicting_Ext (Ext);
            end if;

            Exts (Index) := Ext;
            Index := Index + 1;
         end loop;

         return Exts;
      end;
   end Externals;

   --------------------
   -- Is_Conflicting --
   --------------------

   function Is_Conflicting (Ext : Object) return Boolean is
   begin
      return Ext.Conflicting;
   end Is_Conflicting;

   --------------
   -- Is_Typed --
   --------------

   function Is_Typed (Ext : Object) return Boolean is
   begin
      return Ext.Typed;
   end Is_Typed;

   ----------
   -- Name --
   ----------

   function Name (Ext : Object) return String is
   begin
      return To_String (Ext.Name);
   end Name;

   ------------------------
   -- Possible_Values_Of --
   ------------------------

   function Possible_Values_Of
     (Ext  : Object) return Unbounded_String_Array
   is
      Values : Unbounded_String_Array
                 (1 .. Integer (Ext.Possible_Values.Length));
      Index  : Positive := 1;

   begin
      for E of Ext.Possible_Values loop
         Values (Index) := E;
         Index          := Index + 1;
      end loop;

      return Values;
   end Possible_Values_Of;

end GPR2.Project.External;
