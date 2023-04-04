--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;

with Gpr_Parser_Support.Text;

with Gpr_Parser.Analysis;
with Gpr_Parser.Common;

with GPR2.Containers;
with GPR2.Source_Reference.Identifier.Set;

package body GPR2.Build.Source.Ada_Parser is

   -------------
   -- Compute --
   -------------

   procedure Compute
     (Data             : in out Source.Object'Class;
      Get_Withed_Units : Boolean)
   is
      use Gpr_Parser.Analysis;
      use Gpr_Parser.Common;
      use Gpr_Parser_Support.Text;

      Index : Unit_Index := No_Index;
      --  Source index, incremented every time we parse a compilation unit

      U_Withed : GPR2.Source_Reference.Identifier.Set.Object;
      W_Found  : Containers.Name_Set;

      Parsed   : Boolean := False;
      --  If something has been parsed on this source file, then Parsed is set
      --  to True and New_Data will replace Data parameter.

      function Callback (Node : Gpr_Node'Class) return Visit_Status;
      --  LibAdaLang parser's callback

      --------------
      -- Callback --
      --------------

      function Callback (Node : Gpr_Node'Class) return Visit_Status is
         --  use all type GPR2.Unit.Flag;

         function Process_Defining_Name
           (N     : Gpr_Node'Class;
            Index : Natural := 0;
            Count : Positive := 1) return Unbounded_String;

         procedure Register
           (Name         : String;
            Sloc         : GPR2.Source_Reference.Object;
            Parents_Only : Boolean := False);
         --  Register Name and parent if any

         ---------------------------
         -- Process_Defining_Name --
         ---------------------------

         function Process_Defining_Name
           (N     : Gpr_Node'Class;
            Index : Natural := 0;
            Count : Positive := 1) return Unbounded_String
         is
         begin
            case N.Kind is
               when Gpr_Prefix =>
                  declare
                     P : constant Prefix := N.As_Prefix;
                  begin
                     return Process_Defining_Name (P.F_Prefix)
                       & "." & Process_Defining_Name (P.F_Suffix);
                  end;

               when Gpr_Identifier =>
                  return +To_UTF8 (N.Text);

               when Gpr_Expr_List =>
                  pragma Assert (N.As_Expr_List.Children_Count = Count);
                  pragma Assert (N.As_Expr_List.Children_Count > Index);

                  return Process_Defining_Name
                    (N.As_Expr_List.Child
                       (N.As_Expr_List.First_Child_Index + Index));

               when others =>
                  raise Constraint_Error
                    with "unexpected node in process_defining_name";
            end case;
         end Process_Defining_Name;

         --------------
         -- Register --
         --------------

         procedure Register
           (Name         : String;
            Sloc         : GPR2.Source_Reference.Object;
            Parents_Only : Boolean := False)
         is
            subtype O is GPR2.Source_Reference.Identifier.Object;

            N      : constant Name_Type := Name_Type (Name);
            B_Name : constant String :=
                       Directories.Base_Name (Name);
            Item   : constant O :=
                       O (GPR2.Source_Reference.Identifier.Create
                          (Sloc, Name_Type (Name)));
            Position : Containers.Name_Type_Set.Cursor;
            Inserted : Boolean;
         begin
            if not Parents_Only then
               W_Found.Insert (N, Position, Inserted);

               if Inserted then
                  U_Withed.Insert (Item);
               end if;
            end if;

            if B_Name /= Name then
               Register (B_Name, Sloc);
            end if;
         end Register;

      begin
         if Node = No_Gpr_Node then
            return Over;
         end if;

         case Node.Kind is
            when Gpr_Ada_With =>
               if Get_Withed_Units then
                  declare
                     N    : constant Ada_With := Node.As_Ada_With;
                     Sloc : constant GPR2.Source_Reference.Object :=
                              GPR2.Source_Reference.Object
                                (GPR2.Source_Reference.Create
                                   (Filename => Data.Path_Name.Value,
                                    Line     => Natural
                                      (Node.Sloc_Range.Start_Line),
                                    Column   => Natural
                                      (Node.Sloc_Range.Start_Column)));

                  begin
                     for I in 0 .. N.F_Packages.As_Expr_List.Children_Count - 1
                     loop
                        Register (-(Process_Defining_Name (N.F_Packages, I,
                                  N.F_Packages.As_Expr_List.Children_Count)),
                                  Sloc);

                     end loop;
                  end;
               end if;

               return Over;

            when Gpr_Ada_Library_Item =>
               --  As the parser only support a single unit per file the
               --  index should always be 1.
               pragma Assert (Index = No_Index);

               declare
                  N          : constant Ada_Library_Item :=
                                 Node.As_Ada_Library_Item;
                  Sloc       : constant GPR2.Source_Reference.Object :=
                                 GPR2.Source_Reference.Object
                                   (GPR2.Source_Reference.Create
                                      (Filename => Data.Path_Name.Value,
                                       Line     => Natural
                                         (Node.Sloc_Range.Start_Line),
                                       Column   => Natural
                                         (Node.Sloc_Range.Start_Column)));
                  --  U_Main  : constant GPR2.Unit.Main_Type := GPR2.Unit.None;
                  U_Name     : Unbounded_String;
                  U_Sep_Name : Unbounded_String;
                  --  U_Flags : GPR2.Unit.Flags_Set := GPR2.Unit.Default_Flags;
                  U_Kind     : Unit_Kind;
                  --  L_Type     : GPR2.Unit.Library_Item_Type;
               begin
                  --  Check generic

                  --  U_Flags (Is_Generic) := not N.F_Generic_Stub.Is_Null;

                  --  Check Spec/Body

                  case N.F_Main.Kind is
                     when Gpr_Ada_Pkg =>
                        --  L_Type := GPR2.Unit.Is_Package;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Pkg.F_Name);
                        U_Kind := S_Spec;

                     when Gpr_Ada_Pkg_Body =>
                        --  L_Type := GPR2.Unit.Is_Package;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Pkg_Body.F_Name);
                        U_Kind := S_Body;

                     when Gpr_Ada_Subp =>
                        --  Keep the unit kind information. Indeed,
                        --  the Ada parser do not make the difference
                        --  between the a procedure spec or body but
                        --  this information has been already computed
                        --  based on the naming scheme while adding
                        --  the files to the view.

                        if Index = No_Index then
                           if Data.Is_Defined
                             and then Data.Has_Unit_At (No_Index)
                           then
                              U_Kind := Data.CU_List (No_Index).Kind;
                           else
                              U_Kind := Data.Kind;
                           end if;
                        end if;

                        --  L_Type := GPR2.Unit.Is_Subprogram;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Subp.F_Name);

                     when others =>
                        pragma Assert (False);
                  end case;

                  --  If this is a child package, we register the
                  --  parent package(s) as visible.

                  if N.F_Separate.Is_Null then
                     Register (-U_Name, Sloc, True);
                  end if;

                  --  Check separate

                  if not N.F_Separate.Is_Null then
                     U_Kind := S_Separate;

                     U_Sep_Name :=  U_Name;
                     U_Name :=
                       Process_Defining_Name (N.F_Separate.F_Parent_Name);
                  end if;

                  --  Construct the unit

                  declare
                     CU : constant Source.Unit_Part :=
                            Source.Create
                              (Unit_Name       => Name_Type (-U_Name),
                               Index           => Index,
                               Kind            => U_Kind,
                               Kind_Ambiguous  => False,
                               Separate_Name   =>
                                 Optional_Name_Type (-U_Sep_Name));
                  begin
                     --  Kind of first unit is also recorded in Data.Kind

                     if Index = No_Index then
                        Data.Kind := U_Kind;
                     end if;

                     --  Note that we do not append but actually replace the
                     --  new unit created with all information (dependencies,
                     --  generic, etc...).

                     Data.CU_List.Units.Replace (Index, CU);

                     Parsed := True;
                  end;
               end;

               --  Note that this parser supports only a single unit per
               --  file. So only index 1 will be used. But this is made
               --  so to support a full parser if one is implemented.

               Index := Index + 1;

               return Over;

            when others =>
               return Into;
         end case;
      end Callback;

      Ctx    : constant Analysis_Context := Create_Context;
      A_Unit : constant Analysis_Unit    :=
                 Get_From_File
                   (Ctx, Data.Path_Name.Value,
                    Rule    => Ada_Prelude_Rule,
                    Reparse => True);

   begin
      if not A_Unit.Root.Is_Null then
         Traverse (A_Unit.Root, Callback'Access);
      end if;

      if Parsed then
         Data.Language := GPR2.Ada_Language;
      end if;
   end Compute;

end GPR2.Build.Source.Ada_Parser;
