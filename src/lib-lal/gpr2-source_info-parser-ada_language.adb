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
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common;

with GNAT.Case_Util;

with GPR2.Project.Source;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info;
with GPR2.Source;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Identifier.Set;
with GPR2.Unit;

package body GPR2.Source_Info.Parser.Ada_Language is

   Handle : Object;

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object)
   is
      use GNAT;
      use Libadalang.Analysis;

      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Characters.Conversions;

      use Libadalang.Common;

      use Langkit_Support.Text;

      Ctx    : constant Analysis_Context := Create_Context;
      A_Unit : constant Analysis_Unit    :=
                 Get_From_File
                   (Ctx, Source.Path_Name.Value, Reparse => True);

      function To_String (T : Unbounded_Text_Type) return String is
        (To_String (To_Wide_Wide_String (T)));

      Index : Integer := 0;
      --  Source index, incremented every time we parse a compilation unit

      function Callback (Node : Ada_Node'Class) return Visit_Status;
      --  LibAdaLang parser's callback

      --------------
      -- Callback --
      --------------

      function Callback (Node : Ada_Node'Class) return Visit_Status is
         use all type GPR2.Unit.Flag;
      begin
         if Node = No_Ada_Node then
            return Over;
         end if;

         case Node.Kind is
            when Ada_Compilation_Unit =>
               if Node.As_Compilation_Unit.Is_Null then
                  return Over;
               end if;

               declare
                  function Process_Defining_Name
                    (N : Ada_Node'Class) return Unbounded_String;

                  ---------------------------
                  -- Process_Defining_Name --
                  ---------------------------

                  function Process_Defining_Name
                    (N : Ada_Node'Class) return Unbounded_String is
                  begin
                     case N.Kind is
                        when Ada_Identifier =>
                           return +To_UTF8 (N.Text);

                        when Ada_Dotted_Name =>
                           return Process_Defining_Name
                             (N.As_Dotted_Name.F_Prefix) & (+".") &
                           (+To_UTF8 (N.As_Dotted_Name.F_Suffix.Text));

                        when others =>
                           pragma Assert (False);
                           return +("");
                     end case;
                  end Process_Defining_Name;

                  U_Prelude  : constant Ada_Node_List :=
                                 Node.As_Compilation_Unit.F_Prelude;
                  U_Body     : constant Ada_Node :=
                                 Node.As_Compilation_Unit.F_Body;

                  U_Name        : Unbounded_String;
                  U_Sep_From    : Unbounded_String;
                  U_Kind        : Unit.Library_Unit_Type;
                  L_Type        : Unit.Library_Item_Type;
                  U_Withed      : Source_Reference.Identifier.Set.Object;
                  W_Found       : Containers.Name_Set;

                  --  ??? For now we don't parse those:
                  U_Main        : constant Unit.Main_Type :=
                                    Unit.None;
                  U_Flags       : Unit.Flags_Set :=
                                    Unit.Default_Flags;
               begin
                  if U_Prelude.Is_Null or else U_Body.Is_Null then
                     return Over;
                  end if;

                  --  Unit name, fully qualified.
                  --  The Unit and its parents are also the first direct
                  --  dependencies that we register.

                  declare
                     Is_First_Iteration : Boolean := True;
                  begin
                     for UN of Node.As_Compilation_Unit.
                       P_Syntactic_Fully_Qualified_Name
                     loop
                        if Is_First_Iteration then
                           U_Name := +To_String (UN);
                        else
                           U_Name := U_Name & (+".") & (+To_String (UN));
                        end if;

                        Is_First_Iteration := False;
                     end loop;
                  exception
                     when Property_Error =>
                        return Over;
                  end;

                  --  Get the direct dependencies, if any

                  for WC of U_Prelude loop
                     if WC.Kind = Ada_With_Clause then
                        for P of WC.As_With_Clause.F_Packages loop
                           --  We record this package and all parent if any
                           declare
                              Sloc : constant Source_Reference.Object :=
                                       Source_Reference.Object
                                         (Source_Reference.Create
                                           (Filename => Source.Path_Name.Value,
                                            Line     => Natural
                                              (WC.Sloc_Range.Start_Line),
                                            Column   => Natural
                                              (WC.Sloc_Range.Start_Column)));

                              procedure Register (Name : String);
                              --  Register Name and parent if any

                              --------------
                              -- Register --
                              --------------

                              procedure Register (Name : String) is
                                 subtype O
                                   is Source_Reference.Identifier.Object;

                                 N      : constant Name_Type :=
                                            Name_Type (Name);
                                 B_Name : constant String :=
                                            Directories.Base_Name (Name);
                                 Item   : constant O :=
                                            O (Source_Reference.Identifier.
                                                Create
                                                 (Sloc, Name_Type (Name)));
                              begin
                                 if not W_Found.Contains (N) then
                                    U_Withed.Insert (Item);
                                    W_Found.Include (N);
                                 end if;

                                 if B_Name /= Name then
                                    Register (B_Name);
                                 end if;
                              end Register;

                           begin
                              Register (-(Process_Defining_Name (P)));
                           end;
                        end loop;
                     end if;
                  end loop;

                  --  Unit kind

                  case U_Body.Kind is
                     when Ada_Library_Item =>
                        case Node.As_Compilation_Unit.P_Unit_Kind is
                           when Analysis_Unit_Kind'(Unit_Specification) =>
                              U_Kind := Unit.S_Spec;
                           when Analysis_Unit_Kind'(Unit_Body)          =>
                              U_Kind := Unit.S_Body;
                        end case;

                        case U_Body.As_Library_Item.F_Item.Kind is
                           when Ada_Generic_Package_Decl =>
                              U_Flags (Is_Generic) := True;
                              L_Type := Unit.Is_Package;

                           when Ada_Generic_Subp_Decl =>
                              U_Flags (Is_Generic) := True;
                              L_Type := Unit.Is_Subprogram;

                           when Ada_Subp_Decl
                              | Ada_Null_Subp_Decl
                              | Ada_Subp_Body
                              | Ada_Generic_Subp_Instantiation
                              =>
                              L_Type := Unit.Is_Subprogram;

                           when Ada_Package_Decl
                              | Ada_Package_Body
                              | Ada_Generic_Package_Instantiation
                              =>
                              L_Type := Unit.Is_Package;

                           when others =>
                              pragma Assert
                                (False,
                                 U_Body.As_Library_Item.F_Item.Kind'Img);
                        end case;

                     when Ada_Subunit =>
                        U_Kind := Unit.S_Separate;
                        U_Sep_From := Process_Defining_Name
                                        (U_Body.As_Subunit.F_Name);

                        case U_Body.As_Subunit.F_Body.Kind is
                           when Ada_Package_Body =>
                              L_Type := Unit.Is_Package;
                           when Ada_Subp_Body =>
                              L_Type := Unit.Is_Subprogram;
                           when others =>
                              pragma Assert
                                (False, U_Body.As_Subunit.F_Body.Kind'Img);
                        end case;

                        pragma Assert
                          (Length (U_Name) > Length (U_Sep_From) + 1);

                     when others =>
                        pragma Assert (False);
                  end case;

                  declare
                     Unit_Name : String := -U_Name;
                  begin
                     Case_Util.To_Mixed (Unit_Name);

                     for I in Unit_Name'First + 1 .. Unit_Name'Last loop
                        if Unit_Name (I - 1) = '.' then
                           Unit_Name (I) := Case_Util.To_Upper (Unit_Name (I));
                        end if;
                     end loop;

                     U_Name := +Unit_Name;
                  end;

                  --  Construct the unit and add it to "Found"

                  Index := Index + 1;

                  declare
                     CU : constant Unit.Object :=
                            Unit.Create
                              (Name          => Name_Type (-U_Name),
                               Index         => Index,
                               Main          => U_Main,
                               Flags         => U_Flags,
                               Lib_Unit_Kind => U_Kind,
                               Lib_Item_Kind => L_Type,
                               Dependencies  => U_Withed,
                               Sep_From      =>
                                 Optional_Name_Type (-U_Sep_From));
                  begin
                     --  Kind of first unit is also recorded in Data.Kind

                     if Index = 1 then
                        Data.Kind := U_Kind;
                     end if;

                     Data.CU_List.Append (CU);
                  end;
               end;

               return Over;

            when others =>
               return Into;
         end case;
      end Callback;

   begin
      Data.Clear;

      Traverse (A_Unit.Root, Callback'Access);

      Data.Parsed := Source_Info.Source;
      Data.Is_Ada := True;
   end Compute;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister is
   begin
      Parser.Registry.Unregister (Handle);
   end Unregister;

begin
   Parser.Registry.Register (Handle);
end GPR2.Source_Info.Parser.Ada_Language;
