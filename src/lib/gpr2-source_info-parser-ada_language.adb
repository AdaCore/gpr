------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with Ada.Directories;

with GNAT.Case_Util;

with Langkit_Support.Text;

with GPR_Parser.Analysis;
with GPR_Parser.Common;

with GPR2.Project.Source;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier.Set;
with GPR2.Source_Reference.Identifier;
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
      use GPR_Parser.Analysis;
      use GPR_Parser.Common;
      use Langkit_Support.Text;

      Index : Natural := 0;
      --  Source index, incremented every time we parse a compilation unit

      U_Withed : Source_Reference.Identifier.Set.Object;
      W_Found  : Containers.Name_Set;

      function Callback (Node : GPR_Node'Class) return Visit_Status;
      --  LibAdaLang parser's callback

      function Capitalize_Unit_Name (Name : String) return Name_Type;
      --  Use mixed-case for the unit name stored into the unit object

      --------------
      -- Callback --
      --------------

      function Callback (Node : GPR_Node'Class) return Visit_Status is
         use all type GPR2.Unit.Flag;

         function Process_Defining_Name
           (N : GPR_Node'Class) return Unbounded_String;

         ---------------------------
         -- Process_Defining_Name --
         ---------------------------

         function Process_Defining_Name
           (N : GPR_Node'Class) return Unbounded_String is
         begin
            case N.Kind is
               when GPR_Prefix =>
                  declare
                     P : constant Prefix := N.As_Prefix;
                  begin
                     return Process_Defining_Name (P.F_Prefix)
                       & "." & Process_Defining_Name (P.F_Suffix);
                  end;

               when GPR_Identifier =>
                  return +To_UTF8 (N.Text);

               when GPR_Expr_List =>
                  pragma Assert (N.As_Expr_List.Children_Count = 1);

                  return Process_Defining_Name
                    (N.As_Expr_List.Child (N.As_Expr_List.First_Child_Index));

               when others =>
                  raise Constraint_Error
                    with "unexpected node in process_defining_name";
            end case;
         end Process_Defining_Name;

      begin
         if Node = No_GPR_Node then
            return Over;
         end if;

         case Node.Kind is
            when GPR_Ada_With =>
               declare
                  N    : constant Ada_With := Node.As_Ada_With;
                  Sloc : constant Source_Reference.Object :=
                           Source_Reference.Object
                             (Source_Reference.Create
                                (Filename => Source.Path_Name.Value,
                                 Line     => Natural
                                   (Node.Sloc_Range.Start_Line),
                                 Column   => Natural
                                   (Node.Sloc_Range.Start_Column)));

                  procedure Register (Name : String);
                  --  Register Name and parent if any

                  --------------
                  -- Register --
                  --------------

                  procedure Register (Name : String) is
                     subtype O is Source_Reference.Identifier.Object;

                     N      : constant Name_Type := Name_Type (Name);
                     B_Name : constant String :=
                                Directories.Base_Name (Name);
                     Item   : constant O :=
                                O (Source_Reference.Identifier.Create
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
                  Register (-(Process_Defining_Name (N.F_Packages)));
               end;

               return Over;

            when GPR_Ada_Library_Item =>
               declare
                  N          : constant Ada_Library_Item :=
                                 Node.As_Ada_Library_Item;
                  U_Main     : constant Unit.Main_Type := Unit.None;
                  U_Name     : Unbounded_String;
                  U_Sep_From : Unbounded_String;
                  U_Flags    : Unit.Flags_Set := Unit.Default_Flags;
                  U_Kind     : Unit.Library_Unit_Type;
                  L_Type     : Unit.Library_Item_Type;
               begin
                  --  Check generic

                  U_Flags (Is_Generic) := not N.F_Generic_Stub.Is_Null;

                  --  Check separate

                  if not N.F_Separate.Is_Null then
                     U_Kind := Unit.S_Separate;

                     U_Sep_From :=
                       Process_Defining_Name (N.F_Separate.F_Parent_Name);
                  end if;

                  --  Check Spec/Body

                  case N.F_Main.Kind is
                     when GPR_Ada_Pkg =>
                        L_Type := Unit.Is_Package;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Pkg.F_Name);
                        U_Kind := Unit.S_Spec;

                     when GPR_Ada_Pkg_Body =>
                        L_Type := Unit.Is_Package;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Pkg_Body.F_Name);
                        U_Kind := Unit.S_Body;

                     when GPR_Ada_Subp =>
                        L_Type := Unit.Is_Subprogram;
                        U_Name := Process_Defining_Name
                          (N.F_Main.As_Ada_Subp.F_Name);
                        U_Kind := Unit.S_Body;

                     when others =>
                        pragma Assert (False);
                  end case;

                  --  Use mixed-casing for the unit name

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

                  pragma Assert (Length (U_Name) > Length (U_Sep_From) + 1);

                  --  Construct the unit and add it to "Found"

                  --  Note that this parser supports only a single unit per
                  --  file. So only index 1 will be used. But this is made
                  --  so to support a full parser if one is implemented.

                  Index := Index + 1;

                  declare
                     CU : constant Unit.Object :=
                            Unit.Create
                              (Name          => Capitalize_Unit_Name (-U_Name),
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

      --------------------------
      -- Capitalize_Unit_Name --
      --------------------------

      function Capitalize_Unit_Name (Name : String) return Name_Type is
         Unit_Name : String := Name;
      begin
         Case_Util.To_Mixed (Unit_Name);

         for I in Unit_Name'First + 1 .. Unit_Name'Last loop
            if Unit_Name (I - 1) = '.' then
               Unit_Name (I) := Case_Util.To_Upper (Unit_Name (I));
            end if;
         end loop;

         return Name_Type (Unit_Name);
      end Capitalize_Unit_Name;

      Ctx    : constant Analysis_Context := Create_Context;
      A_Unit : constant Analysis_Unit    :=
                 Get_From_File
                   (Ctx, Source.Path_Name.Value,
                    Rule    => Ada_Prelude_Rule,
                    Reparse => True);

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
   GPR2.Source_Info.Parser.Registry.Register (Handle);
end GPR2.Source_Info.Parser.Ada_Language;
