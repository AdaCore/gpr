--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;

with Gpr_Parser.Analysis;
with Gpr_Parser.Basic_Ada_Parser;

with GPR2.Containers;
with GPR2.File_Readers;

package body GPR2.Build.Source.Ada_Parser is

   procedure Compute
     (Tree             : access GPR2.Project.Tree.Object;
      Data             : in out Source.Object'Class;
      Get_Withed_Units : Boolean;
      Success          : out Boolean)
   is
      use Gpr_Parser.Analysis;

      U_Withed : Containers.Name_Set;
      W_Found  : Containers.Name_Set;

      Parsed   : Boolean := False;
      --  If something has been parsed on this source file, then Parsed is set
      --  to True and New_Data will replace Data parameter.

      procedure No_Body_CB;

      procedure Register
        (Name         : String;
         Parents_Only : Boolean := False);
      --  Register Name and parent if any

      procedure Unit_Name_CB
        (Unit_Name     : String;
         Separate_From : String;
         Generic_Unit  : Boolean);
         --  Callback called by the Ada parser when a unit has been
         --  processed. Create a GPR2 unit with its dependencies, and
         --  add it to Data.CU_List.

      procedure With_Clause_CB
        (Unit_Name  : String;
         Is_Limited : Boolean);
         --  Callback called by the Ada parser when a dependency has
         --  been processed. Add the withed unit to U_Withed if it has
         --  not already been inserted, so it can be linked to the unit
         --  during Unit_Name_CB.

      ----------------
      -- No_Body_CB --
      ----------------

      procedure No_Body_CB is
      begin
         Data.Update_Unit (Create (Unit_Name => No_Name,
                                   Index     => No_Index,
                                   Kind      => S_No_Body,
                                   Parsed    => True));
         Data.Kind := S_No_Body;
         Parsed    := True;
      end No_Body_CB;

      --------------
      -- Register --
      --------------

      procedure Register
        (Name         : String;
         Parents_Only : Boolean := False)
      is
         N        : constant Name_Type := Name_Type (Name);
         B_Name   : constant String    := Directories.Base_Name (Name);
         Position : Containers.Name_Type_Set.Cursor;
         Inserted : Boolean;

      begin
         if not Parents_Only then
            W_Found.Insert (N, Position, Inserted);

            if Inserted then
               U_Withed.Insert (N);
            end if;
         end if;

         if B_Name /= Name then
            Register (B_Name);
         end if;
      end Register;

      ------------------
      -- Unit_Name_CB --
      ------------------

      procedure Unit_Name_CB
        (Unit_Name     : String;
         Separate_From : String;
         Generic_Unit  : Boolean)
      is
         pragma Unreferenced (Generic_Unit);

      begin

         --  Only single unit parsing is supported. Data kind has been already
         --  computed based on the naming scheme while adding the files
         --  to the view. Source parsing allows to disambiguate between a
         --  source containing a body without spec, and a source containing
         --  a separate.

         if Separate_From /= "" then
            Data.Kind := S_Separate;
         elsif Data.Kind = S_Separate then
            Data.Kind := S_Body;
         end if;

         --  Check generic

         --  ??? We should store the info as this means that the body is
         --  also withed by anyone using the spec

         --  U_Flags (Is_Generic) := Generic_Unit;

         if Separate_From = "" then
            --  If this is a child package, we register the parent
            --  package(s) as visible.

            Register (Unit_Name, True);
         end if;

         --  Construct the unit

         declare
            U_Name : constant Name_Type :=
                       (if Separate_From'Length > 0
                        then Name_Type (Separate_From)
                        else Name_Type (Unit_Name));
            Sep_Name : constant Optional_Name_Type :=
                         (if Separate_From'Length > 0
                          then Optional_Name_Type (Unit_Name)
                          else "");

            CU       : constant Unit_Part :=
                         Build.Source.Create
                           (Unit_Name     => U_Name,
                            Index         => No_Index,
                            Kind          => Data.Kind,
                            Separate_Name => Sep_Name,
                            Parsed        => True);

         begin
            --  Note that we do not append but actually replace the new unit
            --  created with all information (dependencies, generic, etc...).

            Data.Update_Unit (CU);

            Parsed := True;
         end;

      end Unit_Name_CB;

      --------------------
      -- With_Clause_CB --
      --------------------

      procedure With_Clause_CB
        (Unit_Name  : String;
         Is_Limited : Boolean)
      is
         pragma Unreferenced (Is_Limited);
      begin
         Register (Unit_Name);
      end With_Clause_CB;

      Ctx : constant Analysis_Context :=
            Create_Context
               (File_Reader =>
                  GPR2.File_Readers.Convert (Tree.File_Reader));

   begin
      Gpr_Parser.Basic_Ada_Parser.Parse_Context_Clauses
        (Filename       => Data.Path_Name.Value,
         Context        => Ctx,
         Log_Error      => null, --  Ignore parsing errors for now
         With_Clause_CB => (if Get_Withed_Units
                            then With_Clause_CB'Access
                            else null),
         Unit_Name_CB   => Unit_Name_CB'Access,
         No_Body_CB     => No_Body_CB'Access);

      if not Parsed
        and then Data.Has_Unit_At (No_Index)
        and then Data.Unit.Is_Parsed
      then
         declare
            Old : Unit_Part := Data.Unit;
         begin
            Old.Is_Parsed := False;
            Data.Update_Unit (Old);
         end;
      end if;

      Success := Parsed;
   end Compute;

end GPR2.Build.Source.Ada_Parser;
