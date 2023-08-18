--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--
with Ada.Directories;

with Gpr_Parser.Analysis;
with Gpr_Parser.Basic_Ada_Parser;

with GPR2.Containers;
with GPR2.File_Readers;
with GPR2.Project.Source;
with GPR2.Project.Tree;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Unit;

package body GPR2.Source_Info.Parser.Ada_Language is

   Handle : Object;

   -------------
   -- Compute --
   -------------

   overriding procedure Compute
     (Self   :        not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source :        Project.Source.Object) is
      use Gpr_Parser.Analysis;
      use Gpr_Parser.Basic_Ada_Parser;
      use GPR2.Unit;

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
         Separate_From : String := "";
         Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
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
         Source.View.Hide_Unit_Body (Data.Unit_Name (No_Index));

         Data.CU_List (No_Index) :=
           GPR2.Unit.Create
             (Name          => Data.Unit_Name (No_Index),
              Index         => No_Index,
              Lib_Unit_Kind => S_Body,
              Lib_Item_Kind => Is_No_Body,
              Main          => None,
              Dependencies  => GPR2.Containers.Name_Type_Set.Empty_Set,
              Sep_From      => No_Name,
              Flags         => GPR2.Unit.Default_Flags);

         Data.Kind     := S_Body;
         Data.Parsed   := Source_Info.Source;
         Data.Language := GPR2.Ada_Language;
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
         Separate_From : String := "";
         Lib_Item_Type : Gpr_Parser.Basic_Ada_Parser.Library_Item_Type;
         Generic_Unit  : Boolean)
      is
         U_Main  : constant GPR2.Unit.Main_Type := GPR2.Unit.None;
         U_Name  : Unbounded_String;
         U_Kind  : GPR2.Unit.Library_Unit_Type;
         U_Flags : GPR2.Unit.Flags_Set          := GPR2.Unit.Default_Flags;
         L_Type  : GPR2.Unit.Library_Item_Type;

      begin

         case Lib_Item_Type is
            when Is_Package =>
               L_Type := Is_Package;
            when Is_Subprogram =>
               L_Type := Is_Subprogram;
         end case;

         --  Only single unit parsing is supported. Data kind has been already
         --  computed based on the naming scheme while adding the files
         --  to the view. Source parsing allows to disambiguate between a
         --  source containing a body without spec, and a source containing
         --  a separate.

         if Separate_From /= "" then
            U_Kind := S_Separate;
         else
            --  Keep the unit kind information. Indeed, the Ada parser do not
            --  make the difference between the a procedure spec or body but
            --  this information has been already computed based on the naming
            --  scheme while adding the files to the view.

            U_Kind := Data.CU_List (No_Index).Kind;
         end if;

         --  Check generic

         U_Flags (Is_Generic) := Generic_Unit;

         if Separate_From /= "" then

            U_Name := To_Unbounded_String (Separate_From & '.' & Unit_Name);

         else
            U_Name := To_Unbounded_String (Unit_Name);

            --  If this is a child package, we register the parent
            --  package(s) as visible.

            Register (-U_Name, True);
         end if;

         --  Construct the unit

         declare
            CU : constant GPR2.Unit.Object :=
                 GPR2.Unit.Create
                   (Name          => Name_Type (-U_Name),
                    Index         => No_Index,
                    Main          => U_Main,
                    Flags         => U_Flags,
                    Lib_Unit_Kind => U_Kind,
                    Lib_Item_Kind => L_Type,
                    Dependencies  => U_Withed,
                    Sep_From      => Optional_Name_Type (Separate_From));

         begin
            --  Kind of first unit is also recorded in Data.Kind. As we only
            --  support single unit, this is always the case.

            Data.Kind := U_Kind;

            --  Note that we do not append but actually replace the new unit
            --  created with all information (dependencies, generic, etc...).

            Data.CU_List (No_Index) := CU;

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
                  GPR2.File_Readers.Convert (Source.View.Tree.File_Reader));

   begin
      Data.Dependencies.Clear;

      --  Do not process no_body source files yet

      Gpr_Parser.Basic_Ada_Parser.Parse_Context_Clauses
        (Filename       => Source.Path_Name.Value,
         Context        => Ctx,
         Log_Error      => null, --  Ignore parsing errors for now
         With_Clause_CB => With_Clause_CB'Access,
         Unit_Name_CB   => Unit_Name_CB'Access,
         No_Body_CB     => No_Body_CB'Access);

      if Parsed then
         Data.Parsed   := Source_Info.Source;
         Data.Language := GPR2.Ada_Language;
      else
         Data.Parsed := None;
      end if;
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
