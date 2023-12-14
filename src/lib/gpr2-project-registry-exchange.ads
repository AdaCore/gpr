--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides a way to import/export adacore & non-adacore
--  tools GPR registy. Export support is provided in GPR2.Options package
--  through a new Print_GPR_Registry option. ("--print-gpr-registry")
--  Use GPR2.Options.Print_GPR_Registry API to print registry & exit if needed.
--  Textual (export only), JSON & JSON_COMPACT formats are supported.
--  exported/imported packages can be filtered. Default filter excludes
--  project level scope attributes & all packages defined by GPR2 library.

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Containers;
with GPR2.Project.Registry.Pack;

package GPR2.Project.Registry.Exchange is

   type Export_Format is (K_TEXT, K_JSON, K_JSON_COMPACT);
   --  Supported export format

   procedure Export
     (Included : GPR2.Containers.Package_Id_List :=
                    GPR2.Project.Registry.Pack.All_Packages;
      Excluded : GPR2.Containers.Package_Id_List :=
                    GPR2.Project.Registry.Pack.Predefined_Packages;
      Format   : Export_Format := K_JSON_COMPACT;
      Output   : access procedure (Item : String) := Ada.Text_IO.Put'Access);
   --  Export Packages filtered by Included/Excluded using right formating
   --  By default export all packages added after gpr2 initialization.

   procedure Import (Definitions : Ada.Strings.Unbounded.Unbounded_String;
                     Included    : GPR2.Containers.Package_Id_List :=
                       GPR2.Containers.Package_Id_Type_List.Empty;
                     Excluded    : GPR2.Containers.Package_Id_List :=
                       GPR2.Project.Registry.Pack.Predefined_Packages);
   --  Import in registry Definitions (in JSON format) filtered by
   --  Included/Excluded parameters.
   --  By default import all definitions except project scope and gpr2
   --  prefedined packages.
   --  Raise GNATCOLL.JSON.Invalid_JSON_Stream exception if Definitions is an
   --  invalid JSON string.

end GPR2.Project.Registry.Exchange;
