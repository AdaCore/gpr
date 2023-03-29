--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Project.Registry.Pack.Description is

   -----------------------------
   -- Get_Package_Description --
   -----------------------------

   function Get_Package_Description (Key : Package_Id) return String is
   begin
      if Pack_Package_Description.Contains (Package_Description, Key)
      then
         return Pack_Package_Description.Element (Package_Description, Key);
      else
         return "";
      end if;
   end Get_Package_Description;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Package_Id)
                  return Hash_Type is (Ada.Strings.Hash (Image (Key)));

   -----------------------------
   -- Set_Package_Description --
   -----------------------------

   procedure Set_Package_Description
     (Key : Package_Id; Description : String) is
      use Pack_Package_Description;

      C : constant Cursor := Find (Package_Description, Key);
   begin
      if C = No_Element then
         Insert (Package_Description, Key, Description);
      else
         Replace_Element (Package_Description, C, Description);
      end if;
   end Set_Package_Description;

begin

   --  Binder
   Set_Package_Description
     (Binder,
      "This package specifies characteristics useful when invoking the binder "
      & "either directly via the gnat driver or when using GPRbuild.");

   --  Builder
   Set_Package_Description
     (Builder,
      "This package specifies the compilation options used when building an "
      & "executable or a library for a project. Most of the options should be "
      & "set in one of Compiler, Binder or Linker packages, but there are "
      & "some general options that should be defined in this package.");

   --  Clean
   Set_Package_Description
     (Clean,
      "This package specifies the options used when cleaning a project or a "
      & "project tree using the tools gnatclean or gprclean.");

   --  Compiler
   Set_Package_Description
     (Compiler,
      "This package specifies the compilation options used by the compiler "
      & "for each language.");

   --  Gnatls
   Set_Package_Description
     (Gnatls,
      "This package specifies the options to use when invoking gnatls via the "
      & "gnat driver.");

   --  Install
   Set_Package_Description
     (Install,
      "This package specifies the options used when installing a project with "
      & "gprinstall.");

   --  Linker
   Set_Package_Description
     (Linker,
      "This package specifies the options used by the linker.");

   --  Naming
   Set_Package_Description
     (Naming,
      "This package specifies the naming conventions that apply to the source "
      & "files in a project. In particular, these conventions are used to "
      & "automatically find all source files in the source directories, or "
      & "given a file name to find out its language for proper processing.");

   --  Remote
   Set_Package_Description
     (Remote,
      "This package is used by GPRbuild to describe how distributed "
      & "compilation should be done.");

end GPR2.Project.Registry.Pack.Description;
