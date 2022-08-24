--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;
   package PRP renames GPR2.Project.Registry.Pack;

   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;

   procedure Test (Project_Name : GPR2.Filename_Type);

   procedure Test (Project_Name : GPR2.Filename_Type) is
   begin
      Ada.Text_IO.Put_Line ("testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Tree.Log_Messages.Output_Messages (Information => False);
   exception
      when Project_Error =>
         Tree.Log_Messages.Output_Messages (Information => False);
   end Test;

begin
   PRP.Check_Attributes (PRP.Builder);
   PRP.Check_Attributes (PRP.Naming);
   Test ("gpr/err_unknown_toplevel.gpr");
   Test ("gpr/err_unknown_package.gpr");
   Test ("gpr/err_single_value.gpr");
   Test ("gpr/err_list_value.gpr");
   Test ("gpr/err_unexp_index.gpr");
   Test ("gpr/err_empty_value.gpr");
   Test ("gpr/warn_empty_value.gpr");
   Test ("gpr/err_no_index.gpr");
   Test ("gpr/err_unexp_index.gpr");
   Test ("gpr/err_unexp_others.gpr");
end Main;
