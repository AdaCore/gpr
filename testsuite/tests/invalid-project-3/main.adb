--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Directory_Operations;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      for C in Tree.Log_Messages.Iterate
        (Information => False,
         Warning     => not Tree.Log_Messages.Has_Error, --  Show warning only when no error
         Error       => True,
         Read        => False,
         Unread      => True)
      loop
         Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
      end loop;
   end Print_Messages;

   procedure Test (Project_Name : GPR2.Filename_Type;
                   Implicit     : Boolean := False)
   is
      Old_Cwd  : constant GNAT.Directory_Operations.Dir_Name_Str :=
                   GNAT.Directory_Operations.Get_Current_Dir;
      Gpr_Path : constant GPR2.Path_Name.Object :=
                   (if not Implicit
                    then GPR2.Path_Name.Create_File (Project_Name)
                    else GPR2.Path_Name.Create_Directory (Project_Name));
      Gpr_Dir : constant GNAT.Directory_Operations.Dir_Name_Str :=
                  String (Gpr_Path.Dir_Name);
   begin
      Tree.Unload;
      GNAT.Directory_Operations.Change_Dir (Gpr_Dir);

      Tree.Load_Autoconf
        (Filename => Gpr_Path,
         Context  => Context);
      Tree.Update_Sources;

      Print_Messages;

      GNAT.Directory_Operations.Change_Dir (Old_Cwd);
   exception
      when Project_Error =>
         Print_Messages;
         GNAT.Directory_Operations.Change_Dir (Old_Cwd);
      when E : others =>
         Ada.Text_IO.Put_Line ("!!! Uncaught exception raised !!!");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.Directory_Operations.Change_Dir (Old_Cwd);
   end Test;

begin
   Ada.Text_IO.Put_Line ("** builtins: external");
   Ada.Text_IO.New_Line;
   Test ("builtins/external/empty.gpr");
   Test ("builtins/external/non_string.gpr");
   Test ("builtins/external/non_string2.gpr");
   Test ("builtins/external/empty_string.gpr");
   Test ("builtins/external/three_arg.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** builtins: external_as_list");
   Ada.Text_IO.New_Line;
   Test ("builtins/external_as_list/empty.gpr");
   Test ("builtins/external_as_list/non_string.gpr");
   Test ("builtins/external_as_list/non_string2.gpr");
   Test ("builtins/external_as_list/empty_string.gpr");
   Test ("builtins/external_as_list/empty_string2.gpr");
   Test ("builtins/external_as_list/one_arg.gpr");
   Test ("builtins/external_as_list/three_arg.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** builtins: split");
   Ada.Text_IO.New_Line;
   Test ("builtins/split/empty.gpr");
   Test ("builtins/split/non_string.gpr");
   Test ("builtins/split/non_string2.gpr");
   Test ("builtins/split/empty_string2.gpr");
   Test ("builtins/split/one_arg.gpr");
   Test ("builtins/split/three_arg.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** packages:");
   Ada.Text_IO.New_Line;
   Test ("packages/extends_nonexist.gpr");
   Test ("packages/extends_nonexist2.gpr");
   Test ("packages/renames_nonexist.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** attributes:");
   Ada.Text_IO.New_Line;
   Test ("attributes/conf.gpr");
   Test ("attributes/builtin.gpr");
   Test ("attributes/empty_warning.gpr");
   Test ("attributes/empty_error.gpr");
   Test ("attributes/no_package.gpr");
   Test ("attributes/no_package2.gpr");
   Test ("attributes/unexpected_index.gpr");
   Test ("attributes/unexpected_index2.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** variables:");
   Ada.Text_IO.New_Line;
   Test ("variables/undef_project_ref.gpr");
   Test ("variables/in_pack.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** projects:");
   Ada.Text_IO.New_Line;
   Test ("projects/no_lang.gpr");
   Test ("projects/agg_lib_shared_libdir.gpr");
   Test ("projects/agg_lib_shared_libdir2.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** naming convention:");
   Ada.Text_IO.New_Line;
   Test ("naming/dot_repl_is_dot.gpr");
   Test ("naming/name_except.gpr");
   Test ("naming/same_suffix.gpr");
   Test ("naming/same_suffix2.gpr");
   Test ("naming/same_suffix3.gpr");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** configuration:");
   Ada.Text_IO.New_Line;
   Test ("config", True);
end Main;
