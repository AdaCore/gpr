------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;
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

   procedure Test (Project_Name : GPR2.Filename_Type) is
   begin
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Print_Messages;
   exception
      when Project_Error =>
         Print_Messages;
      when E : others =>
         Ada.Text_IO.Put_Line ("!!! Uncaught exception raised !!!");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
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
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("** variables:");
   Ada.Text_IO.New_Line;
   Test ("variables/undef_project_ref.gpr");
   Test ("variables/in_pack.gpr");
end Main;
