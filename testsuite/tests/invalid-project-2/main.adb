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
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate
           (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Test (Project_Name : GPR2.Filename_Type) is
   begin
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Test ("1/prj.gpr");
   Test ("2/prj.gpr");
   Test ("3/prj.gpr");
   Test ("4/prj.gpr");
   Test ("5/prj.gpr");
   Test ("6/prj.gpr");
   Test ("7/prj.gpr");
   Test ("8/prj.gpr");
   Test ("9/prj.gpr");
   Test ("10/prj.gpr");
   Test ("11/prj.gpr");
   Test ("12/prj.gpr");
   Test ("13/prj.gpr");
   Test ("14/prj.gpr");
   Test ("15/prj.gpr");
   Test ("16/prj.gpr");
   Test ("17/prj.gpr");
   Test ("18/prj.gpr");
   Test ("1/prj2.gpr");
   Test ("2/prj2.gpr");
   Test ("3/prj2.gpr");
   Test ("4/prj2.gpr");
   Test ("1/prj4.gpr");
   Test ("2/prj4.gpr");
end Main;
