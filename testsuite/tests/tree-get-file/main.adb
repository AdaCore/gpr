------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Project_Name : constant GPR2.Filename_Type := "test";
   use GPR2;
begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Project_Name),
         GPR2.Path_Name.No_Resolution),
      Context  => Context);
   if not Tree.Log_Messages.Has_Error then
      declare
         function Check (S : GPR2.Simple_Name) return Boolean;
         function Check (S : GPR2.Simple_Name) return Boolean is
            File : constant GPR2.Path_Name.Object := Tree.Get_File (S);
         begin
            return File.Is_Defined and then File.Exists and then
              File.Simple_Name = S;
         end Check;
      begin
         if Check ("gpr2.gpr") and then Check ("main.adb") and then
           Check ("main.o") and then Check ("gpr2-project-tree.ads") and then
           Check ("a-textio.ads") and then not Check ("none.ads")
         then
            Ada.Text_IO.Put_Line ("OK");
         end if;
      end;
   end if;
end Main;
