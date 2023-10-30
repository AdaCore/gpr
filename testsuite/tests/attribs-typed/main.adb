--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Variable.Set;
with GPR2.Context;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;
   use GPR2.Project.Registry.Attribute;

   use all type GPR2.Project.Name_Values.Value_Kind;


   procedure Test (Project_File : Filename_Type);

   procedure Test (Project_File : Filename_Type) is
      Prj : Project.Tree.Object;
      Ctx : Context.Object;

   begin

      begin
         Project.Tree.Load_Autoconf (Prj, Create (Project_File), Ctx);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
            Prj.Log_Messages.Output_Messages (Information => False);
      end;

      Prj.Unload;
   end Test;

begin

   Test ("ko_externally_built.gpr");
   Test ("ok_externally_built.gpr");

   Test ("ok_library_kind.gpr");
   Test ("ko_library_kind.gpr");

end Main;
