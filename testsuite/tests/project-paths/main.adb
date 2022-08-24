--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Load (Tree : in out Project.Tree.Object);

   ----------
   -- Load --
   ----------

   procedure Load (Tree : in out Project.Tree.Object) is
      Ctx : Context.Object;
   begin
      Project.Tree.Load (Tree, Create ("demo.gpr"), Ctx);
   exception
      when GPR2.Project_Error =>
         if Tree.Has_Messages then
            Text_IO.Put_Line ("Messages found:");

            for C in Tree.Log_Messages.Iterate
              (False, False, True, True, True)
            loop
               declare
                  M   : constant Message.Object := Log.Element (C);
                  Mes : constant String := M.Format;
                  L   : constant Natural :=
                          Strings.Fixed.Index (Mes, "project-paths");
               begin
                  if L /= 0 then
                     Text_IO.Put_Line (Mes (L .. Mes'Last));
                  else
                     Text_IO.Put_Line (Mes);
                  end if;
               end;
            end loop;
         end if;
   end Load;

   Current_Directory : Filename_Type :=
                         Filename_Type (Directories.Current_Directory);
   Prj1, Prj2, Prj3  : Project.Tree.Object;

begin
   --  Prj1

   Text_IO.Put_Line ("=== Prj1");
   Load (Prj1);

   --  Prj2

   Text_IO.Put_Line ("=== Prj2");
   Prj2.Register_Project_Search_Path
     (Path_Name.Create_Directory ("one", Current_Directory));
   Load (Prj2);

   --  Prj3

   Text_IO.Put_Line ("=== Prj3");
   Prj3.Register_Project_Search_Path
     (Path_Name.Create_Directory ("one", Current_Directory));
   Prj3.Register_Project_Search_Path
     (Path_Name.Create_Directory ("two", Current_Directory));
   Load (Prj3);
end Main;
