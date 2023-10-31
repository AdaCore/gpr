--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Environment_Variables;
with Ada.Text_IO;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Project.External; use GPR2.Project.External;

procedure Main is

   Tree : GPR2.Project.Tree.Object;
   use GPR2;

   procedure Print_Messages is
   begin
      if Tree.Has_Messages then
         for C in Tree.Log_Messages.Iterate (False, True, True, True, True)
         loop
            Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
         end loop;
      end if;
   end Print_Messages;

   procedure Test (Project_Name : GPR2.Filename_Type) is

      procedure Print_Externals (Root_Only : Boolean) is
         Exts : External_Arr := Externals (Tree, Root_Only);

      begin
         Ada.Text_IO.Put_Line ("");
         Ada.Text_IO.Put_Line ("======= ROOT_ONLY : " & Root_Only'Image & " =======");

         Ada.Text_IO.Put_Line ("");
         Ada.Text_IO.Put_Line ("=============");
         Ada.Text_IO.Put_Line ("== UNTYPED ==");
         Ada.Text_IO.Put_Line ("=============");

         for E of Exts loop
            if not E.Is_Typed then
               Ada.Text_IO.Put_Line (E.Name);
            end if;
         end loop;

         Ada.Text_IO.Put_Line ("");
         Ada.Text_IO.Put_Line ("=============");
         Ada.Text_IO.Put_Line ("==  TYPED  ==");
         Ada.Text_IO.Put_Line ("=============");

         for E of Exts loop
            if E.Is_Typed then
               if E.Is_Conflicting then
                  Ada.Text_IO.Put_Line (E.Name & " (Conflicting)");
               else
                  Ada.Text_IO.Put_Line (E.Name);
               end if;

               for Possible_Val of Possible_Values_Of (E) loop
                  Ada.Text_IO.Put_Line ("   - " & To_String (Possible_Val));
               end loop;
            end if;
         end loop;

         Ada.Text_IO.Put_Line ("");
         Ada.Text_IO.Put_Line ("==============");
         Ada.Text_IO.Put_Line ("== WARNINGS ==");
         Ada.Text_IO.Put_Line ("==============");

         if Tree.Has_Messages then
            for C in Tree.Log_Messages.Iterate
              (Information => False,
               Warning => True,
               Error => True,
               Lint => True,
               Read => False,
               Unread => True)
               loop
                  Ada.Text_IO.Put_Line (GPR2.Log.Element (C).Format);
               end loop;
         end if;

      end Print_Externals;

      Context      : GPR2.Context.Object;

   begin

      Ada.Text_IO.Put_Line ("Testing " & String (Project_Name));
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename =>
           GPR2.Path_Name.Create_File
             (GPR2.Project.Ensure_Extension (Project_Name),
              GPR2.Path_Name.No_Resolution),
         Context  => Context);

      Print_Externals (True);
      Tree.Log_Messages.Clear;
      Print_Externals (False);

   exception
      when Project_Error =>
         Print_Messages;
   end Test;

begin
   Test ("parsed");
end Main;
