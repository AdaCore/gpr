------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--          Copyright (C) 2016-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;

   use GPR2;
   use GPR2.Containers;

   Project_Tree : Project.Tree.Object;
   Ctx          : Context.Object := Context.Empty;
   RTS          : Name_Value_Map := Name_Value_Map_Package.Empty_Map;

   procedure Print_Config_Info;

   -----------------------
   -- Print_Config_Info --
   -----------------------

   procedure Print_Config_Info is
      Config_View     : constant Project.View.Object :=
                          Project_Tree.Configuration.Corresponding_View;
      Compiler_Driver : constant Path_Name.Object :=
                          Path_Name.Create_File
                            (Name_Type (Config_View.Pack ("compiler").Attribute
                                          (Name  => "driver",
                                           Index => "Ada").Value.Text));
      Runtime_Dir     : constant Path_Name.Object :=
                          Path_Name.Create_Directory
                            (Name_Type (Config_View.Attribute
                                          (Name  => "Runtime_Dir",
                                           Index => "Ada").Value.Text));
   begin
      Text_IO.Put_Line ("target = "
                        & Config_View.Attribute ("Target").Value.Text);
      Text_IO.Put_Line ("compiler driver = "
                        & String (Compiler_Driver.Base_Name));

      declare
         use Strings.Fixed;

         RT_Dir_Str      : constant String := String (Runtime_Dir.Dir_Name);
         Sec_To_Last_Sep : constant Integer :=
                             Index (Source  => RT_Dir_Str,
                                    Pattern => Tail (RT_Dir_Str, 1),
                                    From    => RT_Dir_Str'Last - 1,
                                    Going   => Strings.Backward);
      begin
         Text_IO.Put_Line
           ("runtime dir = "
            & RT_Dir_Str (Sec_To_Last_Sep + 1 .. RT_Dir_Str'Last - 1));
      end;
   end Print_Config_Info;

begin
   --  Equivalent to command line options:
   --     --RTS=zfp target=x86-linux

   RTS.Insert ("Ada", "rtp");
   Ctx.Insert ("VSB_DIR", ".");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("projects/a.gpr"),
      Context           => Ctx,
      Target            => "x86_64-vx7",
      Language_Runtimes => RTS);

   Print_Config_Info;

   Project_Tree.Unload;

   Text_IO.New_Line;

   --  Equivalent to command line without --RTS / --target

   Project_Tree.Load_Autoconf
     (Filename => Project.Create ("projects/a.gpr"),
      Context  => Ctx);

   Print_Config_Info;
end Main;
