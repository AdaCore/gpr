--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

procedure Main is

   use Ada;

   use GPR2;
   use GPR2.Containers;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;

   Project_Tree : Project.Tree.Object;
   Ctx          : Context.Object := Context.Empty;
   RTS          : Lang_Value_Map := Lang_Value_Maps.Empty_Map;
   This_Target  : constant String := System.OS_Constants.Target_Name;

   procedure Print_Config_Info;

   -----------------------
   -- Print_Config_Info --
   -----------------------

   procedure Print_Config_Info is
      Ada_I           : constant Project.Attribute_Index.Object :=
                          Project.Attribute_Index.Create (GPR2.Ada_Language);
      Config_View     : constant Project.View.Object :=
                          Project_Tree.Configuration.Corresponding_View;
      Var              : constant String :=
                           String
                             (Project_Tree.Root_Project.Variable
                                ("Var").Value.Text);
      Target           : constant String :=
                           Config_View.Attribute (PRA.Target).Value.Text;
      Canonical_Target : constant String :=
                           Config_View.Attribute 
                             (PRA.Canonical_Target).Value.Text;
      Languages        : constant GPR2.Containers.Source_Value_List :=
                           Project_Tree.Root_Project.Languages;
      Has_C            : Boolean := False;

   begin
      Text_IO.Put_Line ("target = "
                        & (if Target = This_Target
                           then "this-target"
                           else Target));
      if Target /= This_Target then
         Text_IO.Put_Line ("canonical_target = " & Canonical_Target);
      end if;
      if Var /= Target then
         Text_IO.Put_Line
           ("!!! Error: Var is different from actual target value");
         Text_IO.Put_Line ("Var = " & Var);
      end if;

      --  Check languages list change during autoconf
      if not Languages.Is_Empty then
         for Value of Languages loop
            Text_IO.Put_Line ("language: " & Value.Text);
            declare
               package PAI renames Project.Attribute_Index;
               Driver_Attr     : constant Project.Attribute.Object :=
                                   Config_View.Attribute
                                     (Name  => PRA.Compiler.Driver,
                                      Index => PAI.Create 
                                                 (+Name_Type (Value.Text)));
               Compiler_Driver : constant Path_Name.Object :=
                                   Path_Name.Create_File
                                     (Filename_Type (Driver_Attr.Value.Text));
               Runtime_Dir     : constant Path_Name.Object :=
                                   (if Value.Text = "Ada"
                                    then Path_Name.Create_Directory
                                      (Filename_Type (Config_View.Attribute
                                       (Name  => PRA.Runtime_Dir,
                                        Index => Ada_I).Value.Text))
                                    else Path_Name.Undefined);
            begin
               Text_IO.Put_Line ("- compiler driver = "
                                 & String (Compiler_Driver.Base_Name));

               if Runtime_Dir.Is_Defined then
                  declare
                     use Strings.Fixed;

                     RT_Dir_Str      : constant String :=
                                         String (Runtime_Dir.Dir_Name);
                     Sec_To_Last_Sep : constant Integer :=
                                         Index (Source  => RT_Dir_Str,
                                                Pattern => Tail (RT_Dir_Str, 1),
                                                From    => RT_Dir_Str'Last - 1,
                                                Going   => Strings.Backward);
                  begin
                     Text_IO.Put_Line
                       ("- runtime dir = "
                        & RT_Dir_Str (Sec_To_Last_Sep + 1 .. RT_Dir_Str'Last - 1));
                  end;
               end if;
            end;
         end loop;
      end if;
   end Print_Config_Info;

begin
   --  Equivalent to command line options:
   --     --RTS=rtp -Xtarget=x86_64-wrs-vxworks7

   RTS.Insert (Ada_Language, "rtp");
   Ctx.Insert ("VSB_DIR", ".");
   Ctx.Insert ("target", "x86_64-wrs-vxworks7");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("projects/a.gpr"),
      Context           => Ctx,
      Language_Runtimes => RTS);

   Print_Config_Info;

   Project_Tree.Unload;

   Text_IO.New_Line;

   --  --RTS=rtp -Xtarget=x86-linux --target=x86_64-wrs-vxworks7
   --  --target will take precedence over Target definition in the project

   Ctx.Clear;
   Ctx.Insert ("VSB_DIR", ".");
   Ctx.Insert ("target", This_Target);

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("projects/a.gpr"),
      Context           => Ctx,
      Target            => "x86_64-wrs-vxworks7",
      Language_Runtimes => RTS);

   Print_Config_Info;

   Project_Tree.Unload;

   Text_IO.New_Line;

   --  Equivalent to command line without --RTS / --target

   Project_Tree.Load_Autoconf
     (Filename => Project.Create ("projects/a.gpr"),
      Context  => Ctx);

   Print_Config_Info;

exception
   when Project_Error =>
      for M of Project_Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;
end Main;
