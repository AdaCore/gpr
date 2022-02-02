with Ada.Strings.Fixed;
with Ada.Text_IO;

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

begin
   RTS.Insert (Ada_Language, "rtp");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("agg.gpr"),
      Context           => Ctx,
      Target            => "x86_64-wrs-vxworks7",
      Language_Runtimes => RTS);

   if Project_Tree.Log_Messages.Has_Error then
      for M of Project_Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;
   end if;

exception
   when Project_Error =>
      for M of Project_Tree.Log_Messages.all loop
         Text_IO.Put_Line (M.Format);
      end loop;
end Main;
