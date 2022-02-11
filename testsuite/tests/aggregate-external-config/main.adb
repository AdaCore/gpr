with Ada.Text_IO;
with Ada.Environment_Variables;

with GPR2.KB;
with GPR2.Log;
with GPR2.Project.Configuration;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada.Text_IO;

   use GPR2;
   use GPR2.Containers;
   use GPR2.KB;

   Project_Tree : Project.Tree.Object;
   Ctx          : Context.Object := Context.Empty;
   RTS          : Lang_Value_Map := Lang_Value_Maps.Empty_Map;

   Msgs : GPR2.Log.Object;

   Tgt : constant Optional_Name_Type := "x86_64-wrs-vxworks7";

   Descrs : constant Project.Configuration.Description_Set :=
              (1 => Project.Configuration.Create
                 (Language => GPR2.Ada_Language,
                  Runtime  => "rtp"));

   KBase : GPR2.KB.Object := Create_Default (Default_Flags);

   Conf_Obj : GPR2.Project.Configuration.Object :=
                GPR2.Project.Configuration.Create
                  (Descrs, Tgt, Project.Create ("foo.gpr"), KBase);

   procedure Report_If_Errors (S : String);

   procedure Report_If_Errors (S : String) is
   begin
      if Project_Tree.Log_Messages.Has_Error then
         Put_Line (S);

         for M of Project_Tree.Log_Messages.all loop
            Put_Line (M.Format);
         end loop;
      end if;
   end Report_If_Errors;

begin

   RTS.Insert (Ada_Language, "rtp");

   --  Autoconf + external set by aggregate project

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("agg.gpr"),
      Context           => Ctx,
      Target            => Tgt,
      Language_Runtimes => RTS);
   Report_If_Errors ("autoconf + agg external");
   Project_Tree.Unload;

   --  Configuration object + external set by aggregate project

   Project_Tree.Load
     (Filename =>  Project.Create ("agg.gpr"),
      Context  =>  Ctx,
      Config   =>  Conf_Obj);
   Report_If_Errors ("conf obj + agg external");
   Project_Tree.Unload;

   Ada.Environment_Variables.Set("VSB_DIR", "/foo");

   --  Autoconf + external set by environment

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("agg2.gpr"),
      Context           => Ctx,
      Target            => Tgt,
      Language_Runtimes => RTS);
   Report_If_Errors ("autoconf + env external");
   Project_Tree.Unload;

   --  Configuration object + external set by environment

   Project_Tree.Load
     (Filename =>  Project.Create ("agg2.gpr"),
      Context  =>  Ctx,
      Config   =>  Conf_Obj);
   Report_If_Errors ("conf obj + env external");
   Project_Tree.Unload;

exception
   when Project_Error =>
      for M of Project_Tree.Log_Messages.all loop
         Put_Line (M.Format);
      end loop;
end Main;
