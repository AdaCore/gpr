with Ada.Text_IO;
with Ada.Environment_Variables;

with GPR2.KB;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;

procedure Main is

   use Ada.Text_IO;

   use GPR2;

   Project_Tree : Project.Tree.Object;
   Opt          : GPR2.Options.Object;


   Tgt : constant Optional_Name_Type := "x86_64-wrs-vxworks7r2";

   Conf_File : Filename_Type := "testconfig.cgpr";
   KBase     : GPR2.KB.Object := GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   Descrs    : constant Project.Configuration.Description_Set :=
                 (1 => Project.Configuration.Create
                    (Language => GPR2.Ada_Language,
                     Runtime  => "rtp"));
   Conf_Obj  : GPR2.Project.Configuration.Object :=
                 GPR2.Project.Configuration.Create
                   (Descrs, Tgt, Project.Create ("foo.gpr"), KBase,
                    Save_Name => Path_Name.Create_File (Conf_File))
     with Unreferenced;


begin

   Put_Line ("Autoconf + external set by aggregate project");

   Opt.Add_Switch (Options.P, "agg.gpr");
   Opt.Add_Switch (Options.Target, String (Tgt));
   Opt.Add_Switch (Options.RTS, "rtp", "Ada");

   if not Project_Tree.Load (Opt) then
      Put_Line ("Could not load project");
   end if;
   Project_Tree.Unload;

   Put_Line ("Configuration object + external set by aggregate project");

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "agg.gpr");
   Opt.Add_Switch (Options.Config, String (Conf_File));

   if not Project_Tree.Load (Opt) then
      Put_Line ("Could not load project");
   end if;
   Project_Tree.Unload;

   Ada.Environment_Variables.Set("VSB_DIR", "/foo");

   Put_Line ("Autoconf + external set by environment");

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "agg.gpr");
   Opt.Add_Switch (Options.Target, String (Tgt));
   Opt.Add_Switch (Options.RTS, "rtp", "Ada");

   if not Project_Tree.Load (Opt) then
      Put_Line ("Could not load project");
   end if;
   Project_Tree.Unload;

   Put_Line ("Configuration object + external set by environment");

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "agg.gpr");
   Opt.Add_Switch (Options.Config, String (Conf_File));

   if not Project_Tree.Load (Opt) then
      Put_Line ("Could not load project");
   end if;
   Project_Tree.Unload;
end Main;
