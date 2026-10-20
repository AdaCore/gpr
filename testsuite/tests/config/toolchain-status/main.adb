with Ada.Text_IO; use Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.KB;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Reporter;
with GPR2.Reporter.Console;

procedure Main is
   package GPC renames GPR2.Project.Configuration;
   use type GPC.Toolchain_Status_Kind;

   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Base : GPR2.KB.Object := GPR2.KB.Create (GPR2.KB.Default_Flags);
   Conf : GPC.Object;

   procedure Check_Tree
     (Label    : String;
      Expected : GPC.Toolchain_Status_Kind;
      Success  : Boolean := True;
      Config   : GPC.Object := GPC.Undefined);

   procedure Check_Tree
     (Label    : String;
      Expected : GPC.Toolchain_Status_Kind;
      Success  : Boolean := True;
      Config   : GPC.Object := GPC.Undefined)
   is
      Loaded : constant Boolean := Tree.Load
        (Opt,
         Reporter         => GPR2.Reporter.Console.Create
                               (GPR2.Reporter.Quiet),
         Absent_Dir_Error => No_Error,
         Config           => Config);
   begin
      Put_Line (Label & ": " & Tree.Toolchain_Status'Image);
      if Loaded /= Success or else Tree.Toolchain_Status /= Expected then
         raise Program_Error with "unexpected load result or toolchain status";
      end if;

      if Tree.Has_Configuration then
         if Tree.Configuration.Toolchain_Status /= Expected then
            raise Program_Error with "inconsistent configuration status";
         end if;
      elsif Expected /= GPC.Unknown then
         raise Program_Error with "configuration missing";
      end if;
   end Check_Tree;

begin
   if Tree.Toolchain_Status /= GPC.Unknown
     or else Conf.Toolchain_Status /= GPC.Unknown
   then
      raise Program_Error with "unexpected status before discovery";
   end if;

   Opt.Add_Switch (GPR2.Options.P, "simple.gpr");
   Check_Tree ("Native", GPC.Found);

   Opt.Add_Switch (GPR2.Options.Target, "missing-target");
   Check_Tree ("Missing target", GPC.Not_Found);
   if not Tree.Has_Configuration or else Tree.Configuration.Has_Error then
      raise Program_Error with "unexpected configuration errors";
   end if;

   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "missing.gpr");
   Check_Tree ("Failed load", GPC.Not_Found, Success => False);

   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "simple.gpr");
   Check_Tree ("Retry", GPC.Found);

   Conf := GPC.Create
     (Settings  => GPC.Default_Description,
      Target    => "all",
      Project   => GPR2.Path_Name.Create_File ("simple.gpr"),
      Base      => Base,
      Save_Name => GPR2.Path_Name.Create_File ("explicit.cgpr"));
   if Conf.Toolchain_Status /= GPC.Found then
      raise Program_Error with "configuration lost discovery result";
   end if;
   Check_Tree ("Provided configuration", GPC.Found, Config => Conf);

   Conf := GPC.Load (GPR2.Path_Name.Create_File ("explicit.cgpr"));
   if Conf.Toolchain_Status /= GPC.Unknown then
      raise Program_Error with "loaded configuration claimed discovery";
   end if;
   Opt.Add_Switch (GPR2.Options.Config, "explicit.cgpr");
   Check_Tree ("Explicit configuration", GPC.Unknown);

   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "no-languages.gpr");
   Check_Tree ("No languages", GPC.Unknown);

   Opt := GPR2.Options.Empty_Options;
   Opt.Add_Switch (GPR2.Options.P, "simple.gpr");
   Check_Tree ("Before unload", GPC.Found);

   Tree.Unload;
   if Tree.Toolchain_Status /= GPC.Unknown then
      raise Program_Error with "unloaded tree retained discovery result";
   end if;
   Put_Line ("Unloaded: " & Tree.Toolchain_Status'Image);
end Main;
