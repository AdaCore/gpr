with GPR2; use GPR2;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.Log;

with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Main is

   Project_Tree : Project.Tree.Object;
   Opt          : Options.Object;

   procedure Check_Messages
     (Name    : Boolean := False;
      Path    : Boolean := False;
      Version : Boolean := False);

   procedure Check_Messages
     (Name    : Boolean := False;
      Path    : Boolean := False;
      Version : Boolean := False)
   is
      use Ada.Strings.Fixed;
   begin
      for C in Project_Tree.Configuration.Log_Messages.Iterate
        (Information => False,
         Warning     => True,
         Error	     => True,
         Read        => True,
         Unread      => True)
      loop
         declare
            Msg      : constant String := GPR2.Log.Element (C).Format;
            Expected : Boolean := False;
         begin
            if Index (Msg, "can't find a toolchain") > 0 then
               if not (Index (Msg, "name '") > 0 xor Name)
                 and then not (Index (Msg, "path '") > 0 xor Path)
                 and then not (Index (Msg, "version '") > 0 xor Version)
               then
                  Expected := True;
               end if;
            end if;
            if Expected then
               Ada.Text_IO.Put_Line
                 (Msg (Index (Msg, "default runtime") + 17 .. Msg'Last));
            else
               Ada.Text_IO.Put_Line ("unexpected message: " & Msg);
            end if;
         end;
      end loop;
   end Check_Messages;

begin

   Opt.Add_Switch (Options.P, "prj.gpr");
   if Project_Tree.Load (Opt, Absent_Dir_Error => No_Error, Verbosity => Project.Tree.Quiet) then
      Check_Messages;
   end if;
   Project_Tree.Unload;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "prj.gpr");
   Opt.Add_Switch (Options.X, "TC_NAME=toto");
   if Project_Tree.Load (Opt, Absent_Dir_Error => No_Error, Verbosity => Project.Tree.Quiet) then
      Check_Messages (Name => True);
   end if;
   Project_Tree.Unload;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "prj.gpr");
   Opt.Add_Switch (Options.X, "TC_PATH=toto");
   if Project_Tree.Load (Opt, Absent_Dir_Error => No_Error, Verbosity => Project.Tree.Quiet) then
      Check_Messages (Path => True);
   end if;
   Project_Tree.Unload;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "prj.gpr");
   Opt.Add_Switch (Options.X, "TC_VERSION=toto");
   if Project_Tree.Load (Opt, Absent_Dir_Error => No_Error, Verbosity => Project.Tree.Quiet) then
      Check_Messages (Version => True);
   end if;
   Project_Tree.Unload;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "prj.gpr");
   Opt.Add_Switch (Options.X, "TC_NAME=toto");
   Opt.Add_Switch (Options.X, "TC_PATH=toto");
   Opt.Add_Switch (Options.X, "TC_VERSION=toto");
   if Project_Tree.Load (Opt, Absent_Dir_Error => No_Error, Verbosity => Project.Tree.Quiet) then
      Check_Messages (True, True, True);
   end if;
   Project_Tree.Unload;

end Main;
