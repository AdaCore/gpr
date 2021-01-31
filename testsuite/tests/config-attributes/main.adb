with GPR2; use GPR2;
with GPR2.Project.Tree;
with GPR2.Context; use GPR2.Context;
with GPR2.Log;

with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Main is

   Ctx          : Context.Object := Context.Empty;
   Project_Tree : Project.Tree.Object;

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
      for C in Project_Tree.Log_Messages.Iterate
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

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("prj.gpr"),
      Context           => Ctx);

   Check_Messages;

   Project_Tree.Unload;

   Ctx.Include ("TC_NAME", "toto");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("prj.gpr"),
      Context           => Ctx);

   Check_Messages (Name => True);

   Project_Tree.Unload;

   Ctx.Clear;
   Ctx.Include ("TC_PATH", "toto");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("prj.gpr"),
      Context           => Ctx);

   Check_Messages (Path => True);

   Project_Tree.Unload;

   Ctx.Clear;
   Ctx.Include ("TC_VERSION", "toto");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("prj.gpr"),
      Context           => Ctx);

   Check_Messages (Version => True);

   Project_Tree.Unload;
   Ctx.Include ("TC_NAME", "toto");
   Ctx.Include ("TC_PATH", "toto");

   Project_Tree.Load_Autoconf
     (Filename          => Project.Create ("prj.gpr"),
      Context           => Ctx);


   Check_Messages (True, True, True);
   Project_Tree.Unload;

end Main;
