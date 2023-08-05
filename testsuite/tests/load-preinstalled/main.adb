with GPR2;
with GPR2.Log;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name; use GPR2.Path_Name;
with GPR2.Containers; use GPR2.Containers;
with GPR2.Environment;

with Ada.Directories;
with Ada.Text_IO;

with GNAT.OS_Lib;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Msg  : GPR2.Log.Object;
   F    : Ada.Text_IO.File_Type;
   RTS  : Lang_Value_Map := Lang_Value_Maps.Empty_Map;
   Ctx  : GPR2.Context.Object := GPR2.Context.Empty;

   Env : GPR2.Environment.Object := GPR2.Environment.Process_Environment;
begin
   Env.Insert ("GPR_PROJECT_PATH", "");
   Env.Set_Inherit (True);

   Ctx.Insert ("VSB_DIR", ".");
   RTS.Insert (GPR2.Ada_Language, "rtp");

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("preinstalled.gpr", No_Resolution),
      Context  => Ctx,
      Target => "x86_64-wrs-vxworks7",
      Language_Runtimes => RTS,
      Environment => Env);

   Ada.Text_IO.Put_Line ("Cross:");
   Ada.Text_IO.Put_Line (Tree.Root_Project.Path_Name.Value);

   Tree.Unload;

   --  For the native case we need to manipulate the path a bit to get rid of
   --  actual native compilers.
   Env.Insert
     ("PATH",
      Ada.Directories.Current_Directory
      & GNAT.OS_Lib.Directory_Separator
      & "fake-ada-native"
      & GNAT.OS_Lib.Directory_Separator
      & "bin");

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("preinstalled.gpr", No_Resolution),
      Context  => GPR2.Context.Empty,
      Target => "all",
      Environment => Env);

   Ada.Text_IO.Put_Line ("Native:");
   Ada.Text_IO.Put_Line (Tree.Root_Project.Path_Name.Value);

   Tree.Unload;
exception
   when others =>
      for M of Tree.Log_Messages.all loop
         Ada.Text_IO.Put_Line (M.Format);
      end loop;
end Main;
