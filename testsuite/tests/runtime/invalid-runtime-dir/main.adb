with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Message;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Log;

procedure Main is
   Opt : GPR2.Options.Object;

   procedure Check (Title : String; With_Runtime : Boolean := True);
   procedure Write_File (Name : String; Content : String := "");

   procedure Check (Title : String; With_Runtime : Boolean := True) is
      Tree   : GPR2.Project.Tree.Object;
      Loaded : Boolean;
   begin
      Put_Line (Title);
      Loaded := Tree.Load
        (Opt,
         With_Runtime => With_Runtime,
         Reporter     => GPR2.Reporter.Log.Create);
      Put_Line ("Loaded: " & Loaded'Image);
      Put_Line ("Has runtime: " & Tree.Has_Runtime_Project'Image);

      for Msg of Tree.Log_Messages.all loop
         if Msg.Level in GPR2.Message.Any_Error then
            Put_Line (Msg.Format);
         end if;
      end loop;
   end Check;

   procedure Write_File (Name : String; Content : String := "") is
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      Put_Line (File, Content);
      Close (File);
   end Write_File;

begin
   Opt.Add_Switch (GPR2.Options.P, "demo.gpr");
   Opt.Add_Switch (GPR2.Options.Config, "config.cgpr");

   Check ("Missing runtime directory");

   Ada.Directories.Create_Directory ("dir");
   Write_File ("dir/_dummy");
   Check ("Unrelated directory");
   Check ("Runtime not requested", With_Runtime => False);

   Ada.Directories.Create_Directory ("dir/adalib");
   Check ("Missing runtime sources");

   Ada.Directories.Delete_Directory ("dir/adalib");
   Ada.Directories.Create_Directory ("dir/adainclude");
   Check ("Missing runtime objects");

   Ada.Directories.Create_Directory ("dir/adalib");
   Check ("Standard runtime");

   Ada.Directories.Delete_Directory ("dir/adainclude");
   Ada.Directories.Create_Directory ("dir/sources");
   Write_File ("dir/ada_source_path", "sources");
   Check ("Runtime source path");

   Ada.Directories.Delete_Directory ("dir/adalib");
   Ada.Directories.Create_Directory ("dir/objects");
   Write_File ("dir/ada_object_path", "objects");
   Check ("Runtime path files");
end Main;
