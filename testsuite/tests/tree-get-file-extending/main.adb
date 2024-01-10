with Ada.Text_IO;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Log;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   Project_Name : constant GPR2.Filename_Type := "extending";
   use GPR2;
   Log  : GPR2.Log.Object;
begin
   Tree.Load_Autoconf
     (Filename     => GPR2.Path_Name.Create_File
        (GPR2.Project.Ensure_Extension (Project_Name),
         GPR2.Path_Name.No_Resolution),
      Context      => Context,
      With_Runtime => True);

   Tree.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

   declare
      use Ada.Text_IO;

      File : constant GPR2.Path_Name.Object :=
               Tree.Get_File ("hello.adb", Use_Object_Path => False);
   begin
      if not File.Is_Defined then
         Put_Line ("Overriding source not found");
      else
         Put_Line ("OK");
      end if;
   end;

   Tree.Unload;
end Main;