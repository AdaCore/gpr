with GPR2;
with GPR2.KB;
with GPR2.Log;
with GPR2.Project.Configuration; use GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Path_Name; use GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   KB   : GPR2.KB.Object := GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   Cfg  : Ada.Strings.Unbounded.Unbounded_String;
   Msg  : GPR2.Log.Object;
   F    : Ada.Text_IO.File_Type;

   Pseudo_Rts : GPR2.Name_Type :=
     GPR2.Name_Type (Get_Current_Dir & Dir_Separator & "pseudo_rts");

begin
   Cfg := KB.Configuration
     (Description_Set'(1 => Create (Language => GPR2.Ada_Language,
                                    Runtime => Pseudo_Rts)),
      GPR2.KB.Default_Target,
      Msg);

   Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, "cfg.cgpr");
   Ada.Text_IO.Put_Line (F, Ada.Strings.Unbounded.To_String (Cfg));
   Ada.Text_IO.Close (F);

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj1.gpr"),
      Context  => GPR2.Context.Empty);
   Tree.Log_Messages.Output_Messages (Information => False);

   Ada.Text_IO.Put_Line
     (String
        (Tree.Runtime_Project.Path_Name.Containing_Directory.Simple_Name));

   Tree.Unload;

   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
        ("prj2.gpr"),
      Context  => GPR2.Context.Empty);
   Tree.Log_Messages.Output_Messages (Information => False);

   Ada.Text_IO.Put_Line
     (String
        (Tree.Runtime_Project.Path_Name.Containing_Directory.Simple_Name));

   Tree.Unload;
end Main;
