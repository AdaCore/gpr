with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Environment;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Test is

   PATH : constant String :=
            Ada.Environment_Variables.Value ("PATH", "");
   GPR_PROJECT_PATH : constant String :=
                        Ada.Environment_Variables.Value
                          ("GPR_PROJECT_PATH", "");
   ADA_PROJECT_PATH : constant String :=
                        Ada.Environment_Variables.Value
                          ("ADA_PROJECT_PATH", "");
   function Environment
     (Root : GPR2.Filename_Optional) return GPR2.Environment.Object
   is
      Environment : GPR2.Environment.Object;
      Project_Path_File : constant GPR2.Path_Name.Object :=
                            GPR2.Path_Name.Create_File
                              (Name      => "gpr_project_path_file.txt",
                               Directory => Root);
      File : Ada.Text_IO.File_Type;
      Content : constant String :=
                  GPR2.Path_Name.Create_Directory
                    (Name      => "gpr_project_path_file",
                     Directory => Root).String_Value;
   begin
      Environment.Insert ("PATH", PATH);
      Ada.Environment_Variables.Set ("PATH", "");

      Environment.Insert
        ("ADA_PROJECT_PATH",
         GPR2.Path_Name.Create_Directory
           (Name      => "ada_project_path",
            Directory => Root).String_Value
         & GNAT.OS_Lib.Path_Separator
         & ADA_PROJECT_PATH);
      Ada.Environment_Variables.Set ("ADA_PROJECT_PATH", "");

      Environment.Insert
        ("GPR_PROJECT_PATH",
         GPR2.Path_Name.Create_Directory
           (Name      => "gpr_project_path",
            Directory => Root).String_Value
         & GNAT.OS_Lib.Path_Separator
         & GPR_PROJECT_PATH);
      Ada.Environment_Variables.Set ("GPR_PROJECT_PATH", "");

      Ada.Text_IO.Open
        (File => File,
         Mode => Ada.Text_IO.Out_File,
         Name => Project_Path_File.String_Value);
      Ada.Text_IO.Put_Line
        (File => File,
         Item => Content);
      Ada.Text_IO.Close (File);
      Environment.Insert
        ("GPR_PROJECT_PATH_FILE", Project_Path_File.String_Value);
      Ada.Environment_Variables.Set ("GPR_PROJECT_PATH_FILE", "");

      return Environment;
   end Environment;

   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;
   Res     : Boolean;
begin

   --  testing load_autoconf
   Options.Add_Switch (GPR2.Options.P, "./files/prj");
   Options.Add_Switch (GPR2.Options.AP, "./files/registered");
   Options.Add_Switch (GPR2.Options.Autoconf, "./files/autoconf.cgpr");
   Res := Tree.Load
     (Options,
      Absent_Dir_Error       => GPR2.No_Error,
      Allow_Implicit_Project => False,
      Environment            => Environment ("files"));

   --  testing load_autoconf on default project
   Options := GPR2.Options.Empty_Options;
   Ada.Directories.Set_Directory ("./files");
   Options.Add_Switch (GPR2.Options.AP, "./registered");
   Res := Tree.Load
     (Options,
      Absent_Dir_Error       => GPR2.No_Error,
      Allow_Implicit_Project => True,
      Environment            => Environment ("."));
end Test;
