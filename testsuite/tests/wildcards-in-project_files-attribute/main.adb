with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure main is
   function Get_File_Names_Case_Sensitive return Integer
     with Import, Convention => C,
     External_Name => "__gnat_get_file_names_case_sensitive";
   File            : Ada.Text_IO.File_Type;
   Absolute1       : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File ("files/absolute1.gpr");
   Tree            : GPR2.Project.Tree.Object;
   Opt             : GPR2.Options.Object;
   Aggr_Found      : Boolean := False;
   Absolute1_Found : Boolean := False;
   Regexp3_Found   : Boolean := False;
   Relative2_Found : Boolean := False;
   Relative4_Found : Boolean := False;
   Project5_Found  : Boolean := False;
   Project6_Found  : Boolean := Get_File_Names_Case_Sensitive /= 0;
   Project7_Found  : Boolean := False;
   Regexp8_Found   : Boolean := False;
   Project9_Found  : Boolean := Get_File_Names_Case_Sensitive /= 0;
   Project10_Found : Boolean := False;
   Runtime_Found   : Boolean := False;

begin
   Ada.Text_IO.Create (File => File, Name => "files/aggr.gpr");
   Ada.Text_IO.Put_Line (File, "aggregate project Aggr is");
   Ada.Text_IO.Put_Line (File, "for Project_Files use (");
   Ada.Text_IO.Put_Line (File, """" & Absolute1.String_Value & """,");
   --  test absolute path support
   Ada.Text_IO.Put_Line (File, """relative2.gpr"",");
   --  test relative path without dir support
   Ada.Text_IO.Put_Line (File, """tree1/relative4.gpr"",");
   --  test relative path with dir support
   Ada.Text_IO.Put_Line (File, """*3.gpr"",");
   --  test regexp without dir support
   Ada.Text_IO.Put_Line (File, """tree2/?e*.gpr"",");
   --  test regexp with dir support
   Ada.Text_IO.Put_Line (File, """tree2/*9.gpr"",");
   --  test case insensitive on windows
   Ada.Text_IO.Put_Line (File, """**/*8.gpr"",");
   --  test recursive without dir support
   Ada.Text_IO.Put_Line (File, """tree3/**/project7.gpr"",");
   --  test recursive with dir support
   Ada.Text_IO.Put_Line (File, """notfound.gpr"",");
   Ada.Text_IO.Put_Line (File, """notexist/relative4.gpr"",");
   Ada.Text_IO.Put_Line (File, """invalid*/**/project7.gpr"");");
   --  test aggregated file not found support
   Ada.Text_IO.Put_Line (File, "end Aggr;");
   Ada.Text_IO.Close (File);

   --  Load the project (if defined) and its configuration

   Opt.Add_Switch (GPR2.Options.P, "files/aggr.gpr");
   if not Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error) then
      return;
   end if;

   for Cursor in Tree.Iterate loop
      declare
         use type GPR2.Name_Type;
         Name : constant GPR2.Name_Type :=
                  GPR2.Project.Tree.Element (Cursor).Name;
      begin
         if Name = "Aggr" and then not Aggr_Found then
            Aggr_Found := True;
         elsif Name = "Absolute1" and then not Absolute1_Found then
            Absolute1_Found := True;
         elsif Name = "Regexp3" and then not Regexp3_Found then
            Regexp3_Found := True;
         elsif Name = "Relative2" and then not Relative2_Found then
            Relative2_Found := True;
         elsif Name = "Relative4" and then not Relative4_Found then
            Relative4_Found := True;
         elsif Name = "Project5" and then not Project5_Found then
            Project5_Found := True;
         elsif Name = "Project6" and then not Project6_Found then
            Project6_Found := True;
         elsif Name = "Project7" and then not Project7_Found then
            Project7_Found := True;
         elsif Name = "Regexp8" and then not Regexp8_Found then
            Regexp8_Found := True;
         elsif Name = "Project9" and then not Project9_Found then
            Project9_Found := True;
         elsif Name = "Project10" and then not Project10_Found then
            Project10_Found := True;
         elsif Name = "Runtime" then
            Runtime_Found := True;
         else
            Ada.Text_IO.Put_Line ("Unexpected view:" & String (Name));
         end if;
      end;
   end loop;

   if not Aggr_Found
     or else not Absolute1_Found
     or else not Regexp3_Found
     or else not Relative2_Found
     or else not Relative4_Found
     or else not Project5_Found
     or else not Project7_Found
     or else not Regexp8_Found
     or else not Runtime_Found
   then
      Ada.Text_IO.Put_Line ("Missing view");
   end if;
end main;
