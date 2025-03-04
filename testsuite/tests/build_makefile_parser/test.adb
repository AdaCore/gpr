with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.Build.Makefile_Parser;
with GPR2.Containers;
with GPR2.Path_Name;

with GNATCOLL.OS; use GNATCOLL.OS;
with GNATCOLL.OS.Constants;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with Test_Assert; use Test_Assert;

function Test return Integer is
   Directory    : constant Filename_Optional :=
                    "makefile_db";

   Is_Windows_Host : constant Boolean :=
                       Constants.OS = Windows with Warnings => Off;
   Host            : constant Filename_Type :=
                       (if Is_Windows_Host then "_win" else "");

   Makefile_Toto_7_X_2   : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File ("toto_7.X.2" & Host & ".d", Directory);
   Makefile_Toto_5_04a1  : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File ("toto_5.04a1" & Host & ".d", Directory);
   Makefile_Toto_RP      : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_relative_path" & Host & ".d", Directory);
   Makefile_Toto_MLCL    : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_multiline_commented_line" & Host & ".d", Directory);
   Makefile_Toto_EC      : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_escaped_char" & Host & ".d", Directory);
   Makefile_Toto_Win     : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_windows_format" & Host & ".d", Directory);
   Makefile_Toto_Error1  : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_wrong_object" & Host & ".d", Directory);
   Makefile_Toto_Error2  : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File ("toto_no_colon" & Host & ".d", Directory);
   Makefile_Toto_Error3  : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File ("toto_no_colon2" & Host & ".d", Directory);
   Makefile_Toto_Error4  : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File
       ("toto_separator_without_dependencies" & Host & ".d", Directory);

   Object_Foo            : constant GPR2.Path_Name.Object
     := GPR2.Path_Name.Create_File ("foo.o", Directory);

   All_Deps      : GPR2.Containers.Filename_Set;
   Nb_Dep        : Integer := 0;


   Path_Prefix    : constant Filename_Optional :=
                     (if Is_Windows_Host then "C:\" else "/");
   Path_Separator : constant Filename_Optional :=
                     (if Is_Windows_Host then "\" else "/");

   DepsA_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "aaa.c";
   DepsA_R_C       : constant Filename_Optional :=
                       "../" & "titi/tata/tutu" & Path_Separator & "aaa.c";
   DepsB_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "bbb.c";
   DepsC_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "ccc.c";
   DepsD_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "ddd.c";
   DepsE_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "eee.c";
   DepsF_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "fff.c";
   DepsG_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "ggg.c";
   DepsH_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "hhh.c";
   DepsI_C         : constant Filename_Optional :=
                       Path_Prefix & "titi" & Path_Separator & "tata"
                       & Path_Separator & "tutu" & Path_Separator & "iii.c";

   DepsAt_C              : constant Filename_Optional :=
                             Path_Prefix & "titi" & Path_Separator & "ta@ta"
                             & Path_Separator & "tutu" & Path_Separator
                             & "aaa.c";
   DepsSpaceA_C          : constant Filename_Optional :=
                             Path_Prefix & "titi" & Path_Separator & "ta ta"
                             & Path_Separator & "tutu" & Path_Separator
                             & "aaa.c";
   DepsDoubleBackSlash_C : constant Filename_Optional :=
                             "\\" & Path_Prefix & "titi" & Path_Separator
                             & "tata" & Path_Separator & "tutu"
                             & Path_Separator & "ddd.c";
begin
   Put_Line ("Step 1 - Test 5.04a1 format");
   All_Deps.Clear;
   Nb_Dep := 2;
   Assert
     (GPR2.Build.Makefile_Parser.Dependencies
        (Makefile_Toto_5_04a1, Object_Foo, All_Deps, Strict => True),
      "Parsing " & String (Makefile_Toto_5_04a1.Simple_Name));
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));
   Assert
     (All_Deps.Contains (DepsB_C), "contains " & String (DepsB_C));

   Put_Line ("Step 2 - Test 7.X.2 format");
   All_Deps.Clear;
   Nb_Dep := 2;
   Assert
     (GPR2.Build.Makefile_Parser.Dependencies
        (Makefile_Toto_7_X_2, Object_Foo, All_Deps, Strict => True),
      "Parsing " & String (Makefile_Toto_7_X_2.Simple_Name));
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));
   Assert
     (All_Deps.Contains (DepsB_C), "contains " & String (DepsB_C));


   Put_Line ("Step 3 - Test relative paths in makefile (single line)");
   All_Deps.Clear;
   if Is_Windows_Host then
      Nb_Dep := 6;
   else
      Nb_Dep := 5;
   end if;
   Assert
     (GPR2.Build.Makefile_Parser.Dependencies
        (Makefile_Toto_RP, Object_Foo, All_Deps, Strict => True),
      "Parsing " & String (Makefile_Toto_RP.Simple_Name));
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   if Is_Windows_Host then
      Assert
        (All_Deps.Contains (DepsA_R_C), "contains " & String (DepsA_R_C));
   end if;
   Assert
     (All_Deps.Contains (DepsC_C), "contains " & String (DepsC_C));
   Assert
     (All_Deps.Contains (DepsD_C), "contains " & String (DepsD_C));
   Assert
     (All_Deps.Contains (DepsE_C), "contains " & String (DepsE_C));
   Assert
     (All_Deps.Contains (DepsH_C), "contains " & String (DepsH_C));
   Assert
     (All_Deps.Contains (DepsI_C), "contains " & String (DepsI_C));

   Put_Line ("Step 4 - Error test : wrong object name in makefile");
   All_Deps.Clear;
   Nb_Dep := 0;
   declare
      Error : constant String :=
                "dependency file " & String (Makefile_Toto_Error1.Simple_Name)
                & " has wrong format : expected object file name foo.o,"
                & " got wrong.o";
   begin
      Assert
        (not GPR2.Build.Makefile_Parser.Dependencies
           (Makefile_Toto_Error1, Object_Foo, All_Deps, Strict => True),
         "Parsing " & String (Makefile_Toto_Error1.Simple_Name));
   exception
      when E : others =>
         Assert
           (Ada.Exceptions.Exception_Message (E) = Error,
            "expected : <" & Error & ">, got : <"
            & Ada.Exceptions.Exception_Message (E) & ">");
   end;
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);

   Put_Line ("Step 5 - Error test : line with data but no object name");
   All_Deps.Clear;
   Nb_Dep := 1;
   declare
      Error : constant String :=
                "dependency file " & String (Makefile_Toto_Error2.Simple_Name)
                & " has wrong format : no colon";
   begin
      Assert
        (not GPR2.Build.Makefile_Parser.Dependencies
           (Makefile_Toto_Error2, Object_Foo, All_Deps, Strict => True),
         "Parsing " & String (Makefile_Toto_Error2.Simple_Name));
   exception
      when E : others =>
         Assert
           (Ada.Exceptions.Exception_Message (E) = Error,
            "expected : <" & Error & ">, got : <"
            & Ada.Exceptions.Exception_Message (E) & ">");
   end;
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));

   Put_Line ("Step 5 - Error test : line with data but no object name 2");
   All_Deps.Clear;
   Nb_Dep := 1;
   declare
      Error : constant String :=
                "dependency file " & String (Makefile_Toto_Error3.Simple_Name)
                & " has wrong format : no colon";
   begin
      Assert
        (not GPR2.Build.Makefile_Parser.Dependencies
           (Makefile_Toto_Error3, Object_Foo, All_Deps, Strict => True),
         "Parsing " & String (Makefile_Toto_Error3.Simple_Name));
   exception
      when E : others =>
         Assert
           (Ada.Exceptions.Exception_Message (E) = Error,
            "expected : <" & Error & ">, got : <"
            & Ada.Exceptions.Exception_Message (E) & ">");
   end;
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));

   Put_Line ("Step 6 - Error test : line separator without dependencies");
   All_Deps.Clear;
   Nb_Dep := 1;
   declare
      Error : constant String :=
                "dependency file " & String (Makefile_Toto_Error4.Simple_Name)
                & " has wrong format";
   begin
      Assert
        (not GPR2.Build.Makefile_Parser.Dependencies
           (Makefile_Toto_Error4, Object_Foo, All_Deps, Strict => True),
         "Parsing " & String (Makefile_Toto_Error4.Simple_Name));
   exception
      when E : others =>
         Assert
           (Starts_With (Ada.Exceptions.Exception_Message (E), Error),
            "expected : <" & Error & ">, got : <"
            & Ada.Exceptions.Exception_Message (E) & ">");
   end;
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));

   Put_Line ("Step 7 - Test multiline with commented lines");
   All_Deps.Clear;
   Nb_Dep := 4;
   Assert
     (GPR2.Build.Makefile_Parser.Dependencies
        (Makefile_Toto_MLCL, Object_Foo, All_Deps, Strict => True),
      "Parsing " & String (Makefile_Toto_MLCL.Simple_Name));
   Assert
     (Integer (All_Deps.Length), Nb_Dep,
      "contains the right number of dependencies :" & Nb_Dep'Img);
   Assert
     (All_Deps.Contains (DepsA_C), "contains " & String (DepsA_C));
   Assert
     (All_Deps.Contains (DepsB_C), "contains " & String (DepsB_C));
   Assert
     (All_Deps.Contains (DepsC_C), "contains " & String (DepsC_C));
   Assert
     (All_Deps.Contains (DepsF_C), "contains " & String (DepsF_C));

   Put_Line ("Step 8 - Test escaped character");
   All_Deps.Clear;
   Assert
     (GPR2.Build.Makefile_Parser.Dependencies
        (Makefile_Toto_EC, Object_Foo, All_Deps, Strict => True),
      "Parsing " & String (Makefile_Toto_EC.Simple_Name));
   if Is_Windows_Host then
      Nb_Dep := 3;
      Assert
        (Integer (All_Deps.Length), Nb_Dep,
         "contains the right number of dependencies :" & Nb_Dep'Img);
      Assert
        (All_Deps.Contains (DepsSpaceA_C), "contains "
         & String (DepsSpaceA_C));
      Assert
        (All_Deps.Contains (DepsDoubleBackSlash_C), "contains "
         & String (DepsDoubleBackSlash_C));
      Assert
        (All_Deps.Contains (DepsC_C), "contains "
         & String (DepsC_C));

   else
      Nb_Dep := 2;
      Assert
        (Integer (All_Deps.Length), Nb_Dep,
         "contains the right number of dependencies :" & Nb_Dep'Img);
      Assert
        (All_Deps.Contains (DepsAt_C), "contains " & String (DepsAt_C));
      Assert
        (All_Deps.Contains (DepsSpaceA_C), "contains "
         & String (DepsSpaceA_C));
   end if;

   return Report;
end Test;
