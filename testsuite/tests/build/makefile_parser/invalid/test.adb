with Ada.Exceptions;

with GPR2;
with GPR2.Build.Makefile_Parser;
with GPR2.Containers;
with GPR2.Path_Name;

with GNATCOLL.Utils;

with Test_Assert; use Test_Assert;

function Test return Integer is
   package Parser renames GPR2.Build.Makefile_Parser;

   Object_File : constant GPR2.Path_Name.Object :=
     GPR2.Path_Name.Create_File ("foo.o");

   procedure Check_Invalid (Name : String) is
      Path : constant GPR2.Path_Name.Object := GPR2.Path_Name.Create_File
        (GPR2.Filename_Type ("files/" & Name & ".d"));
      Deps : GPR2.Containers.Filename_Set;
   begin
      Assert (Path.Exists, "Fixture exists: " & Name);
      Deps.Include ("existing.h");

      for Strict in Boolean loop
         begin
            Assert
              (not Parser.Dependencies
                 (Path, Object_File, Deps, Strict => Strict),
               "Reject " & Name);
            Assert (not Strict, "Strict parsing must raise: " & Name);
         exception
            when E : others =>
               Assert (Strict, "Only strict parsing raises: " & Name);
               Assert
                 (GNATCOLL.Utils.Starts_With
                    (Ada.Exceptions.Exception_Message (E),
                     "dependency file " & String (Path.Simple_Name)
                     & " has wrong format"),
                  "Report dependency parsing error: " & Name);
         end;
         Assert
           (Integer (Deps.Length) = 1 and then Deps.Contains ("existing.h"),
            "Preserve caller entries without partial results: " & Name);
      end loop;
   end Check_Invalid;
begin
   Check_Invalid ("empty");
   Check_Invalid ("comments_only");
   Check_Invalid ("no_dependencies");
   Check_Invalid ("only_timestamp");
   Check_Invalid ("wrong_object");
   Check_Invalid ("missing_colon");
   Check_Invalid ("invalid_filename");
   Check_Invalid ("partial_dependencies");
   Check_Invalid ("unfinished_continuation");
   Check_Invalid ("missing_continued_dependency");
   Check_Invalid ("unterminated_line");

   return Report;
end Test;
