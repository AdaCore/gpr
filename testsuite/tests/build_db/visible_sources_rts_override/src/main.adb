--  reprod.adb
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;

procedure Main is
   Project_Name    : constant String := (if Argument_Count = 3
                                         then Argument (1)
                                         else "tree/prj/prj.gpr");
   Src_Simple_Name : constant String := (if Argument_Count = 3
                                         then Argument (2)
                                         else "s-memory.adb");
   Src_Unit_Name   : constant String := (if Argument_Count = 3
                                         then Argument (3)
                                         else "System.Memory");

   O : GPR2.Options.Object;
   T : GPR2.Project.Tree.Object;
   V : GPR2.Project.View.Object;
   Src : GPR2.Build.Compilation_Unit.Object;

begin
   Put_Line ("Looking for " & Src_Simple_Name);
   O.Add_Switch (GPR2.Options.P, Project_Name);
   if not T.Load
     (O,
      Absent_Dir_Error     => GPR2.No_Error,
      With_Runtime         => True,
      Artifacts_Info_Level => GPR2.Sources_Units)
   then
      raise Program_Error;
   end if;
   V := T.Root_Project;

   Put_Line ("From ""Visible_Sources"":");
   for Source of V.Visible_Sources loop
      if Source.Is_Defined
        and then String (Source.Path_Name.Simple_Name) = Src_Simple_Name
      then
         Ada.Text_IO.Put_Line (Source.Path_Name.String_Value);
      end if;
   end loop;

   Put_Line ("From ""Visible_Source"":");
   declare
      Ambiguous : Boolean;
      Source    : GPR2.Build.Source.Object :=
                    V.Visible_Source
                      (GPR2.Simple_Name (Src_Simple_Name),
                       Ambiguous);
   begin
      if Source.Is_Defined then
         Ada.Text_IO.Put_Line (Source.Path_Name.String_Value);
         if Ambiguous then
            Ada.Text_IO.Put_Line (" (ambiguous result)");
         end if;
      else
         Ada.Text_IO.Put_Line ("not found...");
      end if;
   end;

   Put_Line ("Looking up " & Src_Unit_Name & " owning project:");
   Src := V.Unit ("System.Memory");
   Put_Line (String (Src.Owning_View.Name));
   Put_Line ("Main Source:");
   Put_Line (Src.Main_Body.Source.String_Value);
end Main;
