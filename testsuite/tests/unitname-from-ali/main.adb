with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info;
with GPR2.Project.Unit_Info.Set;

procedure Main is
   Tree : GPR2.Project.Tree.Object;

   use type GPR2.Filename_Optional;
begin
   Tree.Load_Autoconf
     (GPR2.Path_Name.Create_File ("p.gpr"), GPR2.Context.Empty);
   Tree.Update_Sources (With_Runtime => True);
   Tree.Log_Messages.Output_Messages (Information => False);

   for C in Tree.Runtime_Project.Units.Iterate loop
      declare
         Key : constant GPR2.Name_Type :=
                 GPR2.Project.Unit_Info.Set.Set.Key (C);
         U   : constant GPR2.Project.Unit_Info.Object :=
                 GPR2.Project.Unit_Info.Set.Set.Element (C);
      begin
         if U.Has_Spec and then U.Spec.Source.Simple_Name = "a-direio.ads" then
            Put_Line ("Key:");
            Put_Line (" - " & String (Key));
            Put_Line ("Name:");
            Put_Line (" - " & String (U.Name));
            if U.Has_Spec then
               Put_Line ("Spec:");
               Put_Line (" - " & String (U.Spec.Source.Simple_Name));
            end if;
            if U.Has_Body then
               Put_Line ("Body:");
               Put_Line (" - " & String (U.Main_Body.Source.Simple_Name));
            end if;
         end if;
      end;
   end loop;

   declare
      N : constant GPR2.Name_Type := "ada.direct_io";
      U : GPR2.Project.Unit_Info.Object := Tree.Runtime_Project.Unit (N);
   begin
      Put_Line (String (N) & " found: " & U.Is_Defined'Image);
   end;
end Main;
