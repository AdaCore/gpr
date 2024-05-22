with Ada.Text_IO;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Build.Source.Sets;
with GPR2.Project.View;
with GPR2.Project.Tree;

--  with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Object;
                              Base_Dir : Path_Name.Object);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj  : Project.Tree.Object;
      Opt  : Options.Object;
      View : Project.View.Object;
      Log  : GPR2.Log.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         Prj.Update_Sources (Messages => Log);
         Log.Output_Messages;

         View := Prj.Root_Project;
         Text_IO.Put_Line ("Project: " & String (View.Name));

         for Source of View.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit.Name;
            begin
               Output_Filename
                 (Source.Path_Name,
                  Prj.Root_Project.Path_Name.Containing_Directory);

               Text_IO.Set_Col (16);
               Text_IO.Put ("   language: " & Image (Source.Language));

               Text_IO.Set_Col (33);
               Text_IO.Put
                 ("   Kind: " & Source.Kind'Image);

               if U /= "" then
                  Text_IO.Put ("   unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end if;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Object;
                              Base_Dir : Path_Name.Object)
   is
      Rel : constant Filename_Type := Filename.Relative_Path (Base_Dir);
   begin
      --  Remove the starting "./"
      Text_IO.Put (" > " & String (Rel (3 .. Rel'Last)));
   end Output_Filename;

begin
   Check ("simple.gpr");
end Main;
