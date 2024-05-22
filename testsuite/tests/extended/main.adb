with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);

      if Prj.Kind /= K_Abstract then
         Text_IO.Put
           ("obj_dir=" & String (Prj.Object_Directory.Simple_Name) & ' ');
      end if;

      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Prj.Kind in With_Object_Dir_Kind then
         for Source of Prj.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit.Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Set_Col (16);
               Text_IO.Put ("   language: " & Image (Source.Language));

               Text_IO.Set_Col (33);
               Text_IO.Put ("   Kind: " & Source.Kind'Image);

               if U /= "" then
                  Text_IO.Put ("   unit: " & String (U));
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end if;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "extended";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Opt        : GPR2.Options.Object;
   Log        : GPR2.Log.Object;
begin
   Opt.Add_Switch (Options.P, "prj1.gpr");
   if Prj1.Load (Opt, Absent_Dir_Error => No_Error) then
      Prj1.Update_Sources (Messages => Log);
      Log.Output_Messages;
   end if;

   Opt := Options.Empty_Options;
   Opt.Add_Switch (Options.P, "prj2.gpr");
   if Prj2.Load (Opt, Absent_Dir_Error => No_Error) then
      Prj2.Update_Sources (Messages => Log);
      Log.Output_Messages;
   end if;

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for P of Prj1 loop
      Display (P, Full => False);
   end loop;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Prj2.Iterate
     (Kind => (I_Project | I_Imported | I_Recursive => True, others => False))
   loop
      Display (Project.Tree.Element (C), Full => False);
   end loop;
end Main;
