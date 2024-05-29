with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Check (Project_Name : String);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is

      procedure List_Sources (View : Project.View.Object);

      procedure Copy_Source (From : String;
                             Name : String);
      --  Copy source file from srcp directory to src

      ------------------
      -- List_Sources --
      ------------------

      procedure List_Sources (View : Project.View.Object) is
      begin
         Text_IO.Put_Line ("----------");

         for Source of View.Sources loop
            declare
               U : constant Optional_Name_Type := Source.Unit.Name;
            begin
               Output_Filename (Source.Path_Name.Value);

               Text_IO.Set_Col (20);
               Text_IO.Put ("   language: " & Image (Source.Language));

               Text_IO.Set_Col (36);
               Text_IO.Put
                 ("   Kind: " & Source.Kind'Image);

               if U /= "" then
                  Text_IO.Set_Col (60);
                  Text_IO.Put ("unit: " & String (U));

                  if Source.Kind = S_Separate then
                     Text_IO.Put (" (" & String (Source.Unit.Separate_Name) & ")");
                  end if;
               end if;

               Text_IO.New_Line;
            end;
         end loop;
      end List_Sources;

      -----------------
      -- Copy_Source --
      -----------------

      procedure Copy_Source (From : String;
                             Name : String) is
      begin
         Ada.Directories.Copy_File (From & "/" & Name, "src/" & Name);
      end Copy_Source;

      Prj  : Project.Tree.Object;
      Opt  : Options.Object;

   begin
      Opt.Add_Switch (options.P, Project_Name);

      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Text_IO.Put_Line ("Project: " & String (Prj.Root_Project.Name));

      --  Create api-call.adb as a separate
      Copy_Source ("src1", "api.ads");
      Copy_Source ("src1", "api.adb");
      Copy_Source ("src1", "api-call.adb");

      Prj.Update_Sources;

      List_Sources (Prj.Root_Project);

      --  Wait 1 second so that timestamp check works
      delay (1.0);

      Copy_Source ("src2", "api.ads");
      Copy_Source ("src2", "api.adb");
      Copy_Source ("src2", "api-call.adb");

      Prj.Update_Sources;

      List_Sources (Prj.Root_Project);
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "source4";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

begin
   Check ("demo.gpr");
end Main;
