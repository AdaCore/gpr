with Ada.Strings.Fixed;
with Ada.Text_IO;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   procedure Output_Sources (Tree : Project.Tree.Object);
   --  output the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "load";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

   --------------------
   -- Output_Sources --
   --------------------

   procedure Output_Sources (Tree : Project.Tree.Object) is
   begin
      Text_IO.Put_Line ("== SOURCES ==");
      for Source of Tree.Root_Project.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit.Full_Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (20);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (36);
            Text_IO.Put ("   Kind: " & Source.Kind'Image);

            if U /= "" then
               Text_IO.Set_Col (57);
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Output_Sources;

   Opt  : Options.Object;
   Tree : Project.Tree.Object;
begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Text_IO.Put_Line ("Failed to load the project");

         return;
   end if;

   Output_Sources (Tree);
   Tree.Unload;

   if not Tree.Load
     (Opt,
      Absent_Dir_Error     => No_Error,
      Artifacts_Info_Level => Sources_Only)
   then
      Text_IO.Put_Line ("Failed to load the project");

      return;
   end if;

   Output_Sources (Tree);
   Tree.Unload;

   if not Tree.Load
     (Opt, Absent_Dir_Error  => No_Error)
   then
      Text_IO.Put_Line ("Failed to load the project");

      return;
   end if;

   if not Tree.Update_Sources then
      Text_IO.Put_Line ("Failed to update sources");
   end if;

   Output_Sources (Tree);
end Main;
