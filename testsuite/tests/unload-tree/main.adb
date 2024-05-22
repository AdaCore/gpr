with Ada.Text_IO;
with Ada.Strings.Fixed;

pragma Warnings (Off);
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;

   procedure Display (Prj : Project.View.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Text_IO.Put
           ("A:   " & Image (A.Name.Id.Attr));
         Text_IO.Put (" ->");

         for V of A.Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;

      for Source of Prj.Sources loop
         declare
            U : constant Optional_Name_Type := Source.Unit.Name;
         begin
            Output_Filename (Source.Path_Name.Value);

            Text_IO.Set_Col (16);
            Text_IO.Put ("   language: " & Image (Source.Language));

            Text_IO.Set_Col (33);
            Text_IO.Put
              ("   Kind: "
               & GPR2.Image (Source.Kind));

            if U /= "" then
               Text_IO.Put ("   unit: " & String (U));
            end if;

            Text_IO.New_Line;
         end;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "unload-tree";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

   Prj1, Prj2 : Project.Tree.Object;
   Prj_Copy   : Project.Tree.Object;
   Opt1, Opt2 : Options.Object;
   Log        : GPR2.Log.Object;

begin
   Opt1.Add_Switch (Options.P, "first.gpr");
   Opt1.Add_Switch (Options.X, "LSRC=one");

   Opt2.Add_Switch (Options.P, "second.gpr");
   Opt2.Add_Switch (Options.X, "LSRC=two");

   if not Prj1.Load (Opt1, Absent_Dir_Error => No_Error)
     or else not Prj2.Load (Opt2, Absent_Dir_Error => No_Error)
   then
      Text_IO.Put_Line ("Fatal error, exiting");
      return;
   end if;

   Prj1.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

   Prj2.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

   Text_IO.Put_Line ("**************** Iterator Prj1");

   for C in Project.Tree.Iterate (Prj1) loop
      Display (Project.Tree.Element (C));
      if Project.Tree.Is_Root (C) then
         Text_IO.Put_Line ("   is root");
      end if;
   end loop;

   Prj_Copy := Prj1;
   Prj1.Unload;

   if Prj1.Is_Defined then
      Text_IO.Put_Line ("Not completely unloaded");
   end if;

   if not Prj_Copy.Is_Defined then
      Text_IO.Put_Line ("Copy cleared unexpectedly");
   elsif Prj_Copy.Root_Project.Is_Defined then
      Text_IO.Put_Line ("Prj_Copy not unloaded");
   end if;

   Text_IO.Put_Line ("**************** Iterator Prj2");

   for C in Project.Tree.Iterate (Prj2) loop
      Display (Project.Tree.Element (C));
      if Project.Tree.Is_Root (C) then
         Text_IO.Put_Line ("   is root");
      end if;
   end loop;
end Main;
