with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Context;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Build.Source.Sets;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Display (Prj : Project.View.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   procedure Output_Filename (Filename : Path_Name.Full_Name);

   ----------------------
   -- Changed_Callback --
   ----------------------

   procedure Changed_Callback (Prj : Project.View.Object) is
   begin
      Text_IO.Put_Line (">>> Changed_Callback for " & String (Prj.Name));
   end Changed_Callback;

   -------------
   -- Display --
   -------------

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      if Prj.Is_Runtime then
         return;
      end if;

      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      for A in Prj.Attributes (With_Defaults => False, With_Config => False).Iterate loop
         Text_IO.Put
           ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
         Text_IO.Put (" ->");

         for V of Element (A).Values loop
            Text_IO.Put (" " & V.Text);
         end loop;
         Text_IO.New_Line;
      end loop;
   end Display;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      S : constant String := String (Filename);
      Test : constant String := "aggregate-lib";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      Text_IO.Put_Line (" > " & S (I + Test'Length + 1 .. S'Last));
   end Output_Filename;

   Prj : Project.Tree.Object;
   Opt : GPR2.Options.Object;
   Ctx : Context.Object;
   Log : GPR2.Log.Object;

begin
   Text_IO.Put_Line ("//// OS set to Linux");
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "OS=Linux");

   if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Text_IO.Put_Line ("!!! Failed to load project");
      return;
   end if;

   for P of Prj loop
      Display (P);
   end loop;

   Prj.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

   Text_IO.Put_Line ("sources:");
   for Agg of Prj.Root_Project.Aggregated loop
      for S of Agg.Sources loop
         Output_Filename (S.Path_Name.Value);
      end loop;
   end loop;

   Text_IO.New_Line;
   Text_IO.Put_Line ("//// OS set to Windows");

   Ctx := Prj.Context;
   Ctx.Include ("OS", "Windows");
   Prj.Set_Context (Ctx, Changed_Callback'Access);

   for P of Prj loop
      Display (P);
   end loop;

   Prj.Update_Sources (Messages => Log);
   Log.Output_Messages (Information => False);

   Text_IO.Put_Line ("sources:");
   for Agg of Prj.Root_Project.Aggregated loop
      for S of Agg.Sources loop
         Output_Filename (S.Path_Name.Value);
      end loop;
   end loop;
end Main;
