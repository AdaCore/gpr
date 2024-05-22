with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.View;
with GPR2.Project.Tree;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Indent : Natural := 0;

   procedure Put_Indent;

   procedure Display (Prj : Project.View.Object);

   procedure Display (Att : Project.Attribute.Object);

   procedure Display (Var : Project.Variable.Object);

   procedure Changed_Callback (Prj : Project.View.Object);

   ----------------
   -- Put_Indent --
   ----------------

   procedure Put_Indent is
      Space : constant String (1 .. Indent) := (others => ' ');
   begin
      Text_IO.Put (Space);
   end Put_Indent;

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

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Put_Indent;
      Text_IO.Put (Image (Att.Name.Id.Attr));

      if Att.Has_Index then
         Text_IO.Put (" (" & String (Att.Index.Text) & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Var : Project.Variable.Object) is
   begin
      Put_Indent;
      Text_IO.Put (String (Var.Name.Text) & " =");
      for V of Var.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
      Old   : Natural;
      First : Boolean;
   begin
      Text_IO.Put_Line ('[' & String (Prj.Name) & "] " & Prj.Qualifier'Img);
      Indent := Indent + 3;

      for I of Prj.Imports loop
         if not I.Is_Runtime then
            Put_Indent;
            Text_IO.Put ("with       ");
            Indent := Indent + 3;
            Display (I);
            Indent := Indent - 3;
         end if;
      end loop;

      if Prj.Is_Extending then
         Put_Indent;
         Text_IO.Put ("extends   ");
         Indent := Indent + 3;
         Display (Prj.Extended_Root);
         Indent := Indent - 3;
      end if;

      for A of Prj.Attributes (With_Defaults => False, With_Config => False) loop
         Display (A);
      end loop;

      for V of Prj.Variables loop
         Display (V);
      end loop;

      First := True;

      for Pck of Prj.Packages loop
         Old := Indent;

         for A of Prj.Attributes (Pack => Pck, With_Defaults => False, With_Config => False) loop
            if First then
               First := False;
               Put_Indent;
               Text_IO.Put_Line ("Pck:   " & Image (Pck));
               Indent := Indent + 3;
            end if;

            Display (A);
         end loop;

         for Var of Prj.Variables (Pck) loop
            Display (Var);
         end loop;

         Indent := Old;
      end loop;


      if Prj.Kind in Aggregate_Kind then
         for Agg of Prj.Aggregated loop
            Put_Indent;
            Text_IO.Put ("aggregates ");
            Indent := Indent + 3;
            Display (Agg);
            Indent := Indent - 3;
         end loop;
      end if;

      Indent := Indent - 3;
      Text_IO.New_Line;
   end Display;

   Prj : Project.Tree.Object;
   Opt : GPR2.Options.Object;

begin
   Opt.Add_Switch (Options.P, "agg.gpr");
   Opt.Finalize;
   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);
   else
      Text_IO.Put_Line ("Cannot load project");
   end if;
end Main;
