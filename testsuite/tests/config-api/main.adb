with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with GPR2.Options;
with GPR2.KB;
with GPR2.Log;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Reporter.Console;

procedure Main is

   use Ada;
   use Ada.Exceptions;
   use GPR2;
   use GPR2.Project;
   use GPR2.Reporter;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True);

   procedure Display (Att : Project.Attribute.Object);

   -------------
   -- Display --
   -------------

   procedure Display (Att : Project.Attribute.Object) is
   begin
      Text_IO.Put ("   " & Image (Att.Name.Id.Attr));

      if Att.Has_Index then
         Text_IO.Put (" (" & Att.Index.Text & ")");
      end if;

      Text_IO.Put (" ->");

      for V of Att.Values loop
         Text_IO.Put (" " & V.Text);
      end loop;
      Text_IO.New_Line;
   end Display;

   procedure Display (Prj : Project.View.Object; Full : Boolean := True) is
      use GPR2.Project.Attribute.Set;
      use GPR2.Project.Variable.Set.Set;
   begin
      Text_IO.Put (String (Prj.Name) & " ");
      Text_IO.Set_Col (10);
      Text_IO.Put_Line (Prj.Qualifier'Img);

      if Full then
         for A in Prj.Attributes (With_Defaults => False,
                                  With_Config   => False).Iterate loop
            Text_IO.Put
              ("A:   " & Image (Attribute.Set.Element (A).Name.Id.Attr));
            Text_IO.Put (" ->");

            for V of Element (A).Values loop
               Text_IO.Put (" " & V.Text);
            end loop;
            Text_IO.New_Line;
         end loop;

         if Prj.Has_Variables then
            for V in Prj.Variables.Iterate loop
               Text_IO.Put ("V:   " & String (Key (V)));
               Text_IO.Put (" -> ");
               Text_IO.Put (Element (V).Value.Text);
               Text_IO.New_Line;
            end loop;
         end if;
         Text_IO.New_Line;

         for Pck of Prj.Packages (With_Defaults => False,
                                  With_Config   => False)
         loop
            Text_IO.Put_Line (" " & Image (Pck));

            for A of Prj.Attributes (Pack => Pck,
                                     With_Defaults => False,
                                     With_Config   => False)
            loop
               Display (A);
            end loop;
         end loop;
      end if;
   end Display;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

   Des      : Configuration.Description :=
                Configuration.Create (Language => Ada_Language);
   KB       : GPR2.KB.Object := GPR2.KB.Create (GPR2.KB.Default_Flags);
   Reporter : GPR2.Reporter.Console.Object := Console.Create;
   Cnf      : Configuration.Object :=
                Configuration.Create
                  (Configuration.Description_Set'(1 => Des), "all",
                   Path_Name.Create_File ("demo.gpr"),
                   Base      => KB,
                   Save_Name => Path_Name.Create_File ("conf.cgpr"));

begin
   if Cnf.Has_Messages then
      Reporter.Report (Cnf.Log_Messages);
   end if;

   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "OS=Linux");
   Opt.Add_Switch (Options.Config, "conf.cgpr");

   if Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Display (Prj.Root_Project);

      if Prj.Has_Configuration then
         Display (Prj.Configuration.Corresponding_View, Full => False);
      end if;
   end if;
end Main;
