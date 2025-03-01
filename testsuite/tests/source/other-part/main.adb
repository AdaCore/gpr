with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
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

   function Filter_Filename (Filename : Path_Name.Full_Name) return String;
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : String) is
      Prj   : Project.Tree.Object;
      Opt   : Options.Object;
      View  : Project.View.Object;
      Other : GPR2.Path_Name.Object;
   begin
      Opt.Add_Switch (Options.P, Project_Name);
      if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Prj.Update_Sources;

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         Other := Path_Name.Undefined;

         if Source.Has_Units then
            declare
               Unit : GPR2.Build.Compilation_Unit.Object :=
                        View.Unit (Source.Unit.Name);
            begin
               if Source.Kind = S_Spec and then Unit.Has_Part (S_Body) then
                  Other := Unit.Get (S_Body).Source;
               elsif Source.Kind = S_Body and then Unit.Has_Part (S_Spec) then
                  Other := Unit.Get (S_Spec).Source;
               end if;
            end;
         end if;

         Text_IO.Put_Line
           (Filter_Filename (Source.Path_Name.Value)  & " -> " &
            (if Other.Is_Defined
               then Filter_Filename (Other.Value)
               else "undefined"));

         Text_IO.Set_Col (4);
         Text_IO.Put ("   language: " & Image (Source.Language));

         Text_IO.Set_Col (22);
         Text_IO.Put
           ("   Kind: " & Source.Kind'Image);

         if Source.Has_Units then
            Text_IO.Put ("   unit: " & String (Source.Unit.Name));
         end if;

         Text_IO.New_Line;
      end loop;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   function Filter_Filename (Filename : Path_Name.Full_Name) return String is
      S : constant String := String (Filename);
      Test : constant String := "other-part";
      I : constant Positive := Strings.Fixed.Index (S, Test);
   begin
      return S (I + Test'Length + 1 .. S'Last);
   end Filter_Filename;

begin
   Check ("demo.gpr");
end Main;
