with Ada.Real_Time;     use Ada.Real_Time;
with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Build.Source.Sets;
with GPR2.Options;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   use GPR2;
   use GPR2.Build;
   use type GPR2.Project.View.Object;

   procedure Test (Gpr : String)
   is
      Tree        : Project.Tree.Object;
      Log         : GPR2.Log.Object;
      Opt         : Options.Object;
      Src_Count   : Natural := 0;
      Src_Count_2 : Natural := 0;

      ------------------
      -- Print_Source --
      ------------------

      procedure Print_Source (S : Build.Source.Object) is
         Root : constant Path_Name.Object := Tree.Root_Project.Dir_Name;

         function Image (Kind : Unit_Kind) return String
         is (case Kind is
                when S_Spec     => "spec",
                when S_Body     => "body",
                when S_Separate => "sep.",
                when S_No_Body  => "no_body");
      begin
         Ada.Text_IO.Put ("- " &
                            String (S.Path_Name.Relative_Path (Root)));

         if not S.Has_Units or else not S.Has_Index then
            Ada.Text_IO.Put_Line (" (" & Image (S.Kind) & ")");
         else
            Ada.Text_IO.New_Line;
         end if;

         if S.Has_Units then
            for U of S.Units loop
               Ada.Text_IO.Put ("  " & String (U.Name));

               if U.Kind = S_Separate then
                  Ada.Text_IO.Put ("." & String (U.Separate_Name));
               end if;

               Ada.Text_IO.Put_Line
                 (" (" & Image (U.Kind) &
                  (if U.Kind = S_Separate
                     then "from " & String (U.Name) else "") & ")");
            end loop;
         end if;
      end Print_Source;

   begin
      Opt.Add_Switch (Options.P, Gpr);
      if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         return;
      end if;

      Ada.Text_IO.Put_Line
        ("*** checking sources of project " &
           String (Tree.Root_Project.Path_Name.Relative_Path
             (Path_Name.Create_Directory ("."))));

      Tree.Update_Sources (Messages => Log);
      Log.Output_Messages (Information => False);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("* Views *");
      Ada.Text_IO.New_Line;

      for C in Tree.Iterate loop
         declare
            V : constant Project.View.Object := Project.Tree.Element (C);
            First : Boolean;
         begin
            Ada.Text_IO.Put_Line
              ("** " & String (V.Name) & " (" & V.Kind'Image & ") **");

            First := True;

            if V.Kind = K_Aggregate then
               for Agg of V.Aggregated loop
                  if First then
                     Ada.Text_IO.Put_Line ("- Aggregated project(s):");
                     First := False;
                  end if;

                  Ada.Text_IO.Put_Line ("  - " & String (Agg.Name));
               end loop;

            else
               First := True;

               for Root of V.Namespace_Roots loop
                  if First then
                     Ada.Text_IO.Put_Line ("- Root project(s):");
                     First := False;
                  end if;

                  Ada.Text_IO.Put_Line ("  - " & String (Root.Name));
               end loop;

               if V.Kind = K_Aggregate_Library then
                  First := True;

                  for Agg of V.Aggregated loop
                     if First then
                        Ada.Text_IO.Put_Line ("- Aggregated by:");
                        First := False;
                     end if;

                     Ada.Text_IO.Put_Line ("  - " & String (Agg.Name));
                  end loop;
               end if;

               if V.Is_Aggregated_In_Library then
                  First := True;

                  for Agg of V.Aggregate_Libraries loop
                     if First then
                        Ada.Text_IO.Put_Line ("- Aggregated in library:");
                        First := False;
                     end if;

                     Ada.Text_IO.Put_Line ("  - " & String (Agg.Name));
                  end loop;
               end if;

               if V.Is_Extending then
                  Ada.Text_IO.Put ("- Extends");

                  if V.Is_Extending_All then
                     Ada.Text_IO.Put_Line (" all:");
                  end if;

                  Ada.Text_IO.Put_Line ("  - " & String (V.Extended_Root.Name));
               end if;
            end if;
         end;
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("* Sources *");
      Ada.Text_IO.New_Line;

      Src_Count := 0;
      Src_Count_2 := 0;

      for V of Tree loop
         if V /= Tree.Runtime_Project
           and then V.Kind in GPR2.With_Object_Dir_Kind
         then
            Ada.Text_IO.Put ("sources visible by " & String (V.Name));

            if V.Is_Extended then
               Ada.Text_IO.Put (" extended by " &
                                  String (V.Extending.Name));
            end if;

            Ada.Text_IO.New_Line;

            for S of V.View_Db.Visible_Sources loop
               Src_Count := Src_Count + 1;
               Print_Source (S);
            end loop;
         end if;
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("* Units *");
      Ada.Text_IO.New_Line;

      for NS of Tree.Namespace_Root_Projects loop
         for U of NS.Units loop
            Ada.Text_IO.Put_Line (String (U.Name));

            for Kind in GPR2.S_Spec .. GPR2.S_Body loop
               if U.Has_Part (Kind) then
                  Ada.Text_IO.Put_Line ("- " & Kind'Image & " " & String (U.Get (Kind).Source.Simple_Name));
               end if;
               for C in U.Separates.Iterate loop
                  declare
                     Key : constant Name_Type :=
                             GPR2.Build.Compilation_Unit.Separate_Maps.Key (C);
                     Elem : constant GPR2.Build.Compilation_Unit.Unit_Location :=
                              GPR2.Build.Compilation_Unit.Separate_Maps.Element (C);
                  begin
                     Ada.Text_IO.Put_Line ("- " & String (Key) & " " & String (Elem.Source.Simple_Name));
                  end;
               end loop;
            end loop;
         end loop;
      end loop;

      Tree.Unload;
      Ada.Text_IO.New_Line;
   end Test;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line ("Usage: " & Ada.Command_Line.Command_Name &
                              " <project.gpr>");
      return;
   end if;

   Test (Ada.Command_Line.Argument (1));
end Main;
