with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.Build.Actions_Population;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Options;
with GPR2.Build.Source;
with GPR2.Build.Source.Sets;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   use Ada;

   use GPR2;

   Tree  : Project.Tree.Object;
   Opt   : GPR2.Options.Object;
   B_Opt : Build.Options.Build_Options;

   ------------------
   -- Print_Source --
   ------------------

   procedure Print_Source (S : Build.Source.Object) is
      DN   : Path_Name.Object;
      CU   : GPR2.Build.Compilation_Unit.Object;
      Comp : GPR2.Build.Actions.Compile.Ada.Object;
   begin
      Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
      Text_IO.Put_Line ("  single-unit          = " & S.Has_Single_Unit'Image);
      Text_IO.Put_Line
        ("  has naming exception = " & S.Has_Naming_Exception'Image);
      for U_Info of S.Units loop
         Text_IO.Put_Line ("  - compilation unit at" & U_Info.Index'Image);
         Text_IO.Put_Line ("    unit name    = " & String (U_Info.Name));
         Text_IO.Put_Line ("    kind         = " & U_Info.Kind'Image);

         CU := Tree.Root_Project.Unit (U_Info.Name);

         if CU.Is_Defined then

            declare
               UID  : constant Build.Actions.Compile.Ada.Ada_Compile_Id :=
                 Build.Actions.Compile.Ada.Create (CU);
               Deps : GPR2.Containers.Name_Set;
            begin
               if Tree.Artifacts_Database.Has_Action (UID) then
                  Comp :=
                    Build.Actions.Compile.Ada.Object
                      (Tree.Artifacts_Database.Action (UID));

                  if U_Info.Kind = S_Spec then
                     Deps := Comp.Withed_Units_From_Spec;
                  else
                     Deps := Comp.Withed_Units_From_Body;
                  end if;

                  if not Deps.Is_Empty then

                     Text_IO.Put ("    withed units = { ");

                     for W of Deps loop
                        Text_IO.Put (String (W) & " ");
                     end loop;
                     Text_IO.Put_Line ("}");
                  end if;

                  Text_IO.Put_Line
                    ("    object file  = "
                     & String (Comp.Object_File.Path.Simple_Name));

                  DN := Comp.Intf_Ali_File.Path;

                  Text_IO.Put_Line
                    ("    deps file    = " & String (DN.Simple_Name));
               end if;
            end;
         end if;
      end loop;
   end Print_Source;

begin
   Opt.Add_Switch (Options.P, "files/multi.gpr");

   if not Tree.Load (Opt) then
      return;
   end if;

   if not Tree.Update_Sources then
      return;
   end if;

   if not Build.Actions_Population.Populate_Actions (Tree, B_Opt, True) then
      return;
   end if;

   for S of Tree.Root_Project.Sources loop
      Print_Source (S);
   end loop;
end Main;
