with Ada.Calendar;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GPR2; use GPR2;
with GPR2.Build; use GPR2.Build;
with GPR2.Build.Actions.Compile.Ada;
with GPR2.Build.Actions_Population;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Options;
with GPR2.Build.Source.Sets;
with GPR2.Build.Unit_Info;
with GPR2.Build.View_Db;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   use Ada;

   Tree  : Project.Tree.Object;
   Opt   : GPR2.Options.Object;
   B_Opt : GPR2.Build.Options.Build_Options;

   procedure Print (S : Build.Source.Object);
   --  Print source information and remove dependency files if exists

   -----------
   -- Print --
   -----------

   procedure Print (S : Build.Source.Object) is
      Dep     : Path_Name.Object;
      View_Db : constant GPR2.Build.View_Db.Object :=
        Tree.Artifacts_Database (S.Owning_View);

   begin
      Text_IO.Put_Line (String (S.Path_Name.Simple_Name));
      Text_IO.Put_Line ("  single-unit          = " & S.Has_Single_Unit'Image);
      Text_IO.Put_Line
        ("  has naming exception = " & S.Has_Naming_Exception'Image);
      for CU of S.Units loop
         Text_IO.Put_Line ("  - compilation unit at" & CU.Index'Image);
         Text_IO.Put_Line ("    unit name    = " & String (CU.Name));
         Text_IO.Put_Line ("    kind         = " & CU.Kind'Image);

         if not CU.Dependencies.Is_Empty then
            Text_IO.Put ("    withed units = { ");

            for W of CU.Dependencies loop
               Text_IO.Put (String (W) & " ");
            end loop;

            Text_IO.Put_Line ("}");
         end if;

         if CU.Name'Length > 0
           and then View_Db.Has_Compilation_Unit (CU.Name)
          then
            declare
               Unit : GPR2.Build.COmpilation_Unit.Object :=
                 View_Db.Compilation_Unit (CU.Name);
               C_ID : constant GPR2.Build.Actions.Compile.Ada.Ada_Compile_Id :=
                 GPR2.Build.Actions.Compile.Ada.Create (Unit);
               Comp : GPR2.Build.Actions.Compile.Ada.Object;
            begin
               if Tree.Artifacts_Database.Has_Action (C_Id) then
                  Comp :=
                    Build.Actions.Compile.Ada.Object
                      (Tree.Artifacts_Database.Action (C_Id));
                  Text_IO.Put_Line
                    ("    object file  = "
                     & String (Comp.Object_File.Path.Simple_Name));
                  Text_IO.Put_Line
                    ("    dep file     = "
                     & String (Comp.Intf_Ali_File.Path.Simple_Name));

                  for D of Comp.Dependencies loop
                     Text_IO.Put_Line ("    depends on " & String (D));
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Print;

begin
   Opt.Add_Switch (GPR2.Options.P, "files/p.gpr");

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
      Print (S);
   end loop;
end Main;
