with Ada.Text_IO;
with GNAT.OS_Lib;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Context;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Log;

function Main return Natural is
   use GPR2;
   use GPR2.Build;

   function Test (Gpr        : String;
                  Src_Subdir : String := "") return Natural
   is
      procedure Print_Path (P : GPR2.Path_Name.Object);
      procedure Print_View (View : GPR2.Project.View.Object);

      Tree : Project.Tree.Object;
      Opts : GPR2.Options.Object;
      Root : GPR2.Path_Name.Object := GPR2.Path_Name.Create_Directory (".");

      ----------------
      -- Print_Path --
      ----------------

      procedure Print_Path (P : GPR2.Path_Name.Object) is
      begin
         if P.Simple_Name = "runtime.gpr" then
            Ada.Text_IO.Put_Line
              ("<runtine dir>" & GNAT.OS_Lib.Directory_Separator &
                 "runtime.gpr");
         elsif P.Is_Directory and then P.Simple_Name = "adainclude" then
            Ada.Text_IO.Put_Line
              ("<runtine dir>" & GNAT.OS_Lib.Directory_Separator &
                 "adainclude" & GNAT.OS_Lib.Directory_Separator);
         else
            Ada.Text_IO.Put_Line (String (P.Relative_Path (Root)));
         end if;
      end Print_Path;

      ----------------
      -- Print_View --
      ----------------

      procedure Print_View (View : GPR2.Project.View.Object) is
      begin
         Ada.Text_IO.Put ("* ");
         Print_Path (View.Path_Name);

         for Lang of View.Language_Ids loop
            Ada.Text_IO.Put_Line ("  - " & GPR2.Image (Lang) & ":");

            for P of View.Include_Path (Lang) loop
               Ada.Text_IO.Put ("    ");
               Print_Path (P);
            end loop;
         end loop;

         if View.Kind = K_Aggregate_Library then
            for V of View.Aggregated loop
               Print_View (V);
            end loop;
         end if;
      end Print_View;

   begin

      Opts.Add_Switch (GPR2.Options.P, Gpr);

      if Src_Subdir /= "" then
         Opts.Add_Switch (GPR2.Options.Src_Subdirs, Src_Subdir);
      end if;

      if not Tree.Load (Opts,
                        With_Runtime     => True,
                        Absent_Dir_Error => No_Error)
      then
         return 1;
      end if;

      for NS of Tree.Namespace_Root_Projects loop
         Ada.Text_IO.Put_Line ("=========================================");
         Ada.Text_IO.Put ("Testing ");
         Print_Path (NS.Path_Name);
         Ada.Text_IO.Put_Line ("=========================================");

         for V of NS.Closure (True, True) loop
            if V.Path_Name.Simple_Name = "base.gpr" then
               Print_View (V);
            end if;
         end loop;
      end loop;

      Tree.Unload;

      return 0;
   end Test;

   Res : Natural;

begin

   Res := Test ("trees/base.gpr");

   if Res /= 0 then
      return Res;
   end if;

   Res := Test ("trees/container.gpr");

   if Res /= 0 then
      return Res;
   end if;

   return 0;
end Main;
