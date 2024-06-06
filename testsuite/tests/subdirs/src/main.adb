with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   use GPR2;

   procedure Test (Subdir : String);

   procedure Test (Subdir : String) is
      Tree : Project.Tree.Object;
      Opt  : Options.Object;
   begin
      Ada.Text_IO.Put_Line ("=== test with --subdirs=""" & Subdir & """ ===");
      Opt.Add_Switch (Options.P, "trees/prj.gpr");
      if Subdir'Length > 0 then
         Opt.Add_Switch (Options.Subdirs, Subdir);
      end if;

      if Tree.Load (Opt) then
         for V of reverse Tree.Ordered_Views loop
            Ada.Text_IO.Put_Line ("Project " & String (V.Name) & " " & V.Kind'Image);
            if V.Kind in With_Object_Dir_Kind then
               Ada.Text_IO.Put_Line (" - Obj: " & V.Object_Directory.String_Value);
            end if;
            if V.Is_Library then
               Ada.Text_IO.Put_Line (" - Lib: " & V.Library_Directory.String_Value);
            end if;
         end loop;
      end if;
   end Test;
begin

   Test ("");
   Test ("subdir");
   Test ("non_existing");
end Main;
