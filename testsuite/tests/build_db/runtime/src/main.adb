with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.OS.Dir;
with GNATCOLL.OS.FSUtil;

with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Options;
with GPR2.View_Ids;

procedure Main is
   use GPR2;
   use type GPR2.View_Ids.View_Id;

   Tree     : GPR2.Project.Tree.Object;
   Options  : GPR2.Options.Object;
   Result   : Boolean;
   RTS_Dir  : GPR2.Path_Name.Object;
   RTS_Dest : constant String :=
                GNAT.OS_Lib.Normalize_Pathname ("rts");
   Added    : GPR2.Containers.Value_Set;
   Ign      : Boolean;

   procedure Copy_File (Handle  : GNATCOLL.OS.Dir.Dir_Handle;
                        Element : GNATCOLL.OS.Dir.Dir_Entry)
   is
      use GNATCOLL.OS;
      Full_Name : constant String := Dir.Path (Handle, Element);
      Dest      : constant String := RTS_Dest & "/" & Dir.Name (Element);
   begin
      if Dir.Name (Element) = "memtrack.adb" then
         --  alternate implementation of s-memory.adb, produces warnings for
         --  name clash. So don't copy it
         return;
      end if;

      Added.Include (Dest);
      Ign := GNATCOLL.OS.FSUtil.Copy_File (Full_Name, Dest);
   end Copy_File;

begin
   --  For this test, the goal is to check with a project with all krunch
   --  special cases available that our version of krunch still holds. Only
   --  way to do that is to use the runtime. However libgpr2 does no checks on
   --  the actual runtime. so we need to copy it locally and check no error is
   --  produced when loading it with units resolved.

   --  Load test project just to retrieve the RTS sources from the runtime
   --  project

   Options.Add_Switch (GPR2.Options.P, "test.gpr");

   if Tree.Load (Options, Absent_Dir_Error => No_Error) then
      RTS_Dir := Tree.Runtime_Project.Source_Directories.First_Element;
   end if;
   Tree.Unload;

   GNATCOLL.OS.Dir.Walk
     (String (RTS_Dir.Value),
      File_Handler => Copy_File'Unrestricted_Access);

   Options := GPR2.Options.Empty_Options;
   Options.Add_Switch (GPR2.Options.P, "rts/rts.gpr");

   if Tree.Load (Options, Absent_Dir_Error => No_Error) then
      Tree.Update_Sources (Sources_Units_Artifacts);

      declare
         RTS_Unit : constant String := "Ada.Streams";
         CU       : constant GPR2.Build.Compilation_Unit.Object :=
                      Tree.Root_Project.Unit (Name_Type (RTS_Unit));
      begin
         if not CU.Is_Defined then
            Ada.Text_IO.Put_Line ("Could not find unit " & RTS_Unit);
         else
            Ada.Text_IO.Put_Line (RTS_Unit & " located");
         end if;

         for Dep of CU.Known_Dependencies loop
            Ada.Text_IO.Put_Line (" dep: " & String (Dep));
         end loop;
      end;
   end if;
   Tree.Unload;

   for Path of Added loop
      GNAT.OS_Lib.Delete_File (String (Path), Ign);
   end loop;
end Main;
