with Ada.Text_IO;
with GPR2.Build.Compilation_Unit;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

procedure Main is
   Tree     : GPR2.Project.Tree.Object;
   Opt      : GPR2.Options.Object;
   Filename : constant GPR2.Path_Name.Object :=
                GPR2.Path_Name.Create_File
                  (GPR2.Project.Ensure_Extension ("file/prj.gpr"),
                   GPR2.Path_Name.No_Resolution);
   use GPR2;

   procedure Test
     (Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean;
      Allow_Unit_Name : Boolean)
   is
      Source : constant GPR2.Path_Name.Object :=
                 Tree.Root_Project.Source_Path
                   (Name            => Name,
                    Allow_Spec_File => Allow_Spec_File,
                    Allow_Unit_Name => Allow_Unit_Name);
   begin
      Ada.Text_IO.Put ("Source_Path(" & String (Name) & ","
                       & Allow_Spec_File'Img & "," & Allow_Unit_Name'Img);
      if Source.Is_Defined then
         Ada.Text_IO.Put_Line (") is " & String (Source.Simple_Name));
      else
         Ada.Text_IO.Put_Line (") is Undefined");
      end if;
   end Test;

begin
   Opt.Add_Switch (Options.P, "file/prj.gpr");
   if not Tree.Load (Opt, Absent_Dir_Error => No_Error) then
      return;
   end if;

   Tree.Update_Sources;

   --  check full filename works
   Test ("main.2.ada", True, True);
   --  check alternate language spec/body suffix used
   Test ("test1", True, False);
   Test ("test2", True, False);
   --  check spec not returned when suffix & unit name used
   Test ("root", False, True);
   --  check spec returned suffix used
   Test ("root", True, False);
   --  check body returned when suffix used
   Test ("root-child", True, False);
   --  check body returned when unit name used
   Test ("root.child", True, True);
   --  check spec returned when unit name used
   Test ("root.child.child", True, True);
   --  check spec not returned when unit name used
   Test ("root.child.child", False, True);
end Main;
