with Ada.Text_IO;
with GPR2.Build.Compilation_Unit;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;

procedure Main is
   Tree         : GPR2.Project.Tree.Object;
   Context      : GPR2.Context.Object;
   use GPR2;
   use GPR2.BUild;

   procedure Test (Project_Name : GPR2.Filename_Type) is
      Log : GPR2.Log.Object;
   begin
      Tree.Unload;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File
           (GPR2.Project.Ensure_Extension (Project_Name),
            GPR2.Path_Name.No_Resolution),
         Context  => Context);
      Tree.Log_Messages.Output_Messages (Information => False);
      Tree.Update_Sources (Messages => Log);
      Log.Output_Messages;

      for U of Tree.Root_Project.Units loop
         Ada.Text_IO.Put_Line (String (U.Name));
         for Part in S_Spec .. S_Body loop
            if U.Has_Part (Part) then
               Ada.Text_IO.Put ("- ");
               Ada.Text_IO.Put (Part'Image);
               Ada.Text_IO.Put (": ");
               Ada.Text_IO.Put_Line (String (U.Get (Part).Source.Simple_Name));
            end if;
         end loop;

         for C in U.Separates.Iterate loop
            declare
               Key  : constant Name_Type :=
                        GPR2.Build.Compilation_Unit.Separate_Maps.Key (C);
               Part : Compilation_Unit.Unit_Location :=
                        Compilation_Unit.Separate_Maps.Element (C);
            begin
               Ada.Text_IO.Put ("- SEP. ");
               Ada.Text_IO.Put (String (Key));
               Ada.Text_IO.Put (": ");
               Ada.Text_IO.Put_Line (String (Part.Source.Simple_Name));
            end;
         end loop;
      end loop;

   exception
      when Project_Error =>
         Tree.Log_Messages.Output_Messages (Information => False);
   end Test;

begin
   Test("tree/spec");
   Context.Insert ("DOT_REPLACEMENT", "-");
   Ada.Text_IO.Put_Line ("DOT_REPLACEMENT=""-""");
   Test("tree/spec");
end Main;
