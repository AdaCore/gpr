with Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

use GPR2;

procedure Main is

   procedure Test (Gpr : String) is
      Tree : Project.Tree.Object;
      Ctx  : Context.Object;
   begin
      Project.Tree.Load_Autoconf
        (Tree,
         Path_Name.Create_File (Filename_Type (Gpr)),
         Context => Ctx);

      if Tree.Log_Messages.Has_Error then
         Tree.Log_Messages.Output_Messages (Information => False);
         return;
      end if;

      for V of Tree.Ordered_Views loop
         if V.Kind /= K_Aggregate then
            Ada.Text_IO.Put ("Roots for " & String (V.Name));

            if V.Is_Extended then
               Ada.Text_IO.Put (" extended by " & String (V.Extending.Name));
            end if;

            Ada.Text_IO.New_Line;

            for Root of V.Namespace_Roots loop
               Ada.Text_IO.Put_Line (" - " & String (Root.Name));
            end loop;
         end if;
      end loop;

      Tree.Unload;
   end Test;

begin
   Test ("tree/agg.gpr");
end Main;
