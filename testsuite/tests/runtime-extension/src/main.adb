with Ada.Directories;
with Ada.Text_IO;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.Registry.Attribute;

procedure Main is
   use GPR2;
   use type GPR2.Project.Registry.Attribute.Value_Kind;
   package PRA renames GPR2.Project.Registry.Attribute;

   procedure Display_Path (Value : String);

   Opt : Options.Object;
   Tree : Project.Tree.Object;

   procedure Display_Path (Value : String) is
      P : Path_Name.Object := Path_Name.Create_Directory (Filename_Type (Value));
   begin
      Ada.Text_IO.Put_Line
        (String (P.Relative_Path (Tree.Root_Project.Dir_Name).Name));
   end Display_Path;

begin
   Ada.Directories.Set_Directory ("tree");
   Opt.Add_Switch (Options.RTS, "rts-ext");
   Opt.Finalize
     (Allow_Implicit_Project => True);
   if Opt.Load_Project (Tree) then
      Tree.Log_Messages.Output_Messages (Information => False);

      for Attr of Tree.Runtime_Project.Attributes
        (With_Defaults => False, With_Config => False)
      loop
         Ada.Text_IO.Put (Image (Attr.Name.Id) & " : ");
         if Attr.Kind = PRA.Single then
            if Attr.Name.Id = PRA.Object_Dir then
               Display_Path (Attr.Value.Text);
            else
               Ada.Text_IO.Put_Line (Attr.Value.Text);
            end if;
         else
            Ada.Text_IO.New_Line;
            if Attr.Name.Id = PRA.Source_Dirs then
               for V of Attr.Values loop
                  Ada.Text_IO.Put ("  ");
                  Display_Path (V.Text);
               end loop;
            else
               for V of Attr.Values loop
                  Ada.Text_IO.Put_Line ("  " & V.Text);
               end loop;
            end if;
         end if;
      end loop;
   else
      Tree.Log_Messages.Output_Messages (Information => False,
                                         Warning     => False);
   end if;
end Main;
