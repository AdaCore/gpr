with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Log;
with GPR2.Unit;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.Tree;

with GPR2.Source_Info.Parser.Ada_Language;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   procedure Check (Project_Name : Filename_Type);
   --  Do check the given project's sources

   procedure Output_Filename (Filename : Path_Name.Full_Name);
   --  Remove the leading tmp directory

   -----------
   -- Check --
   -----------

   procedure Check (Project_Name : Filename_Type) is
      Prj  : Project.Tree.Object;
      Ctx  : Context.Object;
      View : Project.View.Object;
   begin
      Project.Tree.Load_Autoconf (Prj, Create (Project_Name), Ctx);

      View := Prj.Root_Project;
      Text_IO.Put_Line ("Project: " & String (View.Name));

      for Source of View.Sources loop
         Output_Filename (Source.Path_Name.Value);

         Text_IO.Set_Col (20);
         Text_IO.Put ("   language: " & Image (Source.Language));

         Text_IO.Set_Col (37);
         Text_IO.Put ("   Kind: "
                        & GPR2.Unit.Library_Unit_Type'Image (Source.Kind));

         if Source.Has_Units then
            Text_IO.Put ("   unit: " & String (Source.Unit_Name));
         end if;

         Text_IO.New_Line;
      end loop;
   exception
      when Project_Error =>
         if Prj.Log_Messages.Has_Error then
            for C in Prj.Log_Messages.Iterate
              (Information => False,
               Warning     => False,
               Error       => True,
               Read        => False,
               Unread      => True)
            loop
               Text_IO.Put_Line (Log.Element (C).Format);
            end loop;
         end if;
   end Check;

   ---------------------
   -- Output_Filename --
   ---------------------

   procedure Output_Filename (Filename : Path_Name.Full_Name) is
      I : constant Positive :=
            Strings.Fixed.Index (Filename, "duplicate-source");
   begin
      Text_IO.Put (" > " & Filename (I + 16 .. Filename'Last));
   end Output_Filename;

begin
   Check ("prj_c.gpr");
   Check ("prj_c_2.gpr");
   Check ("prj_c_3.gpr");
   Check ("prj_ada.gpr");
   Check ("prj_ada_2.gpr");
   Check ("prj_ada_3.gpr");
end Main;