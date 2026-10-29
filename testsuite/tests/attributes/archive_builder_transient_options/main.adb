with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Reporter.Log;

procedure Main is
   use GPR2;

   procedure Check
     (Project, Config : String; Expected_Error : String := "");

   procedure Check
     (Project, Config : String; Expected_Error : String := "")
   is
      Tree : GPR2.Project.Tree.Object;
      Opt  : Options.Object;
   begin
      Opt.Add_Switch (Options.P, Project & ".gpr");
      Opt.Add_Switch (Options.Config, Config & ".cgpr");
      Put (Project & "/" & Config & ":");

      if Tree.Load
        (Opt, Reporter => Reporter.Log.Create, Check_Drivers => False)
      then
         if Expected_Error /= "" then
            raise Program_Error with "expected rejection of " & Project;
         end if;

         declare
            Attr : constant GPR2.Project.Attribute.Object :=
              Tree.Root_Project.Attribute
                (GPR2.Project.Registry.Attribute.
                   Archive_Builder_Transient_Options);
         begin
            if not Attr.Is_Defined then
               Put (" undefined");
            elsif Attr.Values.Is_Empty then
               Put (" empty");
            else
               for Value of Attr.Values loop
                  Put (" " & Value.Text);
               end loop;
            end if;
         end;
      else
         if Expected_Error = "" then
            raise Program_Error with "unexpected rejection of " & Project;
         end if;

         declare
            Found : Boolean := False;
         begin
            for Msg of Reporter.Log.Object (Tree.Reporter).Log loop
               if Ada.Strings.Fixed.Index
                 (Msg.Message, Expected_Error) > 0
               then
                  Found := True;
               end if;
            end loop;

            if not Found then
               raise Program_Error with "missing diagnostic: " & Expected_Error;
            end if;
         end;

         Put (" rejected");
      end if;

      New_Line;
   end Check;
begin
   Check ("absent", "empty");
   Check ("absent", "configured");
   Check ("base", "configured");
   Check ("legacy", "configured");
   Check ("disabled", "configured");
   Check ("extending", "empty");
   Check ("extending", "configured");
   Check ("indexed", "empty", "does not expect an index");
   Check ("scalar", "empty", "expects a list");
   Check ("list", "empty");
   Check ("empty_list", "empty");
   Check ("invalid", "empty");
   Check ("wrong_case", "empty");
   Check ("absent", "invalid");
end Main;
