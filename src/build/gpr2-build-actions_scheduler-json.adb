--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Build.Actions.Process;

package body GPR2.Build.Actions_Scheduler.JSON is

   --------------------
   -- Collect_Action --
   --------------------

   overriding
   function Collect_Action
     (Self    : in out Object;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status
   is
      Job_Summary : constant JSON_Value := Create_Object;
      Env_Summary : JSON_Value;
      Cmd         : Unbounded_String;
      Args        : GNATCOLL.OS.Process.Argument_List;
      Env         : GNATCOLL.OS.Process.Environment_Dict;

   begin
      if not Action.View.Is_Externally_Built then
         Job_Summary.Set_Field (TEXT_ACTION_UID, Action.UID.Image);

         if Action in Actions.Process.Object'Class then
            Args := Actions.Process.Object (Action).Command_Line.Argument_List;
            Env :=
              Actions.Process.Object (Action)
                .Command_Line
                .Environment_Variables;

            for Arg of Args loop
               if Length (Cmd) > 0 then
                  Append (Cmd, " ");
               end if;

               Append (Cmd, Arg);
            end loop;

            Job_Summary.Set_Field (TEXT_COMMAND, Cmd);

            if not Env.Is_Empty then
               Env_Summary := Create_Object;

               for C in Env.Iterate loop
                  declare
                     Key  : constant UTF8_String :=
                       GNATCOLL.OS.Process.Env_Dicts.Key (C);
                     Elem : constant UTF8_String :=
                       GNATCOLL.OS.Process.Env_Dicts.Element (C);
                  begin
                     Env_Summary.Set_Field (Key, Elem);
                  end;
               end loop;

               Job_Summary.Set_Field (TEXT_ENV, Env_Summary);
            end if;
         end if;

         Job_Summary.Set_Field
           (TEXT_CWD, Action.Working_Directory.String_Value);

         case Handler.Status is
            when Running          =>
               pragma Annotate (Xcov, Exempt_On, "unreachable code");
               raise Internal_Error
                 with
                   "The process linked to the action '"
                   & Action.UID.Image
                   & "' is still running. Cannot collect the job before it "
                   & "finishes";
               pragma Annotate (Xcov, Exempt_Off);

            when Finished         =>
               Job_Summary.Set_Field
                 (TEXT_STATUS,
                  Ada.Strings.Fixed.Trim
                    (Handler.Return_Code'Image, Ada.Strings.Left));

            when Deactivated      =>
               Job_Summary.Set_Field (TEXT_STATUS, "DEACTIVATED");

            when Skipped          =>
               Job_Summary.Set_Field (TEXT_STATUS, "SKIPPED");

            when Failed_To_Launch =>
               Job_Summary.Set_Field (TEXT_STATUS, "FAILED_TO_LAUNCH");

            when others           =>
               --  This case should not happen
               null;
         end case;

         Job_Summary.Set_Field (TEXT_STDOUT, Handler.Stdout);
         Job_Summary.Set_Field (TEXT_STDERR, Handler.Stderr);

         GNATCOLL.JSON.Append (Self.JSON, Job_Summary);
      end if;

      return
        GPR2.Build.Actions_Scheduler.Object (Self).Collect_Action
          (Action, Handler, Context);
   end Collect_Action;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class)
   is
      use GNATCOLL.OS.FS;
      File : GNATCOLL.OS.FS.File_Descriptor;
      Repr : Unbounded_String;
   begin
      GPR2.Build.Actions_Scheduler.Object (Self).Execute
        (Tree_Db, Context, Options);

      if not Self.JSON_File.Is_Defined then
         return;
      end if;

      Repr := Write (Create (Self.JSON));

      File := Open (Self.JSON_File.String_Value, Write_Mode);
      Write_Unbounded (File, Repr);
      Close (File);
   end Execute;

   -------------------
   -- Set_JSON_File --
   -------------------

   procedure Set_JSON_File (Self : in out Object; Path : GPR2.Path_Name.Object)
   is
   begin
      Self.JSON_File := Path;
   end Set_JSON_File;

end GPR2.Build.Actions_Scheduler.JSON;
