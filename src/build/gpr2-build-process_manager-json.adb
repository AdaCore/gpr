--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Actions; use GPR2.Build.Actions;

package body GPR2.Build.Process_Manager.JSON is

   -----------------
   -- Collect_Job --
   -----------------

   overriding function Collect_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Context        : in out Process_Execution_Context;
      Proc_Handler   : Process_Handler;
      Stdout, Stderr : Unbounded_String)
      return Collect_Status
   is
      Job_Summary : constant JSON_Value := Create_Object;
      Env_Summary : JSON_Value;
      Cmd         : Unbounded_String;
      Args        : GNATCOLL.OS.Process.Argument_List;
      Env         : GNATCOLL.OS.Process.Environment_Dict;

   begin
      if not Job.View.Is_Externally_Built then
         Job_Summary.Set_Field (TEXT_ACTION_UID, Job.UID.Image);

         Args := Job.Command_Line.Argument_List;
         Env  := Job.Command_Line.Environment_Variables;

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

         Job_Summary.Set_Field (TEXT_CWD, Job.Working_Directory.String_Value);

         case Proc_Handler.Status is
            when Running =>
               pragma Annotate (Xcov, Exempt_On, "unreachable code");
               raise Internal_Error with
                 "The process linked to the action '" & Job.UID.Image &
                 "' is still running. Cannot collect the job before it " &
                 "finishes";
               pragma Annotate (Xcov, Exempt_Off);


            when Finished =>
               Job_Summary.Set_Field
                 (TEXT_STATUS,
                  Ada.Strings.Fixed.Trim
                    (Proc_Handler.Process_Status'Image, Ada.Strings.Left));

            when Deactivated =>
               Job_Summary.Set_Field (TEXT_STATUS, "DEACTIVATED");

            when Skipped =>
               Job_Summary.Set_Field (TEXT_STATUS, "SKIPPED");

            when Failed_To_Launch =>
               Job_Summary.Set_Field (TEXT_STATUS, "FAILED_TO_LAUNCH");

            when Pending =>
               --  This case should not happen
               null;
         end case;

         Job_Summary.Set_Field (TEXT_STDOUT, Stdout);
         Job_Summary.Set_Field (TEXT_STDERR, Stderr);

         GNATCOLL.JSON.Append (Self.JSON, Job_Summary);
      end if;

      return GPR2.Build.Process_Manager.Object (Self).Collect_Job
        (Job, Context, Proc_Handler, Stdout, Stderr);
   end Collect_Job;

     ----------------------------
     -- Execution_Post_Process --
     ----------------------------

   overriding procedure Execution_Post_Process (Self : in out Object) is
      File : GNATCOLL.OS.FS.File_Descriptor;
      Repr : Unbounded_String;
   begin
      if not Self.JSON_File.Is_Defined then
         return;
      end if;

      Repr := Write (Create (Self.JSON));

      File := Open (Self.JSON_File.String_Value, Write_Mode);
      Write_Unbounded (File, Repr);
      Close (File);
   end Execution_Post_Process;

   -------------------
   -- Set_JSON_File --
   -------------------

   procedure Set_JSON_File
     (Self : in out Object;
      Path : GPR2.Path_Name.Object) is
   begin
      Self.JSON_File := Path;
   end Set_JSON_File;

end GPR2.Build.Process_Manager.JSON;
