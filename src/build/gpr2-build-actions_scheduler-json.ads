--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Directed_Graph;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

private with GNATCOLL.JSON;

package GPR2.Build.Actions_Scheduler.JSON is

   package DG renames GNATCOLL.Directed_Graph;

   type Object is new GPR2.Build.Actions_Scheduler.Object with private;

   procedure Set_JSON_File
     (Self : in out Object; Path : GPR2.Path_Name.Object);
   --  Sets the location of the JSON report to generate

   overriding
   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class);

private

   use GNATCOLL.JSON;

   TEXT_ACTION_UID : constant UTF8_String := "uid";
   TEXT_COMMAND    : constant UTF8_String := "command";
   TEXT_ENV        : constant UTF8_String := "environment";
   TEXT_CWD        : constant UTF8_String := "cwd";
   TEXT_STATUS     : constant UTF8_String := "status";
   TEXT_STDOUT     : constant UTF8_String := "stdout";
   TEXT_STDERR     : constant UTF8_String := "stderr";

   type Object is new GPR2.Build.Actions_Scheduler.Object with record
      JSON      : JSON_Array := Empty_Array;
      JSON_File : GPR2.Path_Name.Object;
   end record;

   overriding
   function Collect_Action
     (Self    : in out Object;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status;

end GPR2.Build.Actions_Scheduler.JSON;
