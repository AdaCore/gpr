--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Directed_Graph;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

private with GNATCOLL.JSON;

package GPR2.Build.Process_Manager.JSON is

   package DG renames GNATCOLL.Directed_Graph;

   type Object is new GPR2.Build.Process_Manager.Object with private;

   procedure Set_JSON_File
     (Self : in out Object;
      Path : GPR2.Path_Name.Object);
   --  Sets the location of the JSON report to generate

   overriding function Collect_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Context        : in out Process_Execution_Context;
      Proc_Handler   : Process_Handler;
      Stdout, Stderr : Unbounded_String)
      return Collect_Status;

   overriding procedure Execute
     (Self            : in out Object;
      Tree_Db         : GPR2.Build.Tree_Db.Object_Access;
      Context         : access Process_Execution_Context;
      Options         : PM_Options);
   --  Same execution as GPR2.Build.Process_Manager, but with a summary of all
   --  job executions written to a JSON file.

private

   use GNATCOLL.JSON;

   TEXT_ACTION_UID : constant UTF8_String := "uid";
   TEXT_COMMAND    : constant UTF8_String := "command";
   TEXT_ENV        : constant UTF8_String := "environment";
   TEXT_CWD        : constant UTF8_String := "cwd";
   TEXT_STATUS     : constant UTF8_String := "status";
   TEXT_STDOUT     : constant UTF8_String := "stdout";
   TEXT_STDERR     : constant UTF8_String := "stderr";

   type Object is new GPR2.Build.Process_Manager.Object with record
      JSON      : JSON_Array := Empty_Array;
      JSON_File : GPR2.Path_Name.Object;
   end record;

end GPR2.Build.Process_Manager.JSON;
