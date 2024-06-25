--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Directed_Graph;
with GPR2.Path_Name;

private with GNATCOLL.JSON;

package GPR2.Build.Process_Manager.JSON is

   package DG renames GNATCOLL.Directed_Graph;

   type Object is new GPR2.Build.Process_Manager.Object with private;

   overriding
   function Collect_Job
        (Self           : in out Object;
         Job            : DG.Node_Id;
         Proc_Status    : Integer;
         Stdout, Stderr : Unbounded_String)
        return Collect_Status;

   procedure Execute
     (Self      : in out Object;
      Tree_Db   : GPR2.Build.Tree_Db.Object_Access;
      Jobs      : Natural := 0;
      JSON_File : GPR2.Path_Name.Object);
   --  Execute the process manager and store the jobs results in the provided
   --  JSON file ``JSON_FILE``.

   overriding
   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Jobs    : Natural := 0);
   --  Same as above, but store the jobs results in the default JSON file
   --  ``<current_directory>/jobs.json``.

   overriding
   procedure Execution_Post_Process (Self : in out Object);
   --  Write the summary of all jobs execution in a JSON file. Used internally
   --  by the process manager but needs to be public to be overridden.

private
   use GNATCOLL.JSON;

   TEXT_COMMAND : constant UTF8_String := "command";
   TEXT_STATUS  : constant UTF8_String := "status";
   TEXT_STDOUT  : constant UTF8_String := "stdout";
   TEXT_STDERR  : constant UTF8_String := "stderr";

   type Object is new GPR2.Build.Process_Manager.Object with record
      JSON      : JSON_Array := Empty_Array;
      JSON_File : GPR2.Path_Name.Object;
   end record;

end GPR2.Build.Process_Manager.JSON;