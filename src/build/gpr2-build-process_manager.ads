--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Directed_Graph;
with GPR2.Utils.Process_Manager; use GPR2.Utils.Process_Manager;
with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GPR2.Build.Tree_Db;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private with GNATCOLL.Traces;

package GPR2.Build.Process_Manager is

   package DG renames GNATCOLL.Directed_Graph;

   type Object is new GPR2.Utils.Process_Manager.Process_Manager with private;

   overriding
   function Collect_Job
     (Self           : in out Object;
      Job            : DG.Node_Id;
      Proc_Handler   : Process_Handler;
      Stdout, Stderr : Unbounded_String)
      return Collect_Status;

   overriding
   procedure Launch_Job
     (Self           : in out Object;
      Job            : DG.Node_Id;
      Proc_Handler   : out Process_Handler;
      Capture_Stdout : out File_Descriptor;
      Capture_Stderr : out File_Descriptor);

   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Jobs    : Natural := 0;
      Stop_On_Fail : Boolean := True);

private

   use GNATCOLL.Traces;

   type Object is new GPR2.Utils.Process_Manager.Process_Manager with record
      Tree_Db      : GPR2.Build.Tree_Db.Object_Access;
      Traces       : Trace_Handle := Create ("PROCESS_MANAGER");
      Stop_On_Fail : Boolean := True;
   end record;

end GPR2.Build.Process_Manager;
