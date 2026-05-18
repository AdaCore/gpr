--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Command_Line;
with GPR2.Build.Response_Files;

package GPR2.Build.Actions.Process is

   type Object is abstract new GPR2.Build.Actions.Object with private;
   --  Base type for actions executed as external processes

   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is abstract;
   --  Return the command line and environment corresponding to the action
   --  If Signature_Only is set, then no temp file should be generated, and
   --  only the arguments that are part of the signature are to be computed.

   procedure Compute_Response_Files
     (Self : in out Object; Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is null;
   --  Return the command line and response files corresponding to the action
   --  If Signature_Only is set, then no temp file should be generated, and
   --  only the arguments that are part of the signature are to be computed.

   overriding
   function Write_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String) return Boolean;
   --  Used to store the signature of the action after it has been executed.
   --  Returns false in case an expected artifact is missing.

   overriding
   procedure Load_Signature
     (Self : in out Object; Check_Checksums : Boolean := True);
   --  Load the signature checksums from the build DB. The obtained signature
   --  is saved in the action. If Check_Checksums is True, compare the current
   --  action signature to the loaded one, and invalidate it as soon as a
   --  mismatch is found. Invalidating a signature means clearing all its
   --  checksums.

   procedure Update_Command_Line (Self : in out Object'Class; Slot : Positive);
   --  Updates the command line and update the signature accordingly

   function Command_Line (Self : Object) return GPR2.Build.Command_Line.Object;

   function Response_File
     (Self : Object) return GPR2.Build.Response_Files.Object;

private

   type Object is abstract new GPR2.Build.Actions.Object with record
      Cmd_Line       : GPR2.Build.Command_Line.Object;
      --  Command line used to run the action. Used also in the signature
      Response_Files : GPR2.Build.Response_Files.Object;
   end record;

   function Command_Line (Self : Object) return GPR2.Build.Command_Line.Object
   is (Self.Cmd_Line);

   function Response_File
     (Self : Object) return GPR2.Build.Response_Files.Object
   is (Self.Response_Files);
end GPR2.Build.Actions.Process;
