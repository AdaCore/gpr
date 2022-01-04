------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Sockets;

with GPR2.Containers;
with GPR2.Time_Stamp;

private with Ada.Finalization;

package GPR2.Compilation.Protocol is

   pragma Warnings (Off, "already use-visible");
   use Ada.Strings.Unbounded;
   pragma Warnings (On, "already use-visible");

   use GNAT;
   use GNAT.Sockets;

   Wrong_Command : exception;
   --  Raised when a command cannot be parsed

   WD_Path_Tag : constant String := "<1>";
   --  The string replacing root working diretory of full path name, see
   --  Set_Rewrite below.

   CD_Path_Tag   : constant String := "<2>";
   --  The string replacing the compiler root directory, see Set_Rewrite below

   Any_OS : constant String := "any";
   --  Used when OS check is not necessary, for example gprclean does not need
   --  this check. It is safe to clean-up a Solaris slave from a Windows
   --  master.

   --
   --  Communication
   --

   type Communication_Channel is tagged private;
   --  A communication channel, this channel is used for any communication
   --  between the build master and the slaves.

   No_Channel : constant Communication_Channel;

   function Create
     (Sock    : Socket_Type;
      Virtual : Boolean := False) return Communication_Channel;
   --  Create a communication channel. If Virtual is True it only creates a
   --  virtual channel which cannot be used as a regular channel. This is
   --  meant to be used as a key for comparing against another channel.

   function Sock (Channel : Communication_Channel) return Socket_Type;
   pragma Inline (Sock);

   procedure Close (Channel : in out Communication_Channel);
   --  Close the channel

   procedure Set_Rewrite_WD
     (Channel : in out Communication_Channel; Path : String);
   --  Add rewrite information for the working directory. This is needed to
   --  translate paths to/from build master and slave working directories.

   procedure Set_Rewrite_CD
     (Channel : in out Communication_Channel; Path : String);
   --  Add rewrite information for the compiler directory. This is needed to
   --  translate paths to/from compilers path in build master and in slave.
   --  This is needed to be able to find the files from other projects
   --  installed with the compiler. The translated paths are in the
   --  gprbuild mapping file.

   procedure Clear_Rewrite (Channel : in out Communication_Channel);
   --  Remove any rewrite information from the channel

   function Translate_Receive
     (Channel : Communication_Channel; Str : String) return String;
   --  Translate Str using Channel rewrite

   function Translate_Send
     (Channel : Communication_Channel; Str : String) return String;
   --  Translate Str using Channel rewrite

   --
   --  Command
   --

   type Command is tagged private;

   type Command_Kind is
     (EX,  -- execute a command
      AK,  -- acknowledge received command (with pid)
      TS,  -- a file timestamp
      ES,  -- end of file timestamp
      FL,  -- a file, content being rewritten from builder/slave PATH
      FR,  -- a RAW file, no rewrite taking place
      OK,  -- compilation ok (with optional pid)
      KO,  -- compilation failed (with optional pid)
      CX,  -- master context
      CU,  -- clean-up request
      DP,  -- display output
      EC,  -- end of compilation
      SI,  -- a signal as been detected (like EC but no ACK needed)
      SY,  -- synchronization requested
      IR,  -- information requested
      PG); -- PING just to know if the slave is listening

   function Kind (Cmd : Command) return Command_Kind;
   pragma Inline (Kind);

   function Args (Cmd : Command) return OS_Lib.Argument_List_Access;
   pragma Inline (Args);
   --  Returns all arguments for Cmd

   function Output (Cmd : Command) return Unbounded_String;
   pragma Inline (Output);
   --  Returns the output for a DP command

   function Get_Command (Channel : Communication_Channel'Class) return Command;
   --  Wait and return a command as parsed from the communication channel

   Invalid_Pid : constant := -1;

   --
   --  From GPRbuild / GPRremote
   --

   procedure Send_Context
     (Channel                    : Communication_Channel;
      Target                     : String;
      Project_Name               : String;
      Build_Env                  : String;
      Sync                       : Boolean;
      Hash                       : String;
      Included_Artifact_Patterns : String);
   --  Send initial context to the slave

   procedure Send_Exec
     (Channel  : Communication_Channel;
      Project  : String;
      Dir      : String;
      Language : String;
      Target   : String;
      Runtime  : String;
      Options  : Containers.Value_List;
      Obj_Name : Name_Type;
      Dep_Name : String;
      Env      : String;
      Filter   : access function (Str, Sep : String) return String := null);
   --  Send a compilation job to a slave. The compilation must be done on
   --  Dir. This directory is specified relative to the root directory of
   --  the sources. Dep_Name is the dependency file that is generated by this
   --  compilation and which must be sent back to the build master after the
   --  compilation. Filter is a function used to make path on the command line
   --  all relatives to the root directory. The build master root in full path
   --  is replaced by Full_Path_Tag. The slaves will change back this tag to
   --  the actual full path on their working environment. The Env string is a
   --  set of environment variables (name=value[;name=value]) to set before
   --  spawning the process.
   --  If Language is empty, this is not a compilation based on a specific
   --  language. In this case the command in Options (Options'First) is to be
   --  executed as-is.

   procedure Send_File
     (Channel         : Communication_Channel;
      Path_Name       : String;
      Rewrite         : Boolean;
      Keep_Time_Stamp : Boolean := False);
   --  Path_Name is the full path name to the local filename

   procedure Sync_Files
     (Channel  : Communication_Channel;
      Root_Dir : String;
      Files    : File_Data_Set.Vector);
   --  Send a set of filenames and associated timestamps. Will receive a OK or
   --  KO with the list of files to be transferred to the slave.

   procedure Send_End_Of_Compilation (Channel : Communication_Channel);
   --  Send an end of compilation signal, the slave will at this point be able
   --  to get jobs from another build master (Get_Context).

   procedure Send_End_Of_File_List (Channel : Communication_Channel);
   --  Send an end of file list signal, it means that all files timestamps have
   --  been checked. After this the compilation can be started.

   procedure Get_Pid
     (Channel : Communication_Channel;
      Pid     : out Remote_Id;
      Success : out Boolean);
   --  Get a process id, Success is set to False in case of failure

   procedure Send_Clean_Up
     (Channel : Communication_Channel; Project_Name : String);
   --  Send a clean-up requets to the slave

   procedure Send_Sync_Request (Channel : Communication_Channel);
   --  Send a sync request to the slave

   procedure Send_Info_Request (Channel : Communication_Channel);
   --  Send a info request to the slave

   procedure Get_Info_Response
     (Channel          : Communication_Channel;
      Version_String   : out Unbounded_String;
      Current_UTC_Time : out Time_Stamp.Data;
      GPR_Hash         : out Unbounded_String;
      Success          : out Boolean);
   --  Read and return the info sent from the slave

   --
   --  From GPRslave
   --

   procedure Get_Context
     (Channel                    : Communication_Channel;
      Target                     : out Unbounded_String;
      Project_Name               : out Unbounded_String;
      Build_Env                  : out Unbounded_String;
      Sync                       : out Boolean;
      Timestamp                  : out Time_Stamp.Data;
      Version                    : out Unbounded_String;
      Hash                       : out Unbounded_String;
      Included_Artifact_Patterns : out Unbounded_String;
      Is_Ping                    : out Boolean);
   --  Wait for an initial context from a build master

   procedure Send_Slave_Config
     (Channel        : Communication_Channel;
      Max_Process    : Positive;
      Root_Directory : String;
      Clock_Status   : Boolean);
   --  Send the slave configuration to the build master

   procedure Send_Ack
     (Channel : Communication_Channel; Pid : Remote_Id);
   --  Send Acknoledgement of a received compilation job

   procedure Send_Ok
     (Channel : Communication_Channel; Pid : Remote_Id);
   --  Send Pid of a successful command

   procedure Send_Ko
     (Channel : Communication_Channel; Pid : Remote_Id);
   --  Send Pid of an un-successful command

   procedure Send_Ok (Channel : Communication_Channel);
   --  Send Ok for a successful command (clean-up for example)

   procedure Send_Ko (Channel : Communication_Channel; Message : String := "");
   --  Send Ko to initial handshake (slave not compatible with master for
   --  example).

   procedure Send_Ko
     (Channel : Communication_Channel;
      Files   : File_Data_Set.Vector);
   --  Send a Ko message with a list of file names

   procedure Send_Ping_Response
     (Channel          : Communication_Channel;
      Version_String   : String;
      Current_UTC_Time : Time_Stamp.Data;
      GPR_Hash         : String);
   --  Send a ping response with some environment information

   procedure Send_Info_Response
     (Channel          : Communication_Channel;
      Version_String   : String;
      Current_UTC_Time : Time_Stamp.Data;
      GPR_Hash         : String);
   --  Send an information response

   procedure Send_Output (Channel : Communication_Channel; File_Name : String);
   --  Send an output of a command

   procedure Get_RAW_File_Content
     (Channel   : Communication_Channel;
      Path_Name : String;
      Timestamp : Time_Stamp.Data := Time_Stamp.Empty);
   --  Create Path_Name from data received from the channel. The data must be
   --  sent by Send_RAW_File_Content to have the correct format. If specified
   --  the file's timestamp is set.

private

   type Communication_Channel is new Finalization.Controlled with record
      Sock           : Socket_Type;
      Channel        : Stream_Access;
      WD_From, WD_To : Unbounded_String; -- working directory
      CD_From, CD_To : Unbounded_String; -- compiler directory
      Refs           : Shared_Counter_Access;
   end record;

   overriding procedure Initialize (Channel : in out Communication_Channel);
   overriding procedure Adjust     (Channel : in out Communication_Channel);
   overriding procedure Finalize   (Channel : in out Communication_Channel);

   No_Channel : constant Communication_Channel :=
                  (Finalization.Controlled
                   with Sock    => No_Socket,
                        Channel => null,
                        Refs    => new Shared_Counter (1),
                        others  => Null_Unbounded_String);

   type Command is new Finalization.Controlled with record
      Cmd    : Command_Kind;
      Args   : OS_Lib.Argument_List_Access;
      Output : Unbounded_String;
      Refs   : Shared_Counter_Access;
   end record;

   overriding procedure Initialize (Cmd : in out Command);
   overriding procedure Adjust     (Cmd : in out Command);
   overriding procedure Finalize   (Cmd : in out Command);

end GPR2.Compilation.Protocol;
