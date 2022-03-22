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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Containers;

package body GPR2.Compilation.Process is

   use type Containers.Count_Type;

   package Env_Maps renames GPR2.Containers.Name_Value_Map_Package;
   --  A set of key=value

   package Prj_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Env_Maps.Map, Env_Maps."<", Env_Maps."=");
   --  A set of project+language=map

   function "<" (Left, Right : Id) return Boolean is
     (Left.R_Pid < Right.R_Pid);

   package Failures_Slave_Set is
     new Ada.Containers.Indefinite_Ordered_Maps (Id, Optional_Name_Type);

   --  function Get_Env
   --    (Project  : GPR2.Project.View.Object;
   --     Language : Language_Id) return String;
   --  --  Get the environment for a specific project and language
   --
   --  function To_Argument_List
   --    (List : Containers.Value_List) return GNAT.OS_Lib.Argument_List;
   --  --  Returns an argument list representation for the list of values

   Environments : Prj_Maps.Map;

   type Process_Data is record
      Process : Id;
      Status  : Boolean;
   end record;

   package Endded_Process is
     new Ada.Containers.Doubly_Linked_Lists (Process_Data);

   protected Results is
      procedure Add (Result : Process_Data);
      --  entry Get (Result : out Process_Data);

      procedure Record_Remote_Failure (Pid : Id; Slave : Name_Type);
      --  This is to be able to display on which slaves a specific compilation
      --  has failed.

      function Get_Slave_For (Pid : Id) return Optional_Name_Type;
      --  Returns the remote slave for the given compilation, or the empty
      --  string if the compilation was successful.

   private
      List        : Endded_Process.List;
      Failed_Proc : Failures_Slave_Set.Map;
   end Results;

   ----------------
   -- Add_Result --
   ----------------

   procedure Add_Result
     (Process : Id;
      Status  : Boolean;
      Slave   : Optional_Name_Type := "") is
   begin
      Results.Add (Process_Data'(Process, Status));

      --  For a compilation failure records the slave to be able to report it

      if not Status and then Slave /= "" then
         Results.Record_Remote_Failure (Process, Slave);
      end if;
   end Add_Result;

   ------------------
   -- Create_Local --
   ------------------

   function Create_Local (Pid : GNAT.OS_Lib.Process_Id) return Id is
   begin
      return Id'(Local, Pid);
   end Create_Local;

   -------------------
   -- Create_Remote --
   -------------------

   function Create_Remote (Pid : Remote_Id) return Id is
   begin
      return Id'(Remote, Pid);
   end Create_Remote;

   -------------
   -- Get_Env --
   -------------

   --  function Get_Env
   --    (Project  : GPR2.Project.View.Object;
   --     Language : Language_Id) return String
   --  is
   --     Key  : constant Name_Type :=
   --              Name_Type (String (Project.Name) & "+" &
   --                           String (Name (Language)));
   --     Res  : Unbounded_String;
   --  begin
   --     if Environments.Contains (Key) then
   --        for C in Environments (Key).Iterate loop
   --           if Res /= Null_Unbounded_String then
   --              Res := Res & Opts_Sep;
   --           end if;
   --
   --           Append
   --             (Res,
   --              String (Env_Maps.Key (C)) & '=' & Env_Maps.Element (C));
   --        end loop;
   --     end if;
   --
   --     return To_String (Res);
   --  end Get_Env;

   -------------------
   -- Get_Slave_For --
   -------------------

   function Get_Slave_For (Pid : Id) return Optional_Name_Type is
   begin
      if Pid.Kind = Local then
         return "";

      else
         return Results.Get_Slave_For (Pid);
      end if;
   end Get_Slave_For;

   ----------
   -- Hash --
   ----------

   function Hash (Process : Id) return Header_Num is
      use GNAT;
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
   begin
      if Process.Kind = Local then
         return Header_Num (OS_Lib.Pid_To_Integer (Process.Pid) mod Modulo);
      else
         return Header_Num (Process.R_Pid mod Remote_Id (Modulo));
      end if;
   end Hash;

   ------------------------
   -- Record_Environment --
   ------------------------

   procedure Record_Environment
     (Project  : GPR2.Project.View.Object;
      Language : Name_Type;
      Name     : Name_Type;
      Value    : Value_Type)
   is
      Key      : constant Name_Type :=
                   Name_Type (String (Project.Name) & "+" & String (Language));
      New_Item : Env_Maps.Map;
   begin
      --  Create new item, variable association

      New_Item.Include (Name, Value);

      if Environments.Contains (Key) then
         if Environments (Key).Contains (Name) then
            Environments (Key).Replace (Name, Value);
         else
            Environments (Key).Insert (Name, Value);
         end if;

      else
         Environments.Insert (Key, New_Item);
      end if;
   end Record_Environment;

   -------------
   -- Results --
   -------------

   protected body Results is

      ---------
      -- Add --
      ---------

      procedure Add (Result : Process_Data) is
      begin
         List.Append (Result);
      end Add;

      ---------
      -- Get --
      ---------

      --  entry Get (Result : out Process_Data) when List.Length /= 0 is
      --  begin
      --     Result := List.First_Element;
      --     List.Delete_First;
      --  end Get;

      -------------------
      -- Get_Slave_For --
      -------------------

      function Get_Slave_For (Pid : Id) return Optional_Name_Type is
         use type Failures_Slave_Set.Cursor;
         Pos : constant Failures_Slave_Set.Cursor := Failed_Proc.Find (Pid);
      begin
         if Pos = Failures_Slave_Set.No_Element then
            return "";
         else
            return Failures_Slave_Set.Element (Pos);
         end if;
      end Get_Slave_For;

      ---------------------------
      -- Record_Remote_Failure --
      ---------------------------

      procedure Record_Remote_Failure (Pid : Id; Slave : Name_Type) is
      begin
         Failed_Proc.Insert (Pid, Slave);
      end Record_Remote_Failure;

   end Results;

   --  ---------
   --  -- Run --
   --  ---------
   --
   --  function Run
   --    (Executable    : Name_Type;
   --     Options       : GPR2.Containers.Value_List;
   --     Project       : GPR2.Project.View.Object;
   --     GPR_Options   : GPRtools.Options.Object;
   --     Obj_Name      : Name_Type;
   --     Source        : String := "";
   --     Language      : Language_Id := No_Language;
   --     Dep_Name      : String := "";
   --     Output_File   : String := "";
   --     Err_To_Out    : Boolean := False;
   --     Force_Local   : Boolean := False;
   --     Response_File : Path_Name.Object := Path_Name.Undefined) return Id
   --  is
   --     use GNAT.OS_Lib;
   --
   --     Env : constant String := Get_Env (Project, Language);
   --     Success : Boolean;
   --
   --  begin
   --     --  Run locally first, then send jobs to remote slaves. Note that to
   --     --  build remotely we need an output file and a language, if one of
   --    --  this requirement is not fulfilled we just run the process locally.
   --
   --     if Force_Local
   --       or else not GPR_Options.Distributed_Mode
   --       or else Local_Process.Count < GPR_Options.Maximum_Processes
   --       or else Output_File /= ""
   --       or else Language = No_Language
   --     then
   --        Run_Local : declare
   --           P    : Id (Local);
   --           Args : String_List_Access :=
   --                    new String_List'(To_Argument_List (Options));
   --        begin
   --           Set_Env (Env, Fail => True);
   --
   --           if Response_File.Is_Defined then
   --              declare
   --                 Opts : constant GNAT.OS_Lib.Argument_List :=
   --                          (1 => new String'("@" & Response_File.Value));
   --              begin
   --                 P.Pid := Non_Blocking_Spawn (String (Executable), Opts);
   --              end;
   --
   --           elsif Output_File /= "" then
   --              P.Pid := Non_Blocking_Spawn
   --                (String (Executable), Args.all, Output_File, Err_To_Out);
   --
   --           elsif Source /= "" then -- and then not No_Complete_Output then
   --              P.Pid := Non_Blocking_Spawn
   --                (String (Executable), Args.all,
   --                 Stdout_File => Source & ".stdout",
   --                 Stderr_File => Source & ".stderr");
   --
   --           else
   --              if Source /= "" then
   --                 Delete_File (Source & ".stdout", Success);
   --                 Delete_File (Source & ".stderr", Success);
   --              end if;
   --
   --              P.Pid := Non_Blocking_Spawn (String (Executable), Args.all);
   --           end if;
   --
   --           --  ??? to be written
   --           --  Script_Write (String (Executable), Options);
   --           Free (Args);
   --
   --           Local_Process.Increment;
   --
   --           return P;
   --        end Run_Local;
   --
   --     else
   --        --  Even if the compilation is done remotely make sure that any
   --        --  .stderr/.stdout from a previous local compilation are removed.
   --
   --        if Source /= "" then
   --           Delete_File (Source & ".stdout", Success);
   --           Delete_File (Source & ".stderr", Success);
   --        end if;
   --
   --        return Registry.Run
   --          (Project, Language, Options, Obj_Name, Dep_Name, Env);
   --     end if;
   --  end Run;

   ----------------------
   -- To_Argument_List --
   ----------------------

   --  function To_Argument_List
   --    (List : Containers.Value_List) return GNAT.OS_Lib.Argument_List
   --  is
   --     Ret : GNAT.OS_Lib.Argument_List (1 .. Natural (List.Length));
   --  begin
   --     for J in 1 .. List.Last_Index loop
   --        Ret (J) := new String'(List (J));
   --     end loop;
   --
   --     return Ret;
   --  end To_Argument_List;

   -----------------
   -- Wait_Result --
   -----------------

   --  procedure Wait_Result (Process : out Id; Status : out Boolean) is
   --     Data : Process_Data;
   --  begin
   --     Results.Get (Data);
   --     Process := Data.Process;
   --     Status := Data.Status;
   --  end Wait_Result;

end GPR2.Compilation.Process;
