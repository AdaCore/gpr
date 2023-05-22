--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package is used to avoid having GPR2 relying on process environment.
--  ADA_PROJECT_FILE, GPR_PROJECT_PATH, GPR_PROJECT_PATH_FILE,
--  GPR_RUNTIME_PATH, GPR_CONFIG, PATH or any cross compiler variables cans be
--  defined in a environment object. It is passed to GPR2 when loading a tree.

with GNATCOLL.OS.Process;

package GPR2.Environment is

   type Object is tagged private;

   function Process_Environment return Object;
   --  Environment passed to the process

   procedure Set_Inherit (Environment : in out Object; Inherit : Boolean);
   --  If Inherit is True, then the environment passed to the process is the
   --  parent environment on top of which the content of Environment is applied

   procedure Insert
     (Environment : in out Object; Key : String; Value : String);
   --  Insert Key=Value into Environment

   function Inherit (Environment : Object) return Boolean;
   --  If Inherit is True, then the environment passed to the process is the
   --  parent environment on top of which the content of Environment is applied

   function Exists (Environment : Object; Key : String) return Boolean;
   --  Return whether Environment.Value (Key) is able to return a value.

   function Value (Environment : Object; Key : String) return String;
   --  Return the Value defined for Key. Throws Constraint_Error if
   --  Environment.Exists (Key) is False

   function Value
     (Environment : Object; Key : String; Default : String) return String;
   --  Return the Value defined for Key. Return Default if
   --  Environment.Exists (Key) is False

   function To_GNATCOLL_Environment
     (Environment : Object) return GNATCOLL.OS.Process.Environment_Dict;
   --  Return the map that should be put on top of environment passed to the
   --  project

private

   type Object is tagged record
       Dict                : GNATCOLL.OS.Process.Environment_Dict;
       Inherit_Process_Env : Boolean := True;
   end record;

   Process_Env : constant Object :=
                   (Dict                =>
                                    GNATCOLL.OS.Process.Env_Dicts.Empty_Map,
                    Inherit_Process_Env => True);

   function Process_Environment return Object is (Process_Env);

   function Inherit (Environment : Object) return Boolean is
     (Environment.Inherit_Process_Env);

   function To_GNATCOLL_Environment
     (Environment : Object) return GNATCOLL.OS.Process.Environment_Dict
     is (Environment.Dict);

end GPR2.Environment;
