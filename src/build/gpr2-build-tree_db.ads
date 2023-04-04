--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Containers.Hashed_Maps;

with GPR2.Build.View_Db;
with GPR2.Log;
limited with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.View_Ids;

package GPR2.Build.Tree_Db is

   type Object is tagged limited private;
   type Object_Access is access all Object;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Tree (Self : Object) return access GPR2.Project.Tree.Object
     with Pre => Self.Is_Defined;

   procedure Load
     (Self : in out Object;
      Tree : GPR2.Project.Tree.Object)
     with Pre => not Self.Is_Defined;
   --  Initializes the object. This operation will load an existing database
   --  if any, or will collect the sources for the tree.

   procedure Unload (Self : in out Object)
     with Post => not Self.Is_Defined;

   procedure Refresh (Self         : in out Object;
                      With_Runtime : Boolean;
                      Messages     : out GPR2.Log.Object)
     with Pre => Self.Is_Defined;

   function View_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Pre => Self.Is_Defined
                   and then View.Is_Defined
                   and then View.Kind in With_Object_Dir_Kind;

   function Reference (Self : Object) return access Object
     with Pre => Self.Is_Defined;

private

   package Build_DB_Maps is new Ada.Containers.Hashed_Maps
     (GPR2.View_Ids.View_Id, Build.View_Db.Object,
      GPR2.View_Ids.Hash, GPR2.View_Ids."=", Build.View_Db."=");

   type Object is tagged limited record
      Self       : access Object;
      Tree       : access GPR2.Project.Tree.Object;
      Build_Dbs  : Build_DB_Maps.Map;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self.Self /= null);

   function Reference (Self : Object) return access Object is
     (Self'Unrestricted_Access);

end GPR2.Build.Tree_Db;
