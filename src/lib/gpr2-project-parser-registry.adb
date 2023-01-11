--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Maps;

package body GPR2.Project.Parser.Registry is

   package Project_Store is
     new Ada.Containers.Ordered_Maps (GPR2.Path_Name.Object,
                                      GPR2.Project.Parser.Object);

   protected Shared is

      function Exist (Pathname : GPR2.Path_Name.Object) return Boolean;

      function Get
        (Pathname : GPR2.Path_Name.Object) return Project.Parser.Object;

      procedure Register
        (Pathname : GPR2.Path_Name.Object;
         Project  : GPR2.Project.Parser.Object);

      procedure Check_Registry
        (Pathname : GPR2.Path_Name.Object;
         Project  : out GPR2.Project.Parser.Object);

      procedure Clear;

   private
      Store : Project_Store.Map;
   end Shared;

   -------------------
   -- Check_Project --
   -------------------

   function Check_Project
     (Pathname : GPR2.Path_Name.Object;
      Project  : out GPR2.Project.Parser.Object) return Boolean is
   begin
      Shared.Check_Registry (Pathname, Project);
      return Project.Is_Defined;
   end Check_Project;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache is
   begin
      Shared.Clear;
   end Clear_Cache;

   ------------
   -- Exists --
   ------------

   function Exists (Pathname : GPR2.Path_Name.Object) return Boolean is
   begin
      return Shared.Exist (Pathname);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Pathname : GPR2.Path_Name.Object) return GPR2.Project.Parser.Object
   is
   begin
      return Shared.Get (Pathname);
   end Get;

   --------------
   -- Register --
   --------------

   procedure Register
     (Pathname : GPR2.Path_Name.Object; Project : GPR2.Project.Parser.Object)
   is
   begin
      Shared.Register (Pathname, Project);
   end Register;

   ------------
   -- Shared --
   ------------

   protected body Shared is

      --------------------
      -- Check_Registry --
      --------------------

      procedure Check_Registry
        (Pathname : GPR2.Path_Name.Object;
         Project  : out GPR2.Project.Parser.Object)
      is
         CP : constant Project_Store.Cursor := Store.Find (Pathname);
      begin
         if Project_Store.Has_Element (CP) then
            declare
               Ref : constant Project_Store.Reference_Type :=
                       Store.Reference (CP);
            begin
               Project := Ref;
            end;
         else
            Project := GPR2.Project.Parser.Undefined;
         end if;
      end Check_Registry;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Store.Clear;
      end Clear;

      -----------
      -- Exist --
      -----------

      function Exist (Pathname : GPR2.Path_Name.Object) return Boolean is
      begin
         return Store.Contains (Pathname);
      end Exist;

      ---------
      -- Get --
      ---------

      function Get
        (Pathname : GPR2.Path_Name.Object) return Project.Parser.Object
      is
      begin
         return Store (Pathname);
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register
        (Pathname : GPR2.Path_Name.Object;
         Project  : GPR2.Project.Parser.Object)
      is
         use type Project_Store.Cursor;
         Pos : constant Project_Store.Cursor := Store.Find (Pathname);
      begin
         if Pos = Project_Store.No_Element then
            Store.Insert (Pathname, Project);
         end if;
      end Register;

   end Shared;

end GPR2.Project.Parser.Registry;
