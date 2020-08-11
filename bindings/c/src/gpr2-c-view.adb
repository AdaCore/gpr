------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;
with GPR2.C.JSON; use GPR2.C.JSON;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;

package body GPR2.C.View is

   ---------------------------------
   -- GPR2_Project_View_Aggregate --
   ---------------------------------

   function GPR2_Project_View_Aggregate
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         if View.Is_Aggregated then
            declare
               Aggregate_View : Project_View_Access :=
                                  new GPR2.Project.View.Object;
            begin
               Aggregate_View.all := View.Aggregate;
               Set_Project_View (Result, "view_id", Aggregate_View);
            end;
         else
            GNATCOLL.JSON.Set_Field (Result, "view_id",
                                    GNATCOLL.JSON.JSON_Null);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Aggregate;

   ----------------------------------
   -- GPR2_Project_View_Aggregated --
   ----------------------------------

   function GPR2_Project_View_Aggregated
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         if View.Kind in Aggregate_Kind then
            Set_Project_Views
              (Result, "view_ids", View.Aggregated);
         else
            Set_Project_Views (Result, "view_ids",
                               GPR2.Project.View.Set.Empty_Set);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Aggregated;

   ---------------------------------
   -- GPR2_Project_View_Artifacts --
   ---------------------------------

   function GPR2_Project_View_Artifacts
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         Set_Path_Name_Set_Object (Result, "artifacts", View.Artifacts);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Artifacts;

   ---------------------------------
   -- GPR2_Project_View_Attribute --
   ---------------------------------

   function GPR2_Project_View_Attribute
      (Request : C_Request;
       Answer  : out C_Answer)
      return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
            Get_Project_View (Request, "view_id");
         Attr : GPR2.Project.Attribute.Object;
      begin
         Attr := GPR2.Project.View.Attribute
            (Self => View.all,
             Name => Name_Type (Get_String (Request, "name")),
             Index => Value_Type (Get_String (Request, "index", "")));
         Set_Project_Attribute (Result, "attr", Attr);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Attribute;

   ----------------------------------
   -- GPR2_Project_View_Attributes --
   ----------------------------------

   function GPR2_Project_View_Attributes
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View     : constant Project_View_Access :=
                      Get_Project_View (Request, "view_id");
         Packages : GNATCOLL.JSON.JSON_Array;
      begin
         Set_Attributes (Result, "attributes", View.Attributes);
         if View.Has_Packages then
            for Pack of View.Packages loop
               declare
                  Package_Value : constant GNATCOLL.JSON.JSON_Value :=
                                    GNATCOLL.JSON.Create_Object;
               begin
                  GNATCOLL.JSON.Set_Field (Package_Value, "name",
                                           String (Pack.Name));
                  Set_Attributes (Package_Value, "attributes",
                                  Pack.Attributes);
                  GNATCOLL.JSON.Append (Packages, Package_Value);
               end;
            end loop;
         end if;
         GNATCOLL.JSON.Set_Field (Result, "packages", Packages);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Attributes;

   ----------------------------------------
   -- GPR2_Project_View_Binder_Artifacts --
   ----------------------------------------

   function GPR2_Project_View_Binder_Artifacts
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View     : constant Project_View_Access :=
                      Get_Project_View (Request, "view_id");
         Name     : constant Name_Type :=
                      Name_Type (Get_String (Request, "name"));
         Language : constant Optional_Name_Type :=
                      Optional_Name_Type
                        (Get_String (Request, "language", String (No_Name)));
      begin
         Set_Path_Name_Set_Object (Result, "binder_artifacts",
                                   View.Binder_Artifacts (Name, Language));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Binder_Artifacts;

   -------------------------------
   -- GPR2_Project_View_Context --
   -------------------------------

   function GPR2_Project_View_Context
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         if View.all.Has_Context then
            Set_Context (Result, "context", View.all.Context);
         else
            GNATCOLL.JSON.Set_Field (Result, "context",
                                     GNATCOLL.JSON.JSON_Null);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Context;

   --------------------------------
   -- GPR2_Project_View_Extended --
   --------------------------------

   function GPR2_Project_View_Extended
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                 Get_Project_View (Request, "view_id");
      begin
         if View.Is_Extending then
            declare
               Extended_View : Project_View_Access :=
                                 new GPR2.Project.View.Object;
            begin
               Extended_View.all := View.Extended;
               Set_Project_View (Result, "view_id", Extended_View);
            end;
         else
            GNATCOLL.JSON.Set_Field (Result, "view_id",
                                     GNATCOLL.JSON.JSON_Null);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Extended;

   ---------------------------------
   -- GPR2_Project_View_Extending --
   ---------------------------------

   function GPR2_Project_View_Extending
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         if View.Is_Extended then
            declare
               Extending_View : Project_View_Access :=
                                  new GPR2.Project.View.Object;
            begin
               Extending_View.all := View.Extending;
               Set_Project_View (Result, "view_id", Extending_View);
            end;
         else
            GNATCOLL.JSON.Set_Field (Result, "view_id",
                                     GNATCOLL.JSON.JSON_Null);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Extending;

   -------------------------------
   -- GPR2_Project_View_Imports --
   -------------------------------

   function GPR2_Project_View_Imports
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         if View.Has_Imports then
            Set_Project_Views
              (Result, "view_ids", View.Imports);
         else
            Set_Project_Views
              (Result, "view_ids", GPR2.Project.View.Set.Empty_Set);
         end if;
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Imports;

   -----------------------------------
   -- GPR2_Project_View_Information --
   -----------------------------------

   function GPR2_Project_View_Information
      (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
            Get_Project_View (Request, "view_id");
      begin
         Set_String (Result, "path_name", View.all.Path_Name.Value);
         Set_String (Result, "dir_name", View.all.Dir_Name.Value);
         Set_String (Result, "name", String (View.all.Name));
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Information;

   ------------------------------------------
   -- GPR2_Project_View_Invalidate_Sources --
   ------------------------------------------

   function GPR2_Project_View_Invalidate_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         pragma Unreferenced (Result);
      begin
         Get_Project_View (Request, "view_id").Invalidate_Sources;
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Invalidate_Sources;

   -----------------------------------
   -- GPR2_Project_View_Source_Path --
   -----------------------------------

   function GPR2_Project_View_Source_Path
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View        : constant Project_View_Access :=
                         Get_Project_View (Request, "view_id");
         Filename    : constant Simple_Name :=
                         Simple_Name (Get_String (Request, "filename"));
         Need_Update : constant Boolean :=
                         Get_Boolean (Request, "need_update", True);
      begin
         Set_String (Result, "source_path",
                     View.Source_Path (Filename, Need_Update).Value);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Source_Path;

   -----------------------------
   -- GPR2_Project_View_Types --
   -----------------------------

   function GPR2_Project_View_Types
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
      begin
         Set_Types (Result, "types", View.Types);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Types;

   ------------------------------
   -- GPR2_Project_View_Unload --
   ------------------------------

   function GPR2_Project_View_Unload
        (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : Project_View_Access := Get_Project_View (Request, "view_id");

         procedure Free is new Ada.Unchecked_Deallocation
            (GPR2.Project.View.Object, Project_View_Access);

         pragma Unreferenced (Result);
      begin
         Free (View);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Unload;

   ---------------------------------
   -- GPR2_Project_View_Variables --
   ---------------------------------

   function GPR2_Project_View_Variables
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View     : constant Project_View_Access :=
                      Get_Project_View (Request, "view_id");
         Packages : GNATCOLL.JSON.JSON_Array;
      begin
         Set_Variables (Result, "variables", View.Variables);
         if View.Has_Packages then
            for Pack of View.Packages loop
               declare
                  Package_Value : constant GNATCOLL.JSON.JSON_Value :=
                                    GNATCOLL.JSON.Create_Object;
               begin
                  Set_Variables (Package_Value, "variables", Pack.Variables);
                  GNATCOLL.JSON.Set_Field (Package_Value, "name",
                                           String (Pack.Name));
                  GNATCOLL.JSON.Append (Packages, Package_Value);
               end;
            end loop;
         end if;
         GNATCOLL.JSON.Set_Field (Result, "packages", Packages);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_Variables;

   --------------------------------
   -- GPR2_Project_View_View_For --
   --------------------------------

   function GPR2_Project_View_View_For
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         View : constant Project_View_Access :=
                  Get_Project_View (Request, "view_id");
         Name : constant Name_Type :=
                  Name_Type (Get_String (Request, "name"));
      begin
         declare
            Found_View : Project_View_Access :=  new GPR2.Project.View.Object;
         begin
            Found_View.all := View.View_For (Name);
            Set_Project_View (Result, "view_id", Found_View);
         end;
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_View_View_For;

end GPR2.C.View;
