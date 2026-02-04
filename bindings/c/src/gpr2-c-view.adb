--
--  Copyright (C) 2020-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with GPR2.Build.Source.Sets;  --  GNAT 20250530 bug: not referenced warning
pragma Warnings (On);
with GPR2.C.JSON.Arrays;
with GPR2.C.JSON.Codecs.Path_Names;
with GPR2.C.JSON.Codecs.Sources;
with GPR2.C.JSON.Values;
with GPR2.C.Registry;
with GPR2.Project.View;

package body GPR2.C.View is

   function Get_View
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.View.Object;

   -----------------
   -- Constructor --
   -----------------

   procedure Constructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      View : constant GPR2.Project.View.Object := Get_View (Request);

   begin
      Result.Insert ("name", String (View.Name));
      Result.Insert ("path_name", String (View.Path_Name.Value));
   end Constructor;

   ----------------
   -- Destructor --
   ----------------

   procedure Destructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      pragma Unreferenced (Result);

   begin
      GPR2.C.Registry.View.Unregister (Request.Value ("view_id"));
   end Destructor;

   -----------------
   -- Executables --
   -----------------

   procedure Executables
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      View : constant GPR2.Project.View.Object := Get_View (Request);

   begin
      Result.Insert
        ("executables",
         (if View.Is_Defined
          then GPR2.C.JSON.Codecs.Path_Names.Encode
                 (View.Executables).To_JSON_Value
          else GPR2.C.JSON.Values.Null_Value));
   end Executables;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.View.Object is
   begin
      return GPR2.C.Registry.View.Lookup (Request.Value ("view_id"));
   end Get_View;

   ----------------------
   -- Object_Directory --
   ----------------------

   procedure Object_Directory
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      View : constant GPR2.Project.View.Object := Get_View (Request);

   begin
      Result.Insert
        ("object_directory",
         (if View.Is_Defined and then View.Kind in With_Object_Dir_Kind
          then GPR2.C.JSON.Values.To_JSON_Value
                 (String (View.Object_Directory.Value))
          else GPR2.C.JSON.Values.Null_Value));
   end Object_Directory;

   -------------
   -- Sources --
   -------------

   procedure Sources
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      View    : constant GPR2.Project.View.Object := Get_View (Request);
      Sources : GPR2.C.JSON.Arrays.JSON_Array;

   begin
      if View.Is_Defined then
         for Source of View.Sources loop
            Sources.Append (GPR2.C.JSON.Codecs.Sources.Encode (Source));
         end loop;
      end if;

      Result.Insert ("sources", Sources);
   end Sources;

   ---------------------
   -- Visible_Sources --
   ---------------------

   procedure Visible_Sources
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      View    : constant GPR2.Project.View.Object := Get_View (Request);
      Sources : GPR2.C.JSON.Arrays.JSON_Array;

   begin
      if View.Is_Defined then
         for Source of View.Visible_Sources loop
            Sources.Append (GPR2.C.JSON.Codecs.Sources.Encode (Source));
         end loop;
      end if;

      Result.Insert ("visible_sources", Sources);
   end Visible_Sources;

end GPR2.C.View;
