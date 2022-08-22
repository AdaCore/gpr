--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Calendar.Conversions;
with GPR2.Context;
with GPR2.Project.Registry.Attribute;
with GPR2.View_Ids;
with System.Storage_Elements;

package body GPR2.C.JSON.Encoders is

   use type GPR2.Project.Registry.Attribute.Value_Kind;

   function From_Address (Value : System.Address) return JSON_Value;

   function From_Address (Value : System.Address) return JSON_Value is
      use System.Storage_Elements;
      Addr_Int : constant Integer_Address := To_Integer (Value);

      --  When compiling with -fstack-usage, the following call is marked as
      --  "dynamic". Nevertheless we know that max integer for a given address
      --  is in fact bounded thus we can ignore the issue.
      Addr_Img : constant String := Addr_Int'Img;
   begin
      return From_String (Addr_Img (Addr_Img'First + 1 .. Addr_Img'Last));
   end From_Address;

   ------------------
   -- From_Boolean --
   ------------------

   function From_Boolean (Value : Boolean) return JSON_Value is
   begin
      return GNATCOLL.JSON.Create (Value);
   end From_Boolean;

   ------------------
   -- From_Context --
   ------------------

   function From_Context (Value : GPR_Context) return JSON_Value is
      JSON_Context : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      for C in Value.Iterate loop
         GNATCOLL.JSON.Set_Field
           (JSON_Context, String (GPR2.Context.Key_Value.Key (C)),
            GPR2.Context.Key_Value.Element (C));
      end loop;
      return JSON_Context;
   end From_Context;

   -------------------
   -- From_Filename --
   -------------------

   function From_Filename (Value : GPR2.Filename_Optional) return JSON_Value is
   begin
      if Value = GPR2.No_Filename then
         return GNATCOLL.JSON.JSON_Null;
      else
         return From_String (String (Value));
      end if;
   end From_Filename;

   ------------------------
   -- From_Filename_List --
   ------------------------

   function From_Filename_List
     (Value : GPR2.Containers.Filename_List) return JSON_Value
   is
      Result_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for Path of Value loop
         GNATCOLL.JSON.Append (Result_Array, From_Filename (Path));
      end loop;
      return GNATCOLL.JSON.Create (Result_Array);
   end From_Filename_List;

   ------------------------
   -- From_GPR_Attribute --
   ------------------------

   function From_GPR_Attribute (Value : GPR_Attribute) return JSON_Value is
      Result : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create_Object;
   begin
      if not Value.Is_Defined then
         Set (Result, "value", GNATCOLL.JSON.JSON_Null);
      elsif Value.Kind = GPR2.Project.Registry.Attribute.Single then
         Set (Result, "value", From_String (Value.Value.Text));
      else
         declare
            Value_Array : GNATCOLL.JSON.JSON_Array;
         begin
            for Index in 1 .. Value.Count_Values loop
               declare
                  JSON_Str : constant JSON_Value :=
                    GNATCOLL.JSON.Create
                      (Value.Values.Element (Integer (Index)).Text);
               begin
                  GNATCOLL.JSON.Append (Value_Array, JSON_Str);
               end;
            end loop;

            Set (Result, "value", GNATCOLL.JSON.Create (Value_Array));
         end;
      end if;

      if Value.Is_Defined then
         Set (Result, "is_default", GNATCOLL.JSON.Create (Value.Is_Default));
      else
         Set (Result, "is_default", GNATCOLL.JSON.Create (True));
      end if;
      return Result;
   end From_GPR_Attribute;

   ----------------------
   -- From_GPR_Message --
   ----------------------

   function From_GPR_Message (Value : GPR_Message) return JSON_Value is
      JSON_Message : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create_Object;
   begin
      Set (JSON_Message, "level", From_String (Value.Level'Img));
      Set (JSON_Message, "message", From_String (Value.Message));
      Set (JSON_Message, "sloc", From_Source_Reference (Value.Sloc));
      return JSON_Message;
   end From_GPR_Message;

   -----------------------
   -- From_GPR_Path --
   -----------------------

   function From_GPR_Path (Value : GPR_Path) return JSON_Value is
   begin
      if Value.Is_Defined then
         return From_String (Value.Value);
      else
         return GNATCOLL.JSON.JSON_Null;
      end if;
   end From_GPR_Path;

   --------------------
   -- From_GPR_Paths --
   --------------------

   function From_GPR_Paths (Value : GPR_Paths) return JSON_Value
   is
      Result_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for Path of Value loop
         GNATCOLL.JSON.Append (Result_Array, From_GPR_Path (Path));
      end loop;
      return GNATCOLL.JSON.Create (Result_Array);
   end From_GPR_Paths;

   -----------------
   -- From_GPR_Source --
   -----------------

   function From_GPR_Source (Value : GPR_Source) return JSON_Value is
      Result : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Set (Result, "path", From_GPR_Path (Value.Path_Name));
      Set (Result, "is_aggregated", From_Boolean (Value.Is_Aggregated));
      Set
        (Result, "is_compilable",
         From_Boolean (Value.Is_Compilable (No_Index)));
      Set (Result, "is_interface", From_Boolean (Value.Is_Interface));
      Set
        (Result, "has_name_exception",
         From_Boolean (Value.Has_Naming_Exception));
      Set (Result, "is_main", From_Boolean (Value.Is_Main));
      Set (Result, "language", From_Language_Id (Value.Language));
      Set (Result, "timestamp", From_Time (Value.Timestamp (ALI => False)));
      return Result;
   end From_GPR_Source;

   ----------------------
   -- From_GPR_Sources --
   ----------------------

   function From_GPR_Sources (Value : GPR_Sources) return JSON_Value is
      Sources_Array : JSON_Array;
   begin
      for Source of Value loop
         GNATCOLL.JSON.Append (Sources_Array, From_GPR_Source (Source));
      end loop;
      return GNATCOLL.JSON.Create (Sources_Array);
   end From_GPR_Sources;

   -----------------------
   -- From_GPR_Tree --
   -----------------------

   function From_GPR_Tree (Value : GPR_Tree_Access) return JSON_Value is
   begin
      return From_Address (Value.all'Address);
   end From_GPR_Tree;

   -----------------------
   -- From_GPR_View --
   -----------------------

   function From_GPR_View (Value : GPR_View) return JSON_Value is
   begin
      if Value.Is_Defined then
         return
           GNATCOLL.JSON.Create
             (GNATCOLL.JSON.UTF8_String (GPR2.View_Ids.Image (Value.Id)));
      else
         return GNATCOLL.JSON.JSON_Null;
      end if;
   end From_GPR_View;

   --------------------
   -- From_GPR_Views --
   --------------------

   function From_GPR_Views (Value : GPR_Views) return JSON_Value is
      Result_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for View of Value loop
         GNATCOLL.JSON.Append (Result_Array, From_GPR_View (View));
      end loop;
      return GNATCOLL.JSON.Create (Result_Array);
   end From_GPR_Views;

   ----------------------
   -- From_Language_Id --
   ----------------------

   function From_Language_Id (Value : GPR2.Language_Id) return JSON_Value is
   begin
      return From_String (String (GPR2.Name (Value)));
   end From_Language_Id;

   ---------------
   -- From_Name --
   ---------------

   function From_Name (Value : Optional_Name_Type) return JSON_Value is
   begin
      if Value = No_Name then
         return GNATCOLL.JSON.JSON_Null;
      else
         return From_String (String (Value));
      end if;
   end From_Name;

   -----------------------
   -- From_Project_Kind --
   -----------------------

   function From_Project_Kind (Value : GPR2.Project_Kind) return JSON_Value is
   begin
      return From_String (GPR2.Project_Kind'Image (Value));
   end From_Project_Kind;

   ---------------------------
   -- From_Source_Reference --
   ---------------------------

   function From_Source_Reference
     (Value : GPR_Source_Reference) return JSON_Value
   is
      JSON_Sloc : constant GNATCOLL.JSON.JSON_Value :=
        GNATCOLL.JSON.Create_Object;
   begin
      Set (JSON_Sloc, "filename", From_String (Value.Filename));

      if Value.Has_Source_Reference then
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "line", Value.Line);
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "column", Value.Column);
      else
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "line", GNATCOLL.JSON.JSON_Null);
         GNATCOLL.JSON.Set_Field
           (JSON_Sloc, "column", GNATCOLL.JSON.JSON_Null);
      end if;

      return JSON_Sloc;
   end From_Source_Reference;

   -----------------
   -- From_String --
   -----------------

   function From_String (Value : String) return JSON_Value is
   begin
      return GNATCOLL.JSON.Create (GNATCOLL.JSON.UTF8_String (Value));
   end From_String;

   function From_Time (Value : Ada.Calendar.Time) return JSON_Value is
      use Ada.Calendar.Conversions;
   begin
      return GNATCOLL.JSON.Create (Long_Integer (To_Unix_Time (Value)));
   end From_Time;

   --------------------
   -- From_Unit_Info --
   --------------------

   function From_Unit_Info (Value : GPR_Unit_Info) return JSON_Value is
      Result : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Set (Result, "name", From_Name (Value.Name));
      return Result;
   end From_Unit_Info;

   ---------------------
   -- From_Unit_Infos --
   ---------------------

   function From_Unit_Infos (Value : GPR_Unit_Infos) return JSON_Value is
      Units_Array : JSON_Array;
   begin
      for Unit of Value loop
         GNATCOLL.JSON.Append (Units_Array, From_Unit_Info (Unit));
      end loop;
      return GNATCOLL.JSON.Create (Units_Array);
   end From_Unit_Infos;

end GPR2.C.JSON.Encoders;
