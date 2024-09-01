--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Calendar;

limited with GPR2.Build.Tree_Db;
with GPR2.Build.Unit_Info.List;
with GPR2.Path_Name;

package GPR2.Build.Source_Base is

   type Object is tagged private;
   --  Source represents the data computed from the source filename and
   --  the project view only: in particular unit(s) and kind information come
   --  from naming schema and naming exceptions.
   --
   --  For Ada. it should be noted that the default naming schema is ambiguous,
   --  so the kind of unit can't be fully trusted (it can be a body or a
   --  separate).
   --  Unit names are also tentative names, deduced from the filename. As this
   --  relationship between filename and unit name, and can be very wrong
   --  in particular, but not only, if the filename is krunched.
   --  Naming exceptions give normally the good unit names, but they can also
   --  have typos, that are hard to report to the user.
   --
   --  Only the Object_Info object (so after parsing the ALI or the source)
   --  give correct results.

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True if Self is defined

   function Create
     (Filename         : GPR2.Path_Name.Object;
      Language         : Language_Id;
      Kind             : Unit_Kind;
      Timestamp        : Ada.Calendar.Time;
      Tree_Db          : access GPR2.Build.Tree_Db.Object;
      Naming_Exception : Boolean;
      Source_Dir_Idx   : Natural;
      Is_Compilable    : Boolean := False)
      return Object'Class
     with Pre  => Filename.Is_Defined,
          Post => Create'Result.Is_Defined;
   --  Constructor for a non-Ada source object

   procedure Update_Unit
     (Self : in out Object;
      Unit : Unit_Info.Object)
     with Pre => Self.Is_Defined
                   and then Self.Language = Ada_Language;
   --  Change the unit info stored in Self with updated information in Unit

   procedure Remove_Unit
     (Self : in out Object;
      Index : Unit_Index)
     with Pre => Self.Has_Unit_At (Index),
          Post => not Self.Has_Unit_At (Index);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the path-name for the given source

   function Modification_Time
     (Self       : Object;
      ALI_Format : Boolean := False) return Ada.Calendar.Time
     with Inline, Pre => Self.Is_Defined;
   --  Returns last modification of the source file from the time point when
   --  the last successful build was done.
   --  If ALI_Format is set, the returned value is adjusted to match the
   --  ALI files time precision.

   procedure Update_Modification_Time
     (Self : in out Object;
      Time : Ada.Calendar.Time);
   --  Update the source modification time field.

   function Language (Self : Object) return Language_Id
     with Pre => Self.Is_Defined;
   --  Returns the language of the source

   function Has_Units (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source is unit-based (i.e. Ada)

   function Kind
     (Self  : Object;
      Index : Unit_Index := No_Index) return Unit_Kind
     with Pre => Self.Is_Defined
                 and then (not Self.Has_Units
                           or else Self.Has_Unit_At (Index))
                 and then (Self.Has_Units or else Index = No_Index);
   --  Returns the kind of Self's source at the given index

   function Has_Unit_At
     (Self : Object; Index : Unit_Index) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self has a compilation unit at Index

   function Has_Single_Unit (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns true if the source contains a single unit

   function Has_Index (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source contains one or more units declared in Naming
   --  package with "at" Index.

   function Unit
     (Self  : Object;
      Index : Unit_Index := No_Index) return Unit_Info.Object
     with Pre => Self.Is_Defined
                   and then Self.Has_Units and then Self.Has_Unit_At (Index);
   --  Return the unit object at specified index (use No_Index if the source
   --  is a single unit source.

   function Units (Self : Object) return Unit_Info.List.Object
     with Inline,
          Pre  => Self.Is_Defined and then Self.Has_Units;
   --  Returns all compilation units for self.

   function Has_Naming_Exception (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the source comes from a naming exception

   function Is_Compilable (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Source_Dir_Value_Index
     (Self : Object) return Natural
     with Pre => Self.Is_Defined;
   --  Used to determine internally if a source overrides another.

private

   function To_ALI_Timestamp (Stamp : Calendar.Time) return Calendar.Time;

   type Object is tagged record
      Db                : access Build.Tree_Db.Object;
      --  The view's build database
      Path_Name         : GPR2.Path_Name.Object;
      --  Source path
      Modification_Time : Calendar.Time := No_Time;
      --  Source file modification time
      Language          : Language_Id   := No_Language;
      --  Source's language
      Kind              : Unit_Kind := S_Separate;
      --  Source library unit kind in case the language is not unit-based
      CU_List           : Unit_Info.List.Object;
      --  Source's units in case of unit-based language
      Inherited         : Boolean := False;
      --  Whether the source has been inherited by project extension
      Naming_Exception  : Boolean := False;
      --  Whether a naming exception concerns this source
      Is_Compilable     : Boolean := False;
      --  Whether the source can be compiled (e.g. we need a compiler to
      --  build a source for the specified language)
      Source_Dir_Idx    : Natural := 0;
      --  The value of Source_Dirs responsible for loading this value
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

   function Modification_Time
     (Self       : Object;
      ALI_Format : Boolean := False) return Ada.Calendar.Time
   is (if ALI_Format then To_ALI_Timestamp (Self.Modification_Time)
       else Self.Modification_Time);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path_Name);

   function Has_Units (Self : Object) return Boolean is
     (Self.Language = Ada_Language);
   --  For now, only supported language defining compilation units is Ada

   function Has_Single_Unit (Self : Object) return Boolean is
     (Self.CU_List.Length = 1);

   function Has_Index (Self : Object) return Boolean is
     (Self.CU_List.Is_Indexed_List);

   function Language (Self : Object) return Language_Id is
     (Self.Language);

   function Units (Self : Object) return Unit_Info.List.Object is
     (Self.CU_List);

   function Unit
     (Self  : Object;
      Index : Unit_Index := No_Index) return Unit_Info.Object
   is (Self.CU_List.Element (Index));

   --  function Aggregated (Self : Object) return Project.View.Object is
   --    (Self.Aggregated);
   --
   --  function Is_Aggregated (Self : Object) return Boolean is
   --    (Self.Aggregated.Is_Defined);

   function Is_Compilable (Self : Object) return Boolean is
     (Self.Is_Compilable);

   function Has_Naming_Exception (Self : Object) return Boolean is
     (Self.Naming_Exception);

   function Source_Dir_Value_Index
     (Self : Object) return Natural is
     (Self.Source_Dir_Idx);

end GPR2.Build.Source_Base;
