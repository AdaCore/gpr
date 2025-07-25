--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Compilation_Unit.Maps;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

package GPR2.Build.Actions.Link is

   type Link_Kind is (Executable, Global_Archive, Library);

   type Link_Id (<>) is new Actions.Action_Id with private;

   type Object is new Actions.Object with private;
   --  Action responsible for linking Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class
     with Pre => Self.Is_Defined;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize
     (Self     : in out Object;
      Kind     : Link_Kind;
      Context  : GPR2.Project.View.Object       := GPR2.Project.View.Undefined;
      Src      : Compilation_Unit.Unit_Location := Compilation_Unit.No_Unit;
      No_Rpath : Boolean                        := True;
      Output   : Filename_Optional              := "");

   procedure Add_Objects_From_Attribute
     (Self : Object;
      Id   : Q_Attribute_Id);

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   procedure Add_Option (Self : in out Object; Option : String)
     with Pre => Self.Is_Defined;
   --  Add an option to the linking command line

   procedure Set_Bind_Action
     (Self : in out Object;
      Bind : Actions.Ada_Bind.Object);

   procedure Set_Has_Library_Dependency_Circle
     (Self  : in out Object;
      State : Boolean);
   --  Mark the action as having a library dependency circle requiring the use
   --  of --start-group --end-group as linker option

   function Options (Self : Object'Class) return Containers.Value_List;
   --  Return the options added with the Add_Option procedure

   function Is_Library (Self : Object'Class) return Boolean;

   function Is_Static_Library (Self : Object'Class) return Boolean;

   function Interface_Units
     (Self : Object'Class) return Compilation_Unit.Maps.Map
     with Pre => Self.Is_Defined and then Self.View.Is_Library;
   --  Return the complete list of Ada units that are exposed by this library.
   --  For regular libraries this will be the list of units owned by the view
   --  or the aggregated views (aggregate library case).
   --  For standalone libraries this will be the list of units listed by the
   --  Library_Interface or Interfaces attributes complemented by their
   --  dependencies.

   function Output (Self : Object) return Artifacts.Files.Object'Class
     with Pre => Self.Is_Defined;
   --  Return the executable or library generated by the link

   function Embedded_Objects
     (Self : Object'Class) return Build.Tree_Db.Artifact_Sets.Set;
   --  List of objects embedded in this library or executable.
   --  This includes the objects coming from embedded libraries.

   function Library_Dependencies
     (Self : Object'Class) return Actions.Action_Id_Vectors.Vector
     with Pre => Self.Is_Defined;
   --  List of libraries this action uses

   overriding function On_Tree_Insertion
     (Self : Object;
      Db   : in out GPR2.Build.Tree_Db.Object) return Boolean
     with Pre => Self.Is_Defined;

   overriding function On_Ready_State
     (Self : in out Object) return Boolean
     with Pre => Self.Is_Defined;

   overriding procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding procedure Compute_Response_Files
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);

   overriding function Pre_Command
     (Self : in out Object) return Boolean;

   overriding function Post_Command
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;

   overriding function Skip (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;

private

   package PAI renames GPR2.Project.Attribute_Index;
   package PRA renames GPR2.Project.Registry.Attribute;

   type Link_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      Is_Static_Lib : Boolean := False;
      View          : GPR2.Project.View.Object;
      Exec_Name     : Filename_Type (1 .. Name_Len);
   end record;

   overriding function View (Self : Link_Id) return Project.View.Object is
     (Self.View);

   overriding function Action_Class (Self : Link_Id) return Value_Type is
     (if Self.Is_Static_Lib
      then "Archive"
      else "Link");

   overriding function Language (Self : Link_Id) return Language_Id is
     (No_Language);

   overriding function Action_Parameter (Self : Link_Id) return Value_Type
   is (Value_Type (Self.Exec_Name));

   type Object is new Actions.Object with record
      Is_Library     : Boolean := False;
      Is_Static      : Boolean := False;
      In_Obj         : Boolean := False;

      Executable     : Artifacts.Files.Object;
      --  Executable produced by the linker

      Main_Src       : Compilation_Unit.Unit_Location;
      --  Source of the Main when an executable is produced

      Library        : Artifacts.Library.Object;
      --  Library produced by the linker

      Ctxt           : GPR2.Project.View.Object;
      --  The view defining the Main, or the library

      Static_Options : Containers.Value_List :=
                         Containers.Empty_Value_List;
      --  Command line options added manually with the Add_Option procedure

      Bind            : Actions.Ada_Bind.Object;
      --  The bind action generating the initialisation of the linked library

      No_Rpath        : Boolean := False;
      --  When set, the RPATH will not be set for shared libraries resolution

      Lib_Dep_Circle  : Boolean := False;
      --  Whether the libraries are inter-dependent

      Lib_Symbol_File : Artifacts.Files.Object;
      --  User-defined list of exported symbols
   end record;

   procedure Handle_Export_File
     (Self           : in out Object;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean;
      No_Warning     : Boolean);
   --  Generates the export file that lists the symbols to be exported
   --  by the shared library and add the proper options to the command line.
   --  This can have no effects if symbols cannot be filtered or if the
   --  library symbol policy is unrestricted.

   overriding procedure Compute_Signature
     (Self      : in out Object;
      Load_Mode : Boolean);

   overriding function Extended (Self : Object) return Object is
     (raise Internal_Error with "This action is not extending");

   function Check_Archive_Driver (Self : Object'Class) return Boolean;
   --  True if the archive driver is found

   function Check_Linker_Driver (Self : Object) return Boolean;
   --  True if the linker driver is found

   function Is_Library (Self : Object'Class) return Boolean is
     (Self.Is_Library);

   function Is_Static_Library (Self : Object'Class) return Boolean is
     (Self.Is_Library and then Self.Is_Static);

   function Output (Self : Object) return Artifacts.Files.Object'Class is
     (if Self.Is_Library then Self.Library else Self.Executable);

   function Options (Self : Object'Class) return Containers.Value_List is
     (Self.Static_Options);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Check_Archive_Driver (Self : Object'Class) return Boolean is
     (Self.View.Attribute (PRA.Archive_Builder).Is_Defined
      and then not Self.View.Attribute (PRA.Archive_Builder).Values.Is_Empty);

   function Check_Linker_Driver (Self : Object) return Boolean is
     (Self.View.Attribute (PRA.Linker.Driver).Is_Defined
      and then Self.View.Attribute (PRA.Linker.Driver).Value.Text'Length > 0);

   overriding function Skip (Self : Object) return Boolean is
     ((Self.Is_Static_Library and then not Self.Check_Archive_Driver) or else
      not Self.Check_Linker_Driver);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (if Self.In_Obj
      then Self.Ctxt.Object_Directory
      elsif Self.Ctxt.Is_Library
      then Self.Ctxt.Library_Directory
      else Self.Ctxt.Executable_Directory);

end GPR2.Build.Actions.Link;
