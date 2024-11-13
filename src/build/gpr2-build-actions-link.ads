--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

with GNATCOLL.OS.Process;

package GPR2.Build.Actions.Link is

   type Link_Id (<>) is new Actions.Action_Id with private;

   function Create (View       : GPR2.Project.View.Object;
                    Output     : Simple_Name;
                    Is_Library : Boolean) return Link_Id;

   type Object is new Actions.Object with private;
   --  Action responsible for linking Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize_Executable
     (Self       : in out Object;
      Src        : Artifacts.File_Part.Object;
      Context    : GPR2.Project.View.Object);
   --  Initialize a link action.

   procedure Initialize_Library
     (Self    : in out Object;
      Context : GPR2.Project.View.Object);
   --  Initialize a link action to link a library

   overriding function View (Self : Object) return GPR2.Project.View.Object;

   procedure Add_Option (Self : in out Object; Option : String);
   --  Add an option to the linking command line

   function Is_Library (Self : Object) return Boolean;

   function Is_Static_Library (Self : Object) return Boolean;

   function Output (Self : Object) return Artifacts.Files.Object'Class;
   --  Return the executable or library generated by the link

   function Embedded_Objects
     (Self : Object) return Build.Tree_Db.Artifact_Sets.Set;
   --  List of objects embedded in this library or executable
   --  This includes the objects coming from embedded libraries

   function Library_Dependencies
     (Self : Object) return Actions.Action_Id_Sets.Set;
   --  List of libraries this action uses

   overriding function On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object) return Boolean;

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict;
      Slot : Positive);

   overriding function Skip (Self : Object) return Boolean;

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

private

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   type Link_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      Is_Lib    : Boolean := False;
      View      : GPR2.Project.View.Object;
      Exec_Name : Filename_Type (1 .. Name_Len);
   end record;

   overriding function View (Self : Link_Id) return Project.View.Object is
     (Self.View);

   overriding function Action_Class (Self : Link_Id) return Value_Type is
     (if Self.Is_Lib
      then "Archive"
      else "Link");

   overriding function Language (Self : Link_Id) return Language_Id is
     (No_Language);

   overriding function Action_Parameter (Self : Link_Id) return Value_Type
   is (Value_Type (Self.Exec_Name));

   function Create (View       : GPR2.Project.View.Object;
                    Output     : Simple_Name;
                    Is_Library : Boolean) return Link_Id is
     (Output'Length, Is_Library, View, Output);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
    (Index_Type => Positive, Element_Type => String);

   type Object is new Actions.Object with record
      Is_Library      : Boolean := False;

      Executable      : Artifacts.Files.Object;
      Main_Src        : Artifacts.File_Part.Object;
      --  Executable produced by the linker

      Library         : Artifacts.Library.Object;
      --  Library produced by the linker

      Ctxt            : GPR2.Project.View.Object;
      --  The view defining the Main, or the library

      Static_Options  : String_Vectors.Vector := String_Vectors.Empty_Vector;
      --  Command line options added manually with the Add_Option procedure
   end record;

   overriding procedure Compute_Signature
     (Self   : in out Object;
      Stdout : Unbounded_String;
      Stderr : Unbounded_String);

   function Check_Archive_Driver (Self : Object) return Boolean;
   --  True if the archive driver is found

   function Check_Linker_Driver (Self : Object) return Boolean;
   --  True if the linker driver is found

   function Is_Library (Self : Object) return Boolean is
     (Self.Is_Library);

   function Is_Static_Library (Self : Object) return Boolean is
     (Self.Is_Library
      and then Self.View.Library_Kind in "static" | "static-pic");

   function Output (Self : Object) return Artifacts.Files.Object'Class is
     (if Self.Is_Library then Self.Library else Self.Executable);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Check_Archive_Driver (Self : Object) return Boolean is
     (Self.View.Attribute (PRA.Archive_Builder).Is_Defined
      and then not Self.View.Attribute (PRA.Archive_Builder).Values.Is_Empty);

   function Check_Linker_Driver (Self : Object) return Boolean is
     (Self.View.Attribute (PRA.Linker.Driver).Is_Defined
      and then Self.View.Attribute (PRA.Linker.Driver).Value.Text'Length > 0);

   overriding function Skip (Self : Object) return Boolean is
     (Self.View.Is_Externally_Built or else
      (Self.Is_Static_Library and then not Self.Check_Archive_Driver) or else
      not Self.Check_Linker_Driver);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (if Self.Ctxt.Is_Library then Self.Ctxt.Library_Directory
      else Self.Ctxt.Executable_Directory);

end GPR2.Build.Actions.Link;
