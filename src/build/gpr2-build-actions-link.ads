--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;

with GNATCOLL.OS.Process;

private with GPR2.View_Ids;

package GPR2.Build.Actions.Link is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Link_Id (<>) is new Actions.Action_Id with private;

   function Create (View       : GPR2.Project.View.Object;
                    Output     : Simple_Name;
                    Is_Library : Boolean) return Link_Id;

   overriding function Image (Self : Link_Id) return String;

   overriding function Db_Filename
     (Self : Link_Id) return Simple_Name;

   overriding function "<" (L, R : Link_Id) return Boolean;

   type Object is new Actions.Object with private;
   --  Action responsible for linking Ada sources

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   function Is_Defined (Self : Object) return Boolean;

   procedure Initialize_Executable
     (Self       : in out Object;
      Executable : GPR2.Path_Name.Object;
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

   function Output (Self : Object) return Artifacts.Files.Object'Class;
   --  Return the executable or library generated by the link

   function Embedded_Objects
     (Self : Object) return Build.Tree_Db.Artifact_Sets.Set;
   --  List of objects embedded in this library or executable
   --  This includes the objects coming from embedded libraries

   function Library_Dependencies
     (Self : Object) return Actions.Action_Id_Sets.Set;
   --  List of libraries this action uses

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object;

private

   use type GPR2.View_Ids.View_Id;

   type Link_Id (Name_Len : Natural) is new Actions.Action_Id
   with record
      Is_Lib    : Boolean := False;
      View      : GPR2.Project.View.Object;
      Exec_Name : Filename_Type (1 .. Name_Len);
   end record;

   function Create (View       : GPR2.Project.View.Object;
                    Output     : Simple_Name;
                    Is_Library : Boolean) return Link_Id is
      (Output'Length, Is_Library, View, Output);

   overriding function Image (Self : Link_Id) return String is
     ((if Self.Is_Lib then "[Archive] " else "[Link] ") &
        String (Self.Exec_Name) & " (" &
        String (Self.View.Path_Name.Simple_Name) & ')');

   overriding function Db_Filename
     (Self : Link_Id) return Simple_Name is
     ((if Self.Is_Lib then "archive_" else "link_") &
        Simple_Name (Self.Exec_Name & ".json"));

   overriding function "<" (L, R : Link_Id) return Boolean is
     (if L.View.Id /= R.View.Id
      then L.View.Id < R.View.Id
      elsif L.Is_Lib < R.Is_Lib
      then L.Is_Lib
      else L.Exec_Name < R.Exec_Name);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
    (Index_Type => Positive, Element_Type => String);

   type Object is new Actions.Object with record
      Is_Library      : Boolean := False;

      Executable      : Artifacts.Files.Object;
      --  Executable produced by the linker

      Library         : Artifacts.Library.Object;
      --  Library produced by the linker

      Ctxt            : GPR2.Project.View.Object;
      --  The view defining the Main, or the library

      Static_Options : String_Vectors.Vector := String_Vectors.Empty_Vector;
      --  Command line options added manually with the Add_Option procedure
   end record;

   overriding procedure Compute_Signature (Self : in out Object);

   function Is_Library (Self : Object) return Boolean is
     (Self.Is_Library);

   function Output (Self : Object) return Artifacts.Files.Object'Class is
     (if Self.Is_Library then Self.Library else Self.Executable);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
     (Self.Ctxt);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object is
     (if Self.Ctxt.Is_Library then Self.Ctxt.Library_Directory
      else Self.Ctxt.Executable_Directory);

end GPR2.Build.Actions.Link;
