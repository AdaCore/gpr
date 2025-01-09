--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.View_Ids;

package GPR2.Build.Artifacts.Source is

   type Object is new Artifacts.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create
     (View  : GPR2.Project.View.Object;
      Name  : Simple_Name;
      Index : Unit_Index := No_Index) return Object;

   overriding function Image (Self : Object) return String;

   function Index (Self : Object) return Unit_Index;

   overriding function "<" (L, R : Object) return Boolean;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

   overriding procedure Unserialize
     (Ctxt : GPR2.Project.View.Object;
      S    : String;
      Val  : out Object);

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class;

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest;

   function Path (Self : Object) return GPR2.Path_Name.Object;

   function Source_Name (Self : Object) return Simple_Name;

private

   use type GPR2.Path_Name.Object;
   use type GPR2.View_Ids.View_Id;

   type Object is new Artifacts.Object with record
      Name  : Unbounded_String;
      View  : GPR2.Project.View.Object;
      Path  : GPR2.Path_Name.Object;
      Index : Unit_Index := No_Index;
   end record;

   overriding function Protocol (Self : Object) return String is
     ("src");

   overriding function "<" (L, R : Object) return Boolean is
     (if L.View.Id /= R.View.Id
      then L.View.Id < R.View.Id
      elsif L.Name /= R.Name
      then L.Name < R.Name
      else L.Index < R.Index);

   Undefined : constant Object := (others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest
   is (Utils.Hash.Hash_File (Self.Path.Value));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Path);

   Sep : constant Character := '/';
   --  Separator between simple name and unit index if any.

   function Suffix (Index : Unit_Index) return String is
     (if Index = No_Index then "" else Sep & Index'Image);

   overriding function Image (Self : Object) return String is
     (-Self.Name & Suffix (Self.Index));

   function Index (Self : Object) return Unit_Index is
      (Self.Index);

   overriding function SLOC
     (Self : Object) return Source_Reference.Object'Class is
      (Source_Reference.Create (Self.Path.Value, 0, 0));

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (GPR2.View_Ids.Image (Self.View.Id) & Self.Image));

   function Source_Name (Self : Object) return Simple_Name is
     (-Self.Name);

end GPR2.Build.Artifacts.Source;
