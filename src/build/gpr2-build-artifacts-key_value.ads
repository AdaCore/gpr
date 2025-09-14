--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private with Ada.Strings.Hash;

package GPR2.Build.Artifacts.Key_Value is

   type Object is new Artifacts.Object with private;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create
     (Key   : Value_Type;
      Value : Value_Type) return Object;

   overriding function Serialize (Self : Object) return String;

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object);

   overriding
   function Checksum
     (Self : Object; Hash : in out Utils.Hash.Object) return String;

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type;

   overriding function Image (Self : Object) return String;

private

   type Object is new Artifacts.Object with record
      Key   : Unbounded_String;
      Value : Unbounded_String;
   end record;

   overriding function Protocol (Self : Object) return String is
     ("keyvalue");

   overriding function "<" (L, R : Object) return Boolean is
     (if L.Key /= R.Key then L.Key < R.Key else L.Value < R.Value);

   Undefined : constant Object := (others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Create (Key : Value_Type; Value : Value_Type) return Object
   is (Key => +Key, Value => +Value);

   pragma Warnings (Off, "*formal parameter ""Hash"" is not referenced*");
   overriding
   function Checksum
     (Self : Object; Hash : in out Utils.Hash.Object) return String
   is (-Self.Value);
   pragma Warnings (On, "*formal parameter ""Hash"" is not referenced*");

   overriding function Serialize (Self : Object) return String is
     (-Self.Key);

   overriding function Hash (Self : Object) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (-Self.Key & (-Self.Value)));

   overriding function Image (Self : Object) return String is
      (Self.Protocol & "(" & To_String (Self.Key) & ", "
       & To_String (Self.Value));

end GPR2.Build.Artifacts.Key_Value;
