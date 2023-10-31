--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Tree;
private with GPR2.Project.Typ;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;

package GPR2.Project.External is

   --  This API provides a mean to obtain externals for a given project.
   --
   --  An external value is an expression whose value is obtained
   --  from the command that invoked the processing of the current
   --  project file (typically a gprbuild commmand).
   --
   --  Example:
   --   type Type_Name is ("A", "B");
   --   Variable : Type_Name := external ("EXTERNAL_NAME", "A")
   --  In this case, the External EXTERNAL_NAME is "typed", and has
   --  two possible values: "A" and "B". In the case where EXTERNAL_NAME
   --  is undefined, "A" is its default value.
   --
   --  Another example:
   --   Variable_2 := external ("EXTERNAL_NAME_2", "default")
   --  In this case, EXTERNAL_NAME_2 is untyped, and can take any value.

   type Object is tagged private;

   type External_Arr is array (Positive range <>) of Object;

   function Externals
     (Tree      : GPR2.Project.Tree.Object;
      Root_Only : Boolean := False)
      return External_Arr;
   --  Return both typed and untyped externals found in the provided tree

   function Name (Ext : Object) return String;
   --  Return the external name, which is the first field of the external.
   --  For example, A in "external ("A", "default-value").

   function Is_Typed (Ext : Object) return Boolean;
   --  Return True if an external is typed

   function Is_Conflicting (Ext : Object) return Boolean;
   --  Return True if an external has been assigned several types whose
   --  values are not identicals.

   type Unbounded_String_Array is
     array (Positive range <>) of Unbounded_String;

   function Possible_Values_Of (Ext : Object) return Unbounded_String_Array;
      --  In case of a typed variable, return all possible values.
      --  Otherwise, return an empty array.

private

   type Type_Assignment is record
      Typ             : GPR2.Project.Typ.Object;
      Assignment_Sloc : Source_Reference.Object;
   end record;

   package Type_Assignments_Vec is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Type_Assignment);

   package Unbounded_String_Sets is new Ada.Containers.Ordered_Sets
   (Element_Type => Unbounded_String);
   type Object is tagged record
      Name              : Unbounded_String;
      Typed             : Boolean;
      Conflicting       : Boolean := False;
      Possible_Values   : Unbounded_String_Sets.Set;
      Types_Assignments : Type_Assignments_Vec.Vector;
   end record;

end GPR2.Project.External;
