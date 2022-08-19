--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Containers;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attr_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Value;

package GPR2.Project.Attribute is

   use all type Registry.Attribute.Value_Kind;

   type Object is new Attr_Values.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Index   : Attribute_Index.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean := False;
      Frozen  : Boolean := False) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = 1
                  and then Create'Result.Is_Default = Default
                  and then Create'Result.Is_Frozen = Frozen;
   --  Creates a single-valued attribute.
   --  Default: sets the "default" state of the attribute value
   --  Frozen:  sets the "frozen" state of the attribute value.
   --           when an attribute is frozen, any write to its value will raise
   --           an error.

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Index   : Attribute_Index.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean := False) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = Values.Length
                  and then Create'Result.Is_Default = Default;
   --  Creates a multi-valued object

   overriding function Create
     (Name  : Source_Reference.Attribute.Object;
      Value : Source_Reference.Value.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = 1;
   --  Creates a single-valued object

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean;
      Frozen  : Boolean := False) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = 1
                  and then Create'Result.Is_Default = Default
                  and then Create'Result.Is_Frozen = Frozen;
   --  Creates a single-valued object with default flag

   overriding function Create
     (Name   : Source_Reference.Attribute.Object;
      Values : Containers.Source_Value_List) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = Values.Length;
   --  Creates a multi-valued object

   function Create
     (Name    : Source_Reference.Attribute.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name.Id = Name.Id
                  and then Create'Result.Count_Values = Values.Length
                  and then Create'Result.Is_Default = Default;
   --  Creates a multi-valued object with Default flag

   function Create
     (Name    : Q_Attribute_Id;
      Index   : Attribute_Index.Object;
      Source  : GPR2.Path_Name.Object;
      Default : Value_Type;
      As_List : Boolean) return Object
     with Post => Create'Result.Is_Default
                  and then Create'Result.Name.Id = Name;

   function Create
     (Name                 : Q_Attribute_Id;
      Index                : Value_Type;
      Index_Case_Sensitive : Boolean;
      Source               : GPR2.Path_Name.Object;
      Default              : Value_Type;
      As_List              : Boolean) return Object
     with Post => Create'Result.Is_Default
                  and then Create'Result.Name.Id = Name;

   function Has_Index (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the attribute has an index

   procedure Set_Index
     (Self  : in out Object;
      Index : Attribute_Index.Object);
   --  Overrwrite the attribute's index value

   function Index (Self : Object) return Attribute_Index.Object
     with Inline,
          Pre  => Self.Is_Defined,
          Post => Self.Has_Index = Index'Result.Is_Defined;
   --  Returns the attribute's index value

   procedure Set_Case
     (Self                    : in out Object;
      Index_Is_Case_Sensitive : Boolean;
      Value_Is_Case_Sensitive : Boolean)
     with Pre => Self.Is_Defined;
   --  Sets attribute case sensitivity for the index and the value.
   --  By default both are case-sensitive.

   function Image
     (Self : Object; Name_Len : Natural := 0) return String
     with Pre => Self.Is_Defined;
   --  Returns a string representation. The attribute name is represented with
   --  Name_Len characters (right padding with space) except if Name_Len is 0.

   procedure Set_Default_Flag
     (Self       : in out Object;
      Is_Default : Boolean)
     with Pre  => Self.Is_Defined,
          Post => Self.Is_Default = Is_Default;
   --  Set the default flag

   function Is_Default (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Attribute did not exist in attribute set and was created from default
   --  value.

   procedure Freeze (Self : in out Object)
     with Pre  => Self.Is_Defined,
          Post => Self.Is_Frozen;
   --  Set the freeze state of the attribute

   function Is_Frozen (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  The freeze state of the attribute value. If an attribute is frozen, then
   --  its value shall not be modified.

   function Get_Alias
     (Self     : Object;
      New_Name : Q_Attribute_Id) return Object
     with Pre => Self.Is_Defined
          and then Self.Name.Id.Pack = New_Name.Pack;
   --  Indicate that this attribute is another name for an existing attribute

   function Is_Alias (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Indicates whether this attribute is an alias of another attribute

   procedure Set_From_Config
     (Self        : in out Object;
      From_Config : Boolean)
     with Pre => Self.Is_Defined;
   --  Sets the From_Config flag

   function Is_From_Config (Self : Object) return Boolean;
   --  Whether the attribute comes from the configuration project

   overriding function Rename
     (Self : Object;
      Name : Source_Reference.Attribute.Object) return Object
     with Pre => Self.Is_Defined;
   --  Returns object with the new name Name, and the is_default flag set

private

   type Value_At_Pos (Length : Natural) is record
      Value  : Value_Type (1 .. Length);
      At_Pos : Unit_Index := No_Index;
   end record;

   function "<" (Left, Right : Value_At_Pos) return Boolean is
     (Left.Value < Right.Value
      or else (Left.Value = Right.Value and then Left.At_Pos < Right.At_Pos));

   function Create
     (Index          : Attribute_Index.Object;
      Default_At_Pos : Unit_Index := No_Index) return Value_At_Pos;
   --  Create the key Value_At_Pos for the given index

   type Object is new Attr_Values.Object with record
      Index       : Attribute_Index.Object;
      Default     : Boolean := False;
      Frozen      : Boolean := False;
      Is_Alias    : Boolean := False;
      From_Config : Boolean := False;
   end record;

   function Case_Aware_Index (Self : Object) return Value_At_Pos is
     (Create
        (Index          => Self.Index,
         Default_At_Pos => At_Pos_Or (Self.Index, No_Index)));
   --  Returns Index in lower case if index is case insensitive, returns as is
   --  otherwise.

   Undefined : constant Object := (Attr_Values.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Default (Self : Object) return Boolean is (Self.Default);

   function Is_Frozen (Self : Object) return Boolean is (Self.Frozen);

   function Is_Alias (Self : Object) return Boolean is (Self.Is_Alias);

   function Is_From_Config
     (Self : Object) return Boolean is (Self.From_Config);

end GPR2.Project.Attribute;
