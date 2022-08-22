--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This object represents an attribute index. Such index can be "others" or
--  any string representing a language or a source filename for example.

with GPR2.Source_Reference.Value;

package GPR2.Project.Attribute_Index is

   type Object is new Source_Reference.Value.Object with private;

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if the attribute's index is equal to Value taking into
   --  account the case-sensitivity of the index.

   Undefined : constant Object;

   Any       : constant Object;
   --  Represents any index values

   I_Others  : constant Object;
   --  others index

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Index          : Source_Reference.Value.Object;
      Is_Others      : Boolean;
      Case_Sensitive : Boolean) return Object
     with Pre  => Index.Is_Defined,
          Post => Create'Result.Is_Defined;
   --  Creates an attribute index for the given Index source reference

   function Create
     (Value          : Value_Type;
      Case_Sensitive : Boolean := False) return Object
     with Post => Create'Result.Is_Defined;
   --  Creates a built-in attribute index

   function Create
     (Value : Language_Id) return Object;
   --  Creates an attribute index for the given language

   function Is_Others (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if this attribute index is the special others index

   function Is_Any_Index (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the attribute can be returned from the set for any
   --  index in a request. Main case to use such attributes is to get attribute
   --  with default value from the set when the default value defined for any
   --  index.

   function Is_Case_Sensitive (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Value
     (Self          : Object;
      Preserve_Case : Boolean := True) return Value_Type
     with Pre => Self.Is_Defined;
   --  Returns the actual value for the given attribute index

   procedure Set_Case
     (Self              : in out Object;
      Is_Case_Sensitive : Boolean)
     with Pre => Self.Is_Defined;

private

   type Object is new Source_Reference.Value.Object with record
      Is_Others      : Boolean := False;
      Case_Sensitive : Boolean := True;
   end record
     with Dynamic_Predicate =>
       (if Object.Is_Others
        then Source_Reference.Value.Object (Object).Text = "others");

   Undefined : constant Object :=
                 (Source_Reference.Value.Undefined with others => <>);

   I_Others  : constant Object :=
                    (Source_Reference.Value.Object
                       (Source_Reference.Value.Create
                          (Filename => "/others",
                           Line     => 0,
                           Column   => 0,
                           Text     => "others")) with
                     Is_Others => True,
                     Case_Sensitive => False);

   Any       : constant Object :=
                 (Source_Reference.Value.Object
                    (Source_Reference.Value.Create
                       (Filename => "/any",
                        Line     => 0,
                        Column   => 0,
                        Text     => (1 => ASCII.NUL))) with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Others (Self : Object) return Boolean is (Self.Is_Others);

   function Is_Any_Index (Self : Object) return Boolean is (Self = Any);

   function Create
     (Index          : Source_Reference.Value.Object;
      Is_Others      : Boolean;
      Case_Sensitive : Boolean) return Object
   is (Index with Is_Others, Case_Sensitive);

   function Create
     (Value          : Value_Type;
      Case_Sensitive : Boolean := False) return Object
   is
     (Create (Source_Reference.Value.Object
              (Source_Reference.Value.Create
               (Source_Reference.Builtin, Value)),
              False, Case_Sensitive));

   function Create
     (Value : Language_Id) return Object
   is
     (Create (Source_Reference.Value.Object
              (Source_Reference.Value.Create
               (Source_Reference.Builtin, Value_Type (Name (Value)))),
              False, False));

   function Is_Case_Sensitive (Self : Object) return Boolean is
     (Self.Case_Sensitive);

   function Value
     (Self          : Object;
      Preserve_Case : Boolean := True) return Value_Type
   is
     (if Preserve_Case
      then Self.Text
      else Ada.Characters.Handling.To_Lower (Self.Text));

end GPR2.Project.Attribute_Index;
