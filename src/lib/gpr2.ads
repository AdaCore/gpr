--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  This is the root package of the GPR2 project support library. There is
--  different child units:
--
--     Parser
--        This child unit and all the children are for the low-level parsing
--        and support. This layer is based on the LangKit parser.
--
--     Project
--        This child unit and all the children are the high-level API to
--         work with projects. This is the end-user API.
--
--     Message
--        Messages (warnings, error,information) with source reference
--
--     Context
--        Context of a project
--
--     Builtin
--        The project's build-in implementation
--
--     Log
--        Set of messages
--
--     Source
--        Represent a source file
--
--     Source_Reference
--        Represent a source file reference (line, column).
--
--     Unit
--        A unit with its spec and possible bodies (main body and separates)

with GNAT.Regexp;

with Ada.Containers;

private with Ada.Calendar;
private with Ada.Characters.Handling;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash;
private with Ada.Strings.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;
private with GNATCOLL.Utils;

pragma Warnings
  (Off, """System.OS_Constants"" is an internal GNAT unit");
private with System.OS_Constants;
pragma Warnings (On);

package GPR2 is

   Project_Error : exception;
   --  Raised when an error related to project syntax occurs. This exception is
   --  raised with a minimal message but the actual messages are to be found in
   --  the Tree log messages.

   Processing_Error : exception;
   --  Raised when an error related to project processing occurs. Exception is
   --  raised with a minimal message but the actual messages are to be found in
   --  the Tree log messages.

   Attribute_Error : exception;
   --  Raised when querying an invalid attribute (not in the registry,
   --  invalid context, ...).

   type Project_Kind is
     (K_Configuration, K_Abstract,
      K_Standard, K_Library, K_Aggregate, K_Aggregate_Library);

   subtype Aggregate_Kind
     is Project_Kind range K_Aggregate .. K_Aggregate_Library;

   subtype With_Source_Dirs_Kind is Project_Kind range
     K_Standard .. K_Library;

   subtype With_Object_Dir_Kind is Project_Kind
     with Static_Predicate =>
       With_Object_Dir_Kind in With_Source_Dirs_Kind | K_Aggregate_Library;

   function Image (Kind : Project_Kind) return String;
   --  Returns a human representation of kind value

   --
   --  Name / Value
   --

   type Optional_Name_Type is new String;
   --  with Dynamic_Predicate =>
   --    (for all C of Optional_Name_Type => C not in '/' | '\');

   No_Name : constant Optional_Name_Type;

   subtype Name_Type is Optional_Name_Type
     with Dynamic_Predicate => Name_Type'Length > 0;
   --  A non case sensitive name

   subtype Value_Type is String;
   --  A basic string type (case-sensitive, may be empty)

   subtype Value_Not_Empty is Value_Type
     with Dynamic_Predicate => Value_Not_Empty'Length > 0;
   --  A string type which cannot be empty

   --  filenames for example.

   overriding function "=" (Left, Right : Optional_Name_Type) return Boolean;
   overriding function "<" (Left, Right : Optional_Name_Type) return Boolean;
   function Hash (N : Optional_Name_Type) return Ada.Containers.Hash_Type;

   function To_Lower (Name : Name_Type) return Value_Not_Empty;
   --  Convert name to lowercased String. Need to be able to use "in" operator
   --  because of predefined equality used, see ARM 2012 4.5.2 28.1/4.

   type Filename_Optional is new String;

   overriding function "=" (Left, Right : Filename_Optional) return Boolean;
   overriding function "<" (Left, Right : Filename_Optional) return Boolean;

   subtype Filename_Type is Filename_Optional
     with Dynamic_Predicate => Filename_Type'Length > 0;

   subtype Simple_Name is Filename_Type
     with Dynamic_Predicate =>
       (for all C of Simple_Name => C not in '/' | '\');
   --  A simple name, non empty and without some characters not allowed in

   No_Filename : constant Filename_Optional;

   No_Value : constant Value_Type;

   function Unquote (Str : Value_Type) return Value_Type with
     Post => (if Unquote'Result'Length >= 2
                and then
                 ((Str (Str'First) = ''' and then Str (Str'Last) = ''')
                  or else
                  (Str (Str'First) = '"' and then Str (Str'Last) = '"'))
              then
               Str (Str'First + 1 .. Str'Last - 1) = Unquote'Result);

   function Quote
     (Str        : Value_Type;
      Quote_With : Character := '"') return Value_Type
     with Post => Quote'Result'Length = Str'Length + 2
                    and then
                  ((Quote'Result (Quote'Result'First) = '''
                    and then Quote'Result (Quote'Result'Last) = ''')
                   or else
                     (Quote'Result (Quote'Result'First) = '"'
                      and then Quote'Result (Quote'Result'Last) = '"'));

   type Case_Sensitive_Name_Type is new String
     with Dynamic_Predicate => Case_Sensitive_Name_Type'Length > 0;
   --  A case sensitive name

   procedure Set_Debug (Mode : Character; Enable : Boolean := True);
   --  Sets global debug flag's value

   function Is_Debug (Mode : Character) return Boolean;
   --  Gets global debug flag's value

   type Word is mod 2 ** 32;

   File_Names_Case_Sensitive : constant Boolean;
   On_Windows                : constant Boolean;

   function Hash (Fname : Filename_Type) return Ada.Containers.Hash_Type;

   function To_Hex_String (Num : Word) return String;

   function Has_Directory_Separator (Name : String) return Boolean;
   --  Returns True if Name contains directory separator character

   function Parent_Name (Name : Name_Type) return Optional_Name_Type;
   --  Returns name prefix before last dot.
   --  Returns No_Name if there is not dot in the name.

   function Is_Runtime_Unit_Name (Name : Name_Type) return Boolean;
   --  Return True if Name is "Ada", "System", "GNAT", "Interfaces",
   --  "Calendar", "Direct_IO", "IO_Exceptions", "Machine_Code",
   --  "Unchecked_Conversion", "Unchecked_Deallocation" or
   --  started with "Ada.", "System.", "GNAT.", "Interfaces.".

   function Compile_Regexp
     (Filename_Regexp : Filename_Optional) return GNAT.Regexp.Regexp;
   --  Returns Regexp object for Filename_Regexp pattern
   --  Allows '?' & '*' wildchars. Use case insensitive match when required

   --  Compilation unit index for multi-unit sources

   type Unit_Index is new Integer range 0 .. Integer'Last;
   --  Index of a compilation unit part in a source file

   No_Index          : constant Unit_Index := 0;
   --  Value of the unit index in case the source contains a single unit or
   --  is not unit based.

   subtype Multi_Unit_Index is Unit_Index range 1 .. Unit_Index'Last;
   --  Value range of the unit index

   --  Name tables definition

   type Language_Id is new Natural with Default_Value => 0;

   No_Language  : constant Language_Id;
   Ada_Language : constant Language_Id;
   C_Language   : constant Language_Id;
   CPP_Language : constant Language_Id; -- C++

   function "+" (L : Optional_Name_Type) return Language_Id;
   function Name (L : Language_Id) return Optional_Name_Type;
   function Image (L : Language_Id) return String;
   function Hash (L : Language_Id) return Ada.Containers.Hash_Type;

   type Optional_Attribute_Id is new Natural with Default_Value => 0;
   subtype Attribute_Id is Optional_Attribute_Id range
     1 .. Optional_Attribute_Id'Last;
   No_Attribute : constant Optional_Attribute_Id;
   function "+" (Name : Optional_Name_Type) return Optional_Attribute_Id;
   function Name (Id : Optional_Attribute_Id) return Optional_Name_Type;
   function Image (Id : Optional_Attribute_Id) return String;
   function Hash (Id : Optional_Attribute_Id) return Ada.Containers.Hash_Type;

   type Package_Id is new Natural with Default_Value => 0;
   Project_Level_Scope : constant Package_Id;

   function "+" (Name : Optional_Name_Type) return Package_Id;
   function Name (Id : Package_Id) return Optional_Name_Type;
   function Image (Id : Package_Id) return String;
   function Hash (Id : Package_Id) return Ada.Containers.Hash_Type;

   type Q_Optional_Attribute_Id is record
      Pack : Package_Id;
      Attr : Optional_Attribute_Id;
   end record;
   --  A qualified name is an attribute name possibly prefixed with a package
   --  name. It is the only way to create a non-ambiguous reference to an
   --  attribute.

   subtype Q_Attribute_Id is Q_Optional_Attribute_Id
     with Dynamic_Predicate => Q_Attribute_Id.Attr in Attribute_Id'Range;

   No_Attribute_Id : constant Q_Optional_Attribute_Id;

   function "<" (Left, Right : Q_Attribute_Id) return Boolean;
   function Image (Name : Q_Attribute_Id) return String;
   --  Returns qualified name image

private

   use Ada;
   use Ada.Strings.Unbounded;

   No_Name             : constant Optional_Name_Type := "";
   No_Value            : constant Value_Type := "";
   No_Filename         : constant Filename_Optional := "";
   No_Time             : Calendar.Time renames GNATCOLL.Utils.No_Time;
   No_Language         : constant Language_Id := 0;
   Ada_Language        : constant Language_Id := 1;
   C_Language          : constant Language_Id := 2;
   CPP_Language        : constant Language_Id := 3;
   No_Attribute        : constant Optional_Attribute_Id := 0;
   Project_Level_Scope : constant Package_Id := 0;
   No_Attribute_Id     : constant Q_Optional_Attribute_Id :=
                           (Project_Level_Scope, No_Attribute);

   function Image (Kind : Project_Kind) return String is
     ((case Kind is
         when K_Standard          => "standard",
         when K_Configuration     => "configuration",
         when K_Abstract          => "abstract",
         when K_Library           => "library",
         when K_Aggregate         => "aggregate",
         when K_Aggregate_Library => "aggregate library") & " project");

   Debug : array (Character range '0' .. 'Z') of Boolean := (others => False);

   function Get_File_Names_Case_Sensitive return Integer
     with Import, Convention => C,
          External_Name => "__gnat_get_file_names_case_sensitive";

   File_Names_Case_Sensitive : constant Boolean :=
                                 Get_File_Names_Case_Sensitive /= 0;

   On_Windows : constant Boolean :=
                  System.OS_Constants."="
                    (System.OS_Constants.Target_OS,
                     System.OS_Constants.Windows);

   function "+"
     (Source : String) return Unbounded_String renames To_Unbounded_String;

   function "-"
     (Source : Unbounded_String) return String renames To_String;

   function To_Mixed (A : String) return String;

   function Get_Tools_Directory return String;
   --  Get the GNAT prefix

   function To_Lower (Name : Name_Type) return Value_Not_Empty is
     (Ada.Characters.Handling.To_Lower (String (Name)));

   function Has_Directory_Separator (Name : String) return Boolean is
     (for some Char of Name => GNATCOLL.Utils.Is_Directory_Separator (Char));

   function Compile_Regexp
     (Filename_Regexp : Filename_Optional) return GNAT.Regexp.Regexp
   is
     (GNAT.Regexp.Compile
        (Pattern        => String (Filename_Regexp),
         Glob           => True,
         Case_Sensitive => File_Names_Case_Sensitive));

   function Hash (Fname : Filename_Type) return Ada.Containers.Hash_Type
   is (if File_Names_Case_Sensitive
       then Ada.Strings.Hash (String (Fname))
       else Ada.Strings.Hash_Case_Insensitive (String (Fname)));

   -------------------
   -- String tables --
   -------------------

   package Name_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   package Name_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   type Name_List is record
      Name_To_Id : Name_Maps.Map;
      Id_To_Name : Name_Vectors.Vector;
   end record;

   function Id
     (List : in out Name_List; Name : Optional_Name_Type) return Natural;
   function Name
     (List : Name_List; Id : Natural) return Optional_Name_Type;
   function Image
     (List : Name_List; Id : Natural) return String;

   Language_List : Name_List;

   function "+" (L : Optional_Name_Type) return Language_Id is
     (Language_Id (Id (Language_List, L)));
   function Name (L : Language_Id) return Optional_Name_Type is
     (Name (Language_List, Natural (L)));
   function Image (L : Language_Id) return String is
     (Image (Language_List, Natural (L)));
   function Hash (L : Language_Id) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (L));

   Attr_Id_List : Name_List;

   function "+" (Name : Optional_Name_Type) return Optional_Attribute_Id is
     (Optional_Attribute_Id (Id (Attr_Id_List, Name)));
   function Name (Id : Optional_Attribute_Id) return Optional_Name_Type is
     (Name (Attr_Id_List, Natural (Id)));
   function Image (Id : Optional_Attribute_Id) return String is
     (Image (Attr_Id_List, Natural (Id)));
   function Hash (N : Optional_Name_Type) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (String (N)));
   function Hash
     (Id : Optional_Attribute_Id) return Ada.Containers.Hash_Type
   is
     (Ada.Containers.Hash_Type (Id));

   Pck_Id_List : Name_List;

   function "+" (Name : Optional_Name_Type) return Package_Id is
     (Package_Id (Id (Pck_Id_List, Name)));
   function Name (Id : Package_Id) return Optional_Name_Type is
     (Name (Pck_Id_List, Natural (Id)));
   function Image (Id : Package_Id) return String is
     (Image (Pck_Id_List, Natural (Id)));
   function Hash (Id : Package_Id) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Id));

   function "<" (Left, Right : Q_Attribute_Id) return Boolean is
     (if Left.Pack /= Right.Pack then Left.Pack < Right.Pack
      else Left.Attr < Right.Attr);
   function Image (Name : Q_Attribute_Id) return String is
     (if Name.Pack = Project_Level_Scope
      then Image (Name.Attr)
      else Image (Name.Pack) & "'" & Image (Name.Attr));

   function Is_Debug (Mode : Character) return Boolean is
     (Debug (Mode));

end GPR2;
