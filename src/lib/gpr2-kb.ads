--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Knowledge base manipulations
--
--  This package is mostly intended to be used by GPR2 tools, users intending
--  to simply load the project and explore its contents should not call any
--  of these subprograms directly.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name.Set;
with GPR2.Project.Configuration;
with GPR2.Source_Reference;

private with GNAT.Regpat;

package GPR2.KB is

   pragma Warnings (Off, "already use-visible");
   use Ada.Strings.Unbounded;
   pragma Warnings (On, "already use-visible");

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns whether Self is defined

   type Flags_Kind is (Compiler_Info, Pedantic, Validation);
   type Parsing_Flags is array (Flags_Kind) of Boolean;
   --  Describes set of possible option while parsing the knowledge base.
   --
   --  Compiler_Info indicate whether part of KB related to compilers shall be
   --  parsed. This is not needed for target normalization and may be skipped.
   --
   --  Pedantic expects strict accordance between the expected knowledge
   --  base scheme and actual files parsed. When parsing an older knowledge
   --  base some attributes may be missing (i.e. canonical target) and that
   --  would lead to Invalid raised.
   --
   --  Validation indicates that the contents of the knowledge base should be
   --  first validated with an XSD schema that comes with predefined KB.

   Default_Flags : constant Parsing_Flags;
   --  Default set of flags used by gprtools

   Targetset_Only_Flags : constant Parsing_Flags;
   --  Flags used for loading targets definitions only

   Default_Location_Error : exception;
   --  Raised when default location of the knowledge base cannot be found

   function Default_Location return GPR2.Path_Name.Object
     with Post => Default_Location'Result.Is_Defined;
   --  Returns the default location of the knowledge database. This is based
   --  on the location of gprbuild in path. If the default location cannot
   --  be found or doesn't exist, raises Default_Location_Error.

   function Create
     (Flags      : Parsing_Flags := Targetset_Only_Flags;
      Default_KB : Boolean := True;
      Custom_KB  : GPR2.Path_Name.Set.Object := GPR2.Path_Name.Set.Empty_Set)
      return Object
     with Post => Create'Result.Is_Defined;
   --  Main entry point for creating a KB object.
   --  The Flags will indicate how the knowledge base is read.
   --  If Default_KB is set, then the default knowledge base embedded in the
   --  gpr2 library is used to create the object. Otherwise, an empty knowledge
   --  base is used.
   --  Custom_KB provides a list of additional directories to use when reading
   --  the knowledge base. If Default_KB is set, those directories will be used
   --  in conjunction with the default knowledge base, while if Default_Kb is
   --  not set, only those locations will be parsed.

   function Create
     (Location : GPR2.Path_Name.Object;
      Flags    : Parsing_Flags) return Object
     with Pre  => Location.Is_Defined and then Location.Exists,
          Post => Create'Result.Is_Defined;
   --  Parses info from the knowledge base available at Location,
   --  and store it in memory.
   --  Location must be defined and can be either a single file or a directory,
   --  in which case all of its contents are parsed (no in-depth recursion).
   --  Only information relevant to the current host is parsed.
   --  If the base contains incorrect data, resulting object will have error
   --  messages.
   --  During KB parsing and configuration creation a set of calls to external
   --  processes is performed in order to collect info on compilers. Each call
   --  is perfromed only once and cashed for efficiency.

   function Create
     (Content : GPR2.Containers.Value_List;
      Flags   : Parsing_Flags) return Object
     with Post => Create'Result.Is_Defined;
   --  Same as above, but the knowledge base is parsed from a list of Values

   function Create_Default
     (Flags : Parsing_Flags) return Object
     with Post => Create_Default'Result.Is_Defined;
   --  Parses default contents of the knowledge base embedded
   --  into the library.

   function Create_Empty return Object
     with Post => Create_Empty'Result.Is_Defined;
   --  Creates an empty but initialized knowledge base to be later updated
   --  with additional chunks.

   procedure Add
     (Self     : in out Object;
      Flags    : Parsing_Flags;
      Location : GPR2.Path_Name.Object)
     with Pre => Self.Is_Defined;
   procedure Add
     (Self    : in out Object;
      Flags   : Parsing_Flags;
      Content : Value_Not_Empty)
     with Pre => Self.Is_Defined;
   --  Adds new portions of configuration chunks to the knowledge base

   function Has_Messages (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether some messages are present for this knowledge base.
   --  Messages can appear during creation of knowledge base object as well as
   --  during parsing additional of chunks by Add operation.
   --  Messages can be of 3 kinds: warning, error and information.

   function Has_Error (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether error messages are present for this knowledge base

   function Log_Messages (Self : Object) return Log.Object
     with Pre  => Self.Is_Defined,
          Post => not Self.Has_Messages
            or else not Log_Messages'Result.Is_Empty;
   --  Returns the Logs (information, warning and error messages) produced by
   --  Create and subsequent Add operations for this knowledge base.

   function Is_Default_Db (Self : Object) return Boolean;
   --  Whether the Knowledge base object is the default KB or was created empty

   function Custom_KB_Locations
     (Self : Object) return GPR2.Path_Name.Set.Object;
   --  The various paths the Knowledge Base object uses to retrieve the kb
   --  data.

   function Configuration
     (Self     : in out Object;
      Settings : Project.Configuration.Description_Set;
      Target   : Name_Type;
      Messages : in out GPR2.Log.Object;
      Fallback : Boolean := False)
      return Ada.Strings.Unbounded.Unbounded_String
     with Pre  => Self.Is_Defined,
          Post => Configuration'Result /= Null_Unbounded_String
             or else Messages.Has_Error or else Self.Has_Error;
   --  Creates configuration string

   procedure Release (Self : in out Object)
     with Pre  => Self.Is_Defined,
          Post => Self = Undefined;
   --  Releases memory associated with the knowledge base

   function Normalized_Target
     (Self   : Object;
      Target : Name_Type) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the normalized name for given target. If a given target is not
   --  not found in any of the knowledge base target sets, returns "unknown".

   function Default_Target return Name_Type;
   --  Returns name of the default target which is either specified in
   --  <gprtools directory>/share/gprconfig/default_target if it exists, or
   --  default host name.

   function Fallback_List
     (Self   : Object;
      Target : Name_Type) return GPR2.Containers.Name_List
     with Pre  => Self.Is_Defined,
          Post => not Fallback_List'Result.Is_Empty;
   --  Gets the list of fallback targets for a given target. The list will
   --  contain at least the given target itself.

   -----------------------
   -- Interactive usage --
   -----------------------

   type Compiler is private;
   --  Describes one of the compilers found on the PATH

   No_Compiler : constant Compiler;

   type Compiler_Array is array (Positive range <>) of Compiler;
   --  List of compilers

   No_Compilers : constant Compiler_Array;

   procedure Set_Selection  (Comp : in out Compiler; Selected : Boolean);
   --  Toggles the selection status of a compiler in the list

   function Is_Selected (Comp : Compiler) return Boolean;
   --  Returns the selection status of the compiler

   function Is_Selectable (Comp : Compiler) return Boolean;
   --  Returns wether compiler can be selected with the already existing
   --  selection.

   function Requires_Compiler (Comp : Compiler) return Boolean;
   --  Returns wether Compiler is a real compiler or a placeholder
   --  for a language that does not require a compiler

   function Target (Comp : Compiler) return Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns target of the compiler

   function Executable (Comp : Compiler) return Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns executable of the compiler

   function Path (Comp : Compiler) return Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns path to the compiler

   function Language (Comp : Compiler) return Language_Id;
   --  Returns language of the compiler

   function Name (Comp : Compiler) return Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns name of the compiler

   function Version (Comp : Compiler) return Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns version of the compiler

   function Runtime
     (Comp      : Compiler;
      Alternate : Boolean := False) return Optional_Name_Type
     with Pre => Requires_Compiler (Comp);
   --  Returns runtime of the compiler. When the same runtime gets found twice
   --  due to e.g. a symbolic link that matches a regexp in the knowledge base
   --  is pointing at another runtime and Alternate is set to True,
   --  returns "runtime [alt_runtime]".

   function All_Compilers
     (Self     : in out Object;
      Settings : Project.Configuration.Description_Set;
      Target   : Name_Type;
      Messages : in out GPR2.Log.Object) return Compiler_Array;
   --  Returns the list of all compilers for given target, or all compilers
   --  for any target when "all" is passed as Target.
   --  Settings affect the selection status of the compilers, but do not
   --  exclude any compilers from the resulting list.

   function Configuration
     (Self      : in out Object;
      Selection : Compiler_Array;
      Target    : Name_Type;
      Messages  : in out GPR2.Log.Object)
      return Ada.Strings.Unbounded.Unbounded_String
     with Pre  => Self.Is_Defined and then Selection'Length > 0,
          Post => Configuration'Result /= Null_Unbounded_String
                  or else Messages.Has_Error;
   --  Creates configuration string based on the selected compilers in
   --  Selection.

   function Known_Compiler_Names (Self : Object) return Unbounded_String;
   --  Return a comma-separated list of known compilers

   procedure Filter_Compilers_List
     (Self       : Object;
      Compilers  : in out Compiler_Array;
      For_Target : Name_Type);
   --  Based on the currently selected compilers, check which other compilers
   --  can or cannot be selected by the user.
   --  This is not the case if the resulting selection in Compilers is not a
   --  supported config (multiple compilers for the same language, set of
   --  compilers explicitly marked as unsupported in the knowledge base,...).

private

   use GNAT;

   type Targets_Set_Id is range -1 .. Natural'Last;

   All_Target_Sets     : constant Targets_Set_Id := -1;
   Unknown_Targets_Set : constant Targets_Set_Id := 0;

   Default_Flags : constant Parsing_Flags :=
     (Compiler_Info => True,
      Pedantic      => True,
      Validation    => False);

   Targetset_Only_Flags : constant Parsing_Flags :=
     (Compiler_Info => False,
      Pedantic      => True,
      Validation    => False);

   package Variables_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Unbounded_String);

   type Compiler is record
      Name            : Unbounded_String := Null_Unbounded_String;
      --  The name of the compiler, as specified in the <name> node of the
      --  knowledge base. If Compiler represents a filter as defined on through
      --  --config switch, then name can also be the base name of the
      --  executable we are looking for. In such a case, it never includes the
      --  exec suffix (.exe on Windows)

      Executable      : Unbounded_String := Null_Unbounded_String;
      Target          : Unbounded_String := Null_Unbounded_String;
      Targets_Set     : Targets_Set_Id;
      Path            : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;

      Base_Name       : Unbounded_String := Null_Unbounded_String;
      --  Base name of the executable. This does not include the exec suffix

      Version         : Unbounded_String := Null_Unbounded_String;
      Variables       : Variables_Maps.Map;
      Prefix          : Unbounded_String := Null_Unbounded_String;
      Runtime         : Unbounded_String := Null_Unbounded_String;
      Alt_Runtime     : Unbounded_String := Null_Unbounded_String;
      Runtime_Dir     : Unbounded_String := Null_Unbounded_String;
      Default_Runtime : Boolean := False;
      Any_Runtime     : Boolean := False;
      Path_Order      : Integer;

      Language        : Language_Id := No_Language;
      --  The supported language

      Selectable       : Boolean := True;
      Selected         : Boolean := False;
      Complete         : Boolean := True;
   end record;

   No_Compiler : constant Compiler :=
                   (Name            => Null_Unbounded_String,
                    Target          => Null_Unbounded_String,
                    Targets_Set     => Unknown_Targets_Set,
                    Executable      => Null_Unbounded_String,
                    Base_Name       => Null_Unbounded_String,
                    Path            => GPR2.Path_Name.Undefined,
                    Variables       => Variables_Maps.Empty_Map,
                    Version         => Null_Unbounded_String,
                    Prefix          => Null_Unbounded_String,
                    Runtime         => Null_Unbounded_String,
                    Alt_Runtime     => Null_Unbounded_String,
                    Default_Runtime => False,
                    Any_Runtime     => False,
                    Runtime_Dir     => Null_Unbounded_String,
                    Language        => No_Language,
                    Selectable      => False,
                    Selected        => False,
                    Complete        => True,
                    Path_Order      => 0);

   No_Compilers : constant Compiler_Array :=
                    Compiler_Array'(1 .. 0 => No_Compiler);

   function Is_Selected (Comp : Compiler) return Boolean is
     (Comp.Selected);

   function Is_Selectable (Comp : Compiler) return Boolean is
     (Comp.Selectable);

   function Target (Comp : Compiler) return Name_Type is
     (Name_Type (To_String (Comp.Target)));

   function Requires_Compiler (Comp : Compiler) return Boolean is
     (Comp.Executable /= Null_Unbounded_String);

   function Executable (Comp : Compiler) return Name_Type is
     (Name_Type (To_String (Comp.Executable)));

   function Path (Comp : Compiler) return Name_Type is
     (Name_Type (Comp.Path.Dir_Name));

   function Language (Comp : Compiler) return Language_Id is
     (Comp.Language);

   function Name (Comp : Compiler) return Name_Type is
     (Name_Type (To_String (Comp.Name)));

   function Version (Comp : Compiler) return Name_Type is
     (Name_Type (To_String (Comp.Version)));

   package Compiler_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Compiler);

   function Filter_Match
     (Self   : Object;
      Comp   : Compiler;
      Filter : Compiler) return Boolean;
   --  Returns True if Comp match Filter

   function "="
     (Dummy_Left  : Regpat.Pattern_Matcher;
      Dummy_Right : Regpat.Pattern_Matcher) return Boolean is (False);
   --  Always considers two Pattern_Matchers to be different as there is no way
   --  to actually compare them.

   package Pattern_Matcher_Holders is new Ada.Containers.Indefinite_Holders
     (Regpat.Pattern_Matcher);

   subtype Pattern_Matcher_Holder is Pattern_Matcher_Holders.Holder;

   type External_Value_Type is (Value_Constant,
                                Value_Shell,
                                Value_Directory,
                                Value_Grep,
                                Value_Nogrep,
                                Value_Filter,
                                Value_Must_Match,
                                Value_Variable,
                                Value_Done);

   type External_Value_Node (Typ : External_Value_Type := Value_Constant) is
      record
         case Typ is
            when Value_Constant  =>
               Value           : Unbounded_String;
            when Value_Shell      =>
               Command         : Unbounded_String;
            when Value_Directory  =>
               Directory       : Unbounded_String;
               Directory_Group : Integer;
               Dir_If_Match    : Unbounded_String;
               Contents        : Pattern_Matcher_Holder;
            when Value_Grep       =>
               Regexp_Re       : Pattern_Matcher_Holder;
               Group           : Natural;
            when Value_Nogrep     =>
               Regexp_No       : Pattern_Matcher_Holder;
            when Value_Filter     =>
               Filter          : Unbounded_String;
            when Value_Must_Match =>
               Must_Match      : Unbounded_String;
            when Value_Variable   =>
               Var_Name        : Unbounded_String;
            when Value_Done =>
               null;
         end case;
      end record;

   package External_Value_Nodes is
     new Ada.Containers.Doubly_Linked_Lists (External_Value_Node);

   type External_Value is record
      EV   : External_Value_Nodes.List;
      Sloc : GPR2.Source_Reference.Object;
   end record;

   Null_External_Value : constant External_Value :=
                           (EV   => External_Value_Nodes.Empty_List,
                            Sloc => Source_Reference.Undefined);
   type Compiler_Description is record
      Name             : Unbounded_String := Null_Unbounded_String;
      Executable       : Unbounded_String := Null_Unbounded_String;
      Executable_Re    : Pattern_Matcher_Holder;
      Prefix_Index     : Integer := -1;
      Target           : External_Value;
      Version          : External_Value;
      Variables        : External_Value;
      Languages        : External_Value;
      Runtimes         : External_Value;
      Default_Runtimes : GPR2.Containers.Value_List;
   end record;
   --  Executable_Re is only set if the name of the <executable> must be
   --  taken as a regular expression.

   package Compiler_Description_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Name_Type, Compiler_Description);

   type Compiler_Filter is record
      Name        : Unbounded_String;
      Name_Re     : Pattern_Matcher_Holder;
      Version     : Unbounded_String;
      Version_Re  : Pattern_Matcher_Holder;
      Runtime     : Unbounded_String;
      Runtime_Re  : Pattern_Matcher_Holder;
      Language    : Language_Id;
   end record
     with Dynamic_Predicate =>
       (Name = Null_Unbounded_String or else not Name_Re.Is_Empty)
        and then (Version = Null_Unbounded_String
                  or else not Version_Re.Is_Empty)
        and then (Runtime = Null_Unbounded_String
                  or else not Runtime_Re.Is_Empty);
   --  Representation for a <compiler> node (in <configuration>)

   package Compiler_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compiler_Filter);

   type Compilers_Filter is record
      Compiler : Compiler_Filter_Lists.List;
      Negate   : Boolean := False;
   end record;

   No_Compilers_Filter : constant Compilers_Filter :=
     (Compiler => Compiler_Filter_Lists.Empty_List,
      Negate   => False);
   --  a <compilers> filter, that matches if any of its <compiler> child
   --  matches.

   package Compilers_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compilers_Filter);

   type Double_String is record
      Positive_Regexp : Unbounded_String;
      Negative_Regexp : Unbounded_String;
   end record
     with Dynamic_Predicate => Positive_Regexp /= Null_Unbounded_String;

   package Double_String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Double_String);

   type Configuration_Type is record
      Compilers_Filters : Compilers_Filter_Lists.List;
      Targets_Filters   : Double_String_Lists.List;  --  these are regexps
      Negate_Targets    : Boolean  := False;
      Config            : Unbounded_String;
      Sloc              : Source_Reference.Object;

      Supported         : Boolean;
      --  Whether the combination of compilers is supported
   end record;

   package Configuration_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Configuration_Type);

   package Target_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Regpat.Pattern_Matcher);

   type Target_Set_Description is record
      Name     : Unbounded_String;
      Patterns : Target_Lists.List;
   end record;

   subtype Known_Targets_Set_Id
     is Targets_Set_Id range 1 .. Targets_Set_Id'Last;
   --  Known targets set.  They are in the base

   package Targets_Set_Vectors is new Ada.Containers.Vectors
     (Known_Targets_Set_Id, Target_Set_Description, "=");

   package Fallback_Targets_Set_Vectors is new Ada.Containers.Vectors
     (Known_Targets_Set_Id, GPR2.Containers.Name_List,
      GPR2.Containers.Name_Type_List."=");

   type Object is tagged record
      Compilers               : Compiler_Description_Maps.Map;
      No_Compilers            : Containers.Language_Set;
      Check_Executable_Regexp : Boolean := False;
      Configurations          : Configuration_Lists.List;
      Targets_Sets            : Targets_Set_Vectors.Vector;
      Fallback_Targets_Sets   : Fallback_Targets_Set_Vectors.Vector;
      Languages_Known         : Containers.Language_Set;

      Parsed_Directories      : GPR2.Path_Name.Set.Object;

      External_Calls_Cache    : GPR2.Containers.Name_Value_Map;

      Initialized             : Boolean := False;
      Messages                : Log.Object;

      Is_Default              : Boolean := False;
      Schema_File             : GPR2.Path_Name.Object :=
                                  GPR2.Path_Name.Undefined;
   end record;
   --  Check_Executable_Regexp is set to True if at least some of the
   --  executable names are specified as regular expressions. In such a case,
   --  a slightly slower algorithm is used to search for compilers.
   --  No_Compilers is the list of languages that require no compiler, and thus
   --  should not be searched on the PATH.
   --  Schema_File is reflevant when Is_Default id False. In that case the
   --  first .xsd file found in the given knowledge base directory is taken
   --  as a schema for the knowledge base. The file name is stored to later
   --  get access to schema again for validating additional KB chunks.

   function Name_As_Directory (Dir : String) return String;
   --  Ensures that Dir ends with a directory separator

   function Query_Targets_Set
     (Self   : Object;
      Target : Name_Type) return Targets_Set_Id
     with Pre => Self.Is_Defined;
   --  Gets the target alias set id for a target, or Unknown_Targets_Set_Id if
   --  no such target is in the base.

   Undefined : constant Object := (others => <>);

   function Custom_KB_Locations
     (Self : Object) return GPR2.Path_Name.Set.Object is
     (Self.Parsed_Directories);

   function Has_Error (Self : Object) return Boolean is
      (Self.Messages.Has_Error);

   function Has_Messages (Self : Object) return Boolean is
     (not Self.Messages.Is_Empty);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Default_Db (Self : Object) return Boolean is
      (Self.Is_Default);

   function Log_Messages (Self : Object) return Log.Object is
     (Self.Messages);

   Invalid_KB : exception;
   --  Raised when an error occurred while parsing the knowledge base

   type External_Value_Item is record
      Value          : Unbounded_String;
      Alternate      : Unbounded_String;
      Extracted_From : Unbounded_String;
   end record;
   --  Value is the actual value of the <external_value> node.
   --  Extracted_From will either be set to Value itself, or when the node is
   --  a <directory node> to the full directory, before the regexp match.
   --  When the value comes from a <shell> node, Extracted_From is set to the
   --  full output of the shell command.

   package External_Value_Lists is new Ada.Containers.Doubly_Linked_Lists
     (External_Value_Item);

   package String_To_External_Value is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => External_Value_Lists.Cursor,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=",
         "="             => External_Value_Lists."=");

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Merge_Same_Dirs  : Boolean := False;
      Calls_Cache      : in out GPR2.Containers.Name_Value_Map;
      Messages         : in out Log.Object;
      Processed_Value  : out External_Value_Lists.List;
      Ignore_Compiler  : out Boolean);
   --  Computes the value of Value, depending on its type. When an external
   --  command needs to be executed, Path is put first on the PATH environment
   --  variable. Results of external command execution are cached for effciency
   --  and are stored/looked up in Calls_Cache.
   --  Sets Ignore_Compiler if the value doesn't match its <must_have>
   --  regexp.
   --  The <filter> node is also taken into account.
   --  If Split_Into_Words is true, then the value read from <shell> or as a
   --  constant string is further assumed to be a comma-separated or space-
   --  separated string, and split.
   --  Comparisong with Matching is case-insensitive (this is needed for
   --  languages, does not matter for versions, is not used for targets)
   --
   --  If Merge_Same_Dirs is True, then the values that come from a
   --  <directory> node will be merged (the last one is kept, other removed) if
   --  they point to the same physical directory (after normalizing names).
   --
   --  This is only for use within a <compiler_description> context.

   procedure Get_Words
     (Words                : String;
      Filter               : String;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out Containers.Value_List;
      Allow_Empty_Elements : Boolean);
   --  Returns the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes
   --  If Allow_Empty_Elements is false, then empty strings are not stored in
   --  the list.

end GPR2.KB;
