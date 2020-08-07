------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  Knowledge base manipulations
--
--  This package is mostly intended to be used by GPR2 tools, users intending
--  to simply load the project and explore its contents should not call any
--  of these subprograms directly.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name.Set;

private with GNAT.Regpat;

package GPR2.Project.Configuration.KB is

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

   Default_Location_Error : exception;
   --  Raised when default location of the knowledge base cannot be found

   function Default_Location return GPR2.Path_Name.Object
     with Post => Default_Location'Result.Is_Defined;
   --  Returns the default location of the knowledge database. This is based on
   --  the location of gprconfig in path. If gprconfig is not found, raises
   --  Default_Location_Error.

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
          Post => not Self.Has_Messages or else Log_Messages'Result.Count > 0;
   --  Returns the Logs (information, warning and error messages) produced by
   --  Create and subsequent Add operations for this knowledge base.

   function Configuration
     (Self     : Object;
      Settings : Description_Set;
      Target   : Name_Type) return GPR2.Project.Configuration.Object
     with Pre  => Self.Is_Defined and then Settings'Length > 0,
          Post => Configuration'Result.Is_Defined;
   --  Creates configuration object

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

   function Fallback_List
     (Self   : Object;
      Target : Name_Type) return GPR2.Containers.Name_List
     with Pre  => Self.Is_Defined,
          Post => not Fallback_List'Result.Is_Empty;
   --  Gets the list of fallback targets for a given target. The list will
   --  contain at least the given target itself.

private

   use GNAT;

   type Targets_Set_Id is range -1 .. Natural'Last;

   All_Target_Sets     : constant Targets_Set_Id := -1;
   Unknown_Targets_Set : constant Targets_Set_Id := 0;

   Default_Flags : constant Parsing_Flags :=
     (Compiler_Info => True,
      Pedantic      => True,
      Validation    => False);

   package Variables_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Unbounded_String);

   type Compiler is record
      Name        : Unbounded_String := Null_Unbounded_String;
      --  The name of the compiler, as specified in the <name> node of the
      --  knowledge base. If Compiler represents a filter as defined on through
      --  --config switch, then name can also be the base name of the
      --  executable we are looking for. In such a case, it never includes the
      --  exec suffix (.exe on Windows)

      Executable  : Unbounded_String := Null_Unbounded_String;
      Target      : Unbounded_String := Null_Unbounded_String;
      Targets_Set : Targets_Set_Id;
      Path        : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;

      Base_Name   : Unbounded_String := Null_Unbounded_String;
      --  Base name of the executable. This does not include the exec suffix

      Version     : Unbounded_String := Null_Unbounded_String;
      Variables   : Variables_Maps.Map;
      Prefix      : Unbounded_String := Null_Unbounded_String;
      Runtime     : Unbounded_String := Null_Unbounded_String;
      Alt_Runtime : Unbounded_String := Null_Unbounded_String;
      Runtime_Dir : Unbounded_String := Null_Unbounded_String;
      Default_Runtime : Boolean := False;
      Any_Runtime     : Boolean := False;
      Path_Order  : Integer;

      Language_Case : Unbounded_String := Null_Unbounded_String;
      --  The supported language, with the casing read from the compiler. This
      --  is for display purposes only

      Language_LC : Unbounded_String := Null_Unbounded_String;
      --  The supported language, always lower case

      Selectable   : Boolean := True;
      Selected     : Boolean := False;
      Complete     : Boolean := True;
   end record
     with Dynamic_Predicate => Name = Null_Unbounded_String
     or else (GPR2.Path_Name."/=" (Path, GPR2.Path_Name.Undefined)
              and then Base_Name /= Null_Unbounded_String
              and then Prefix /= Null_Unbounded_String
              and then Executable /= Null_Unbounded_String);

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
                    Language_Case   => Null_Unbounded_String,
                    Language_LC     => Null_Unbounded_String,
                    Selectable      => False,
                    Selected        => False,
                    Complete        => True,
                    Path_Order      => 0);

   function "="
     (Dummy_Left  : Regpat.Pattern_Matcher;
      Dummy_Right : Regpat.Pattern_Matcher) return Boolean is (False);
   --  Always consideres two Pattern_Matchers different as there is no way
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

   subtype External_Value is External_Value_Nodes.List;

   Null_External_Value : constant External_Value :=
     External_Value_Nodes.Empty_List;

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
      Default_Runtimes : GPR2.Containers.Name_List;
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
      Language_LC : Unbounded_String;
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

   package Known_Languages renames Variables_Maps;

   type Object is tagged record
      Compilers               : Compiler_Description_Maps.Map;
      No_Compilers            : GPR2.Containers.Name_List;
      Check_Executable_Regexp : Boolean := False;
      Configurations          : Configuration_Lists.List;
      Targets_Sets            : Targets_Set_Vectors.Vector;
      Fallback_Targets_Sets   : Fallback_Targets_Set_Vectors.Vector;
      Languages_Known         : Known_Languages.Map;

      Parsed_Directories      : GPR2.Path_Name.Set.Object;

      External_Calls_Cache    : GPR2.Containers.Name_Value_Map;

      Initialized             : Boolean := False;
      Messages                : Log.Object;
   end record;
   --  Check_Executable_Regexp is set to True if at least some of the
   --  executable names are specified as regular expressions. In such a case,
   --  a slightly slower algorithm is used to search for compilers.
   --  No_Compilers is the list of languages that require no compiler, and thus
   --  should not be searched on the PATH.

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Has_Error (Self : Object) return Boolean is
      (Self.Messages.Has_Error);

   function Has_Messages (Self : Object) return Boolean is
     (not Self.Messages.Is_Empty);

   function Log_Messages (Self : Object) return Log.Object is
     (Self.Messages);

end GPR2.Project.Configuration.KB;
