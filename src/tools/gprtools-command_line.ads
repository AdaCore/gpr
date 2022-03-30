------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

--  This package handles the command line options.
--
--  In particular this handles:
--  switch indexes in the form of --switch:index
--  switch parameters with various delimiters:
--    -P<param> and -P param
--    -P param only
--    -jnnn no space between switch and parameter
--    --foo=param or --foo param
--  switch sections, that is sections where switches handling is delegated
--   to a separate tool so is unknown. Such handling supports also to go
--   back to own section. For example:
--   gprbuild -cargs [gcc arguments] -gargs [back to gprbuild arguments]
--  Note that switch sections can support indexes:
--    gprbuild -cargs:ada [gnat1 specific arguments]
--
--  The switch definitions are handled by groups of switches to better
--  separate various functionality (such as project loading, autoconf,
--  verbosity or tool-specific switches).
--
--  From the command line definition, the parser issues a Usage string
--  when the tool is invoked with -h or --help, and a copyright/tool version
--  string when invoked with --version.

with GPR2.Containers;

private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Less_Case_Insensitive;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

package GPRtools.Command_Line is

   Command_Line_Definition_Error : exception;
   --  Raised when there's issues with the definition of switches in the
   --  command line parser.

   type Switch_Type is new String
     with Dynamic_Predicate =>
            Switch_Type'Length > 0
              and then Switch_Type (Switch_Type'First) = '-';

   ------------------------------------
   -- COMMAND LINE RESULT DEFINITION --
   ------------------------------------

   type Command_Line_Result is tagged private;
   --  used to store the result of the command line parsing

   Empty_Result : constant Command_Line_Result;

   function Remaining_Arguments
     (Result : Command_Line_Result) return GPR2.Containers.Value_List
     with Inline;

   --------------------------------------
   -- COMMAND LINE ARGUMENT DEFINITION --
   --------------------------------------

   type Argument_Definition is private;
   --  Definition of a command line argument

   type Argument_Parameter_Delimiter is
     (None, Space, Optional_Space, Equal);
   --  Delimiter to be used between a switch and its parameter if expected.
   --
   --  None: switch and argument are aggregated (for example -gnatwa)
   --  Space: blank space between switch and argument (-P project)
   --  Optional_Space: space or argument immediately following the switch
   --  Equal: equal sign or space

   function Is_Defined (Def : Argument_Definition) return Boolean;

   function Name (Def : Argument_Definition) return Switch_Type
     with Pre => Is_Defined (Def);

   function Has_Alt_Name (Def : Argument_Definition) return Boolean
     with Pre => Is_Defined (Def);

   function Alt_Name (Def : Argument_Definition) return Switch_Type
     with Pre => Has_Alt_Name (Def);

   function Create
     (Name           : Switch_Type;
      Help           : String;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False)
      return Argument_Definition;
   --  Argument definition without parameter or alternative name.
   --
   --  Name: is the argument (for example "-A", "-switch" or "--switch")
   --  Help: is the description of the switch displayed in the Usage
   --  Index: when not empty, indicates that the switch accepts indexes.
   --   Indexes are separated from the argument via a colon (for example
   --   "-switch:ada"). The value of the Index parameter is used in the Usage
   --   string.
   --  In_Switch_Attr: whether the argument is allowed in a Package'Switch
   --   attribute definition.
   --  Hidden: when set, the attribute definition won't be displayed in the
   --   Usage string.

   function Create
     (Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Help           : String;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False)
      return Argument_Definition;
   --  Argument definition without parameter, Allows setting t2o switches
   --  for the same action, for example -h and --help.
   --
   --  Name: is the argument (for example "-A", "-switch" or "--switch")
   --  Alt_Name: an alternative name for the switch (for example "-s",
   --   "--switch"). Note : argument callback is always called using Name as
   --   argument.
   --  Help: is the description of the switch displayed in the Usage
   --  Index: when not empty, indicates that the switch accepts indexes.
   --   Indexes are separated from the argument via a colon (for example
   --   "-switch:ada"). The value of the Index parameter is used in the Usage
   --   string.
   --  In_Switch_Attr: whether the argument is allowed in a Package'Switch
   --   attribute definition.
   --  Hidden: when set, the attribute definition won't be displayed in the
   --   Usage string.

   function Create
     (Name           : Switch_Type;
      Help           : String;
      Delimiter      : Argument_Parameter_Delimiter;
      Parameter      : String  := "ARG";
      Default        : String  := "";
      Required       : Boolean := False;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False)
      return Argument_Definition;
   --  Argument definition with parameter

   function Create
     (Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Help           : String;
      Delimiter      : Argument_Parameter_Delimiter;
      Parameter      : String  := "ARG";
      Default        : String  := "";
      Required       : Boolean := False;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False)
      return Argument_Definition;
   --  Argument definition with parameter and alternate name

   -------------------------------
   -- ARGUMENT GROUP DEFINITION --
   -------------------------------

   type Argument_Group is private;
   --  An argument group displays together conceptually related arguments
   --  in the Usage display.
   --  Mutually_Exclusive argument groups on the other hand identifies
   --  arguments or groups of arguments that are mutually exclusive.

   No_Group : constant Argument_Group;

   -------------------------
   -- COMMAND LINE PARSER --
   -------------------------

   type Command_Line_Parser is tagged private;

   type Argument_Action is access procedure
     (Parser : Command_Line_Parser'Class;
      Result : not null access Command_Line_Result'Class;
      Arg    : Switch_Type;
      Index  : String;
      Param  : String);
   --  Callback when parsing a new known argument.
   --
   --  Parser: the parser being used to parse the command line
   --  Result: the structure holding the result
   --  Arg: the primary name of the switch
   --  Index: the switch index, if any, or the empty string
   --  Param: the switch parameter, if any, or the empty string.

   type Section_Action is access procedure
     (Parser  : Command_Line_Parser'Class;
      Result  : not null access Command_Line_Result'Class;
      Section : String;
      Index   : String;
      Arg     : Switch_Type);
   --  Callback when an argument for an external section is founc.
   --
   --  Parser: the parser being used to parse the command line
   --  Result: the structure holding the result
   --  Section: the switch used to delimit a new section
   --  Index: if defined for the switch, or the empty string
   --  Arg: the argument to handle

   function Is_Defined (Self : Command_Line_Parser) return Boolean;

   function Create
     (Initial_Year : String;
      Cmd_Line     : String := "";
      Tool_Name    : String := "";
      Help         : String := "") return Command_Line_Parser'Class
     with Post => Create'Result.Is_Defined;
   --  Initialize internal structures and sets values for version and help
   --  arguments

   function Main_Group
     (Self : in out Command_Line_Parser) return Argument_Group
     with Pre => Self.Is_Defined;

   function Has_Group
     (Self : Command_Line_Parser;
      Name : GPR2.Name_Type) return Boolean
     with Pre => Self.Is_Defined;

   function Group
     (Self : Command_Line_Parser;
      Name : GPR2.Name_Type) return Argument_Group
     with
       Pre  => Self.Is_Defined,
       Post => (if Self.Has_Group (Name)
                then Group'Result /= No_Group
                else Group'Result = No_Group);

   procedure Version (Self : Command_Line_Parser)
     with Pre => Self.Is_Defined;
   --  Displays the version string. This is automatically called when --version
   --  is found in the command line.

   procedure Usage (Self : Command_Line_Parser)
     with Pre => Self.Is_Defined;
   --  Displays the usage string. This is automatically called when -h or
   --  --help is found in the command line.

   procedure Try_Help;
   --  Displays 'try "<tool> --help" for more information'. Typically called
   --  when catching a Usage_Error exception.

   procedure Get_Opt
     (Self   : Command_Line_Parser;
      Result : in out Command_Line_Result'Class)
     with Pre => Self.Is_Defined;
   --  Parse the command line from Ada.Command_Line

   procedure Get_Opt
     (Self      : Command_Line_Parser;
      From_Pack : GPR2.Package_Id;
      Values    : GPR2.Containers.Source_Value_List;
      Result    : in out Command_Line_Result'Class);
   --  Parse the command line from an attribute value (typically the Switches
   --  attribute).

   function Has_Argument
     (Self : Command_Line_Parser;
      Name : Switch_Type) return Boolean
     with Pre => Self.Is_Defined;

   procedure Add_Argument
     (Self  : in out Command_Line_Parser;
      Group : Argument_Group;
      Def   : Argument_Definition)
     with Pre => not Self.Has_Argument (Name (Def))
                   and then (not Has_Alt_Name (Def)
                             or else not Self.Has_Argument (Alt_Name (Def)));
   --  Add an argument definition to the new argument group

   function Add_Argument_Group
     (Self     : in out Command_Line_Parser;
      Name     : GPR2.Name_Type;
      Callback : Argument_Action;
      Help     : String := "";
      Last     : Boolean := False) return Argument_Group
     with Pre => not Self.Has_Group (Name);
   --  Add a new Argument group.
   --
   --  Name: the name of the group. Will be displayed in the Usage string as
   --   "  <group> switches:"
   --   followed by the group's switches definition unless the name is prefixed
   --   with an underscore.
   --  Callback: the subprogram to call whenever a switch of the group is
   --   found in the command line.
   --  Help: if not empty, is displayed before the list of the group's switches
   --   in the usage string.
   --  Last: two series of groups are defined, regular ones and last ones. If
   --   set, the group is appended to the last ones else it is appended to
   --   regular ones. The regular groups are displayed before the last groups
   --   in the usage string.

   procedure Add_Section_Argument
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True)
     with Pre => not Self.Has_Argument (Name)
                   and then not Self.Has_Argument (Alt_Name);
   --  Add a new section argument. Such argument instruct the parser that
   --  the switches after that are meant for a different tool, so should
   --  not be handled by the parser but be preserved as-is without
   --  analysis.
   --  If Index is not empty, then the section accepts an index parameter
   --  in the form -switch:index.
   --  If Callback is null, this instructs the parser that the new section
   --  is back to default, so that following switches need to be parsed
   --  normally. Only one such section can be defined.

   procedure Add_Section_Argument
     (Self           : in out Command_Line_Parser;
      Name           : Switch_Type;
      Callback       : Section_Action;
      Help           : String := "";
      Index          : String := "";
      In_Switch_Attr : Boolean := True)
     with Pre => not Self.Has_Argument (Name);

private

   use Ada;
   use Ada.Strings.Unbounded;

   function To_Unbounded_String (S : Switch_Type) return Unbounded_String
     is (To_Unbounded_String (String (S)));

   type Argument_Group is new Unbounded_String;

   No_Group : constant Argument_Group :=
                Argument_Group (Null_Unbounded_String);

   type Argument_Definition (With_Value : Boolean := False) is record
      Name     : Unbounded_String;
      Alt_Name : Unbounded_String;
      Group    : Argument_Group;
      Help     : Unbounded_String;
      Index    : Unbounded_String;
      In_Attr  : Boolean := True;
      Hidden   : Boolean := False;

      case With_Value is
         when False =>
            Is_Section       : Boolean := False;
            Section_Callback : Section_Action;
         when True =>
            Parameter : Unbounded_String;
            Delimiter : Argument_Parameter_Delimiter;
            Default   : Unbounded_String;
            Required  : Boolean := False;
      end case;
   end record;

   function Is_Defined (Def : Argument_Definition) return Boolean is
     (Def /= Argument_Definition'(others => <>));

   function Name (Def : Argument_Definition) return Switch_Type is
     (Switch_Type (To_String (Def.Name)));

   function Has_Alt_Name (Def : Argument_Definition) return Boolean is
     (Length (Def.Alt_Name) > 0);

   function Alt_Name (Def : Argument_Definition) return Switch_Type is
     (Switch_Type (To_String (Def.Alt_Name)));

   ------------
   -- Create --
   ------------

   function Create
     (Name           : Switch_Type;
      Help           : String;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False) return Argument_Definition
   is (Argument_Definition'(With_Value       => False,
                            Name             => To_Unbounded_String (Name),
                            Alt_Name         => Null_Unbounded_String,
                            Group            => No_Group,
                            Help             => To_Unbounded_String (Help),
                            Index            => To_Unbounded_String (Index),
                            In_Attr          => In_Switch_Attr,
                            Hidden           => Hidden,
                            Is_Section       => False,
                            Section_Callback => null));

   function Create
     (Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Help           : String;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False) return Argument_Definition
   is (Argument_Definition'(With_Value => False,
                            Name             => To_Unbounded_String (Name),
                            Alt_Name         => To_Unbounded_String (Alt_Name),
                            Group            => No_Group,
                            Help             => To_Unbounded_String (Help),
                            Index            => To_Unbounded_String (Index),
                            In_Attr          => In_Switch_Attr,
                            Hidden           => Hidden,
                            Is_Section       => False,
                            Section_Callback => null));

   function Create
     (Name           : Switch_Type;
      Help           : String;
      Delimiter      : Argument_Parameter_Delimiter;
      Parameter      : String  := "ARG";
      Default        : String  := "";
      Required       : Boolean := False;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False) return Argument_Definition
   is (Argument_Definition'(With_Value => True,
                            Name       => To_Unbounded_String (Name),
                            Alt_Name   => Null_Unbounded_String,
                            Group      => No_Group,
                            Help       => To_Unbounded_String (Help),
                            Index      => To_Unbounded_String (Index),
                            In_Attr    => In_Switch_Attr,
                            Hidden     => Hidden,
                            Parameter  => To_Unbounded_String (Parameter),
                            Delimiter  => Delimiter,
                            Default    => To_Unbounded_String (Default),
                            Required   => Required));

   function Create
     (Name           : Switch_Type;
      Alt_Name       : Switch_Type;
      Help           : String;
      Delimiter      : Argument_Parameter_Delimiter;
      Parameter      : String  := "ARG";
      Default        : String  := "";
      Required       : Boolean := False;
      Index          : String := "";
      In_Switch_Attr : Boolean := True;
      Hidden         : Boolean := False) return Argument_Definition
   is (Argument_Definition'(With_Value => True,
                            Name       => To_Unbounded_String (Name),
                            Alt_Name   => To_Unbounded_String (Alt_Name),
                            Group      => No_Group,
                            Help       => To_Unbounded_String (Help),
                            Index      => To_Unbounded_String (Index),
                            In_Attr    => In_Switch_Attr,
                            Hidden     => Hidden,
                            Parameter  => To_Unbounded_String (Parameter),
                            Delimiter  => Delimiter,
                            Default    => To_Unbounded_String (Default),
                            Required   => Required));

   type Command_Line_Result is tagged record
      Remaining : GPR2.Containers.Value_List;
   end record;

   Empty_Result : constant Command_Line_Result := (others => <>);

   function Remaining_Arguments
     (Result : Command_Line_Result) return GPR2.Containers.Value_List
   is (Result.Remaining);

   function Dash_Dash
     (S : Switch_Type) return Boolean
   is (if S'Length > 2 then S (S'First .. S'First + 1) = "--" else False);

   function Arg_Less (S1, S2 : Switch_Type) return Boolean is
     (if Dash_Dash (S1) /= Dash_Dash (S2)
      then not Dash_Dash (S1)
      elsif Strings.Equal_Case_Insensitive (String (S1), String (S2))
      then S1 < S2
      else Strings.Less_Case_Insensitive (String (S1), String (S2)));
   --  We use case insensitive sort for displaying the switches in the
   --  usage string, but switch comparison is always case sensitive.

   package Switches_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Switch_Type, "<" => Arg_Less);
   package Switches_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Switch_Type, Switch_Type);

   type Argument_Group_Internal is record
      Help           : Unbounded_String;
      Switches       : Switches_Sets.Set;
      Callback       : Argument_Action;
      Subgroups      : GPR2.Containers.Name_List;
      Last_Subgroups : GPR2.Containers.Name_List;
      Exclusive      : Boolean;
      Required       : Boolean;
   end record;

   package Group_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Name_Type, Argument_Group_Internal, "<" => GPR2."<");

   package Arg_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Switch_Type, Argument_Definition, Arg_Less);

   type Command_Line_Parser is tagged record
      Groups          : Group_Maps.Map;
      Cmd_Line_Help   : Unbounded_String;
      Tool            : Unbounded_String;
      Initial_Year    : Unbounded_String;
      Help            : Unbounded_String;
      Default_Section : Unbounded_String;
      Switches        : Arg_Maps.Map;
      Aliases         : Switches_Maps.Map;
   end record;

   function Add_Argument_Group
     (Self     : in out Command_Line_Parser;
      Group    : Argument_Group;
      Name     : GPR2.Name_Type;
      Callback : Argument_Action;
      Help     : String := "";
      Last     : Boolean := False) return Argument_Group;
   --  Add a subgroup to an existing group

   function Add_Mutually_Exclusive_Argument_Group
     (Self     : in out Command_Line_Parser;
      Group    : Argument_Group;
      Name     : GPR2.Name_Type;
      Help     : String := "";
      Required : Boolean := False) return Argument_Group;

   function Is_Defined (Self : Command_Line_Parser) return Boolean is
     (Self /= Command_Line_Parser'(others => <>));

   function Has_Group
     (Self : Command_Line_Parser;
      Name : GPR2.Name_Type) return Boolean
   is (Self.Groups.Contains (Name));

   function Group
     (Self : Command_Line_Parser;
      Name : GPR2.Name_Type) return Argument_Group
   is (if Self.Groups.Contains (Name)
       then To_Unbounded_String (String (Name))
       else No_Group);

   function Main_Group
     (Self : in out Command_Line_Parser) return Argument_Group
   is (To_Unbounded_String ("_root"));

   function Has_Argument
     (Self : Command_Line_Parser;
      Name : Switch_Type) return Boolean
   is (Self.Switches.Contains (Name));

end GPRtools.Command_Line;
