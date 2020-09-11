------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with Input_Sources.File;
with Input_Sources.Strings;
with Schema.Dom_Readers;
with Sax.Readers;
with Unicode.CES.Utf8;

with GNATCOLL.Traces;

with GPR2.Containers;
with GPR2.Message;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

package body GPR2.Project.Configuration.KB.Parsing is

   function Default_Content return GPR2.Containers.Name_Value_Map
     with Post => not Default_Content'Result.Is_Empty;
   --  Returns default contents of the knowledge base embedded
   --  into the library. Result maps name of the file used for embedding
   --  to its contents.
   --  Result maps base names of files used to create the embedded knowledge
   --  base to their contents.

   function Get_Attribute
     (N         : DOM.Core.Node;
      Attribute : Value_Not_Empty;
      Default   : Value_Type) return Value_Type
     with Pre  => DOM.Core."/=" (N, null),
          Post => Get_Attribute'Result'Length > 0
                  or else Get_Attribute'Result = Default;
   --  Returns the value of an attribute, or Default if the attribute does not
   --  exist.

   function Node_Value_As_String (N : DOM.Core.Node) return String
     with Pre  => DOM.Core."/=" (N, null);
   --  Returns the value of the node, concatenating all Text children

   procedure Parse_Knowledge_Base
     (Base      : in out Object;
      Root_Node : DOM.Core.Node;
      Flags     : Parsing_Flags;
      From_File : Value_Not_Empty)
     with Pre => Base.Is_Defined and then DOM.Core."/=" (Root_Node, null);
   --  Parses a single top-level KB node. From_File used for diagnostics

   procedure Get_Words
     (Words                : String;
      Filter               : String;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out Containers.Name_List;
      Allow_Empty_Elements : Boolean);
   --  Returns the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes
   --  If Allow_Empty_Elements is false, then empty strings are not stored in
   --  the list.

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

   function Name_As_Directory (Dir : String) return String;
   --  Ensures that Dir ends with a directory separator

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Merge_Same_Dirs  : Boolean := False;
      Calls_Cache      : in out GPR2.Containers.Name_Value_Map;
      Error_Sloc       : Source_Reference.Object;
      Messages         : in out Log.Object;
      Processed_Value  : out External_Value_Lists.List);
   --  Computes the value of Value, depending on its type. When an external
   --  command needs to be executed, Path is put first on the PATH environment
   --  variable. Results of external command execution are cached for effciency
   --  and are stored/looked up in Calls_Cache.
   --  Raises Ignore_Compiler if the value doesn't match its <must_have>
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

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Visited         : in out String_To_External_Value.Map;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Regpat.Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : String;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0;
      Contents        : Pattern_Matcher_Holder;
      Merge_Same_Dirs : Boolean;
      Error_Sloc      : Source_Reference.Object;
      Messages        : in out Log.Object);
   --  Parses all subdirectories of Current_Dir for those that match
   --  Path_To_Check (see description of <directory>). When a match is found,
   --  the regexp is evaluated against the current directory, and the matching
   --  parenthesis group is appended to Append_To (comma-separated).
   --  If Group is -1, then Value_If_Match is used instead of the parenthesis
   --  group.
   --  Group_Match is the substring that matched Group (if it has been matched
   --  already). Group_Count is the number of parenthesis groups that have been
   --  processed so far. The idea is to compute the matching substring as we
   --  go, since the regexp might no longer match in the end, if for instance
   --  it includes ".." directories.
   --
   --  If Merge_Same_Dirs is True, then the values that come from a
   --  <directory> node will be merged (the last one is kept, other removed) if
   --  they point to the same physical directory (after normalizing names). In
   --  this case, Visited contains the list of normalized directory names.
   --
   --  Contents, if specified, is a regular expression. It indicates that any
   --  file matching the pattern should be parsed, and the first line matching
   --  that regexp should be used as the name of the file instead. This is a
   --  way to simulate symbolic links on platforms that do not use them.

   generic
      with function Callback (Var_Name, Index : String) return String;
   function Substitute_Variables
     (Str        : String;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String;
   --  Substitutes variables in Str (their value is computed through Callback).
   --  Possible errors are stored in Messages.

   function Substitute_Variables_In_Compiler_Description
     (Str        : String;
      Comp       : Compiler;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String;
   --  Substitutes the special "$..." names.
   --  Depending on the XML nodes we are in (specified by the context) the list
   --  of variables might be different.

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String;
   --  Returns the value of a predefined or user-defined variable.
   --  If the variable is not defined a warning is emitted and an empty
   --  string is returned.

   function Get_String_No_Adalib (Str : String) return String;
   --  Returns the name without "adalib" at the end

   Embed_Pseudo_Dir : constant String :=
                        "embedded_kb" & Directory_Operations.Dir_Separator;
   --  Used for reporting potential errors in the embeded base

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
                  GNATCOLL.Traces.Create
                    ("KNOWLEDGE_BASE.PARSING_TRACE",
                     GNATCOLL.Traces.From_Config);

   Invalid_KB : exception;
   --  Raised when an error occurred while parsing the knowledge base

   Ignore_Compiler : exception;
   --  Raised when the compiler should be ignored

   ---------
   -- Add --
   ---------

   procedure Add
     (Self    : in out Object;
      Flags   : Parsing_Flags;
      Content : Value_Not_Empty)
   is
      use DOM.Core, DOM.Core.Nodes;
      use Input_Sources.Strings;
      use Schema.Dom_Readers;
      use Sax.Readers;

      use GNATCOLL.Traces;

      String_Argument : constant String := "string_argument";
      Reader          : Schema.Dom_Readers.Tree_Reader;
      Input           : String_Input;

   begin
      Trace (Main_Trace, "Parsing string");
      Reader.Set_Feature (Schema_Validation_Feature, Flags (Validation));
      Reader.Set_Feature (Validation_Feature, False);  --  Do not use DTD

      Open (Unicode.CES.Byte_Sequence (String (Content)),
            Unicode.CES.Utf8.Utf8_Encoding,
            Input);
      Parse (Reader, Input);
      Close (Input);

      Parse_Knowledge_Base
        (Self,
         DOM.Core.Documents.Get_Element (Get_Tree (Reader)),
         Flags,
         Embed_Pseudo_Dir & String_Argument);

      declare
         Doc : Document := Get_Tree (Reader);
      begin
         Free (Doc);
      end;

      Free (Reader);
   end Add;

   ---------------------
   -- Default_Content --
   ---------------------

   function Default_Content return GPR2.Containers.Name_Value_Map is
      use Ada.Strings.Fixed;
      use GPR2.Containers;

      KB_Start    : constant Character
                      with Import     => True,
                           Convention => C,
                           Link_Name  => "_binary_config_kb_start";
      KB_Length   : constant Integer
                      with Import     => True,
                           Convention => C,
                           Link_Name  => "_binary_config_kb_size";

      Result      : Name_Value_Map;
      KB          : String (1 .. KB_Length) with Address => KB_Start'Address;
      Idx1        : Integer;
      Idx2        : Integer;
      Idx3        : Integer;
      File_Length : Positive;

   begin
      --  Embedded knowledge base is expected to be a string representing all
      --  individual files forming the knowledge base in the following format:
      --
      --  <file_name>:<length>:<content>{<file_name>:<length>:<content>}
      --
      --  where <length> indicates the length of the <content> field and
      --  <file_name> is the base name of the file included in the base.

      Idx1 := KB'First;

      while Idx1 in KB'First .. KB'Last - 2 loop
         Idx2 := Index (KB, ":", Idx1);

         if Idx2 <= Idx1 or else Idx2 = KB'Last then
            raise Invalid_KB with "malformed default knowledge base at"
              & Idx1'Img;
         end if;

         Idx3 := Index (KB, ":", Idx2 + 1);

         if Idx3 <= Idx2 then
            raise Invalid_KB with "malformed default knowledge base at"
              & Idx2'Img;
         end if;

         File_Length := Positive'Value (KB (Idx2 + 1 .. Idx3 - 1));

         if Idx3 + File_Length > KB'Last then
            raise Invalid_KB with "malformed default knowledge base at"
              & Idx3'Img;
         end if;

         Result.Include
           (Name_Type (KB (Idx1 .. Idx2 - 1)),
            Value_Type (KB (Idx3 + 1 .. Idx3  + File_Length)));

         Idx1 := Idx3 + File_Length + 1;
      end loop;

      return Result;
   end Default_Content;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N         : DOM.Core.Node;
      Attribute : Value_Not_Empty;
      Default   : Value_Type) return Value_Type
   is
      use DOM.Core;
      use DOM.Core.Nodes;

      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return Default;
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   ------------------------
   -- Get_External_Value --
   ------------------------

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Merge_Same_Dirs  : Boolean := False;
      Calls_Cache      : in out GPR2.Containers.Name_Value_Map;
      Error_Sloc       : Source_Reference.Object;
      Messages         : in out Log.Object;
      Processed_Value  : out External_Value_Lists.List)
   is
      use External_Value_Nodes;
      use GNAT.Expect;
      use GNAT.Regpat;

      use GNATCOLL.Traces;

      function Get_Command_Output_Cache
        (Path    : String;
         Command : String) return Unbounded_String;
      --  Spawns given command and caches results. When the same command
      --  (same full path and arguments) should be spawned again,
      --  returns output from cache instead.

      ------------------------------
      -- Get_Command_Output_Cache --
      ------------------------------

      function Get_Command_Output_Cache
        (Path    : String;
         Command : String) return Unbounded_String
      is
         use GNAT.OS_Lib;
         use GPR2.Containers.Name_Value_Map_Package;

         Key : constant Name_Type := Name_Type (Path & Command);
         Cur : constant GPR2.Containers.Name_Value_Map_Package.Cursor :=
                 Calls_Cache.Find (Key);

         Tmp_Result : Unbounded_String;
         Status     : aliased Integer;
      begin
         if Cur = GPR2.Containers.Name_Value_Map_Package.No_Element then
            declare
               Args   : Argument_List_Access :=
                          Argument_String_To_List (Command);
               Output : constant String := Get_Command_Output
                          (Command    => Args (Args'First).all,
                           Arguments  => Args (Args'First + 1 .. Args'Last),
                           Input      => "",
                           Status     => Status'Unchecked_Access,
                           Err_To_Out => True);
            begin
               OS_Lib.Free (Args);
               Tmp_Result := To_Unbounded_String (Output);
               Calls_Cache.Include (Key, To_String (Tmp_Result));
               return Tmp_Result;
            end;

         else
            return To_Unbounded_String (Calls_Cache.Element (Key));
         end if;
      end Get_Command_Output_Cache;

      Saved_Path     : constant String :=
                         Environment_Variables.Value ("PATH");
      Extracted_From : Unbounded_String := Null_Unbounded_String;
      Tmp_Result     : Unbounded_String;
      Node_Cursor    : External_Value_Nodes.Cursor := Value.First;
      Node           : External_Value_Node;
      From_Static    : Boolean := False;

      Visited        : String_To_External_Value.Map;
   begin
      Processed_Value.Clear;

      while Has_Element (Node_Cursor) loop
         while Has_Element (Node_Cursor) loop
            Node := External_Value_Nodes.Element (Node_Cursor);

            case Node.Typ is
               when Value_Variable =>
                  Extracted_From := Node.Var_Name;

               when Value_Constant =>
                  if Node.Value = Null_Unbounded_String then
                     Tmp_Result := Null_Unbounded_String;
                  else
                     Tmp_Result := To_Unbounded_String
                       (Substitute_Variables_In_Compiler_Description
                          (To_String (Node.Value),
                           Comp,
                           Error_Sloc,
                           Messages));
                  end if;

                  From_Static := True;
                  Trace
                    (Main_Trace,
                     Attribute & ": constant := " & To_String (Tmp_Result));

               when Value_Shell =>
                  Ada.Environment_Variables.Set
                    ("PATH",
                     Comp.Path.Value
                     & OS_Lib.Path_Separator & Saved_Path);

                  declare
                     Command : constant String :=
                                 Substitute_Variables_In_Compiler_Description
                                   (To_String (Node.Command),
                                    Comp,
                                    Error_Sloc,
                                    Messages);
                  begin
                     Tmp_Result := Null_Unbounded_String;
                     Tmp_Result :=
                       Get_Command_Output_Cache (Comp.Path.Value, Command);
                     Ada.Environment_Variables.Set ("PATH", Saved_Path);

                     Trace (Main_Trace,
                            Attribute & ": executing """ & Command
                            & """ output="""
                            & To_String (Tmp_Result) & """");
                  exception
                     when Invalid_Process =>
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "Spawn failed for " & Command,
                              Sloc => Error_Sloc));
                  end;

               when Value_Directory =>
                  declare
                     Search : constant String :=
                                Substitute_Variables_In_Compiler_Description
                                  (To_String (Node.Directory),
                                   Comp,
                                   Error_Sloc,
                                   Messages);
                  begin
                     if Search (Search'First) = '/' then
                        Increase_Indent
                          (Main_Trace,
                           Attribute & ": search directories matching "
                           & Search & ", starting from /");

                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Visited         => Visited,
                           Current_Dir     => "",
                           Path_To_Check   => Search,
                           Contents        => Node.Contents,
                           Regexp          =>
                             Compile (Search (Search'First + 1
                                              .. Search'Last)),
                           Regexp_Str      => Search,
                           Value_If_Match  => To_String (Node.Dir_If_Match),
                           Merge_Same_Dirs => Merge_Same_Dirs,
                           Group           => Node.Directory_Group,
                           Error_Sloc      => Error_Sloc,
                           Messages        => Messages);
                     else
                        Increase_Indent
                          (Main_Trace,
                           Attribute & ": search directories matching "
                           & Search & ", starting from "
                           & Comp.Path.Value);

                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Visited         => Visited,
                           Current_Dir     => Comp.Path.Value,
                           Path_To_Check   => Search,
                           Contents        => Node.Contents,
                           Regexp          => Compile (Search),
                           Regexp_Str      => Search,
                           Value_If_Match  => To_String (Node.Dir_If_Match),
                           Merge_Same_Dirs => Merge_Same_Dirs,
                           Group           => Node.Directory_Group,
                           Error_Sloc      => Error_Sloc,
                           Messages        => Messages);
                     end if;

                     Decrease_Indent
                       (Main_Trace, "Done search directories");
                  end;

               when Value_Grep =>
                  declare
                     Tmp_Str : constant String := To_String (Tmp_Result);
                     Matched : Match_Array (0 .. Node.Group);
                  begin
                     Match (Node.Regexp_Re.Element, Tmp_Str, Matched);

                     if Matched (0) /= No_Match then
                        Tmp_Result := To_Unbounded_String
                          (Tmp_Str (Matched (Node.Group).First ..
                                    Matched (Node.Group).Last));
                        Trace
                          (Main_Trace,
                           Attribute & ": grep matched="""
                           & To_String (Tmp_Result) & """");
                     else
                        Tmp_Result := Null_Unbounded_String;
                        Trace (Main_Trace, Attribute & ": grep no match");
                     end if;
                  end;

               when Value_Nogrep =>
                  declare
                     Tmp_Str : constant String := To_String (Tmp_Result);
                     Matched : Match_Array (0 .. 0);
                  begin
                     Match (Node.Regexp_No.Element, Tmp_Str, Matched);

                     if Matched (0) /= No_Match then
                        Trace
                          (Main_Trace,
                           Attribute & ": nogrep matched=""" & Tmp_Str & """");
                        raise Ignore_Compiler;

                     else
                        Trace (Main_Trace, Attribute & ": nogrep no match");
                     end if;
                  end;

               when Value_Must_Match =>
                  if not Match
                    (Expression => To_String (Node.Must_Match),
                     Data       => To_String (Tmp_Result))
                  then
                     Trace
                       (Main_Trace,
                        "Ignore compiler since external value """
                        & To_String (Tmp_Result) & """ must match "
                        & To_String (Node.Must_Match));

                     Tmp_Result := Null_Unbounded_String;
                     raise Ignore_Compiler;
                  end if;

                  exit;

               when Value_Done | Value_Filter =>
                  exit;
            end case;

            Next (Node_Cursor);
         end loop;

         case Node.Typ is
            when Value_Done | Value_Filter | Value_Must_Match =>
               if Tmp_Result = Null_Unbounded_String then
                  --  Value could not be computed
                  if Extracted_From /= Null_Unbounded_String then
                     Processed_Value.Append
                        (External_Value_Item'
                          (Value          => Null_Unbounded_String,
                           Alternate      => Null_Unbounded_String,
                           Extracted_From => Extracted_From));
                  end if;

               elsif Split_Into_Words then
                  declare
                     Filter : constant String :=
                                (if Node.Typ = Value_Filter
                                 then To_String (Node.Filter)
                                 else "");
                     Split  : Containers.Name_List;
                  begin
                     --  When an external value is defined as a static string,
                     --  the only valid separator is ','. When computed
                     --  however, we also allow space as a separator.

                     if From_Static then
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ',',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);

                     else
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ' ',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);
                     end if;

                     for Elem of Split loop
                        Processed_Value.Append
                           (External_Value_Item'
                              (Value          =>
                                   To_Unbounded_String (String (Elem)),
                              Alternate      => Null_Unbounded_String,
                              Extracted_From => Extracted_From));
                     end loop;
                  end;

               else
                  Processed_Value.Append
                    (External_Value_Item'
                       (Value          => Tmp_Result,
                        Alternate      => Null_Unbounded_String,
                        Extracted_From => Extracted_From));
               end if;

            when others =>
               null;
         end case;

         Extracted_From := Null_Unbounded_String;

         Next (Node_Cursor);
      end loop;
   end Get_External_Value;

   --------------------------
   -- Get_String_No_Adalib --
   --------------------------

   function Get_String_No_Adalib (Str : String) return String is
      use GNAT.OS_Lib;

      Name : constant String (1 .. Str'Length) := Str;
      Last : Natural := Name'Last;
   begin
      if Last > 7
        and then (Name (Last) in Directory_Separator | '/')
      then
         Last := Last - 1;
      end if;

      if Last > 6
        and then Name (Last - 5 .. Last) = "adalib"
        and then (Name (Last - 6) in Directory_Separator | '/')
      then
         Last := Last - 6;
      else
         Last := Name'Last;
      end if;

      return Name (1 .. Last);
   end Get_String_No_Adalib;

   ------------------------
   -- Get_Variable_Value --
   ------------------------

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String
   is
      N : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Variables_Maps.Contains (Comp.Variables, N) then
         return To_String (Variables_Maps.Element (Comp.Variables, N));
      elsif Name = "HOST" then
         return System.OS_Constants.Target_Name;
      elsif Name = "TARGET" then
         return To_String (Comp.Target);
      elsif Name = "RUNTIME_DIR" then
         return Name_As_Directory (To_String (Comp.Runtime_Dir));
      elsif Name = "EXEC" then
         return To_String (Comp.Executable);
      elsif Name = "VERSION" then
         return To_String (Comp.Version);
      elsif Name = "LANGUAGE" then
         return To_String (Comp.Language_LC);
      elsif Name = "RUNTIME" then
         return To_String (Comp.Runtime);
      elsif Name = "PREFIX" then
         return To_String (Comp.Prefix);
      elsif Name = "PATH" then
         return Comp.Path.Value;
      elsif Name = "GPRCONFIG_PREFIX" then
         return Get_Tools_Directory;
      end if;

      raise Invalid_KB
        with "variable '" & Name & "' is not defined";
   end Get_Variable_Value;

   ---------------
   -- Get_Words --
   ---------------

   procedure Get_Words
     (Words                : String;
      Filter               : String;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out Containers.Name_List;
      Allow_Empty_Elements : Boolean)
   is
      First      : Integer := Words'First;
      Last       : Integer;
      Filter_Set : GPR2.Containers.Name_List;
   begin
      if Filter /= "" then
         Get_Words (Filter, "", Separator1,
                    Separator2, Filter_Set,
                    Allow_Empty_Elements => True);
      end if;

      if not Allow_Empty_Elements then
         while First <= Words'Last
           and then (Words (First) = Separator1
                     or else Words (First) = Separator2)
         loop
            First := First + 1;
         end loop;
      end if;

      while First <= Words'Last loop
         if Words (First) /= Separator1
           and then Words (First) /= Separator2
         then
            Last := First + 1;
            while Last <= Words'Last
              and then Words (Last) /= Separator1
              and then Words (Last) /= Separator2
            loop
               Last := Last + 1;
            end loop;
         else
            Last := First;
         end if;

         if (Allow_Empty_Elements or else First <= Last - 1)
           and then
             (Filter_Set.Is_Empty
              or else Filter_Set.Contains
                (Optional_Name_Type (Words (First .. Last - 1))))
         then
            Map.Append (Optional_Name_Type (Words (First .. Last - 1)));
         end if;

         First := Last + 1;
      end loop;
   end Get_Words;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory (Dir : String) return String is
      use GNAT.OS_Lib;
   begin
      if Dir = ""
        or else Dir (Dir'Last) in Directory_Separator | '/'
      then
         return Dir;
      else
         return Dir & Directory_Separator;
      end if;
   end Name_As_Directory;

   --------------------------
   -- Node_Value_As_String --
   --------------------------

   function Node_Value_As_String (N : DOM.Core.Node) return String is
      use DOM.Core;
      use DOM.Core.Nodes;

      Result : Unbounded_String;
      Child  : Node := First_Child (N);
   begin
      while Child /= null loop
         exit when Node_Type (Child) = Element_Node;
         Append (Result, Node_Value (Child));
         Child := Next_Sibling (Child);
      end loop;

      return To_String (Result);
   end Node_Value_As_String;

   --------------------
   -- Parse_All_Dirs --
   --------------------

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Visited         : in out String_To_External_Value.Map;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Regpat.Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : String;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0;
      Contents        : Pattern_Matcher_Holder;
      Merge_Same_Dirs : Boolean;
      Error_Sloc      : Source_Reference.Object;
      Messages        : in out Log.Object)
   is
      use GNAT.Regpat;
      use GNAT.OS_Lib;
      use GNATCOLL.Traces;

      procedure Save_File (Current_Dir : String; Val : String);
      --  Mark the given directory as valid for the <directory> configuration.
      --  This takes care of removing duplicates if needed.

      function Is_Regexp (Str : String) return Boolean;
      --  Whether Str is a regular expression

      function Unquote_Regexp
        (Str : String; Remove_Quoted : Boolean := False) return String;
      --  Remove special '\' quoting characters from Str.
      --  As a special case, if Remove_Quoted is true, then '\'
      --  and the following char are simply omitted in the output.
      --  For instance:
      --      Str="A\." Remove_Quoted=False  => output is "A."
      --      Str="A\." Remove_Quoted=False  => output is "A"

      ---------------
      -- Is_Regexp --
      ---------------

      function Is_Regexp (Str : String) return Boolean is
         --  Take into account characters quoted by '\'. We just remove them
         --  for now, so that when we quote the regexp it won't see these
         --  potentially special characters.
         --  The goal is that for instance "\.\." is not considered
         --  as a regexp, but "\.." is.
         Str2 : constant String := Unquote_Regexp (Str, Remove_Quoted => True);
      begin
         return Regpat.Quote (Str2) /= Str2;
      end Is_Regexp;

      ---------------
      -- Save_File --
      ---------------

      procedure Save_File (Current_Dir : String; Val : String) is
      begin
         if not Merge_Same_Dirs then
            Trace (Main_Trace, "<dir>: SAVE " & Current_Dir);

            Processed_Value.Append
               ((Value          => To_Unbounded_String (Val),
                 Alternate      => Null_Unbounded_String,
                 Extracted_From =>
                   To_Unbounded_String (Get_String_No_Adalib (Current_Dir))));

         else
            declare
               use String_To_External_Value;

               Normalized : constant String := Normalize_Pathname
                              (Name           => Current_Dir,
                               Directory      => "",
                               Resolve_Links  => True,
                               Case_Sensitive => True);
               Prev       : External_Value_Lists.Cursor;
               Rec        : External_Value_Item;
            begin
               if Visited.Contains (Normalized) then
                  Trace
                    (Main_Trace,
                     "<dir>: ALREADY FOUND (" & Val & ") " & Current_Dir);

                  Prev          := Visited.Element (Normalized);
                  Rec           := External_Value_Lists.Element (Prev);
                  Rec.Alternate := To_Unbounded_String (Val);

                  External_Value_Lists.Replace_Element
                    (Container => Processed_Value,
                     Position  => Prev,
                     New_Item  => Rec);

               else
                  Trace
                    (Main_Trace, "<dir>: SAVE (" & Val & ") " & Current_Dir);

                  Processed_Value.Append
                    ((Value          => To_Unbounded_String (Val),
                      Alternate      => Null_Unbounded_String,
                      Extracted_From =>
                        To_Unbounded_String
                          (Get_String_No_Adalib (Current_Dir))));

                  Visited.Include
                    (Normalized, External_Value_Lists.Last (Processed_Value));
               end if;
            end;
         end if;
      end Save_File;

      --------------------
      -- Unquote_Regexp --
      --------------------

      function Unquote_Regexp
        (Str : String; Remove_Quoted : Boolean := False) return String
      is
         Str2  : String (Str'Range);
         S     : Integer := Str'First;
         Index : Integer := Str2'First;
      begin
         while S <= Str'Last loop
            if Str (S) = '\' then
               S := S + 1;

               if not Remove_Quoted then
                  Str2 (Index) := Str (S);
                  Index := Index + 1;
               end if;

            else
               Str2 (Index) := Str (S);
               Index := Index + 1;
            end if;

            S     := S + 1;
         end loop;

         return Str2 (Str2'First .. Index - 1);
      end Unquote_Regexp;

      First : constant Integer := Path_To_Check'First;
      Last  : Integer;
      Val   : Unbounded_String;

   begin
      if Path_To_Check'Length = 0
        or else Path_To_Check = "/"
        or else Path_To_Check = String'(1 => Directory_Separator)
      then
         if Group = -1 then
            Val := To_Unbounded_String (Value_If_Match);
         else
            Val := To_Unbounded_String (Group_Match);
         end if;

         if not Contents.Is_Empty
           and then Is_Regular_File (Current_Dir)
         then
            Trace (Main_Trace, "<dir>: Checking inside file " & Current_Dir);

            declare
               use Ada.Text_IO;
               use Directory_Operations;
               F : File_Type;
            begin
               Open (F, In_File, Current_Dir);

               while not End_Of_File (F) loop
                  declare
                     Line : constant String := Get_Line (F);
                  begin
                     Trace (Main_Trace, "<dir>: read line " & Line);

                     if Match (Contents.Element, Line) then
                        Save_File
                          (Normalize_Pathname
                             (Name => Line,
                              Directory => Dir_Name (Current_Dir),
                              Resolve_Links => True),
                           To_String (Val));
                        exit;
                     end if;
                  end;
               end loop;

               Close (F);
            end;

         else
            Save_File (Current_Dir, To_String (Val));
         end if;

      else
         --  Do not split on '\', since we document we only accept UNIX paths
         --  anyway. This leaves \ for regexp quotes.
         Last := First + 1;

         while Last <= Path_To_Check'Last
           and then Path_To_Check (Last) /= '/'
         loop
            Last := Last + 1;
         end loop;

         --  If we do not have a regexp

         if not Is_Regexp (Path_To_Check (First .. Last - 1)) then
            declare
               Dir     : constant String :=
                           Normalize_Pathname
                             (Current_Dir, Resolve_Links => False)
                           & Directory_Separator
                           & Unquote_Regexp
                              (Path_To_Check (First .. Last - 1));
               Remains : constant String :=
                           Path_To_Check (Last + 1 .. Path_To_Check'Last);
            begin
               if (Remains'Length = 0
                   or else Remains = "/"
                   or else Remains = String'(1 => Directory_Separator))
                 and then Is_Regular_File (Dir)
               then
                  Trace (Main_Trace, "<dir>: Found file " & Dir);
                  --  If there is such a subdir, keep checking

                  Parse_All_Dirs
                    (Processed_Value => Processed_Value,
                     Visited         => Visited,
                     Current_Dir     => Dir,
                     Path_To_Check   => Remains,
                     Regexp          => Regexp,
                     Regexp_Str      => Regexp_Str,
                     Value_If_Match  => Value_If_Match,
                     Group           => Group,
                     Group_Match     => Group_Match,
                     Group_Count     => Group_Count,
                     Contents        => Contents,
                     Merge_Same_Dirs => Merge_Same_Dirs,
                     Error_Sloc      => Error_Sloc,
                     Messages        => Messages);

               elsif Is_Directory (Dir) then
                  Trace (Main_Trace, "<dir>: Recurse into " & Dir);
                  --  If there is such a subdir, keep checking

                  Parse_All_Dirs
                    (Processed_Value => Processed_Value,
                     Visited         => Visited,
                     Current_Dir     => Dir & Directory_Separator,
                     Path_To_Check   => Remains,
                     Regexp          => Regexp,
                     Regexp_Str      => Regexp_Str,
                     Value_If_Match  => Value_If_Match,
                     Group           => Group,
                     Group_Match     => Group_Match,
                     Group_Count     => Group_Count,
                     Contents        => Contents,
                     Merge_Same_Dirs => Merge_Same_Dirs,
                     Error_Sloc      => Error_Sloc,
                     Messages        => Messages);
               else
                  Trace (Main_Trace, "<dir>: No such directory: " & Dir);
               end if;
            end;

         --  Else we have a regexp, check all files

         else
            declare
               use Ada.Directories;

               File_Re     : constant String :=
                               Path_To_Check (First .. Last - 1);
               File_Regexp : constant Pattern_Matcher := Compile (File_Re);
               Search      : Search_Type;
               File        : Directory_Entry_Type;
               Filter      : Ada.Directories.Filter_Type;
            begin
               Trace
                 (Main_Trace,
                  "Potential error: .. is generally not meant as a regexp,"
                  & " and should be quoted in this case, as in \.\.");

               if Path_To_Check (Last) = '/' then
                  Trace
                    (Main_Trace,
                     "<dir>: Check directories in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (Directory => True, others => False);

               else
                  Trace
                    (Main_Trace,
                     "<dir>: Check files in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (others => True);
               end if;

               Start_Search
                 (Search    => Search,
                  Directory => Current_Dir,
                  Filter    => Filter,
                  Pattern   => "");

               while More_Entries (Search) loop
                  Get_Next_Entry (Search, File);

                  if Directories.Simple_Name (File) /= "."
                    and then Directories.Simple_Name (File) /= ".."
                  then
                     declare
                        Simple  : constant String :=
                                    Directories.Simple_Name (File);
                        Count   : constant Natural :=
                                    Paren_Count (File_Regexp);
                        Matched : Match_Array (0 .. Integer'Max (Group, 0));
                     begin
                        Match (File_Regexp, Simple, Matched);

                        if Matched (0) /= No_Match then
                           Trace
                             (Main_Trace,
                              "<dir>: Matched "
                              & Ada.Directories.Simple_Name (File));

                           if Group_Count < Group
                             and then Group_Count + Count >= Group
                           then
                              Trace
                                (Main_Trace,
                                 "<dir>: Found matched group: "
                                 & Simple (Matched (Group - Group_Count).First
                                   .. Matched (Group - Group_Count).Last));

                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Visited         => Visited,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     =>
                                   Simple (Matched (Group - Group_Count).First
                                       .. Matched (Group - Group_Count).Last),
                                 Group_Count     => Group_Count + Count,
                                 Contents        => Contents,
                                 Merge_Same_Dirs => Merge_Same_Dirs,
                                 Error_Sloc      => Error_Sloc,
                                 Messages        => Messages);

                           else
                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Visited         => Visited,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     => Group_Match,
                                 Group_Count     => Group_Count + Count,
                                 Contents        => Contents,
                                 Merge_Same_Dirs => Merge_Same_Dirs,
                                 Error_Sloc      => Error_Sloc,
                                 Messages        => Messages);
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Parse_All_Dirs;

   ----------------------------------
   -- Parse_Default_Knowledge_Base --
   ----------------------------------

   function Parse_Default_Knowledge_Base
     (Flags : Parsing_Flags) return Object
   is
      use GNAT.Directory_Operations;
      use GNATCOLL.Traces;
      use DOM.Core;
      use DOM.Core.Nodes;
      use Input_Sources.Strings;
      use Schema.Dom_Readers;
      use Sax.Readers;

      use GPR2.Containers.Name_Value_Map_Package;

      KB_Content : GPR2.Containers.Name_Value_Map;
      Result     : Object;

      type Resolving_Reader is new Schema.Dom_Readers.Tree_Reader with record
         Current_Source : Unbounded_String;
      end record;

      overriding function Resolve_Entity
        (Handler   : Resolving_Reader;
         Public_Id : Unicode.CES.Byte_Sequence;
         System_Id : Unicode.CES.Byte_Sequence)
         return Input_Sources.Input_Source_Access;
      --  Resolves entities from knowledge base

      --------------------
      -- Resolve_Entity --
      --------------------

      overriding function Resolve_Entity
        (Handler   : Resolving_Reader;
         Public_Id : Unicode.CES.Byte_Sequence;
         System_Id : Unicode.CES.Byte_Sequence)
         return Input_Sources.Input_Source_Access
      is
         Pub_Id : constant Optional_Name_Type :=
                    Optional_Name_Type (Public_Id);
         Sys_Id : constant Optional_Name_Type :=
                    Optional_Name_Type (System_Id);
         Input  : constant String_Input_Access :=
                    (if Pub_Id /= "" or else Sys_Id /= "" then new String_Input
                     else null);
      begin
         Trace (Main_Trace, "Public_Id=""" & Public_Id & """");
         Trace (Main_Trace, "System_Id=""" & System_Id & """");

         if Pub_Id /= "" and then KB_Content.Contains (Pub_Id) then
            Open
              (KB_Content.Element (Pub_Id),
               Unicode.CES.Utf8.Utf8_Encoding,
               Input.all);

         elsif Sys_Id /= "" and then KB_Content.Contains (Sys_Id) then
            Open
              (KB_Content.Element (Sys_Id),
               Unicode.CES.Utf8.Utf8_Encoding,
               Input.all);
         else
            Result.Messages.Append
              (Message.Create
                 (Message.Error,
                  "entity not found for Public_Id="""
                  & Public_Id
                  & """, System_Id="""
                  & System_Id
                  & """",
                  Source_Reference.Create
                    (Embed_Pseudo_Dir & To_String (Handler.Current_Source),
                     0, 0)));
         end if;

         return Input_Sources.Input_Source_Access (Input);
      end Resolve_Entity;

      Reader : Resolving_Reader;
      Input  : String_Input;
      Cur    : Containers.Name_Value_Map_Package.Cursor;
   begin
      Result.Initialized := True;
      KB_Content := Default_Content;

      Cur := KB_Content.First;

      while Cur /= No_Element loop
         if File_Extension (String (Key (Cur))) = ".xml" then
            Trace (Main_Trace, "Parsing embedded file " & String (Key (Cur)));
            Reader.Current_Source := To_Unbounded_String (String (Key (Cur)));
            Reader.Set_Feature (Schema_Validation_Feature, Flags (Validation));
            Reader.Set_Feature (Validation_Feature, False);  --  Do not use DTD

            Open
              (Containers.Name_Value_Map_Package.Element (Cur),
               Unicode.CES.Utf8.Utf8_Encoding,
               Input);
            Parse (Reader, Input);
            Close (Input);

            Parse_Knowledge_Base
              (Result,
               DOM.Core.Documents.Get_Element (Get_Tree (Reader)),
               Flags,
               Embed_Pseudo_Dir & String (Key (Cur)));

            declare
               Doc : Document := Get_Tree (Reader);
            begin
               Free (Doc);
            end;

            Free (Reader);
         end if;

         Next (Cur);
      end loop;

      return Result;
   end Parse_Default_Knowledge_Base;

   --------------------------
   -- Parse_Knowledge_Base --
   --------------------------

   procedure Parse_Knowledge_Base
     (Self     : in out Object;
      Location : GPR2.Path_Name.Object;
      Flags    : Parsing_Flags)
   is
      use GPR2.Path_Name;
      use Ada.Directories;

      procedure Parse_Single_File (File : GPR2.Path_Name.Object);
      --  Parses a single .xml file containing KB chunks

      -----------------------
      -- Parse_Single_File --
      -----------------------

      procedure Parse_Single_File (File : GPR2.Path_Name.Object) is
         use GNATCOLL.Traces;
         use DOM.Core, DOM.Core.Nodes;
         use Input_Sources.File;
         use Schema.Dom_Readers;
         use Sax.Readers;

         Reader : Schema.Dom_Readers.Tree_Reader;
         Input  : File_Input;
      begin
         Trace (Main_Trace, "Parsing file " & String (File.Value));
         Reader.Set_Feature (Schema_Validation_Feature, Flags (Validation));
         Reader.Set_Feature (Validation_Feature, False);  --  Do not use DTD

         Open (String (File.Value), Input);
         Parse (Reader, Input);
         Close (Input);

         Parse_Knowledge_Base
           (Self,
            DOM.Core.Documents.Get_Element (Get_Tree (Reader)),
            Flags, File.Value);

         declare
            Doc : Document := Get_Tree (Reader);
         begin
            Free (Doc);
         end;

         Free (Reader);
      end Parse_Single_File;

      Search : Search_Type;
      File   : Directory_Entry_Type;

   begin
      if Location.Is_Directory then
         Start_Search
           (Search,
            Directory => String (Location.Value),
            Pattern   => "*.xml",
            Filter    => (Ordinary_File => True, others => False));

         while More_Entries (Search) loop
            Get_Next_Entry (Search, File);
            Parse_Single_File
              (Create_File (Name_Type (Ada.Directories.Full_Name ((File)))));
         end loop;

         End_Search (Search);

      else
         Parse_Single_File (Location);
      end if;
   end Parse_Knowledge_Base;

   --------------------------
   -- Parse_Knowledge_Base --
   --------------------------

   procedure Parse_Knowledge_Base
     (Base      : in out Object;
      Root_Node : DOM.Core.Node;
      Flags     : Parsing_Flags;
      From_File : Value_Not_Empty)
   is
      use DOM.Core;
      use DOM.Core.Nodes;

      Error_Sloc : constant Source_Reference.Object :=
                     Source_Reference.Object
                       (Source_Reference.Create (From_File, 0, 0));

      procedure Parse_Compiler_Description
        (Base        : in out Object;
         Description : DOM.Core.Node);
      --  Parses a compiler description described by N. Appends the result to
      --  Base.Compilers or Base.No_Compilers.

      procedure Parse_Configuration
        (Append_To   : in out Configuration_Lists.List;
         Description : DOM.Core.Node);
      --  Parses a configuration node

      procedure Parse_Targets_Set
        (Append_To   : in out Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node);
      --  Parses a targets set node

      procedure Parse_Fallback_Targets_Set
        (Append_To   : in out Fallback_Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node);
      --  Parses a fallback_targets set node

      --------------------------------
      -- Parse_Compiler_Description --
      --------------------------------

      procedure Parse_Compiler_Description
        (Base        : in out Object;
         Description : DOM.Core.Node)
      is
         use Ada.Characters.Handling;
         use GNAT.Regpat;
         use External_Value_Lists;
         use Pattern_Matcher_Holders;

         procedure Parse_External_Value
           (Value    : out External_Value;
            External : Node);
         --  Parses an XML node that describes an external value

         function Ends_With (Str, Suffix : String) return Boolean;
         --  Whether the string ends with Suffix. Always True if Suffix
         --  is an empty string.

         ---------------
         -- Ends_With --
         ---------------

         function Ends_With (Str, Suffix : String) return Boolean is
         begin
            return Suffix = ""
              or else
                (Str'Length >= Suffix'Length
                 and then Str
                   (Str'Last - Suffix'Length + 1 .. Str'Last) = Suffix);
         end Ends_With;

         --------------------------
         -- Parse_External_Value --
         --------------------------

         procedure Parse_External_Value
           (Value    : out External_Value;
            External : Node)
         is
            Static_Value  : constant String := Node_Value_As_String (External);
            Tmp           : Node := First_Child (External);
            External_Node : External_Value_Node;
            Is_Done       : Boolean := True;
            Has_Static    : Boolean := False;

            use External_Value_Nodes;
         begin
            for S in Static_Value'Range loop
               if Static_Value (S) not in ' ' | ASCII.LF then
                  Has_Static := True;
                  exit;
               end if;
            end loop;

            --  Constant value is not within a nested node

            if Has_Static then
               External_Node :=
                 (Typ   => Value_Constant,
                  Value => To_Unbounded_String (Static_Value));
               Append (Value, External_Node);
               Is_Done := False;
            end if;

            while Tmp /= null loop
               if Node_Type (Tmp) /= Element_Node then
                  null;

               elsif Node_Name (Tmp) = "external" then
                  if not Is_Done then
                     Append (Value, (Typ => Value_Done));
                  end if;

                  External_Node :=
                    (Typ     => Value_Shell,
                     Command => To_Unbounded_String
                       (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := False;

               elsif Node_Name (Tmp) = "directory" then
                  declare
                     C        : constant String :=
                                  Get_Attribute (Tmp, "contents", "");
                     Contents : Pattern_Matcher_Holder;
                  begin
                     if C /= "" then
                        Contents := To_Holder (Compile (C));
                     end if;

                     External_Node :=
                       (Typ             => Value_Directory,
                        Directory       => To_Unbounded_String
                                             (Node_Value_As_String (Tmp)),
                        Contents        => Contents,
                        Dir_If_Match    => Null_Unbounded_String,
                        Directory_Group => 0);
                  end;

                  begin
                     External_Node.Directory_Group :=
                       Integer'Value
                         (Get_Attribute (Tmp, "group", "0"));
                  exception
                     when Constraint_Error =>
                        External_Node.Directory_Group := -1;
                        External_Node.Dir_If_Match :=
                          To_Unbounded_String
                            (Get_Attribute (Tmp, "group", "0"));
                  end;

                  Append (Value, External_Node);
                  Is_Done := True;

               elsif Node_Name (Tmp) = "getenv" then
                  if not Is_Done then
                     Append (Value, (Typ => Value_Done));
                  end if;

                  declare
                     Name : constant String := Get_Attribute (Tmp, "name", "");
                  begin
                     if Ada.Environment_Variables.Exists (Name) then
                        External_Node :=
                          (Typ        => Value_Constant,
                           Value      => To_Unbounded_String
                             (Ada.Environment_Variables.Value (Name)));
                     else
                        Base.Messages.Append
                          (Message.Create
                             (Message.Warning,
                              "environment variable '"
                              & Name & "' is not defined",
                              Sloc => Error_Sloc));

                        External_Node :=
                          (Typ   => Value_Constant,
                           Value => Null_Unbounded_String);
                     end if;
                  end;

                  Append (Value, External_Node);
                  Is_Done := False;

               elsif Node_Name (Tmp) = "filter" then
                  External_Node :=
                    (Typ        => Value_Filter,
                     Filter     => To_Unbounded_String
                       (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := True;

               elsif Node_Name (Tmp) = "must_match" then
                  External_Node :=
                    (Typ        => Value_Must_Match,
                     Must_Match => To_Unbounded_String
                       (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := True;

               elsif Node_Name (Tmp) = "grep" then
                  External_Node :=
                    (Typ       => Value_Grep,
                     Regexp_Re => To_Holder
                                    (Compile
                                      (Get_Attribute (Tmp, "regexp", ".*"),
                                       Multiple_Lines)),
                     Group     => Integer'Value
                                    (Get_Attribute (Tmp, "group", "0")));
                  Append (Value, External_Node);

               elsif Node_Name (Tmp) = "nogrep" then
                  External_Node :=
                    (Typ       => Value_Nogrep,
                     Regexp_No => To_Holder
                                    (Compile
                                      (Get_Attribute (Tmp, "regexp", ".*"),
                                       Multiple_Lines)));
                  Append (Value, External_Node);

               else
                  Base.Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "invalid XML description for "
                        & Node_Name (External)
                        & ": invalid tag: " & Node_Name (Tmp),
                        Sloc => Error_Sloc));
                  Value := Null_External_Value;
               end if;

               Tmp := Next_Sibling (Tmp);
            end  loop;

            if not Is_Done then
               Append (Value, (Typ => Value_Done));
            end if;

         exception
            when Constraint_Error =>
               Base.Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "invalid group number for " & Node_Name (External),
                     Sloc => Error_Sloc));
               Value := Null_External_Value;
         end Parse_External_Value;

         Compiler    : Compiler_Description;
         N           : Node := First_Child (Description);
         Lang        : External_Value_Lists.List;
         C           : External_Value_Lists.Cursor;

         Exec_Suffix : OS_Lib.String_Access :=
                         OS_Lib.Get_Executable_Suffix;

      begin
         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "executable" then
               declare
                  Prefix : constant String :=
                             Get_Attribute (N, "prefix", "@@");
                  Val    : constant String := Node_Value_As_String (N);
               begin
                  if Val = "" then
                     --  A special language that requires no executable. We do
                     --  not store it in the list of compilers, since these
                     --  should not be detected on the PATH anyway.

                     Compiler.Executable := Null_Unbounded_String;

                  else
                     Compiler.Executable := To_Unbounded_String (Val);

                     begin
                        Compiler.Prefix_Index := Integer'Value (Prefix);
                     exception
                        when Constraint_Error =>
                           Compiler.Prefix_Index := -1;
                     end;

                     if not Ends_With (Val, Exec_Suffix.all) then
                        Compiler.Executable_Re := To_Holder
                          (Compile ("^" & Val & Exec_Suffix.all & "$"));
                     else
                        Compiler.Executable_Re := To_Holder
                          (Compile ("^" & Val & "$"));
                     end if;

                     Base.Check_Executable_Regexp := True;
                  end if;

               exception
                  when Expression_Error =>
                     Base.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Invalid regular expression found in configuration:"
                           & Val,
                           Sloc => Error_Sloc));
                     Compiler.Executable_Re.Clear;
               end;

            elsif Node_Name (N) = "name" then
               Compiler.Name := To_Unbounded_String (Node_Value_As_String (N));

            elsif Node_Name (N) = "version" then
               Parse_External_Value
                 (Value    => Compiler.Version,
                  External => N);

            elsif Node_Name (N) = "variable" then
               declare
                  Name : constant String := Get_Attribute (N, "name", "@@");
               begin
                  Compiler.Variables.Append
                    (External_Value_Node'
                      (Typ      => Value_Variable,
                       Var_Name => To_Unbounded_String (Name)));
                  Parse_External_Value
                    (Value    => Compiler.Variables,
                     External => N);
               end;

            elsif Node_Name (N) = "languages" then
               Parse_External_Value
                 (Value    => Compiler.Languages,
                  External => N);

            elsif Node_Name (N) = "runtimes" then
               declare
                  Defaults : constant String :=
                               Get_Attribute (N, "default", "");
               begin
                  if Defaults /= "" then
                     Get_Words (Defaults, "", ' ', ',',
                                Compiler.Default_Runtimes, False);
                  end if;

                  Parse_External_Value
                    (Value    => Compiler.Runtimes,
                     External => N);
               end;

            elsif Node_Name (N) = "target" then
               Parse_External_Value
                 (Value    => Compiler.Target,
                  External => N);

            else
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Unknown XML tag in " & Node_Name (N),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

            N := Next_Sibling (N);
         end loop;

         if Compiler.Executable = Null_Unbounded_String then
            Get_External_Value
              (Attribute        => "languages",
               Value            => Compiler.Languages,
               Comp             => No_Compiler,
               Split_Into_Words => True,
               Calls_Cache      => Base.External_Calls_Cache,
               Error_Sloc       => Error_Sloc,
               Messages         => Base.Messages,
               Processed_Value  => Lang);

            C := First (Lang);

            while Has_Element (C) loop
               Base.No_Compilers.Append
                 (Name_Type
                    (To_Lower
                       (To_String
                          (External_Value_Lists.Element (C).Value))));
               Next (C);
            end loop;

         elsif Compiler.Name /= Null_Unbounded_String then
            Base.Compilers.Include
              (Optional_Name_Type (To_String (Compiler.Name)), Compiler);

            --  Include the language name in the Languages_Known hashed map,
            --  if it is not already there.

            declare
               use External_Value_Nodes;

               Languages : External_Value_Nodes.Cursor :=
                             Compiler.Languages.First;
               Lang      : External_Value_Node;
            begin
               while Languages /= External_Value_Nodes.No_Element loop
                  Lang := External_Value_Nodes.Element (Languages);

                  if Lang.Typ = Value_Constant then
                     if not Base.Languages_Known.Contains (Lang.Value) then
                        Base.Languages_Known.Include (Lang.Value, Lang.Value);
                     end if;
                  end if;

                  Next (Languages);
               end loop;
            end;
         end if;

         OS_Lib.Free (Exec_Suffix);
      end Parse_Compiler_Description;

      -------------------------
      -- Parse_Configuration --
      -------------------------

      procedure Parse_Configuration
        (Append_To   : in out Configuration_Lists.List;
         Description : DOM.Core.Node)
      is
         use Ada.Characters.Handling;
         use GNAT.Regpat;
         use Compilers_Filter_Lists;

         function Compile_And_Check
           (Name : String) return Pattern_Matcher_Holder;
         --  Compiles pattern and report illegal regexp if needed

         -----------------------
         -- Compile_And_Check --
         -----------------------

         function Compile_And_Check
           (Name : String) return Pattern_Matcher_Holder
         is
            use Pattern_Matcher_Holders;
         begin
            if Name = "" then
               return Empty_Holder;
            else
               return To_Holder
                 (Compile (Name, Regpat.Case_Insensitive));
            end if;
         exception
            when Expression_Error =>
               --  We do not want to invalidate the whole Knowledge
               --  Base because of a wrong regexp. Istead, report it
               --  and skip corresponding <configuration> node.
               Base.Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "Invalid regexp '"
                     & Name & "'; corresponding configuration "
                     & "node skipped",
                     Sloc => Error_Sloc));
               raise;
         end Compile_And_Check;

         Config           : Configuration_Type;
         Chunk            : Unbounded_String;
         N                : Node := First_Child (Description);
         N2               : Node;
         Compilers        : Compilers_Filter;
         Ignore_Config    : Boolean := False;
         Negate           : Boolean;
         Filter           : Compiler_Filter;

         Default_Hostname : String renames System.OS_Constants.Target_Name;

      begin
         Config.Supported := True;

         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "compilers" then
               Compilers := No_Compilers_Filter;
               N2 := First_Child (N);

               while N2 /= null loop
                  if Node_Type (N2) /= Element_Node then
                     null;

                  elsif Node_Name (N2) = "compiler" then
                     declare
                        Name    : constant String :=
                                    Get_Attribute (N2, "name", "");
                        Version : constant String :=
                                    Get_Attribute (N2, "version", "");
                        Runtime : constant String :=
                                    Get_Attribute (N2, "runtime", "");
                     begin
                        Filter := Compiler_Filter'
                          (Name        => To_Unbounded_String (Name),
                           Name_Re     => Compile_And_Check (Name),
                           Version     => To_Unbounded_String (Version),
                           Version_Re  => Compile_And_Check (Version),
                           Runtime     => To_Unbounded_String (Runtime),
                           Runtime_Re  => Compile_And_Check (Runtime),
                           Language_LC => To_Unbounded_String
                                            (To_Lower
                                               (Get_Attribute
                                                  (N2, "language", ""))));
                     end;

                     Compilers.Compiler.Append (Filter);

                  else
                     Base.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Unknown XML tag in " & Node_Name (N2),
                           Sloc => Error_Sloc));
                     raise Invalid_KB;
                  end if;

                  N2 := Next_Sibling (N2);
               end loop;

               Compilers.Negate :=
                 Boolean'Value (Get_Attribute (N, "negate", "False"));
               Config.Compilers_Filters.Append (Compilers);

            elsif Node_Name (N) = "targets" then
               if not Config.Targets_Filters.Is_Empty then
                  Base.Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "Multiple <targets> filters specified for "
                        & "configuration node, filter ignored",
                        Sloc => Error_Sloc));

               else
                  N2 := First_Child (N);

                  while N2 /= null loop
                     if Node_Type (N2) /= Element_Node then
                        null;

                     elsif Node_Name (N2) = "target" then
                        declare
                           Double_Regexp : constant Double_String :=
                             (To_Unbounded_String
                                (Get_Attribute (N2, "name", "")),
                              To_Unbounded_String
                                (Get_Attribute (N2, "except", "")));
                        begin
                           Config.Targets_Filters.Append (Double_Regexp);
                        end;

                     else
                        Base.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "Unknown XML tag in " & Node_Name (N2),
                           Sloc => Error_Sloc));
                        raise Invalid_KB;
                     end if;

                     N2 := Next_Sibling (N2);
                  end loop;

                  Config.Negate_Targets := Boolean'Value
                    (Get_Attribute (N, "negate", "False"));
               end if;

            elsif Node_Name (N) = "hosts" then
               --  Resolve this filter immediately. This saves memory, since we
               --  don't need to store it in memory if we know it won't apply.

               N2 := First_Child (N);
               Negate := Boolean'Value (Get_Attribute (N, "negate", "False"));

               Ignore_Config := not Negate;

               while N2 /= null loop
                  if Node_Type (N2) /= Element_Node then
                     null;

                  elsif Node_Name (N2) = "host" then
                     declare
                        Host_Same : constant Boolean :=
                                      Match
                                        (Get_Attribute
                                           (N2, "name", ""), Default_Hostname);
                        No_Except : constant Boolean :=
                                      Get_Attribute (N2, "except", "") = "";
                        Excepted  : constant Boolean :=
                                      Match
                                        (Get_Attribute (N2, "except", ""),
                                         Default_Hostname);
                     begin
                        if
                          Host_Same and then (No_Except or else not Excepted)
                        then
                           Ignore_Config := Negate;
                           exit;
                        end if;
                     end;

                  else
                     Base.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Unknown XML tag in " & Node_Name (N2),
                           Sloc => Error_Sloc));
                     raise Invalid_KB;
                  end if;

                  N2 := Next_Sibling (N2);
               end loop;

               exit when Ignore_Config;

            elsif Node_Name (N) = "config" then
               if Node_Value_As_String (N) = "" then
                  Config.Supported := False;
               else
                  Append (Chunk, Node_Value_As_String (N));
               end if;

            else
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Unknown XML tag in " & Node_Name (N),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

            N := Next_Sibling (N);
         end loop;

         if not Ignore_Config then
            Config.Config := Chunk;
            Append_To.Append (Config);
         end if;
      exception
         when Expression_Error =>
            null;
            --  Proper warning message has been already emitted, so we just
            --  skip corresponding configuration node.
      end Parse_Configuration;

      --------------------------------
      -- Parse_Fallback_Targets_Set --
      --------------------------------

      procedure Parse_Fallback_Targets_Set
        (Append_To   : in out Fallback_Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node)
      is
         Set : GPR2.Containers.Name_List;
         N   : Node := First_Child (Description);
      begin
         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "target" then
               Set.Append (Optional_Name_Type (Node_Value_As_String (N)));

            else
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Unknown XML tag in " & Node_Name (N),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

            N := Next_Sibling (N);
         end loop;

         if not Set.Is_Empty then
            Fallback_Targets_Set_Vectors.Append (Append_To, Set);
         end if;
      end Parse_Fallback_Targets_Set;

      -----------------------
      -- Parse_Targets_Set --
      -----------------------

      procedure Parse_Targets_Set
        (Append_To   : in out Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node)
      is
         use GNAT.Regpat;

         Canon : constant String :=
                   Get_Attribute (Description, "canonical", "");
         Name  : Unbounded_String;
         Set   : Target_Lists.List;
         N     : Node := First_Child (Description);
      begin
         if Canon = "" then
            if Flags (Pedantic) then
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "No canonical target specified for target-set",
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

         else
            Name := To_Unbounded_String (Canon);
         end if;

         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "target" then
               declare
                  Val : constant String := Node_Value_As_String (N);
               begin
                  Target_Lists.Append (Set, Compile ("^" & Val & "$"));

                  if Name = Null_Unbounded_String then
                     --  When not in pedantic mode and working with
                     --  an old KB the first target in the target set
                     --  is taken as canonical target.
                     Name := To_Unbounded_String (Val);
                  end if;
               exception
                  when Expression_Error =>
                     Base.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Invalid regular expression " & Val
                           & " found in the target-set",
                           Sloc => Error_Sloc));
                        raise Invalid_KB;
               end;

            else
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Unknown XML tag " & Node_Name (N),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

            N := Next_Sibling (N);
         end loop;

         if not Target_Lists.Is_Empty (Set) then
            Targets_Set_Vectors.Append (Append_To, (Name, Set));
         end if;
      end Parse_Targets_Set;

      N : Node;

   begin
      if Node_Name (Root_Node) = "gprconfig" then
         N := First_Child (Root_Node);

         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "compiler_description" then
               if Flags (Compiler_Info) then
                  Parse_Compiler_Description
                    (Base        => Base,
                     Description => N);
               end if;

            elsif Node_Name (N) = "configuration" then
               if Flags (Compiler_Info) then
                  Parse_Configuration
                    (Append_To   => Base.Configurations,
                     Description => N);
               end if;

            elsif Node_Name (N) = "targetset" then
               Parse_Targets_Set
                 (Append_To   => Base.Targets_Sets,
                  Description => N);

            elsif Node_Name (N) = "fallback_targets" then
               Parse_Fallback_Targets_Set
                 (Append_To   => Base.Fallback_Targets_Sets,
                  Description => N);

            else
               Base.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "Unknown XML tag "
                     & Node_Name (N),
                     Sloc => Error_Sloc));
               raise Invalid_KB;
            end if;

            N := Next_Sibling (N);
         end loop;

      else
         Base.Messages.Append
           (Message.Create
              (Message.Error, "Invalid toplevel XML tag", Sloc => Error_Sloc));
      end if;
   exception
      when Invalid_KB =>
         --  Error messages have been added to the log, no point continuing
         --  parsing an inconsistent file.
         null;
   end Parse_Knowledge_Base;

   --------------------------
   -- Substitute_Variables --
   --------------------------

   function Substitute_Variables
     (Str        : String;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String
   is
      use Ada.Characters.Handling;

      Str_Len              : constant Natural := Str'Last;
      Pos                  : Natural := Str'First;
      Last                 : Natural := Pos;
      Result               : Unbounded_String;
      Word_Start, Word_End : Natural;
      Tmp                  : Natural;
      Has_Index            : Boolean;

   begin
      while Pos < Str_Len loop
         if Str (Pos) = '$' and then Str (Pos + 1) = '$' then
            Append (Result, Str (Last .. Pos - 1));
            Append (Result, "$");
            Last := Pos + 2;
            Pos  := Last;

         elsif Str (Pos) = '$' then
            if Str (Pos + 1)  = '{' then
               Word_Start := Pos + 2;
               Tmp := Pos + 2;

               while Tmp <= Str_Len and then Str (Tmp) /= '}' loop
                  Tmp := Tmp + 1;
               end loop;

               Tmp := Tmp + 1;
               Word_End := Tmp - 2;

            else
               Word_Start := Pos + 1;
               Tmp := Pos + 1;

               while Tmp <= Str_Len
                 and then (Is_Alphanumeric (Str (Tmp)) or else Str (Tmp) = '_')
               loop
                  Tmp := Tmp + 1;
               end loop;

               Word_End := Tmp - 1;
            end if;

            Append (Result, Str (Last ..  Pos - 1));

            Has_Index := False;

            for W in Word_Start .. Word_End loop
               if Str (W) = '(' then
                  Has_Index := True;

                  if Str (Word_End) /= ')' then
                     Messages.Append
                       (Message.Create
                          (Message.Error,
                           "Missing closing parenthesis in variable name: "
                           & Str (Word_Start .. Word_End),
                           Sloc => Error_Sloc));
                     raise Invalid_KB;

                  else
                     Append
                       (Result,
                        Callback
                          (Var_Name => Str (Word_Start .. W - 1),
                           Index    => Str (W + 1 .. Word_End - 1)));
                  end if;

                  exit;
               end if;
            end loop;

            if not Has_Index then
               Append (Result, Callback (Str (Word_Start .. Word_End), ""));
            end if;

            Last := Tmp;
            Pos  := Last;
         else
            Pos := Pos + 1;
         end if;
      end loop;

      Append (Result, Str (Last .. Str_Len));
      return To_String (Result);
   end Substitute_Variables;

   --------------------------------------------------
   -- Substitute_Variables_In_Compiler_Description --
   --------------------------------------------------

   function Substitute_Variables_In_Compiler_Description
     (Str        : String;
      Comp       : Compiler;
      Error_Sloc : Source_Reference.Object;
      Messages   : in out Log.Object) return String
   is

      function Callback (Var_Name, Index : String) return String;
      --  Wraps Get_Variable_Value for <compiler_description> nodes
      --  and aborts KB parsing in case of improper use of indexed
      --  variables in those nodes.

      --------------
      -- Callback --
      --------------

      function Callback (Var_Name, Index : String) return String is
      begin
         if Index /= "" then
            Messages.Append
              (Message.Create
                 (Message.Error,
                  "Indexed variables only allowed in <configuration> (in "
                  & Var_Name & "(" & Index & ")",
                  Sloc => Error_Sloc));
            raise Invalid_KB;
         end if;

         return Get_Variable_Value (Comp, Var_Name);
      end Callback;

      function Do_Substitute is new Substitute_Variables (Callback);

   begin
      return Do_Substitute (Str, Error_Sloc, Messages);
   end Substitute_Variables_In_Compiler_Description;

end GPR2.Project.Configuration.KB.Parsing;
