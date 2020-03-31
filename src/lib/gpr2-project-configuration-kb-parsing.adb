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

with Ada.Directories;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with Input_Sources.File;
with Input_Sources.Strings;
with Schema.Dom_Readers;
with Sax.Readers;
with Unicode.CES.Utf8;

with GNATCOLL.Traces;

with GPR2.Message;

package body GPR2.Project.Configuration.KB.Parsing is

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
      From_File : String := "")
     with Pre => Base.Is_Defined and then DOM.Core."/=" (Root_Node, null);
   --  Parses a single top-level KB node. From_File used for diagnostics

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
                  GNATCOLL.Traces.Create
                    ("KNOWLEDGE_BASE.PARSING_TRACE",
                     GNATCOLL.Traces.From_Config);

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : in out Object;
      Flags    : Parsing_Flags;
      Content  : Value_Not_Empty)
   is
      use DOM.Core, DOM.Core.Nodes;
      use Input_Sources.Strings;
      use Schema.Dom_Readers;
      use Sax.Readers;

      use GNATCOLL.Traces;

      Reader : Schema.Dom_Readers.Tree_Reader;
      Input  : String_Input;
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
        (Self, DOM.Core.Documents.Get_Element (Get_Tree (Reader)), Flags);

      declare
         Doc : Document := Get_Tree (Reader);
      begin
         Free (Doc);
      end;
      Free (Reader);
   end Add;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N         : DOM.Core.Node;
      Attribute : Value_Not_Empty;
      Default   : Value_Type) return Value_Type
   is
      use DOM.Core, DOM.Core.Nodes;
      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return Default;
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   --------------------------
   -- Node_Value_As_String --
   --------------------------

   function Node_Value_As_String (N : DOM.Core.Node) return String is
      use DOM.Core, DOM.Core.Nodes;

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

      Search : Search_Type;
      File   : Directory_Entry_Type;

      procedure Parse_Single_File (File : GPR2.Path_Name.Object);

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
      From_File : String := "")
   is
      use DOM.Core;
      use DOM.Core.Nodes;

      Invalid_KB : exception;

      Error_Sloc : constant Source_Reference.Object :=
                     (if From_File = ""
                      then Source_Reference.Builtin
                      else Source_Reference.Create (From_File, 0, 0));

      procedure Parse_Compiler_Description
        (Base        : in out Object;
         Description : DOM.Core.Node);
      --  Parses a compiler description described by N. Appends the result to
      --  Base.Compilers or Base.No_Compilers

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
         Description : DOM.Core.Node) is
      begin
         null;
      end Parse_Compiler_Description;

      -------------------------
      -- Parse_Configuration --
      -------------------------

      procedure Parse_Configuration
        (Append_To   : in out Configuration_Lists.List;
         Description : DOM.Core.Node) is
      begin
         null;
      end Parse_Configuration;

      --------------------------------
      -- Parse_Fallback_Targets_Set --
      --------------------------------

      procedure Parse_Fallback_Targets_Set
        (Append_To   : in out Fallback_Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node) is
      begin
         null;
      end Parse_Fallback_Targets_Set;

      -----------------------
      -- Parse_Targets_Set --
      -----------------------

      procedure Parse_Targets_Set
        (Append_To   : in out Targets_Set_Vectors.Vector;
         Description : DOM.Core.Node)
      is
         use GNAT.Regpat;

         Canon   : constant String :=
                     Get_Attribute (Description, "canonical", "");
         Name    : Unbounded_String;
         Set     : Target_Lists.List;
         Pattern : Pattern_Matcher_Access;
         N       : Node := First_Child (Description);
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
                  Pattern := new Pattern_Matcher'(Compile ("^" & Val & "$"));
                  Target_Lists.Append (Set, Pattern);

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
end GPR2.Project.Configuration.KB.Parsing;
