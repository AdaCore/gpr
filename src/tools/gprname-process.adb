------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Ada.Characters.Conversions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with GNAT.Case_Util;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regpat;

with GNATCOLL.OS.Constants;

with Gpr_Parser.Analysis;
with Gpr_Parser.Common;
with Gpr_Parser.Rewriting;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Configuration;
with GPR2.Project.Pretty_Printer;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GPRname.Common;
with GPRname.Options;
with GPRname.Section;
with GPRname.Source.Set;
with GPRname.Unit;

with Langkit_Support.Text;

procedure GPRname.Process (Opt : GPRname.Options.Object) is

   use Ada;
   use Ada.Exceptions;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   use GNAT;

   use Gpr_Parser.Analysis;
   use Gpr_Parser.Common;
   use Gpr_Parser.Rewriting;

   use GPR2;
   use GPR2.Project;
   use GPR2.Project.Attribute;

   use GPRname.Common;
   use GPRname.Section;
   use GPRname.Source;
   use GPRname.Options;

   use Langkit_Support.Text;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;

   package Language_Sources_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Language_Type,
      Source.Set.Object,
      Str_Hash_Case_Insensitive,
      "=",
      Source.Set."=");

   use type GNATCOLL.OS.OS_Type;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

   procedure Search_Directory
     (Dir_Path       : Path_Name.Object;
      Sect           : Section.Object;
      Processed_Dirs : in out Path_Name_Set.Set;
      Recursively    : Boolean;
      Compiler_Args  : OS_Lib.Argument_List_Access);
   --  Process stage that searches a directory (recursively or not) for sources
   --  matching the patterns in section Sect.

   procedure Put (Str : String; Lvl : Verbosity_Level_Type);
   --  Call Ada.Text_IO.Put (Str) if Opt.Verbosity is at least Lvl

   procedure Put_Line (Str : String; Lvl : Verbosity_Level_Type);
   --  Call Ada.Text_IO.Put_Line (Str) if Opt.Verbosity is at least Lvl

   procedure Show_Tree_Load_Errors (Tree : GPR2.Project.Tree.Object);
   --  Print errors/warnings following a project tree load

   ---------
   -- Put --
   ---------

   procedure Put (Str : String; Lvl : Verbosity_Level_Type) is
   begin
      if Opt.Verbosity >= Lvl then
         Text_IO.Put (Str);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String; Lvl : Verbosity_Level_Type) is
   begin
      if Opt.Verbosity >= Lvl then
         Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   ---------------------------
   -- Show_Tree_Load_Errors --
   ---------------------------

   procedure Show_Tree_Load_Errors (Tree : GPR2.Project.Tree.Object) is
   begin
      if Opt.Verbosity > None then
         for M of Tree.Log_Messages.all loop
            M.Output;
         end loop;

      else
         for C in Tree.Log_Messages.Iterate (False, False, True, True, True)
         loop
            GPR2.Log.Element (C).Output;
         end loop;
      end if;
   end Show_Tree_Load_Errors;

   Tree    : GPR2.Project.Tree.Object;
   Context : GPR2.Context.Object;

   Project_Path : GPR2.Path_Name.Object := GPR2.Project.Create
     (Filename_Type (Opt.Project_File));

   From_Scratch : constant Boolean := not Project_Path.Exists;
   --  Indicates that we need to create the project from scratch

   Project_Dir : constant GPR2.Path_Name.Object :=
                   GPR2.Path_Name.Create_Directory
                     (Filename_Optional (Project_Path.Dir_Name));

   Naming_Project_Basename : constant String :=
                               String (Project_Path.Base_Name) & "_naming.gpr";
   Naming_Project_Path     : constant Path_Name.Object :=
                               Project_Dir.Compose
                                 (Filename_Optional (Naming_Project_Basename));

   Naming_Project_Name     : String := String (Naming_Project_Path.Base_Name);

   Source_List_File_Basename : constant String :=
                                 String (Project_Path.Base_Name)
                                 & "_source_list.txt";

   Source_List_File_Path     : constant Path_Name.Object :=
                                 Project_Dir.Compose
                                   (Filename_Optional
                                      (Source_List_File_Basename));

   Compiler_Path : GPR2.Path_Name.Object;

   --  Some containers used throughout the process

   Lang_Sources_Map  : Language_Sources_Map.Map;
   Lang_With_Sources : Language_Vector.Vector;
   Source_Names      : Source.Set.Object;

   --  Strings used in the GPR node templates for project rewriting

   Lang_With_Sources_List : Unbounded_String;  --  attribute Languages
   Dir_List               : Unbounded_String;  --  attribute Source_Dirs

   function Int_Image (X : Integer) return String is
     (if X < 0 then Integer'Image (X)
      else Integer'Image (X) (2 .. Integer'Image (X)'Length));
   --  Integer image, removing the leading whitespace for positive integers

   ----------------------
   -- Search_Directory --
   ----------------------

   procedure Search_Directory
     (Dir_Path       : Path_Name.Object;
      Sect           : Section.Object;
      Processed_Dirs : in out Path_Name_Set.Set;
      Recursively    : Boolean;
      Compiler_Args  : OS_Lib.Argument_List_Access)
   is separate;

begin
   --  Properly set the naming project's name (use mixed case)

   Case_Util.To_Mixed (Naming_Project_Name);

   --
   --  If the project file doesn't exist, create it with the minimum content,
   --  i.e. an empty project with the expected name.
   --

   if From_Scratch then
      declare
         File         : Text_IO.File_Type;
         Project_Name : String := String (Project_Path.Base_Name);

      begin
         --  Properly set the main project's name (use mixed case)

         Case_Util.To_Mixed (Project_Name);

         Text_IO.Create (File, Text_IO.Out_File, String (Project_Path.Name));

         --  Write the bare minimum to be able to parse the project

         Text_IO.Put_Line (File, "project " & Project_Name & " is");
         Text_IO.Put (File, "end " & Project_Name & ";");
         Text_IO.Close (File);

         --  Re-create the object, otherwise the Value field will be wrong

         Project_Path := GPR2.Project.Create
           (Filename_Type (Opt.Project_File));

      exception
         when others =>
            raise GPRname_Exception with
              "could not create project file " & Project_Path.Value;
      end;

   else
      Put_Line ("parsing already existing project file " & Project_Path.Value,
                Low);
   end if;

   --
   --  Load the project and its configuration
   --

   --  Load the raw project, as it may define config-relevant attributes

   declare
      use GPR2.Containers;
      RTS_Map : Lang_Value_Map := Lang_Value_Maps.Empty_Map;
   begin
      if Opt.RTS /= No_String then
         RTS_Map.Insert (Ada_Language, Value_Type (Opt.RTS));
      end if;
      Tree.Load_Autoconf
        (Project_Path, Context,
         Check_Shared_Lib  => False,
         Target            =>
           (if Opt.Target = No_String then
               No_Name
            else
               Name_Type (Opt.Target)),
         Language_Runtimes => RTS_Map);
   exception
      when Project_Error  | Processing_Error =>
         Show_Tree_Load_Errors (Tree);
         raise GPRname_Exception with "failed to load project tree";

      when E : others =>
         Show_Tree_Load_Errors (Tree);
         raise GPRname_Exception with Exception_Information (E);
   end;

   --  Some project kinds are not supported in gprname

   if Tree.Root_Project.Qualifier = K_Aggregate then
      raise GPRname_Exception with
        "aggregate projects are not supported";
   elsif Tree.Root_Project.Qualifier = K_Aggregate_Library then
      raise GPRname_Exception with
        "aggregate library projects are not supported";
   end if;

   if not Tree.Has_Configuration then
      raise GPRname_Exception with "no configuration loaded for the project";
   end if;

   --
   --  Get the compiler path from the project: either a user-defined Compiler
   --  Driver or the one provided by the configuration project.
   --

   declare
      use type OS_Lib.String_Access;

      Ada_I : constant Attribute_Index.Object :=
                Attribute_Index.Create ("ada");

      Proj : constant View.Object := Tree.Root_Project;
      Conf : constant View.Object := Tree.Configuration.Corresponding_View;

      Driver_Attr : GPR2.Project.Attribute.Object :=
                      GPR2.Project.Attribute.Undefined;

      Default_Compiler : OS_Lib.String_Access :=
                           OS_Lib.Locate_Exec_On_Path ("gcc");

   begin
      if Proj.Has_Attribute (PRA.Compiler.Driver, Ada_I)
      then
         --  Use the main project's driver if it is defined

         Driver_Attr := Proj.Attribute (PRA.Compiler.Driver, Ada_I);

      elsif Conf.Has_Attribute (PRA.Compiler.Driver, Ada_I)
      then
         --  Otherwise, we expect to have a configuration-defined driver

         Driver_Attr := Conf.Attribute (PRA.Compiler.Driver, Ada_I);

      else
         raise GPRname_Exception with
           "no compiler driver found in configuration project";
      end if;

      Compiler_Path :=
        Path_Name.Create_File (Filename_Type (Driver_Attr.Value.Text));

      if Is_Windows_Host and then not Compiler_Path.Exists then
         pragma Warnings
           (Off, "this code can never be executed and has been deleted");
         Compiler_Path := Compiler_Path.Change_Extension (".exe");
         pragma Warnings (On);
      end if;

      if not Compiler_Path.Exists then
         Put_Line ("warning: invalid compiler path from configuration ("
                   & Compiler_Path.Value & ")", Low);

         if Default_Compiler /= null then
            Compiler_Path := Path_Name.Create_File
              (Filename_Type (Default_Compiler.all));
            Put_Line ("trying default gcc (" & Compiler_Path.Value & ")", Low);

         else
            raise GPRname_Exception with "no gcc found on PATH";
         end if;
      end if;

      OS_Lib.Free (Default_Compiler);
   end;

   Put_Line ("compiler path = " & Compiler_Path.Value, Low);

   --
   --  Process the section's directories to get the sources that match the
   --  naming patterns. Ada sources are checked by the compiler to get details
   --  of the unit(s) they contain.
   --

   declare
      Processed_Dirs : Path_Name_Set.Set;
      Compiler_Args  : OS_Lib.Argument_List_Access;

   begin
      --  Fill the compiler arguments used to check ada sources

      Compiler_Args :=
        new OS_Lib.Argument_List (1 .. Natural (Opt.Prep_Switches.Length) + 6);

      Compiler_Args (1) := new String'("-c");
      Compiler_Args (2) := new String'("-gnats");
      Compiler_Args (3) := new String'("-gnatu");

      for J in 1 .. Opt.Prep_Switches.Last_Index loop
         Compiler_Args
           (3 + J) := new String'(Opt.Prep_Switches.Element (J));
      end loop;

      Compiler_Args
        (4 + Opt.Prep_Switches.Last_Index) := new String'("-x");
      Compiler_Args
        (5 + Opt.Prep_Switches.Last_Index) := new String'("ada");

      --  Process sections

      for Section of Opt.Sections loop
         Processed_Dirs.Clear;

         --  Process directories in the section

         for D of Section.Directories loop
            Search_Directory
              (D.Value,
               Section,
               Processed_Dirs,
               D.Is_Recursive,
               Compiler_Args);
            Append (Dir_List, Quote (D.Orig) & ',');
         end loop;
      end loop;

      --  Remove the trailing comma in the Source_Dirs template

      if Length (Dir_List) > 0 then
         Head (Dir_List, Length (Dir_List) - 1);
      end if;

      --  Fill the list of languages for which we have found some sources

      for Curs in Lang_Sources_Map.Iterate loop
         declare
            use type Ada.Containers.Count_Type;

            Lang    : constant Language_Type :=
                        Language_Sources_Map.Key (Curs);
            Sources : constant Source.Set.Object :=
                        Language_Sources_Map.Element (Curs);
         begin
            if Sources.Length > 0 then
               Lang_With_Sources.Append (Lang);
               Append (Lang_With_Sources_List, Quote (String (Lang)) & ",");
            end if;
         end;
      end loop;

      --  Remove the trailing comma in the Languages template

      if Length (Lang_With_Sources_List) > 0 then
         Head (Lang_With_Sources_List, Length (Lang_With_Sources_List) - 1);
      end if;

      OS_Lib.Free (Compiler_Args);
   end;

   --
   --  Rewrite the main project
   --

   declare
      use Characters.Conversions;

      function Get_Name_Type (Node : Single_Tok_Node'Class) return Name_Type is
        (Name_Type (To_UTF8 (Node.Text)));
      --  Get the string (as a Name_Type) associated with a single-token node

      function Get_Dir_List return Unbounded_String;
      --  return Source_Dirs list handling --minimal-dirs switches

      ------------------
      -- Get_Dir_List --
      ------------------

      function Get_Dir_List return Unbounded_String is
      begin
         if not Opt.Minimal_Dirs then
            return Dir_List;
         end if;

         declare
            Current_Dir  : constant GPR2.Path_Name.Object :=
                             GPR2.Path_Name.Create_Directory
                               (Filename_Optional
                                  (Ada.Directories.
                                     Current_Directory));

            Dirs         : GPR2.Path_Name.Set.Object;
            --  directories containing sources

            Added_Dirs   : GPR2.Path_Name.Set.Object;
            --  directories already appended to New_Dir_List

            New_Dir_List : Unbounded_String;
            --  value returned if minimal-dirs requested

            procedure Add_Dir
              (Dir : GPR2.Path_Name.Object; Recursive : Boolean);
            --  If Dir part of Dirs, add it to Added_Dirs & New_Dir_List

            procedure Append_Dir
              (Dir          : GPR2.Path_Name.Object;
               Use_New_List : Boolean := False);
            --  If not already done append Dir to Dirs
            --  If Use_New_List is True, use Added_Dirs & New_Dir_List

            -------------
            -- Add_Dir --
            -------------

            procedure Add_Dir
              (Dir : GPR2.Path_Name.Object; Recursive : Boolean)
            is
            begin
               if Dirs.Contains (Dir) then
                  Append_Dir (Dir, True);
               end if;

               --  If recursive mode add subdirs in an order equivalent to
               --  dir/**

               if Recursive then
                  declare
                     use all type Ada.Directories.File_Kind;

                     D_Search : Ada.Directories.Search_Type;
                     D_Entry  : Ada.Directories.Directory_Entry_Type;
                  begin
                     Ada.Directories.Start_Search
                       (D_Search, String (Dir.Value), "",
                        Filter => (Directory     => True,
                                   Ordinary_File => False,
                                   Special_File  => False));

                     while Ada.Directories.More_Entries (D_Search)
                     loop
                        Ada.Directories.Get_Next_Entry
                          (D_Search, D_Entry);

                        case Ada.Directories.Kind (D_Entry) is

                           when Directory =>
                              if Ada.Directories.Simple_Name
                                (D_Entry) not in "." | ".."
                              then
                                 Add_Dir
                                   (GPR2.Path_Name.Create_Directory
                                      (Filename_Optional
                                           (Ada.Directories.Full_Name
                                                (D_Entry))),
                                    True);
                              end if;

                           when others =>
                              raise Program_Error;

                        end case;
                     end loop;

                     Ada.Directories.End_Search (D_Search);
                  end;
               end if;
            end Add_Dir;

            ----------------
            -- Append_Dir --
            ----------------

            procedure Append_Dir
              (Dir          : GPR2.Path_Name.Object;
               Use_New_List : Boolean := False)
            is
               New_Dir : constant GPR2.Path_Name.Object :=
                           GPR2.Path_Name.Relative_Path (Dir, Current_Dir);
            begin
               if Dirs.Contains (New_Dir) then
                  if Use_New_List and then not Added_Dirs.Contains (New_Dir)
                  then
                     declare
                        Dir_Name : constant Filename_Optional :=
                                     New_Dir.Name;
                     begin

                        if not Added_Dirs.Is_Empty then
                           New_Dir_List := New_Dir_List &
                             To_Unbounded_String (", ");
                        end if;

                        --  remove trailing path separator

                        if Dir_Name'Length > 1 then
                           New_Dir_List := New_Dir_List & Quote
                             (String (Dir_Name
                              (Dir_Name'First .. Dir_Name'Last - 1)));
                        end if;
                     end;
                     Added_Dirs.Append (New_Dir);
                  end if;
               elsif not Use_New_List then
                  Dirs.Append (New_Dir);
               end if;
            end Append_Dir;

         begin
            for Curs in Lang_Sources_Map.Iterate loop
               declare
                  Sources : constant Source.Set.Object :=
                              Language_Sources_Map.Element (Curs);
               begin
                  for S_Curs in Sources.Iterate loop
                     Append_Dir (GPR2.Path_Name.Create_Directory
                                 (Filename_Optional
                                    (GPR2.Path_Name.Relative_Path
                                       (Sources (S_Curs).File,
                                          Current_Dir).Name)));
                  end loop;
               end;
            end loop;

            --  Let New_Dir_List minimal-dirs have an equivalent order

            for Section of Opt.Sections loop
               for D of Section.Directories loop
                  Add_Dir (D.Value, D.Is_Recursive);
               end loop;
            end loop;
            return New_Dir_List;
         end;
      end Get_Dir_List;

      Ctx  : constant Analysis_Context := Create_Context;
      Unit : constant Analysis_Unit := Get_From_File (Ctx, Project_Path.Value);
      Hand : Rewriting_Handle := Start_Rewriting (Ctx);

      --  The rewriting handles that we will use:
      --  Note that it may be better to use the Create_* utils if we move
      --  this to a rewriting package (more efficient).

      With_H : constant Node_Rewriting_Handle :=
                 Create_From_Template
                   (Hand, "with " & To_Wide_Wide_String (Quote
                    (Naming_Project_Basename))
                    & ";", (1 .. 0 => <>), With_Decl_Rule);

      Pkg_H : constant Node_Rewriting_Handle :=
                Create_From_Template
                  (Hand, "package Naming renames " & To_Wide_Wide_String
                     (Naming_Project_Name) & ".Naming;",
                   (1 .. 0 => <>), Package_Decl_Rule);

      Lang_H : constant Node_Rewriting_Handle :=
                 Create_From_Template
                   (Hand, "for Languages use (" & To_Wide_Wide_String
                      (To_String (Lang_With_Sources_List)) & ");",
                    (1 .. 0 => <>), Attribute_Decl_Rule);

      Src_Dirs_H : constant Node_Rewriting_Handle :=
                     Create_From_Template
                       (Hand, "for Source_Dirs use (" & To_Wide_Wide_String
                          (To_String (Get_Dir_List)) & ");",
                        (1 .. 0 => <>), Attribute_Decl_Rule);

      Src_List_File_H : constant Node_Rewriting_Handle :=
                          Create_From_Template
                            (Hand, "for Source_List_File use "
                             & To_Wide_Wide_String
                               (Quote (Source_List_File_Basename)) & ";",
                             (1 .. 0 => <>), Attribute_Decl_Rule);

      function Rewrite_Main (N : Gpr_Node'Class) return Visit_Status;
      --  Our rewriting callback for the main project:
      --     - Add a with clause for the naming project, if not already present
      --     - Add the attributes: Source_List_File, Source_Dirs, Languages.
      --     - Add the Naming package declaration which renames the one from
      --       our naming project.

      ------------------
      -- Rewrite_Main --
      ------------------

      function Rewrite_Main (N : Gpr_Node'Class) return Visit_Status is
      begin
         case Kind (N) is

            when Gpr_With_Decl_List =>
               if not Tree.Root_Project.Has_Imports or else
                 not (for some Imported of Tree.Root_Project.Imports =>
                        String (Imported.Path_Name.Base_Name) =
                          String (Naming_Project_Path.Base_Name))
                   --  ???
                   --  Base_Name comparisons should not be case insensitive.
                   --  We must cast to String to work around this.
               then
                  declare
                     Children_Handle : constant Node_Rewriting_Handle :=
                                         Handle (N);
                  begin
                     Insert_Child (Children_Handle, 1, With_H);
                  end;
               end if;
               return Into;

            when Gpr_Project_Declaration =>
               declare
                  Children        : constant Gpr_Node_List :=
                                      F_Decls (As_Project_Declaration (N));
                  Children_Handle : constant Node_Rewriting_Handle :=
                                      Handle (Children);

                  Child     : Gpr_Node;
                  In_Bounds : Boolean;

               begin
                  for I in reverse 1 .. Children_Count (Children_Handle) loop
                     Get_Child (F_Decls (As_Project_Declaration (N)),
                                I, In_Bounds, Child);

                     if not Child.Is_Null then
                        if Kind (Child) = Gpr_Attribute_Decl then
                           declare
                              Attr_Name : constant Attribute_Id :=
                                +Get_Name_Type
                                  (F_Attr_Name (Child.As_Attribute_Decl).
                                     As_Single_Tok_Node);
                           begin
                              if Attr_Name = PRA.Languages.Attr
                                or else Attr_Name = PRA.Source_Dirs.Attr
                                or else Attr_Name = PRA.Source_List_File.Attr
                                or else Attr_Name = PRA.Source_Files.Attr
                              then
                                 Remove_Child (Children_Handle, I);
                              end if;
                           end;

                        elsif Kind (Child) = Gpr_Package_Decl then
                           declare
                              Pack_Name : constant Package_Id := +Get_Name_Type
                                (F_Pkg_Name (Child.As_Package_Decl).
                                   As_Single_Tok_Node);
                           begin
                              if Pack_Name = PRP.Naming then
                                 Remove_Child (Children_Handle, I);
                              end if;
                           end;
                        end if;
                     end if;
                  end loop;

                  Insert_Child (Children_Handle, 1, Src_List_File_H);
                  Insert_Child (Children_Handle, 2, Src_Dirs_H);
                  Insert_Child (Children_Handle, 3, Lang_H);
                  Insert_Child (Children_Handle, 4, Pkg_H);
               end;
               return Stop;

            when others => return Into;

         end case;
      end Rewrite_Main;

   begin
      Traverse (Root (Unit), Rewrite_Main'Access);

      declare
         Result : constant Apply_Result := Apply (Hand);
      begin
         if not Result.Success then
            for D of Result.Diagnostics loop
               Put_Line (Format_GNU_Diagnostic (Result.Unit, D), Low);
            end loop;
            raise GPRname_Exception with "rewriting of main project failed";
         end if;
      end;

      --  Do a backup if required and there is something to back up (i.e. we
      --  didn't create the project from scratch)

      if not Opt.No_Backup and then not From_Scratch then
         --  Find the files with name <orig_proj_filename>.saved_<0..N> and
         --  use <orig_proj_filename>.saved_<N+1> as backup file.

         declare
            use Regpat;

            Bkp_Reg     : constant String :=
                            "^" & String (Project_Path.Simple_Name)
                          & "\.saved_(\d+)$";
            Bkp_Matcher : constant Pattern_Matcher := Compile (Bkp_Reg);
            Matches     : Match_Array (0 .. 1);

            Str  : String (1 .. 2_000);
            Last : Natural;
            Dir  : Directory_Operations.Dir_Type;
            File : GPR2.Path_Name.Object;

            Bkp_Number     : Natural;
            New_Bkp_Number : Natural := 0;

            Success : Boolean;

         begin
            begin
               Directory_Operations.Open (Dir, Project_Path.Dir_Name);
            exception
               when Directory_Operations.Directory_Error =>
                  raise GPRname_Exception with
                    "cannot open directory " & String (Project_Path.Dir_Name);
            end;

            loop
               Directory_Operations.Read (Dir, Str, Last);
               exit when Last = 0;

               File := GPR2.Path_Name.Create_File
                 (Filename_Type (Str (1 .. Last)),
                  Filename_Optional (Project_Path.Dir_Name));

               if File.Exists then
                  Match (Bkp_Matcher, Str (1 .. Last), Matches);

                  if Matches (0) /= No_Match then
                     Bkp_Number := Natural'Value
                       (Str (Matches (1).First .. Matches (1).Last));
                     if Bkp_Number >= New_Bkp_Number then
                        New_Bkp_Number := Bkp_Number + 1;
                     end if;
                  end if;
               end if;
            end loop;

            --  We have found the correct <N> to use, now copy the original
            --  project file to the backup.

            declare
               Bkp_Filename : constant String :=
                                String (Project_Path.Value)
                                & ".saved_" & Int_Image (New_Bkp_Number);
            begin
               Put_Line
                 ("copying file " & String (Project_Path.Value)
                  & " to file " & Bkp_Filename, Low);

               OS_Lib.Copy_File
                 (String (Project_Path.Value),
                  Bkp_Filename,
                  Success);
            end;

            Directory_Operations.Close (Dir);

            if not Success then
               raise GPRname_Exception with
                 "could not copy project file for backup";
            end if;
         end;
      end if;

      --  Finally, rewrite the project file

      declare
         File : Stream_IO.File_Type;
         PP   : GPR2.Project.Pretty_Printer.Object;
      begin
         PP.Pretty_Print (Analysis_Unit => Unit);

         Stream_IO.Create
           (File, Stream_IO.Out_File, String (Project_Path.Value));
         String'Write (Stream_IO.Stream (File), PP.Result);

         Stream_IO.Close (File);
      end;

   end;

   --
   --  Create the naming project and the source list file
   --

   --  (this code will change once we have the full pretty-printer)

   declare
      use GPRname.Unit;

      Naming_Project_Buffer : Unbounded_String;
      Implementation_Except : Unbounded_String;
      File_Src_List         : Text_IO.File_Type;

      Listed_Source_Files : Source.Set.Object;
   begin
      Text_IO.Create
        (File_Src_List, Text_IO.Out_File, String (Source_List_File_Path.Name));

      Naming_Project_Buffer := To_Unbounded_String
        ("abstract project " & Naming_Project_Name & " is "
         & "package Naming is ");

      --  Sources for none Ada languages

      for Curs in Lang_Sources_Map.Iterate loop
         declare
            use type Source.Set.Cursor;

            Lang    : constant Language_Type :=
                        Language_Sources_Map.Key (Curs);
            Sources : constant Source.Set.Object :=
                        Language_Sources_Map.Element (Curs);

         begin
            if Lang /= Ada_Lang then
               Implementation_Except :=
                 To_Unbounded_String
                   ("for Implementation_Exceptions (" & Quote (String (Lang))
                    & ") use (");

               for S_Curs in Sources.Iterate loop
                  Append
                    (Implementation_Except,
                     Quote (String (Sources (S_Curs).File.Simple_Name))
                     & (if S_Curs = Sources.Last then ");" else ", "));

                  Listed_Source_Files.Insert
                    (New_Item => Source.Create
                       (File       => Sources (S_Curs).File,
                        Language   => "",
                        Unit_Based => False)
                    );

                  --  Write the source to the source list file

                  Text_IO.Put_Line
                    (File_Src_List,
                     String (Sources (S_Curs).File.Simple_Name));
               end loop;
            end if;
         end;
      end loop;

      --  Spec/Body attributes for Ada sources

      if Lang_Sources_Map.Contains (Ada_Lang) then
         for S of Lang_Sources_Map (Ada_Lang) loop
            for U of S.Units loop
               Append
                 (Naming_Project_Buffer,
                  "for " & (if U.Kind = K_Spec then "Spec" else "Body")
                  & " (" & Quote (String (U.Name)) & ") use "
                  & Quote (String (S.File.Simple_Name))
                  & (if U.Index_In_Source > 0
                     then " at " & Int_Image (U.Index_In_Source) & ";"
                     else ";"));
            end loop;

            Listed_Source_Files.Insert
              (New_Item => Source.Create
                 (File       => S.File,
                  Language   => "",
                  Unit_Based => False)
              );

            --  Write the source to the source list file

            Text_IO.Put_Line (File_Src_List, String (S.File.Simple_Name));
         end loop;
      end if;

      --  Adding explicitly added files to the source list if not present yet.
      --  If the file is already present, verify if the file is not hidden
      --  by another file found from Source_Dirs search.

      for Section of Opt.Sections loop
         for File of Section.Files loop
            if (for some Listed_File of Listed_Source_Files
                => Listed_File.File.Value = File.File.Value)
            then
               for Listed_File of Listed_Source_Files loop
                  if File.File.Value = Listed_File.File.Value then
                     exit;
                  elsif File.File.Simple_Name = Listed_File.File.Simple_Name
                  then
                     Text_IO.Put_Line
                       (Item => "warning: " & File.File.Value
                        & " hidden by "
                        & Listed_File.File.Value
                        & " found through source dirs");
                     exit;
                  end if;
               end loop;
            else
               Text_IO.Put_Line (File_Src_List,
                                 String (File.File.Simple_Name));
            end if;
         end loop;
      end loop;

      Append (Naming_Project_Buffer, Implementation_Except);
      Append
        (Naming_Project_Buffer,
         "end Naming; end " & Naming_Project_Name & ";");

      --  We are done with the source list file

      Text_IO.Close (File_Src_List);

      --  Parse the naming project buffer and pretty-print the resulting AST
      --  to the actual naming project file.

      declare
         File : Stream_IO.File_Type;
         PP   : GPR2.Project.Pretty_Printer.Object;

         Ctx  : constant Analysis_Context := Create_Context;
         Unit : constant Analysis_Unit :=
                  Get_From_Buffer
                    (Context  => Ctx,
                     Filename => "<buffer>",
                     Charset  => "ASCII",
                     Buffer   => To_String (Naming_Project_Buffer),
                     Rule     => Compilation_Unit_Rule);

      begin
         if Has_Diagnostics (Unit) then
            for D of Diagnostics (Unit) loop
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                    (D.Message));
            end loop;

            raise GPRname_Exception with
              "could not create naming project file "
              & Naming_Project_Path.Value;
         end if;

         PP.Pretty_Print (Analysis_Unit => Unit);

         Stream_IO.Create
           (File, Stream_IO.Out_File, String (Naming_Project_Path.Name));
         String'Write (Stream_IO.Stream (File), PP.Result);
         Stream_IO.Close (File);
      end;

   exception
      when others =>
         raise GPRname_Exception with
           "could not create naming project file " & Naming_Project_Path.Value;
   end;
end GPRname.Process;
