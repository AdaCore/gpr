--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Project.Registry;
with GPR2.Project.Tree;

package body GPR2.Options is

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Self   : in out Object;
      Switch : Option;
      Param  : String := "";
      Index  : String := "") is
   begin
      case Switch is
         when AP =>
            Self.Search_Paths.Append
              ((GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param))));

         when Autoconf =>
            Self.Config_Project :=
              GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (Param));
            Self.Create_Missing_Config := True;

         when Config =>
            Self.Config_Project :=
              GPR2.Path_Name.Create_File
                (GPR2.Filename_Type (Param));
            Self.Create_Missing_Config := False;

         when Db =>
            declare
               KB_Norm : constant String :=
                           GNAT.OS_Lib.Normalize_Pathname (Param);
               KB_Path : GPR2.Path_Name.Object;
            begin
               if GNAT.OS_Lib.Is_Directory (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_Directory
                      (GPR2.Filename_Type (KB_Norm));

               elsif GNAT.OS_Lib.Is_Regular_File (KB_Norm) then
                  KB_Path :=
                    GPR2.Path_Name.Create_File (GPR2.Filename_Type (KB_Norm));

               else
                  raise Usage_Error with
                    KB_Norm & " is not a file or directory";
               end if;

               Self.KB_Locations.Append (KB_Path);
            end;

         when Db_Minus =>
            Self.Skip_Default_KB := True;

         when Implicit_With =>
            Self.Implicit_With.Append
              (GPR2.Path_Name.Create_File
                 (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param))));

         when No_Project =>
            Self.No_Project := True;

         when P =>
            if not Self.Project_File.Is_Defined then
               Self.Project_File :=
                 GPR2.Path_Name.Create_File
                   (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param)),
                    GPR2.Path_Name.No_Resolution);
            else
               if Self.Prj_Got_On_Extra_Arg then
                  raise GPR2.Options.Usage_Error with
                    "cannot have -P<prj> and <prj> on the same command line";

               else
                  raise GPR2.Options.Usage_Error with
                    """-P"", project already """
                    & (if Self.Project_File.Has_Dir_Name
                       then Self.Project_File.String_Value
                       else String (Self.Project_File.Name)) & '"';
               end if;
            end if;

         when Print_GPR_Registry =>
            Self.Print_GPR_Registry := True;

         when Relocate_Build_Tree =>
            Self.Build_Path :=
              GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

         when Root_Dir =>
            Self.Root_Path :=
              GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

         when RTS =>
            declare
               Lang_Idx : constant GPR2.Language_Id :=
                            (if Index'Length > 0
                             then GPR2."+" (GPR2.Name_Type (Index))
                             else GPR2.No_Language);
            begin
               if Lang_Idx = GPR2.No_Language then
                  Self.RTS_Map.Include (GPR2.Ada_Language, Param);
               else
                  Self.RTS_Map.Include (Lang_Idx, Param);
               end if;
            end;

         when Src_Subdirs =>
            Self.Src_Subdirs := To_Unbounded_String (Param);

         when Subdirs =>
            Self.Subdirs := To_Unbounded_String (Param);

         when Target =>
            Self.Target := To_Unbounded_String (Param);

         when Resolve_Links =>
            Self.Resolve_Links := True;

         when Unchecked_Shared_Lib_Imports =>
            Self.Unchecked_Shared_Lib := True;

         when X =>
            declare
               Idx : constant Natural := Strings.Fixed.Index (Param, "=");
            begin
               if Idx = 0 then
                  raise Usage_Error with
                    "Can't split '" & Param & "' to name and value";
               end if;

               Self.Context.Include
                 (GPR2.Name_Type (Param (Param'First .. Idx - 1)),
                  Param (Idx + 1 .. Param'Last));
            end;
      end case;
   end Add_Switch;

   -------------------------------
   -- Check_For_Default_Project --
   -------------------------------

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object
   is
      use Directories;
      Default_Name : constant String :=
                       (if Directory = ""
                        then "default.gpr"
                       else Directory
                        & GNAT.OS_Lib.Directory_Separator
                        & "default.gpr");
      Search       : Search_Type;
      Item         : Directory_Entry_Type;

   begin
      if Exists (Default_Name)
        and then Kind (Default_Name) = Ordinary_File
      then
         return Path_Name.Create_File (Filename_Type (Default_Name));
      end if;

      Start_Search
        (Search,
         (if Directory = "" then "." else Directory),
         "*.gpr",
         (Ordinary_File => True, others => False));

      if More_Entries (Search) then
         Get_Next_Entry (Search, Item);

         if not More_Entries (Search) then
            --  Only one project in current directory can be default one

            return Path_Name.Create_File (Filename_Type (Full_Name (Item)));
         end if;
      end if;

      return Path_Name.Undefined;
   end Check_For_Default_Project;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Self                   : in out Object;
      Allow_Implicit_Project : Boolean := True;
      Quiet                  : Boolean := False;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment) is
   begin
      Self.Environment := Environment;

      if Self.Project_File.Is_Defined
        and then not Self.Project_File.Has_Dir_Name
        and then Self.Root_Path.Is_Defined
      then
         --  We have to resolve the project directory without target specific
         --  directories in search path because --root-dir exists in command
         --  line parameters.

         declare
            Search_Paths : Path_Name.Set.Object :=
                             GPR2.Project.Default_Search_Paths
                               (True, Self.Environment);
         begin
            for P of Self.Search_Paths loop
               Search_Paths.Prepend (P);
            end loop;

            Self.Project_File := GPR2.Project.Create
              (Self.Project_File.Name, Self.Resolve_Links, Search_Paths);
         end;
      end if;

      Self.Project_Is_Defined := Self.Project_File.Is_Defined;

      if not Self.Project_File.Is_Defined then
         if Self.No_Project then
            Self.Project_Base := GPR2.Path_Name.Create_Directory
              (GPR2.Filename_Type (Directories.Current_Directory));

         elsif Allow_Implicit_Project then
            Self.Project_File := Check_For_Default_Project;

            if not Self.Project_File.Is_Defined then
               Self.Project_Base :=
                 GPR2.Path_Name.Create_Directory
                   (GPR2.Filename_Type (Directories.Current_Directory));

               if not Quiet then
                  Ada.Text_IO.Put_Line
                    ("use implicit project in " &
                       Self.Project_Base.String_Value);
               end if;

            elsif not Quiet then
               Ada.Text_IO.Put_Line
                 ("using project file " & Self.Project_File.String_Value);
            end if;
         end if;

      elsif Self.No_Project then
         raise Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      if not Self.Build_Path.Is_Defined
        and then Self.Root_Path.Is_Defined
      then
         raise Usage_Error with
           "cannot use --root-dir without --relocate-build-tree option";
      end if;

      Self.Finalized := True;
   end Finalize;

   ------------------
   -- On_Extra_Arg --
   ------------------

   function On_Extra_Arg (Self : in out Object; Arg : String) return Boolean is
   begin
      if GNATCOLL.Utils.Ends_With
        (GPR2.Path_Name.To_OS_Case (Filename_Optional (Arg)),
         String (GPR2.Project.Project_File_Extension))
      then
         if not Self.Project_File.Is_Defined then
            Self.Add_Switch
              (Switch => GPR2.Options.P,
               Param  => Arg,
               Index  => "");
            Self.Prj_Got_On_Extra_Arg := True;

            return True;

         elsif not Self.Prj_Got_On_Extra_Arg then
            raise GPR2.Options.Usage_Error with
              "cannot have -P<prj> and <prj> on the same command line";

         else
            raise GPR2.Options.Usage_Error with
              "cannot have multiple <proj> on the same command line";
         end if;

      else
         return False;
      end if;
   end On_Extra_Arg;

   ------------------------
   -- Print_GPR_Registry --
   ------------------------

   procedure Print_GPR_Registry
     (Self   : Object;
      Format : GPR2.Project.Registry.Exchange.Export_Format :=
                 GPR2.Project.Registry.Exchange.K_JSON_COMPACT) is
   begin
      if Self.Print_GPR_Registry then
         GPR2.Project.Registry.Exchange.Export (Format => Format);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
   end Print_GPR_Registry;

   -----------------------------------
   -- Register_Project_Search_Paths --
   -----------------------------------

   procedure Register_Project_Search_Paths
     (Self : Object;
      Tree : in out GPR2.Project.Tree.Object) is
   begin
      for Path of Self.Search_Paths loop
         Tree.Register_Project_Search_Path (Path);
      end loop;
   end Register_Project_Search_Paths;

end GPR2.Options;
