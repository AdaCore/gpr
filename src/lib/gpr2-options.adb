--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GNAT.OS_Lib;

with GPR2.Project.Registry;

package body GPR2.Options is

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Self     : in out Object;
      Switch   : Option;
      Param    : String := "";
      Index    : String := "";
      Override : Boolean := False) is
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
            if not Self.Project_File.Is_Defined or else Override then
               if GNAT.OS_Lib.Is_Directory (Param) then
                  Self.Project_File :=
                    GPR2.Path_Name.Create_Directory
                      (GPR2.Filename_Type (Param),
                        GPR2.Path_Name.No_Resolution);
               else
                  Self.Project_File :=
                    GPR2.Path_Name.Create_File
                      (GPR2.Project.Ensure_Extension
                         (GPR2.Filename_Type (Param)),
                       GPR2.Path_Name.No_Resolution);
               end if;
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

         when X =>
            declare
               Idx : constant Natural := Strings.Fixed.Index (Param, "=");
            begin
               if Idx = 0 then
                  raise Usage_Error with
                    "Can't split '" & Param & "' to name and value";
               end if;

               Self.Context.Include
                 (GPR2.External_Name_Type (Param (Param'First .. Idx - 1)),
                  Param (Idx + 1 .. Param'Last));
            end;
      end case;
   end Add_Switch;

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

end GPR2.Options;
