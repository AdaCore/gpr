--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNAT.OS_Lib;
with GNAT.String_Split;

with GPR2.Project.Configuration; use GPR2.Project.Configuration;
with GPR2.Source_Reference;

package body GPR2.Configuration_Internal is

   -------------------------
   -- Resolve_Runtime_Dir --
   -------------------------

   procedure Resolve_Runtime_Dir
     (Descriptor   : in out GPR2.Project.Configuration.Description;
      Project_Path : GPR2.Path_Name.Object;
      Environment  : GPR2.Environment.Object;
      Message      : out GPR2.Message.Object)
   is

      function Check_Runtime_Dir (Dir : Path_Name.Object) return Boolean;
      --  Checks if Dir can be a runtime directory

      function Locate_Runtime
        (Dir : Filename_Optional; Path : String) return Path_Name.Object;
      --  Returns runtime DIR directory resolved against Path or Undefined
      --  if nothing is found.

      -----------------------
      -- Check_Runtime_Dir --
      -----------------------

      function Check_Runtime_Dir (Dir : Path_Name.Object) return Boolean is
         Adalib_Dir     : constant Path_Name.Object :=
                            Path_Name.Create_Directory ("adalib", Dir.Value);
         Adainclude_Dir : constant Path_Name.Object :=
                            Path_Name.Create_Directory
                              ("adainclude", Dir.Value);
         AOP_File       : constant Path_Name.Object :=
                            Path_Name.Create_File
                              ("ada_object_path", Dir.Value);
         ASP_File       : constant Path_Name.Object :=
                            Path_Name.Create_File
                              ("ada_source_path", Dir.Value);
      begin
         return (Adalib_Dir.Exists or else AOP_File.Exists)
           and then (Adainclude_Dir.Exists or else ASP_File.Exists);
      end Check_Runtime_Dir;

      --------------------
      -- Locate_Runtime --
      --------------------

      function Locate_Runtime
        (Dir : Filename_Optional; Path : String) return Path_Name.Object
      is
         use GNAT;

         Paths       : GNAT.String_Split.Slice_Set;
         Runtime_Dir : Path_Name.Object;
      begin
         String_Split.Create (Paths, Path, (1 => OS_Lib.Path_Separator));
         for J in 1 .. String_Split.Slice_Count (Paths) loop
            Runtime_Dir := Path_Name.Create_Directory
              (Dir,
               Filename_Optional (String_Split.Slice (Paths, J)));

            if Runtime_Dir.Exists then
               return Runtime_Dir;
            end if;
         end loop;

         return Path_Name.Undefined;
      end Locate_Runtime;

   begin
      --  Ada runtime has a special 3 step lookup:
      --  1) check <runtime> subdir relatively to root project location
      --  2) check <runtime> subdir relatively to GPR_RUNTIME_PATH value
      --  3) pass it as is to configuration creation.
      --
      --  If step 1 or 2 results in a valid runtime dir, pass full path
      --  to it to configuration creation.
      --  If on step 2 corresponding directory is found, but it does not
      --  have runtime features, configuration is abandoned.

      Message := GPR2.Message.Undefined;

      if Language (Descriptor) = Ada_Language
        and then Runtime (Descriptor) /= No_Name
        and then not GNAT.OS_Lib.Is_Absolute_Path
                       (String (Runtime (Descriptor)))
      then
         declare
            Runtime_Dir : Path_Name.Object;
         begin
            if Project_Path.Is_Defined then
               Runtime_Dir :=
                 Path_Name.Create_Directory
                   (Filename_Optional (Runtime (Descriptor)),
                    Filename_Optional (Project_Path.Dir_Name));

               if Runtime_Dir.Exists
                 and then Check_Runtime_Dir (Runtime_Dir)
               then
                  Descriptor :=
                    Project.Configuration.Create
                      (Language => Language (Descriptor),
                       Version  => Version (Descriptor),
                       Runtime  => Optional_Name_Type (Runtime_Dir.Value),
                       Path     => Path (Descriptor),
                       Name     => Name (Descriptor));
               end if;
            end if;

            if Environment.Exists ("GPR_RUNTIME_PATH") then
               Runtime_Dir := Locate_Runtime
                 (Filename_Optional (Runtime (Descriptor)),
                  Environment.Value ("GPR_RUNTIME_PATH"));

               if Runtime_Dir.Is_Defined and then Runtime_Dir.Exists then
                  if Check_Runtime_Dir (Runtime_Dir) then
                     Descriptor :=
                       Project.Configuration.Create
                         (Language => Language (Descriptor),
                          Version  => Version (Descriptor),
                          Runtime  =>
                            Optional_Name_Type (Runtime_Dir.Value),
                          Path     => Path (Descriptor),
                          Name     => Name (Descriptor));

                  else
                     Message :=
                       GPR2.Message.Create
                         (GPR2.Message.Error,
                          "invalid runtime directory " &
                            Runtime_Dir.String_Value,
                          Sloc =>
                            (if Project_Path.Is_Defined
                             then Source_Reference.Create
                               (Project_Path.Value, 0, 0)
                             else Source_Reference.Undefined));
                  end if;
               end if;
            end if;
         end;
      end if;
   end Resolve_Runtime_Dir;

end GPR2.Configuration_Internal;
