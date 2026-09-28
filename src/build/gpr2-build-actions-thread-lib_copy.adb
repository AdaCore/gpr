--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Source_Files;
with GPR2.Build.Source;
with GPR2.Build.Tree_Db;

package body GPR2.Build.Actions.Thread.Lib_Copy is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.THREAD.LIB_COPY", GNATCOLL.Traces.Off);

   -------------------------
   -- Add_Interface_Unit --
   -------------------------

   function Add_Interface_Unit
     (Self            : in out Object;
      CU              : GPR2.Build.Compilation_Unit.Object;
      Dependency_File : Path_Name.Object) return Boolean
   is
      use GPR2.Containers;
      Ali_Artifact : constant GPR2.Build.Artifacts.Files.Object :=
                       GPR2.Build.Artifacts.Files.Create (Dependency_File);
      Ign          : Boolean;
   begin
      Self.Units.Include (CU.Name, CU);

      declare
         C : Filename_Type_Set.Cursor :=
           Self.Alis.Find (Ali_Artifact.Path.Value);
      begin
         if not Filename_Type_Set.Has_Element (C) then
            Self.Alis.Insert (Dependency_File.Value, C, Ign);

            if Self.Tree /= null then
               Self.Tree.Add_Input (Self.UID, Ali_Artifact);

               if not Self.Tree.Add_Output
                 (Self.UID,
                  Artifacts.Files.Create
                    (Self.Ali_Dir.Compose (Ali_Artifact.Path.Simple_Name)))
               then
                  return False;
               end if;
            end if;
         end if;
      end;

      return True;
   end Add_Interface_Unit;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      for Artifact of Self.Tree.Inputs (Self.UID) loop
         if not Self.Signature.Add_Input (Artifact, Check_Checksums)
         then
            return;
         end if;
      end loop;

      for Artifact of Self.Tree.Outputs (Self.UID) loop
         if not Self.Signature.Add_Output (Artifact, Check_Checksums)
         then
            return;
         end if;
      end loop;
   end Compute_Signature;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Self   : in out Object;
      Stdout : in out Unbounded_String;
      Stderr : in out Unbounded_String) return Integer
   is
      pragma Unreferenced (Stdout);

      function Copy_SL (From, Dest : Filename_Type) return Integer;
      --  Install an Ali file with amended SL flag added to it

      procedure Report_Error (Text : String);
      --  Append an error to Stderr. This subprogram runs in a dedicated task,
      --  so it must not use the tree's reporter, which is owned by the main
      --  task: just like a process action, it reports through its standard
      --  error output, which the scheduler displays when the action is
      --  collected.

      -------------
      -- Copy_SL --
      -------------

      function Copy_SL (From, Dest : Filename_Type) return Integer
       is
         use GNATCOLL.OS;
         use type GNATCOLL.OS.FS.File_Descriptor;

         Attrs : constant Stat.File_Attributes := Stat.Stat (String (From));

         Last : constant Integer :=
           Integer
             (Long_Long_Integer'Min
                (64 * 1024, Stat.Length (Attrs)));
         --  64k length: more than enough to find the P line but
         --  not too much footprint on the stack to copy the whole
         --  ALI file if very large.

         Offset : Long_Long_Integer := 0;
            --  Current offset, used to copy the whole file

         Buffer : String (1 .. Last);
            --  Some ALI files can be pretty large, for example
            --  in libadalang the generated source comes with a
            --  24MB ali file. We cannot use strings here, so need
            --  to move to a more generic solution.

         Length : Natural;
         Idx    : Natural := Buffer'First;
         Ign    : Natural with Unreferenced;
         Found  : Boolean := False;
         Input  : GNATCOLL.OS.FS.File_Descriptor;
         Output : GNATCOLL.OS.FS.File_Descriptor;

      begin
         Input := FS.Open (String (From), FS.Read_Mode);

         if Input = FS.Invalid_FD then
            Report_Error
              ("could not read the ali file """
               & Path_Name.Simple_Name (String (From))
               & '"');

            return 2;
         end if;

         Output := FS.Open (String (Dest), FS.Write_Mode);

         if Output = FS.Invalid_FD then
            Report_Error
              ("could not create the ali file """
               & Path_Name.Simple_Name (String (Dest))
               & '"');
            FS.Close (Input);

            return 3;
         end if;

         if Traces.Is_Active then
            Traces.Trace
              ("Installing """
               & String (From)
               & """ to """
               & Self.Ali_Dir.String_Value
               & """ as library interface for "
               & (-Self.Lib_Name));
         end if;

         Offset := Long_Long_Integer (FS.Read (Input, Buffer));

         Search_Loop : while Idx < Buffer'Last loop
            --  Check end of line to retrieve the header char

            while Buffer (Idx) in ASCII.CR | ASCII.LF loop
               Idx := Idx + 1;

               exit when Idx > Buffer'Last;

               if Buffer (Idx) = 'P' then
                     --  Check if it's followed by a space or a new
                     --  line.

                  if Idx = Buffer'Last
                    or else
                      Buffer (Idx + 1) in ' ' | ASCII.CR | ASCII.LF
                  then
                        --  we have the P line
                     Found := True;
                     FS.Write (Output, String (Buffer (1 .. Idx)));
                     FS.Write (Output, " SL");

                        --  Write the rest of the ALI file

                     FS.Write
                       (Output, Buffer (Idx + 1 .. Buffer'Last));

                     while Offset < Stat.Length (Attrs) loop
                        Length := FS.Read (Input, Buffer);
                        Offset :=
                          Offset + Long_Long_Integer (Length);
                        FS.Write (Output, Buffer (1 .. Length));
                     end loop;

                     exit Search_Loop;
                  end if;
               end if;
            end loop;

            Idx := Idx + 1;
         end loop Search_Loop;

         FS.Close (Input);
         FS.Close (Output);

         if not Found then
            Report_Error
              ("incorrectly formatted ali file """
               & String (From)
               & '"');

            if not GNATCOLL.OS.FSUtil.Copy_File
                     (String (From), String (Dest))
            then
               Report_Error
                 ("could not copy ali file "
                  & Path_Name.Simple_Name (String (From))
                  & " to the library directory");

               return 4;
            end if;
         end if;

         return 0;
      end Copy_SL;

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Text : String) is
      begin
         if Length (Stderr) > 0 then
            Append (Stderr, ASCII.LF);
         end if;

         Append (Stderr, "error: " & Text);
      end Report_Error;

      Status : Integer;

   begin
      --  Note: this subprogram is executed in a dedicated task, so it must
      --  query neither the tree database nor the project tree. Self.Copies
      --  holds everything it needs.

      for Ali of Self.Alis loop
         declare
            Dest : constant Filename_Type :=
              Self.Ali_Dir.Compose (GPR2.Path_Name.Simple_Name (Ali)).Value;
         begin
            if not Self.Is_Standalone then
               if Traces.Is_Active then
                  Traces.Trace
                    ("Copying """
                     & String (Ali)
                     & """ to """
                     & Self.Ali_Dir.String_Value
                     & '"');
               end if;

               if not GNATCOLL.OS.FSUtil.Copy_File
                 (String (Ali), String (Dest))
               then
                  Report_Error
                    ("could not copy ali file "
                     & GPR2.Path_Name.Simple_Name (String (Ali))
                     & " to the library directory");

                  return 1;
               end if;
            else
               Status := Copy_SL (Ali, Dest);

               if Status /= 0 then
                  return Status;
               end if;
            end if;
         end;
      end loop;

      for Src of Self.Srcs loop
         declare
            Dest : constant Filename_Type :=
              Self.Src_Dir.Compose (GPR2.Path_Name.Simple_Name (Src)).Value;
         begin
            if Traces.Is_Active then
               Traces.Trace
                 ("Copying """
                  & String (Src)
                  & """ to """
                  & Self.Src_Dir.String_Value
                  & '"');
            end if;

            if not GNATCOLL.OS.FSUtil.Copy_File (String (Src), String (Dest))
            then
               Report_Error
                 ("could not copy source file "
                  & GPR2.Path_Name.Simple_Name (String (Src))
                  & " to the library src directory");

               return 3;
            end if;
         end;
      end loop;

      return 0;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Object; Ctxt : GPR2.Project.View.Object)
   is
      Ign : Boolean with Unreferenced;
   begin
      Self.Ctxt          := Ctxt;
      Self.Ali_Dir       := Ctxt.Library_Ali_Directory;
      Self.Is_Standalone := Ctxt.Is_Library_Standalone;
      Self.Lib_Name      := To_Unbounded_String (String (Ctxt.Name));

      if Ctxt.Has_Library_Src_Directory then
         Self.Src_Dir := Ctxt.Library_Src_Directory;

         --  The non-Ada interface sources are known from the view alone: the
         --  Ada ones come with their compile action, see Add_Interface_Unit.

         for C in Ctxt.Interface_Sources.Iterate loop
            declare
               Path : constant Filename_Type :=
                 GPR2.Containers.Source_Path_To_Sloc.Key (C);
               Src  : constant GPR2.Build.Source.Object :=
                 Ctxt.Visible_Source (Path);
            begin
               --  Ada sources are derived from the units in the interfact so
               --  are handled later in the process.

               if Src.Language /= Ada_Language then
                  Self.Srcs.Include (Src.Path_Name.Value);
               end if;
            end;
         end loop;
      end if;
   end Initialize;

---------------------
-- Needed_For_View --
---------------------

   function Needed_For_View (Ctxt : GPR2.Project.View.Object) return Boolean is
   begin
      if Ctxt.Is_Library_Standalone then
         return not Ctxt.Interface_Closure.Is_Empty;
      end if;

      return not Ctxt.Own_Units.Is_Empty
        or else
          (Ctxt.Has_Library_Src_Directory
           and then not Ctxt.Interface_Sources.Is_Empty);
   end Needed_For_View;

-------------------
-- On_Ali_Parsed --
-------------------

   function On_Ali_Parsed
     (Self : in out Object; Comp : Process.Compile.Ada.Object) return Boolean
   is
      function Add_Part (Part : Compilation_Unit.Unit_Location) return Boolean;

      --------------
      -- Add_Part --
      --------------

      function Add_Part (Part : Compilation_Unit.Unit_Location) return Boolean
      is
         Src : Path_Name.Object renames Part.Source;
         From : constant Artifacts.Source_Files.Object :=
           Artifacts.Source_Files.Create (Src);
         Dest : constant Artifacts.Source_Files.Object :=
           Artifacts.Source_Files.Create
             (Self.Src_Dir.Compose (Src.Simple_Name));
      begin
         Self.Tree.Add_Input (Self.UID, From);
         Self.Srcs.Include (Src.Value);

         if not Self.Tree.Add_Output (Self.UID, Dest) then
            return False;
         end if;

         return True;
      end Add_Part;

      CU : Compilation_Unit.Object renames Comp.Unit;

   begin
      if not Self.Src_Dir.Is_Defined then
         return True;
      end if;

      if CU.Has_Part (S_Spec) then
         if not Add_Part (CU.Spec) then
            return False;
         end if;
      end if;

      if not CU.Has_Part (S_Spec)
        or else CU.Is_Body_Needed_For_SAL
      then
         if not Add_Part (CU.Main_Body) then
            return False;
         end if;

         for S of CU.Separates loop
            if not Add_Part (S) then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end On_Ali_Parsed;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      --  Add the non-Ada sources as inputs of Self in the tree
      for Src of Self.Srcs loop
         Db.Add_Input (Self.UID, Artifacts.Source_Files.Create (Src));

         if not Db.Add_Output
                  (Self.UID,
                   Artifacts.Files.Create
                     (Self.Src_Dir.Compose (Path_Name.Simple_Name (Src))))
         then
            return False;
         end if;
      end loop;

      for Ali of Self.Alis loop
         Db.Add_Input (Self.UID, Artifacts.Source_Files.Create (Ali));

         if not Db.Add_Output
                  (Self.UID,
                   Artifacts.Files.Create
                     (Self.Ali_Dir.Compose (Path_Name.Simple_Name (Ali))))
         then
            return False;
         end if;
      end loop;

      return True;
   end On_Tree_Insertion;

   -------------------
   -- Pre_Execution --
   -------------------

   overriding
   function Pre_Execution (Self : in out Object) return Boolean is
      To_Check : Containers.Filename_Set;

   begin
      --  Compare the actual inputs against our Alis to check if some path
      --  changed, in particular in the context of extending projects
      --  overloading.

      for Ali of Self.Alis loop
         if not Self.Tree.Has_Artifact (Artifacts.Files.Create (Ali)) then
            To_Check.Include (Ali);
         end if;
      end loop;

      for Ali of To_Check loop
         declare
            Ali_Simple_Name : constant Filename_Type :=
              Path_Name.Simple_Name (Ali);
         begin
            Input_Loop :
            for Input of Self.Tree.Inputs (Self.UID) loop
               if Input in Artifacts.Files.Object'Class then
                  declare
                     Path : constant Path_Name.Object :=
                       Artifacts.Files.Object'Class (Input).Path;
                  begin
                     if Path.Simple_Name = Ali_Simple_Name then
                        Self.Alis.Delete (Ali);
                        Self.Alis.Include (Path.Value);

                        exit Input_Loop;
                     end if;
                  end;
               end if;
            end loop Input_Loop;
         end;
      end loop;

      return True;
   end Pre_Execution;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Action_Id'Class is
   begin
      return Lib_Copy_Id'(Ctxt => Self.Ctxt);
   end UID;

end GPR2.Build.Actions.Thread.Lib_Copy;
