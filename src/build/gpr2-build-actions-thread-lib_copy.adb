--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;

with GPR2.Build.Actions.Process.Compile.Ada;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Source_Files;
with GPR2.Build.Source;
with GPR2.Build.Tree_Db;

package body GPR2.Build.Actions.Thread.Lib_Copy is

   package CA renames Actions.Process.Compile.Ada;

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPR.BUILD.ACTIONS.THREAD.LIB_COPY", GNATCOLL.Traces.Off);

   function Add_Copy
     (Self   : in out Object;
      From   : Path_Name.Object;
      To     : Path_Name.Object;
      Unit   : Compilation_Unit.Object := Compilation_Unit.Undefined;
      Kind   : Unit_Kind := S_Spec;
      Is_Ali : Boolean := False;
      Add_SL : Boolean := False) return Boolean;
   --  Append a file to copy, and declare its artifacts when the action is
   --  already in the tree. Else On_Tree_Insertion declares them.
   --  Returns False if To is already produced by another action.

   function Spec_Needs_Body
     (Self : Object; CU : Compilation_Unit.Object) return Boolean;
   --  Whether the body of CU belongs to the library interface. Only known
   --  once CU has been compiled, so this must not be called before the
   --  action is ready to run.

   function Copy_Needed (Self : Object; E : Copy_Entry) return Boolean;
   --  Whether E is to be copied: all ALIs are, and of the sources only the
   --  specs unless the unit has no spec or its body is part of the interface

   function Register
     (UID : Action_Id'Class;
      E   : Copy_Entry;
      Db  : in out GPR2.Build.Tree_Db.Object) return Boolean;
   --  Declare the source and destination of E as input and output of UID

   --------------
   -- Add_Copy --
   --------------

   function Add_Copy
     (Self   : in out Object;
      From   : Path_Name.Object;
      To     : Path_Name.Object;
      Unit   : Compilation_Unit.Object := Compilation_Unit.Undefined;
      Kind   : Unit_Kind := S_Spec;
      Is_Ali : Boolean := False;
      Add_SL : Boolean := False) return Boolean
   is
      E : constant Copy_Entry :=
            (From   => From,
             To     => To,
             Unit   => Unit,
             Kind   => Kind,
             Is_Ali => Is_Ali,
             Add_SL => Add_SL,
             Skip   => False);

   begin
      --  Registered before being kept, so that a destination that another
      --  action already produces is not copied anyway. Before the action is
      --  in the tree there is nothing to register: On_Tree_Insertion does it
      --  for the entries collected by then.

      if Self.Tree /= null
        and then not Register (Object'Class (Self).UID, E, Self.Tree.all)
      then
         return False;
      end if;

      Self.Copies.Append (E);

      return True;
   end Add_Copy;


   -------------------------
   -- Add_Interface_Unit --
   -------------------------

   function Add_Interface_Unit
     (Self            : in out Object;
      CU              : GPR2.Build.Compilation_Unit.Object;
      Dependency_File : Path_Name.Object) return Boolean
   is
      Success  : Boolean := True;
      Ali_From : Path_Name.Object renames Dependency_File;
      Ali_To   : constant Path_Name.Object :=
                   Self.Ctxt.Library_Ali_Directory.Compose
                     (Ali_From.Simple_Name);
      Src_Dir  : Path_Name.Object;

      procedure Add_Source
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type);

      ----------------
      -- Add_Source --
      ----------------

      procedure Add_Source
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type)
      is
         pragma Unreferenced (View, Index, Sep_Name);
      begin
         if not Self.Add_Copy
                  (From => Path,
                   To   => Src_Dir.Compose (Path.Simple_Name),
                   Unit => CU,
                   Kind => Kind)
         then
            Success := False;
         end if;
      end Add_Source;

   begin
      --  The interface of a standalone library is discovered incrementally
      --  while the binder parses the ALI files, so a unit may be added twice

      declare
         Pos      : Filename_Sets.Cursor;
         Inserted : Boolean;
      begin
         Self.Alis.Insert (Ali_From.Value, Pos, Inserted);

         if not Inserted then
            return True;
         end if;
      end;

      if not Self.Add_Copy
               (From   => Ali_From,
                To     => Ali_To,
                Unit   => CU,
                Is_Ali => True,
                Add_SL => Self.Ctxt.Is_Library_Standalone)
      then
         return False;
      end if;

      --  Every part is registered: which ones are actually copied is only
      --  known once the unit has been compiled, see Pre_Execution.

      if Self.Ctxt.Has_Library_Src_Directory then
         Src_Dir := Self.Ctxt.Library_Src_Directory;
         CU.For_All_Part (Add_Source'Access);
      end if;

      return Success;
   end Add_Interface_Unit;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      for E of Self.Copies loop
         if not Self.Copy_Needed (E) then
            null;

         elsif E.Is_Ali then
            if not Self.Signature.Add_Input
                     (Artifacts.Files.Create (E.From), Check_Checksums)
              or else not Self.Signature.Add_Output
                            (Artifacts.Files.Create (E.To), Check_Checksums)
            then
               return;
            end if;

         else
            if not Self.Signature.Add_Input
                     (Artifacts.Source_Files.Create (E.From), Check_Checksums)
              or else not Self.Signature.Add_Output
                            (Artifacts.Source_Files.Create (E.To),
                             Check_Checksums)
            then
               return;
            end if;
         end if;
      end loop;
   end Compute_Signature;

   -----------------
   -- Copy_Needed --
   -----------------

   function Copy_Needed (Self : Object; E : Copy_Entry) return Boolean is
   begin
      if E.Is_Ali or else E.Kind = S_Spec then
         return True;
      end if;

      return not E.Unit.Has_Part (S_Spec)
        or else Self.Spec_Needs_Body (E.Unit);
   end Copy_Needed;

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

      procedure Report_Error (Text : String);
      --  Append an error to Stderr. This subprogram runs in a dedicated task,
      --  so it must not use the tree's reporter, which is owned by the main
      --  task: just like a process action, it reports through its standard
      --  error output, which the scheduler displays when the action is
      --  collected.

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

      Attrs : GNATCOLL.OS.Stat.File_Attributes;

   begin
      --  Note: this subprogram is executed in a dedicated task, so it must
      --  query neither the tree database nor the project tree. Self.Copies
      --  holds everything it needs.

      for E of Self.Copies loop
         if E.Skip then
            null;

         elsif not E.Add_SL then
            if E.Is_Ali then
               Traces.Trace
                 ("Copying """
                  & E.From.String_Value
                  & """ to """
                  & E.To.Containing_Directory.String_Value
                  & '"');
            end if;

            if not GNATCOLL.OS.FSUtil.Copy_File
                     (E.From.String_Value, E.To.String_Value)
            then
               if E.Is_Ali then
                  Report_Error
                    ("could not copy ali file "
                     & String (E.From.Simple_Name)
                     & " to the library directory");

                  return 1;

               else
                  Report_Error
                    ("Cannot copy """
                     & String (E.From.Simple_Name)
                     & """ to the Library_Src_Dir """
                     & E.To.Containing_Directory.String_Value
                     & '"');

                  return (if E.Unit.Is_Defined then 5 else 6);
               end if;
            end if;

         else
            --  Amend the ALI to add the SL (StandAlone) flag to
            --  it to prevent multiple elaboration of the unit.

            Attrs := GNATCOLL.OS.Stat.Stat (E.From.String_Value);

            declare
               use GNATCOLL.OS;
               use type GNATCOLL.OS.FS.File_Descriptor;

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
               Ign    : Natural
               with Unreferenced;
               Found  : Boolean := False;
               Input  : GNATCOLL.OS.FS.File_Descriptor;
               Output : GNATCOLL.OS.FS.File_Descriptor;

            begin
               Input := FS.Open (E.From.String_Value, FS.Read_Mode);

               if Input = FS.Invalid_FD then
                  Report_Error
                    ("could not read the ali file """
                     & String (E.From.Simple_Name)
                     & '"');

                  return 2;
               end if;

               Output := FS.Open (E.To.String_Value, FS.Write_Mode);

               if Output = FS.Invalid_FD then
                  Report_Error
                    ("could not create the ali file """
                     & String (E.To.Simple_Name)
                     & '"');
                  FS.Close (Input);

                  return 3;
               end if;

               Traces.Trace
                 ("Installing """
                  & E.From.String_Value
                  & """ to """
                  & E.To.Containing_Directory.String_Value
                  & """ as library interface for "
                  & To_String (Self.Lib_Name));

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
                     & E.From.String_Value
                     & '"');

                  if not GNATCOLL.OS.FSUtil.Copy_File
                           (E.From.String_Value, E.To.String_Value)
                  then
                     Report_Error
                       ("could not copy ali file "
                        & String (E.From.Simple_Name)
                        & " to the library directory");

                     return 4;
                  end if;
               end if;
            end;
         end if;
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
      Self.Ctxt     := Ctxt;
      Self.Lib_Name := To_Unbounded_String (String (Ctxt.Name));

      --  The non-Ada interface sources are known from the view alone: the Ada
      --  ones come with their compile action, see Add_Interface_Unit.

      if Ctxt.Has_Library_Src_Directory then
         declare
            Src_Dir : constant Path_Name.Object := Ctxt.Library_Src_Directory;
         begin
            for C in Ctxt.Interface_Sources.Iterate loop
               declare
                  Path : constant Filename_Type :=
                           GPR2.Containers.Source_Path_To_Sloc.Key (C);
                  Src  : constant GPR2.Build.Source.Object :=
                           Ctxt.Visible_Source (Path);
               begin
                  if Src.Language /= Ada_Language then
                     --  Not in the tree yet, so this cannot fail

                     Ign := Self.Add_Copy
                       (From => Src.Path_Name,
                        To   =>
                          Src_Dir.Compose (Src.Path_Name.Simple_Name));
                  end if;
               end;
            end loop;
         end;
      end if;
   end Initialize;

   ---------------------
   -- Needed_For_View --
   ---------------------

   function Needed_For_View (Ctxt : GPR2.Project.View.Object) return Boolean
   is
   begin
      if Ctxt.Is_Library_Standalone then
         return not Ctxt.Interface_Closure.Is_Empty;
      end if;

      return not Ctxt.Own_Units.Is_Empty or else
        not Ctxt.Interface_Sources.Is_Empty;
   end Needed_For_View;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      UID : constant Action_Id'Class := Object'Class (Self).UID;

   begin
      for E of Self.Copies loop
         if not Register (UID, E, Db) then
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
   begin
      --  Runs in the main task once the units are compiled, which is when the
      --  ALIs can tell which parts belong to the interface. Execute runs in
      --  its own task and could not query them.

      for E of Self.Copies loop
         E.Skip := not Self.Copy_Needed (E);
      end loop;

      return True;
   end Pre_Execution;

   --------------
   -- Register --
   --------------

   function Register
     (UID : Action_Id'Class;
      E   : Copy_Entry;
      Db  : in out GPR2.Build.Tree_Db.Object) return Boolean is
   begin
      if E.Is_Ali then
         Db.Add_Input (UID, Artifacts.Files.Create (E.From));

         return Db.Add_Output (UID, Artifacts.Files.Create (E.To));

      else
         Db.Add_Input (UID, Artifacts.Source_Files.Create (E.From));

         return Db.Add_Output (UID, Artifacts.Source_Files.Create (E.To));
      end if;
   end Register;

   ---------------------
   -- Spec_Needs_Body --
   ---------------------

   function Spec_Needs_Body
     (Self : Object; CU : Compilation_Unit.Object) return Boolean
   is
      C_Id : constant CA.Ada_Compile_Id := CA.Create (CU);
   begin
      if not Self.Tree.Has_Action (C_Id) then
         return False;
      end if;

      return CA.Object'Class
               (Self.Tree.Action_Id_To_Reference (C_Id).Element.all)
                 .Spec_Needs_Body;
   end Spec_Needs_Body;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Action_Id'Class is
   begin
      return Lib_Copy_Id'(Ctxt => Self.Ctxt);
   end UID;

end GPR2.Build.Actions.Thread.Lib_Copy;
