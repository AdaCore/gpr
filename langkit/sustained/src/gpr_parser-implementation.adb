
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--











with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Task_Lock;
with GNAT.Traceback.Symbolic;

with GNATCOLL.Traces;

with Gpr_Parser_Support.Adalog.Debug;
with Gpr_Parser_Support.Generic_API.Analysis;
with Gpr_Parser_Support.Generic_API.Introspection;
with Gpr_Parser_Support.Hashes; use Gpr_Parser_Support.Hashes;
with Gpr_Parser_Support.Images; use Gpr_Parser_Support.Images;
with Gpr_Parser_Support.Names;  use Gpr_Parser_Support.Names;
with Gpr_Parser_Support.Relative_Get;

with Gpr_Parser.Private_Converters;
use Gpr_Parser.Private_Converters;

pragma Warnings (Off, "referenced");


pragma Warnings (On, "referenced");



package body Gpr_Parser.Implementation is

   use Precomputed_Symbols;

   pragma Warnings (Off, "has no effect");
   use Solver;
   pragma Warnings (On, "has no effect");

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Internal_Context);

   type Contexts_Destructor is limited
      new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (CD : in out Contexts_Destructor);
   --  Helper to destroy all contexts when terminating the process

   package Context_Pool is

      procedure Acquire (Context : out Internal_Context)
         with Post => Context /= null;
      --  If a context is free for reuse, increment its serial number and
      --  return it. Otherwise, allocate a new one. In any case, this does not
      --  initialize it, except for the Serial_Number field.

      procedure Release (Context : in out Internal_Context)
         with Pre  => Context /= null,
              Post => Context = null;
      --  Tag Context as free for reuse and set it to null

      procedure Free;
      --  Free all contexts in this pool. Intended to be called only when the
      --  process is terminating, to avoid reported memory leaks.

   private

      Available : Context_Vectors.Vector;
      --  List of allocated contexts that can be re-used right now

      CD : Contexts_Destructor with Unreferenced;
      --  Singleton whose only purpose is to free all contexts in Available
      --  when finalized.

   end Context_Pool;

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure);
   --  Common underlying implementation for Register_Destroyable_Gen

   pragma Warnings (Off, "referenced");
   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return Internal_Entity_Array_Access;
   pragma Warnings (On, "referenced");

   procedure Destroy (Env : in out Lexical_Env_Access);

   function Snaps_At_Start (Self : Bare_Gpr_Node) return Boolean;
   function Snaps_At_End (Self : Bare_Gpr_Node) return Boolean;

   --  Those maps are used to give unique ids to lexical envs while pretty
   --  printing them.

   package Address_To_Id_Maps is new Ada.Containers.Hashed_Maps
     (Lexical_Env, Integer, Hash, "=");

   type Dump_Lexical_Env_State is record
      Env_Ids : Address_To_Id_Maps.Map;
      --  Mapping: Lexical_Env -> Integer, used to remember which unique Ids we
      --  assigned to the lexical environments we found.

      Next_Id : Positive := 1;
      --  Id to assign to the next unknown lexical environment

      Root_Env : Lexical_Env;
      --  Lexical environment we consider a root (this is the Root_Scope from
      --  the current analysis context), or null if unknown.
   end record;
   --  Holder for the state of lexical environment dumpers

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String;
   --  If E is known, return its unique Id from State. Otherwise, assign it a
   --  new unique Id and return it.

   procedure Print
     (Node        : Gpr_Parser_Support.Generic_API.Analysis.Lk_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Helper for the public overload, but working on the generic API node type

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   pragma Warnings (Off, "referenced");
   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is
   pragma Warnings (On, "referenced");
   begin
         declare
            Raw_Text : constant Text_Type := (case Index is
            
               when Precomputed_Sym_Access => "access",
               when Precomputed_Sym_Aggregate => "aggregate",
               when Precomputed_Sym_Body => "body",
               when Precomputed_Sym_Configuration => "configuration",
               when Precomputed_Sym_Function => "function",
               when Precomputed_Sym_Generic => "generic",
               when Precomputed_Sym_Library => "library",
               when Precomputed_Sym_Procedure => "procedure",
               when Precomputed_Sym_Project => "project",
               when Precomputed_Sym_Separate => "separate",
               when Precomputed_Sym_Standard => "standard"
            );

            Symbol : constant Symbolization_Result :=
                  Gpr_Parser_Support.Symbols.Fold_Case (Raw_Text)
            ;
         begin
            if Symbol.Success then
               return Symbol.Symbol;
            else
               raise Program_Error with
                 "Cannot canonicalize symbol literal: " & Image (Raw_Text);
            end if;
         end;
   end Precomputed_Symbol;

   ----------------------------
   -- Construct_Entity_Array --
   ----------------------------

   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return Internal_Entity_Array_Access
   is
      Ret : Internal_Entity_Array_Access :=
        Create_Internal_Entity_Array (V.Length);
   begin
      for J in V.First_Index .. V.Last_Index loop
         Ret.Items (J) := V.Get (J);
      end loop;

      declare
         Tmp : AST_Envs.Entity_Vectors.Vector := V;
      begin
         Tmp.Destroy;
      end;

      return Ret;
   end Construct_Entity_Array;

   -----------
   -- Image --
   -----------

   function Image (Self : Symbol_Type) return String_Type is
   begin
      return Create_String (Image (Self));
   end Image;

   ------------------
   -- Context_Pool --
   ------------------

   package body Context_Pool is

      -------------
      -- Acquire --
      -------------

      procedure Acquire (Context : out Internal_Context) is
      begin
         GNAT.Task_Lock.Lock;

         if Available.Is_Empty then
            Context := new Analysis_Context_Type;
            Context.Serial_Number := 1;
         else
            Context := Available.Last_Element;
            Available.Delete_Last;
         end if;

         GNAT.Task_Lock.Unlock;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (Context : in out Internal_Context) is
      begin
         GNAT.Task_Lock.Lock;

         Available.Append (Context);
         Context.Serial_Number := Context.Serial_Number + 1;
         Context := null;

         GNAT.Task_Lock.Unlock;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end Release;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         GNAT.Task_Lock.Lock;

         for C of Available loop
            Free (C);
         end loop;

         GNAT.Task_Lock.Unlock;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end Free;

   end Context_Pool;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (CD : in out Contexts_Destructor) is
      pragma Unreferenced (CD);
   begin
      Context_Pool.Free;
   end Finalize;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_File_Reader'Class, Internal_File_Reader_Access);
   begin
      if File_Reader /= null and then File_Reader.all.Dec_Ref then
         Destroy (File_Reader);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Event_Handler'Class, Internal_Event_Handler_Access);
   begin
      if Self /= null and then Self.all.Dec_Ref then
         Destroy (Self);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Unit_Provider'Class, Internal_Unit_Provider_Access);
   begin
      if Provider /= null and then Provider.all.Dec_Ref then
         Destroy (Provider);
      end if;
   end Dec_Ref;

   ----------------
   -- Get_Env_Id --
   ----------------

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String
   is
      C        : Address_To_Id_Maps.Cursor;
      Inserted : Boolean;
   begin
      if E = Null_Lexical_Env then
         return "$null";

      elsif E = State.Root_Env then
         --  Insert root env with a special Id so that we only print it once
         State.Env_Ids.Insert (E, -1, C, Inserted);
         return "$root";
      end if;

      State.Env_Ids.Insert (E, State.Next_Id, C, Inserted);
      if Inserted then
         State.Next_Id := State.Next_Id + 1;
      end if;

      return '@' & Stripped_Image (Address_To_Id_Maps.Element (C));
   end Get_Env_Id;

   pragma Warnings (Off, "referenced");
   function To_Lookup_Kind_Type (K : Lookup_Kind) return Lookup_Kind_Type
   is
     (Lookup_Kind_Type'Val (Lookup_Kind'Pos (K)));
   pragma Warnings (On, "referenced");

   ----------------------
   -- Allocate_Context --
   ----------------------

   function Allocate_Context return Internal_Context is
   begin
      return Context : Internal_Context do
         Context_Pool.Acquire (Context);
         Context.Ref_Count := 1;
      end return;
   end Allocate_Context;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Context        : Internal_Context;
      Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive)
   is
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Precomputed_Symbol_Table
        := Create_Symbol_Table;
   begin
      Context.Symbols := Symbol_Table (Symbols);
      Context.Charset := To_Unbounded_String (Actual_Charset);
      Context.Tab_Stop := Tab_Stop;
      Context.With_Trivia := With_Trivia;
      Context.Root_Scope := Create_Static_Lexical_Env
        (Parent => Null_Lexical_Env,
         Node   => null);

      --  Create a new ownership share for Event_Handler so that it lives at
      --  least as long as this analysis context.
      Context.Event_Handler := Event_Handler;
      if Context.Event_Handler /= null then
         Context.Event_Handler.Inc_Ref;
      end if;

      --  Create a new ownership share for File_Reader so that it lives at
      --  least as long as this analysis context.
      Context.File_Reader := File_Reader;
      if Context.File_Reader /= null then
         Context.File_Reader.Inc_Ref;
      end if;

      --  Create a new ownership share for Unit_Provider so that it lives at
      --  least as long as this analysis context.
      Context.Unit_Provider := Unit_Provider;
      if Context.Unit_Provider /= null then
         Context.Unit_Provider.Inc_Ref;
      end if;


      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout :=
        Gpr_Parser_Support.Adalog.Default_Timeout_Ticks_Number;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Rewriting_Handle := No_Rewriting_Handle_Pointer;
      Context.Templates_Unit := No_Analysis_Unit;

      Context.Available_Rebindings := Env_Rebindings_Vectors.Empty_Vector;

      

   end Initialize_Context;

   -----------------
   -- Create_Unit --
   -----------------

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Unit : Internal_Unit;
   begin
      Unit := Create_Special_Unit
        (Context, Normalized_Filename, Charset, Rule);
      Context.Units.Insert (Normalized_Filename, Unit);
      return Unit;
   end Create_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant GNATCOLL.VFS.Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);

      Cur     : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
      Created : constant Boolean := Cur = No_Element;
      Unit    : Internal_Unit;

      Actual_Charset : Unbounded_String;
      Refined_Input  : Internal_Lexer_Input := Input;

   begin
      --  Determine which encoding to use. Use the Charset parameter (if
      --  provided), otherwise use the context-wide default.

      Actual_Charset := (if Charset'Length /= 0
                         then To_Unbounded_String (Charset)
                         else Context.Charset);

      if Refined_Input.Kind = File then
         Refined_Input.Filename := Normalized_Filename;
      end if;

      if Refined_Input.Kind in File | Bytes_Buffer then
         Refined_Input.Charset := Actual_Charset;

         --  Unless the caller requested a specific charset for this unit,
         --  allow the lexer to automatically discover the source file encoding
         --  before defaulting to the context-specific one. We do this trying
         --  to match a byte order mark.

         Refined_Input.Read_BOM := Charset'Length = 0;
      end if;

      --  Create the Internal_Unit if needed

      Unit :=
        (if Created
         then Create_Unit (Context, Normalized_Filename,
                           To_String (Actual_Charset), Rule)
         else Element (Cur));

      --  If an internal unit is requested, set the corresponding flag.
      --  Otherwise, make sure that the unit we return isn't internal.

      if Is_Internal then
         Unit.Is_Internal := True;
      end if;

      --  (Re)parse it if needed

      if Created or else Reparse then

         --  It is illegal to reparse an internal unit for public API users.
         --  Since public APIs do not allow to pass True to Is_Internal, we can
         --  check here that only the implementation can ask to reparse an
         --  internal unit.

         if Unit.Is_Internal and then not Is_Internal then
            raise Precondition_Failure with "cannot reparse an internal unit";
         end if;

         declare
            Reparsed : Reparsed_Unit;
         begin
            Do_Parsing (Unit, Refined_Input, Reparsed);
            Update_After_Reparse (Unit, Reparsed);
         end;

         --  Now that we have removed reparsed the unit, update its current
         --  charset.

         Unit.Charset := Actual_Charset;
      end if;

      if Context.Event_Handler /= null then
         Context.Event_Handler.Unit_Parsed_Callback
           (Context  => Context,
            Unit     => Unit,
            Reparsed => not Created and then Reparse);
      end if;

      return Unit;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
   begin
      return Context.Units.Contains
        (Normalized_Unit_Filename (Context, Unit_Filename));
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind     => File,
         Charset  => <>,
         Read_BOM => False,
         Filename => <>);
   begin
      if Reparse and then Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot reparse during tree rewriting";
      end if;

      return Get_Unit (Context, Filename, Charset, Reparse, Input, Rule);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind        => Bytes_Buffer,
         Charset     => <>,
         Read_BOM    => False,
         Bytes       => Buffer'Address,
         Bytes_Count => Buffer'Length);
   begin
      if Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot parse from buffer during tree rewriting";

      elsif Context.File_Reader /= null then
         raise Precondition_Failure with
            "cannot parse from buffer with a file reader";
      end if;

      return Get_Unit (Context, Filename, Charset, True, Input, Rule);
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);
      Cur                 : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
   begin
      if Cur = No_Element then
         declare
            Unit : constant Internal_Unit := Create_Unit
              (Context, Normalized_Filename, Charset, Rule);
         begin
            Append (Unit.Diagnostics, No_Source_Location_Range, Error);
            return Unit;
         end;
      else
         return Element (Cur);
      end if;
   end Get_With_Error;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access
   is (Context.Unit_Provider);

   ------------------
   -- Resolve_Unit --
   ------------------

   procedure Resolve_Unit
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Unit    : out Resolved_Unit)
   is
      --  Look for the cache entry corresponding to Unit; create one if needed

      Dummy    : Resolved_Unit_Array;
      Key      : constant Symbol_Type := Find (Context.Symbols, Name);
      Pos      : Unit_Provider_Cache_Maps.Cursor;
      Inserted : Boolean;
   begin
      Context.Unit_Provider_Cache.Insert (Key, Dummy, Pos, Inserted);
      declare
         Units : Resolved_Unit_Array renames
           Context.Unit_Provider_Cache.Reference (Pos);
         U     : Resolved_Unit renames Units (Kind);
      begin
         --  If the cache entry is not populated for the requested kind, run
         --  the query and save the result for later requests.

         if U.Filename = null then
            declare
               Provider : Internal_Unit_Provider'Class renames
                 Context.Unit_Provider.all;
               Filename : Unbounded_String;
            begin
               Provider.Get_Unit_Location
                 (Name           => Name,
                  Kind           => Kind,
                  Filename       => Filename,
                  PLE_Root_Index => U.PLE_Root_Index);
               Provider.Get_Unit_And_PLE_Root
                 (Context        => Context,
                  Name           => Name,
                  Kind           => Kind,
                  Unit           => U.Unit,
                  PLE_Root_Index => U.PLE_Root_Index);
               U.Filename := new String'(To_String (Filename));
            end;
         end if;

         Unit := U;
      end;
   end Resolve_Unit;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   procedure Get_Unit_Location
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out String_Access;
      PLE_Root_Index : out Positive)
   is
      U : Resolved_Unit;
   begin
      Resolve_Unit (Context, Name, Kind, U);
      Filename := U.Filename;
      PLE_Root_Index := U.PLE_Root_Index;
   end Get_Unit_Location;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   procedure Get_Unit_And_PLE_Root
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive)
   is
      U : Resolved_Unit;
   begin
      Resolve_Unit (Context, Name, Kind, U);
      Unit := U.Unit;
      PLE_Root_Index := U.PLE_Root_Index;
   end Get_Unit_And_PLE_Root;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Internal_Context) return Hash_Type is
      function H is new Hash_Access (Analysis_Context_Type, Internal_Context);
   begin
      return H (Context);
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Internal_Context) return Boolean is
   begin
      return Context.With_Trivia;
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean) is
   begin
      Context.Discard_Errors_In_Populate_Lexical_Env := Discard;
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural) is
   begin
      Context.Logic_Resolution_Timeout := Timeout;
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean is
   begin
      return Context.Rewriting_Handle /= No_Rewriting_Handle_Pointer;
   end Has_Rewriting_Handle;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Context : Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Context : in out Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count - 1;
         if Context.Ref_Count = 0 then
            Destroy (Context);
         end if;
      end if;
   end Dec_Ref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Internal_Context) is
   begin
      --  Destroy all named environment data structures
      for Desc of Context.Named_Envs loop
         for V of Desc.Foreign_Nodes loop
            V.Destroy;
         end loop;
         Destroy (Desc);
      end loop;
      Context.Named_Envs.Clear;

      --  If we are asked to free this context, it means that no one else have
      --  references to its analysis units, so it's safe to destroy these.
      for Unit of Context.Units loop
         Destroy (Unit);
      end loop;
      Context.Units := Units_Maps.Empty_Map;
      Context.Filenames := Virtual_File_Maps.Empty_Map;

      declare
         procedure Destroy is new Ada.Unchecked_Deallocation
           (Env_Rebindings_Type, Env_Rebindings);

         AR : Env_Rebindings_Vectors.Vector renames
            Context.Available_Rebindings;
         R  : Env_Rebindings;
      begin
         for I in AR.First_Index .. AR.Last_Index loop
            R := AR.Get (I);
            Destroy (R);
         end loop;
         AR.Destroy;
      end;

      for Pos in Context.Unit_Provider_Cache.Iterate loop
         declare
            Units : Resolved_Unit_Array renames
              Context.Unit_Provider_Cache.Reference (Pos);
         begin
            for U of Units loop
               Free (U.Filename);
            end loop;
         end;
      end loop;
      Context.Unit_Provider_Cache.Clear;

      Destroy (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Dec_Ref (Context.File_Reader);
      Dec_Ref (Context.Unit_Provider);
      Dec_Ref (Context.Event_Handler);
      Context_Pool.Release (Context);
   end Destroy;

   -------------
   -- Context --
   -------------

   function Context (Unit : Internal_Unit) return Internal_Context is
   begin
      return Unit.Context;
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Internal_Unit) return Hash_Type is
      function H is new Hash_Access (Analysis_Unit_Type, Internal_Unit);
   begin
      return H (Unit);
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String) is
      Dummy : constant Internal_Unit := Get_From_File
        (Unit.Context, +Unit.Filename.Full_Name, Charset,
         Reparse => True,
         Rule    => Unit.Rule);
   begin
      null;
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String; Buffer : String)
   is
      Dummy : constant Internal_Unit := Get_From_Buffer
        (Unit.Context, +Unit.Filename.Full_Name, Charset, Buffer, Unit.Rule);
   begin
      null;
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env
     (Unit           : Internal_Unit;
      PLE_Root_Index : Positive
         := 1
   ) is
      Context : constant Internal_Context := Unit.Context;

      Saved_In_Populate_Lexical_Env : constant Boolean :=
        Context.In_Populate_Lexical_Env;

      Has_Errors : Boolean := False;
      --  Whether at least one Property_Error occurred during this PLE pass

      procedure Reset_Envs_Caches (Unit : Internal_Unit);
      --  Reset the env caches of all lexical environments created for ``Unit``

      -----------------------
      -- Reset_Envs_Caches --
      -----------------------

      procedure Reset_Envs_Caches (Unit : Internal_Unit) is
         procedure Internal (Node : Bare_Gpr_Node);
         --  Reset env caches in ``Node`` and then in its children recursively

         --------------
         -- Internal --
         --------------

         procedure Internal (Node : Bare_Gpr_Node) is
         begin
            if Node = null then
               return;
            end if;
            Reset_Caches (Node.Self_Env);
            for I in 1 .. Children_Count (Node) loop
               Internal (Child (Node, I));
            end loop;
         end Internal;
      begin
         Internal (Unit.Ast_Root);
      end Reset_Envs_Caches;

   begin
      --  TODO??? Handle env invalidation when reparsing a unit and when a
      --  previous call raised a Property_Error.

      --  If we have already run PLE on this root, there is nothing to do.
      --  Otherwise, keep track of the fact that PLE was requested for it,
      --  possibly extending the vector if needed.

      if Unit.Env_Populated_Roots.Last_Index >= PLE_Root_Index
         and then Unit.Env_Populated_Roots.Get (PLE_Root_Index)
      then
         return;
      end if;
      for Dummy in Unit.Env_Populated_Roots.Last_Index + 1 .. PLE_Root_Index
      loop
         Unit.Env_Populated_Roots.Append (False);
      end loop;
      Unit.Env_Populated_Roots.Set (PLE_Root_Index, True);

      --  Create context for the PLE run: all exit points must call the Cleanup
      --  procedure above first to clean this context.

      Context.In_Populate_Lexical_Env := True;
      if Main_Trace.Active then
         Main_Trace.Trace
           ("Populating lexical envs for"
            & " unit: " & Basename (Unit));
         Main_Trace.Increase_Indent;
      end if;

      --  Fetch the node on which to run PLE: it's the unit root node, or one
      --  of its children if PLE roots are enabled and the unit has a list of
      --  PLE roots. Then run PLE itself.

      declare
         PLE_Root : Bare_Gpr_Node := Unit.Ast_Root;
      begin

         if PLE_Root /= null then
            Has_Errors := Populate_Lexical_Env (PLE_Root);
         end if;
      end;

      --  Restore the context for PLE run (undo what was done above)

      Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
      if Main_Trace.Active then
         Main_Trace.Decrease_Indent;
         Main_Trace.Trace
           ("Finished populating lexical envs for"
            & " unit: " & Basename (Unit));
      end if;

      Reset_Envs_Caches (Unit);

      if Has_Errors and then not Context.Discard_Errors_In_Populate_Lexical_Env
      then
         raise Property_Error with
            "errors occurred in Populate_Lexical_Env";
      end if;
   end Populate_Lexical_Env;

   -----------------------------------
   -- Populate_Lexical_Env_For_Unit --
   -----------------------------------

   procedure Populate_Lexical_Env_For_Unit (Node : Bare_Gpr_Node) is
      Root  : Bare_Gpr_Node;
      Index : Natural;
   begin
      Lookup_PLE_Root (Node, Root, Index);
      if Index = 0 then
         Index := 1;
      end if;
      Populate_Lexical_Env (Node.Unit, Index);
   end Populate_Lexical_Env_For_Unit;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Internal_Unit) return String is
     (+Unit.Filename.Full_Name);

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Internal_Unit) return String is
   begin
      return To_String (Unit.Charset);
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean is
   begin
      return not Unit.Diagnostics.Is_Empty;
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array is
      Result : Diagnostics_Array (1 .. Natural (Unit.Diagnostics.Length));
      I      : Natural := 1;
   begin
      for D of Unit.Diagnostics loop
         Result (I) := D;
         I := I + 1;
      end loop;
      return Result;
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String
   is
      Filename : constant String := Basename (Unit);
      Sloc     : constant Source_Location := Start_Sloc (D.Sloc_Range);
      Msg      : constant String :=
         Image
           (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (D.Message));
   begin
      return (Filename
              & (if Sloc = No_Source_Location then "" else ":" & Image (Sloc))
              & ": " & Msg);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Internal_Unit) return Bare_Gpr_Node is
     (Unit.Ast_Root);

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            First_Token_Or_Trivia (Unit.TDH)));

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            Last_Token_Or_Trivia (Unit.TDH)));

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Tokens.Length);

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Trivias.Length);

   ----------
   -- Text --
   ----------

   function Text (Unit : Internal_Unit) return Text_Type is
   begin
      return Text (First_Token (Unit), Last_Token (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference
   is
      Result : constant Token_Or_Trivia_Index := Lookup_Token (Unit.TDH, Sloc);
   begin
      return Wrap_Token_Reference (Unit.Context, Unit.TDH'Access, Result);
   end Lookup_Token;

   ---------------------
   -- Lookup_PLE_Root --
   ---------------------

   procedure Lookup_PLE_Root
     (Node  : Bare_Gpr_Node;
      Root  : out Bare_Gpr_Node;
      Index : out Natural)
   is
      Unit : constant Internal_Unit := Node.Unit;
   begin
      --  If this unit does not contain a list of PLE roots, just return the
      --  unit root node.

      if Unit.PLE_Roots_Starting_Token.Is_Empty then
         Root := Unit.Ast_Root;
         Index := 0;
         return;
      end if;

      --  Otherwise, look for the last PLE root whose first token (in
      --  Unit.PLE_Roots_Starting_Token) appears before Node's (T). This vector
      --  is sorted by construction, so we can perform a binary search.

      declare
         T      : constant Token_Index := Node.Token_Start_Index;
         Tokens : Token_Index_Vectors.Vector renames
           Unit.PLE_Roots_Starting_Token;

         First : Positive := Tokens.First_Index;
         Last  : Positive := Tokens.Last_Index;
         I     : Positive;
      begin
         while First < Last loop

            --  Because we look for the "floor" (last element that is <= T), we
            --  need to look at the value in Last when there are only two
            --  elements left to look at. If we did not do that, then we would
            --  go into an infinite loop when Tokens[First] < T.

            I := (if First + 1 = Last
                  then Last
                  else (First + Last) / 2);
            declare
               I_T : constant Token_Index := Tokens.Get (I);
            begin
               if I_T <= T then
                  First := I;
               else
                  Last := I - 1;
               end if;
            end;
         end loop;

         Root := Child (Unit.Ast_Root, First);
         Index := First;
      end;
   end Lookup_PLE_Root;

   --------------
   -- Ple_Root --
   --------------

   function Ple_Root
     (Node : Bare_Gpr_Node) return Bare_Gpr_Node
   is
      Root        : Bare_Gpr_Node;
      Dummy_Index : Natural;
   begin
      if Node = null then
         raise Property_Error with "null node dereference";
      end if;
      Lookup_PLE_Root (Node, Root, Dummy_Index);
      return Root;
   end Ple_Root;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Internal_Unit) is
      Node     : constant Bare_Gpr_Node := Unit.Ast_Root;
      Root_Env : constant Lexical_Env := Unit.Context.Root_Scope;
      State    : Dump_Lexical_Env_State := (Root_Env => Root_Env, others => <>);

      function Get_Parent (Env : Lexical_Env) return Lexical_Env
      is (Unwrap (Env).Parent);

      --------------------------
      -- Explore_Parent_Chain --
      --------------------------

      procedure Explore_Parent_Chain (Env : Lexical_Env) is
         P : Lexical_Env;
      begin
         if Env /= Null_Lexical_Env then
            P := Get_Parent (Env);
            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (P, State));
            Explore_Parent_Chain (P);
         end if;
      end Explore_Parent_Chain;

      --------------
      -- Internal --
      --------------

      procedure Internal (Current : Bare_Gpr_Node) is
         Explore_Parent : Boolean := False;
         Env, Parent    : Lexical_Env;
      begin
         if Current = null then
            return;
         end if;

         --  We only dump environments that we haven't dumped before. This way
         --  we'll only dump environments at the site of their creation, and
         --  not in any subsequent link. We use the Env_Ids map to check which
         --  envs we have already seen or not.
         if not State.Env_Ids.Contains (Current.Self_Env) then
            Env := Current.Self_Env;
            Parent := Get_Parent (Env);
            Explore_Parent := not State.Env_Ids.Contains (Parent);

            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (Parent, State));

            if Explore_Parent then
               Explore_Parent_Chain (Parent);
            end if;
         end if;

         for Child of Internal_Bare_Gpr_Node_Array'(Children (Current))
         loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (Bare_Gpr_Node (Node));
   end Dump_Lexical_Env;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
   begin
      return Get_Line (Unit.TDH, Line_Number);
   end Get_Line;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean) is
   begin
      if Unit.Ast_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Print (Unit.Ast_Root, Show_Slocs);
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Internal_Unit) is

      procedure Process (Trivia : Token_Index) is
         Data : constant Stored_Token_Data :=
            Unit.TDH.Trivias.Get (Natural (Trivia)).T;
      begin
         Put_Line (Image (Text (Unit.TDH, Data)));
      end Process;

      Last_Token : constant Token_Index :=
         Token_Index (Token_Vectors.Last_Index (Unit.TDH.Tokens) - 1);
      --  Index for the last token in Unit excluding the Termination token
      --  (hence the -1).
   begin
      for Tok of Get_Leading_Trivias (Unit.TDH) loop
         Process (Tok);
      end loop;

      PP_Trivia (Unit.Ast_Root);

      for Tok of Get_Trivias (Unit.TDH, Last_Token) loop
         Process (Tok);
      end loop;
   end PP_Trivia;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : in out Internal_Unit) is
   begin
      if Unit = No_Analysis_Unit then
         return;
      end if;

      Unit.PLE_Roots_Starting_Token.Destroy;
      Unit.Env_Populated_Roots.Destroy;

      Unit.Exiled_Entries.Destroy;
      Unit.Foreign_Nodes.Destroy;
      Unit.Exiled_Entries_In_NED.Destroy;
      Unit.Exiled_Envs.Destroy;
      Unit.Named_Envs.Destroy;
      Analysis_Unit_Sets.Destroy (Unit.Referenced_Units);


      Destroy_Rebindings (Unit.Rebindings'Access);
      Unit.Rebindings.Destroy;

      if Unit.Ast_Root /= null then
         Destroy (Unit.Ast_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.Ast_Mem_Pool);
      Destroy_Unit_Destroyables (Unit);
      Destroyable_Vectors.Destroy (Unit.Destroyables);
      

      Free (Unit);
   end Destroy;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Bare_Gpr_Node) return Boolean is
   begin
      return Is_Token_Node (Node.Kind);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Bare_Gpr_Node) return Boolean is
   begin
      return Node.Kind in Synthetic_Nodes;
   end Is_Synthetic;

   ------------------------------
   -- Raise_Property_Exception --
   ------------------------------

   procedure Raise_Property_Exception
     (Node    : Bare_Gpr_Node;
      Exc     : Ada.Exceptions.Exception_Id;
      Message : String)
   is
      Sloc_Prefix : constant String :=
        (if Node = null
         then ""
         else Ada.Directories.Simple_Name (Get_Filename (Unit (Node)))
              & ":" & Image (Sloc_Range (Node)) & ": ");
   begin
      Ada.Exceptions.Raise_Exception (Exc, Sloc_Prefix & Message);
   end Raise_Property_Exception;

   ------------------------------
   -- Register_Destroyable_Gen --
   ------------------------------

   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access)
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Destroy_Procedure);
      procedure Destroy_Procedure (Object : in out T_Access) renames Destroy;
   begin
      Register_Destroyable_Helper
        (Unit,
         Object.all'Address,
         Convert (Destroy_Procedure'Address));
   end Register_Destroyable_Gen;

      

   


   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Bare_Gpr_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Gpr_Node
   is
      function Absolute_Get
        (T : Bare_Gpr_Node_Array_Access; Index : Integer)
         return Bare_Gpr_Node
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Bare_Gpr_Node,
         Sequence_Type => Bare_Gpr_Node_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Gpr_Node;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Bare_Gpr_Node;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Bare_Gpr_Node_Array_Access) return Bare_Gpr_Node_Array_Access is
      Ret : Bare_Gpr_Node_Array_Access := Create_Bare_Gpr_Node_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Gpr_Node_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Bare_Gpr_Node_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Gpr_Node_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Bare_Gpr_Node_Array (Items_Count : Natural) return Bare_Gpr_Node_Array_Access
   is (if Items_Count = 0
       then No_Bare_Gpr_Node_Array_Type
       else new Bare_Gpr_Node_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Bare_Gpr_Node_Array
     (Items : Internal_Bare_Gpr_Node_Array) return Bare_Gpr_Node_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Bare_Gpr_Node_Array_Type;
      end if;

      return new Bare_Gpr_Node_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Bare_Gpr_Node_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Gpr_Node_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity
   is
      function Absolute_Get
        (T : Internal_Entity_Array_Access; Index : Integer)
         return Internal_Entity
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Internal_Entity,
         Sequence_Type => Internal_Entity_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access is
      Ret : Internal_Entity_Array_Access := Create_Internal_Entity_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access
   is (if Items'Length = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'
         (N         => Items'Length,
          Items     => Implementation.Internal_Internal_Entity_Array (Items),
          Ref_Count => 1));

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Array_Type;
      end if;

      return new Internal_Entity_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc
   is
      function Absolute_Get
        (T : Internal_Inner_Env_Assoc_Array_Access; Index : Integer)
         return Internal_Inner_Env_Assoc
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Internal_Inner_Env_Assoc,
         Sequence_Type => Internal_Inner_Env_Assoc_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Inner_Env_Assoc;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Inner_Env_Assoc;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access is
      Ret : Internal_Inner_Env_Assoc_Array_Access := Create_Internal_Inner_Env_Assoc_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access
   is (if Items_Count = 0
       then No_Internal_Inner_Env_Assoc_Array_Type
       else new Internal_Inner_Env_Assoc_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Inner_Env_Assoc_Array_Type;
      end if;

      return new Internal_Inner_Env_Assoc_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env
   is
      function Absolute_Get
        (T : Lexical_Env_Array_Access; Index : Integer)
         return Lexical_Env
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Lexical_Env,
         Sequence_Type => Lexical_Env_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Lexical_Env;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return Empty_Env;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access is
      Ret : Lexical_Env_Array_Access := Create_Lexical_Env_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Lexical_Env_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Lexical_Env_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Lexical_Env_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access
   is (if Items_Count = 0
       then No_Lexical_Env_Array_Type
       else new Lexical_Env_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Lexical_Env_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Lexical_Env_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               not Equivalent (L.Items (I), R.Items (I))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Lexical_Env_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   


   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Gpr_Node;
      T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type
   is
      function Absolute_Get
        (T : Symbol_Type_Array_Access; Index : Integer)
         return Symbol_Type
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Symbol_Type,
         Sequence_Type => Symbol_Type_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Symbol_Type;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access is
      Ret : Symbol_Type_Array_Access := Create_Symbol_Type_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Symbol_Type_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Symbol_Type_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Symbol_Type_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access
   is (if Items_Count = 0
       then No_Symbol_Type_Array_Type
       else new Symbol_Type_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Symbol_Type_Array_Type;
      end if;

      return new Symbol_Type_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if
               L.Items (I) /= R.Items (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Symbol_Type_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;





         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Bare_Gpr_Node_Iterator_Access;
      Element : out Bare_Gpr_Node) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Gpr_Node_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Gpr_Node_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Gpr_Node_Iterator_Access) return String is
      begin
         return "<Iterator of GprNode, index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String is
      begin
         return "<Iterator of GprNode.entity, index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String is
      begin
         return "<Iterator of InnerEnvAssoc, index="
                & A.Index'Image & ">";
      end Trace_Image;



   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Internal_Unit) return Boolean is
   begin
      return Left.Filename < Right.Filename;
   end "<";


   -------------------
   -- Solve_Wrapper --
   -------------------

   function Solve_Wrapper
     (R            : Solver.Relation;
      Context_Node : Bare_Gpr_Node) return Boolean is
   begin
      if Context_Node /= null and then Gpr_Parser_Support.Adalog.Debug.Debug then
         Assign_Names_To_Logic_Vars (Context_Node);
      end if;

      begin
         return Solver.Solve_First
           (R, Timeout => Context_Node.Unit.Context.Logic_Resolution_Timeout);
      exception
         when Gpr_Parser_Support.Adalog.Early_Binding_Error =>
            Raise_Property_Exception
              (Context_Node,
               Property_Error'Identity,
               "invalid equation for logic resolution");
         when Gpr_Parser_Support.Adalog.Timeout_Error =>
            Raise_Property_Exception
              (Context_Node,
               Property_Error'Identity,
               "logic resolution timed out");
      end;
   end Solve_Wrapper;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : in out Lexical_Env_Access) is
      Mutable_Env : Lexical_Env :=
        (Wrap (Env), 0, Env.Kind, No_Generic_Unit, 0);
   begin
      Destroy (Mutable_Env);
      Env := null;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self              : Bare_Gpr_Node;
      Kind              : Gpr_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Gpr_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env) is
   begin
      pragma Unreferenced (Kind);
      Self.Parent := Parent;
      Self.Unit := Unit;

      Self.Token_Start_Index := Token_Start_Index;
      Self.Token_End_Index := Token_End_Index;

      Self.Self_Env := Self_Env;
      Self.Last_Attempted_Child := -1;

      

   end Initialize;

   --------------------
   -- Use_Direct_Env --
   --------------------

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env)
   is
   begin
      State.Current_Env := Env;
      State.Current_NED := null;
   end Use_Direct_Env;

   -------------------
   -- Use_Named_Env --
   -------------------

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type) is
   begin
      State.Current_NED := Get_Named_Env_Descriptor (Context, Name);
      State.Current_Env := State.Current_NED.Env_With_Precedence;
   end Use_Named_Env;

   ---------------------
   -- Set_Initial_Env --
   ---------------------

   procedure Set_Initial_Env
     (Self         : Bare_Gpr_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_Designated_Env;
      DSL_Location : String) is
   begin
      case Env.Kind is
         when None =>
            Use_Direct_Env (State, Empty_Env);

         when Current_Env =>
            null;

         when Named_Env =>
            Use_Named_Env (State, Self.Unit.Context, Env.Env_Name);

         when Direct_Env =>

            --  Sanitize this environment value: make sure it's a non-foreign
            --  and primary environment.

            if Env.Direct_Env.Kind /= Static_Primary then
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "Cannot set an env that is not static-primary as the"
                  & " initial env");

            elsif Is_Foreign_Strict (Env.Direct_Env, Self) then
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "unsound foreign environment in SetInitialEnv ("
                  & DSL_Location & ")");
            end if;
            Use_Direct_Env (State, Env.Direct_Env);
      end case;
   end Set_Initial_Env;

   ----------------
   -- Add_To_Env --
   ----------------

   procedure Add_To_Env
     (Self         : Bare_Gpr_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Gpr_Node;
      Md           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_Designated_Env;
      DSL_Location : String)
   is
      Context    : constant Internal_Context := Self.Unit.Context;
      Root_Scope : Lexical_Env renames Context.Root_Scope;
      --  Shortcuts

      Actual_Dest_Env : Lexical_Env;
      Dest_NED        : Named_Env_Descriptor_Access;
      --  Description for the destination environment
   begin
      --  Skip the env addition if explicitly requested

      if Key = null
         or else Value = null
         or else (case Dest_Env.Kind is
                  when None        => True,
                  when Current_Env => False,
                  when Named_Env   => Dest_Env.Env_Name = null,
                  when Direct_Env  => Dest_Env.Direct_Env = Empty_Env)
      then
         return;
      end if;

      if Value.Unit /= Self.Unit then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add_to_env an AST node that comes from another analysis"
            & " unit");
      end if;

      

      --  Then determine the destination environment

      case Dest_Env.Kind is
         when None =>
            raise Program_Error with "unreachable code";

         when Current_Env =>
            --  Just use the current environment
            Dest_NED := State.Current_NED;
            Actual_Dest_Env := State.Current_Env;

         when Named_Env =>
            --  There is an environment name: just lookup the corresponding
            --  NED/env.
            Dest_NED := Get_Named_Env_Descriptor (Context, Dest_Env.Env_Name);
            Actual_Dest_Env := Dest_NED.Env_With_Precedence;

         when Direct_Env =>
            --  There is an explicit destination environment
            Dest_NED := null;
            Actual_Dest_Env := Dest_Env.Direct_Env;
      end case;

      --  Sanitize it

      if Actual_Dest_Env.Kind /= Static_Primary then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add elements to a lexical env that is not static-primary");

      elsif
         --  Since lexical envs need to sort the foreign nodes they contain,
         --  and that the total order on nodes is not defined for synthetic
         --  nodes, it is not possible to add a synthetic node to a foreign
         --  lexical environment.
         --
         --  This reasoning applies to environments that belong to foreign
         --  units, but also to the root environment.
         Is_Foreign (Actual_Dest_Env, Self) and then Is_Synthetic (Value)
      then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add a synthetic node to a lexical env from another"
            & " analysis unit");

      elsif
         --  Reject direct references to foreign destination environments.
         --
         --  This is an attempt at identifying uses of the unsound relocation
         --  mechanism (as opposed to named environments), so this applies to
         --  all foreign environments (root scope included).
         DSL_Location'Length > 0
         and then Dest_Env.Kind = Direct_Env
         and then Is_Foreign_Strict (Actual_Dest_Env, Self)
      then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "unsound foreign environment in AddToEnv (" & DSL_Location & ")");
      end if;

      --  Now that everything is sanitized, we can proceed with the actual
      --  key/value pair addition. Note that this does nothing if
      --  Actual_Dest_Env ended up empty.
      Add (Actual_Dest_Env, Key, Value, Md, Resolver);

      --  If we're adding the element to an environment by env name, we must
      --  register this association in two places: in the target named env
      --  entry, and in Value's unit.
      if Dest_NED /= null then
         declare
            use NED_Assoc_Maps;

            FN    : Map renames Dest_NED.Foreign_Nodes;
            Dummy : Boolean;
            Cur   : Cursor;
         begin
            FN.Insert (Key      => Key,
                       New_Item => Internal_Map_Node_Vectors.Empty_Vector,
                       Position => Cur,
                       Inserted => Dummy);
            declare
               V : Internal_Map_Node_Vectors.Vector renames
                  FN.Reference (Cur);
            begin
               V.Append ((Value, Md, Resolver));
            end;
         end;
         Value.Unit.Exiled_Entries_In_NED.Append ((Dest_NED, Key, Value));

      --  Otherwise, if we're adding the element to an environment that belongs
      --  to a different unit, or to the root scope, then...
      elsif Is_Foreign_Not_Empty (Actual_Dest_Env, Self) then
         --  Add the Key/Value association to the list of entries contained in
         --  other units, so we can remove them when reparsing Value's unit.
         Value.Unit.Exiled_Entries.Append ((Actual_Dest_Env, Key, Value));

         if Actual_Dest_Env /= Root_Scope then
            --  Add Val to the list of foreign nodes that Actual_Dest_Env's
            --  unit contains, so that when that unit is reparsed, we can call
            --  Add_To_Env again on those nodes.
            Convert_Unit (Actual_Dest_Env.Owner).Foreign_Nodes.Append
              ((Value, Self.Unit));
         end if;
      end if;
   end Add_To_Env;

   -------------
   -- Ref_Env --
   -------------

   procedure Ref_Env
     (Self                : Bare_Gpr_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Gpr_Node_Array_Access;
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean) is
   begin
      for N of Ref_Env_Nodes.Items loop
         if N /= null then
            if N.Unit /= Self.Unit then
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "attempt to add a referenced environment to a foreign unit");
            end if;
            Reference (Dest_Env, N, Resolver, Kind, Cats, Shed_Rebindings);
         end if;
      end loop;
      Dec_Ref (Ref_Env_Nodes);
   end Ref_Env;

   -------------
   -- Add_Env --
   -------------

   procedure Add_Env
     (Self              : Bare_Gpr_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access)
   is
      Parent_From_Name : constant Boolean := State.Current_NED /= null;
      --  Does the parent environment comes from a named environment lookup?

      --  Determine the parent of this new environment:
      --
      --  (1) no parent if requested;
      --  (2) the current environment as the static parent if it comes from a
      --      named env lookup or if it is not foreign (or is the empty/root
      --      environment).
      Parent : constant Lexical_Env :=
        (if No_Parent
         then Null_Lexical_Env
         else State.Current_Env);
   begin
      --  Create the environment itself
      Self.Self_Env := Create_Static_Lexical_Env
        (Parent            => Parent,
         Node              => Self,
         Transitive_Parent => Transitive_Parent);

      --  If the parent of this new environment comes from a named environment
      --  lookup, register this new environment so that its parent is updated
      --  when the precence for this named environment changes.
      if Parent_From_Name then
         declare
            NED : constant Named_Env_Descriptor_Access := State.Current_NED;
         begin
            Self.Unit.Exiled_Envs.Append ((NED, Self.Self_Env));
            NED.Foreign_Envs.Insert (Self, Self.Self_Env);
         end;
      end if;

      --  From now on, the current environment is Self.Self_Env, with a direct
      --  access to it. It does not go through the env naming scheme, since
      --  only this node and its children (i.e. non-foreign nodes) will access
      --  it as a "current" environment during PLE.
      Use_Direct_Env (State, Self.Self_Env);

      --  Register the environment we just created on all the requested names
      if Names /= null then
         declare
            Context   : constant Internal_Context := Self.Unit.Context;
            Env       : constant Lexical_Env := Self.Self_Env;
            NENU      : NED_Maps.Map renames
               State.Unit_State.Named_Envs_Needing_Update;
         begin
            for N of Names.Items loop
               Register_Named_Env (Context, N, Env, NENU);
            end loop;
            Dec_Ref (Names);
         end;
      end if;
   end Add_Env;

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   procedure Pre_Env_Actions
     (Self            : Bare_Gpr_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False) is
   begin

      
   

   case Self.Kind is
      when others =>  null; 
   end case;


         pragma Unreferenced (State, Add_To_Env_Only);
   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self : Bare_Gpr_Node; State : in out PLE_Node_State) is
   begin
      
   

   case Self.Kind is
      when others =>  null; 
   end case;


         pragma Unreferenced (State);
   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : Bare_Gpr_Node) return Symbol_Type is
   begin
      if Node = null then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "cannot get the symbol of a null node");
      end if;
      return Get_Symbol (Token (Node, Node.Token_Start_Index));
   end Get_Symbol;

   ----------
   -- Text --
   ----------

   function Text
     (Node : Bare_Gpr_Node) return Text_Type
   is
   begin
      if Node = null then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "cannot get the text of a null node");
      end if;

      declare
         Start_T : constant Token_Reference :=
            Token (Node, Node.Token_Start_Index);
         End_T   : constant Token_Reference :=
            Token (Node, Node.Token_End_Index);
      begin
         --  No text is associated to synthetic and ghost nodes

         if Is_Synthetic (Node) then
            return "";
         end if;

         if Is_Ghost (Node) then
            return "";
         end if;

         return Text (Start_T, End_T);
      end;
   end Text;

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Self                     : Bare_Gpr_Node;
      Referenced_Env, Base_Env : Lexical_Env) return Boolean
   is
      Referenced_Unit : constant Internal_Unit :=
         Convert_Unit (Referenced_Env.Owner);
      Base_Unit       : constant Internal_Unit :=
         Convert_Unit (Base_Env.Owner);
   begin
      if Referenced_Unit = null then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "referenced environment does not belong to any analysis unit");
      elsif Base_Unit = null then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "base environment does not belong to any analysis unit");
      end if;
      return Is_Referenced_From (Referenced_Unit, Base_Unit);
   end Is_Visible_From;

   ----------
   -- Unit --
   ----------

   function Unit (Node : Bare_Gpr_Node) return Internal_Unit is
   begin
      return Node.Unit;
   end Unit;

   function Lookup_Internal
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Bare_Gpr_Node;
   procedure Lookup_Relative
     (Node       : Bare_Gpr_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Gpr_Node);
   --  Implementation helpers for the looking up process

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Node, Parent : Bare_Gpr_Node)
   is
   begin
      if Node = null then
         return;
      end if;

      Node.Parent := Bare_Gpr_Node (Parent);

      for I in 1 .. Children_Count (Node) loop
         Set_Parents (Child (Node, I), Node);
      end loop;
   end Set_Parents;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : Bare_Gpr_Node) is
   begin
      if Node = null then
         return;
      end if;

      Free_User_Fields (Node);
      for I in 1 .. Children_Count (Node) loop
         Destroy (Child (Node, I));
      end loop;
   end Destroy;

   -----------
   -- Child --
   -----------

   function Child (Node  : Bare_Gpr_Node;
                   Index : Positive) return Bare_Gpr_Node
   is
      Result          : Bare_Gpr_Node;
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return Result;
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node)
              return Visit_Status)
     return Visit_Status
   is
      Status : Visit_Status := Into;

   begin
      if Node /= null then
         Status := Visit (Node);

         --  Skip processing the child nodes if the returned status is Over
         --  or Stop. In the former case the previous call to Visit has taken
         --  care of processing the needed childs, and in the latter case we
         --  must immediately stop processing the tree.

         if Status = Into then
            for I in 1 .. Children_Count (Node) loop
               declare
                  Cur_Child : constant Bare_Gpr_Node :=
                     Child (Node, I);

               begin
                  if Cur_Child /= null then
                     Status := Traverse (Cur_Child, Visit);
                     exit when Status /= Into;
                  end if;
               end;
            end loop;
         end if;
      end if;

      if Status = Stop then
         return Stop;

      --  At this stage the Over status has no sense and we just continue
      --  processing the tree.

      else
         return Into;
      end if;
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   ------------------------
   -- Traverse_With_Data --
   ------------------------

   function Traverse_With_Data
     (Node  : Bare_Gpr_Node;
      Visit : access function (Node : Bare_Gpr_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : Bare_Gpr_Node) return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : Bare_Gpr_Node) return Visit_Status is
      begin
         return Visit (Node, Data);
      end Helper;

      Saved_Data : Data_Type;
      Result     : Visit_Status;

   begin
      if Reset_After_Traversal then
         Saved_Data := Data;
      end if;
      Result := Traverse (Node, Helper'Access);
      if Reset_After_Traversal then
         Data := Saved_Data;
      end if;
      return Result;
   end Traverse_With_Data;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Bare_Gpr_Node) return Source_Location_Range
   is
      type Token_Anchor is (T_Start, T_End);
      type Token_Pos is record
         Pos    : Token_Index;
         Anchor : Token_Anchor;
      end record;

      TDH                    : Token_Data_Handler renames Node.Unit.TDH;
      Token_Start, Token_End : Token_Pos;

      function Get (Index : Token_Index) return Stored_Token_Data is
        (Get_Token (TDH, Index));

      function Sloc (T : Token_Pos) return Source_Location is
        (if T.Anchor = T_Start
         then Sloc_Start (TDH, Get (T.Pos))
         else Sloc_End (TDH, Get (T.Pos)));

   begin
      if Is_Synthetic (Node) then
         return Sloc_Range (Node.Parent);
      end if;

      if Is_Ghost (Node) then
         Token_Start := (if Node.Token_Start_Index = 1
                         then (1, T_Start)
                         else (Node.Token_Start_Index - 1, T_End));
         Token_End := Token_Start;
      else
         Token_Start := (Node.Token_Start_Index, T_Start);
         Token_End := (Node.Token_End_Index, T_End);
      end if;

      if Snaps_At_Start (Node)
         and then not Is_Ghost (Node)
         and then Token_Start.Pos /= 1
      then
         Token_Start := (Token_Start.Pos - 1, T_End);
      end if;

      if Snaps_At_End (Node) and then Token_End.Pos /= Last_Token (TDH) then
         Token_End := (Token_End.Pos + 1, T_Start);
      end if;

      return Make_Range (Sloc (Token_Start), Sloc (Token_End));
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Bare_Gpr_Node
   is
      Position : Relative_Position;
      Result   : Bare_Gpr_Node;
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (Bare_Gpr_Node (Node), Sloc, Position, Result);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Bare_Gpr_Node
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node), Sloc) = Inside);

      Children : constant Internal_Bare_Gpr_Node_Array :=
         Implementation.Children (Node);
      Pos      : Relative_Position;
      Result   : Bare_Gpr_Node;
   begin
      --  Look for a child node that contains Sloc (i.e. return the most
      --  precise result).

      for Child of Children loop
         --  Note that we assume here that child nodes are ordered so that the
         --  first one has a sloc range that is before the sloc range of the
         --  second child node, etc.

         if Child /= null then
            Lookup_Relative (Child, Sloc, Pos, Result);
            case Pos is
               when Before =>
                   --  If this is the first node, Sloc is before it, so we can
                   --  stop here.  Otherwise, Sloc is between the previous
                   --  child node and the next one...  so we can stop here,
                   --  too.
                   return Node;

               when Inside =>
                   return Result;

               when After =>
                   --  Sloc is after the current child node, so see with the
                   --  next one.
                   null;
            end case;
         end if;
      end loop;

      --  If we reach this point, we found no children that covers Sloc, but
      --  Node still covers it (see the assertion).
      return Node;
   end Lookup_Internal;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Bare_Gpr_Node;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : Bare_Gpr_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Gpr_Node)
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc)
                     else null);
   end Lookup_Relative;

   -------------
   -- Compare --
   -------------

   function Compare
     (Self, Left, Right : Bare_Gpr_Node;
      Relation          : Comparison_Relation) return Boolean
   is
      LS, RS : Source_Location;
   begin
      if Left = null or else Right = null or else Left.Unit /= Right.Unit then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "invalid node comparison");
      end if;

      LS := Start_Sloc (Sloc_Range (Left));
      RS := Start_Sloc (Sloc_Range (Right));
      return (case Relation is
              when Gpr_Parser_Support.Types.Less_Than        => LS < RS,
              when Gpr_Parser_Support.Types.Less_Or_Equal    => LS <= RS,
              when Gpr_Parser_Support.Types.Greater_Than     => LS > RS,
              when Gpr_Parser_Support.Types.Greater_Or_Equal => LS >= RS);
   end Compare;

   --------------
   -- Children --
   --------------

   function Children
     (Node : Bare_Gpr_Node) return Internal_Bare_Gpr_Node_Array
   is
      First : constant Integer := Bare_Gpr_Node_Vectors.Index_Type'First;
      Last  : constant Integer := First + Children_Count (Node) - 1;
   begin
      return A : Internal_Bare_Gpr_Node_Array (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : Bare_Gpr_Node) return Bare_Gpr_Node_Array_Access
   is
      C : Internal_Bare_Gpr_Node_Array := Children (Node);
   begin
      return Ret : Bare_Gpr_Node_Array_Access :=
         Create_Bare_Gpr_Node_Array (C'Length)
      do
         Ret.Items := C;
      end return;
   end Children;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : Bare_Gpr_Node;
      Node    : Bare_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Gpr_Node
   is
      function Length (Node : Bare_Base_List) return Natural
      is (Node.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Absolute_Get
        (L     : Bare_Base_List;
         Index : Integer) return Bare_Gpr_Node
      is (L.Nodes.all (Index + 1));
      --  L.Nodes is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Bare_Gpr_Node,
         Sequence_Type => Bare_Base_List,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Gpr_Node;
   begin
      if Node = null and then Or_Null then
         return null;
      elsif Relative_Get (Node, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out-of-bounds AST list access");
      end if;
   end Get;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node        : Bare_Gpr_Node;
      Line_Prefix : String := "")
   is
      Children_Prefix : constant String := Line_Prefix & "|  ";
   begin
      if Node = null then
         Put_Line (Line_Prefix & "None");
         return;
      end if;
      Put_Line (Line_Prefix & Kind_Name (Node));
      for C of Children_And_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Image (Text (C.Trivia)));
            when Child =>
               PP_Trivia (C.Node, Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   function Populate_Lexical_Env (Node : Bare_Gpr_Node) return Boolean is

      Context    : constant Internal_Context := Node.Unit.Context;
      Unit_State : aliased PLE_Unit_State := (Named_Envs_Needing_Update => <>);
      Root_State : constant PLE_Node_State :=
        (Unit_State  => Unit_State'Unchecked_Access,
         Current_Env => Context.Root_Scope,
         Current_NED => null);

      function Populate_Internal
        (Node         : Bare_Gpr_Node;
         Parent_State : PLE_Node_State) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      procedure Register_Foreign_Env
        (Node : Bare_Gpr_Node; State : PLE_Node_State);
      --  Given a node and its PLE state, register Node.Self_Env as being
      --  initialized through the named environment mechanism, if that's indeed
      --  the case. Do nothing otherwise.

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node         : Bare_Gpr_Node;
         Parent_State : PLE_Node_State) return Boolean
      is
         Result : Boolean := False;
         State  : PLE_Node_State := Parent_State;
      begin
         if Node = null then
            return Result;
         end if;

         --  By default (i.e. unless env actions add a new env), the
         --  environment we store in Node is the current one.
         Node.Self_Env := State.Current_Env;

         --  Run pre/post actions, and run PLE on children in between. Make
         --  sure we register the potential foreign Node.Self_Env environment
         --  at the end, even when an exception interrupts PLE to keep the
         --  state consistent.
         begin
            Pre_Env_Actions (Node, State);
            if State.Current_Env /= Null_Lexical_Env then
               Node.Self_Env := State.Current_Env;
               Register_Foreign_Env (Node, State);
            end if;

            --  Call recursively on children
            for I in First_Child_Index (Node) .. Last_Child_Index (Node) loop
               Result := Populate_Internal
                 (Child (Node, I), State) or else Result;
            end loop;

            Post_Env_Actions (Node, State);
         exception
            when Exc : Property_Error =>
               if PLE_Errors_Trace.Is_Active then
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      "Exception raised during PLE "
                      & Ada.Exceptions.Exception_Name (Exc) & " : "
                      & Ada.Exceptions.Exception_Message (Exc));
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      GNAT.Traceback.Symbolic.Symbolic_Traceback (Exc));
               end if;
               Register_Foreign_Env (Node, State);
               return True;
         end;

         return Result;
      end Populate_Internal;

      --------------------------
      -- Register_Foreign_Env --
      --------------------------

      procedure Register_Foreign_Env
        (Node : Bare_Gpr_Node; State : PLE_Node_State) is
      begin
         if State.Current_NED /= null then
            State.Current_NED.Nodes_With_Foreign_Env.Insert (Node);
            Node.Unit.Nodes_With_Foreign_Env.Insert (Node, State.Current_NED);
         end if;
      end Register_Foreign_Env;

   begin
      --  This is intended to be called on the root node only (when there is no
      --  PLE root) or on a PLE root (child of the root node with a specific
      --  kind).
      if
         Node.Parent /= null
      then
         raise Program_Error;
      end if;

      return Result : constant Boolean :=
         Populate_Internal (Node, Root_State)
      do
         Update_Named_Envs (Context, Unit_State.Named_Envs_Needing_Update);
      end return;
   end Populate_Lexical_Env;

   ------------------------------
   -- AST_Envs_Node_Text_Image --
   ------------------------------

   function AST_Envs_Node_Text_Image
     (Node  : Bare_Gpr_Node;
      Short : Boolean := True) return Text_Type is
   begin
      if Short then
         if Node = null then
            return "null";
         end if;
         return To_Text (Basename (Node.Unit))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node))));
      else
         return Short_Text_Image (Node);
      end if;
   end AST_Envs_Node_Text_Image;

   -------------------
   -- Is_Rebindable --
   -------------------

   function Is_Rebindable (Node : Bare_Gpr_Node) return Boolean is
   begin
      
         pragma Unreferenced (Node);
         return True;
   end Is_Rebindable;

   -----------------------
   -- Acquire_Rebinding --
   -----------------------

   function Acquire_Rebinding
     (Node             : Bare_Gpr_Node;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
   is
      Result    : Env_Rebindings;
      Available : Env_Rebindings_Vectors.Vector renames
         Node.Unit.Context.Available_Rebindings;
   begin
      --  Use an existing and available Env_Rebindings_Type record for Node's
      --  Context, otherwise allocate a new rebinding.
      Result := (if Available.Is_Empty
                 then new Env_Rebindings_Type'(Version => 0, others => <>)
                 else Available.Pop);

      Result.Parent := Parent;
      Result.Old_Env := Old_Env;
      Result.New_Env := New_Env;
      Result.Children := Env_Rebindings_Vectors.Empty_Vector;
      return Result;
   end Acquire_Rebinding;

   -----------------------
   -- Release_Rebinding --
   -----------------------

   procedure Release_Rebinding (Self : in out Env_Rebindings) is
      Available : Env_Rebindings_Vectors.Vector renames
         Unwrap (Self.Old_Env).Node.Unit.Context.Available_Rebindings;
   begin
      --  Bumping the version number, to invalidate existing references to
      --  Self.
      Self.Version := Self.Version + 1;

      Self.Children.Destroy;
      Available.Append (Self);
      Self := null;
   end Release_Rebinding;

   ------------------------
   -- Register_Rebinding --
   ------------------------

   procedure Register_Rebinding
     (Node : Bare_Gpr_Node; Rebinding : Env_Rebindings) is
   begin
      Node.Unit.Rebindings.Append (Rebinding);
   end Register_Rebinding;

   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : Bare_Gpr_Node) return Bare_Gpr_Node
   is (Node.Parent);

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : Bare_Gpr_Node) return Generic_Unit_Ptr is
   begin
      return Convert_Unit (Node.Unit);
   end Node_Unit;

   ----------
   -- Hash --
   ----------

   function Hash (Node : Bare_Gpr_Node) return Hash_Type
   is
      function H is new Hash_Access
        (Root_Node_Record, Bare_Gpr_Node);
   begin
      return H (Node);
   end Hash;

      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));





   ------------------------
   -- Named environments --
   ------------------------

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node)
   is
      use NED_Assoc_Maps;

      Pos   : Cursor;
      Dummy : Boolean;
   begin
      --  Make sure there is a vector entry for Key
      Self.Insert (Key, Internal_Map_Node_Vectors.Empty_Vector, Pos, Dummy);

      --  Append Node to that vector
      declare
         V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Pos);
      begin
         V.Append (Node);
      end;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : Bare_Gpr_Node)
   is
      use NED_Assoc_Maps;

      V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Key);
   begin
      --  Remove the (assumed unique) entry in V whose node is Node. The order
      --  of items in V is not significant, so we can use Pop for efficient
      --  removal. Do the traversal in reverse order for correctness.
      for I in reverse 1 .. V.Length loop
         if V.Get_Access (I).Node = Node then
            V.Pop (I);
            exit;
         end if;
      end loop;
   end Remove;

   ------------------------------
   -- Get_Named_Env_Descriptor --
   ------------------------------

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access
   is
      use NED_Maps;

      --  Look for an existing entry for Name
      Pos : constant Cursor := Context.Named_Envs.Find (Name);
   begin
      if Has_Element (Pos) then
         return Element (Pos);
      end if;

      --  There is no such entry: create one
      return Result : constant Named_Env_Descriptor_Access :=
         new Named_Env_Descriptor'
           (Name                   => Name,
            Envs                   => <>,
            Env_With_Precedence    => Empty_Env,
            Foreign_Nodes          => <>,
            Foreign_Envs           => <>,
            Nodes_With_Foreign_Env => <>)
      do
         Context.Named_Envs.Insert (Name, Result);
      end return;
   end Get_Named_Env_Descriptor;

   ------------------------
   -- Register_Named_Env --
   ------------------------

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map)
   is
      NED_Access : constant Named_Env_Descriptor_Access :=
         Get_Named_Env_Descriptor (Context, Name);
      NED        : Named_Env_Descriptor renames NED_Access.all;
      Node       : constant Bare_Gpr_Node := Env_Node (Env);
   begin
      NED.Envs.Insert (Node, Env);
      Node.Unit.Named_Envs.Append ((Name, Env));

      --  If that insertion must change the env that has precedence, signal
      --  that NED requires an update.

      if NED.Envs.First_Element /= NED.Env_With_Precedence then
         Named_Envs_Needing_Update.Include (Name, NED_Access);
      end if;
   end Register_Named_Env;

   ----------------------
   -- Update_Named_Env --
   ----------------------

   procedure Update_Named_Envs
     (Context : Internal_Context; Named_Envs : NED_Maps.Map)
   is
      Require_Cache_Reset : Boolean := False;
   begin
      for Cur in Named_Envs.Iterate loop
         declare
            NE      : Named_Env_Descriptor renames NED_Maps.Element (Cur).all;
            New_Env : constant Lexical_Env :=
              (if NE.Envs.Is_Empty
               then Empty_Env
               else NE.Envs.First_Element);
         begin
            --  If there was an environment with precedence, remove its foreign
            --  nodes.
            if NE.Env_With_Precedence /= Empty_Env then
               for Cur in NE.Foreign_Nodes.Iterate loop
                  declare
                     Key   : constant Symbol_Type :=
                        NED_Assoc_Maps.Key (Cur);
                     Nodes : Internal_Map_Node_Vectors.Vector renames
                        NE.Foreign_Nodes.Reference (Cur);
                  begin
                     for N of Nodes loop
                        Remove (NE.Env_With_Precedence, Key, N.Node);
                     end loop;
                  end;
               end loop;
            end if;

            --  Now, set the new environment that has precedence
            NE.Env_With_Precedence := New_Env;

            --  Add the foreign nodes to the new environment with precedence,
            --  if any.
            for Cur in NE.Foreign_Nodes.Iterate loop
               declare
                  Key   : constant Symbol_Type :=
                     NED_Assoc_Maps.Key (Cur);
                  Nodes : Internal_Map_Node_Vectors.Vector renames
                     NE.Foreign_Nodes.Reference (Cur);
               begin
                  for N of Nodes loop
                     Add (New_Env, Key, N.Node, N.Md, N.Resolver);
                  end loop;
               end;
            end loop;

            --  Set the parent environment of all foreign environments
            for Cur in NE.Foreign_Envs.Iterate loop
               declare
                  Env : Lexical_Env_Record renames
                     Unwrap (Sorted_Env_Maps.Element (Cur)).all;
               begin
                  Env.Parent := New_Env;

                  --  We have updated the lexical env hierarchy (e.g. an env
                  --  which had no parent may have one now), so the cached
                  --  entries for queries that traveresed the old env hierarchy
                  --  need to be invalidated.
                  Require_Cache_Reset := True;
               end;
            end loop;

            --  Update nodes whose environment was the old env with precedence
            for N of NE.Nodes_With_Foreign_Env loop
               N.Self_Env := New_Env;
            end loop;
         end;
      end loop;
      if Require_Cache_Reset then
         Invalidate_Caches (Context, Invalidate_Envs => True);
      end if;
   end Update_Named_Envs;

   --------------------------
   -- Big integers wrapper --
   --------------------------

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type
   is
      use GNATCOLL.GMP;
      use GNATCOLL.GMP.Integers;
   begin
      return new Big_Integer_Record'(Value     => Make (Image, Int (Base)),
                                     Ref_Count => 1);
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type
   is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (Big_Int);
      return Result;
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Int : Integer) return Big_Integer_Type is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (GNATCOLL.GMP.Long (Int));
      return Result;
   end Create_Big_Integer;

   -------------------------------
   -- Create_Public_Big_Integer --
   -------------------------------

   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer is
   begin
      return Result : GNATCOLL.GMP.Integers.Big_Integer do
         Result.Set (Big_Int.Value);
      end return;
   end Create_Public_Big_Integer;

   -----------------
   -- Trace_Image --
   -----------------

   function Trace_Image (I : Big_Integer_Type) return String is
   begin
      return GNATCOLL.GMP.Integers.Image (I.Value);
   end Trace_Image;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Self    : Bare_Gpr_Node;
      Big_Int : Big_Integer_Type) return Integer
   is
      Image : constant String := Big_Int.Value.Image;
   begin
      return Integer'Value (Image);
   exception
      when Constraint_Error =>
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out of range big integer");
   end To_Integer;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Big_Int : Big_Integer_Type) is
   begin
      if Big_Int.Ref_Count /= -1 then
         Big_Int.Ref_Count := Big_Int.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Big_Int : in out Big_Integer_Type) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Big_Integer_Record, Big_Integer_Type);
   begin
      if Big_Int = null or else Big_Int.Ref_Count = -1 then
         return;
      end if;

      Big_Int.Ref_Count := Big_Int.Ref_Count - 1;
      if Big_Int.Ref_Count = 0 then
         Destroy (Big_Int);
      end if;
   end Dec_Ref;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value = Right.Value;
   end Equivalent;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value < Right.Value;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value <= Right.Value;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value > Right.Value;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value >= Right.Value;
   end ">=";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value + Right.Value);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value - Right.Value);
   end "-";

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number is
   begin
      return Convert_Unit (Unit).Unit_Version;
   end Unit_Version;

   -------------------------
   -- Get_Context_Version --
   -------------------------

   function Get_Context_Version
     (Node : Bare_Gpr_Node) return Version_Number is
   begin
      return Node.Unit.Context.Cache_Version;
   end Get_Context_Version;

   --------------------------
   -- Properties_May_Raise --
   --------------------------

   function Properties_May_Raise
     (Exc : Ada.Exceptions.Exception_Occurrence) return Boolean is
   begin
      return Ada.Exceptions.Exception_Identity (Exc) in
            Property_Error'Identity
      ;
   end Properties_May_Raise;

   ----------------------
   -- Short_Text_Image --
   ----------------------

   function Short_Text_Image (Self : Bare_Gpr_Node) return Text_Type
   is
   begin
      if Self = null then
         return "None";
      end if;

      
   

   case Self.Kind is
      when others => 
         return "<" & To_Text (Kind_Name (Self))
                & " "
                & To_Text
                  (Ada.Directories.Simple_Name
                     (Get_Filename (Unit (Self))))
                & ":" & To_Text (Image (Sloc_Range (Self))) & ">";
      
   end case;

   end Short_Text_Image;

   --------------------
   -- Snaps_At_Start --
   --------------------

   function Snaps_At_Start (Self : Bare_Gpr_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return False;
      
   end case;

   end Snaps_At_Start;

   ------------------
   -- Snaps_At_End --
   ------------------

   function Snaps_At_End (Self : Bare_Gpr_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return Is_Incomplete (Self);
      
   end case;

   end Snaps_At_End;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Gpr_Node;
      With_Self : Boolean := True)
      return Bare_Gpr_Node_Array_Access
   is
      Count : Natural := 0;
      Start : Bare_Gpr_Node :=
        (if With_Self then Node else Node.Parent);
      Cur   : Bare_Gpr_Node := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant Bare_Gpr_Node_Array_Access :=
            Create_Bare_Gpr_Node_Array (Count);
      begin
         Cur := Start;
         for I in Result.Items'Range loop
            Result.Items (I) := Cur;
            Cur := Cur.Parent;
         end loop;
         return Result;
      end;
   end Parents;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index (Node : Bare_Gpr_Node) return Natural
   is (1);

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index (Node : Bare_Gpr_Node) return Natural
   is (Children_Count (Node));

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Bare_Gpr_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Gpr_Node)
   is
      K : constant Gpr_Node_Kind_Type := Node.Kind;
   begin
      

      Index_In_Bounds := True;
      Result := null;
      case Gpr_Gpr_Node (K) is
when Gpr_Ada_Access_Subp_Range =>
declare
N_Bare_Ada_Access_Subp : constant Bare_Ada_Access_Subp := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Access_Subp.Ada_Access_Subp_F_Subp_Kind;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_Access_Subp.Ada_Access_Subp_F_Skips;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Pragma_Range =>
declare
N_Bare_Ada_Pragma : constant Bare_Ada_Pragma := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Pragma.Ada_Pragma_F_Skips;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Use_Range =>
declare
N_Bare_Ada_Use : constant Bare_Ada_Use := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Use.Ada_Use_F_Skips;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_With_Range =>
declare
N_Bare_Ada_With : constant Bare_Ada_With := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_With.Ada_With_F_Has_Limited;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_With.Ada_With_F_Has_Private;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Ada_With.Ada_With_F_Packages;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Generic_Range =>
declare
N_Bare_Ada_Generic : constant Bare_Ada_Generic := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Generic.Ada_Generic_F_Skips;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Library_Item_Range =>
declare
N_Bare_Ada_Library_Item : constant Bare_Ada_Library_Item := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Library_Item.Ada_Library_Item_F_Generic_Stub;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_Library_Item.Ada_Library_Item_F_Separate;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Ada_Library_Item.Ada_Library_Item_F_Main;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Pkg_Range =>
declare
N_Bare_Ada_Pkg : constant Bare_Ada_Pkg := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Pkg.Ada_Pkg_F_Has_Private;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_Pkg.Ada_Pkg_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Pkg_Body_Range =>
declare
N_Bare_Ada_Pkg_Body : constant Bare_Ada_Pkg_Body := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Pkg_Body.Ada_Pkg_Body_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Subp_Range =>
declare
N_Bare_Ada_Subp : constant Bare_Ada_Subp := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Subp.Ada_Subp_F_Subp_Kind;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_Subp.Ada_Subp_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Prelude_Range =>
declare
N_Bare_Ada_Prelude : constant Bare_Ada_Prelude := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Prelude.Ada_Prelude_F_Context_Clauses;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_Prelude.Ada_Prelude_F_Library_Item;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_Separate_Range =>
declare
N_Bare_Ada_Separate : constant Bare_Ada_Separate := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_Separate.Ada_Separate_F_Parent_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Ada_With_Formal_Range =>
declare
N_Bare_Ada_With_Formal : constant Bare_Ada_With_Formal := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ada_With_Formal.Ada_With_Formal_F_Kind;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Ada_With_Formal.Ada_With_Formal_F_Skips;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Attribute_Decl_Range =>
declare
N_Bare_Attribute_Decl : constant Bare_Attribute_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Attribute_Decl.Attribute_Decl_F_Attr_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Attribute_Decl.Attribute_Decl_F_Attr_Index;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Attribute_Decl.Attribute_Decl_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Attribute_Reference_Range =>
declare
N_Bare_Attribute_Reference : constant Bare_Attribute_Reference := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Attribute_Reference.Attribute_Reference_F_Attribute_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Attribute_Reference.Attribute_Reference_F_Attribute_Index;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Base_List =>
declare
N_Bare_Base_List : constant Bare_Base_List := Node;
begin

                    if Index > N_Bare_Base_List.Count then
                        Index_In_Bounds := False;
                    else
                        Result := N_Bare_Base_List.Nodes (Index);
                    end if;
                    return;
                
end;
when Gpr_Builtin_Function_Call_Range =>
declare
N_Bare_Builtin_Function_Call : constant Bare_Builtin_Function_Call := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Builtin_Function_Call.Builtin_Function_Call_F_Function_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Builtin_Function_Call.Builtin_Function_Call_F_Parameters;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Case_Construction_Range =>
declare
N_Bare_Case_Construction : constant Bare_Case_Construction := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Case_Construction.Case_Construction_F_Var_Ref;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Case_Construction.Case_Construction_F_Items;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Case_Item_Range =>
declare
N_Bare_Case_Item : constant Bare_Case_Item := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Case_Item.Case_Item_F_Choice;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Case_Item.Case_Item_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Compilation_Unit_Range =>
declare
N_Bare_Compilation_Unit : constant Bare_Compilation_Unit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Compilation_Unit.Compilation_Unit_F_Project;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Prefix_Range =>
declare
N_Bare_Prefix : constant Bare_Prefix := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Prefix.Prefix_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Prefix.Prefix_F_Suffix;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Package_Decl_Range =>
declare
N_Bare_Package_Decl : constant Bare_Package_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Package_Decl.Package_Decl_F_Pkg_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Package_Decl.Package_Decl_F_Pkg_Spec;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Package_Extension_Range =>
declare
N_Bare_Package_Extension : constant Bare_Package_Extension := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Package_Extension.Package_Extension_F_Extended_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Package_Renaming_Range =>
declare
N_Bare_Package_Renaming : constant Bare_Package_Renaming := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Package_Renaming.Package_Renaming_F_Renamed_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Package_Spec_Range =>
declare
N_Bare_Package_Spec : constant Bare_Package_Spec := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Package_Spec.Package_Spec_F_Extension;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Package_Spec.Package_Spec_F_Decls;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Package_Spec.Package_Spec_F_End_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Project_Range =>
declare
N_Bare_Project : constant Bare_Project := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Project.Project_F_Context_Clauses;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Project.Project_F_Project_Decl;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Project_Declaration_Range =>
declare
N_Bare_Project_Declaration : constant Bare_Project_Declaration := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Project_Declaration.Project_Declaration_F_Qualifier;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Project_Declaration.Project_Declaration_F_Project_Name;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Project_Declaration.Project_Declaration_F_Extension;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Project_Declaration.Project_Declaration_F_Decls;
                            return;
                    

                        when 5 =>
                            Result := N_Bare_Project_Declaration.Project_Declaration_F_End_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Project_Extension_Range =>
declare
N_Bare_Project_Extension : constant Bare_Project_Extension := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Project_Extension.Project_Extension_F_Is_All;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Project_Extension.Project_Extension_F_Path_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_String_Literal_At_Range =>
declare
N_Bare_String_Literal_At : constant Bare_String_Literal_At := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_String_Literal_At.String_Literal_At_F_Str_Lit;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_String_Literal_At.String_Literal_At_F_At_Lit;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Terms_Range =>
declare
N_Bare_Terms : constant Bare_Terms := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Terms.Terms_F_Terms;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Type_Reference_Range =>
declare
N_Bare_Type_Reference : constant Bare_Type_Reference := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Type_Reference.Type_Reference_F_Var_Type_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Typed_String_Decl_Range =>
declare
N_Bare_Typed_String_Decl : constant Bare_Typed_String_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Typed_String_Decl.Typed_String_Decl_F_Type_Id;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Typed_String_Decl.Typed_String_Decl_F_String_Literals;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Variable_Decl_Range =>
declare
N_Bare_Variable_Decl : constant Bare_Variable_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Variable_Decl.Variable_Decl_F_Var_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Variable_Decl.Variable_Decl_F_Var_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Variable_Decl.Variable_Decl_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_Variable_Reference_Range =>
declare
N_Bare_Variable_Reference : constant Bare_Variable_Reference := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Variable_Reference.Variable_Reference_F_Variable_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Variable_Reference.Variable_Reference_F_Attribute_Ref;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Gpr_With_Decl_Range =>
declare
N_Bare_With_Decl : constant Bare_With_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_With_Decl.With_Decl_F_Is_Limited;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_With_Decl.With_Decl_F_Path_Names;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when others => null;
end case;

      --  Execution should reach this point iff nothing matched this index, so
      --  we must be out of bounds.
      Index_In_Bounds := False;
   end Get_Child;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Gpr_Parser_Support.Generic_API.Analysis.Lk_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      use Gpr_Parser_Support.Generic_API.Analysis;
      use Gpr_Parser_Support.Generic_API.Introspection;

      T : Type_Ref;
   begin
      if Node.Is_Null then
         Put_Line ("None");
         return;
      end if;

      T := Type_Of (Node);
      Put (Line_Prefix & Image (Node_Type_Repr_Name (T)));
      if Show_Slocs then
         Put ("[" & Image (Node.Sloc_Range) & "]");
      end if;

      if Node.Is_Incomplete then
         Put (" <<INCOMPLETE>>");
      end if;

      if Node.Is_Token_Node then
         Put_Line (": " & Image (Node.Text));

      elsif Is_List_Node (Node) then

         --  List nodes are displayed in a special way (they have no field)

         declare
            Count : constant Natural := Node.Children_Count;
            Child : Lk_Node;
         begin
            if Count = 0 then
               Put_Line (": <empty list>");
               return;
            end if;
            New_Line;

            for I in 1 .. Count loop
               Child := Node.Child (I);
               if not Child.Is_Null then
                  Print (Child, Show_Slocs, Line_Prefix & "|  ");
               end if;
            end loop;
         end;

      else
         --  This is for regular nodes: display each syntax field (i.e.
         --  non-property member).

         declare
            Attr_Prefix     : constant String := Line_Prefix & "|";
            Children_Prefix : constant String := Line_Prefix & "|  ";
            M_List          : constant Struct_Member_Ref_Array := Members (T);
            Child           : Lk_Node;
         begin
            New_Line;
            for M of M_List loop
               if not Is_Property (M) and then not Is_Null_For (M, T) then
                  Child := As_Node (Eval_Node_Member (Node, M));
                  Put (Attr_Prefix
                       & Image (Format_Name (Member_Name (M), Lower)) & ":");
                  if Child.Is_Null then
                     Put_Line (" <null>");
                  else
                     New_Line;
                     Print (Child, Show_Slocs, Children_Prefix);
                  end if;
               end if;
            end loop;
         end;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Bare_Gpr_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      Entity : constant Internal_Entity := (Node, No_Entity_Info);
   begin
      Print (To_Generic_Node (Entity), Show_Slocs, Line_Prefix);
   end Print;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Bare_Gpr_Node) return Bare_Gpr_Node is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : Bare_Gpr_Node;
      Token : Token_Reference) return Token_Index
   is
      Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
   begin
      if Node.Unit.TDH'Access /= Get_Token_TDH (Token) then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "Cannot associate a token and a node from different analysis"
            & " units");
      elsif Index.Trivia /= No_Token_Index then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "A node cannot hold trivia");
      end if;

      return Index.Token;
   end Stored_Token;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Bare_Gpr_Node) return Bare_Children_Vector
   is
      Ret_Vec : Bare_Children_Vector;
      Ctx     : Internal_Context renames Node.Unit.Context;
      TDH     : Token_Data_Handler renames Node.Unit.TDH;

      procedure Append_Trivias (First, Last : Token_Index);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      function Filter_Children
        (Parent : Bare_Gpr_Node)
         return Internal_Bare_Gpr_Node_Array;
      --  Return an array for all children in Parent that are not null

      --------------------
      -- Append_Trivias --
      --------------------

      procedure Append_Trivias (First, Last : Token_Index) is
      begin
         for I in First .. Last loop
            for D of Get_Trivias (TDH, I) loop
               Ret_Vec.Append
                 (Bare_Child_Record'
                    (Kind   => Trivia,
                     Trivia => Wrap_Token_Reference
                                 (Ctx, TDH'Access, (I, D))));
            end loop;
         end loop;
      end Append_Trivias;

      ---------------------
      -- Filter_Children --
      ---------------------

      function Filter_Children
        (Parent : Bare_Gpr_Node)
         return Internal_Bare_Gpr_Node_Array
      is
         Children : constant Internal_Bare_Gpr_Node_Array :=
            Implementation.Children (Parent);
         Result   : Internal_Bare_Gpr_Node_Array (Children'Range);
         Next     : Integer := Result'First;
      begin
         for I in Children'Range loop
            if Children (I) /= null then
               Result (Next) := Children (I);
               Next := Next + 1;
            end if;
         end loop;
         return Result (Result'First .. Next - 1);
      end Filter_Children;

      First_Child : constant Positive := 1;
      N_Children  : constant Internal_Bare_Gpr_Node_Array :=
         Filter_Children (Node);
   begin
      if N_Children'Length > 0
        and then (Node.Token_Start_Index
                    /= N_Children (First_Child).Token_Start_Index)
      then
         Append_Trivias (Node.Token_Start_Index,
                         N_Children (First_Child).Token_Start_Index - 1);
      end if;

      --  Append each node to Ret_Vec, and append trivia that follow after each
      --  non-ghost nodes.
      for I in N_Children'Range loop
         Ret_Vec.Append (Bare_Child_Record'(Child, N_Children (I)));
         if not Is_Ghost (N_Children (I)) then
            Append_Trivias (N_Children (I).Token_End_Index,
                            (if I = N_Children'Last
                             then Node.Token_End_Index - 1
                             else N_Children (I + 1).Token_Start_Index - 1));
         end if;
      end loop;

      return Ret_Vec;
   end Children_And_Trivia;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost (Node : Bare_Gpr_Node) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Node : Bare_Gpr_Node) return Boolean
   is
      LGC : Bare_Gpr_Node;
   begin
     if Is_List_Node (Node.Kind) then
        LGC := (if Last_Child_Index (Node) /= 0
                then Child (Node, Last_Child_Index (Node))
                else null);
        return LGC /= null and then Is_Incomplete (LGC);
      else
         return Node.Last_Attempted_Child > -1;
      end if;
   end;

   -----------------
   -- Token_Start --
   -----------------

   function Token_Start (Node : Bare_Gpr_Node) return Token_Reference
   is (Token (Node, Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End (Node : Bare_Gpr_Node) return Token_Reference
   is
     (if Node.Token_End_Index = No_Token_Index
      then Token_Start (Node)
      else Token (Node, Node.Token_End_Index));

   -----------
   -- Token --
   -----------

   function Token
     (Node  : Bare_Gpr_Node;
      Index : Token_Index) return Token_Reference
   is
      Unit    : constant Internal_Unit := Node.Unit;
      Context : constant Internal_Context := Unit.Context;
   begin
      return Wrap_Token_Reference
        (Context, Token_Data (Unit), (Index, No_Token_Index));
   end Token;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Bare_Gpr_Node) return Boolean is
   begin
      --  Reject invalid inputs
      if Left /= null and Is_Synthetic (Left) then
         raise Property_Error with "left node is synthetic";
      elsif Right /= null and Is_Synthetic (Right) then
         raise Property_Error with "right node is synthetic";
      end if;

      --  Null nodes come first
      if Left = null then
         return Right /= null;
      elsif Right = null then
         return False;
      end if;

      --  So we have two non-null nodes. Sort by unit filename
      if Left.Unit < Right.Unit then
         return True;
      elsif Left.Unit /= Right.Unit then
         return False;
      end if;

      --  Both nodes come from the same unit: compare their token indexes
      if Left.Token_Start_Index < Right.Token_Start_Index then
         return True;
      elsif Left.Token_Start_Index > Right.Token_Start_Index then
         return False;
      else
         return Left.Token_End_Index < Right.Token_End_Index;
      end if;
   end "<";

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Bare_Gpr_Node) return Boolean
   is (Node = null);

   ----------
   -- Kind --
   ----------

   function Kind (Node : Bare_Gpr_Node) return Gpr_Node_Kind_Type
   is (Node.Kind);

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index (Node : Bare_Gpr_Node) return Integer
   is
      N : Bare_Gpr_Node := null;
   begin
      if Node.Parent = null then
         raise Property_Error with
            "Trying to get the child index of a root node";
      end if;

      for I in First_Child_Index (Node.Parent)
            .. Last_Child_Index (Node.Parent)
      loop
         N := Child (Node.Parent, I);
         if N = Node then
            return I - 1;
         end if;
      end loop;

      --  If we reach this point, then Node isn't a Child of Node.Parent. This
      --  is not supposed to happen.
      raise Program_Error;
   end Child_Index;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : Bare_Gpr_Node;
      Offset : Integer) return Bare_Gpr_Node is
   begin
      --  Root nodes have no sibling: handle them now to avoid invalid requests
      --  in the code below.
      if Node.Parent = null then
         return null;
      end if;

      declare
         Node_Index : constant Positive := Child_Index (Node) + 1;
         --  Child_Index is 0-based, but the Child primitive expects a 1-based
         --  index.

         Sibling_Index : constant Integer := Node_Index + Offset;
      begin
         --  Child returns null for out-of-bound indexes

         return (if Sibling_Index >= 1
                 then Child (Node.Parent, Sibling_Index)
                 else null);
      end;
   end Fetch_Sibling;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity
   is
      Sibling : constant Bare_Gpr_Node := Fetch_Sibling (Node, Offset);
   begin
      --  Don't forget to clear entity info if the result is nul

      return (if Sibling = null
              then No_Entity
              else (Sibling, E_Info));
   end Fetch_Sibling;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, -1);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, 1);
   end Next_Sibling;


   ----------------------
   -- Compare_Metadata --
   ----------------------

   --  Deactivate "not referenced" warnings because if the metadata struct has
   --  no fields, formals and temporaries won't be referenced in the two
   --  following functions.
   pragma Warnings (Off, "referenced");
   function Compare_Metadata (L, R : Internal_Metadata) return Boolean is
   begin
      return True;
   end Compare_Metadata;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Internal_Metadata) return Hash_Type is
      Ret : Hash_Type := Gpr_Parser_Support.Hashes.Initial_Hash;
   begin
      return Ret;
   end Hash;
   pragma Warnings (On, "referenced");

   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata
   is
      pragma Unreferenced (L, R);
      Ret : Internal_Metadata := No_Metadata;
   begin
      return Ret;
   end Combine;

   -------------------------------
   -- Create_Static_Lexical_Env --
   -------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Gpr_Node;
      Transitive_Parent : Boolean := False) return Lexical_Env
   is
      Unit : constant Internal_Unit :=
        (if Node = null then null else Node.Unit);
   begin
      return Result : Lexical_Env := Create_Lexical_Env
        (Parent, Node, Transitive_Parent, Convert_Unit (Unit))
      do
         if Unit /= null then
            Register_Destroyable (Unit, Unwrap (Result.Env));
         end if;
      end return;
   end Create_Static_Lexical_Env;

   ---------
   -- Get --
   ---------

   function Get
     (Self  : Bare_Gpr_Node;
      A     : AST_Envs.Entity_Array;
      Index : Integer) return Internal_Entity
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer) return Internal_Entity
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Gpr_Parser_Support.Relative_Get
        (Item_Type     => Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : Internal_Entity;
   begin
      if Relative_Get (A, Index, Result) then
         return Result;
      else
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out-of-bounds array access");
      end if;
   end Get;

   -----------
   -- Group --
   -----------

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env
   is (Group (Lexical_Env_Array (Envs.Items), Env_Md));

       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   



       

   




   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Lexical_Env
   is (Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Lexical_Env
   is
      function Get_Base_Env return Lexical_Env;
      --  Return the environment that we need to rebind before returning

      ------------------
      -- Get_Base_Env --
      ------------------

      function Get_Base_Env return Lexical_Env is
         pragma Warnings (Off, "referenced");
         function Get_Parent_Env return Lexical_Env;
         pragma Warnings (On, "referenced");

         --------------------
         -- Get_Parent_Env --
         --------------------

         function Get_Parent_Env return Lexical_Env is
            Parent : constant Lexical_Env := AST_Envs.Parent (Node.Self_Env);
         begin
            --  If Node is the root scope or the empty environment, Parent can
            --  be a wrapper around the null node. Turn this into the
            --  Empty_Env, as null envs are erroneous values in properties.
            return (if Unwrap (Parent) = null
                    then Empty_Env
                    else Parent);
         end Get_Parent_Env;

      begin
         
         return
           Node.Self_Env;
      end Get_Base_Env;

      Base_Env : Lexical_Env := Get_Base_Env;
      Result   : constant Lexical_Env := Rebind_Env (Base_Env, E_Info);
   begin
      Dec_Ref (Base_Env);
      return Result;
   end Node_Env;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      --  TODO: shed entity information as appropriate
      return (Node.Parent, E_Info);
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Gpr_Node;
      With_Self : Boolean := True;
      E_Info    : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Parents : Bare_Gpr_Node_Array_Access := Parents (Node, With_Self);
      Result       : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Parents.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Parents.Items'Range loop
         Result.Items (I) := (Bare_Parents.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Parents);
      return Result;
   end Parents;

   --------------
   -- Children --
   --------------

   function Children
     (Node   : Bare_Gpr_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Children : Bare_Gpr_Node_Array_Access := Children (Node);
      Result        : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Children.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Children.Items'Range loop
         Result.Items (I) := (Bare_Children.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Children);
      return Result;
   end Children;

   ---------------------
   -- New_Unit_String --
   ---------------------

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access
   is
      procedure Register_Destroyable_String is new Register_Destroyable_Gen
        (String, String_Access, Free);
   begin
      return Ret : String_Access := new String'(Str) do
         Register_Destroyable_String (Unit, Ret);
      end return;
   end New_Unit_String;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Gpr_Node) is

      pragma Warnings (Off, "referenced");

      procedure Assign
        (Node  : Bare_Gpr_Node;
         LV    : in out Logic_Var_Record;
         Field : String);
      --  Assign a name to the LV logic variable. Node must be the node that
      --  owns LV, and Field must be the name of the field in Node that holds
      --  LV.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Node  : Bare_Gpr_Node;
         LV    : in out Logic_Var_Record;
         Field : String) is
      begin
         LV.Dbg_Name :=
           new String'(Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant Gpr_Node_Kind_Type := Node.Kind;

      pragma Warnings (On, "referenced");

   begin
      
      pragma Unreferenced (K);
      for Child of Internal_Bare_Gpr_Node_Array'(Children (Node)) loop
         if Child /= null then
            Assign_Names_To_Logic_Vars (Child);
         end if;
      end loop;
   end Assign_Names_To_Logic_Vars;

   ----------------
   -- Text_Image --
   ----------------

   function Text_Image (Ent : Internal_Entity) return Text_Type is
   begin
      if Ent.Node /= null then
         declare
            Node_Image : constant Text_Type := Short_Text_Image (Ent.Node);
         begin
            return
            (if Ent.Info.Rebindings /= null
             then "<| "
             & Node_Image (Node_Image'First + 1 .. Node_Image'Last - 1) & " "
             & AST_Envs.Text_Image (Ent.Info.Rebindings) & " |>"
             else Node_Image);
         end;
      else
         return "None";
      end if;
   end Text_Image;

   ---------------------
   -- Full_Sloc_Image --
   ---------------------

   function Full_Sloc_Image (Node : Bare_Gpr_Node) return String_Type
   is
      Res      : constant Text_Type :=
        To_Text
          (Ada.Directories.Simple_Name
             (Get_Filename (Unit (Node))))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node)))) & ": ";
   begin
      return Create_String (Res);
   end Full_Sloc_Image;

   -----------
   -- Image --
   -----------

   function Image (Ent : Internal_Entity) return String is
      Result : constant Text_Type := Text_Image (Ent);
   begin
      return Image (Result);
   end Image;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : Bare_Gpr_Node) return Boolean is
   begin
      return Gpr_Node_P_Can_Reach (El, From);
   end Can_Reach;

   -----------------
   -- Hash_Entity --
   -----------------

   function Hash_Entity (Self : Internal_Entity) return Hash_Type is
   begin
      return Combine
        ((Hash (Self.Node), Hash (Self.Info.Rebindings), Hash (Self.Info.Md)));
   end Hash_Entity;

   --------------------
   -- Compare_Entity --
   --------------------

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean
   is
   begin
      return Left.Node = Right.Node
             and then Left.Info.Rebindings = Right.Info.Rebindings
             and then Compare_Metadata (Left.Info.Md, Right.Info.Md);
   end Compare_Entity;

   --------------------------------
   -- Create_Dynamic_Lexical_Env --
   --------------------------------

   function Create_Dynamic_Lexical_Env
     (Self              : Bare_Gpr_Node;
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean) return Lexical_Env
   is
      Unit : constant Internal_Unit := Self.Unit;
   begin
      --  This restriction is necessary to avoid relocation issues when
      --  Self.Self_Env is terminated.
      if Is_Foreign_Strict (Self.Self_Env, Self) then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "cannot create a dynamic lexical env when Self.Self_Env is"
            & " foreign");
      end if;

      return Result : constant Lexical_Env := Create_Dynamic_Lexical_Env
        (Parent            => Null_Lexical_Env,
         Node              => Self,
         Transitive_Parent => Transitive_Parent,
         Owner             => Convert_Unit (Unit),
         Assocs_Getter     => Assocs_Getter,
         Assoc_Resolver    => Assoc_Resolver)
      do
         --  Since dynamic lexical environments can only be created in lazy
         --  field initializers, it is fine to tie Result's lifetime to the
         --  its owning unit's lifetime.
         Register_Destroyable (Unit, Unwrap (Result));
      end return;
   end Create_Dynamic_Lexical_Env;

   procedure Destroy_Synthetic_Node (Node : in out Bare_Gpr_Node);
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length (Node : Bare_Base_List) return Natural
   is (if Node = null then 0 else Children_Count (Node));


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (B : Boolean) return String is
      begin
         return (if B then "True" else "False");
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (I : Integer) return String is
      begin
         return Integer'Image (I);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : Symbol_Type) return String is
      begin
         return (if S = null
                 then "None"
                 else Image (S.all, With_Quotes => True));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (C : Character_Type) return String is
         C_Str : constant Text_Type := (1 => C);
      begin
         return "'" & Image (C_Str) & "'";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : String_Type) return String is
      begin
         return Image (S.Content, With_Quotes => True);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Env : Lexical_Env) return String is
      begin
         case Env.Kind is
         when Static_Primary =>
            return "<LexicalEnv static-primary for "
                   & Trace_Image (Env_Node (Env)) & ">";
         when others =>
            return "<LexicalEnv synthetic>";
         end case;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : Env_Rebindings) return String is
      begin
         return Image (Text_Image (R));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Unit : Internal_Unit) return String is
      begin
         return "Internal_Unit (""" & Basename (Unit) & """)";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Eq : Logic_Equation) return String is
         pragma Unreferenced (Eq);
      begin
         return "<LogicEquation>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Var : Logic_Var) return String is
         pragma Unreferenced (Var);
      begin
         return "<LogicVariable>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (K : Analysis_Unit_Kind) return String is
      begin
         return Analysis_Unit_Kind'Image (K);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Self : Ref_Categories) return String is
         Result : Unbounded_String;
         First  : Boolean := True;
      begin
         Append (Result, "RefCategories(");
         for C in Ref_Category loop
            if Self (C) then
               if First then
                  First := False;
               else
                  Append (Result, ", ");
               end if;
               Append (Result, C'Image);
            end if;
         end loop;
         Append (Result, ")");
         return To_String (Result);
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Designated_Env) is
      begin
               Inc_Ref (R.Direct_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Designated_Env) is
      begin
               Dec_Ref (R.Direct_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Designated_Env) return Boolean is
      begin
         return L.Kind = R.Kind and then L.Env_Name = R.Env_Name and then Equivalent (L.Direct_Env, R.Direct_Env);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Designated_Env) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Kind => " & Trace_Image (R.Kind)
                        & ", "
                     & "Env_Name => " & Trace_Image (R.Env_Name)
                        & ", "
                     & "Direct_Env => " & Trace_Image (R.Direct_Env)
               & ")");
      end Trace_Image;


   

   




   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Metadata) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                  & "null record"
               & ")");
      end Trace_Image;


   

   




   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Info) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine ((Hash (R.Md), Hash (R.Rebindings), Hash (R.From_Rebound)));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Info) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Md => " & Trace_Image (R.Md)
                        & ", "
                     & "Rebindings => " & Trace_Image (R.Rebindings)
                        & ", "
                     & "From_Rebound => " & Trace_Image (R.From_Rebound)
               & ")");
      end Trace_Image;


   

   



      function Create_Internal_Entity
        (Node : Bare_Gpr_Node; Info : Internal_Entity_Info)
         return Internal_Entity is
      begin
         if Node = null then
            return No_Entity;
         end if;
         return (Node => Node, Info => Info);
      end;



   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Prelude_Node
        (Node : Bare_Ada_Prelude_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude_Node is
      begin
         if Node = null then
            return No_Entity_Ada_Prelude_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Prelude_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Access_Subp
        (Node : Bare_Ada_Access_Subp; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Access_Subp is
      begin
         if Node = null then
            return No_Entity_Ada_Access_Subp;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Access_Subp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Context_Clause
        (Node : Bare_Ada_Context_Clause; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Context_Clause is
      begin
         if Node = null then
            return No_Entity_Ada_Context_Clause;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Context_Clause) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_List
        (Node : Bare_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_List is
      begin
         if Node = null then
            return No_Entity_Base_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Context_Clause_List
        (Node : Bare_Ada_Context_Clause_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Context_Clause_List is
      begin
         if Node = null then
            return No_Entity_Ada_Context_Clause_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Context_Clause_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Entity_Kind
        (Node : Bare_Ada_Entity_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind is
      begin
         if Node = null then
            return No_Entity_Ada_Entity_Kind;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Entity_Kind_Function
        (Node : Bare_Ada_Entity_Kind_Function; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Function is
      begin
         if Node = null then
            return No_Entity_Ada_Entity_Kind_Function;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Function) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Entity_Kind_Package
        (Node : Bare_Ada_Entity_Kind_Package; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Package is
      begin
         if Node = null then
            return No_Entity_Ada_Entity_Kind_Package;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Package) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Entity_Kind_Procedure
        (Node : Bare_Ada_Entity_Kind_Procedure; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Entity_Kind_Procedure is
      begin
         if Node = null then
            return No_Entity_Ada_Entity_Kind_Procedure;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Entity_Kind_Procedure) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Generic
        (Node : Bare_Ada_Generic; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Generic is
      begin
         if Node = null then
            return No_Entity_Ada_Generic;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Generic) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Library_Item
        (Node : Bare_Ada_Library_Item; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Library_Item is
      begin
         if Node = null then
            return No_Entity_Ada_Library_Item;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Library_Item) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Main
        (Node : Bare_Ada_Main; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Main is
      begin
         if Node = null then
            return No_Entity_Ada_Main;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Main) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Pkg
        (Node : Bare_Ada_Pkg; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pkg is
      begin
         if Node = null then
            return No_Entity_Ada_Pkg;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Pkg) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Pkg_Body
        (Node : Bare_Ada_Pkg_Body; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pkg_Body is
      begin
         if Node = null then
            return No_Entity_Ada_Pkg_Body;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Pkg_Body) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Pragma
        (Node : Bare_Ada_Pragma; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Pragma is
      begin
         if Node = null then
            return No_Entity_Ada_Pragma;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Pragma) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Prelude
        (Node : Bare_Ada_Prelude; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude is
      begin
         if Node = null then
            return No_Entity_Ada_Prelude;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Prelude) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Prelude_Node_List
        (Node : Bare_Ada_Prelude_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Prelude_Node_List is
      begin
         if Node = null then
            return No_Entity_Ada_Prelude_Node_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Prelude_Node_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Separate
        (Node : Bare_Ada_Separate; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Separate is
      begin
         if Node = null then
            return No_Entity_Ada_Separate;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Separate) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Skip
        (Node : Bare_Ada_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Skip is
      begin
         if Node = null then
            return No_Entity_Ada_Skip;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Skip) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Skip_List
        (Node : Bare_Ada_Skip_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Skip_List is
      begin
         if Node = null then
            return No_Entity_Ada_Skip_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Skip_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Subp
        (Node : Bare_Ada_Subp; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Subp is
      begin
         if Node = null then
            return No_Entity_Ada_Subp;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Subp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_Use
        (Node : Bare_Ada_Use; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_Use is
      begin
         if Node = null then
            return No_Entity_Ada_Use;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_Use) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_With
        (Node : Bare_Ada_With; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_With is
      begin
         if Node = null then
            return No_Entity_Ada_With;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_With) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ada_With_Formal
        (Node : Bare_Ada_With_Formal; Info : Internal_Entity_Info)
         return Internal_Entity_Ada_With_Formal is
      begin
         if Node = null then
            return No_Entity_Ada_With_Formal;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ada_With_Formal) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_All_Qualifier
        (Node : Bare_All_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier is
      begin
         if Node = null then
            return No_Entity_All_Qualifier;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_All_Qualifier) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_All_Qualifier_Absent
        (Node : Bare_All_Qualifier_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier_Absent is
      begin
         if Node = null then
            return No_Entity_All_Qualifier_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_All_Qualifier_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_All_Qualifier_Present
        (Node : Bare_All_Qualifier_Present; Info : Internal_Entity_Info)
         return Internal_Entity_All_Qualifier_Present is
      begin
         if Node = null then
            return No_Entity_All_Qualifier_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_All_Qualifier_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Attribute_Decl
        (Node : Bare_Attribute_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Attribute_Decl is
      begin
         if Node = null then
            return No_Entity_Attribute_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Attribute_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Attribute_Reference
        (Node : Bare_Attribute_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Attribute_Reference is
      begin
         if Node = null then
            return No_Entity_Attribute_Reference;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Attribute_Reference) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Builtin_Function_Call
        (Node : Bare_Builtin_Function_Call; Info : Internal_Entity_Info)
         return Internal_Entity_Builtin_Function_Call is
      begin
         if Node = null then
            return No_Entity_Builtin_Function_Call;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Builtin_Function_Call) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Case_Construction
        (Node : Bare_Case_Construction; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Construction is
      begin
         if Node = null then
            return No_Entity_Case_Construction;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Case_Construction) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Case_Item
        (Node : Bare_Case_Item; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Item is
      begin
         if Node = null then
            return No_Entity_Case_Item;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Case_Item) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Case_Item_List
        (Node : Bare_Case_Item_List; Info : Internal_Entity_Info)
         return Internal_Entity_Case_Item_List is
      begin
         if Node = null then
            return No_Entity_Case_Item_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Case_Item_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Gpr_Node_List
        (Node : Bare_Gpr_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Gpr_Node_List is
      begin
         if Node = null then
            return No_Entity_Gpr_Node_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Gpr_Node_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Choices
        (Node : Bare_Choices; Info : Internal_Entity_Info)
         return Internal_Entity_Choices is
      begin
         if Node = null then
            return No_Entity_Choices;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Choices) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Compilation_Unit
        (Node : Bare_Compilation_Unit; Info : Internal_Entity_Info)
         return Internal_Entity_Compilation_Unit is
      begin
         if Node = null then
            return No_Entity_Compilation_Unit;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Compilation_Unit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Empty_Decl
        (Node : Bare_Empty_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Empty_Decl is
      begin
         if Node = null then
            return No_Entity_Empty_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Empty_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr is
      begin
         if Node = null then
            return No_Entity_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List is
      begin
         if Node = null then
            return No_Entity_Expr_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Single_Tok_Node
        (Node : Bare_Single_Tok_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Tok_Node is
      begin
         if Node = null then
            return No_Entity_Single_Tok_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Single_Tok_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Identifier
        (Node : Bare_Identifier; Info : Internal_Entity_Info)
         return Internal_Entity_Identifier is
      begin
         if Node = null then
            return No_Entity_Identifier;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Identifier) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Identifier_List
        (Node : Bare_Identifier_List; Info : Internal_Entity_Info)
         return Internal_Entity_Identifier_List is
      begin
         if Node = null then
            return No_Entity_Identifier_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Identifier_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Limited_Node
        (Node : Bare_Limited_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Node is
      begin
         if Node = null then
            return No_Entity_Limited_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Limited_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Limited_Absent
        (Node : Bare_Limited_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Absent is
      begin
         if Node = null then
            return No_Entity_Limited_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Limited_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Limited_Present
        (Node : Bare_Limited_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Limited_Present is
      begin
         if Node = null then
            return No_Entity_Limited_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Limited_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Num_Literal
        (Node : Bare_Num_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_Num_Literal is
      begin
         if Node = null then
            return No_Entity_Num_Literal;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Num_Literal) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Others_Designator
        (Node : Bare_Others_Designator; Info : Internal_Entity_Info)
         return Internal_Entity_Others_Designator is
      begin
         if Node = null then
            return No_Entity_Others_Designator;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Others_Designator) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Package_Decl
        (Node : Bare_Package_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Decl is
      begin
         if Node = null then
            return No_Entity_Package_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Package_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Package_Extension
        (Node : Bare_Package_Extension; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Extension is
      begin
         if Node = null then
            return No_Entity_Package_Extension;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Package_Extension) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Package_Renaming
        (Node : Bare_Package_Renaming; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Renaming is
      begin
         if Node = null then
            return No_Entity_Package_Renaming;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Package_Renaming) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Package_Spec
        (Node : Bare_Package_Spec; Info : Internal_Entity_Info)
         return Internal_Entity_Package_Spec is
      begin
         if Node = null then
            return No_Entity_Package_Spec;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Package_Spec) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Prefix
        (Node : Bare_Prefix; Info : Internal_Entity_Info)
         return Internal_Entity_Prefix is
      begin
         if Node = null then
            return No_Entity_Prefix;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Prefix) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Private_Node
        (Node : Bare_Private_Node; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Node is
      begin
         if Node = null then
            return No_Entity_Private_Node;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Private_Node) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Private_Absent
        (Node : Bare_Private_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Absent is
      begin
         if Node = null then
            return No_Entity_Private_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Private_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Private_Present
        (Node : Bare_Private_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Private_Present is
      begin
         if Node = null then
            return No_Entity_Private_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Private_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project
        (Node : Bare_Project; Info : Internal_Entity_Info)
         return Internal_Entity_Project is
      begin
         if Node = null then
            return No_Entity_Project;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Declaration
        (Node : Bare_Project_Declaration; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Declaration is
      begin
         if Node = null then
            return No_Entity_Project_Declaration;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Declaration) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Extension
        (Node : Bare_Project_Extension; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Extension is
      begin
         if Node = null then
            return No_Entity_Project_Extension;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Extension) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier
        (Node : Bare_Project_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Abstract
        (Node : Bare_Project_Qualifier_Abstract; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Abstract is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Abstract;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Abstract) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Aggregate
        (Node : Bare_Project_Qualifier_Aggregate; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Aggregate is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Aggregate;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Aggregate) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Aggregate_Library
        (Node : Bare_Project_Qualifier_Aggregate_Library; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Aggregate_Library is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Aggregate_Library;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Aggregate_Library) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Configuration
        (Node : Bare_Project_Qualifier_Configuration; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Configuration is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Configuration;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Configuration) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Library
        (Node : Bare_Project_Qualifier_Library; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Library is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Library;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Library) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Project_Qualifier_Standard
        (Node : Bare_Project_Qualifier_Standard; Info : Internal_Entity_Info)
         return Internal_Entity_Project_Qualifier_Standard is
      begin
         if Node = null then
            return No_Entity_Project_Qualifier_Standard;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Project_Qualifier_Standard) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Literal
        (Node : Bare_String_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal is
      begin
         if Node = null then
            return No_Entity_String_Literal;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Literal) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Literal_At
        (Node : Bare_String_Literal_At; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal_At is
      begin
         if Node = null then
            return No_Entity_String_Literal_At;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Literal_At) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Literal_List
        (Node : Bare_String_Literal_List; Info : Internal_Entity_Info)
         return Internal_Entity_String_Literal_List is
      begin
         if Node = null then
            return No_Entity_String_Literal_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Literal_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Term_List
        (Node : Bare_Term_List; Info : Internal_Entity_Info)
         return Internal_Entity_Term_List is
      begin
         if Node = null then
            return No_Entity_Term_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Term_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Term_List_List
        (Node : Bare_Term_List_List; Info : Internal_Entity_Info)
         return Internal_Entity_Term_List_List is
      begin
         if Node = null then
            return No_Entity_Term_List_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Term_List_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Terms
        (Node : Bare_Terms; Info : Internal_Entity_Info)
         return Internal_Entity_Terms is
      begin
         if Node = null then
            return No_Entity_Terms;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Terms) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Type_Reference
        (Node : Bare_Type_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Reference is
      begin
         if Node = null then
            return No_Entity_Type_Reference;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Type_Reference) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Typed_String_Decl
        (Node : Bare_Typed_String_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Typed_String_Decl is
      begin
         if Node = null then
            return No_Entity_Typed_String_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Typed_String_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Variable_Decl
        (Node : Bare_Variable_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Variable_Decl is
      begin
         if Node = null then
            return No_Entity_Variable_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Variable_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Variable_Reference
        (Node : Bare_Variable_Reference; Info : Internal_Entity_Info)
         return Internal_Entity_Variable_Reference is
      begin
         if Node = null then
            return No_Entity_Variable_Reference;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Variable_Reference) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_With_Decl
        (Node : Bare_With_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_With_Decl is
      begin
         if Node = null then
            return No_Entity_With_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_With_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_With_Decl_List
        (Node : Bare_With_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_With_Decl_List is
      begin
         if Node = null then
            return No_Entity_With_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_With_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Env_Assoc) is
      begin
               Inc_Ref (R.Dest_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Env_Assoc) is
      begin
               Dec_Ref (R.Dest_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Env_Assoc) return Boolean is
      begin
         return L.Key = R.Key and then L.Value = R.Value and then Equivalent (L.Dest_Env, R.Dest_Env) and then L.Metadata = R.Metadata;
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => " & Trace_Image (R.Key)
                        & ", "
                     & "Value => " & Trace_Image (R.Value)
                        & ", "
                     & "Dest_Env => " & Trace_Image (R.Dest_Env)
                        & ", "
                     & "Metadata => " & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;


   

   




   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Inner_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => " & Trace_Image (R.Key)
                        & ", "
                     & "Value => " & Trace_Image (R.Value)
                        & ", "
                     & "Metadata => " & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;



   
         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         

         



      

   --
   --  Primitives for Bare_Gpr_Node
   --

   





   







--# property-start GprNode.can_reach /home/lambourg/sbx/gpr/x86_64-linux/langkit/install/lib/python3.9/site-packages/Langkit-0.1.0-py3.9.egg/langkit/compiled_types.py:0
pragma Warnings (Off, "is not referenced");
 function Gpr_Node_P_Can_Reach
  
  (Node : Bare_Gpr_Node
      ; From_Node : Bare_Gpr_Node
  )

   return Boolean
is
   Self : Bare_Gpr_Node  := Bare_Gpr_Node (Node);
      --# bind self Self

   

   --# bind from_node From_Node

   Property_Result : Boolean;

      

      Cast_Expr : Bare_Gpr_Node;
Cast_Result : Bare_Gpr_Node;
Is_Equal : Boolean;
Fld : Internal_Unit;
Fld_1 : Internal_Unit;
Is_Equal_1 : Boolean;
Not_Val : Boolean;
If_Result : Boolean;
Node_Comp : Boolean;
If_Result_1 : Boolean;
Let_Result : Boolean;



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         --# expr-start 7 '<Block at ???>' Let_Result None
--# scope-start






--# expr-start 1 '<Eq at ???>' Is_Equal None






Cast_Expr := From_Node; 



   
      Cast_Result := Cast_Expr;



Is_Equal := Cast_Result = No_Bare_Gpr_Node; 
--# expr-done 1
if Is_Equal then
   
   If_Result := True;
else
   --# expr-start 5 '<Not at ???>' Not_Val None
--# expr-start 4 '<Eq at ???>' Is_Equal_1 None
--# expr-start 2 '<FieldAccess for unit at ???>' Fld None






   

      if Self = null then
         Raise_Property_Exception
           (Self, Property_Error'Identity, "dereferencing a null access");
      end if;



Fld := Gpr_Parser.Implementation.Unit (Node => Self);
--# expr-done 2
--# expr-start 3 '<FieldAccess for unit at ???>' Fld_1 None






   

      if From_Node = null then
         Raise_Property_Exception
           (Self, Property_Error'Identity, "dereferencing a null access");
      end if;



Fld_1 := Gpr_Parser.Implementation.Unit (Node => From_Node);
--# expr-done 3
Is_Equal_1 := Fld = Fld_1; 
--# expr-done 4
Not_Val := not (Is_Equal_1); 
--# expr-done 5
   If_Result := Not_Val;
end if;



if If_Result then
   
   If_Result_1 := True;
else
   --# expr-start 6 '<OrderingTest '"'"'lt'"'"' at ???>' Node_Comp None



Node_Comp := Compare (Self, Self, From_Node, Less_Than); 
--# expr-done 6
   If_Result_1 := Node_Comp;
end if;



Let_Result := If_Result_1; 
--# end
--# expr-done 7

         Property_Result := Let_Result;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end Gpr_Node_P_Can_Reach;
--# end



   


      

   --
   --  Primitives for Bare_Ada_Prelude_Node
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Access_Subp
   --

   



      
      procedure Initialize_Fields_For_Ada_Access_Subp
        (Self : Bare_Ada_Access_Subp
         ; Ada_Access_Subp_F_Subp_Kind : Bare_Ada_Entity_Kind
         ; Ada_Access_Subp_F_Skips : Bare_Ada_Skip_List
        ) is
      begin

            Self.Ada_Access_Subp_F_Subp_Kind := Ada_Access_Subp_F_Subp_Kind;
            Self.Ada_Access_Subp_F_Skips := Ada_Access_Subp_F_Skips;
         

      end Initialize_Fields_For_Ada_Access_Subp;

      
   function Ada_Access_Subp_F_Subp_Kind
     (Node : Bare_Ada_Access_Subp) return Bare_Ada_Entity_Kind
   is
      

   begin
         
         return Node.Ada_Access_Subp_F_Subp_Kind;
      
   end;

      
   function Ada_Access_Subp_F_Skips
     (Node : Bare_Ada_Access_Subp) return Bare_Ada_Skip_List
   is
      

   begin
         
         return Node.Ada_Access_Subp_F_Skips;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Context_Clause
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Pragma
   --

   



      
      procedure Initialize_Fields_For_Ada_Pragma
        (Self : Bare_Ada_Pragma
         ; Ada_Pragma_F_Skips : Bare_Ada_Skip_List
        ) is
      begin

            Self.Ada_Pragma_F_Skips := Ada_Pragma_F_Skips;
         

      end Initialize_Fields_For_Ada_Pragma;

      
   function Ada_Pragma_F_Skips
     (Node : Bare_Ada_Pragma) return Bare_Ada_Skip_List
   is
      

   begin
         
         return Node.Ada_Pragma_F_Skips;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Use
   --

   



      
      procedure Initialize_Fields_For_Ada_Use
        (Self : Bare_Ada_Use
         ; Ada_Use_F_Skips : Bare_Ada_Skip_List
        ) is
      begin

            Self.Ada_Use_F_Skips := Ada_Use_F_Skips;
         

      end Initialize_Fields_For_Ada_Use;

      
   function Ada_Use_F_Skips
     (Node : Bare_Ada_Use) return Bare_Ada_Skip_List
   is
      

   begin
         
         return Node.Ada_Use_F_Skips;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_With
   --

   



      
      procedure Initialize_Fields_For_Ada_With
        (Self : Bare_Ada_With
         ; Ada_With_F_Has_Limited : Bare_Limited_Node
         ; Ada_With_F_Has_Private : Bare_Private_Node
         ; Ada_With_F_Packages : Bare_Expr_List
        ) is
      begin

            Self.Ada_With_F_Has_Limited := Ada_With_F_Has_Limited;
            Self.Ada_With_F_Has_Private := Ada_With_F_Has_Private;
            Self.Ada_With_F_Packages := Ada_With_F_Packages;
         

      end Initialize_Fields_For_Ada_With;

      
   function Ada_With_F_Has_Limited
     (Node : Bare_Ada_With) return Bare_Limited_Node
   is
      

   begin
         
         return Node.Ada_With_F_Has_Limited;
      
   end;

      
   function Ada_With_F_Has_Private
     (Node : Bare_Ada_With) return Bare_Private_Node
   is
      

   begin
         
         return Node.Ada_With_F_Has_Private;
      
   end;

      
   function Ada_With_F_Packages
     (Node : Bare_Ada_With) return Bare_Expr_List
   is
      

   begin
         
         return Node.Ada_With_F_Packages;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Entity_Kind
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Entity_Kind_Function
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Entity_Kind_Package
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Entity_Kind_Procedure
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Generic
   --

   



      
      procedure Initialize_Fields_For_Ada_Generic
        (Self : Bare_Ada_Generic
         ; Ada_Generic_F_Skips : Bare_Gpr_Node
        ) is
      begin

            Self.Ada_Generic_F_Skips := Ada_Generic_F_Skips;
         

      end Initialize_Fields_For_Ada_Generic;

      
   function Ada_Generic_F_Skips
     (Node : Bare_Ada_Generic) return Bare_Gpr_Node
   is
      

   begin
         
         return Node.Ada_Generic_F_Skips;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Library_Item
   --

   



      
      procedure Initialize_Fields_For_Ada_Library_Item
        (Self : Bare_Ada_Library_Item
         ; Ada_Library_Item_F_Generic_Stub : Bare_Ada_Generic
         ; Ada_Library_Item_F_Separate : Bare_Ada_Separate
         ; Ada_Library_Item_F_Main : Bare_Ada_Main
        ) is
      begin

            Self.Ada_Library_Item_F_Generic_Stub := Ada_Library_Item_F_Generic_Stub;
            Self.Ada_Library_Item_F_Separate := Ada_Library_Item_F_Separate;
            Self.Ada_Library_Item_F_Main := Ada_Library_Item_F_Main;
         

      end Initialize_Fields_For_Ada_Library_Item;

      
   function Ada_Library_Item_F_Generic_Stub
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Generic
   is
      

   begin
         
         return Node.Ada_Library_Item_F_Generic_Stub;
      
   end;

      
   function Ada_Library_Item_F_Separate
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Separate
   is
      

   begin
         
         return Node.Ada_Library_Item_F_Separate;
      
   end;

      
   function Ada_Library_Item_F_Main
     (Node : Bare_Ada_Library_Item) return Bare_Ada_Main
   is
      

   begin
         
         return Node.Ada_Library_Item_F_Main;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Main
   --

   




      
   function Ada_Main_F_Name
     (Node : Bare_Ada_Main) return Bare_Expr
   is
      

         Kind : constant Gpr_Ada_Main := Node.Kind;
   begin
         case Kind is
               when Gpr_Ada_Subp =>
                     
         return Node.Ada_Subp_F_Name;
      
               when Gpr_Ada_Pkg =>
                     
         return Node.Ada_Pkg_F_Name;
      
               when Gpr_Ada_Pkg_Body =>
                     
         return Node.Ada_Pkg_Body_F_Name;
      
         end case;
   end;




   


      

   --
   --  Primitives for Bare_Ada_Pkg
   --

   



      
      procedure Initialize_Fields_For_Ada_Pkg
        (Self : Bare_Ada_Pkg
         ; Ada_Pkg_F_Has_Private : Bare_Private_Node
         ; Ada_Pkg_F_Name : Bare_Expr
        ) is
      begin

            Self.Ada_Pkg_F_Has_Private := Ada_Pkg_F_Has_Private;
            Self.Ada_Pkg_F_Name := Ada_Pkg_F_Name;
         

      end Initialize_Fields_For_Ada_Pkg;

      
   function Ada_Pkg_F_Has_Private
     (Node : Bare_Ada_Pkg) return Bare_Private_Node
   is
      

   begin
         
         return Node.Ada_Pkg_F_Has_Private;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Pkg_Body
   --

   



      
      procedure Initialize_Fields_For_Ada_Pkg_Body
        (Self : Bare_Ada_Pkg_Body
         ; Ada_Pkg_Body_F_Name : Bare_Expr
        ) is
      begin

            Self.Ada_Pkg_Body_F_Name := Ada_Pkg_Body_F_Name;
         

      end Initialize_Fields_For_Ada_Pkg_Body;




   


      

   --
   --  Primitives for Bare_Ada_Subp
   --

   



      
      procedure Initialize_Fields_For_Ada_Subp
        (Self : Bare_Ada_Subp
         ; Ada_Subp_F_Subp_Kind : Bare_Ada_Entity_Kind
         ; Ada_Subp_F_Name : Bare_Expr
        ) is
      begin

            Self.Ada_Subp_F_Subp_Kind := Ada_Subp_F_Subp_Kind;
            Self.Ada_Subp_F_Name := Ada_Subp_F_Name;
         

      end Initialize_Fields_For_Ada_Subp;

      
   function Ada_Subp_F_Subp_Kind
     (Node : Bare_Ada_Subp) return Bare_Ada_Entity_Kind
   is
      

   begin
         
         return Node.Ada_Subp_F_Subp_Kind;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Prelude
   --

   



      
      procedure Initialize_Fields_For_Ada_Prelude
        (Self : Bare_Ada_Prelude
         ; Ada_Prelude_F_Context_Clauses : Bare_Ada_Context_Clause_List
         ; Ada_Prelude_F_Library_Item : Bare_Ada_Library_Item
        ) is
      begin

            Self.Ada_Prelude_F_Context_Clauses := Ada_Prelude_F_Context_Clauses;
            Self.Ada_Prelude_F_Library_Item := Ada_Prelude_F_Library_Item;
         

      end Initialize_Fields_For_Ada_Prelude;

      
   function Ada_Prelude_F_Context_Clauses
     (Node : Bare_Ada_Prelude) return Bare_Ada_Context_Clause_List
   is
      

   begin
         
         return Node.Ada_Prelude_F_Context_Clauses;
      
   end;

      
   function Ada_Prelude_F_Library_Item
     (Node : Bare_Ada_Prelude) return Bare_Ada_Library_Item
   is
      

   begin
         
         return Node.Ada_Prelude_F_Library_Item;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Separate
   --

   



      
      procedure Initialize_Fields_For_Ada_Separate
        (Self : Bare_Ada_Separate
         ; Ada_Separate_F_Parent_Name : Bare_Expr
        ) is
      begin

            Self.Ada_Separate_F_Parent_Name := Ada_Separate_F_Parent_Name;
         

      end Initialize_Fields_For_Ada_Separate;

      
   function Ada_Separate_F_Parent_Name
     (Node : Bare_Ada_Separate) return Bare_Expr
   is
      

   begin
         
         return Node.Ada_Separate_F_Parent_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Ada_Skip
   --

   







   


      

   --
   --  Primitives for Bare_Ada_With_Formal
   --

   



      
      procedure Initialize_Fields_For_Ada_With_Formal
        (Self : Bare_Ada_With_Formal
         ; Ada_With_Formal_F_Kind : Bare_Ada_Entity_Kind
         ; Ada_With_Formal_F_Skips : Bare_Ada_Skip_List
        ) is
      begin

            Self.Ada_With_Formal_F_Kind := Ada_With_Formal_F_Kind;
            Self.Ada_With_Formal_F_Skips := Ada_With_Formal_F_Skips;
         

      end Initialize_Fields_For_Ada_With_Formal;

      
   function Ada_With_Formal_F_Kind
     (Node : Bare_Ada_With_Formal) return Bare_Ada_Entity_Kind
   is
      

   begin
         
         return Node.Ada_With_Formal_F_Kind;
      
   end;

      
   function Ada_With_Formal_F_Skips
     (Node : Bare_Ada_With_Formal) return Bare_Ada_Skip_List
   is
      

   begin
         
         return Node.Ada_With_Formal_F_Skips;
      
   end;




   


      

   --
   --  Primitives for Bare_All_Qualifier
   --

   





   







--# property-start '[dispatcher]AllQualifier.as_bool' dispatcher
pragma Warnings (Off, "is not referenced");
 function Dispatcher_All_Qualifier_P_As_Bool
  
  (Node : Bare_All_Qualifier
  )

   return Boolean
is
   Self : Bare_All_Qualifier  := Bare_All_Qualifier (Node);
      --# bind self Self

   


   Property_Result : Boolean;



begin
   --# property-body-start

   pragma Assert (Self = Node);





      if Self = null then
         Raise_Property_Exception
           (Self, Property_Error'Identity, "dispatching on null node");
      end if;

      case Gpr_All_Qualifier (Self.Kind) is
               when Gpr_All_Qualifier_Absent =>
                  --# property-call-start AllQualifier.Absent.as_bool
                  Property_Result := All_Qualifier_Absent_P_As_Bool
                    (Self
                    );
                  --# end
               when Gpr_All_Qualifier_Present =>
                  --# property-call-start AllQualifier.Present.as_bool
                  Property_Result := All_Qualifier_Present_P_As_Bool
                    (Self
                    );
                  --# end
      end case;




   return Property_Result;
end Dispatcher_All_Qualifier_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_All_Qualifier_Absent
   --

   





   







--# property-start AllQualifier.Absent.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:9
pragma Warnings (Off, "is not referenced");
 function All_Qualifier_Absent_P_As_Bool
  
  (Node : Bare_All_Qualifier_Absent
  )

   return Boolean
is
   Self : Bare_All_Qualifier_Absent  := Bare_All_Qualifier_Absent (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := False;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end All_Qualifier_Absent_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_All_Qualifier_Present
   --

   





   







--# property-start AllQualifier.Present.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:9
pragma Warnings (Off, "is not referenced");
 function All_Qualifier_Present_P_As_Bool
  
  (Node : Bare_All_Qualifier_Present
  )

   return Boolean
is
   Self : Bare_All_Qualifier_Present  := Bare_All_Qualifier_Present (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := True;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end All_Qualifier_Present_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Attribute_Decl
   --

   



      
      procedure Initialize_Fields_For_Attribute_Decl
        (Self : Bare_Attribute_Decl
         ; Attribute_Decl_F_Attr_Name : Bare_Identifier
         ; Attribute_Decl_F_Attr_Index : Bare_Gpr_Node
         ; Attribute_Decl_F_Expr : Bare_Term_List
        ) is
      begin

            Self.Attribute_Decl_F_Attr_Name := Attribute_Decl_F_Attr_Name;
            Self.Attribute_Decl_F_Attr_Index := Attribute_Decl_F_Attr_Index;
            Self.Attribute_Decl_F_Expr := Attribute_Decl_F_Expr;
         

      end Initialize_Fields_For_Attribute_Decl;

      
   function Attribute_Decl_F_Attr_Name
     (Node : Bare_Attribute_Decl) return Bare_Identifier
   is
      

   begin
         
         return Node.Attribute_Decl_F_Attr_Name;
      
   end;

      
   function Attribute_Decl_F_Attr_Index
     (Node : Bare_Attribute_Decl) return Bare_Gpr_Node
   is
      

   begin
         
         return Node.Attribute_Decl_F_Attr_Index;
      
   end;

      
   function Attribute_Decl_F_Expr
     (Node : Bare_Attribute_Decl) return Bare_Term_List
   is
      

   begin
         
         return Node.Attribute_Decl_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Attribute_Reference
   --

   



      
      procedure Initialize_Fields_For_Attribute_Reference
        (Self : Bare_Attribute_Reference
         ; Attribute_Reference_F_Attribute_Name : Bare_Identifier
         ; Attribute_Reference_F_Attribute_Index : Bare_Gpr_Node
        ) is
      begin

            Self.Attribute_Reference_F_Attribute_Name := Attribute_Reference_F_Attribute_Name;
            Self.Attribute_Reference_F_Attribute_Index := Attribute_Reference_F_Attribute_Index;
         

      end Initialize_Fields_For_Attribute_Reference;

      
   function Attribute_Reference_F_Attribute_Name
     (Node : Bare_Attribute_Reference) return Bare_Identifier
   is
      

   begin
         
         return Node.Attribute_Reference_F_Attribute_Name;
      
   end;

      
   function Attribute_Reference_F_Attribute_Index
     (Node : Bare_Attribute_Reference) return Bare_Gpr_Node
   is
      

   begin
         
         return Node.Attribute_Reference_F_Attribute_Index;
      
   end;




   


      

   --
   --  Primitives for Bare_Base_List
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Context_Clause_List
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Prelude_Node_List
   --

   







   


      

   --
   --  Primitives for Bare_Ada_Skip_List
   --

   







   


      

   --
   --  Primitives for Bare_Case_Item_List
   --

   







   


      

   --
   --  Primitives for Bare_Expr_List
   --

   







   


      

   --
   --  Primitives for Bare_Gpr_Node_List
   --

   







   


      

   --
   --  Primitives for Bare_Choices
   --

   







   


      

   --
   --  Primitives for Bare_Term_List
   --

   







   


      

   --
   --  Primitives for Bare_Identifier_List
   --

   







   


      

   --
   --  Primitives for Bare_String_Literal_List
   --

   







   


      

   --
   --  Primitives for Bare_Term_List_List
   --

   







   


      

   --
   --  Primitives for Bare_With_Decl_List
   --

   







   


      

   --
   --  Primitives for Bare_Builtin_Function_Call
   --

   



      
      procedure Initialize_Fields_For_Builtin_Function_Call
        (Self : Bare_Builtin_Function_Call
         ; Builtin_Function_Call_F_Function_Name : Bare_Identifier
         ; Builtin_Function_Call_F_Parameters : Bare_Terms
        ) is
      begin

            Self.Builtin_Function_Call_F_Function_Name := Builtin_Function_Call_F_Function_Name;
            Self.Builtin_Function_Call_F_Parameters := Builtin_Function_Call_F_Parameters;
         

      end Initialize_Fields_For_Builtin_Function_Call;

      
   function Builtin_Function_Call_F_Function_Name
     (Node : Bare_Builtin_Function_Call) return Bare_Identifier
   is
      

   begin
         
         return Node.Builtin_Function_Call_F_Function_Name;
      
   end;

      
   function Builtin_Function_Call_F_Parameters
     (Node : Bare_Builtin_Function_Call) return Bare_Terms
   is
      

   begin
         
         return Node.Builtin_Function_Call_F_Parameters;
      
   end;




   


      

   --
   --  Primitives for Bare_Case_Construction
   --

   



      
      procedure Initialize_Fields_For_Case_Construction
        (Self : Bare_Case_Construction
         ; Case_Construction_F_Var_Ref : Bare_Variable_Reference
         ; Case_Construction_F_Items : Bare_Case_Item_List
        ) is
      begin

            Self.Case_Construction_F_Var_Ref := Case_Construction_F_Var_Ref;
            Self.Case_Construction_F_Items := Case_Construction_F_Items;
         

      end Initialize_Fields_For_Case_Construction;

      
   function Case_Construction_F_Var_Ref
     (Node : Bare_Case_Construction) return Bare_Variable_Reference
   is
      

   begin
         
         return Node.Case_Construction_F_Var_Ref;
      
   end;

      
   function Case_Construction_F_Items
     (Node : Bare_Case_Construction) return Bare_Case_Item_List
   is
      

   begin
         
         return Node.Case_Construction_F_Items;
      
   end;




   


      

   --
   --  Primitives for Bare_Case_Item
   --

   



      
      procedure Initialize_Fields_For_Case_Item
        (Self : Bare_Case_Item
         ; Case_Item_F_Choice : Bare_Choices
         ; Case_Item_F_Decls : Bare_Gpr_Node_List
        ) is
      begin

            Self.Case_Item_F_Choice := Case_Item_F_Choice;
            Self.Case_Item_F_Decls := Case_Item_F_Decls;
         

      end Initialize_Fields_For_Case_Item;

      
   function Case_Item_F_Choice
     (Node : Bare_Case_Item) return Bare_Choices
   is
      

   begin
         
         return Node.Case_Item_F_Choice;
      
   end;

      
   function Case_Item_F_Decls
     (Node : Bare_Case_Item) return Bare_Gpr_Node_List
   is
      

   begin
         
         return Node.Case_Item_F_Decls;
      
   end;




   


      

   --
   --  Primitives for Bare_Compilation_Unit
   --

   



      
      procedure Initialize_Fields_For_Compilation_Unit
        (Self : Bare_Compilation_Unit
         ; Compilation_Unit_F_Project : Bare_Project
        ) is
      begin

            Self.Compilation_Unit_F_Project := Compilation_Unit_F_Project;
         

      end Initialize_Fields_For_Compilation_Unit;

      
   function Compilation_Unit_F_Project
     (Node : Bare_Compilation_Unit) return Bare_Project
   is
      

   begin
         
         return Node.Compilation_Unit_F_Project;
      
   end;




   


      

   --
   --  Primitives for Bare_Empty_Decl
   --

   







   


      

   --
   --  Primitives for Bare_Expr
   --

   







   


      

   --
   --  Primitives for Bare_Prefix
   --

   



      
      procedure Initialize_Fields_For_Prefix
        (Self : Bare_Prefix
         ; Prefix_F_Prefix : Bare_Expr
         ; Prefix_F_Suffix : Bare_Identifier
        ) is
      begin

            Self.Prefix_F_Prefix := Prefix_F_Prefix;
            Self.Prefix_F_Suffix := Prefix_F_Suffix;
         

      end Initialize_Fields_For_Prefix;

      
   function Prefix_F_Prefix
     (Node : Bare_Prefix) return Bare_Expr
   is
      

   begin
         
         return Node.Prefix_F_Prefix;
      
   end;

      
   function Prefix_F_Suffix
     (Node : Bare_Prefix) return Bare_Identifier
   is
      

   begin
         
         return Node.Prefix_F_Suffix;
      
   end;




   


      

   --
   --  Primitives for Bare_Single_Tok_Node
   --

   







   


      

   --
   --  Primitives for Bare_Identifier
   --

   







   


      

   --
   --  Primitives for Bare_Num_Literal
   --

   







   


      

   --
   --  Primitives for Bare_String_Literal
   --

   







   


      

   --
   --  Primitives for Bare_Limited_Node
   --

   





   







--# property-start '[dispatcher]Limited.as_bool' dispatcher
pragma Warnings (Off, "is not referenced");
 function Dispatcher_Limited_Node_P_As_Bool
  
  (Node : Bare_Limited_Node
  )

   return Boolean
is
   Self : Bare_Limited_Node  := Bare_Limited_Node (Node);
      --# bind self Self

   


   Property_Result : Boolean;



begin
   --# property-body-start

   pragma Assert (Self = Node);





      if Self = null then
         Raise_Property_Exception
           (Self, Property_Error'Identity, "dispatching on null node");
      end if;

      case Gpr_Limited_Node (Self.Kind) is
               when Gpr_Limited_Absent =>
                  --# property-call-start Limited.Absent.as_bool
                  Property_Result := Limited_Absent_P_As_Bool
                    (Self
                    );
                  --# end
               when Gpr_Limited_Present =>
                  --# property-call-start Limited.Present.as_bool
                  Property_Result := Limited_Present_P_As_Bool
                    (Self
                    );
                  --# end
      end case;




   return Property_Result;
end Dispatcher_Limited_Node_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Limited_Absent
   --

   





   







--# property-start Limited.Absent.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:19
pragma Warnings (Off, "is not referenced");
 function Limited_Absent_P_As_Bool
  
  (Node : Bare_Limited_Absent
  )

   return Boolean
is
   Self : Bare_Limited_Absent  := Bare_Limited_Absent (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := False;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end Limited_Absent_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Limited_Present
   --

   





   







--# property-start Limited.Present.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:19
pragma Warnings (Off, "is not referenced");
 function Limited_Present_P_As_Bool
  
  (Node : Bare_Limited_Present
  )

   return Boolean
is
   Self : Bare_Limited_Present  := Bare_Limited_Present (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := True;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end Limited_Present_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Others_Designator
   --

   







   


      

   --
   --  Primitives for Bare_Package_Decl
   --

   



      
      procedure Initialize_Fields_For_Package_Decl
        (Self : Bare_Package_Decl
         ; Package_Decl_F_Pkg_Name : Bare_Identifier
         ; Package_Decl_F_Pkg_Spec : Bare_Gpr_Node
        ) is
      begin

            Self.Package_Decl_F_Pkg_Name := Package_Decl_F_Pkg_Name;
            Self.Package_Decl_F_Pkg_Spec := Package_Decl_F_Pkg_Spec;
         

      end Initialize_Fields_For_Package_Decl;

      
   function Package_Decl_F_Pkg_Name
     (Node : Bare_Package_Decl) return Bare_Identifier
   is
      

   begin
         
         return Node.Package_Decl_F_Pkg_Name;
      
   end;

      
   function Package_Decl_F_Pkg_Spec
     (Node : Bare_Package_Decl) return Bare_Gpr_Node
   is
      

   begin
         
         return Node.Package_Decl_F_Pkg_Spec;
      
   end;




   


      

   --
   --  Primitives for Bare_Package_Extension
   --

   



      
      procedure Initialize_Fields_For_Package_Extension
        (Self : Bare_Package_Extension
         ; Package_Extension_F_Extended_Name : Bare_Identifier_List
        ) is
      begin

            Self.Package_Extension_F_Extended_Name := Package_Extension_F_Extended_Name;
         

      end Initialize_Fields_For_Package_Extension;

      
   function Package_Extension_F_Extended_Name
     (Node : Bare_Package_Extension) return Bare_Identifier_List
   is
      

   begin
         
         return Node.Package_Extension_F_Extended_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Package_Renaming
   --

   



      
      procedure Initialize_Fields_For_Package_Renaming
        (Self : Bare_Package_Renaming
         ; Package_Renaming_F_Renamed_Name : Bare_Identifier_List
        ) is
      begin

            Self.Package_Renaming_F_Renamed_Name := Package_Renaming_F_Renamed_Name;
         

      end Initialize_Fields_For_Package_Renaming;

      
   function Package_Renaming_F_Renamed_Name
     (Node : Bare_Package_Renaming) return Bare_Identifier_List
   is
      

   begin
         
         return Node.Package_Renaming_F_Renamed_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Package_Spec
   --

   



      
      procedure Initialize_Fields_For_Package_Spec
        (Self : Bare_Package_Spec
         ; Package_Spec_F_Extension : Bare_Package_Extension
         ; Package_Spec_F_Decls : Bare_Gpr_Node_List
         ; Package_Spec_F_End_Name : Bare_Identifier
        ) is
      begin

            Self.Package_Spec_F_Extension := Package_Spec_F_Extension;
            Self.Package_Spec_F_Decls := Package_Spec_F_Decls;
            Self.Package_Spec_F_End_Name := Package_Spec_F_End_Name;
         

      end Initialize_Fields_For_Package_Spec;

      
   function Package_Spec_F_Extension
     (Node : Bare_Package_Spec) return Bare_Package_Extension
   is
      

   begin
         
         return Node.Package_Spec_F_Extension;
      
   end;

      
   function Package_Spec_F_Decls
     (Node : Bare_Package_Spec) return Bare_Gpr_Node_List
   is
      

   begin
         
         return Node.Package_Spec_F_Decls;
      
   end;

      
   function Package_Spec_F_End_Name
     (Node : Bare_Package_Spec) return Bare_Identifier
   is
      

   begin
         
         return Node.Package_Spec_F_End_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Private_Node
   --

   





   







--# property-start '[dispatcher]Private.as_bool' dispatcher
pragma Warnings (Off, "is not referenced");
 function Dispatcher_Private_Node_P_As_Bool
  
  (Node : Bare_Private_Node
  )

   return Boolean
is
   Self : Bare_Private_Node  := Bare_Private_Node (Node);
      --# bind self Self

   


   Property_Result : Boolean;



begin
   --# property-body-start

   pragma Assert (Self = Node);





      if Self = null then
         Raise_Property_Exception
           (Self, Property_Error'Identity, "dispatching on null node");
      end if;

      case Gpr_Private_Node (Self.Kind) is
               when Gpr_Private_Absent =>
                  --# property-call-start Private.Absent.as_bool
                  Property_Result := Private_Absent_P_As_Bool
                    (Self
                    );
                  --# end
               when Gpr_Private_Present =>
                  --# property-call-start Private.Present.as_bool
                  Property_Result := Private_Present_P_As_Bool
                    (Self
                    );
                  --# end
      end case;




   return Property_Result;
end Dispatcher_Private_Node_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Private_Absent
   --

   





   







--# property-start Private.Absent.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:14
pragma Warnings (Off, "is not referenced");
 function Private_Absent_P_As_Bool
  
  (Node : Bare_Private_Absent
  )

   return Boolean
is
   Self : Bare_Private_Absent  := Bare_Private_Absent (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := False;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end Private_Absent_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Private_Present
   --

   





   







--# property-start Private.Present.as_bool /media/psf/Home/src/gpr/langkit/language/parser/decl.py:14
pragma Warnings (Off, "is not referenced");
 function Private_Present_P_As_Bool
  
  (Node : Bare_Private_Present
  )

   return Boolean
is
   Self : Bare_Private_Present  := Bare_Private_Present (Node);
      --# bind self Self

   


   Property_Result : Boolean;

      

      



begin
   --# property-body-start

   pragma Assert (Self = Node);





      begin
         
   --# scope-start

         

         Property_Result := True;
         
   --# end


      exception
         when Exc : Property_Error =>



            raise;
      end;



   return Property_Result;
end Private_Present_P_As_Bool;
--# end



   


      

   --
   --  Primitives for Bare_Project
   --

   



      
      procedure Initialize_Fields_For_Project
        (Self : Bare_Project
         ; Project_F_Context_Clauses : Bare_With_Decl_List
         ; Project_F_Project_Decl : Bare_Project_Declaration
        ) is
      begin

            Self.Project_F_Context_Clauses := Project_F_Context_Clauses;
            Self.Project_F_Project_Decl := Project_F_Project_Decl;
         

      end Initialize_Fields_For_Project;

      
   function Project_F_Context_Clauses
     (Node : Bare_Project) return Bare_With_Decl_List
   is
      

   begin
         
         return Node.Project_F_Context_Clauses;
      
   end;

      
   function Project_F_Project_Decl
     (Node : Bare_Project) return Bare_Project_Declaration
   is
      

   begin
         
         return Node.Project_F_Project_Decl;
      
   end;




   


      

   --
   --  Primitives for Bare_Project_Declaration
   --

   



      
      procedure Initialize_Fields_For_Project_Declaration
        (Self : Bare_Project_Declaration
         ; Project_Declaration_F_Qualifier : Bare_Project_Qualifier
         ; Project_Declaration_F_Project_Name : Bare_Expr
         ; Project_Declaration_F_Extension : Bare_Project_Extension
         ; Project_Declaration_F_Decls : Bare_Gpr_Node_List
         ; Project_Declaration_F_End_Name : Bare_Expr
        ) is
      begin

            Self.Project_Declaration_F_Qualifier := Project_Declaration_F_Qualifier;
            Self.Project_Declaration_F_Project_Name := Project_Declaration_F_Project_Name;
            Self.Project_Declaration_F_Extension := Project_Declaration_F_Extension;
            Self.Project_Declaration_F_Decls := Project_Declaration_F_Decls;
            Self.Project_Declaration_F_End_Name := Project_Declaration_F_End_Name;
         

      end Initialize_Fields_For_Project_Declaration;

      
   function Project_Declaration_F_Qualifier
     (Node : Bare_Project_Declaration) return Bare_Project_Qualifier
   is
      

   begin
         
         return Node.Project_Declaration_F_Qualifier;
      
   end;

      
   function Project_Declaration_F_Project_Name
     (Node : Bare_Project_Declaration) return Bare_Expr
   is
      

   begin
         
         return Node.Project_Declaration_F_Project_Name;
      
   end;

      
   function Project_Declaration_F_Extension
     (Node : Bare_Project_Declaration) return Bare_Project_Extension
   is
      

   begin
         
         return Node.Project_Declaration_F_Extension;
      
   end;

      
   function Project_Declaration_F_Decls
     (Node : Bare_Project_Declaration) return Bare_Gpr_Node_List
   is
      

   begin
         
         return Node.Project_Declaration_F_Decls;
      
   end;

      
   function Project_Declaration_F_End_Name
     (Node : Bare_Project_Declaration) return Bare_Expr
   is
      

   begin
         
         return Node.Project_Declaration_F_End_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Project_Extension
   --

   



      
      procedure Initialize_Fields_For_Project_Extension
        (Self : Bare_Project_Extension
         ; Project_Extension_F_Is_All : Bare_All_Qualifier
         ; Project_Extension_F_Path_Name : Bare_String_Literal
        ) is
      begin

            Self.Project_Extension_F_Is_All := Project_Extension_F_Is_All;
            Self.Project_Extension_F_Path_Name := Project_Extension_F_Path_Name;
         

      end Initialize_Fields_For_Project_Extension;

      
   function Project_Extension_F_Is_All
     (Node : Bare_Project_Extension) return Bare_All_Qualifier
   is
      

   begin
         
         return Node.Project_Extension_F_Is_All;
      
   end;

      
   function Project_Extension_F_Path_Name
     (Node : Bare_Project_Extension) return Bare_String_Literal
   is
      

   begin
         
         return Node.Project_Extension_F_Path_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Project_Qualifier
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Abstract
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Aggregate
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Aggregate_Library
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Configuration
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Library
   --

   







   


      

   --
   --  Primitives for Bare_Project_Qualifier_Standard
   --

   







   


      

   --
   --  Primitives for Bare_String_Literal_At
   --

   



      
      procedure Initialize_Fields_For_String_Literal_At
        (Self : Bare_String_Literal_At
         ; String_Literal_At_F_Str_Lit : Bare_String_Literal
         ; String_Literal_At_F_At_Lit : Bare_Num_Literal
        ) is
      begin

            Self.String_Literal_At_F_Str_Lit := String_Literal_At_F_Str_Lit;
            Self.String_Literal_At_F_At_Lit := String_Literal_At_F_At_Lit;
         

      end Initialize_Fields_For_String_Literal_At;

      
   function String_Literal_At_F_Str_Lit
     (Node : Bare_String_Literal_At) return Bare_String_Literal
   is
      

   begin
         
         return Node.String_Literal_At_F_Str_Lit;
      
   end;

      
   function String_Literal_At_F_At_Lit
     (Node : Bare_String_Literal_At) return Bare_Num_Literal
   is
      

   begin
         
         return Node.String_Literal_At_F_At_Lit;
      
   end;




   


      

   --
   --  Primitives for Bare_Terms
   --

   



      
      procedure Initialize_Fields_For_Terms
        (Self : Bare_Terms
         ; Terms_F_Terms : Bare_Term_List_List
        ) is
      begin

            Self.Terms_F_Terms := Terms_F_Terms;
         

      end Initialize_Fields_For_Terms;

      
   function Terms_F_Terms
     (Node : Bare_Terms) return Bare_Term_List_List
   is
      

   begin
         
         return Node.Terms_F_Terms;
      
   end;




   


      

   --
   --  Primitives for Bare_Type_Reference
   --

   



      
      procedure Initialize_Fields_For_Type_Reference
        (Self : Bare_Type_Reference
         ; Type_Reference_F_Var_Type_Name : Bare_Identifier_List
        ) is
      begin

            Self.Type_Reference_F_Var_Type_Name := Type_Reference_F_Var_Type_Name;
         

      end Initialize_Fields_For_Type_Reference;

      
   function Type_Reference_F_Var_Type_Name
     (Node : Bare_Type_Reference) return Bare_Identifier_List
   is
      

   begin
         
         return Node.Type_Reference_F_Var_Type_Name;
      
   end;




   


      

   --
   --  Primitives for Bare_Typed_String_Decl
   --

   



      
      procedure Initialize_Fields_For_Typed_String_Decl
        (Self : Bare_Typed_String_Decl
         ; Typed_String_Decl_F_Type_Id : Bare_Identifier
         ; Typed_String_Decl_F_String_Literals : Bare_String_Literal_List
        ) is
      begin

            Self.Typed_String_Decl_F_Type_Id := Typed_String_Decl_F_Type_Id;
            Self.Typed_String_Decl_F_String_Literals := Typed_String_Decl_F_String_Literals;
         

      end Initialize_Fields_For_Typed_String_Decl;

      
   function Typed_String_Decl_F_Type_Id
     (Node : Bare_Typed_String_Decl) return Bare_Identifier
   is
      

   begin
         
         return Node.Typed_String_Decl_F_Type_Id;
      
   end;

      
   function Typed_String_Decl_F_String_Literals
     (Node : Bare_Typed_String_Decl) return Bare_String_Literal_List
   is
      

   begin
         
         return Node.Typed_String_Decl_F_String_Literals;
      
   end;




   


      

   --
   --  Primitives for Bare_Variable_Decl
   --

   



      
      procedure Initialize_Fields_For_Variable_Decl
        (Self : Bare_Variable_Decl
         ; Variable_Decl_F_Var_Name : Bare_Identifier
         ; Variable_Decl_F_Var_Type : Bare_Type_Reference
         ; Variable_Decl_F_Expr : Bare_Term_List
        ) is
      begin

            Self.Variable_Decl_F_Var_Name := Variable_Decl_F_Var_Name;
            Self.Variable_Decl_F_Var_Type := Variable_Decl_F_Var_Type;
            Self.Variable_Decl_F_Expr := Variable_Decl_F_Expr;
         

      end Initialize_Fields_For_Variable_Decl;

      
   function Variable_Decl_F_Var_Name
     (Node : Bare_Variable_Decl) return Bare_Identifier
   is
      

   begin
         
         return Node.Variable_Decl_F_Var_Name;
      
   end;

      
   function Variable_Decl_F_Var_Type
     (Node : Bare_Variable_Decl) return Bare_Type_Reference
   is
      

   begin
         
         return Node.Variable_Decl_F_Var_Type;
      
   end;

      
   function Variable_Decl_F_Expr
     (Node : Bare_Variable_Decl) return Bare_Term_List
   is
      

   begin
         
         return Node.Variable_Decl_F_Expr;
      
   end;




   


      

   --
   --  Primitives for Bare_Variable_Reference
   --

   



      
      procedure Initialize_Fields_For_Variable_Reference
        (Self : Bare_Variable_Reference
         ; Variable_Reference_F_Variable_Name : Bare_Identifier_List
         ; Variable_Reference_F_Attribute_Ref : Bare_Attribute_Reference
        ) is
      begin

            Self.Variable_Reference_F_Variable_Name := Variable_Reference_F_Variable_Name;
            Self.Variable_Reference_F_Attribute_Ref := Variable_Reference_F_Attribute_Ref;
         

      end Initialize_Fields_For_Variable_Reference;

      
   function Variable_Reference_F_Variable_Name
     (Node : Bare_Variable_Reference) return Bare_Identifier_List
   is
      

   begin
         
         return Node.Variable_Reference_F_Variable_Name;
      
   end;

      
   function Variable_Reference_F_Attribute_Ref
     (Node : Bare_Variable_Reference) return Bare_Attribute_Reference
   is
      

   begin
         
         return Node.Variable_Reference_F_Attribute_Ref;
      
   end;




   


      

   --
   --  Primitives for Bare_With_Decl
   --

   



      
      procedure Initialize_Fields_For_With_Decl
        (Self : Bare_With_Decl
         ; With_Decl_F_Is_Limited : Bare_Limited_Node
         ; With_Decl_F_Path_Names : Bare_String_Literal_List
        ) is
      begin

            Self.With_Decl_F_Is_Limited := With_Decl_F_Is_Limited;
            Self.With_Decl_F_Path_Names := With_Decl_F_Path_Names;
         

      end Initialize_Fields_For_With_Decl;

      
   function With_Decl_F_Is_Limited
     (Node : Bare_With_Decl) return Bare_Limited_Node
   is
      

   begin
         
         return Node.With_Decl_F_Is_Limited;
      
   end;

      
   function With_Decl_F_Path_Names
     (Node : Bare_With_Decl) return Bare_String_Literal_List
   is
      

   begin
         
         return Node.With_Decl_F_Path_Names;
      
   end;




   



   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out Bare_Gpr_Node) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Root_Node_Record, Bare_Gpr_Node);
   begin
      --  Don't call Node.Destroy, as Node's children may be gone already: they
      --  have their own destructor and there is no specified order for the
      --  call of these destructors.
      Free_User_Fields (Node);
      Free (Node);
   end Destroy_Synthetic_Node;

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image
        (Node       : Bare_Gpr_Node;
         Decoration : Boolean := True) return String is
      begin
         if Node = null then
            return "None";
         else
            declare
               Result : constant String :=
                 (Kind_Name (Node) & " "
                  & Basename (Node.Unit) & ":"
                  & Image (Sloc_Range (Node)));
            begin
               return (if Decoration then "<" & Result & ">" else Result);
            end;
         end if;
      end Trace_Image;

   Kind_Names : array (Gpr_Node_Kind_Type) of Unbounded_String :=
     (Gpr_Ada_Access_Subp => To_Unbounded_String ("AdaAccessSubp"), 
Gpr_Ada_Pragma => To_Unbounded_String ("AdaPragma"), 
Gpr_Ada_Use => To_Unbounded_String ("AdaUse"), 
Gpr_Ada_With => To_Unbounded_String ("AdaWith"), 
Gpr_Ada_Entity_Kind_Function => To_Unbounded_String ("AdaEntityKindFunction"), 
Gpr_Ada_Entity_Kind_Package => To_Unbounded_String ("AdaEntityKindPackage"), 
Gpr_Ada_Entity_Kind_Procedure => To_Unbounded_String ("AdaEntityKindProcedure"), 
Gpr_Ada_Generic => To_Unbounded_String ("AdaGeneric"), 
Gpr_Ada_Library_Item => To_Unbounded_String ("AdaLibraryItem"), 
Gpr_Ada_Pkg => To_Unbounded_String ("AdaPkg"), 
Gpr_Ada_Pkg_Body => To_Unbounded_String ("AdaPkgBody"), 
Gpr_Ada_Subp => To_Unbounded_String ("AdaSubp"), 
Gpr_Ada_Prelude => To_Unbounded_String ("AdaPrelude"), 
Gpr_Ada_Separate => To_Unbounded_String ("AdaSeparate"), 
Gpr_Ada_Skip => To_Unbounded_String ("AdaSkip"), 
Gpr_Ada_With_Formal => To_Unbounded_String ("AdaWithFormal"), 
Gpr_All_Qualifier_Absent => To_Unbounded_String ("AllQualifierAbsent"), 
Gpr_All_Qualifier_Present => To_Unbounded_String ("AllQualifierPresent"), 
Gpr_Attribute_Decl => To_Unbounded_String ("AttributeDecl"), 
Gpr_Attribute_Reference => To_Unbounded_String ("AttributeReference"), 
Gpr_Ada_Context_Clause_List => To_Unbounded_String ("AdaContextClauseList"), 
Gpr_Ada_Prelude_Node_List => To_Unbounded_String ("AdaPreludeNodeList"), 
Gpr_Ada_Skip_List => To_Unbounded_String ("AdaSkipList"), 
Gpr_Case_Item_List => To_Unbounded_String ("CaseItemList"), 
Gpr_Expr_List => To_Unbounded_String ("ExprList"), 
Gpr_Gpr_Node_List => To_Unbounded_String ("GprNodeList"), 
Gpr_Choices => To_Unbounded_String ("Choices"), 
Gpr_Term_List => To_Unbounded_String ("TermList"), 
Gpr_Identifier_List => To_Unbounded_String ("IdentifierList"), 
Gpr_String_Literal_List => To_Unbounded_String ("StringLiteralList"), 
Gpr_Term_List_List => To_Unbounded_String ("TermListList"), 
Gpr_With_Decl_List => To_Unbounded_String ("WithDeclList"), 
Gpr_Builtin_Function_Call => To_Unbounded_String ("BuiltinFunctionCall"), 
Gpr_Case_Construction => To_Unbounded_String ("CaseConstruction"), 
Gpr_Case_Item => To_Unbounded_String ("CaseItem"), 
Gpr_Compilation_Unit => To_Unbounded_String ("CompilationUnit"), 
Gpr_Empty_Decl => To_Unbounded_String ("EmptyDecl"), 
Gpr_Prefix => To_Unbounded_String ("Prefix"), 
Gpr_Identifier => To_Unbounded_String ("Id"), 
Gpr_Num_Literal => To_Unbounded_String ("Num"), 
Gpr_String_Literal => To_Unbounded_String ("Str"), 
Gpr_Limited_Absent => To_Unbounded_String ("LimitedAbsent"), 
Gpr_Limited_Present => To_Unbounded_String ("LimitedPresent"), 
Gpr_Others_Designator => To_Unbounded_String ("OthersDesignator"), 
Gpr_Package_Decl => To_Unbounded_String ("PackageDecl"), 
Gpr_Package_Extension => To_Unbounded_String ("PackageExtension"), 
Gpr_Package_Renaming => To_Unbounded_String ("PackageRenaming"), 
Gpr_Package_Spec => To_Unbounded_String ("PackageSpec"), 
Gpr_Private_Absent => To_Unbounded_String ("PrivateAbsent"), 
Gpr_Private_Present => To_Unbounded_String ("PrivatePresent"), 
Gpr_Project => To_Unbounded_String ("Project"), 
Gpr_Project_Declaration => To_Unbounded_String ("ProjectDeclaration"), 
Gpr_Project_Extension => To_Unbounded_String ("ProjectExtension"), 
Gpr_Project_Qualifier_Abstract => To_Unbounded_String ("ProjectQualifierAbstract"), 
Gpr_Project_Qualifier_Aggregate => To_Unbounded_String ("ProjectQualifierAggregate"), 
Gpr_Project_Qualifier_Aggregate_Library => To_Unbounded_String ("ProjectQualifierAggregateLibrary"), 
Gpr_Project_Qualifier_Configuration => To_Unbounded_String ("ProjectQualifierConfiguration"), 
Gpr_Project_Qualifier_Library => To_Unbounded_String ("ProjectQualifierLibrary"), 
Gpr_Project_Qualifier_Standard => To_Unbounded_String ("ProjectQualifierStandard"), 
Gpr_String_Literal_At => To_Unbounded_String ("StringLiteralAt"), 
Gpr_Terms => To_Unbounded_String ("Terms"), 
Gpr_Type_Reference => To_Unbounded_String ("TypeReference"), 
Gpr_Typed_String_Decl => To_Unbounded_String ("TypedStringDecl"), 
Gpr_Variable_Decl => To_Unbounded_String ("VariableDecl"), 
Gpr_Variable_Reference => To_Unbounded_String ("VariableReference"), 
Gpr_With_Decl => To_Unbounded_String ("WithDecl"));

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Bare_Gpr_Node) return String is
   begin
      return To_String (Kind_Names (Node.Kind));
   end Kind_Name;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Bare_Gpr_Node) return Natural is
      C : Integer := Kind_To_Node_Children_Count (Node.Kind);
   begin
      if C = -1 then
         return Node.Count;
      else
         return C;
      end if;
   end Children_Count;

   ----------------------
   -- Free_User_Fields --
   ----------------------

   procedure Free_User_Fields (Node : Bare_Gpr_Node) is

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record);
      --  Reset the LV logic variable, clearing the value it stores

      ---------------------
      -- Reset_Logic_Var --
      ---------------------

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record) is
      begin
         LV.Value := No_Entity;
         Entity_Vars.Reset (LV'Unrestricted_Access);
         Entity_Vars.Destroy (LV);
      end Reset_Logic_Var;

      K : constant Gpr_Node_Kind_Type := Node.Kind;

   begin
      
      pragma Unreferenced (K, Reset_Logic_Var);
   end Free_User_Fields;

   ----------------
   -- Token_Data --
   ----------------

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access
   is (Unit.TDH'Access);

   -------------------
   -- Lookup_Symbol --
   -------------------

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type
   is
      Canon_Symbol : constant Symbolization_Result :=
            Gpr_Parser_Support.Symbols.Fold_Case (Symbol)
      ;
   begin
      if Canon_Symbol.Success then
         return Get_Symbol
           (Context.Symbols, Find (Context.Symbols, Canon_Symbol.Symbol));
      else
         raise Invalid_Symbol_Error with Image (Canon_Symbol.Error_Message);
      end if;
   end Lookup_Symbol;

   -------------------------
   -- Create_Special_Unit --
   -------------------------

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      Unit : Internal_Unit := new Analysis_Unit_Type'
        (Context                      => Context,
         Is_Internal                  => False,
         Ast_Root                     => null,
         Filename                     => Normalized_Filename,
         Charset                      => To_Unbounded_String (Charset),
         TDH                          => <>,
         Diagnostics                  => <>,
         Rule                         => Rule,
         Ast_Mem_Pool                 => No_Pool,
         Destroyables                 => Destroyable_Vectors.Empty_Vector,
         Referenced_Units             => <>,
         Exiled_Entries               => Exiled_Entry_Vectors.Empty_Vector,
         Foreign_Nodes                =>
            Foreign_Node_Entry_Vectors.Empty_Vector,
         Exiled_Entries_In_NED        =>
            Exiled_Entry_In_NED_Vectors.Empty_Vector,
         Exiled_Envs                  => Exiled_Env_Vectors.Empty_Vector,
         Named_Envs                   => Named_Env_Vectors.Empty_Vector,
         Nodes_With_Foreign_Env       => <>,
         Rebindings                   => Env_Rebindings_Vectors.Empty_Vector,
         Cache_Version                => <>,
         Unit_Version                 => <>,
         others => <>
      );
   begin
      Initialize
        (Unit.TDH, Context.Symbols, Unit.all'Address, Context.Tab_Stop);
      return Unit;
   end Create_Special_Unit;

   --------------------
   -- Templates_Unit --
   --------------------

   function Templates_Unit (Context : Internal_Context) return Internal_Unit is
   begin
      if Context.Templates_Unit = No_Analysis_Unit then
         Context.Templates_Unit := Create_Special_Unit
           (Context             => Context,
            Normalized_Filename => No_File,
            Charset             => Default_Charset,
            Rule                => Compilation_Unit_Rule);
      end if;
      return Context.Templates_Unit;
   end Templates_Unit;

   --------------
   -- Set_Rule --
   --------------

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule) is
   begin
      Unit.Rule := Rule;
   end Set_Rule;

   ------------------------------
   -- Normalized_Unit_Filename --
   ------------------------------

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String) return Virtual_File
   is
      use Virtual_File_Maps;
      Key : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur : Cursor := Context.Filenames.Find (Key);
   begin
      if Cur = No_Element then
         declare
            F : constant Virtual_File := Create
              (Create_From_Base (+Filename).Full_Name,
               Normalize => True);
         begin
            Context.Filenames.Insert (Key, F);
            return F;
         end;
      else
         return Element (Cur);
      end if;
   end Normalized_Unit_Filename;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure)
   is
   begin
      Destroyable_Vectors.Append (Unit.Destroyables, (Object, Destroy));
   end Register_Destroyable_Helper;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : Bare_Gpr_Node)
   is
      procedure Helper is new Register_Destroyable_Gen
        (Root_Node_Record,
         Bare_Gpr_Node,
         Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access)
   is
      procedure Helper is new Register_Destroyable_Gen
        (AST_Envs.Lexical_Env_Record, AST_Envs.Lexical_Env_Access, Destroy);
   begin
      Helper (Unit, Env);
   end Register_Destroyable;

   -----------------------
   -- Invalidate_Caches --
   -----------------------

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean) is
   begin
      --  Increase Context's version number. If we are about to overflow, reset
      --  all version numbers from analysis units.
      if Context.Cache_Version = Version_Number'Last then
         Context.Cache_Version := 1;
         for Unit of Context.Units loop
            Unit.Cache_Version := 0;
         end loop;
      else
         Context.Cache_Version := Context.Cache_Version + 1;
      end if;

      if Invalidate_Envs then
         Context.Reparse_Cache_Version := Context.Cache_Version;
      end if;
   end Invalidate_Caches;

   ------------------
   --  Reset_Envs  --
   ------------------

   procedure Reset_Envs (Unit : Internal_Unit) is

      procedure Deactivate_Refd_Envs (Node : Bare_Gpr_Node);
      procedure Recompute_Refd_Envs (Node : Bare_Gpr_Node);

      --------------------------
      -- Deactivate_Refd_Envs --
      --------------------------

      procedure Deactivate_Refd_Envs (Node : Bare_Gpr_Node) is
      begin
         if Node = null then
            return;
         end if;

         Deactivate_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Deactivate_Refd_Envs (Child (Node, I));
         end loop;
      end Deactivate_Refd_Envs;

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Recompute_Refd_Envs (Node : Bare_Gpr_Node) is
      begin
         if Node = null then
            return;
         end if;
         Recompute_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Recompute_Refd_Envs (Child (Node, I));
         end loop;
      end Recompute_Refd_Envs;

   begin
      --  First pass will deactivate every referenced envs that Unit possesses
      Deactivate_Refd_Envs (Unit.Ast_Root);

      --  Second pass will recompute the env they are pointing to
      Recompute_Refd_Envs (Unit.Ast_Root);
   end Reset_Envs;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Reparsed : in out Reparsed_Unit) is
   begin
      Free (Reparsed.TDH);
      Reparsed.Diagnostics := Diagnostics_Vectors.Empty_Vector;
      Free (Reparsed.Ast_Mem_Pool);
      Reparsed.Ast_Root := null;
   end Destroy;

   --------------
   -- Basename --
   --------------

   function Basename (Filename : String) return String is
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   --------------
   -- Basename --
   --------------

   function Basename (Unit : Internal_Unit) return String is
   begin
      return +Unit.Filename.Base_Name;
   end Basename;

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Unit : Internal_Unit) is
      Cache_Version : constant Version_Number := Unit.Cache_Version;
   begin
      if Cache_Version < Unit.Context.Reparse_Cache_Version then
         Unit.Cache_Version := Unit.Context.Reparse_Cache_Version;
         Reset_Envs (Unit);
      end if;

      if Cache_Version < Unit.Context.Cache_Version then
         Unit.Cache_Version := Unit.Context.Cache_Version;
      end if;
   end Reset_Caches;

   --------------------
   -- Reference_Unit --
   --------------------

   procedure Reference_Unit (From, Referenced : Internal_Unit) is
      Dummy : Boolean;
   begin
      Dummy := Analysis_Unit_Sets.Add (From.Referenced_Units, Referenced);
   end Reference_Unit;

   ------------------------
   -- Is_Referenced_From --
   ------------------------

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean is
   begin
      if Unit = null or else Self = null then
         return False;
      elsif Unit = Self then
         return True;
      else
         return Analysis_Unit_Sets.Has (Unit.Referenced_Units, Self);
      end if;
   end Is_Referenced_From;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit)
   is
      Context  : constant Internal_Context := Unit.Context;
      Unit_TDH : constant Token_Data_Handler_Access := Token_Data (Unit);

      Saved_TDH : Token_Data_Handler;
      --  Holder to save tokens data in Unit.
      --
      --  By design, parsing is required to bind the nodes it creates to an
      --  analysis unit. However, this procedure is supposed to preserve the
      --  Unit itself and return its parsing result in Result.
      --
      --  In order to implement this, we first move "old" token data in this
      --  variable, then we do parsing. Only then, we can move "new" token data
      --  from the unit to Result, and restore the "old" token data to Unit.
      --  This last step is what Rotate_TDH (see below) is above.

      procedure Rotate_TDH;
      --  Move token data from Unit to Result and restore data in Saved_TDH to
      --  Unit.

      ----------------
      -- Rotate_TDH --
      ----------------

      procedure Rotate_TDH is
      begin
         Move (Result.TDH, Unit_TDH.all);
         Move (Unit_TDH.all, Saved_TDH);
      end Rotate_TDH;

   begin
      GNATCOLL.Traces.Trace (Main_Trace, "Parsing unit " & Basename (Unit));

      Result.Ast_Root := null;

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all,
                  Saved_TDH.Symbols,
                  Unit.all'Address,
                  Unit.Context.Tab_Stop);

      --  This is where lexing occurs, so this is where we get most "setup"
      --  issues: missing input file, bad charset, etc. If we have such an
      --  error, catch it, turn it into diagnostics and abort parsing.
      --
      --  As it is quite common, first check if the file is readable: if not,
      --  don't bother opening it and directly emit a diagnostic. This avoid
      --  pointless exceptions which harm debugging. Note that this
      --  optimization is valid only when there is no file reader, which can
      --  work even when there is no real source file.

      if Context.File_Reader = null
         and then Input.Kind = File
         and then (Input.Filename.Is_Directory
                   or else (not Input.Filename.Is_Readable))
      then
         declare
            Name : constant String := Basename (Unit);
         begin
            GNATCOLL.Traces.Trace
              (Main_Trace, "WARNING: File is not readable: " & Name);
            Append
              (Result.Diagnostics,
               No_Source_Location_Range,
               "Cannot read " & To_Text (Name));
            Rotate_TDH;
            return;
         end;
      end if;

      --  Initialize the parser, which fetches the source buffer and extract
      --  all tokens.

      Init_Parser
        (Input, Context.With_Trivia, Unit, Unit_TDH, Unit.Context.Parser);

      --  If we could run the lexer, run the parser and get the root node

      if Unit_TDH.Source_Buffer /= null then
         Result.Ast_Mem_Pool := Create;
         Unit.Context.Parser.Mem_Pool := Result.Ast_Mem_Pool;
         Result.Ast_Root := Bare_Gpr_Node
           (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      end if;

      --  Forward token data and diagnostics to the returned unit

      Rotate_TDH;
      Result.Diagnostics.Append_Vector (Unit.Context.Parser.Diagnostics);
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  Remove the `symbol -> AST node` associations for Unit's nodes in
      --  foreign lexical environments.
      Remove_Exiled_Entries (Unit);

      --  Remove the named envs that Unit created
      declare
         Named_Envs_Needing_Update : NED_Maps.Map;
      begin
         Remove_Named_Envs (Unit, Named_Envs_Needing_Update);
         Update_Named_Envs (Unit.Context, Named_Envs_Needing_Update);
      end;

      --  At this point, envs and nodes that don't belong to this unit no
      --  longer reference this unit's envs and nodes. It is thus now safe to
      --  deallocate this unit's obsolete data.

      --  Replace Unit's diagnostics by Reparsed's
      Unit.Diagnostics := Reparsed.Diagnostics;
      Reparsed.Diagnostics.Clear;

      --  As (re-)loading a unit can change how any AST node property in the
      --  whole analysis context behaves, we have to invalidate caches. This
      --  is likely overkill, but kill all caches here as it's easy to do.
      --
      --  As an optimization, invalidate referenced envs cache only if this is
      --  not the first time we parse Unit.
      Invalidate_Caches
        (Unit.Context, Invalidate_Envs => Unit.Ast_Root /= null);

      --  Likewise for token data
      Free (Unit.TDH);
      Move (Unit.TDH, Reparsed.TDH);

      --  Reparsing will invalidate all lexical environments related to this
      --  unit, so destroy all related rebindings as well. This browses AST
      --  nodes, so we have to do this before destroying the old AST nodes
      --  pool.
      Destroy_Rebindings (Unit.Rebindings'Access);

      --  Destroy the old AST node and replace it by the new one
      if Unit.Ast_Root /= null then
         Destroy (Unit.Ast_Root);
      end if;
      Unit.Ast_Root := Reparsed.Ast_Root;

      --  Likewise for memory pools
      Free (Unit.Ast_Mem_Pool);
      Unit.Ast_Mem_Pool := Reparsed.Ast_Mem_Pool;
      Reparsed.Ast_Mem_Pool := No_Pool;

      --  Increment unit version number to invalidate caches and stale node
      --  reference. Also propagate it to the TDH.
      Unit.Unit_Version := Unit.Unit_Version + 1;
      Unit.TDH.Version := Unit.Unit_Version;

      --  Compute the PLE_Roots_Starting_Token table

      Unit.PLE_Roots_Starting_Token.Clear;

      --  Update all the lexical envs entries affected by the reparse

      declare
         Unit_Name     : constant String := +Unit.Filename.Base_Name;
         Context       : constant Internal_Context := Unit.Context;
         Foreign_Nodes : Bare_Gpr_Node_Vectors.Vector :=
           Bare_Gpr_Node_Vectors.Empty_Vector;

         Saved_In_Populate_Lexical_Env : constant Boolean :=
           Context.In_Populate_Lexical_Env;
         Saved_Env_Populated_Roots     : constant Boolean_Vectors.Vector :=
           Unit.Env_Populated_Roots;
      begin
         Context.In_Populate_Lexical_Env := True;
         if Main_Trace.Active then
            Main_Trace.Trace
              ("Updating lexical envs for " & Unit_Name & " after reparse");
            Main_Trace.Increase_Indent;
         end if;

         --  Collect all nodes that are foreign in this Unit's lexical envs.
         --  Exclude them from the corresponding lists of exiled entries.
         Extract_Foreign_Nodes (Unit, Foreign_Nodes);

         --  Temporarily reset Env_Populated_Roots so that Populate_Lexical_Env
         --  accepts to do its work on reparsed trees.

         Unit.Env_Populated_Roots := Boolean_Vectors.Empty_Vector;

         --  Now that Unit has been reparsed, we can destroy all its
         --  destroyables, which refer to the old tree (i.e. dangling
         --  pointers).
         Destroy_Unit_Destroyables (Unit);

         for FN of Foreign_Nodes loop
            declare
               Node_Image : constant String := Image (Short_Text_Image (FN));
               Unit_Name  : constant String := +FN.Unit.Filename.Base_Name;
            begin
               GNATCOLL.Traces.Trace
                 (Main_Trace, "Rerooting: " & Node_Image
                              & " (from " & Unit_Name & ")");
            end;
            Reroot_Foreign_Node (FN);
         end loop;
         Foreign_Nodes.Destroy;

         --  Re-populate all PLE roots that were requested so far for this
         --  unit. In the case where the unit has no PLE root, run PLE on the
         --  whole unit iff it was requested on at least one PLE root.

         declare
            function At_Least_One_Root_Populated return Boolean
            is (for some B of Saved_Env_Populated_Roots => B);
         begin
               if At_Least_One_Root_Populated then
                  Populate_Lexical_Env (Unit);
               end if;
         end;

         --  Restore the unit's original Env_Populated_Roots flags

         Unit.Env_Populated_Roots.Destroy;
         Unit.Env_Populated_Roots := Saved_Env_Populated_Roots;

         Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
         if Main_Trace.Is_Active then
            Main_Trace.Decrease_Indent;
         end if;
      end;
   end Update_After_Reparse;

   -------------------------------
   -- Destroy_Unit_Destroyables --
   -------------------------------

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit) is
   begin
      for D of Unit.Destroyables loop
         D.Destroy (D.Object);
      end loop;
      Destroyable_Vectors.Clear (Unit.Destroyables);
   end Destroy_Unit_Destroyables;

   ---------------------------
   -- Remove_Exiled_Entries --
   ---------------------------

   procedure Remove_Exiled_Entries (Unit : Internal_Unit) is
   begin
      for EE of Unit.Exiled_Entries loop
         AST_Envs.Remove (EE.Env, EE.Key, EE.Node);

         --  Also strip foreign nodes information from "outer" units so that it
         --  does not contain stale information (i.e. dangling pointers to
         --  nodes that belong to the units in the queue).
         if EE.Env.Owner /= No_Generic_Unit then
            declare
               Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector renames
                  Convert_Unit (EE.Env.Owner).Foreign_Nodes;
               Current       : Positive := Foreign_Nodes.First_Index;
            begin
               while Current <= Foreign_Nodes.Last_Index loop
                  if Foreign_Nodes.Get (Current).Node = EE.Node then
                     Foreign_Nodes.Pop (Current);
                  else
                     Current := Current + 1;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Unit.Exiled_Entries.Clear;
   end Remove_Exiled_Entries;

   -----------------------
   -- Remove_Named_Envs --
   -----------------------

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map) is
   begin
      --  Remove nodes in this unit from the Named_Env_Descriptor.Foreign_Nodes
      --  components in which they are registered and from the foreign
      --  environments themselves.
      for EE of Unit.Exiled_Entries_In_NED loop
         Remove (EE.Named_Env.Foreign_Nodes, EE.Key, EE.Node);
         Remove (EE.Named_Env.Env_With_Precedence, EE.Key, EE.Node);
      end loop;
      Unit.Exiled_Entries_In_NED.Clear;

      --  Remove nodes in this unit from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components in which they
      --  are registered.
      for Cur in Unit.Nodes_With_Foreign_Env.Iterate loop
         declare
            use Node_To_Named_Env_Maps;
            Node : constant Bare_Gpr_Node := Key (Cur);
            NE   : constant Named_Env_Descriptor_Access := Element (Cur);
         begin
            NE.Nodes_With_Foreign_Env.Delete (Node);
         end;
      end loop;
      Unit.Nodes_With_Foreign_Env.Clear;

      --  Remove ends in this unit from the Named_Env_Descriptor.Foreign_Envs
      --  components in which they are registered.
      for EE of Unit.Exiled_Envs loop
         EE.Named_Env.Foreign_Envs.Delete (Env_Node (EE.Env));
      end loop;
      Unit.Exiled_Envs.Clear;

      --  Remove named envs that this unit created
      for NE of Unit.Named_Envs loop
         declare
            NED_Access : constant Named_Env_Descriptor_Access :=
               Unit.Context.Named_Envs.Element (NE.Name);
            NED        : Named_Env_Descriptor renames NED_Access.all;
         begin
            NED.Envs.Delete (Env_Node (NE.Env));

            --  If this named environment had precedence, we must schedule an
            --  update for this name environment entry.
            if NE.Env = NED.Env_With_Precedence then
               Named_Envs_Needing_Update.Include (NE.Name, NED_Access);
               NED.Env_With_Precedence := Empty_Env;
            end if;
         end;
      end loop;
      Unit.Named_Envs.Clear;
   end Remove_Named_Envs;

   ---------------------------
   -- Extract_Foreign_Nodes --
   ---------------------------

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out Bare_Gpr_Node_Vectors.Vector) is
   begin
      --  Go through all foreign nodes registered in Unit's lexical
      --  environments.
      for FN of Unit.Foreign_Nodes loop
         --  Collect them
         Foreign_Nodes.Append (FN.Node);

         --  For each foreign node, remove the corresponding exiled entry in
         --  that foreign unit (each foreign node in unit A has a corresponding
         --  exiled entry in unit B).
         declare
            Exiled_Entries : Exiled_Entry_Vectors.Vector renames
               FN.Unit.Exiled_Entries;
            Current        : Positive := Exiled_Entries.First_Index;
         begin
            while Current <= Exiled_Entries.Last_Index loop
               if Exiled_Entries.Get (Current).Node = FN.Node then
                  Exiled_Entries.Pop (Current);
               else
                  Current := Current + 1;
               end if;
            end loop;
         end;
      end loop;
      Unit.Foreign_Nodes.Clear;
   end Extract_Foreign_Nodes;

   --------------------------
   -- Reroot_Foreign_Nodes --
   --------------------------

   procedure Reroot_Foreign_Node (Node : Bare_Gpr_Node) is
      Unit : constant Internal_Unit := Node.Unit;
   begin
      --  First, filter the exiled entries in foreign units so that they don't
      --  contain references to this unit's lexical environments.  We need to
      --  do that before running the partial Populate_Lexical_Env pass so that
      --  we don't remove exiled entries that this pass will produce.
      declare
         Exiled_Entries : Exiled_Entry_Vectors.Vector renames
            Unit.Exiled_Entries;
         Current        : Positive := Exiled_Entries.First_Index;
      begin
         while Current <= Exiled_Entries.Last_Index loop
            if Exiled_Entries.Get (Current).Node = Node then
               Exiled_Entries.Pop (Current);
            else
               Current := Current + 1;
            end if;
         end loop;
      end;

      --  Re-do a partial Populate_Lexical_Env pass for each foreign node that
      --  this unit contains so that they are relocated in our new lexical
      --  environments.
      declare
         Unit_State : aliased PLE_Unit_State :=
           (Named_Envs_Needing_Update => <>);
         State      : PLE_Node_State :=
           (Unit_State  => Unit_State'Unchecked_Access,
            Current_Env => Node.Self_Env,
            Current_NED => null);
      begin
         Pre_Env_Actions (Node, State, Add_To_Env_Only => True);
         Post_Env_Actions (Node, State);
      end;
   end Reroot_Foreign_Node;

   ----------
   -- Text --
   ----------

   function Text (Node : Bare_Gpr_Node) return String_Type is
   begin
      return Create_String (Text (Node));
   end Text;

   ------------------------
   -- Destroy_Rebindings --
   ------------------------

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector)
   is
      procedure Recurse (R : in out Env_Rebindings);
      --  Destroy R's children and then destroy R. It is up to the caller to
      --  remove R from its parent's Children vector.

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector);
      --  Remove R from Rebindings

      -------------
      -- Recurse --
      -------------

      procedure Recurse (R : in out Env_Rebindings) is
      begin
         for C of R.Children loop
            declare
               C_Var : Env_Rebindings := C;
            begin
               Recurse (C_Var);
            end;
         end loop;
         R.Children.Destroy;

         Unregister (R, Convert_Unit (R.Old_Env.Owner).Rebindings);
         Unregister (R, Convert_Unit (R.New_Env.Owner).Rebindings);

         Release_Rebinding (R);
      end Recurse;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector) is
      begin
         for I in 1 .. Rebindings.Length loop
            if Rebindings.Get (I) = R then
               Rebindings.Pop (I);
               return;
            end if;
         end loop;

         --  We are always supposed to find R in Rebindings, so this should be
         --  unreachable.
         raise Program_Error;
      end Unregister;

   begin
      while Rebindings.Length > 0 loop
         declare
            R : Env_Rebindings := Rebindings.Get (1);
         begin
            --  Here, we basically undo what has been done in AST_Envs.Append

            --  If this rebinding has no parent, then during its creation we
            --  registered it in its Old_Env. Otherwise, it is registered
            --  in its Parent's Children list.
            if R.Parent = null then
               Unwrap (R.Old_Env).Rebindings_Pool.Delete (R.New_Env);
            else
               Unregister (R, R.Parent.Children);
            end if;

            --  In all cases it's registered in Old_Env's and New_Env's units
            Recurse (R);
         end;
      end loop;
   end Destroy_Rebindings;

   --------------------------
   -- Get_Rewriting_Handle --
   --------------------------

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer is
   begin
      return Context.Rewriting_Handle;
   end Get_Rewriting_Handle;

   --------------------------
   -- Set_Rewriting_Handle --
   --------------------------

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer) is
   begin
      Context.Rewriting_Handle := Handle;
   end Set_Rewriting_Handle;

   -----------------------
   -- Create_Safety_Net --
   -----------------------

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net
   is
   begin
      return (Context         => Context,
              Context_Serial  => Context.Serial_Number,
              Context_Version => Context.Cache_Version);
   end Create_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Iterator_Safety_Net) is
   begin
      if Self.Context = null then
         return;
      end if;

      --  Check that the context is still the same (not released nor reused)
      if Self.Context.Serial_Number /= Self.Context_Serial
         or else Self.Context.Cache_Version /= Self.Context_Version
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ----------------------
   -- String_To_Symbol --
   ----------------------

   function String_To_Symbol
     (Self    : Bare_Gpr_Node;
      Context : Internal_Context;
      S       : String_Type) return Symbol_Type is
   begin
      return (if S.Length > 0
              then Lookup_Symbol (Context, S.Content)
              else null);
   exception
      when Exc : Invalid_Symbol_Error =>
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            Ada.Exceptions.Exception_Message (Exc));
   end String_To_Symbol;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : String_Type) is
   begin
      if Self.Ref_Count >= 0 then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out String_Type) is
   begin
      if Self = null or else Self.Ref_Count < 0 then
         return;
      end if;

      if Self.Ref_Count = 1 then
         Free (Self);
      else
         Self.Ref_Count := Self.Ref_Count - 1;
         Self := null;
      end if;
   end Dec_Ref;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Text_Type) return String_Type is
   begin
      return Result : constant String_Type := new String_Record'
        (Length    => Content'Length,
         Ref_Count => 1,
         Content   => Content);
   end Create_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Unbounded_Text_Type) return String_Type is
      S : Big_Wide_Wide_String_Access;
      L : Natural;
   begin
      Get_Wide_Wide_String (Content, S, L);
      return Create_String (S.all (1 .. L));
   end Create_String;

   -------------------
   -- Concat_String --
   -------------------

   function Concat_String (Left, Right : String_Type) return String_Type is
   begin
      return Result : constant String_Type :=
        new String_Record (Length => Left.Length + Right.Length)
      do
         Result.Ref_Count := 1;
         Result.Content (1 .. Left.Length) := Left.Content;
         Result.Content (Left.Length + 1 .. Result.Length) := Right.Content;
      end return;
   end Concat_String;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : String_Type) return Boolean is
   begin
      return Left.Content = Right.Content;
   end Equivalent;

begin
   No_Big_Integer.Value.Set (0);
end Gpr_Parser.Implementation;
