
--
--  Copyright (C) 2019-2023, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--


with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with Gpr_Parser_Support.Slocs; use Gpr_Parser_Support.Slocs;
with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;

with Gpr_Parser.Common;         use Gpr_Parser.Common;
with Gpr_Parser.Implementation;
with Gpr_Parser.Lexer_Implementation;
use Gpr_Parser.Lexer_Implementation;

with Gpr_Parser.Unparsing_Implementation;
use Gpr_Parser.Unparsing_Implementation;

package body Gpr_Parser.Rewriting_Implementation is

   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Rewriting_Handle_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Rewriting_Handle_Pointer, Rewriting_Handle);

   procedure Pre_Check (Value : Boolean; Msg : String);
   --  Raise a Precondition_Failure exception with the given message
   --  if the Value is False.

   ---------------
   -- Pre_Check --
   ---------------

   procedure Pre_Check (Value : Boolean; Msg : String) is
   begin
      if not Value then
         raise Precondition_Failure with Msg;
      end if;
   end Pre_Check;

   

   

   

   

   

   

   

   

   

   

   

   

   

   function Handle (Context : Internal_Context) return Rewriting_Handle is
     (Convert (Get_Rewriting_Handle (Context)));

   function Context (Handle : Rewriting_Handle) return Internal_Context is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Context;
   end Context;

   function Allocate
     (Kind          : Gpr_Node_Kind_Type;
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
      with Pre =>
         Context /= No_Rewriting_Handle
         and then (Unit_Handle = No_Unit_Rewriting_Handle
                   or else Unit_Handle.Context_Handle = Context)
         and then (Parent_Handle = No_Node_Rewriting_Handle
                   or else Parent_Handle.Context_Handle = Context);

   function Allocate
     (Node          : Bare_Gpr_Node;
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
      with Pre =>
         Context /= No_Rewriting_Handle
         and then (Unit_Handle = No_Unit_Rewriting_Handle
                   or else Unit_Handle.Context_Handle = Context)
         and then (Parent_Handle = No_Node_Rewriting_Handle
                   or else Parent_Handle.Context_Handle = Context);
   --  Allocate a handle for Node and register it in Unit_Handle's map

   procedure Expand_Children (Node : Node_Rewriting_Handle)
      with Pre => Node /= No_Node_Rewriting_Handle;
   --  If Node.Children.Kind is Unexpanded, populate Node's list of Children to
   --  mimic the related bare AST node. Otherwise, do nothing.

   procedure Free_Handles (Handle : in out Rewriting_Handle);
   --  Free all resources tied to Handle. This also releases the rewriting
   --  handle singleton in Handle's Context.

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle;
      Unit           : Unit_Rewriting_Handle);
   --  Tie the node represented by handle so that either:
   --
   --    * it is the root of Unit (Parent is null);
   --    * it is a child of Parent (Unit is null).
   --
   --  Do nothing if Handle is null.

   procedure Untie (Handle : Node_Rewriting_Handle);
   --  Untie the node represented by Handle. Do nothing if Handle is null.

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Internal_Context) return Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context) = No_Rewriting_Handle,
         "Handle (Context) must be null");
   

      if Context.File_Reader /= null then
         raise Precondition_Failure with
            "tree rewriting forbidden with a file reader";
      end if;

      declare
         Result : constant Rewriting_Handle := new Rewriting_Handle_Type'
           (Context   => Context,
            Units     => <>,
            Pool      => Create,
            New_Nodes => <>);
      begin
         Result.New_Nodes := Nodes_Pools.Create (Result.Pool);
         Set_Rewriting_Handle (Context, Convert (Result));
         return Result;
      end;
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      Free_Handles (Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is

      type Processed_Unit_Record is record
         Unit     : Internal_Unit;
         New_Data : Reparsed_Unit;
      end record;
      type Processed_Unit is access Processed_Unit_Record;
      procedure Free is new Ada.Unchecked_Deallocation
        (Processed_Unit_Record, Processed_Unit);

      package Processed_Unit_Vectors is new Ada.Containers.Vectors
        (Positive, Processed_Unit);

      Units  : Processed_Unit_Vectors.Vector;
      Result : Apply_Result := (Success => True);

   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

      --  Try to reparse all units that were potentially modified
      for Unit_Handle of Handle.Units loop
         declare
            PU    : constant Processed_Unit := new Processed_Unit_Record'
              (Unit     => Unit_Handle.Unit,
               New_Data => <>);
            Input : Internal_Lexer_Input :=
              (Kind        => Bytes_Buffer,
               Charset     => <>,
               Read_BOM    => False,
               Bytes       => System.Null_Address,
               Bytes_Count => 0);
            Bytes : String_Access;

            function Error_Result return Apply_Result
            is ((Success => False, Unit => PU.Unit, Diagnostics => <>));
         begin
            Units.Append (PU);

            --  Reparse (i.e. unparse and then parse) this rewritten unit
            begin
               Bytes := Unparse
                 (Create_Abstract_Node (Unit_Handle.Root),
                  PU.Unit,
                  Preserve_Formatting => True,
                  As_Unit             => True);
            exception
               when Exc : Malformed_Tree_Error =>
                  Result := Error_Result;
                  Append
                    (Result.Diagnostics,
                     No_Source_Location_Range,
                     To_Text (Exception_Message (Exc)));
                  exit;
            end;
            Input.Charset := Unit_Handle.Unit.Charset;
            Input.Bytes := Bytes.all'Address;
            Input.Bytes_Count := Bytes.all'Length;
            Do_Parsing (PU.Unit, Input, PU.New_Data);
            Free (Bytes);

            --  If there is a parsing error, abort the rewriting process
            if not PU.New_Data.Diagnostics.Is_Empty then
               Result := Error_Result;
               Result.Diagnostics.Move (PU.New_Data.Diagnostics);
               Destroy (PU.New_Data);
               exit;
            end if;
         end;
      end loop;

      --  If all reparsing went fine, actually replace the AST nodes all over
      --  the context and free all resources associated to Handle.
      if Result.Success then
         for PU of Units loop
            Update_After_Reparse (PU.Unit, PU.New_Data);
         end loop;
         Free_Handles (Handle);
      end if;

      --  Clean-up our local resources and return
      for PU of Units loop
         Free (PU);
      end loop;
      return Result;
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

      declare
         Count  : constant Natural := Natural (Handle.Units.Length);
         Result : Unit_Rewriting_Handle_Array (1 .. Count);
         I      : Positive := 1;
      begin
         for Unit of Handle.Units loop
            Result (I) := Unit;
            I := I + 1;
         end loop;
         return Result;
      end;
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Internal_Unit) return Unit_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context (Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Unit)) should not be null");
   
      
      Pre_Check
        (not Has_Diagnostics (Unit),
         "Unit must not have diagnostics");
   

      declare
         use Unit_Maps;

         Context        : constant Internal_Context := Unit.Context;
         Context_Handle : constant Rewriting_Handle := Handle (Context);
         Filename       : constant Unbounded_String :=
            To_Unbounded_String (Get_Filename (Unit));

         Cur : constant Cursor := Context_Handle.Units.Find (Filename);
      begin
         if Cur /= No_Element then
            return Element (Cur);
         end if;

         declare
            Result : constant Unit_Rewriting_Handle :=
               new Unit_Rewriting_Handle_Type'(Context_Handle => Context_Handle,
                                               Unit           => Unit,
                                               Root           => <>,
                                               Nodes          => <>);
         begin
            Context_Handle.Units.Insert (Filename, Result);
            Result.Root := Handle (Root (Unit));
            return Result;
         end;
      end;
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Internal_Unit
   is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Unit;
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Root;
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Root = No_Node_Rewriting_Handle or else not Tied (Root),
         "Root must not be tied to another rewriting context.");
   

      Untie (Handle.Root);
      Handle.Root := Root;
      Tie (Root, No_Node_Rewriting_Handle, Handle);
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Unit_Rewriting_Handle,
         "Handle should not be null");
   
      return Unparsing_Implementation.Unparse
        (Node                => Create_Abstract_Node (Handle.Root),
         Unit                => Handle.Unit,
         Preserve_Formatting => True,
         As_Unit             => True);
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle (Node : Bare_Gpr_Node) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle (Context (Node.Unit)) /= No_Rewriting_Handle,
         "Handle (Context (Node.Unit)) should not be null");
   
      
      Pre_Check
        (not Has_Diagnostics (Node.Unit),
         "Node.Unit must not have diagnostics");
   

      if Node = null then
         return No_Node_Rewriting_Handle;
      end if;

      declare
         use Node_Maps;

         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (Node.Unit);
         Cur         : constant Cursor := Unit_Handle.Nodes.Find (Node);
      begin
         --  If we have already built a handle for this node, just return it
         if Cur /= No_Element then
            return Element (Cur);

         --  Otherwise, if this node has a parent, make sure this parent has
         --  its own handle, then expand its children. This last must create
         --  the handle we are supposed to return.
         elsif Node.Parent /= null then
            Expand_Children (Handle (Node.Parent));
            return Element (Unit_Handle.Nodes.Find (Node));
         end if;

         --  Otherwise, we are dealing with the root node: just create its
         --  rewriting handle.
         return Allocate (Node, Unit_Handle.Context_Handle, Unit_Handle,
                          No_Node_Rewriting_Handle);
      end;
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Bare_Gpr_Node is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Node;
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Context_Handle;
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return To_Wide_Wide_String
        (Unparsing_Implementation.Unparse
           (Create_Abstract_Node (Handle),
            Unit                => null,
            Preserve_Formatting => True,
            As_Unit             => False));
   end Unparse;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Kind          : Gpr_Node_Kind_Type;
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Tied : constant Boolean := Unit_Handle /= No_Unit_Rewriting_Handle;
   begin
      return new Node_Rewriting_Handle_Type'
        (Context_Handle => Context,
         Node           => null,
         Parent         => Parent_Handle,
         Kind           => Kind,
         Tied           => Tied,
         Root_Of        =>
           (if Tied and then Parent_Handle = No_Node_Rewriting_Handle
            then Unit_Handle
            else No_Unit_Rewriting_Handle),
         Children       => Unexpanded_Children);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Node          : Bare_Gpr_Node;
      Context       : Rewriting_Handle;
      Unit_Handle   : Unit_Rewriting_Handle;
      Parent_Handle : Node_Rewriting_Handle)
      return Node_Rewriting_Handle
   is
      Result : constant Node_Rewriting_Handle := Allocate
        (Node.Kind, Context, Unit_Handle, Parent_Handle);
   begin
      Result.Node := Node;
      if Result.Tied then
         Unit_Handle.Nodes.Insert (Node, Result);
      end if;
      return Result;
   end Allocate;

   ---------------------
   -- Expand_Children --
   ---------------------

   procedure Expand_Children (Node : Node_Rewriting_Handle) is
      Children : Node_Children renames Node.Children;
   begin
      --  If this handle has already be expanded, there is nothing to do
      if Children.Kind /= Unexpanded then
         return;
      end if;

      --  Otherwise, expand to the appropriate children form: token node or
      --  regular one.
      declare
         N           : constant Bare_Gpr_Node := Node.Node;
         Unit_Handle : constant Unit_Rewriting_Handle :=
            Handle (N.Unit);
      begin
         if Is_Token_Node (N) then
            Children := (Kind => Expanded_Token_Node,
                         Text => To_Unbounded_Wide_Wide_String (Text (N)));

         else
            Children := (Kind => Expanded_Regular, Vector => <>);
            declare
               Count : constant Natural := Children_Count (N);
            begin
               Children.Vector.Reserve_Capacity
                 (Ada.Containers.Count_Type (Count));
               for I in 1 .. Count loop
                  declare
                     Child : constant Bare_Gpr_Node :=
                        Implementation.Child (N, I);
                  begin
                     Children.Vector.Append
                       ((if Child = null
                         then null
                         else Allocate (Child, Unit_Handle.Context_Handle,
                                        Unit_Handle, Node)));
                  end;
               end loop;
            end;
         end if;
      end;
   end Expand_Children;

   ------------------
   -- Free_Handles --
   ------------------

   procedure Free_Handles (Handle : in out Rewriting_Handle) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Rewriting_Handle_Type, Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Unit_Rewriting_Handle_Type, Unit_Rewriting_Handle);
      procedure Free is new Ada.Unchecked_Deallocation
        (Node_Rewriting_Handle_Type, Node_Rewriting_Handle);

      Ctx : constant Internal_Context := Context (Handle);
   begin
      --  Free all resources tied to Handle
      for Unit of Handle.Units loop
         for Node of Unit.Nodes loop
            Free (Node);
         end loop;
         Free (Unit);
      end loop;
      for Node of Handle.New_Nodes loop
         declare
            N : Node_Rewriting_Handle := Node;
         begin
            Free (N);
         end;
      end loop;
      Free (Handle.Pool);
      Free (Handle);

      --  Release the rewriting handle singleton for its context
      Set_Rewriting_Handle (Ctx, Convert (Handle));
   end Free_Handles;

   ---------
   -- Tie --
   ---------

   procedure Tie
     (Handle, Parent : Node_Rewriting_Handle;
      Unit           : Unit_Rewriting_Handle) is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent := Parent;
         Handle.Tied := True;
         if Parent = No_Node_Rewriting_Handle then
            Handle.Root_Of := Unit;
         end if;
      end if;
   end Tie;

   -----------
   -- Untie --
   -----------

   procedure Untie (Handle : Node_Rewriting_Handle) is
   begin
      if Handle /= No_Node_Rewriting_Handle then
         Handle.Parent := No_Node_Rewriting_Handle;
         Handle.Tied := False;
         Handle.Root_Of := No_Unit_Rewriting_Handle;
      end if;
   end Untie;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Gpr_Node_Kind_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Kind;
   end Kind;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Tied;
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return Handle.Parent;
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      return
        (case Handle.Children.Kind is
         when Unexpanded          => Children_Count (Handle.Node),
         when Expanded_Regular    => Natural (Handle.Children.Vector.Length),
         when Expanded_Token_Node => 0);
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      
      if Index > Children_Count (Handle) then
         raise Precondition_Failure
            with "Invalid index " & Index'Image & ": Handle has " &
                  Children_Count (Handle)'Image & " children";
      end if;
   

      --  If this handle represents an already existing node, make sure it is
      --  expanded so we have a handle to return.
      Expand_Children (Handle);
      return Handle.Children.Vector.Element (Index);
   end Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      
      if Index > Children_Count (Handle) then
         raise Precondition_Failure
            with "Invalid index " & Index'Image & ": Handle has " &
                  Children_Count (Handle)'Image & " children";
      end if;
   
      
      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");
   

      --  If this handle represents an already existing node, make sure it is
      --  expanded so that its children vector can be modified.
      Expand_Children (Handle);

      declare
         Child_Slot : Node_Rewriting_Handle renames
            Handle.Children.Vector.Reference (Index);
      begin
         --  Untie the child to be replaced if it exists
         Untie (Child_Slot);

         --  Tie the new child if it exists
         Tie (Child, Handle, No_Unit_Rewriting_Handle);

         Child_Slot := Child;
      end;
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);
   

      case Handle.Children.Kind is
         when Unexpanded =>
            if Is_Token_Node (Handle.Kind) then
               return Text (Handle.Node);
            else
               raise Program_Error;
            end if;
         when Expanded_Regular =>
            return (raise Program_Error);
         when Expanded_Token_Node =>
            return To_Wide_Wide_String (Handle.Children.Text);
      end case;
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind (Handle)),
         "Expected a token node. Got " & Kind (Handle)'Image);
   

      --  Make sure Handle is expanded so we have a Text field to override
      Expand_Children (Handle);

      Handle.Children.Text := To_Unbounded_Wide_Wide_String (Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Tied (Handle),
         "Handle must be tied to an analysis unit.");
   
      
      Pre_Check
        (New_Node = No_Node_Rewriting_Handle or else not Tied (New_Node),
         "New_Node must not be tied to another rewriting context.");
   

      if Handle = New_Node then
         return;
      end if;

      if Handle.Root_Of = No_Unit_Rewriting_Handle then
         --  If Handle is not the root node of its owning unit, go replace it
         --  in its parent's children list.
         declare
            Parent : Node_Rewriting_Handle renames Handle.Parent;
            Index  : Natural := 0;
         begin
            for I in 1 .. Children_Count (Parent) loop
               if Child (Parent, I) = Handle then
                  Index := I;
                  exit;
               end if;
            end loop;
            pragma Assert (Index > 0);
            Set_Child (Parent, Index, New_Node);
         end;

      else
         --  Otherwise, replace it as a root node
         Set_Root (Handle.Root_Of, New_Node);
      end if;
   end Replace;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   
      
      
      if Index > Children_Count (Handle) + 1 then
         raise Precondition_Failure
            with "Invalid index " & Index'Image & ": Handle has " &
                  Children_Count (Handle)'Image & " children";
      end if;
   
      
      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");
   

      --  First, just create room for the new node and let Set_Child take care
      --  of tiding Child to Handle's tree.
      Expand_Children (Handle);
      Handle.Children.Vector.Insert (Index, No_Node_Rewriting_Handle);
      Set_Child (Handle, Index, Child);
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   
      
      Pre_Check
        (Child = No_Node_Rewriting_Handle or else not Tied (Child),
         "Child must not be tied to another rewriting context.");
   

      Insert_Child (Handle, Children_Count (Handle) + 1, Child);
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) is
   begin
      
      Pre_Check
        (Handle /= No_Node_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_List_Node (Kind (Handle)),
         "Expected a list node. Got " & Kind (Handle)'Image);
   
      
      
      if Index > Children_Count (Handle) + 1 then
         raise Precondition_Failure
            with "Invalid index " & Index'Image & ": Handle has " &
                  Children_Count (Handle)'Image & " children";
      end if;
   

      --  First, let Set_Child take care of untiding the child to remove, and
      --  then actually remove the corresponding children list slot.
      Set_Child (Handle, Index, No_Node_Rewriting_Handle);
      Handle.Children.Vector.Delete (Index);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle;
   begin
      if Handle = No_Node_Rewriting_Handle then
         return Handle;
      end if;

      --  Make sure the original handle is expanded so we can iterate on it
      Expand_Children (Handle);

      --  If the input handle is associated to a node, so should be the cloned
      --  handle, so that its formatting is copied as well.
      Result :=
        (if Handle.Node = null
         then Allocate (Handle.Kind, Handle.Context_Handle,
                        No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle)
         else Allocate (Handle.Node, Handle.Context_Handle,
                        No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle));
      Nodes_Pools.Append (Handle.Context_Handle.New_Nodes, Result);

      --  Recursively clone children
      case Handle.Children.Kind is
         when Unexpanded =>
            raise Program_Error;

         when Expanded_Token_Node =>
            Result.Children := (Kind => Expanded_Token_Node,
                                Text => Handle.Children.Text);

         when Expanded_Regular =>
            Result.Children := (Kind => Expanded_Regular, Vector => <>);
            Result.Children.Vector.Reserve_Capacity
              (Handle.Children.Vector.Length);
            for I in 1 .. Handle.Children.Vector.Last_Index loop
               Result.Children.Vector.Append
                 (Clone (Handle.Children.Vector.Element (I)));
            end loop;
      end case;

      return Result;
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (not Is_Error_Node (Kind),
         "Expected a non-error node. Got " & Kind'Image);
   

      if Is_Token_Node (Kind) then
         return Create_Token_Node (Handle, Kind, "");
      else
         declare
            Count    : constant Integer := Kind_To_Node_Children_Count (Kind);
            Children : constant Node_Rewriting_Handle_Array (1 ..  Count) :=
               (others => No_Node_Rewriting_Handle);
         begin
            return Create_Regular_Node (Handle, Kind, Children);
         end;
      end if;
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Gpr_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (Is_Token_Node (Kind),
         "Expected a token node. Got " & Kind'Image);
   

      declare
         Result : constant Node_Rewriting_Handle := Allocate
           (Kind, Handle, No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle);
      begin
         Result.Children := (Kind => Expanded_Token_Node,
                             Text => To_Unbounded_Wide_Wide_String (Text));
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Gpr_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle is
   begin
      
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   
      
      Pre_Check
        (not Is_Token_Node (Kind),
         "Expected a token node. Got " & Kind'Image);
   
      
      Pre_Check
        (not Is_Error_Node (Kind),
         "Expected a non-error node. Got " & Kind'Image);
   
      for One_Child of Children loop
         
      Pre_Check
        (One_Child = No_Node_Rewriting_Handle or else not Tied (One_Child),
         "One_Child must not be tied to another rewriting context.");
   
      end loop;

      declare
         Result : Node_Rewriting_Handle := Allocate
           (Kind, Handle, No_Unit_Rewriting_Handle, No_Node_Rewriting_Handle);
      begin
         Result.Children := (Kind   => Expanded_Regular,
                             Vector => <>);
         Result.Children.Vector.Reserve_Capacity (Children'Length);
         for C of Children loop
            Result.Children.Vector.Append (C);
            if C /= No_Node_Rewriting_Handle then
               Tie (C, Result, No_Unit_Rewriting_Handle);
            end if;
         end loop;
         Nodes_Pools.Append (Handle.New_Nodes, Result);
         return Result;
      end;
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle
   is
      type State_Type is (
         Default,
         --  Default state: no meta character being processed

         Open_Brace,
         --  The previous character is a open brace: the current one
         --  determines what it means.

         Close_Brace
         --  The previous character is a closing brace: the current one must be
         --  another closing brace.
      );

      Buffer   : Unbounded_Wide_Wide_String;
      State    : State_Type := Default;
      Next_Arg : Positive := Arguments'First;
   begin
      for One_Argument of Arguments loop
         
      Pre_Check
        (One_Argument = No_Node_Rewriting_Handle
            or else Context (One_Argument) = Handle,
         "One_Argument should be associated to rewriting context Handle.");
   
      end loop;

      --  Interpret the template looping over its characters with a state
      --  machine.
      for C of Template loop
         case State is
         when Default =>
            case C is
            when '{' =>
               State := Open_Brace;
            when '}' =>
               State := Close_Brace;
            when others =>
               Append (Buffer, C);
            end case;

         when Open_Brace =>
            case C is
            when '{' =>
               State := Default;
               Append (Buffer, C);
            when '}' =>
               State := Default;
               if Next_Arg in Arguments'Range then
                  declare
                     Unparsed_Arg : constant Wide_Wide_String :=
                        Unparse (Arguments (Next_Arg));
                  begin
                     Next_Arg := Next_Arg + 1;
                     Append (Buffer, Unparsed_Arg);
                  end;
               else
                  raise Template_Args_Error with
                     "not enough arguments provided";
               end if;
            when others =>
               raise Template_Format_Error with
                  "standalone ""{"" character";
            end case;

         when Close_Brace =>
            case C is
            when '}' =>
               State := Default;
               Append (Buffer, C);
            when others =>
               raise Template_Format_Error with
                  "standalone ""}"" character";
            end case;
         end case;
      end loop;

      --  Make sure that there is no standalone metacharacter at the end of the
      --  template.
      case State is
         when Default => null;
         when Open_Brace =>
            raise Template_Format_Error with "standalone ""{"" character";
         when Close_Brace =>
            raise Template_Format_Error with "standalone ""}"" character";
      end case;

      --  Make sure all given arguments were consumed
      if Next_Arg in Arguments'Range then
         raise Template_Args_Error with "too many arguments provided";
      end if;

      --  Now parse the resulting buffer and create the corresponding tree of
      --  nodes.
      declare
         Context  : constant Internal_Context :=
           Rewriting_Implementation.Context (Handle);
         Unit     : constant Internal_Unit := Templates_Unit (Context);
         Reparsed : Reparsed_Unit;
         Text     : constant Text_Type := To_Wide_Wide_String (Buffer);
         Input    : constant Internal_Lexer_Input :=
           (Kind       => Text_Buffer,
            Text       => Text'Address,
            Text_Count => Text'Length);

         function Transform
           (Node   : Bare_Gpr_Node;
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle;
         --  Turn a node from the Reparsed unit into a recursively expanded
         --  node rewriting handle.

         ---------------
         -- Transform --
         ---------------

         function Transform
           (Node   : Bare_Gpr_Node;
            Parent : Node_Rewriting_Handle) return Node_Rewriting_Handle
         is
            Result : Node_Rewriting_Handle;
         begin
            if Node = null then
               return No_Node_Rewriting_Handle;
            end if;

            --  Allocate the handle for Node, and don't forget to remove the
            --  backlink to Node itself as it exists only temporarily for
            --  template instantiation. Also, track the newly allocated node
            --  so that it is freed correctly upon destruction of the
            --  rewriting context.
            Result := Allocate (Node, Handle, No_Unit_Rewriting_Handle,
                                Parent);
            Result.Node := null;
            Nodes_Pools.Append (Handle.New_Nodes, Result);

            if Is_Token_Node (Node) then
               declare
                  Index : constant Natural := Natural (Node.Token_Start_Index);
                  Data  : constant Stored_Token_Data :=
                     Reparsed.TDH.Tokens.Get (Index);
                  Text  : constant Text_Type := Reparsed.TDH.Source_Buffer
                    (Data.Source_First .. Data.Source_Last);
               begin
                  Result.Children :=
                    (Kind => Expanded_Token_Node,
                     Text => To_Unbounded_Wide_Wide_String (Text));
               end;

            else
               declare
                  Count : constant Natural := Children_Count (Node);
               begin
                  Result.Children := (Kind => Expanded_Regular, Vector => <>);
                  Result.Children.Vector.Reserve_Capacity
                    (Ada.Containers.Count_Type (Count));
                  for I in 1 .. Count loop
                     Result.Children.Vector.Append
                       (Transform (Child (Node, I), Result));
                  end loop;
               end;
            end if;
            return Result;
         end Transform;

      begin
         Set_Rule (Unit, Rule);
         Do_Parsing (Unit, Input, Reparsed);
         if not Reparsed.Diagnostics.Is_Empty then
            Destroy (Reparsed);
            raise Template_Instantiation_Error;
         end if;

         declare
            Result : constant Node_Rewriting_Handle :=
               Transform (Reparsed.Ast_Root, No_Node_Rewriting_Handle);
         begin
            Destroy (Reparsed);
            return Result;
         end;
      end;
   end Create_From_Template;


         function Create_Attribute_Decl
           (Handle : Rewriting_Handle
               ; Attribute_Decl_F_Attr_Name : Node_Rewriting_Handle
               ; Attribute_Decl_F_Attr_Index : Node_Rewriting_Handle
               ; Attribute_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Attribute_Decl,
               (1 => Attribute_Decl_F_Attr_Name, 2 => Attribute_Decl_F_Attr_Index, 3 => Attribute_Decl_F_Expr));
         end;


         function Create_Attribute_Reference
           (Handle : Rewriting_Handle
               ; Attribute_Reference_F_Attribute_Name : Node_Rewriting_Handle
               ; Attribute_Reference_F_Attribute_Index : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Attribute_Reference,
               (1 => Attribute_Reference_F_Attribute_Name, 2 => Attribute_Reference_F_Attribute_Index));
         end;


         function Create_Builtin_Function_Call
           (Handle : Rewriting_Handle
               ; Builtin_Function_Call_F_Function_Name : Node_Rewriting_Handle
               ; Builtin_Function_Call_F_Parameters : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Builtin_Function_Call,
               (1 => Builtin_Function_Call_F_Function_Name, 2 => Builtin_Function_Call_F_Parameters));
         end;


         function Create_Case_Construction
           (Handle : Rewriting_Handle
               ; Case_Construction_F_Var_Ref : Node_Rewriting_Handle
               ; Case_Construction_F_Items : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Case_Construction,
               (1 => Case_Construction_F_Var_Ref, 2 => Case_Construction_F_Items));
         end;


         function Create_Case_Item
           (Handle : Rewriting_Handle
               ; Case_Item_F_Choice : Node_Rewriting_Handle
               ; Case_Item_F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Case_Item,
               (1 => Case_Item_F_Choice, 2 => Case_Item_F_Decls));
         end;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; Compilation_Unit_F_Project : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Compilation_Unit,
               (1 => Compilation_Unit_F_Project));
         end;


         function Create_Prefix
           (Handle : Rewriting_Handle
               ; Prefix_F_Prefix : Node_Rewriting_Handle
               ; Prefix_F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Prefix,
               (1 => Prefix_F_Prefix, 2 => Prefix_F_Suffix));
         end;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; Package_Decl_F_Pkg_Name : Node_Rewriting_Handle
               ; Package_Decl_F_Pkg_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Package_Decl,
               (1 => Package_Decl_F_Pkg_Name, 2 => Package_Decl_F_Pkg_Spec));
         end;


         function Create_Package_Extension
           (Handle : Rewriting_Handle
               ; Package_Extension_F_Extended_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Package_Extension,
               (1 => Package_Extension_F_Extended_Name));
         end;


         function Create_Package_Renaming
           (Handle : Rewriting_Handle
               ; Package_Renaming_F_Renamed_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Package_Renaming,
               (1 => Package_Renaming_F_Renamed_Name));
         end;


         function Create_Package_Spec
           (Handle : Rewriting_Handle
               ; Package_Spec_F_Extension : Node_Rewriting_Handle
               ; Package_Spec_F_Decls : Node_Rewriting_Handle
               ; Package_Spec_F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Package_Spec,
               (1 => Package_Spec_F_Extension, 2 => Package_Spec_F_Decls, 3 => Package_Spec_F_End_Name));
         end;


         function Create_Project
           (Handle : Rewriting_Handle
               ; Project_F_Context_Clauses : Node_Rewriting_Handle
               ; Project_F_Project_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Project,
               (1 => Project_F_Context_Clauses, 2 => Project_F_Project_Decl));
         end;


         function Create_Project_Declaration
           (Handle : Rewriting_Handle
               ; Project_Declaration_F_Qualifier : Node_Rewriting_Handle
               ; Project_Declaration_F_Project_Name : Node_Rewriting_Handle
               ; Project_Declaration_F_Extension : Node_Rewriting_Handle
               ; Project_Declaration_F_Decls : Node_Rewriting_Handle
               ; Project_Declaration_F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Project_Declaration,
               (1 => Project_Declaration_F_Qualifier, 2 => Project_Declaration_F_Project_Name, 3 => Project_Declaration_F_Extension, 4 => Project_Declaration_F_Decls, 5 => Project_Declaration_F_End_Name));
         end;


         function Create_Project_Extension
           (Handle : Rewriting_Handle
               ; Project_Extension_F_Is_All : Node_Rewriting_Handle
               ; Project_Extension_F_Path_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Project_Extension,
               (1 => Project_Extension_F_Is_All, 2 => Project_Extension_F_Path_Name));
         end;


         function Create_String_Literal_At
           (Handle : Rewriting_Handle
               ; String_Literal_At_F_Str_Lit : Node_Rewriting_Handle
               ; String_Literal_At_F_At_Lit : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_String_Literal_At,
               (1 => String_Literal_At_F_Str_Lit, 2 => String_Literal_At_F_At_Lit));
         end;


         function Create_Terms
           (Handle : Rewriting_Handle
               ; Terms_F_Terms : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Terms,
               (1 => Terms_F_Terms));
         end;


         function Create_Type_Reference
           (Handle : Rewriting_Handle
               ; Type_Reference_F_Var_Type_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Type_Reference,
               (1 => Type_Reference_F_Var_Type_Name));
         end;


         function Create_Typed_String_Decl
           (Handle : Rewriting_Handle
               ; Typed_String_Decl_F_Type_Id : Node_Rewriting_Handle
               ; Typed_String_Decl_F_String_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Typed_String_Decl,
               (1 => Typed_String_Decl_F_Type_Id, 2 => Typed_String_Decl_F_String_Literals));
         end;


         function Create_Variable_Decl
           (Handle : Rewriting_Handle
               ; Variable_Decl_F_Var_Name : Node_Rewriting_Handle
               ; Variable_Decl_F_Var_Type : Node_Rewriting_Handle
               ; Variable_Decl_F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Variable_Decl,
               (1 => Variable_Decl_F_Var_Name, 2 => Variable_Decl_F_Var_Type, 3 => Variable_Decl_F_Expr));
         end;


         function Create_Variable_Reference
           (Handle : Rewriting_Handle
               ; Variable_Reference_F_Variable_Name : Node_Rewriting_Handle
               ; Variable_Reference_F_Attribute_Ref : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_Variable_Reference,
               (1 => Variable_Reference_F_Variable_Name, 2 => Variable_Reference_F_Attribute_Ref));
         end;


         function Create_With_Decl
           (Handle : Rewriting_Handle
               ; With_Decl_F_Is_Limited : Node_Rewriting_Handle
               ; With_Decl_F_Path_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            
      Pre_Check
        (Handle /= No_Rewriting_Handle,
         "Handle should not be null");
   

            return Create_Regular_Node
              (Handle, Gpr_With_Decl,
               (1 => With_Decl_F_Is_Limited, 2 => With_Decl_F_Path_Names));
         end;

end Gpr_Parser.Rewriting_Implementation;
