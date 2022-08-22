--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

package body GPR2.View_Ids.DAGs is

   use type Ada.Containers.Count_Type;

   package Node_Vectors is new Ada.Containers.Vectors (Positive, Node_Id);
   subtype Node_Vector is Node_Vectors.Vector;

   package Node_Int_Maps is new Ada.Containers.Ordered_Maps (Node_Id, Natural);
   subtype Node_Int_Map is Node_Int_Maps.Map;

   function Allocate_Node
     (Self : in out DAG; Vertex : View_Id) return Node_Id;
   --  Return the Node_Id associated with a vertex name.

   function Min (List : Node_Sets.Set; Map : Node_Int_Map) return Node_Id;
   --  return the key that corresponds to the minimum value in the map

   function Shortest_Path
     (Self           : DAG;
      Source, Target : Node_Id) return View_Ids.Vector.Object;

   procedure Update (Self  : in out Node_Node_Set_Maps.Map;
                     Key   : Node_Id;
                     Value : Node_Sets.Set);
   --  If Self contains Key then map Key to Value, otherwise map Key to the
   --  union of Value and previous mapping of Key.

   procedure Update
     (Self  : in out Node_Node_Set_Maps.Map;
      Key   : Node_Id;
      Value : Node_Id);
   --  Idem as previous function for the case only one Value should be
   --  inserted.

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex
     (Self         : in out DAG;
      Vertex       : View_Id;
      Predecessors : View_Ids.Set.Object := View_Ids.Set.Empty_Set) is
   begin
      if Self.Contains (Vertex) then
         raise DAG_Error with
           "insert error: vertex " & String (Image (Vertex)) &
           " already exist";
      end if;

      Self.Update_Vertex
        (Vertex       => Vertex,
         Predecessors => Predecessors);
   end Add_Vertex;

   --------------------
   --  Allocate_Node --
   --------------------

   function Allocate_Node
     (Self : in out DAG; Vertex : View_Id) return Node_Id
   is
      use Name_Node_Maps;
      Node        : Node_Id;
      Name_Cursor : Cursor;
   begin
      --  Check if there is already a node called Vertex
      Name_Cursor := Self.Vertex_Names.Find (Vertex);

      if Name_Cursor = No_Element then
         --  If there is no node called Vertex allocate a node id and
         --  associate it with Vertex

         Node := Self.Next_Free_Node;
         pragma Assert (Self.Next_Free_Node < Node_Id'Last);
         Self.Next_Free_Node := Self.Next_Free_Node + 1;
         Self.Vertex_Names.Include (Vertex, Node);
         Self.Vertex_Ids.Include (Node, Vertex);
         Self.Predecessors.Include (Node, Node_Sets.Empty_Set);
         Self.Successors.Include (Node, Node_Sets.Empty_Set);
         Self.Cache_Valid := False;

      else
         --  A node id is already associated with Vertex

         Node := Element (Name_Cursor);
      end if;

      return Node;
   end Allocate_Node;

   ------------
   -- As_Dot --
   ------------

   function As_Dot (Self : DAG) return Unbounded_String
   is
      use Node_Node_Set_Maps;
      Result : Unbounded_String;

   begin
      Append (Result, "digraph G {" & ASCII.LF);
      Append (Result, "rankdir=""LR"";" & ASCII.LF);

      for Pred_Cursor in Self.Predecessors.Iterate loop
         Append (Result, """");
         Append
           (Result,
            String (Image (Self.Vertex_Ids.Element (Key (Pred_Cursor)))));
         Append (Result, """");
         Append (Result, ASCII.LF);

         if Element (Pred_Cursor).Length > 0 then
            for Pred of Element (Pred_Cursor) loop
               Append (Result, """");
               Append
                 (Result,
                  String
                    (Image (Self.Vertex_Ids.Element (Key (Pred_Cursor)))));

               Append (Result, """");
               Append (Result, " -> ");

               Append (Result, """");
               Append (Result,
                       String (Image (Self.Vertex_Ids.Element (Pred))));
               Append (Result, """" & ASCII.LF);
            end loop;
         end if;
      end loop;

      Append (Result, "}");

      return Result;
   end As_Dot;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out DAG)  is
   begin
      Self.Next_Free_Node := 1;
      Self.Predecessors.Clear;
      Self.Successors.Clear;
      Self.Vertex_Names.Clear;
      Self.Vertex_Ids.Clear;
      Self.Sort_Cache.Clear;
      Self.Cache_Valid := True;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (Self : DAG; Vertex : View_Id) return Boolean is
   begin
      return Self.Vertex_Names.Contains (Vertex);
   end Contains;

   ---------
   -- Min --
   ---------

   function Min (List : Node_Sets.Set; Map : Node_Int_Map) return Node_Id
   is
      Result : Node_Id;
      Value  : Integer := Integer'Last;
   begin
      for Key of List loop
         if Value > Map.Element (Key) then
            Value := Map.Element (Key);
            Result := Key;
         end if;
      end loop;

      return Result;
   end Min;

   ---------------------
   -- Shortest_Circle --
   ---------------------

   function Shortest_Circle (Self : DAG) return GPR2.View_Ids.Vector.Object
   is
      Result : GPR2.View_Ids.Vector.Object;
   begin
      if not Self.Has_Cycle then
         return GPR2.View_Ids.Vector.Empty_Vector;
      end if;

      for Id of Self.Vertex_Names loop
         Result := Shortest_Path (Self, Id, Id);
         if not Result.Is_Empty then
            return Result;
         end if;
      end loop;

      raise DAG_Error with "Has_Circularity set but no circularity found";
   end Shortest_Circle;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Self           : DAG;
      Source, Target : View_Id) return View_Ids.Vector.Object
   is
   begin
      return Shortest_Path (Self,
                            Self.Vertex_Names.Element (Source),
                            Self.Vertex_Names.Element (Target));
   end Shortest_Path;

   function Shortest_Path
     (Self           : DAG;
      Source, Target : Node_Id) return View_Ids.Vector.Object
   is
      Infinite    : constant Natural := Natural (Self.Vertex_Ids.Length) + 1;
      --  Maximum distance between two vertices is the number of nodes in the
      --  DAG - 1, unless Source and Target are equal in which case the
      --  maximum possible distance is the number of nodes. So infinity is
      --  Length (Nodes) + 1.

      Dist        : Node_Int_Map;
      --  This map will keep track of minimal distance between vertices and the
      --  source.

      Prev        : Node_Node_Maps.Map;
      --  This keeps track of the minimum distance

      Non_Visited : Node_Sets.Set;
      --  Non visited nodes

      T_Id        : Node_Id renames Target;
      S_Id        : Node_Id := Source;

      U, V        : Node_Id;
      Alt         : Natural;
      Result      : View_Ids.Vector.Object;

   begin
      --  We use the Dikjstra algorithm to compute the shortest path.
      --  Note that this is a slight variation so that the algorithm
      --  can be used to compute shortest cycle on a given node.

      --  Initialize Dist:

      for Id of Self.Vertex_Names loop
         if Id = T_Id then
            --  Only known distance at startup
            Dist.Insert (Id, 0);
         else
            Dist.Insert (Id, Infinite);
         end if;
      end loop;

      --  Initialize Prev:

      for Id of Self.Vertex_Names loop
         Prev.Insert (Id, Undefined);
      end loop;

      for Id of Self.Vertex_Names loop
         Non_Visited.Insert (Id);
      end loop;

      if S_Id = T_Id then
         --  If Source is equal to target, default dikjstra algorithm does
         --  not work. Add a fake node and use it as target. When iterating
         --  on predecessors, replace all occurences of sources to that node.
         --  If we find a path between that node and the source, it means we
         --  have our shortest cycle.
         Dist.Insert (Undefined, Infinite);
         Prev.Insert (Undefined, Undefined);
         Non_Visited.Insert (Undefined);
         S_Id := Undefined;
      end if;

      while not Non_Visited.Is_Empty loop
         U := Min (Non_Visited, Dist);
         Non_Visited.Delete (U);

         if U = S_Id then
            --  We found the shortest path
            exit;

         elsif U /= Undefined then
            for U_Pred of Self.Predecessors.Element (U) loop
               if S_Id = Undefined and then U_Pred = T_Id then
                  --  Handle cycle detection case

                  V := Undefined;
               else
                  V := U_Pred;
               end if;

               Alt := Dist.Element (U) + 1;

               if Alt < Dist.Element (V) then
                  Dist.Replace (V, Alt);
                  Prev.Replace (V, U);
               end if;
            end loop;
         end if;
      end loop;

      if Dist.Element (S_Id) = Infinite then
         --  No path between source and target

         return View_Ids.Vector.Empty_Vector;
      end if;

      U := S_Id;
      Result.Append (Self.Vertex_Ids (Source));
      while Prev.Element (U) /= Undefined loop
         U := Prev.Element (U);
         Result.Append (Self.Vertex_Ids.Element (U));
      end loop;

      return Result;
   end Shortest_Path;

   ----------------------
   -- Topological_Sort --
   ----------------------

   function Topological_Sort (Self : DAG) return View_Ids.Vector.Object
   is
   begin
      return Self.Sort_Cache;
   end Topological_Sort;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self        : in out DAG;
      Circularity :    out Boolean)
   is
      Non_Visited       : Node_Int_Map;
      Result_Nodes      : Node_Vector;
      use Node_Node_Set_Maps;

   begin
      if Self.Cache_Valid then
         Circularity := Self.Has_Cycle;

         return;
      end if;

      Self.Sort_Cache.Clear;
      Self.Has_Cycle := False;

      --  First compute set of non visited nodes and the number of
      --  predecessors that should be visited first.

      for Cursor in Self.Predecessors.Iterate loop
         Non_Visited.Include (Key (Cursor),
                              Natural (Element (Cursor).Length));
      end loop;

      while not Non_Visited.Is_Empty loop
         declare
            Visited_Nodes : Node_Sets.Set;
            Excluded      : Boolean := False;

         begin
            --  Add nodes that have no more predecessors to visit

            for Cursor in Non_Visited.Iterate loop
               if Node_Int_Maps.Element (Cursor) = 0 then
                  Visited_Nodes.Include (Node_Int_Maps.Key (Cursor));
                  Result_Nodes.Append (Node_Int_Maps.Key (Cursor));
               end if;
            end loop;

            --  Remove visited from non_visited

            for Visited_Node of Visited_Nodes loop
               Non_Visited.Exclude (Visited_Node);
               Excluded := True;
            end loop;

            --  Update number of predecessors that should be visited
            --  for remaining nodes

            for Visited_Node of Visited_Nodes loop
               for Successor of Self.Successors (Visited_Node) loop
                  Non_Visited.Include
                    (Successor, Non_Visited.Element (Successor) - 1);
               end loop;
            end loop;

            if not Excluded then
               --  Non_Visited is not empty and no more nodes to remove:
               --  no leaf node, so there's some circular dependency.

               Self.Has_Cycle := True;

               exit;
            end if;
         end;
      end loop;

      if not Self.Has_Cycle then
         for Node of Result_Nodes loop
            Self.Sort_Cache.Append (Self.Vertex_Ids.Element (Node));
         end loop;
      end if;

      --  Do not re-compute unless we add/modify vertices
      Self.Cache_Valid := True;
      Circularity      := Self.Has_Cycle;
   end Update;

   procedure Update
     (Self  : in out Node_Node_Set_Maps.Map;
      Key   : Node_Id;
      Value : Node_Sets.Set)
   is
      use Node_Node_Set_Maps;
      Pred_Cursor : Cursor;
      Final_Value : Node_Sets.Set;
   begin
      Final_Value.Union (Value);

      Pred_Cursor := Self.Find (Key);

      if Pred_Cursor /= No_Element then
         Final_Value.Union (Element (Pred_Cursor));
      end if;

      Self.Include (Key, Final_Value);
   end Update;

   procedure Update
     (Self  : in out Node_Node_Set_Maps.Map;
      Key   : Node_Id;
      Value : Node_Id)
   is
      use Node_Node_Set_Maps;
      Pred_Cursor : Cursor;
      Final_Value : Node_Sets.Set;
   begin
      Final_Value.Include (Value);

      Pred_Cursor := Self.Find (Key);

      if Pred_Cursor /= No_Element then
         Final_Value.Union (Element (Pred_Cursor));
      end if;

      Self.Include (Key, Final_Value);
   end Update;

   -------------------
   -- Update_Vertex --
   -------------------

   procedure Update_Vertex
     (Self         : in out DAG;
      Vertex       : View_Id;
      Predecessors : View_Ids.Set.Object := View_Ids.Set.Empty_Set)
   is
      Node : Node_Id;
   begin
      Node := Self.Allocate_Node (Vertex);

      declare
         Pred_Nodes : Node_Sets.Set;
      begin

         --  Compute the corresponding list of nodes

         for Vertex_Name of Predecessors loop
            Pred_Nodes.Include (Self.Allocate_Node (Vertex_Name));
         end loop;

         --  Update predecessors

         Update (Self.Predecessors, Node, Pred_Nodes);

         --  Update successors

         for Pred_Node of Pred_Nodes loop
            Update (Self.Successors, Pred_Node, Node);
         end loop;

         Self.Cache_Valid := False;
      end;
   end Update_Vertex;

   procedure Update_Vertex
     (Self        : in out DAG;
      Vertex      : View_Id;
      Predecessor : View_Id)
   is
      Predecessors : View_Ids.Set.Object;
   begin
      Predecessors.Include (Predecessor);
      Self.Update_Vertex (Vertex, Predecessors);
   end Update_Vertex;

end GPR2.View_Ids.DAGs;
