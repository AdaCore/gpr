------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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

      else
         --  A node id is already associated with Vertex

         Node := Element (Name_Cursor);
      end if;

      return Node;
   end Allocate_Node;

   ------------
   -- As_Dot --
   ------------

   function As_Dot (Self : DAG) return Unbounded_String is
      Result : Unbounded_String;
      use Node_Node_Set_Maps;
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
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (Self : DAG; Vertex : View_Id) return Boolean is
   begin
      return Self.Vertex_Names.Contains (Vertex);
   end Contains;

   ----------
   -- Copy --
   ----------

   procedure Copy (Self : in out DAG; Source : DAG) is
   begin
      Self.Clear;
      Self.Next_Free_Node := Source.Next_Free_Node;
      Self.Successors := Source.Successors;
      Self.Predecessors := Source.Predecessors;
      Self.Vertex_Names := Source.Vertex_Names;
      Self.Vertex_Ids := Source.Vertex_Ids;
   end Copy;

   ----------------------
   -- Topological_Sort --
   ----------------------

   function Topological_Sort (Self : DAG) return View_Ids.Vector.Object
   is
      Result       : View_Ids.Vector.Object;
      Non_Visited  : Node_Int_Map;
      Result_Nodes : Node_Vector;
      use Node_Node_Set_Maps;

   begin
      --  First compute set of non visited nodes and the number of
      --  predecessors that should be visited first.

      for Cursor in Self.Predecessors.Iterate loop
         Non_Visited.Include (Key (Cursor),
                              Natural (Element (Cursor).Length));
      end loop;

      while Non_Visited.Length > 0 loop
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
               raise DAG_Error with "cycle detected";
            end if;
         end;
      end loop;

      for Node of Result_Nodes loop
         Result.Append (Self.Vertex_Ids.Element (Node));
      end loop;

      return Result;
   end Topological_Sort;

   ------------
   -- Update --
   ------------

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
