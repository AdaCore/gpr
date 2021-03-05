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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Containers; use GPR2.Containers;
with GPR2.View_Ids.Vector;
with GPR2.View_Ids.Set;

package GPR2.View_Ids.DAGs is

   DAG_Error : exception;

   type DAG is tagged limited private;
   --  DAG instances

   procedure Add_Vertex
     (Self         : in out DAG;
      Vertex       : View_Id;
      Predecessors : View_Ids.Set.Object := View_Ids.Set.Empty_Set)
     with Pre  => not Self.Contains (Vertex),
          Post => Self.Contains (Vertex);
   --  Adds a node called Vertex to Self and set its predecessors to the
   --  elements of Predecessors set.

   procedure Update_Vertex
     (Self         : in out DAG;
      Vertex       : View_Id;
      Predecessors : View_Ids.Set.Object := View_Ids.Set.Empty_Set)
     with Post => Self.Contains (Vertex);
   --  Updates or creates a node called Vertex to Self and add to its
   --  predecessors the elements of Predecessors set.

   procedure Update_Vertex
     (Self        : in out DAG;
      Vertex      : View_Id;
      Predecessor : View_Id)
     with Post => Self.Contains (Vertex);
   --  Updates or creates a node called Vertex to Self and add a predecessor

   function Contains (Self : DAG; Vertex : View_Id) return Boolean;
   --  Returns True if Self contains a node called Vertex

   function Topological_Sort (Self : DAG) return View_Ids.Vector.Object;
   --  Returns the list of nodes in a topological order

   function As_Dot (Self : DAG) return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns the DAG representation in .dot format (graphviz)

   procedure Clear (Self : in out DAG);

   procedure Copy (Self : in out DAG; Source : DAG);

private

   type Node_Id is mod 2 ** 32;

   package Name_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (View_Id, Node_Id);

   package Node_Name_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Node_Id, View_Id);

   package Node_Sets is new Ada.Containers.Ordered_Sets (Node_Id);

   package Node_Node_Set_Maps is new Ada.Containers.Ordered_Maps
     (Node_Id, Node_Sets.Set, "=" => Node_Sets."=");

   type DAG is tagged limited record
      Next_Free_Node : Node_Id := 1;
      Predecessors   : Node_Node_Set_Maps.Map;
      Successors     : Node_Node_Set_Maps.Map;
      Vertex_Names   : Name_Node_Maps.Map;
      Vertex_Ids     : Node_Name_Maps.Map;
   end record;

end GPR2.View_Ids.DAGs;