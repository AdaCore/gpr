------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     J_LIST--
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
--                                                                          --
--          Copyright (C) 1998 Free Software Foundation, Inc.               --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;

package body J_List is

   procedure Free is new Unchecked_Deallocation (Node,
                                                 Node_Access);

   -------------------------------------------------
   --  Add a new Element to the head of the list  --
   -------------------------------------------------

   procedure Add (Item    : in Element;
                  To_List : in out List)
   is
   begin
      To_List.Head := new Node'(Item => Item,
                                Next => To_List.Head);
   end Add;

   -------------------------------------
   --  Delete every Item in the list  --
   -------------------------------------

   procedure Clean (Alist : in out List)
   is
      Current  : Node_Access := Alist.Head;
      Next_One : Node_Access;
   begin
      while Current /= null loop
         Next_One := Current.Next;
         Free (Current);
         Current := Next_One;
      end loop;
      Alist.Head := null;
   end Clean;

   -----------------------------------------------------
   --  return True if the Item was found in the list  --
   -----------------------------------------------------

   function Exists (Item    : in Element;
                    In_List : in List)
                    return Boolean
   is
      Current  : Node_Access := In_List.Head;
   begin
      while Current /= null loop
         if Current.Item = Item then
            return True;
         end if;
         Current := Current.Next;
      end loop;

      return False;
   end Exists;

   ----------------------------------------------------
   --  Delete the last element inserted in the list  --
   ----------------------------------------------------

   procedure Pop (In_List : in out List) is
      Current : Node_Access := In_List.Head;
   begin
      if In_List.Head = null then
         raise List_Empty;
      end if;

      In_List.Head := In_List.Head.Next;
      Free (Current);
   end Pop;


end J_List;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.1
--  date: Fri May  1 11:36:15 1998;  author: briot
--  Initial revision
--  ----------------------------
--  revision 1.2
--  date: Mon May 18 18:28:55 1998;  author: briot
--  (Pop): new routine
--  (Clean): reset List.Head to null
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
