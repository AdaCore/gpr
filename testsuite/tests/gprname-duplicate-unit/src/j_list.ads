------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     J_LIST--
--                                                                          --
--                                 S p e c                                  --
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

generic
   type Element is private;

package J_List is

   type List is limited private;
   procedure Add (Item    : in Element;
                  To_List : in out List);
   --  add a new element to the list

   procedure Clean (Alist : in out List);
   --  delete all elements of the list

   function Exists (Item    : in Element;
                    In_List : in List)
                    return Boolean;
   --  test if the item already exists in the list

   procedure Pop (In_List : in out List);
   --  delete the last element inserted in the list
   --  raise List_Empty if the list does not contain at least one item

   List_Empty : exception;

private

   type Node;
   type Node_Access is access Node;
   type Node is
      record
         Item  : Element;
         Next  : Node_Access;
      end record;

   type List is
      record
         Head  : Node_Access := null;
      end record;


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
--  date: Mon May 18 18:28:57 1998;  author: briot
--  (Pop): new routine
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
