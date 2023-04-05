------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ S P E C                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

--  This file is part of the Ada-to-JVM compiler.

--  Set of routines to print the Ada specifications corresponding to
--  a JVM class file
--  These routines assume everything is consistent. If this
--  is not the case their behavior is undefined.

with JVM_File; use JVM_File;
with J_Types;  use J_Types;

package JVM_Ada is

   subtype Text_String is Wide_String;
   subtype Text_Char   is Wide_Character;
   --  types used to read from the .class file (identifiers are coded
   --  using Unicode format in a .class file)

   procedure Print_Header (Bytes          : in Stream_Of_U1;
                           Is_Inner_Class : in Boolean := False);
   --  print the Ada declaration corresponding to a java class file

   procedure Create_Directory_Class (Name : in Text_String);
   --  create a very simple .ads file corresponding to a directory in
   --  the Java class tree (like java/lang ...)
   --  the 'Name' parameter should have '/' separators between
   --  identifiers

end JVM_Ada;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.2
--  date: Fri May  1 16:03:05 1998;  author: briot
--  (Get_Type_Description): takes a Wide_String instead of Utf8.Table
--  ----------------------------
--  revision 1.3
--  date: Wed May  6 11:57:58 1998;  author: briot
--  using Text_String rather than Wide_String
--  (Print_Inner_Classes): new function
--  (Print_Type_Description): new function
--  (Get_Type_Description): gone
--  (Print_Fields): suppressed Package_Name parameter
--  ----------------------------
--  revision 1.4
--  date: Mon May 18 18:29:52 1998;  author: briot
--  Moved some routines to jvm_ada.adb
--  (Print_Header): takes a Stream_Of_U1 rather than a Class_File
--  (Create_Directory_Class): new routine
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
