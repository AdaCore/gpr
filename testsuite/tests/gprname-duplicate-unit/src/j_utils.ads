------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ U T I L S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

--  This package contains general utilities used by the JVM back-end

with J_Types;  use J_Types;
with JVM_File; use JVM_File;

package J_Utils is

   function Get_Utf8 (T : CP.Table; K : CP_Index) return Utf8.Table;
   --  Returns the Utf8 which is at the K-th entry of constant pool T. If there
   --  is no Utf8 at such entry an exception is raised.

   function Is_Java_Lang_Object (T : CP.Table; K : CP_Index) return Boolean;
   --  The K-the entry in table T must be a CONSTANT_Class entry, otherwise an
   --  exception is raised. The function returns True if this class is
   --  java.lang.Object, False otherwise.

   function Source_File_Name (CF : Class_File) return Utf8.Table;
   --  Returns the source file name, if any, from which the class file CF was
   --  generated. If no file name is found, returns an empty Utf8 table.

   function Get_Code_Attribute (M : Member_Info) return Member_Attribute_Info;
   --  Returns the code attribute of method M. If no such attribute exists
   --  raise an exception.

   type PC_Src_Map is array (Instruction_Index range <>) of U2;
   function Get_PC_To_Src_Map (C : Code_Attribute.Table) return PC_Src_Map;
   --  Given a method whose code attribute table is C, returns the map between
   --  each bytecode offset (also know as PC) of the method and the number of
   --  the corresponding source line.  Specifically let L be the PC_Src_Map
   --  returned, and let K be a bytecode offset in the current method, then
   --  L (K) is either zero (if no line in the original source maps onto that
   --  offset) or it contains the line number such that the code generated for
   --  it starts at offset K in the method's bytecode. Beware that the last
   --  bytecode offset of the table returned might not be the last bytecode
   --  offset of the method.

   function Get_Var_Info
     (C    : Code_Attribute.Table;
      Var  : Local_Variable_Index;
      PC   : Byte_Index)
      return Variable_Info;
   --  Given the code attribute table C of a method, a local variable slot
   --  number Var in the method as well as a bytecode offset PC, return the
   --  Variable_Info record for the variable in the original source code that
   --  corresponds to Var for that bytecode offset. If no Variable_Info record
   --  is found, return a Variable_Info record where the Name_Index field is
   --  set to CP_Empty.

   generic
      with procedure Process_Single_Class_File (Bytes : in Stream_Of_U1) is <>;
      with procedure Process_Directory (Name : in String) is <>;
   procedure Process_Command_Line (First_File_Pos : in Natural;
                                   Verbose        : in Boolean := False);
   --  For each file given on the command line (the first one beeing at
   --  position First_File_Pos), and for each .class file in a .zip file, call
   --  the procedure Process_Single_Class_File;
   --  For each directory, call Process_Directory

end J_Utils;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.3
--  date: Mon May  4 11:15:45 1998;  author: gasperon
--  (Source_File_Name) new routine.
--  (Get_Code_Attribute) ditto.
--  (Get_PC_To_Src_Map) ditto.
--  (Get_Var_Info) ditto.
--  ----------------------------
--  revision 1.4
--  date: Fri May 15 17:10:29 1998;  author: briot
--  (Process_Command_Line): new routine
--  ----------------------------
--  revision 1.5
--  date: Mon May 18 18:29:18 1998;  author: briot
--  (Process_Command_Line): added a routine for directories processing
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
