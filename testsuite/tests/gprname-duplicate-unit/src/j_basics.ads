------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ B A S I C S                             --
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

--  This package contains baisc utilities used by the JVM back-end

with J_Types; use J_Types;

package J_Basics is

   Debug_ON : constant Boolean := True;

   procedure Debug_Msg (S : String);
   --  Outputs S to standard out when Debug_ON is True

   procedure Set_Message_Prefix (S : String);
   --  Sets the prefix string that is displayed by routines Fatal_Error and
   --  Print_Msg before displaying any error message. Before calling this
   --  routine the prefix is the empty string.

   procedure Fatal_Error (S : String);
   --  Prints S prefixed with the string which was passed to the last call of
   --  Set_Message_Prefix and raises Fatal_Exception.

   Fatal_Exception : exception;

   procedure Print_Msg (S : String);
   --  Same as above, except that no exception is raised

   function Strip (S : String) return String;
   --  Strips all leading and trailing spaces from S and returns the stripped
   --  string.

   function Right_Justify (S : String; Pos : Positive := 5) return String;
   --  Strips all leading and trailing blanks from S and right justifies it in
   --  a string of Pos blanks.

   function Left_Justify (S : String; Pos : Positive := 30) return String;
   --  Strips all leading and trailing blanks from S and left justifies it in
   --  a string of Pos blanks.

   function To_String (Stream : Stream_Of_U1) return String;
   --  Converts Stream into a stream, by converting each byte into the
   --  corresponding character.

   function Get_Stream_Of_U1 (File_Name : String) return Stream_Of_U1_Ptr;
   --  Give the name of a file, this routine reads the file and returns the
   --  stream of bytes contained in the file. If the file cannot be opened an
   --  error message is emitted using Fatal_Error above and Fatal_Exception is
   --  raised.

   procedure Put_Stream_Of_U1 (Stream : Stream_Of_U1; File_Name : String);
   --  Give the name of a file, this routine writes the stream to the file
   --  If the file cannot be opened or written, an error message is emitted
   --  using Fatal_Error above and Fatal_Exception is raised.

   type Line_Position is record
      First : Nat_32;
      Last  : Nat_32;
   end record;
   type Line_Table is array (Pos_32 range <>) of Line_Position;
   function Get_Line_Table (Source_File : Stream_Of_U1) return Line_Table;
   --  Given a source file as a stream of U1, this routine returns a table L,
   --  starting at index 1 and such that L (K) gives the position where the
   --  K-th line of Source_File begins and ends. Specifically, L (K).First
   --  gives the index in Source_File of the first character of the K-th line,
   --  whereas L (K).Last gives the index of the last printable character in
   --  the line. Using a First and Last makes things more portable because the
   --  end of a line is marked by a LF (line feed) on UNIX machines, whereas it
   --  is marked by a CR (carriage return) followed by a LF on Windows
   --  platforms.  If Source_File is empty, return an empty Line_Table.

private
   pragma Inline (Debug_Msg);

end J_Basics;
