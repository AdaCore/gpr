------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ T Y P E S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

--  This package contains the low-level types and services needed by the JVM
--  back-end.

package J_Types is

   --  8, 16 or 32 bit unsigned integers

   type U1 is mod 2 ** 8;
   for  U1'Size use 8;

   type U2 is mod 2 ** 16;
   for  U2'Size use 16;

   type U4 is mod 2 ** 32;
   for  U4'Size use 32;

   type U8 is mod 2 ** 64;
   for  U8'Size use 64;

   --  8, 16 or 32 bit signed integers and their natural and positive subtypes

   type Int_8  is range -2 **  7 .. 2 **  7 - 1;
   for  Int_8'Size use 8;

   type Int_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for  Int_16'Size use 16;

   type Int_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for  Int_32'Size use 32;

   type Int_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for  Int_64'Size use 64;

   subtype Nat_8  is Int_8  range 0 .. Int_8 'Last;
   subtype Nat_16 is Int_16 range 0 .. Int_16'Last;
   subtype Nat_32 is Int_32 range 0 .. Int_32'Last;
   subtype Nat_64 is Int_64 range 0 .. Int_64'Last;

   subtype Pos_8  is Nat_8  range 1 .. Nat_8 'Last;
   subtype Pos_16 is Nat_16 range 1 .. Nat_16'Last;
   subtype Pos_32 is Nat_32 range 1 .. Nat_32'Last;
   subtype Pos_64 is Nat_64 range 1 .. Nat_64'Last;

   --  32 or 64 bit IEEE floats

   type IEEE_Float_32 is new Short_Float;
   type IEEE_Float_64 is new Long_Float;

   --  Stream of bytes that is read from or written to a ".class" JVM file

   type Stream_Of_U1     is array (Nat_32 range <>) of U1;
   type Stream_Of_U1_Ptr is access Stream_Of_U1;

   Empty_Stream : constant Stream_Of_U1 := (1 .. 0 => 0);

   --  Word and half word types in the Java Virtual Machine

   type Half_Word is record
      B0 : U1;
      B1 : U1;
   end record;

   type Word is record
      B0 : U1;
      B1 : U1;
      B2 : U1;
      B3 : U1;
   end record;

   --  Routines to return the 'Image of the corresponding type

   function Image (V : U1) return String;
   function Image (V : U2) return String;
   function Image (V : U4) return String;
   function Image (V : U8) return String;

   function Image (V : Int_8)  return String;
   function Image (V : Int_16) return String;
   function Image (V : Int_32) return String;
   function Image (V : Int_64) return String;

   function Image (V : IEEE_Float_32) return String;
   function Image (V : IEEE_Float_64) return String;

   --  Shift routines for the U1/U2/U4 unsigned integers

   function Shift_Left  (Value : U1; Amount : Natural) return U1;
   function Shift_Right (Value : U1; Amount : Natural) return U1;

   function Shift_Left  (Value : U2; Amount : Natural) return U2;
   function Shift_Right (Value : U2; Amount : Natural) return U2;

   function Shift_Left  (Value : U4; Amount : Natural) return U4;
   function Shift_Right (Value : U4; Amount : Natural) return U4;

   function Shift_Left  (Value : U8; Amount : Natural) return U8;
   function Shift_Right (Value : U8; Amount : Natural) return U8;

   --  Routines to convert a stream of 2/4 bytes into a U2/U4

   function To_U2 (B0, B1 : U1)         return U2;
   function To_U2 (H : Half_Word)       return U2;

   function To_U4 (B0, B1, B2, B3 : U1) return U4;
   function To_U4 (W : Word)            return U4;

   --  Routines to convert a U2/U4 into a stream of 2/4 bytes

   function  To_Half_Word (U : U2) return Half_Word;
   procedure To_Half_Word (U : U2; B0, B1 : out U1);

   function  To_Word      (U : U4) return Word;
   procedure To_Word      (U : U4; B0, B1, B2, B3 : out U1);

   --  Routines to convert a stream of 2/4 bytes into a Int_16/Int_32

   function To_Int_16 (B0, B1 : U1)         return Int_16;
   function To_Int_16 (H : Half_Word)       return Int_16;

   function To_Int_32 (B0, B1, B2, B3 : U1) return Int_32;
   function To_Int_32 (W : Word)            return Int_32;

   --  Routines to convert a Int_16/Int_32 into a stream of 2/4 bytes

   function  To_Half_Word (I : Int_16) return Half_Word;
   procedure To_Half_Word (I : Int_16; B0, B1 : out U1);

   function  To_Word      (I : Int_32) return Word;
   procedure To_Word      (I : Int_32; B0, B1, B2, B3 : out U1);

   --  Routines to convert a U8 into two U4 and conversely

   procedure To_U4 (U : U8; Hi, Lo : out U4);
   function  To_U8 (Hi, Lo : U4) return U8;

   --  Routines to convert a U1/U2/U4/U8 into an Int_8/Int_16/Int_32/Int_64

   function To_Int_8  (V : U1) return Int_8;
   function To_Int_16 (V : U2) return Int_16;
   function To_Int_32 (V : U4) return Int_32;
   function To_Int_64 (V : U8) return Int_64;

   --  Routines to convert an Int_8/Int_16/Int_32/Int_64 into a U1/U2/U4/U8

   function To_U1 (V : Int_8)  return U1;
   function To_U2 (V : Int_16) return U2;
   function To_U4 (V : Int_32) return U4;
   function To_U8 (V : Int_64) return U8;

   --  Routines to convert a U4/U8 into an IEEE_32/IEEE_64

   function To_IEEE_Float_32 (V : U4) return IEEE_Float_32;
   function To_IEEE_Float_64 (V : U8) return IEEE_Float_64;

private
   pragma Inline (Shift_Left);
   pragma Inline (Shift_Right);
   pragma Inline (To_Half_Word);
   pragma Inline (To_Int_8);
   pragma Inline (To_Int_16);
   pragma Inline (To_Int_32);
   pragma Inline (To_Int_64);
   pragma Inline (To_IEEE_Float_32);
   pragma Inline (To_IEEE_Float_64);
   pragma Inline (To_U1);
   pragma Inline (To_U2);
   pragma Inline (To_U4);
   pragma Inline (To_U8);
   pragma Inline (To_Word);

end J_Types;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.4
--  date: Thu Apr 23 16:31:15 1998;  author: gasperon
--  type Stream_Of_U1 is moved here.
--  ----------------------------
--  revision 1.5
--  date: Tue Apr 28 19:46:06 1998;  author: briot
--  added Stream_Of_U1_Ptr
--  ----------------------------
--  revision 1.6
--  date: Mon May  4 11:11:31 1998;  author: gasperon
--  Minor format changes.
--  Added Empty_Stream constant.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
