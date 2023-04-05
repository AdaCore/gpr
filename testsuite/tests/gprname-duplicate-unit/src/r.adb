------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ S P E C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
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
with Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with JVM_File;
with J_Utils;
with J_List;
with J_Basics;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Fixed;

package body JVM_Ada is

   ----------------
   -- Local Data --
   ----------------

   --  Characters used in a class file to describe types
   B            : constant Text_Char := 'B';
   C            : constant Text_Char := 'C';
   D            : constant Text_Char := 'D';
   F            : constant Text_Char := 'F';
   I            : constant Text_Char := 'I';
   J            : constant Text_Char := 'J';
   S            : constant Text_Char := 'S';
   Z            : constant Text_Char := 'Z';
   V            : constant Text_Char := 'V';
   L            : constant Text_Char := 'L';
   Open_Bracket : constant Text_Char := '[';
   Open_Paren   : constant Text_Char := '(';
   Close_Paren  : constant Text_Char := ')';
   Semicolon    : constant Text_Char := ';';

   --  List of strings.  Used for printing unique declarations of
   --  types and 'with' statements.
   subtype String_Length is Natural range 0 .. 255;
   type Short_String (Length : String_Length := String_Length'Last) is
      record
         Str : Text_String (1 .. Length);
      end record;
   package String_List is new J_List (Short_String);

   Class_Definition_List   : String_List.List;
   --  List of the classes beeing defined (that is, the main class and it's
   --  inner classes. This list is required so that we do not print things
   --  like Current_Class.Ptr, which would lead to a
   --  'invalid prefix in selected component' error

   Current_Indent : Natural := 0;
   --  Current indentation for the output

   Current_File   : Ada.Wide_Text_IO.File_Type;
   --  Current file beeing written to

   --------------------
   -- Local Routines --
   --------------------

   procedure Close_File;
   --  closes the current output file

   function Convert (W : in Text_String) return Short_String;
   pragma Inline (Convert);
   --  converts a Wide_String to a bounded string we can use in a list

   procedure Indent (Step : in Integer);
   pragma Inline (Indent);
   --  modify the current indentation for the output file

   function Get_Ada_File_Name (Class_Name : in Text_String)
                               return String;
   --  return the name of the Ada file we should create for this package

   function Get_Class_Name (T : in Text_String) return Text_String;
   --  returns the class name part of T, ignoring the package name

   function Get_Simple_Description (Descr : in Text_Char) return Text_String;
   --  Returns the string corresponding to a basic Java type

   function Get_Super_Class_Name (CF : in Class_File) return Text_String;
   --  returns the name of the super Class

   function Next_Declaration_Pos (Descriptor  : in Text_String)
                                  return Text_String;
   --  returns the first declaration found in Descriptor
   --  returns an empty String if there is no such declaration

   procedure New_Line;
   --  print a new line and indent the cursor position

   procedure Open_File (Filename : in String);
   --  opens a new output file

   procedure Print (S : in Text_String);
   procedure Print (C : in Text_Char);
--   pragma Inline (Print);
   --  functions used for printing strings to the output file

   procedure Print_Array_Description (Descriptor  : in Text_String);
   --  print the Ada string corresponding to the Java descriptor, if Descriptor
   --  represents an array

   procedure Print_Class_Description (Descriptor     : in Text_String);
   --  Print a pointer to a class, in Ada syntax
   --  If the class is the one currently we are currently defining, then use
   --  a simplified syntax to event compilation errors
   --  ex/  if we are defining  package Test :
   --       A : java.lang.String.String      is valid
   --       B : Test.Test                    should be  B : Test
   --       if we are defining inner class Test.Test_Inner
   --       C : Test.Test_Inner.Test_Inner   should be  C : Test_Inner
   --       back in package Test:
   --       D : Test.Test_Inner.Test_Inner   =>    D : Test_Inner.Test_Inner

   procedure Print_Class_Field (CF    : in Class_File;
                                Field : in Member_Info);
   --  print a full equivalent of a static Java field

   procedure Print_Class_Method (CF           : in Class_File;
                                 Package_Name : in Text_String;
                                 Method       : in Member_Info;
                                 First_Arg    : in Text_String := "");
   --  print a full equivalent of a Java method

   procedure Print_Constant_Value (CF    : in Class_File;
                                   Field : in Member_Info;
                                   Index : in Int_32);
   --  returns the string corresponding to a constant value

   procedure Print_Fields (CF : in Class_File);
   --  process the static class fields of the Java class file

   procedure Print_Inner_Classes (CF : in Class_File);
   --  Print the inner classes declarations
   --  This is actually a recursive call to Print_Header above.

   procedure Print_Methods (CF           : in Class_File;
                            Package_Name : in Text_String);
   --  process the class methods of the Java class file

   procedure Print_Method_Description
     (Descriptor        : in Text_String;
      First_Arg         : in Text_String := "";
      Substitute_String : in Boolean := False);
   --  print the Ada description corresponding to the method found in
   --  Descriptor

   procedure Print_Obj_Declaration (CF           : in Class_File;
                                    Package_Name : in Text_String);
   --  Print the 'type Obj is ... ' declaration

   procedure Print_Qualified_Name (W : in Text_String);
   --  print W to the output file, replacing every '/' or '$' with '.'

   procedure Print_Types_Declaration (CF : Class_File);
   --  Create the Ada types corresponding to the Java arrays used in the
   --  class file.
   --  Arrays of basic types are mostly already declared in package java.ads,
   --  so nothing is done for them at this stage.

   procedure Print_Type_Description (Descriptor  : in Text_String);
   --  Return the Ada type description for Descriptor.
   --  Descriptor is a coded string describing a type, as in a .class file.

   procedure Print_With (CF           : in Class_File;
                         Package_Name : in Text_String);
   --  Print every required 'with' for this package.
   --  Every type descriptor is parsed, and, if it contains a class reference,
   --  a 'with' statement is printed.

   function To_Text_String (T : in Utf8.Table) return Text_String;
   function To_Text_String (N : in Natural)    return Text_String;
   function To_Text_String (N : in U4)         return Text_String;
   pragma Inline (To_Text_String);
   --  Converts the numeric argument to a String, omitting the leading space

   -----------------
   --  Close_File --
   -----------------

   procedure Close_File is
   begin
      Ada.Wide_Text_IO.Close (File => Current_File);
   end Close_File;

   --------------
   --  Convert --
   --------------

   function Convert (W : in Text_String) return Short_String is
   begin
      return Short_String'(Length  => W'Length,
                           Str     => W);
   end Convert;

   -----------------------------
   --  Create_Directory_Class --
   -----------------------------

   procedure Create_Directory_Class (Name : in Text_String) is
      procedure Generate_Java;
      --  Generates the java.ads file

      --------------------
      --  Generate_Java --
      --------------------

      procedure Generate_Java is
         type Basic_Types is (Char, Byte, Short, Int, Long, Double);
      begin
         Open_File (Filename => "java");
         Print ("package Java is");
         Indent (2);
         New_Line;
         Print ("subtype char    is Wide_Character;");
         New_Line;
         Print ("subtype byte    is Short_Short_Integer;");
         New_Line;
         Print ("subtype short   is Short_Integer;");
         New_Line;
         Print ("subtype int     is Integer;");
         New_Line;
         Print ("subtype long    is Long_Integer;");
         New_Line;
         Print ("subtype double  is Long_Float;");
         New_Line;

         for T in Basic_Types'Range loop
            for I in 1 .. 3 loop
               Print ("type "
                      & Basic_Types'Wide_Image (T)
                      & "_Array_"
                      & To_Text_String (I)
                      & " is array (natural range <>");
               for J in 2 .. I loop
                  Print (", natural range <>");
               end loop;
               Print (") of Char;");
               New_Line;
               Print ("type "
                      & Basic_Types'Wide_Image (T)
                      & "_Arr_"
                      & To_Text_String (I)
                      & " is access all "
                      & Basic_Types'Wide_Image (T)
                      & "_Array_"
                      & To_Text_String (I)
                      & ";");
               New_Line;
            end loop;
            New_Line;
         end loop;

         Indent (-2);
         New_Line;
         Print ("end Java;");
         Close_File;
      end Generate_Java;

   begin
      if Name = "java" then
         Generate_Java;
      else
         Open_File (Get_Ada_File_Name (Name));
         Print ("package ");
         Print_Qualified_Name (Name);
         Print (" is");
         New_Line;
         Print ("  pragma Convention (Java_Package, ");
         Print_Qualified_Name (Name);
         Print (");");
         New_Line;
         Print ("end ");
         Print_Qualified_Name (Name);
         Print (';');
         New_Line;
         Close_File;
      end if;
   end Create_Directory_Class;

   -------------
   --  Indent --
   -------------

   procedure Indent (Step : in Integer) is
   begin
      Current_Indent := Current_Indent + Step;
   end Indent;

   ------------------------
   --  Get_Ada_File_Name --
   ------------------------

   function Get_Ada_File_Name (Class_Name : in Text_String)
                               return String
   is
      use Ada.Characters.Handling;
      W : String (Class_Name'Range);
   begin
      for K in Class_Name'Range loop
         if Class_Name (K) = '/' then
            W (K) := '-';
         else
            W (K) := To_Lower (To_Character (Class_Name (K)));
         end if;
      end loop;
      return W;
   end Get_Ada_File_Name;

   ---------------------
   --  Get_Class_Name --
   ---------------------

   function Get_Class_Name (T : in Text_String) return Text_String is
   begin
      for J in reverse T'Range loop
         if T (J) = '/' or T (J) = '$' then
            return T (J + 1 .. T'Last);
         end if;
      end loop;
      return T;
   end Get_Class_Name;

   -----------------------------
   --  Get_Simple_Description --
   -----------------------------

   function Get_Simple_Description (Descr : in Text_Char) return Text_String is
   begin
      case Descr is
         when B => return "Byte";
         when C => return "Char";
         when D => return "Double";
         when F => return "Float";
         when I => return "Int";
         when J => return "Long";
         when S => return "Short";
         when Z => return "Boolean";
         when others => null;
      end case;
      return "";
   end Get_Simple_Description;

   ---------------------------
   --  Get_Super_Class_Name --
   ---------------------------

   function Get_Super_Class_Name (CF : in Class_File) return Text_String
   is
      Class_Info : constant CP_Info := JVM_File.CP.Get (CF.Constant_Pool,
                                                        CF.Super_Class);
      Super_Class_Name : constant Utf8.Table := J_Utils.Get_Utf8
        (CF.Constant_Pool, Class_Info.Class_Name_Index);
   begin
      return To_Text_String (Super_Class_Name);
   end Get_Super_Class_Name;

   ---------------------------
   --  Next_Declaration_Pos --
   ---------------------------

   function Next_Declaration_Pos (Descriptor  : in Text_String)
                                  return Text_String
   is
      Next_Arg : Natural := Descriptor'First;
   begin
      case Descriptor (Next_Arg) is
         when L =>
            while Descriptor (Next_Arg) /= Semicolon loop
               Next_Arg := Next_Arg + 1;
            end loop;

         when Open_Bracket =>
            while Descriptor (Next_Arg) = Open_Bracket loop
               Next_Arg := Next_Arg + 1;
            end loop;
            if Descriptor (Next_Arg) = L then
               while Descriptor (Next_Arg) /= Semicolon loop
                  Next_Arg := Next_Arg + 1;
               end loop;
            end if;

         when B | C | D | F | I | J | S | V | Z =>
            null;

         when others =>
            return Text_String'(1 .. 0 => ' ');
      end case;
      return Descriptor (Descriptor'First .. Next_Arg);
   end Next_Declaration_Pos;

   ---------------
   --  New_Line --
   ---------------

   procedure New_Line is
      use Ada.Strings.Wide_Fixed;
   begin
      Ada.Wide_Text_IO.New_Line (File => Current_File);
      Ada.Wide_Text_IO.Put (File => Current_File,
                            Item => Current_Indent * ' ');
   end New_Line;

   ----------------
   --  Open_File --
   ----------------

   procedure Open_File (Filename : in String) is
   begin
      Ada.Wide_Text_IO.Create (File => Current_File,
                               Name => Filename & ".ads");
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Could not create file " & Filename & ".ads");
         raise;
   end Open_File;

   ------------
   --  Print --
   ------------

   procedure Print (S : in Text_String) is
   begin
      Ada.Wide_Text_IO.Put (File => Current_File,
                            Item => S);
   end Print;

   procedure Print (C : in Text_Char) is
   begin
      Ada.Wide_Text_IO.Put (File => Current_File,
                            Item => C);
   end Print;

   ------------------------------
   --  Print_Array_Description --
   ------------------------------

   procedure Print_Array_Description (Descriptor  : in Text_String) is
      Bracket_Count : Natural := 0;
      Pos           : Natural := Descriptor'First;
   begin
      while Descriptor (Pos) = Open_Bracket loop
         Pos := Pos + 1;
         Bracket_Count := Bracket_Count + 1;
      end loop;

      if Descriptor (Pos) = L then
         Print (Get_Class_Name (Descriptor (Pos + 1 .. Descriptor'Last - 1)));
      else
         Print (Get_Simple_Description (Descriptor (Pos)));
      end if;
      Print ("_Arr_");
      Print (To_Text_String (Bracket_Count));
   end Print_Array_Description;

   ------------------------------
   --  Print_Class_Description --
   ------------------------------

   procedure Print_Class_Description (Descriptor     : in Text_String) is
      Tmp       : Text_String (Descriptor'Range);
      Tmp_First : Natural := Tmp'First;
      Tmp_Pos   : Natural := Tmp'First;

      Pos       : Natural := Descriptor'First + 1;
      First     : Natural := Pos;   --  short class name start (no package)
   begin

      loop
         case Descriptor (Pos) is
            when '/' | ';' | '$' =>
               Tmp (Tmp_Pos) := '.';

               if Descriptor (Pos) /= ';' then
                  First := Pos + 1;
               end if;

               if String_List.Exists
                 (Item => Convert (Descriptor
                                   (Descriptor'First + 1 .. Pos - 1)),
                  In_List => Class_Definition_List)
               then
                  Tmp_First := Tmp_Pos + 1;
               end if;

            when others =>
               Tmp (Tmp_Pos) := Descriptor (Pos);
         end case;
         exit when Descriptor (Pos) = ';';

         Tmp_Pos := Tmp_Pos + 1;
         Pos := Pos + 1;
      end loop;

      if Tmp_First <= Tmp_Pos - 1 then
         Print (Tmp (Tmp_First .. Tmp_Pos - 1));
         Print ('.');
      end if;
      Print (Descriptor (First .. Pos - 1));
   end Print_Class_Description;

   ------------------------
   --  Print_Class_Field --
   ------------------------

   procedure Print_Class_Field (CF    : in Class_File;
                                Field : in Member_Info) is
      Name  : constant Utf8.Table :=
        J_Utils.Get_Utf8 (CF.Constant_Pool, Field.Name_Index);
      --  Name of the field

      Descriptor : constant Utf8.Table :=
        J_Utils.Get_Utf8 (CF.Constant_Pool, Field.Descriptor_Index);
      --  coded string to describe the JVM type


      function Is_Constant_Field (Field  : in  Member_Info)
                                  return Int_32;
      --  returns the index in the Attributes array of the constant
      --  value. If there is none, returns -1

      ------------------------
      --  Is_Constant_Field --
      ------------------------

      function Is_Constant_Field (Field : in Member_Info) return Int_32 is
      begin
         for K in 0 .. Member_Attribute.Last (Field.Attributes) loop
            declare
               Attribute : constant Member_Attribute_Info :=
                 Member_Attribute.Get (Field.Attributes, K);
            begin
               if Attribute.Kind = Attr_Constant_Value then
                  return K;
               end if;
            end;
         end loop;
         return -1;
      end Is_Constant_Field;


      --  Start of Print_Class_Field

      Is_Constant : constant Int_32 :=  Is_Constant_Field (Field);
   begin
      Print_Qualified_Name (To_Text_String (Name));

      if Is_Constant /= -1 then
         Print (" : constant ");
      else
         Print (" : ");
      end if;

      Print_Type_Description (To_Text_String (Descriptor));

      if Is_Constant /= -1 then
         Print (" := ");
         Print_Constant_Value (CF, Field, Is_Constant);
      end if;

      Print (';');
   end Print_Class_Field;

   -------------------------
   --  Print_Class_Method --
   -------------------------

   procedure Print_Class_Method (CF           : in Class_File;
                                 Package_Name : in Text_String;
                                 Method       : in Member_Info;
                                 First_Arg    : in Text_String := "") is
      Name  : constant Utf8.Table :=
        J_Utils.Get_Utf8 (CF.Constant_Pool, Method.Name_Index);
      --  Name of the field

      Descriptor : constant Utf8.Table :=
        J_Utils.Get_Utf8 (CF.Constant_Pool, Method.Descriptor_Index);
      --  coded string to describe the JVM type

      W              : Text_String := To_Text_String (Name);
      Descr          : Text_String := To_Text_String (Descriptor);
      Is_Procedure   : constant Boolean
        := Descr (Descr'Last - 1 .. Descr'Last) = ")V";
      Substitute     : Boolean := False;


      function Has_String_Argument (Descriptor  : in Text_String)
                                    return Boolean;
      --  returns True if at least one argument of the function described by
      --  Descriptor is of type java.lang.String or char []

      --------------------------
      --  Has_String_Argument --
      --------------------------

      function Has_String_Argument (Descriptor : in Text_String) return Boolean
      is
         K : Natural := Descriptor'First + 1;
      begin
         loop
            declare
               Declaration : Text_String := Next_Declaration_Pos
                 (Descriptor (K .. Descriptor'Last));
            begin
               exit when Declaration'Length = 0;

               if Declaration = "[C" or Declaration = "Ljava/lang/String;" then
                  return True;
               end if;
               K := K + Declaration'Length;
            end;
         end loop;
         return False;
      end Has_String_Argument;

      --  Start of Print_Class_Method
   begin

      --  loop in case we have two print the same class with two
      --  differents sets of parameters
      loop
         --  special case for constructors
         if W = "<init>" then
            Print ("function New_" & Package_Name);
            Print_Method_Description (Descr,
                                      " This : Ptr := null",
                                      Substitute);
            Print (" return " & Package_Name);
         else
            if Is_Procedure then
               Print ("procedure ");
            else
               Print ("function ");
            end if;
            Print (W);
            Print_Method_Description (Descr, First_Arg, Substitute);
         end if;

         if   Is_Set (CF.Access_Flags, ACC_Interface)
           or Is_Set (Method.Access_Flags, ACC_Abstract) then
            Print (" is abstract");
         end if;

         if W = "<init>" then
            Print (';');
            New_Line;
            Print ("pragma Convention (Java_Constructor, New_" & Package_Name
                   & ");");
         else
            Print (';');
         end if;

         --  If we had at least one String parameter, write a new declaration
         --  for the method, otherwise end
         Substitute := not Substitute and then Has_String_Argument (Descr);
         exit when not Substitute;
         New_Line;
      end loop;

   end Print_Class_Method;

   ---------------------------
   --  Print_Constant_Value --
   ---------------------------

   procedure Print_Constant_Value (CF    : in Class_File;
                                   Field : in Member_Info;
                                   Index : in Int_32) is
      Attribute : constant Member_Attribute_Info :=
        Member_Attribute.Get (Field.Attributes, Index);
      R : constant CP_Info := CP.Get (CF.Constant_Pool,
                                      Attribute.Constant_Value_Index);
   begin
      case R.Tag is
         when CONSTANT_Integer =>
            --  depends on the real type of the variable

            declare
               Descriptor : constant Utf8.Table :=
                 J_Utils.Get_Utf8 (CF.Constant_Pool,
                                   Field.Descriptor_Index);
            begin
               case Utf8.Get (Descriptor, U2'(0)) is
                  when U1 (Text_Char'Pos (C)) =>
                     Print (Text_Char'Wide_Image
                            (Text_Char'Val (R.Bytes)));

                  when U1 (Text_Char'Pos (Z)) =>
                     if R.Bytes = 1 then
                        Print ("True");
                     else
                        Print ("False");
                     end if;

                  when others =>
                     Print (Int_32'Wide_Image (To_Int_32 (R.Bytes)));
               end case;
            end;

         when CONSTANT_Float   =>
            Print (IEEE_Float_32'Wide_Image
                   (J_Types.To_IEEE_Float_32 (R.Bytes)));

         when CONSTANT_Long =>
            Print (To_Text_String (R.High_Bytes)
                   & To_Text_String (R.Low_Bytes));
            null;

         when CONSTANT_Double  =>
            Print (IEEE_Float_64'Wide_Image
                   (J_Types.To_IEEE_Float_64
                    (J_Types.To_U8 (R.High_Bytes,
                                    R.Low_Bytes))));

         when CONSTANT_String =>
            Print ("java.lang.String.New_String (Standard."
                   & "Wide_String'(""");
            Print (To_Text_String
                   (J_Utils.Get_Utf8 (CF.Constant_Pool,
                                      R.String_Index)));
            Print ("""))");

         when others =>
            raise Constraint_Error;
      end case;
   end Print_Constant_Value;

   -------------------
   --  Print_Fields --
   -------------------

   procedure Print_Fields (CF : in Class_File) is
   begin
      --  First print all static fields
      for K in 0 .. JVM_File.Member.Last (CF.Fields) loop

         declare
            Field : constant Member_Info :=
              JVM_File.Member.Get (CF.Fields, K);
         begin
            if Is_Set (Field.Access_Flags, ACC_Public) then
               if Is_Set (Field.Access_Flags, ACC_Static) then
                  New_Line;
                  Print_Class_Field (CF    => CF,
                                     Field => Field);
               end if;
            end if;
         end;
      end loop;
   end Print_Fields;

   -------------------
   --  Print_Header --
   -------------------

   procedure Print_Header (Bytes          : in Stream_Of_U1;
                           Is_Inner_Class : in Boolean := False)
   is
      CF : constant Class_File := JVM_File.Read (Bytes, Check => False);
      Class_Info : constant CP_Info := JVM_File.CP.Get (CF.Constant_Pool,
                                                        CF.This_Class);
      Class_Name : constant Utf8.Table := J_Utils.Get_Utf8
        (CF.Constant_Pool,
         Class_Info.Class_Name_Index);
      Class_Name_String : constant Text_String
        := To_Text_String (Class_Name);

      Package_Name : Text_String := Get_Class_Name (Class_Name_String);
   begin

      if not Is_Inner_Class then

         String_List.Add (Item    => Convert (Class_Name_String),
                          To_List => Class_Definition_List);
         Open_File (Get_Ada_File_Name (Class_Name_String));
         Print_With (CF, Class_Name_String);
         Print ("package ");
         Print_Qualified_Name (Class_Name_String);
         Print (" is");
      else
         Print ("package " & Package_Name & " is");
      end if;

      Indent (2);
      New_Line;

      Print ("pragma Import (Java, "
             & Package_Name
             & ", """);
      Print_Qualified_Name (Class_Name_String);
      Print (""");");
      New_Line;
      New_Line;

      if Class_Name_String = "java/lang/Object" then
         Print ("type Obj (<>) is tagged limited private;");
         New_Line;
         Print ("type Ptr      is access all Obj'class;");
         New_Line;
         Print ("subtype Java_Object is Object.Obj;");
         New_Line;
         Print ("subtype Object      is Ptr;");
      else
         if Get_Super_Class_Name (CF) = "java/lang/Exception" then
            Print (Package_Name & " : Exception;");
            New_Line;
         end if;
         --  Print the Obj declaration
         Print_Obj_Declaration (CF, Package_Name);
      end if;

      --  Print required type declarations
      Print_Types_Declaration (CF);

      --  Print Inner classes
      Print_Inner_Classes (CF);

      --  Print static and non static fields
      Print_Fields (CF);

      --  Print static and non static method
      Print_Methods (CF, Package_Name);

      if Class_Name_String = "java/lang/Object" then
         Indent (-2);
         New_Line;
         Print ("private");
         Indent (2);
         New_Line;
         Print ("type Obj is tagged limited null record;");
      end if;

      Indent (-2);
      New_Line;

      if Is_Inner_Class then
         Print ("end " & Package_Name & ";");
         New_Line;
      else
         Print ("end ");
         Print_Qualified_Name (Class_Name_String);
         Print (';');
         New_Line;
         Close_File;
      end if;

      String_List.Pop (In_List => Class_Definition_List);

   end Print_Header;

   --------------------------
   --  Print_Inner_Classes --
   --------------------------

   procedure Print_Inner_Classes (CF : in Class_File) is
      procedure Free is new Unchecked_Deallocation (Stream_Of_U1,
                                                    Stream_Of_U1_Ptr);
   begin
      for K in 0 .. Class_Attribute.Last (CF.Attributes) loop

         declare
            CA : constant Class_Attribute_Info
              := Class_Attribute.Get (CF.Attributes, K);
         begin
            if CA.Kind = Attr_Inner_Classes then

               for I in 0 .. Inner_Class.Last (CA.Classes) loop

                  declare
                     ICI : constant Inner_Class_Info
                       := Inner_Class.Get (CA.Classes, I);

                     Inner_Info : constant CP_Info
                       := CP.Get (CF.Constant_Pool,
                                  ICI.Inner_Class_Info_Index);
                     Inner_Name_Utf8 : constant Utf8.Table
                       := J_Utils.Get_Utf8 (CF.Constant_Pool,
                                            Inner_Info.Class_Name_Index);
                     IN_Bytes : Stream_Of_U1_Ptr;
                  begin

                     if ICI.Inner_Class_Info_Index /= CF.This_Class then

                        IN_Bytes := J_Basics.Get_Stream_Of_U1
                          (To_String (Inner_Name_Utf8) & ".class");

                        String_List.Add
                          (Item    => Convert (To_Wide_String
                                               (Inner_Name_Utf8)),
                           To_List => Class_Definition_List);
                        New_Line;
                        Print_Header (IN_Bytes.all, True);
                        Free (IN_Bytes);
                     end if;
                  end;

               end loop;
            end if;
         end;
      end loop;

   end Print_Inner_Classes;

   --------------------
   --  Print_Methods --
   --------------------

   procedure Print_Methods (CF           : in Class_File;
                            Package_Name : in Text_String) is
   begin
      for K in 0 .. JVM_File.Member.Last (CF.Methods) loop

         declare
            Method : constant Member_Info :=
              JVM_File.Member.Get (CF.Methods, K);
         begin
            if Is_Set (Method.Access_Flags, ACC_Public) then
               New_Line;
               if Is_Set (Method.Access_Flags, ACC_Static) then
                  Print_Class_Method (CF           => CF,
                                      Package_Name => Package_Name,
                                      Method       => Method,
                                      First_Arg    => "");
               else
                  Print_Class_Method (CF           => CF,
                                      Package_Name => Package_Name,
                                      Method       => Method,
                                      First_Arg    => "this : access Obj");
               end if;
            end if;
         end;
      end loop;
   end Print_Methods;

   --------------------------------
   --  Print_Method_Description  --
   --------------------------------

   procedure Print_Method_Description (Descriptor        : in Text_String;
                                       First_Arg        : in Text_String := "";
                                       Substitute_String : in Boolean := False)
   is
      Is_Procedure : Natural := 0;

      procedure Print_Method_Arguments
        (Descriptor        : in Text_String;
         Substitute_String : in Boolean;
         Separator         : in Text_Char := ';';
         Name              : in Text_Char := 'A');

      -----------------------------
      --  Print_Method_Arguments --
      -----------------------------

      procedure Print_Method_Arguments
        (Descriptor        : in Text_String;
         Substitute_String : in Boolean;
         Separator         : in Text_Char := ';';
         Name              : in Text_Char := 'A')
      is
         Declaration : Text_String := Next_Declaration_Pos (Descriptor);
      begin
         if Declaration'Length = 0 then
            return;
         end if;

         if Substitute_String and Declaration = "Ljava/lang/String;" then
            Print (Separator & Name & " : in Standard.String");

         elsif Substitute_String and Declaration = "[C" then
            Print (Separator & Name & " : in Wide_String");

         else
            Print (Separator & Name & " : in ");
            Print_Type_Description (Declaration);
         end if;

         Print_Method_Arguments
           (Descriptor (Descriptor'First + Declaration'Length
                        .. Descriptor'Last),
            Substitute_String, ';',
            Text_Char'Succ (Name));
      end Print_Method_Arguments;

      --  start of Print_Method_Description
   begin
      if Descriptor (2) /= ')' or First_Arg /= "" then
         Print (" (");
         Print_Method_Arguments (Descriptor (2 .. Descriptor'Last),
                                 Substitute_String, ' ');
         if First_Arg /= "" then
            if Descriptor (2) /= ')' then
               Print ("; ");
            end if;
            Print (First_Arg);
         end if;
         Print (')');
      end if;

      --  Check if we have a function or a procedure
      Is_Procedure := Ada.Strings.Wide_Fixed.Index (Descriptor, ")");
      if Descriptor (Is_Procedure + 1) /= 'V' then
         Print (" return ");
         Print_Type_Description
           (Descriptor (Is_Procedure + 1 .. Descriptor'Last));
      end if;
   end Print_Method_Description;

   ----------------------------
   --  Print_Obj_Declaration --
   ----------------------------

   procedure Print_Obj_Declaration (CF           : in Class_File;
                                    Package_Name : in Text_String) is
      Num_Interfaces : Int_32 :=  Class_Index.Last (CF.Interfaces);
      Record_Written : Boolean := False;
   begin

      Print ("type Obj is ");
      if   Is_Set (CF.Access_Flags, ACC_Interface)
        or Is_Set (CF.Access_Flags, ACC_Abstract) then
         Print ("abstract ");
      end if;

      Print ("new ");
      Print_Qualified_Name (Get_Super_Class_Name (CF));
      Print (".Obj with ");

      --  Add user-defined fields
      for K in 0 .. Member.Last (CF.Fields) loop

         declare
            Field : constant Member_Info :=
              JVM_File.Member.Get (CF.Fields, K);
         begin
            if Is_Set (Field.Access_Flags, ACC_Public)
              and not Is_Set (Field.Access_Flags, ACC_Static) then

               if not Record_Written then
                  Print ("record");
                  Indent (2);
                  Record_Written := True;
               end if;
               New_Line;
               Print_Class_Field (CF    => CF,
                                  Field => Field);
            end if;
         end;
      end loop;

      --  Add Interfaces
      for K in 0 .. Num_Interfaces loop

         declare
            Interf_Index : constant CP_Index :=
              Class_Index.Get (CF.Interfaces, K);
            Interf : constant CP_Info :=
              CP.Get (CF.Constant_Pool, Interf_Index);
            Interf_Name : constant Utf8.Table :=
              J_Utils.Get_Utf8 (CF.Constant_Pool, Interf.Class_Name_Index);
            W : Text_String := To_Text_String (Interf_Name);
            C : constant Text_String := Get_Class_Name (W);
         begin
            if not Record_Written then
               Print ("record");
               Indent (2);
               Record_Written := True;
            end if;
            New_Line;
            Print (C & " : ");
            Print_Qualified_Name (W);
            Print ("." & C & ";");
         end;
      end loop;

      if not Record_Written then
         Print (" null record;");
      else
         Indent (-2);
         New_Line;
         Print ("end record;");
      end if;

      if Is_Set (CF.Access_Flags, ACC_Interface) then
         New_Line;
         Print ("pragma Convention (Java_Interface, "
                & Package_Name & ".Obj);");
      end if;

      New_Line;
      Print ("type Ptr is access all "
             & Package_Name  & ".Obj'Class;");
      New_Line;
      Print ("subtype Java_" & Package_Name & " is "
             & Package_Name  & ".Obj;");
      New_Line;
      Print ("subtype " & Package_Name & " is Ptr;");
      New_Line;
   end Print_Obj_Declaration;

   ----------------------------
   --  Print_Qualified_Name  --
   ----------------------------

   procedure Print_Qualified_Name (W : in Text_String) is
   begin
      for J in W'Range loop
         case W (J) is
            when '/' | '$' => Print ('.');
            when ';'       => return;
            when others    => Print (W (J));
         end case;
      end loop;
   end Print_Qualified_Name;

   ------------------------------
   --  Print_Types_Declaration --
   ------------------------------

   procedure Print_Types_Declaration (CF : Class_File) is

      Types_Declarations_List : String_List.List;
      --  List of the array types already declared, so that we do not print
      --  the same twice

      procedure Print_Array_Declaration (Descriptor : in Text_String);
      --  print the Ada statements to declare an array,

      procedure Print_Single (Descriptor : in Text_String);
      --  Print the declaration for a single Type

      ------------------------------
      --  Print_Array_Declaration --
      ------------------------------

      procedure Print_Array_Declaration (Descriptor : in Text_String)
      is
         Bracket_Count : Natural := 0;
         Pos           : Natural := Descriptor'First;
         Name          : Short_String;
      begin
         --  If a similar declaration was already written then
         if String_List.Exists (Item    => Convert (Descriptor),
                                In_List => Types_Declarations_List) then
            return;
         end if;

         while Descriptor (Pos) = Open_Bracket loop
            Pos := Pos + 1;
            Bracket_Count := Bracket_Count + 1;
         end loop;

         if Descriptor (Pos) = L then
            Name := Convert (Get_Class_Name
                             (Descriptor (Pos + 1 .. Descriptor'Last - 1)));
         elsif Bracket_Count <= 3 then
            --  do nothing if we have a basic type and less than
            --  three brackets. In those cases, the array is already declared
            --  in package Java
            return;
         else
            Name := Convert (Get_Simple_Description (Descriptor (Pos)));
         end if;

         String_List.Add (Item    => Convert (Descriptor),
                          To_List => Types_Declarations_List);

         declare
            Brackets : Text_String := To_Text_String (Bracket_Count);
            TypeDesc : Text_String := "type " & Name.Str & "_array_"
              & Brackets & " is array (";
         begin
            Print (TypeDesc);

            Indent (TypeDesc'Length);
            for I in 1 .. Bracket_Count - 1 loop
               Print ("natural range <>,");
               New_Line;
            end loop;
            Indent (-TypeDesc'Length);

            Print ("natural range <>) of ");

            if Descriptor (Pos) = L then
               Print_Class_Description (Descriptor (Pos .. Descriptor'Last));
            else
               Print (Name.Str);
            end if;

            Print (';');
            New_Line;

            Print ("type " & Name.Str & "_Arr_" & Brackets
                   & " is access all " & Name.Str & "_Array_"
                   & Brackets & ";");
            New_Line;

            Print ("pragma Convention (Java, " & Name.Str
                   & "_Array_" & Brackets & ");");
            New_Line;

            Print ("pragma Open_Access_Type (" & Name.Str
                   & "_Arr_" & Brackets & ");");
            New_Line;
            New_Line;
         end;

      end Print_Array_Declaration;

      -------------------
      --  Print_Single --
      -------------------

      procedure Print_Single (Descriptor : in Text_String)
      is
         I  : Natural := Descriptor'First + 1;
      begin
         case Descriptor (Descriptor'First) is
            when Open_Bracket => Print_Array_Declaration (Descriptor);
            when Open_Paren   =>
               loop
                  declare
                     Declaration : Text_String
                       := Next_Declaration_Pos (Descriptor
                                                (I .. Descriptor'Last));
                  begin
                     exit when Declaration'Length = 0;
                     if Declaration (Declaration'First) = Open_Bracket then
                        Print_Array_Declaration (Declaration);
                     end if;
                     I := I + Declaration'Length;
                  end;
               end loop;
               --  process return type too
               if Descriptor (I + 1) = Open_Bracket then
                  Print_Array_Declaration (Descriptor
                                           (I + 1 .. Descriptor'Last));
               end if;
            when others       => null;
         end case;
      end Print_Single;

      --  Start of Print_Types_Declaration

   begin
      for K in 0 .. Member.Last (CF.Fields) loop
         declare
            Field : constant Member_Info := Member.Get (CF.Fields, K);
            Descriptor : constant Utf8.Table :=
              J_Utils.Get_Utf8 (CF.Constant_Pool, Field.Descriptor_Index);
         begin
            if Is_Set (Field.Access_Flags, ACC_Public) then
               Print_Single (To_Text_String (Descriptor));
            end if;
         end;
      end loop;

      for K in 0 .. Member.Last (CF.Methods) loop
         declare
            Method : constant Member_Info := Member.Get (CF.Methods, K);
            Descriptor : constant Utf8.Table :=
              J_Utils.Get_Utf8 (CF.Constant_Pool, Method.Descriptor_Index);
         begin
            if Is_Set (Method.Access_Flags, ACC_Public) then
               Print_Single (To_Text_String (Descriptor));
            end if;
         end;
      end loop;

      String_List.Clean (Types_Declarations_List);
   end Print_Types_Declaration;

   -----------------------------
   --  Print_Type_Description --
   -----------------------------

   procedure Print_Type_Description (Descriptor     : in Text_String) is
   begin
      case Descriptor (Descriptor'First) is
         when L  =>  Print_Class_Description (Descriptor);
         when Open_Bracket => Print_Array_Description (Descriptor);
         when Close_Paren => null;
         when B | C | D | F | I | J | S | Z | V =>
            Print (Get_Simple_Description (Descriptor (Descriptor'First)));
         when others =>
            raise Constraint_Error;
      end case;
   end Print_Type_Description;

   -----------------
   --  Print_With --
   -----------------

   procedure Print_With (CF           : in Class_File;
                         Package_Name : in Text_String) is

      procedure Print_With_Descriptor (Index : CP_Index);

      With_List : String_List.List;

      -----------------------------
      --  Print_With_Descriptor  --
      -----------------------------

      procedure Print_With_Descriptor (Index : CP_Index) is
         Descriptor : constant Utf8.Table := J_Utils.Get_Utf8
           (CF.Constant_Pool, Index);
         W : Text_String := To_Text_String (Descriptor);
         Start_Index : Natural := 0;
      begin
         for I in W'Range loop

            if W (I) = 'L' and Start_Index = 0 then
               Start_Index := I + 1;
            elsif W (I) = ';' then
               --  Do not print a 'with' statement for the current
               --  package, nor for the inner classes
               if W (Start_Index .. I - 1) /= Package_Name
                 and then not String_List.Exists
                 (Item    => Convert (W (Start_Index .. I - 1)),
                  In_List => With_List)
               then
                  Print ("with ");
                  Print_Qualified_Name (W (Start_Index .. I - 1));
                  Print (';');
                  New_Line;
                  String_List.Add
                    (Item    => Convert (W (Start_Index .. I - 1)),
                     To_List => With_List);
               end if;
               Start_Index := 0;
            end if;
         end loop;
      end Print_With_Descriptor;

   begin
      Print ("with Java; use Java;");
      New_Line;

      if Package_Name /= "java/lang/Object" then
         declare
            Super     : constant Text_String := Get_Super_Class_Name (CF);
         begin
            Print ("with ");
            Print_Qualified_Name (Super);
            Print (';');
            New_Line;

            String_List.Add (Item    => Convert (Super),
                             To_List => With_List);
         end;
      end if;

      --  We do not want to print the 'with' statements for the inner classes
      --  so we do as if they were already printed
      for K in 0 .. Class_Attribute.Last (CF.Attributes) loop

         declare
            CA : constant Class_Attribute_Info
              := Class_Attribute.Get (CF.Attributes, K);
         begin
            if CA.Kind = Attr_Inner_Classes then

               for I in 0 .. Inner_Class.Last (CA.Classes) loop

                  declare
                     ICI : constant Inner_Class_Info
                       := Inner_Class.Get (CA.Classes, I);
                     Inner_Info : constant CP_Info
                       := CP.Get (CF.Constant_Pool,
                                  ICI.Inner_Class_Info_Index);
                     Inner_Name_Utf8 : constant Utf8.Table
                       := J_Utils.Get_Utf8 (CF.Constant_Pool,
                                            Inner_Info.Class_Name_Index);
                  begin
                     String_List.Add (Item => Convert (To_Wide_String
                                                       (Inner_Name_Utf8)),
                                      To_List => With_List);
                  end;
               end loop;
            end if;
         end;
      end loop;


      --  Print the required include files for the interfaces
      for K in 0 .. Class_Index.Last (CF.Interfaces) loop
         declare
            Interf_Index : constant CP_Index :=
              Class_Index.Get (CF.Interfaces, K);
            Interf : constant CP_Info :=
              CP.Get (CF.Constant_Pool, Interf_Index);
            Interf_Name : constant Utf8.Table :=
              J_Utils.Get_Utf8 (CF.Constant_Pool, Interf.Class_Name_Index);
            W : Text_String := To_Text_String (Interf_Name);
         begin
            Print ("with ");
            Print_Qualified_Name (W);
            Print (';');
            New_Line;
            String_List.Add (Item    => Convert (W),
                             To_List => With_List);
         end;
      end loop;

      --  Print the required include files for the fields
      for K in 0 .. Member.Last (CF.Fields) loop
         declare
            Field : constant Member_Info := Member.Get (CF.Fields, K);
         begin
            if Is_Set (Field.Access_Flags, ACC_Public) then
               Print_With_Descriptor (Field.Descriptor_Index);
            end if;
         end;
      end loop;

      --  Print the required include files for the methods
      for K in 0 .. Member.Last (CF.Methods) loop
         declare
            Method : constant Member_Info := Member.Get (CF.Methods, K);
         begin
            if Is_Set (Method.Access_Flags, ACC_Public) then
               Print_With_Descriptor (Method.Descriptor_Index);
            end if;
         end;
      end loop;

      String_List.Clean (With_List);
   end Print_With;

   ---------------------
   --  To_Text_String --
   ---------------------

   function To_Text_String (T : in Utf8.Table) return Text_String is
   begin
      return To_Wide_String (T);
   end To_Text_String;

   function To_Text_String (N : in Natural) return Text_String is
      S : constant Text_String := Natural'Wide_Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end To_Text_String;

   function To_Text_String (N : in U4) return Text_String is
      S : constant Text_String := U4'Wide_Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end To_Text_String;








end JVM_Ada;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.5
--  date: Fri May 15 17:07:53 1998;  author: briot
--  (Print_Constant_Value): corrected print for integer value (they could not
--      be negative
--  (Print_Array_Declaration): Add .Obj for the content type of the array
--      Suppressed the 'Start_Pos' parameter, and now only accepts a class
--      description for argument
--  (Print_Class_Method): change the 'this' parameter for a constructor
--  (Print_Header): Swapped printing of arrays declaration and Obj declaration
--  (Print_With): do not print 'with' statements for inner classes
--  ----------------------------
--  revision 1.6
--  date: Mon May 18 18:29:51 1998;  author: briot
--  (Print_Obj_Declaration): changed the line defined the Ptr subtype
--  Class_Definition_List is used to prevent 'invalid prefix in selected
--     component' errors
--  (Print_Class_Description): Print a simplified syntax when dealing with the
--     package currently defined, to avoid compilation errors.
--  Style changed to conform to gnat programming style
--  functions sorted alphabetically
--  (Create_Directory_Class): new routine
--  ----------------------------
--  revision 1.7
--  date: Tue May 19 19:00:05 1998;  author: briot
--  (Get_Short_Class_Description): gone
--  (Next_Declaration_Pos): returns the current declaration
--  (Print_Constant_Value): take the index in the attributes table where the
--   constant value is
--  (Print_With): Do not call Get_Super_Class_Name when we are creating
--   java/lang/Object
--  Types_Declaration_list: Move to Print_Types_Declaration
--  (Print_Types_Declaration): only print array declarations for public fields
--   and methods
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
--  (Generate_Java): print a ';' at the end of an array declaration
--  (Print_Single): print an array declaration for functions'return types too.
