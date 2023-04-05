------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M _ F I L E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.18 $                              --
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

--  The following provides the basic types, attributes, opcodes (aka bytecodes)
--  and, in general, models the information contained in a class file.

--  A class file is an array of bytes where all the info for a JVM class is
--  stored in a compressed format. A class file contains a symbol table (called
--  constant pool), the list of fields and methods contained in the class as
--  well as the access and modifier information for the class, fields and
--  methods. In addition, each field, method or the class as a whole, has
--  associated a set of attributes that specify various kind of informations
--  such as the name of the source file from which the class file was produced,
--  whether a field is constant or, and most important, the JVM code for each
--  method in the class, as well as various tables containing optional
--  debugging information.

--  For more information on a class file please refer to the following book:
--  "The Java Virtual Machine Specification", by Tim Lindholm and Franck
--  Yellin, publishd by Addison-Wesley, 1997.

--  The routines and data structures declared below allow to read and write the
--  elements of a class file conveniently.  To be able to generate a class file
--  as a stream of bytes, the user of this package must first construct an
--  object of type Class_File along with all the data structures it requires.

--  Several tables are required by the data structures below.  These tables are
--  instantiations of the generic J_Table and carry the restrictions documented
--  in the spec of J_Table.

--  NOTE: In the following we have retained the same entity names as those
--  specified in the above book with the exception that identifiers have been
--  capitalized according to GNAT's coding style and identifiers like
--  "ClassFile" have been changed into "Class_File". In addition, because each
--  Table field contained in a JVM record structure already contains its
--  length, we have omitted all the corresponding length fields that appeared
--  in the JVM specification.

with J_Table;
with J_Types; use J_Types;

package JVM_File is

   ---------------------
   -- Basic Constants --
   ---------------------

   SUN_Magic_Number : constant U4 := 16#CAFE_BABE#;
   --  Class file magic number

   SUN_Minor_Version : constant U2 := 3;
   SUN_Major_Version : constant U2 := 45;
   --  Minor/Major class file version as defined by SUN

   -------------------------
   -- Storage Reclamation --
   -------------------------

   procedure Set_Water_Mark;
   procedure Free_To_Next_Water_Mark;
   --  After having allocated a number of data structures needed by a class
   --  file, it may be desirable to reclaim the storage used by these data
   --  structures. To achieve this we use a water marking protocol very similar
   --  to the one described in the spec of J_Table:
   --
   --  1. Before allocating the data structures call Set_Water_Mark.
   --
   --  2. To reclaim the storage for the data structures allocated from the
   --     last call to Set_Water_Mark, just call Free_To_Next_Water_Mark.
   --
   --  The typical use of these routines is as follows:
   --
   --     Set_Water_Mark;
   --      ...    --  generate class file data structures,
   --      ...    --  use them
   --      ...    --  and then free the storage used by the tables
   --     Free_To_Next_Water_Mark;
   --
   --  If you intermix the creation of several class file structures, be
   --  careful to free storage only when you are done with all of them.

   ------------------------
   -- Utf8 Strings Table --
   ------------------------

   package Utf8 is new J_Table (Data => U1, Index => U2,
                                Default_Init => False);
   --  Utf8.Table is the table containing all Utf8 strings of a class file

   procedure Append_String (T : in out Utf8.Table; S : String);
   --  Appends string S to the end of T.  S should only contain characters
   --  whose position is between 1 and 127.  If this is not the case the
   --  behavior of this routine is undefined.  If T is not an expandable
   --  Utf8.Table an exception is raised.

   procedure Append (T : in out Utf8.Table; C : Character);
   procedure Append (T : in out Utf8.Table; X : Wide_Character);
   procedure Append (T : in out Utf8.Table; S : String);
   procedure Append (T : in out Utf8.Table; W : Wide_String);
   --  Appends a character/wide character/string/wide string to the end of T.
   --  T must be an expandable Utf8.Table otherwise an exception is raised.

   function Has_Wide_Chars (T : Utf8.Table) return Boolean;
   --  Returns True if table T contains wide characters

   function To_String (T : Utf8.Table) return String;
   --  Returns the Ada string that corresponds to T. If Has_Wide_Chars (T) is
   --  True an exception is raised.

   function To_Wide_String (T : Utf8.Table) return Wide_String;
   --  Returns the Ada wide string that corresponds to T

   ------------------------
   -- Constant Pool (CP) --
   ------------------------

   --  The CP is the symbol table of a JVM class file. Each entry in this table
   --  represents either some integer/floating-point/string constant, or the
   --  name of a class/field/method or the type signature of a field/method.
   --  Each CP entry has a tag (see CP_Tag below) giving the nature of the
   --  entry.  Indices of the CP table (see CP_Index below) are referred to
   --  from within the class file structure and its substructures.

   type CP_Tag is (

      CONSTANT_Empty,
      --  Tag of a CP entry that is empty and must be ignored. There are two
      --  types of CP entries that have this tag. The first is for the CP entry
      --  at CP_Index zero. The second is the CP entry immediately following a
      --  CONSTANT_Long or CONSTANT_Double entry. In fact the JVM specification
      --  requires that if a CONSTANT_Long or CONSTANT_Double entry is at
      --  CP_Index n, then the next valid item in the constant pool be located
      --  at index n+2. The constant_pool index n+1 must be considered invalid
      --  and must not be used.

      CONSTANT_Utf8,
      --  Unicode string

      CONSTANT_Integer,
      --  4 byte integer constant

      CONSTANT_Float,
      --  4 byte floating point constant

      CONSTANT_Long,
      --  8 byte integer constant

      CONSTANT_Double,
      --  8 byte floating point constant

      CONSTANT_Class,
      --  Used to represent a class or interface

      CONSTANT_String,
      --  Used to represent a constant object of the type java.lang.String

      CONSTANT_Fieldref,
      --  Used to represent a field of a class

      CONSTANT_Methodref,
      --  Used to represent a method of a class

      CONSTANT_Interface_Methodref,
      --  Used to represent a method of an interface

      CONSTANT_Name_And_Type
      --  Used to give the name and type descriptor of a field or method
   );

   for CP_Tag'Size use 8;
   for CP_Tag use (
      CONSTANT_Empty               => 16#00#,
      CONSTANT_Utf8                => 16#01#,
      CONSTANT_Integer             => 16#03#,
      CONSTANT_Float               => 16#04#,
      CONSTANT_Long                => 16#05#,
      CONSTANT_Double              => 16#06#,
      CONSTANT_Class               => 16#07#,
      CONSTANT_String              => 16#08#,
      CONSTANT_Fieldref            => 16#09#,
      CONSTANT_Methodref           => 16#0A#,
      CONSTANT_Interface_Methodref => 16#0B#,
      CONSTANT_Name_And_Type       => 16#0C#
   );

   -------------------------
   -- Constant Pool Index --
   -------------------------

   type CP_Index is new U2;
   --  Index of a CP table entry

   CP_Empty : constant CP_Index := 0;
   --  The CP entry found at this index value has a CONSTANT_Empty tag

   subtype CP_Index_Class            is CP_Index;
   --  Index of a CONSTANT_Class entry

   subtype CP_Index_Field            is CP_Index;
   --  Index of a CONSTANT_Fieldref entry

   subtype CP_Index_Method           is CP_Index;
   --  Index of a CONSTANT_Methodref entry

   subtype CP_Index_Interface_Method is CP_Index;
   --  Index of a CONSTANT_Interface_Methodref entry

   subtype CP_Index_Name_And_Type    is CP_Index;
   --  Index of a CONSTANT_Name_And_Type entry

   subtype CP_Index_Utf8             is CP_Index;
   --  Index of a CONSTANT_Utf8 entry

   subtype CP_Index_Constant         is CP_Index;
   --  Index of a CONSTANT_Integer, CONSTANT_Float, CONSTANT_Long,
   --  CONSTANT_Double or CONSTANT_String entry.

   subtype CP_Index_Constant_U4      is CP_Index;
   --  Index of a CONSTANT_Integer, CONSTANT_Float, or CONSTANT_String entry

   subtype CP_Index_Constant_U8      is CP_Index;
   --  Index of a CONSTANT_Long or CONSTANT_Double entry

   package Class_Index is new J_Table (Data => CP_Index_Class, Index => U2);
   --  Class_Index.Table is a table of CP_Index_Class

   -------------------------
   -- Constant Pool Entry --
   -------------------------

   type CP_Info (Tag : CP_Tag := CONSTANT_Empty) is record
      case Tag is
         when CONSTANT_Empty =>
            null;

         when CONSTANT_Class =>
            Class_Name_Index : CP_Index_Utf8 := CP_Empty;
            --  Fully qualified name of the class or interface.
            --  In a fully qualified class name every "." is replaced by a "/".
            --  Array types are also represented by CONSTANT_Class entries.
            --  In this case the name has the form of an array type descriptor
            --  (e.g., the name of the Java array type `int[][]' is "[[I").

         when CONSTANT_Fieldref
           |  CONSTANT_Methodref
           |  CONSTANT_Interface_Methodref =>
            Class_Index         : CP_Index_Class := CP_Empty;
            --  Class/interface containing the declaration of the field/method

            Name_And_Type_Index : CP_Index_Name_And_Type := CP_Empty;
            --  Name and type descriptor of the field/method

         when CONSTANT_String =>
            String_Index : CP_Index_Utf8 := CP_Empty;
            --  The sequence of characters of a java.lang.String object

         when CONSTANT_Integer
           |  CONSTANT_Float  =>
            Bytes : U4 := 0;
            --  Contains the value of a 4 byte integer/IEEE float constant

         when CONSTANT_Long
           |  CONSTANT_Double =>
            High_Bytes : U4 := 0;
            Low_Bytes  : U4 := 0;
            --  Contain the value of an 8 byte integer/IEEE float constant

         when CONSTANT_Name_And_Type =>
            Name_Index       : CP_Index_Utf8 := CP_Empty;
            Descriptor_Index : CP_Index_Utf8 := CP_Empty;
            --  Simple name & type descriptor (respectively) of a field/method

         when CONSTANT_Utf8 =>
            Str_Bytes : Utf8.Table;
            --  Table of bytes for the Utf8 string. The original field name
            --  was Bytes but had to be changed because it conflicted with
            --  the field name Bytes for CONSTANT_Integer/CONSTANT_Float.
      end case;
   end record;

   package CP is new J_Table (Data => CP_Info, Index => CP_Index);
   --  A CP.Table is a table of constant pool entries, that is a CT.Table is
   --  the constant pool of a class file. The zero-th entry of a CP.Table must
   --  have a CONSTANT_Empty tag.

   --------------------------------
   -- CP_Index Testing Functions --
   --------------------------------

   --  For each constant pool tag CONSTANT_XX, we define:
   --
   --       function Is_XX (T : CP.Table; K : CP_Index) return Boolean;
   --
   --  T is a constant pool table and K an index in this table.  The function
   --  returns True if the tag of the K-th entry in T is CONSTANT_XX.

   function Is_Class               (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Fieldref            (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Methodref           (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Interface_Methodref (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Name_And_Type       (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Utf8                (T : CP.Table; K : CP_Index) return Boolean;
   function Is_String              (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Integer             (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Float               (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Long                (T : CP.Table; K : CP_Index) return Boolean;
   function Is_Double              (T : CP.Table; K : CP_Index) return Boolean;

   --  In addition to the above we provide the following 3 routines:

   function Is_Constant    (T : CP.Table; K : CP_Index) return Boolean;
   --  Returns True if the K-th entry in T is an Integer, Float, Long, Double
   --  or String Constant.

   function Is_Constant_U4 (T : CP.Table; K : CP_Index) return Boolean;
   --  Returns True if the K-th entry in T is an Integer, Float or String
   --  Constant.

   function Is_Constant_U8 (T : CP.Table; K : CP_Index) return Boolean;
   --  Returns True if the K-th entry in T is a Long or Double Constant

   --------------------------------
   -- Access and Modifiers Flags --
   --------------------------------

   --  A class, field or method has an access/modifier mask where various
   --  properties of the entity are specified. See below for the complete list.

   type Access_Flag is (

      ACC_Public,
      --  Is public, ie may be accessed from outside its JVM package

      ACC_Private,
      --  Is private, ie usable only within the defining class
      --  Fields and Methods only.

      ACC_Protected,
      --  Is protected, ie may be accessed within subclasses.
      --  Fields and Methods only.

      ACC_Static,
      --  Is static, ie applies to the class as a whole.
      --  Fields and Methods only.

      ACC_Final,
      --  Is final, no subclasses or overriding possible

      ACC_Synchronized,
      --  Is synchronized, ie wrap its use in a monitor lock.
      --  Methods only.

      ACC_Volatile,
      --  Is volatile, should not be cached by the JVM.
      --  Fields only.

      ACC_Transient,
      --  Is transient, ie not writtent or read by a persistent object manager.
      --  Fields only.

      ACC_Native,
      --  Is native, implemented on the native machine.
      --  Methods only.

      ACC_Interface,
      --  Is an interface.
      --  Classes only.

      ACC_Abstract
      --  Is abstract, no implementation is provided.
      --  Classes and methods only.
   );

   for Access_Flag'Size use 16;
   for Access_Flag use (
      ACC_Public       => 16#00_01#,
      ACC_Private      => 16#00_02#,
      ACC_Protected    => 16#00_04#,
      ACC_Static       => 16#00_08#,
      ACC_Final        => 16#00_10#,
      ACC_Synchronized => 16#00_20#,
      ACC_Volatile     => 16#00_40#,
      ACC_Transient    => 16#00_80#,
      ACC_Native       => 16#01_00#,
      ACC_Interface    => 16#02_00#,
      ACC_Abstract     => 16#04_00#
   );

   type Access_Mask is new U2;
   --  16-bit mask of access and modifiers flags of a class, field or method

   Empty_Mask         : constant Access_Mask := 16#0000#;
   --  The empty access mask

   Default_Class_Mask : constant Access_Mask := 16#0020#;
   --  The default access mask for a class. This mask has a special bit set
   --  which must be set for all generated classes. The bit is referred to as
   --  the ACC_Super field in the JVM specification.

   function Is_Set (M : Access_Mask; F : Access_Flag) return Boolean;
   --  Given a mask M of access and modifiers properties and a flag F,
   --  return True if F is set in M.

   procedure Set (M : in out Access_Mask; F : Access_Flag);
   --  Set flag F in mask M. Note that there is no Clear procedure. The normal
   --  way to set an access flag is to set it initially to either the
   --  Empty_Mask or the Default_Class_Mask (depending on whether the mask is
   --  for a class or not), and then use the Set procedure to set each
   --  individual flag.

   ----------------
   -- Attributes --
   ----------------

   --  Four types of attributes are used in the structures of a class file.
   --  Class/field/method attributes apply to the corresponding entity in a
   --  class file. Code attributes apply to the code of a method in the class.

   type Attribute_Kind is (
   --  List of all the attributes currently documented in the JVM

      Attr_Unknown,
      --  Unrecognized attribute

      Attr_Deprecated,
      --  If present the class, field or method is deprecated

      ---------------------
      -- Field Attribute --
      ---------------------

      Attr_Constant_Value,
      --  Gives the value of a constant field

      ------------------------------
      -- Field & Method Attribute --
      ------------------------------

      Attr_Synthetic,
      --  If present the field or method does not exist in the source and has
      --  been synthesized by the compiler.

      -----------------------
      -- Method Attributes --
      -----------------------

      Attr_Code,
      --  Gives the actual bytecode and auxiliary information of a method

      Attr_Exceptions,
      --  Indicates which checked exceptions a method may throw

      ---------------------
      -- Code Attributes --
      ---------------------

      Attr_Line_Number_Table,
      --  Gives the mapping between source line numbers and the JVM code of a
      --  given method.

      Attr_Local_Variable_Table,
      --  Gives the mapping between variable names in the source and JVM local
      --  variables used in the code of a given method.

      ----------------------
      -- Class Attributes --
      ----------------------

      Attr_Source_File,
      --  Gives the source file name from which the class file was created

      Attr_Inner_Classes
      --  Gives the encoding of the inner class names, if any.  Every class
      --  that contains nested classes, has an InnerClasses attribute, with a
      --  record for each nested class.  Likewise, every nested class has an
      --  InnerClasses attribute with a record for itself and all of its
      --  enclosing classes, except the outermost. See below for an explanation
      --  of inner classes.
   );

   function Attribute_Name (A : Attribute_Kind) return String;
   --  In the JVM attributes are specified by giving the name of the attribute,
   --  rather than giving a tag. This function returns the actual name of
   --  attribute A as found in a JVM class file.

   function Get_Attribute (T : CP.Table; K : CP_Index) return Attribute_Kind;
   --  T is a constant pool table and K the index of a Utf8 entry in T.  The
   --  function returns the Attribute_Kind corresponding to the string found in
   --  the Utf8 entry. If the string does not correspond to the name of a known
   --  attribute Attr_Unknown is returned.

   ----------------------------------
   -- Code Attribute Specific Data --
   ----------------------------------

   --  The bytecodes of a method are gathered in a table called the code array.
   --  A code array is saved as part of the code attribute of the method.
   --  In the current JVM implementation a code array can contain up to
   --  2 ** 16 bytes.

   type Byte_Index is new U2;
   --  Index type of a Code_Array.Table (see below)

   package Code_Array is new J_Table (Data => U1, Index => Byte_Index,
                                      Default_Init => False);
   --  Code_Array.Table is the code array of a method. It contains the bytes of
   --  the instructions (i.e. bytecode) of a JVM method.

   subtype Instruction_Index is Byte_Index;
   --  While the index of an arbitrary byte in a Code_Array.Table is a
   --  Byte_Index, the index in a Code_Array.Table of the beginning of a JVM
   --  Instruction is an Instruction_Index to avoid possible confusions. See
   --  the end of this package for the definition of a JVM Instruction.

   type Handler_Info is record
   --  Describes an exception handler in the code array

      Start_PC   : Instruction_Index := 0;
      End_PC     : Instruction_Index := 0;
      --  Indicate the index range in the code array at which the exception
      --  handler is active. The range is [Start_Pc, End_Pc) (End_Pc excluded).

      Handler_PC : Instruction_Index := 0;
      --  Gives the index of the exception handler in the code array

      Catch_Type : CP_Index_Class := CP_Empty;
      --  If zero the exception handler applies to all exceptions. Otherwise,
      --  the CP entry at this index gives the name of the exception class.
   end record;

   package Handler is new J_Table (Data => Handler_Info, Index => U2);
   --  Handler.Table ia a table of Handler_Info records

   -----------------------------------------------
   -- Line Number Table Attribute Specific Data --
   -----------------------------------------------

   type Line_Info is record
   --  Matches source lines to indices of a code array

      Start_PC    : Instruction_Index := 0;
      --  Index of the code array at which begin the JVM instructions generated
      --  for a given line in the original source file.

      Line_Number : U2 := 0;
      --  The corresponding line number in the source file
   end record;

   package Line is new J_Table (Data => Line_Info, Index => U2);
   --  Line.Table is a table of Line_Info

   --------------------------------------------------
   -- Local Variable Table Attribute Specific Data --
   --------------------------------------------------

   --  Every method can access up to 2 ** 16 local variables. If a static
   --  method has n parameters they are passed as local variables 0 to n-1. For
   --  non static methods, local variable 0 always contains the THIS parameter.
   --  The n explicit parameters are passed as local variables 1 to n.

   type Local_Variable_Index is new U2;
   --  Index of a local variable

   type Variable_Info is record
   --  Matches local variables in a code array to source code variables

      Start_PC         : Instruction_Index := 0;
      Length           : U2 := 0;
      --  The range of instructions in the code array in which the JVM local
      --  variable is defined. The range is [Start_PC, Start_PC+Length].

      Name_Index       : CP_Index_Utf8 := CP_Empty;
      Descriptor_Index : CP_Index_Utf8 := CP_Empty;
      --  Simple name & type descriptor of the variable in the original source

      Index            : Local_Variable_Index := 0;
      --  Index of the JVM local variable. If the local variable is a two-word
      --  type (long or double) then it occupies positions Index and Index + 1.
   end record;

   package Variable is new J_Table (Data => Variable_Info, Index => U2);
   --  Variable.Table is a table of Variable_Info records

   -------------------------------------------
   -- Inner Classes Attribute Specific Data --
   -------------------------------------------

   --  The largest enhancement to the Java programming language in version 1.1
   --  is the feature of inner classes.  Java 1.0 allowed classes and
   --  interfaces to be defined only at the top level as Java package
   --  members. Java 1.1 adds one new type of top-level class and interface and
   --  adds 3 new types of inner classes as explained below.

   --  Nested Top-Level Classes and Interfaces
   --  ---------------------------------------

   --  A nested top-level class or inteface is defined as a static member of an
   --  enclosing top-level class or interface (static modifier present). A
   --  nested top-level class or interface is a regular member of a Java
   --  package. The only difference is that the name of a nested top-level
   --  class or interface includes the name of the class in which it is defined
   --  using the dotted notation (exactly like in Ada for nested packages).

   --  Member Classes
   --  --------------

   --  A member class is defined as a member of an enclosing class (no static
   --  modifier present). This makes it an inner rather than top-level class. A
   --  member class is analogous to other members (fields or methods). The code
   --  of member classes can implicitely refer to any of the fiedls and
   --  methods, including the private ones, of the enclosing
   --  class. Furthermore, every instance of a member class is associated with
   --  an enclosing instance of the class that defines it.

   --  Local Classes
   --  -------------

   --  A local class is an inner class defined within a block of Java code. It
   --  is visible only within that block. Local classes are not member
   --  classes, but can still use the fields and methods of the enclosing
   --  classes. Local classes are primarily used as adapter classes needed in
   --  the new event-handling model defined in the Java Development Kit 1.1.

   --  Anonymous Classes
   --  -----------------

   --  An anonymous class is a local class with no name that is instantiated in
   --  the same expression that defines it.

   type Inner_Class_Info is record
   --  Gives the encoding of the name of a nested class.

      Inner_Class_Info_Index   : CP_Index_Class := CP_Empty;
      --  The nested class

      Outer_Class_Info_Index   : CP_Index_Class := CP_Empty;
      --  The class where the above nested class is declared (ie the defining
      --  scope of the nested class).  If the inner class is not a member class
      --  (see above), this field is set to zero.

      Inner_Name_Index         : CP_Index_Utf8 := CP_Empty;
      --  The simple name of the nested class in the original source file. If
      --  the inner class is anonymous this field is set to zero.

      Inner_Class_Access_Flags : Access_Mask := Empty_Mask;
      --  Bitmask of the originally declared access flags for the nested class.
      --  If a class C was declared protected, the public access flag bit is
      --  cleared in its Inner_Class_Info record, even though it is set in C's
      --  access_flags field.
   end record;

   package Inner_Class is new J_Table (Data => Inner_Class_Info, Index => U2);
   --  Inner_Class.Table is a table of Inner_Class_Info records. It is used to
   --  give the naming conventions of inner classes.

   -------------------------
   -- Code_Attribute_Info --
   -------------------------

   type Code_Attribute_Info (Kind : Attribute_Kind := Attr_Unknown) is record
   --  Gives the contents of an attribute of a code attribute

      Attribute_Name_Index : CP_Index_Utf8 := CP_Empty;
      --  Name of the attribute (one of the strings returned by Attribute_Name)

      Attribute_Length : U4 := 0;
      --  Length (in bytes) of the following info when stored in a class file

      case Kind is
         when Attr_Line_Number_Table =>
            Line_Number_Table    : Line.Table;
            --  Each entry gives the correspondence between line number in the
            --  original source file and starting index in the code array.

         when Attr_Local_Variable_Table =>
            Local_Variable_Table : Variable.Table;
            --  Gives the mapping between JVM local variables used in a code
            --  array and the variables in the original source.

         when others =>
            null;
      end case;
   end record;

   package Code_Attribute is new J_Table (Data  => Code_Attribute_Info,
                                          Index => U2);
   --  Code_Attribute.Table is the table of Code_Attribute_Info records

   ---------------------------
   -- Member_Attribute_Info --
   ---------------------------

   type Member_Attribute_Info (Kind : Attribute_Kind := Attr_Unknown) is record
   --  Gives the contents of an attribute for a field or method

      Attribute_Name_Index : CP_Index_Utf8 := CP_Empty;
      Attribute_Length     : U4 := 0;
      --  same as for Code_Attr_Info

      case Kind is
         when Attr_Constant_Value =>
            Constant_Value_Index : CP_Index_Constant := CP_Empty;
            --  For a constant field gives its value

         when Attr_Deprecated
           |  Attr_Synthetic  =>
            null;

         when Attr_Code =>
            Max_Stack       : U2 := 0;
            --  The max number of words (word = 4 bytes) on the operand stack
            --  at any point during the execution of the corresponding method.

            Max_Locals      : U2 := 0;
            --  The number of local variables used by the corresponding method,
            --  including the parameters passed to the method on invocation.

            Code            : Code_Array.Table;
            --  The code array for the corresponding method

            Exception_Table : Handler.Table;
            --  Each entry describes one exception handler for the code array

            Attributes      : Code_Attribute.Table;
            --  Attributes associated to the code array

         when Attr_Exceptions =>
            Exception_Index_Table : Class_Index.Table;
            --  Each entry gives an exceptions class that the corresponding
            --  method is declared to throw.

         when others =>
            null;
      end case;
   end record;

   package Member_Attribute is new J_Table (Data  => Member_Attribute_Info,
                                            Index => U2);
   --  Member_Attribute.Table is the table of Member_Attribute_Info records

   --------------------------
   -- Class_Attribute_Info --
   --------------------------

   type Class_Attribute_Info (Kind : Attribute_Kind := Attr_Unknown) is record
   --  Gives the contents of an attribute for a class

      Attribute_Name_Index : CP_Index_Utf8 := CP_Empty;
      Attribute_Length     : U4 := 0;
      --  Same as in Class_Attribute_Info

      case Kind is
         when Attr_Source_File =>
            Source_File_Index : CP_Index_Utf8 := CP_Empty;
            --  Gives the simple name (no directory information) of the source
            --  file from which this class was compiled.

         when Attr_Inner_Classes =>
            Classes : Inner_Class.Table;
            --  Gives the encoding of inner class names

         when others =>
            null;
      end case;
   end record;

   package Class_Attribute is new J_Table (Data  => Class_Attribute_Info,
                                           Index => U2);
   --  Class_Attribute.Table is the table of Class_Attribute_Info records

   ----------------------
   -- Fields & Methods --
   ----------------------

   type Member_Info is record
   --  Describes the information of each field/method in the class

      Access_Flags : Access_Mask := Empty_Mask;
      --  Gives the properties and access permissions of the field/method

      Name_Index       : CP_Index_Utf8 := CP_Empty;
      Descriptor_Index : CP_Index_Utf8 := CP_Empty;
      --  Simple name & type descriptor (respectively) of the field/method

      Attributes       : Member_Attribute.Table;
      --  Attributes that apply to the field/method
   end record;

   package Member is new J_Table (Data => Member_Info, Index => U2);
   --  Member.Table is a table of the Memeber_Info

   ----------------
   -- Class File --
   ----------------

   type Class_File is record
   --  Record modeling a JVM class file used for both classes and interfaces

      Magic         : U4 := SUN_Magic_Number;
      --  Class file magic number

      Minor_Version : U2 := SUN_Minor_Version;
      Major_Version : U2 := SUN_Major_Version;
      --  Minor/Major class file version as defined by SUN

      Constant_Pool : CP.Table;
      --  The constant pool table

      Access_Flags  : Access_Mask := Default_Class_Mask;
      --  The class access flags

      This_Class    : CP_Index_Class := CP_Empty;
      --  Constant pool entry for the class or interface defined here

      Super_Class   : CP_Index_Class := CP_Empty;
      --  When defining a class this is the constant pool entry for the
      --  superclass. For an interface the constant pool entry must represent
      --  java.lang.Object.

      Interfaces    : Class_Index.Table;
      --  Each element in this table gives CP index for one of the interfaces
      --  implemented by this class.

      Fields        : Member.Table;
      --  Table of the fields declared in the class

      Methods       : Member.Table;
      --  Table of the methods declared in the class

      Attributes    : Class_Attribute.Table;
      --  Attributes that apply to the class
   end record;

   package Classes is new J_Table (Data => Class_File, Index => U2);
   --  Classes.Table is a table of Class_File records. It is defined for
   --  convenience. It is not needed for the JVM specification and storage
   --  allocated in this specific table is not reclaimed by the Set_Water_Mark
   --  and Free_To_Next_Water_Mark protocol.

   ---------------------------------
   -- Reading/Writing Class Files --
   ---------------------------------

   function Read (Stream : Stream_Of_U1; Check : Boolean) return Class_File;
   --  Converts a stream of bytes into a Class_File. When Check is True this
   --  routine checks that Stream does indeed contain a well formed class file.
   --  If this is not the case exception JVM_Walk.Class_File_Check_Failed is
   --  raised.

   procedure Compute_Attribute_Lengths (CF : Class_File);
   --  This procedure sets all the Attribute_Length fields of all the
   --  attributes contained in CF and all its sub-structures.

   function Compute_Size (CF : Class_File) return Nat_32;
   --  Given a class file structure CF, this procedure returns the number of
   --  bytes needed to store CF in a ".class" file. If the Attribute_Length
   --  fields in CF or in the structures contained in CF are not set an
   --  exception is raised.

   procedure Write (CF : in out Class_File; Stream : out Stream_Of_U1);
   --  Converts CF, a Class_File, into Stream, a stream of bytes. Stream's
   --  length must be big enough to hold the stream of bytes that are
   --  generating when converting CF. Use function Compute_Size above to
   --  allocate the right size for Stream.

   --------------------
   -- JVM Operations --
   --------------------

   --  JVM Operations are subdivided into the following categories:
   --
   --  * Data Operations
   --    o Stack
   --      - Pushing Constants onto the Stack
   --      - Stack Manipulation
   --    o Local Variables
   --      - Pushing Local Variables onto the Stack
   --      - Popping Stack Values into Local Variables
   --    o Arrays
   --      - Creating Arrays
   --      - Retrieving Values from Arrays
   --      - Storing Values in Arrays
   --      - Miscellaneous Array Instructions
   --    o Objects
   --      - Creating Objects
   --      - Manipulating Object Fields
   --      - Checking an Object's Type
   --
   --  * Arithmetic, Logical and Type Conversion
   --    o Arithmetic Operations
   --    o Integer Increment
   --    o Shift and Logical Operations
   --    o Scalar Conversions
   --
   --  * Flow Control
   --    o Control Transfer
   --      - Compare operations
   --      - Compare and Branch
   --      - Unconditional Branches and Subroutines
   --      - Table Jumping
   --    o Methods
   --      - Method Invocation
   --      - Method Return
   --
   --  * Miscellaneous
   --    o Exceptions
   --    o Monitor Operations
   --    o Miscellaneous

   type Operation is (

      Nop,
      --  Do nothing

      --------------------------------------
      -- Pushing Constants onto the Stack --
      --------------------------------------

      Aconst_Null,
      --  Push null reference

      Iconst_M1, Iconst_0, Iconst_1, Iconst_2, Iconst_3, Iconst_4, Iconst_5,
      --  Push 4 byte integer constant (respectively -1, 0, 1, 2, 3, 4, 5)

      Lconst_0, Lconst_1,
      --  Push 8 byte integer constant (respectively 0, 1)

      Fconst_0, Fconst_1, Fconst_2,
      --  Push 4 byte floating point integer constant (respectively 0, 1, 2)

      Dconst_0, Dconst_1,
      --  Push 8 byte floating point constant (respectively 0, 1)

      Bipush, Sipush,
      --  Push 1/2 byte integer respectively

      Ldc, Ldc_W,
      --  Push 4 byte Integer/Float or String constant from the constant pool

      Ldc2_W,
      --  Push 8 byte Integer/Float constant from the constant pool

      --------------------------------------------
      -- Pushing Local Variables onto the Stack --
      --------------------------------------------

      Iload, Lload, Fload, Dload, Aload,
      --  Load 4/8 byte integer, 4/8 byte float, object reference respectively

      Iload_0, Iload_1, Iload_2, Iload_3,
      --  Load 4 byte integer from local variables 0, 1, 2, 3 respectively

      Lload_0, Lload_1, Lload_2, Lload_3,
      --  Load 8 byte integer from local variables 0, 1, 2, 3 respectively

      Fload_0, Fload_1, Fload_2, Fload_3,
      --  Load 4 byte float from local variables 0, 1, 2, 3 respectively

      Dload_0, Dload_1, Dload_2, Dload_3,
      --  Load 8 byte float from local variables 0, 1, 2, 3 respectively

      Aload_0, Aload_1, Aload_2, Aload_3,
      --  Load object reference from local variables 0, 1, 2, 3 respectively

      -----------------------------------
      -- Retrieving Values from Arrays --
      -----------------------------------

      Iaload, Laload, Faload, Daload, Aaload, Baload, Caload, Saload,
      --  Load 4/8 byte integer, 4/8 byte float, object reference,
      --  1 byte integer (or boolean), character, 2 byte integer respectively.

      -----------------------------------------------
      -- Popping Stack Values into Local Variables --
      -----------------------------------------------

      Istore, Lstore, Fstore, Dstore, Astore,
      Istore_0, Istore_1, Istore_2, Istore_3,
      Lstore_0, Lstore_1, Lstore_2, Lstore_3,
      Fstore_0, Fstore_1, Fstore_2, Fstore_3,
      Dstore_0, Dstore_1, Dstore_2, Dstore_3,
      Astore_0, Astore_1, Astore_2, Astore_3,
      --  Same meaning as for the corresponding load operations

      ------------------------------
      -- Storing Values in Arrays --
      ------------------------------

      Iastore, Lastore, Fastore, Dastore, Aastore, Bastore, Castore, Sastore,
      --  Same meaning as for the corresponding load operations

      ------------------------
      -- Stack Manipulation --
      ------------------------

      Pop, Pop2,
      --  Pop 4/8 bytes respectively

      Dup,
      --  Duplicate top 4 bytes

      Dup_X1, Dup_X2,
      --  Duplicate top 4 bytes and push them 8/12 bytes down respectively

      Dup2,
      --  Duplicate top 8 bytes

      Dup2_X1, Dup2_X2,
      --  Duplicate top 8 bytes and push them 12/16 bytes down respectively

      Swap,
      --  Swap top two 4 byte values

      ---------------------------
      -- Arithmetic Operations --
      ---------------------------

      Iadd, Ladd, Fadd, Dadd,
      --  4/8 byte integer, 4/8 byte float addition

      Isub, Lsub, Fsub, Dsub,
      --  4/8 byte integer, 4/8 byte float subtraction

      Imul, Lmul, Fmul, Dmul,
      --  4/8 byte integer, 4/8 byte float multiplication

      Idiv, Ldiv, Fdiv, Ddiv,
      --  4/8 byte integer, 4/8 byte float division

      Irem, Lrem, Frem, Drem,
      --  4/8 byte integer, 4/8 byte float remainder

      Ineg, Lneg, Fneg, Dneg,
      --  4/8 byte integer, 4/8 byte float negate

      ----------------------------------
      -- Shift and Logical Operations --
      ----------------------------------

      Ishl, Lshl,
      --  4/8 byte shift left

      Ishr, Lshr,
      --  4/8 byte arithmentic shift right

      Iushr, Lushr,
      --  4/8 byte logical shift right

      Iand, Land,
      --  4/8 byte and

      Ior, Lor,
      --  4/8 byte or

      Ixor, Lxor,
      --  4/8 byte xor

      -----------------------
      -- Integer Increment --
      -----------------------

      Iinc,
      --  Increment local integer variable by constant

      ------------------------
      -- Scalar Conversions --
      ------------------------

      I2l, I2f, I2d,
      --  Convert 4 byte integer into 8 byte integer or 4/8 byte float
      --  respectively.

      L2i, L2f, L2d,
      --  Convert 8 byte integer into 4 byte integer or 4/8 byte float
      --  respectively.

      F2i, F2l, F2d,
      --  Convert 4 byte float into 4/8 byte integer or 8 byte float
      --  respectively.

      D2i, D2l, D2f,
      --  Convert 8 byte float into 4/8 byte integer or 4 byte float
      --  respectively.

      I2b, I2c, I2s,
      --  Convert 4 byte integer into 1 byte ineteger, 2 byte character or
      --  2 byte integer respectively.

      ------------------------
      -- Compare operations --
      ------------------------

      Lcmp, Fcmpl, Fcmpg, Dcmpl, Dcmpg,
      --  Compare 8 byte integer, 4/8 byte float respectively

      ------------------------
      -- Compare and Branch --
      ------------------------

      Ifeq, Ifne, Iflt, Ifge, Ifgt, Ifle,
      --  Compare 4 byte integer with 0

      If_Icmpeq, If_Icmpne, If_Icmplt, If_Icmpge, If_Icmpgt, If_Icmple,
      --  Compare two 4 byte integers together

      If_Acmpeq, If_Acmpne,
      --  Compare two references

      ------------------------------------------------------------
      -- Unconditional Branches and Subroutines (2 byte offset) --
      ------------------------------------------------------------

      Jump,
      --  Jump unconditionally

      Jsr,
      --  Jump to subroutine

      Ret,
      --  Return from subroutine

      -------------------
      -- Table Jumping --
      -------------------

      Tableswitch, Lookupswitch,
      --  Case statement lookup

      -------------------
      -- Method Return --
      -------------------

      Ireturn, Lreturn, Freturn, Dreturn, Areturn, Vreturn,
      --  Return from method a 4/8 byte integer, 4/8 byte float,
      --  object reference or nothing respectively.

      --------------------------------
      -- Manipulating Object Fields --
      --------------------------------

      Getstatic, Putstatic,
      --  Get/Put a value from static field

      Getfield, Putfield,
      --  Get/Put a value from regular field

      -----------------------
      -- Method Invocation --
      -----------------------

      Invokevirtual,
      --  Dispatching call

      Invokespecial,
      --  Non dispatching & constructor call

      Invokestatic,
      --  Call a static method

      Invokeinterface,
      --  Call a method belonging to an interface

      -------------------
      -- Miscellaneous --
      -------------------

      Xxxunusedxxx,
      --  Unused opcode

      ----------------------
      -- Creating Objects --
      ----------------------

      Newobject,
      --  New object

      -----------------------------------
      -- Creating 1-Dimensional Arrays --
      -----------------------------------

      Newarray, Anewarray,
      --  New array of scalars/references respectively

      --------------------------------------
      -- Miscellaneous Array Instructions --
      --------------------------------------

      Arraylength,
      --  Retrieve the length of the array

      ----------------
      -- Exceptions --
      ----------------

      Athrow,
      --  Raise an exception

      -------------------------------
      -- Checking an Object's Type --
      -------------------------------

      Checkcast,
      --  Checks that a given object can be casted to a given class. If this is
      --  not possible an exception is raised.

      Instanceof,
      --  Checks that the object is an instance of a given class. This
      --  instruction does not raise an exception.

      ------------------------
      -- Monitor Operations --
      ------------------------

      Monitorenter, Monitorexit,
      --  Entere/Exit the monitor attached to an object

      -------------------
      -- Miscellaneous --
      -------------------

      Wide,
      --  Signals that the instruction that follows is using a wide format

      ---------------------------------------
      -- Creating Multi-Dimansional Arrays --
      ---------------------------------------

      Multianewarray,
      --  New multidemnsional array of references

      ------------------------------------------
      -- Comparing Against the null Reference --
      ------------------------------------------

      Ifnull, Ifnonnull,
      --  Compare with null reference

      ------------------------------------------------------------
      -- Unconditional Branches and Subroutines (4 byte offset) --
      ------------------------------------------------------------

      Goto_W,
      --  Jump unconditionally

      Jsr_W
      --  Jump to subroutine
   );

   for Operation'Size use 8;

   function Operation_Mnemonic (Op : Operation) return String;
   --  Returns the mnemonic of operation Op as specified in the JVM book

   function Has_Wide_Format (Op : Operation) return Boolean;
   --  Returns True for operations that can have a wide format instruction

   subtype Var_Size_Operation is Operation range Tableswitch .. Lookupswitch;
   --  The operations having variable size instructions

   function Bytecode (Op : Operation) return U1;
   --  Converts an Operation into a U1

   function To_Operation (U : U1) return Operation;
   --  Converts a U1 into an Operation

   Wide_Modifier : constant U1;
   --  Signals that the instruction that follows is using a wide format. See
   --  below for the definition of an instruction. Operations that have a wide
   --  instruction format are [ILFDA]load/store, Ret and Iinc.  Note that the
   --  Wide_Modifier is considered part of an instruction with a wide format.

   ----------------------
   -- JVM Instructions --
   ----------------------

   --  Before introducing the main type for JVM instructions (type Instruction
   --  below) we need to introduce some data types needed there.

   --  16/32-bit branch offset into a code array for branching operations

   type Jump_Offset_U2 is new Int_16;
   type Jump_Offset_U4 is new Int_32;

   --  Match-offset pairs needed by operation lookupswitch

   type Lookup_Info is record
      Match  : Int_32         := 0;
      Offset : Jump_Offset_U4 := 0;
   end record;

   type Lookup_Table_Type is array (U4 range <>) of Lookup_Info;
   --  Used by the Lookupswitch operation to construct its jump table. Entries
   --  in a Lookup_Table_Type object must be sorted by increasing Match field.

   --  For the tableswitch operation

   type Jump_Table_Type is array (U4 range <>) of Jump_Offset_U4;
   --  Table of 32 bit signed branch offsets into a code array

   --  For the newarray operation define the element type for scalar arrays

   type Array_Type is (
      T_Empty,
      --  This is a new empty array type. It is used to mark an Array_Type
      --  variable as uninitialized.

      T_Boolean,
      T_Char,
      T_Float,
      T_Double,
      T_Byte,
      T_Short,
      T_Int,
      T_Long
   );

   for Array_Type'Size use 8;
   for Array_Type use (
      T_Empty   =>  0,
      T_Boolean =>  4,
      T_Char    =>  5,
      T_Float   =>  6,
      T_Double  =>  7,
      T_Byte    =>  8,
      T_Short   =>  9,
      T_Int     => 10,
      T_Long    => 11
   );

   ------------------------
   -- Instruction Record --
   ------------------------

   type Instruction (Op : Operation; Table_Length : U4) is record
   --  A JVM instruction is a JVM operation along with its operands, if any.
   --  This type gives the format of each JVM instruction.
   --  Op is the actual operation and Table_Length gives the length of the
   --  jump table used by the Lookupswitch and Tableswitch operations.

      case Op is

         ------------
         -- Bipush --
         ------------

         when Bipush =>
            Byte : Int_8 := 0;
            --  Sign extended to a 32 bit int and pushed on the operand stack

         ------------
         -- Sipush --
         ------------

         when Sipush =>
            Short : Int_16 := 0;
            --  Same as Byte above

         ----------
         -- Iinc --
         ----------

         when Iinc =>
            Wide_Iinc : Boolean := False;
            --  When set, the wide format for this instruction is used. If not
            --  set but the size of either of the two fields below requires 2
            --  bytes the wide format is also used.

            Var_Iinc : Local_Variable_Index := 0;
            --  Index of the local variable to be incremented

            Const_Val : Int_16 := 0;
            --  Sign extended to a 32-bit int and added to the local variable

         ----------------------------
         -- [ILFDA]load/store, Ret --
         ----------------------------

         when Ret
           |  Iload  | Lload  | Fload  | Dload  | Aload
           |  Istore | Lstore | Fstore | Dstore | Astore =>
            Wide  : Boolean := False;
            --  Same meaning as field Wide_Iinc above

            Local_Var : Local_Variable_Index := 0;
            --  Index of the local variable that contains the return address
            --  for the ret operation or to load from or store to for the
            --  load/store operations.

         ---------------------
         -- Invokeinterface --
         ---------------------

         when Invokeinterface =>
            I_Method : CP_Index_Interface_Method := CP_Empty;
            --  CP entry for the interface method to call

            Nargs    : U1 := 0;
            --  Number of arguments on the operand stack to pass to the method

            Unused   : U1 := 0;
            --  Always zero

         --------------
         -- Newarray --
         --------------

         when Newarray =>
            Atype : Array_Type := T_Empty;
            --  The type of scalar array to create

         --------------------
         -- Multianewarray --
         --------------------

         when Multianewarray =>
            Component  : CP_Index_Class := CP_Empty;
            --  CP Index of the component type for the array

            Dimensions : U1 := 0;
            --  Number of dimensions of the multidimensional array to create

         ------------------
         -- Lookupswitch --
         ------------------

         when Lookupswitch =>
            Lookup_Default : Jump_Offset_U4 := 0;
            --  32-bit signed branch offset into the code array to be taken
            --  when no matching entry can be found in the Lookup_Table below.

            Npairs         : U4 := 0;
            --  Number of entries in the Match_Offset table below

            Lookup_Table   : Lookup_Table_Type (1 .. Table_Length);
            --  Table of match-offset entries

         -----------------
         -- Tableswitch --
         -----------------

         when Tableswitch =>
            Table_Default : Jump_Offset_U4 := 0;
            --  32-bit signed branch offset into the code array to be taken
            --  when no matching entry can be found in the Jump_Table below.

            Low  : Int_32 := 0;
            High : Int_32 := 0;
            --  The bounds of the integer values for which Jump_Table below has
            --  an entry (i.e., a branch offset for the corresponding integer).

            Jump_Table : Jump_Table_Type (1 .. Table_Length);
            --  The jump table.

         ------------------------
         -- Ldc, Ldc_W, Ldc2_W --
         ------------------------

         when Ldc =>
            CP_Const_U1 : CP_Index_Constant_U4 := CP_Empty;
            --  This CP index must be between 1 and 255

         when Ldc_W =>
            CP_Const_U2 : CP_Index_Constant_U4 := CP_Empty;
            --  Arbitrary CP index between 1 and 2**16-1

         when Ldc2_W =>
            CP_Const  : CP_Index_Constant_U8 := CP_Empty;
            --  Arbitrary CP index between 1 and 2**16-1

         ----------------------------------------
         -- If*, If_Icmp*, If_Acmp*, Jump, Jsr --
         ----------------------------------------

         when Jump
           |  Jsr
           |  Ifnull    | Ifnonnull
           |  If_Acmpeq | If_Acmpne
           |  Ifeq      | Ifne
           |  Ifle      | Iflt      | Ifge      | Ifgt
           |  If_Icmpeq | If_Icmpne
           |  If_Icmple | If_Icmplt | If_Icmpge | If_Icmpgt =>
            Offset : Jump_Offset_U2 := 0;
            --  Branch offset

         -------------------
         -- Jsr_W, Goto_W --
         -------------------

         when Jsr_W
           |  Goto_W =>
            Offset_U4 : Jump_Offset_U4 := 0;
            --  Branch offset

         --------------------------------
         -- Get/Putstatic Get/Putfield --
         --------------------------------

         when Getstatic | Putstatic
           |  Getfield  | Putfield =>
            Field : CP_Index_Field := CP_Empty;
            --  Field to access

         ------------------------------------------------
         -- Invokevirtual, Invokespecial, Invokestatic --
         ------------------------------------------------

         when Invokestatic
           |  Invokespecial
           |  Invokevirtual =>
            Method : CP_Index_Method := CP_Empty;
            --  Method to invoke

         ------------------------------------------------
         -- Newobject, Anewarray, Checkcast, Instanceof --
         ------------------------------------------------

         when Newobject
           |  Anewarray
           |  Checkcast
           |  Instanceof =>
            Class : CP_Index_Class := CP_Empty;
            --  Class of the object to create (newobject), the component type
            --  of the array to create (Anewarray) or to check against
            --  (checkcast, Instanceof).

         --------------------------
         -- All Other Operations --
         --------------------------

         when others =>
            null;
      end case;

   end record;

   function Needs_Wide_Format (I : Instruction) return Boolean;
   --  Returns True if instruction I needs a wide format. This is the case if
   --  I's operation has a wide format and then any of its operands does indeed
   --  require a wide format to be stored as byte code. Return True also if
   --  I's wide format modifier is set.

   procedure Set_Wide_Format_If_Needed (I : in out Instruction);
   --  If Needs_Wide_Format (I) is True, then this procedure sets the wide
   --  modifier field of I to True.

   procedure Set_Wide_Format (I : in out Instruction);
   --  Set the wide modifier field of I to True. I must have a wide format.
   --  If this is not the case an exception is raised.

   subtype CA_Table is Code_Array.Table;
   subtype I_Index  is Instruction_Index;

   --  So far the only way to access the instructions stored compactly in a
   --  code array is to access them byte by byte by using the Get/Put/Add
   --  routines provided for a Code_Array.Table. The following routines have
   --  therefore been provided to conveniently access JVM Instructions in a
   --  code array.

   function Size (Op : Operation; Wide : Boolean := False) return U4;
   --  Returns the number of bytes occupied by instructions with operation Op.
   --  If Wide is True, then return the size of the corresponding wide
   --  instruction. If Op has no wide instruction format then raise an
   --  exception. Note that the size returned for wide format instructions
   --  always includes the byte for the wide modifier. If Op is Lookupswitch or
   --  Tableswitch the value zero is returned to indicate that the following
   --  version of function Size should be used.

   function Size (Op : Var_Size_Operation; Off : I_Index; N : U4) return U4;
   --  Returns the number of bytes occupied by an instruction whose operation
   --  Op is either a Lookupswitch or a Tableswitch. Op is stored at offset Off
   --  in a code array and Op has N match-offset pairs (Lookupswitch) or N jump
   --  offsets (Tableswitch).

   function Size (T : CA_Table; K : I_Index) return U4;
   --  Returns the number of bytes occupied by the instruction stored at
   --  Instruction_Index K in the code array T. Note that if the instruction
   --  has a wide format (ie K is the index of a Wide operation) the size
   --  returned includes the byte for the Wide modifier. It is an error to
   --  invoke this function if K is not the starting point of a valid
   --  instruction. In particular, this is so if K is the index of the
   --  operation immediately following a Wide modifier.

   function Next_Instruction (T : CA_Table; K : I_Index) return I_Index;
   --  Semantically equivalent to K + Size (T, K)

   function Get (T : CA_Table; K : Instruction_Index) return Instruction;
   --  Gets from the code array table T the instruction stored at index K.  It
   --  is an error to invoke this function if K is not the starting point of a
   --  valid instruction.

   procedure Add (T : in out CA_Table; I : in out Instruction);
   --  Adds instruction I to code array table T. If the wide instruction format
   --  for I needs to be used, then the wide modifier field of I is set (field
   --  Wide_Iinc or field Wide). This is why I is passed as an in out
   --  parameter.

private

   Wide_Modifier : constant U1 := Operation'Pos (Wide);

   pragma Inline (Add);
   pragma Inline (Append);
   pragma Inline (Append_String);
   pragma Inline (Bytecode);
   pragma Inline (Has_Wide_Format);
   pragma Inline (Is_Class);
   pragma Inline (Is_Constant);
   pragma Inline (Is_Constant_U4);
   pragma Inline (Is_Constant_U8);
   pragma Inline (Is_Double);
   pragma Inline (Is_Fieldref);
   pragma Inline (Is_Float);
   pragma Inline (Is_Integer);
   pragma Inline (Is_Interface_Methodref);
   pragma Inline (Is_Long);
   pragma Inline (Is_Methodref);
   pragma Inline (Is_Name_And_Type);
   pragma Inline (Is_Set);
   pragma Inline (Is_String);
   pragma Inline (Is_Utf8);
   pragma Inline (Needs_Wide_Format);
   pragma Inline (Next_Instruction);
   pragma Inline (Set);
   pragma Inline (Set_Wide_Format);
   pragma Inline (Set_Wide_Format_If_Needed);
   pragma Inline (Size);
   pragma Inline (To_Operation);

end JVM_File;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.16
--  date: Fri Apr 17 11:42:51 1998;  author: gasperon
--  Comment update.
--  (Needs_Wide_Format) change its semantics slighttly & update comment.
--  (Set_Wide_Format_If_Needed) update comment.
--  (Compute_Attribute_Lengths) comment update.
--  Remove all Length fields from all JVM data structures since these were
--   redundant.
--  (Append_Name) renamed in Append_String.
--  (Read) add a parameter to check class file consistency.
--  CONSTANT_Empty: document its new use
--  ----------------------------
--  revision 1.17
--  date: Thu Apr 23 16:34:12 1998;  author: gasperon
--  Add Attr_Deprecated as a new member attribute.
--  type Stream_Of_U1 moved to j_types to avoid circularities.
--  ----------------------------
--  revision 1.18
--  date: Mon May  4 11:18:55 1998;  author: gasperon
--  (Operation_Mnemonic) new routine.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
