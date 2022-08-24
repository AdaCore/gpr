--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This packages provides an interface to abstract away the action of reading
--  a source file to parse. Depending on use cases, it allows to override
--  bytes-to-text decoding and preprocess sources (before actual
--  lexing/parsing).

with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount;

with Gpr_Parser_Support.File_Readers;

with GPR2.Log;

package GPR2.File_Readers is

   subtype Text_Type is Wide_Wide_String;
   type Text_Access is access all Text_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Text_Type, Text_Access);

   type Decoded_File_Contents is record
      Buffer : Text_Access;
      First  : Positive;
      Last   : Natural;
   end record;
   --  The "Buffer (First .. Last)" slice contains the decoded file contents as
   --  a sequence of codepoints. We keep track of First/Last indexes in
   --  addition to Ada's Buffer'First/'Last attributes because source buffers
   --  may be oversized.

   type File_Reader_Interface is interface;
   --  Interface to override how source files are fetched and decoded

   procedure Read
     (Self        : File_Reader_Interface;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out GPR2.Log.Object) is abstract;
   --  Read the content of the source at Filename, decoding it using the given
   --  Charset and decoding the byte order mark if Read_BOM is True.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.
   --
   --  Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
   --  to refer to it.

   procedure Release (Self : in out File_Reader_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Self

   procedure Do_Release (Self : in out File_Reader_Interface'Class);
   --  Helper for the instantiation below

   package File_Reader_References is new GNATCOLL.Refcount.Shared_Pointers
     (File_Reader_Interface'Class, Do_Release);

   subtype File_Reader_Reference is File_Reader_References.Ref;
   No_File_Reader_Reference : File_Reader_Reference renames
     File_Reader_References.Null_Ref;

   function Create_File_Reader_Reference
     (File_Reader : File_Reader_Interface'Class) return File_Reader_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create file reader
   --  references.

   function Convert
     (File_Reader : File_Reader_Reference)
      return Gpr_Parser_Support.File_Readers.File_Reader_Reference;
   --  Adapter used to convert File_Reader to the one used by Gpr_Parser

end GPR2.File_Readers;
