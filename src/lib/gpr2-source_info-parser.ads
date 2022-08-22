--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This abstract object represents a source parser. A source parser can be
--  given for a specific language and kind (either parsed from source
--  or some artifact files created during the compilation like ALI for GNAT).
--
--  A soucre parser is created as a child package. It is then
--  registered into the source info parser registry child package.

limited with GPR2.Project.Source;

private with Ada.Calendar;

package GPR2.Source_Info.Parser is

   type Object
     (Language : Language_Id;
      Kind     : Backend) is abstract tagged limited private;

   type Object_Ref is not null access all Object'Class;

   procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object) is abstract;
   --  Set Data with the information for the given source. If LI is undefined
   --  or not present then the source is parsed (using either the LI based
   --  or source based parser). If LI is defined and present then the LI file
   --  is parsed to get the corresponding information. Note that if Data is
   --  already defined (Backend different of None) the routine will update
   --  the information if needed.
   --  The Source parameter should contain the Data inside and used to get
   --  source and unit information provided by project.

   procedure Clear_Cache (Self : not null access Object) is null;
   --  Default version is null, this must be overriden by parsers in need of
   --  cleaning up cache information.

private

   type Object
     (Language : Language_Id;
      Kind     : Backend) is abstract tagged limited
   record
      Self : not null access Object'Class := Object'Unchecked_Access;
   end record;

   subtype Time_String is String (1 .. 14);

   function To_Time (S : Time_String) return Ada.Calendar.Time with Inline;
   --  Convert timestamp in YYYYMMDDHHMMSS format to Time

end GPR2.Source_Info.Parser;
