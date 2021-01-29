------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This abstract object represents a source parser. A source parser can be
--  given for a specific language and kind (either parsed from source
--  or some artifact files created during the compilation like ALI for GNAT).
--
--  A soucre parser is created as a child package. It is then
--  registered into the source info parser registry child package.

limited with GPR2.Project.Source;

package GPR2.Source_Info.Parser is

   type Object
     (Language : not null access Name_Type;
      Kind     : Backend) is abstract tagged limited private;

   type Object_Ref is not null access all Object'Class;

   procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : Project.Source.Object) is abstract
   with Pre'Class  => Self.Kind /= None,
        Post'Class => not Data.Is_Parsed
                        or else
                      Data.Used_Backend in Source_Info.LI | Source_Info.Source;
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
     (Language : not null access Name_Type;
      Kind     : Backend) is abstract tagged limited
   record
      Self : not null access Object'Class := Object'Unchecked_Access;
   end record;

end GPR2.Source_Info.Parser;
