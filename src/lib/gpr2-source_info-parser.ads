------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This abstract object represents a source parser. A source parser can be
--  given for a specific language and kind (either parsed from source
--  or some artifact files created during the compilation like ALI for GNAT).
--
--  A soucre parser is created as a child package. It is then
--  registered into the source info parser registry child package.

limited with GPR2.Source;
limited with GPR2.Project.Tree;

package GPR2.Source_Info.Parser is

   type Object
     (Language : not null access Name_Type;
      Kind     : Backend) is abstract tagged null record;

   procedure Compute
     (Parser : Object;
      Data   : in out Source_Info.Object;
      Source : GPR2.Source.Object;
      LI     : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
      Tree   : access GPR2.Project.Tree.Object := null) is abstract
   with Pre'Class  => Parser.Kind /= None
                        and then
                      (LI.Is_Defined or else Parser.Kind /= Source_Info.LI)
                        and then
                      (LI.Is_Defined or else Tree = null),
        Post'Class => Data.Used_Backend in Source_Info.LI | Source_Info.Source;
   --  Set Data with the information for the given source. If LI is undefined
   --  or not present then the source is parsed (using either the LI based
   --  or source based parser). If LI is defined and present then the LI file
   --  is parsed to get the corresponding information. Note that if Data is
   --  already defined (Backend different of None) the routine will update
   --  the information if needed.

end GPR2.Source_Info.Parser;
