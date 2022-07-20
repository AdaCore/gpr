------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Ada.Strings.Unbounded;

with GPR2.Path_Name;

with GPRname.Common;

package GPRname.Source_Dir is

   use GPR2;

   use GPRname.Common;

   type Object is tagged private;
   --  A source directory as seen by GPR, which may be suffixed by "**" and
   --  thus searched recursively.

   type Dir_And_Optional_File (Has_File : Boolean := False) is
      record
         Dir  : Object;
         case Has_File is
            when True =>
               File : Path_Name.Object;
            when False =>
               null;
         end case;
      end record;

   function Create
     (Name      : Filename_Type;
      Directory : Filename_Optional := No_Filename)
      return Dir_And_Optional_File;
   --  Creates a source directory, setting the Is_Recursive field according to
   --  whether Name has the "**" suffix or not.

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Equality between source dirs, using the Dir field

   function Value (Self : Object) return Path_Name.Object;
   --  Returns the Path_Name holding the full path information for Self

   function Is_Recursive (Self : Object) return Boolean;
   --  Returns the boolean that indicates if Self should be searched
   --  recursively.

   function Orig (Self : Object) return String;
   --  Returns the original string used to create Self, including the "**"
   --  suffix if it was present.

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Dir          : Path_Name.Object;
      Is_Recursive : Boolean;
      Orig         : Unbounded_String;
   end record;

   overriding function "=" (Left, Right : Object) return Boolean is
     (Path_Name."=" (Left.Dir, Right.Dir));

   function Value (Self : Object) return Path_Name.Object is
     (Self.Dir);

   function Is_Recursive (Self : Object) return Boolean is
     (Self.Is_Recursive);

   function Orig (Self : Object) return String is
     (To_String (Self.Orig));

end GPRname.Source_Dir;
