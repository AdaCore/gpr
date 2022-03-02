------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with GPR2.C.JSON;

package GPR2.C.View is

   procedure Load
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);

   procedure Attribute
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);
   --  Request:
   --      {'tree_id':  str,
   --       'view_id':  str,
   --       'name':     str,
   --       'pkg':      Optional[str],
   --       'filename': Optional[str],
   --       'position': Optional[int],
   --       'language': Optional[str],
   --       'index':    Optional[str]}
   --
   --  tree_id is the GPR tree id, view_id the GPR view id, name the attribute
   --  name and pkg the optional package name.
   --
   --  filename, position, language and index are used to query an attribute
   --  for a specific index. If the index is a filename use filename and
   --  optionally position (unit index for an Ada source filename). If the
   --  index is a language use 'language'. In all other cases use 'index'.
   --
   --  Answer:
   --      {"attribute": PROJECT_ATTRIBUTE}

   procedure Sources
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);

   procedure Units
      (Request : GPR2.C.JSON.JSON_Value; Result : GPR2.C.JSON.JSON_Value);

end GPR2.C.View;
