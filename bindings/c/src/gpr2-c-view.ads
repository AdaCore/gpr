--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
