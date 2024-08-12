--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Refcount;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;

private package GPR2.View_Base_Internal is

   type Definition_Base is abstract tagged record
      Id                : Natural := 0;
      Path              : Path_Name.Object;
      Externals         : Containers.External_Name_Set;
      --  List of externals directly or indirectly visible
      Signature         : Context.Binary_Signature :=
                            Context.Default_Signature;
      Is_Root           : Boolean := False;
      --  A view will have Is_Root set to True only of root of tree. In
      --  practice three views are considered as root projects: the view
      --  associated with the loaded main project file, the config view and
      --  the runtime view.
      Kind              : Project_Kind;
   end record;
   --  Base definition of the View data, needed as the actual view data
   --  needs to hold references to view data using shared pointers, and
   --  instanciation requires an actual type.

   package Definition_References is new GNATCOLL.Refcount.Shared_Pointers
     (Definition_Base'Class);

   subtype Weak_Reference is Definition_References.Weak_Ref;

end GPR2.View_Base_Internal;
