--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Project.Tree;
with GPR2.View_Ids;

pragma Warnings (Off, ".* is not referenced");
--  We need it as call to Src.Units has only a limited view over the set
with GPR2.Project.View.Set;
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");

package body GPR2.Build.Compilation_Input.Sets is

   function Create (View : Project.View.Object) return Object
   is
      use type GPR2.View_Ids.View_Id;
      Self : Object;
   begin
      for NS of View.Namespace_Roots loop
         for CU of View.Tree.Artifacts_Database (View).Compilation_Units loop
            if CU.Main_Part.View = View.Id then
               Self.Include ((View,
                              Unit_Kind,
                              +String (CU.Main_Part.Path.Simple_Name),
                              CU.Main_Part.Index));
            end if;
         end loop;
      end loop;

      for Src of View.Sources loop
         if not Src.Has_Units and then Src.Kind = S_Body then
            Self.Insert
              (Compilation_Input.Object'
                 (Src_Owner       => View,
                  Kind            => Body_Kind,
                  Source_Basename => +String (Src.Path_Name.Simple_Name),
                  Index           => No_Index));
         end if;
      end loop;

      return Self;
   end Create;

end GPR2.Build.Compilation_Input.Sets;
