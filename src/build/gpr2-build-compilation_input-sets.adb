--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, ".* is not referenced");
--  We need it as call to Src.Units has only a limited view over the set
with GPR2.Build.Source_Info.Sets;
pragma Warnings (On, ".* is not referenced");

package body GPR2.Build.Compilation_Input.Sets is

   function Create (View : View_Db.Object) return Object
   is
      Self : Object;
   begin
      for Src of View.Sources loop
         if Src.Has_Units then
            for U of Src.Units loop
               if Src.Is_Compilation_Input (U.Index) then
                  Self.Insert
                    (Compilation_Input.Object'
                       (Src_Owner       => View,
                        Source_Basename => +String (Src.Path_Name.Simple_Name),
                        Index           => U.Index));
               end if;
            end loop;

         elsif Src.Is_Compilation_Input then
            Self.Insert
              (Compilation_Input.Object'
                 (Src_Owner       => View,
                  Source_Basename => +String (Src.Path_Name.Simple_Name),
                  Index           => No_Index));
         end if;
      end loop;

      return Self;
   end Create;

end GPR2.Build.Compilation_Input.Sets;
