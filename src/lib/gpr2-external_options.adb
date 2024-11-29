--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.External_Options is

   -----------
   -- Fetch --
   -----------

   function Fetch (Self   : Object;
                   Action : Action_Class;
                   Lang   : Language_Id) return GPR2.Containers.Value_List
   is
      Result : GPR2.Containers.Value_List := GPR2.Containers.Empty_Value_List;
   begin
      if Self.Ext_Opt.Contains (Action)
        and then Self.Ext_Opt (Action).Contains (Lang)
      then
         Result := Self.Ext_Opt (Action) (Lang);
      end if;

      return Result;
   end Fetch;

   --------------
   -- Register --
   --------------

   procedure Register (Self   : in out Object;
                       Action : Action_Class;
                       Lang   : Language_Id;
                       Option : String)
   is
      use GPR2.Containers;
   begin
      if not Self.Ext_Opt.Contains (Action) then
         Self.Ext_Opt.Insert (Action, Empty_Lang_Value_List_Map);
      end if;

      if not Self.Ext_Opt (Action).Contains (Lang) then
         Self.Ext_Opt (Action).Insert
           (Lang, Value_Type_List.Empty_Vector);
      end if;

      Self.Ext_Opt (Action) (Lang).Append (Option);
   end Register;

end GPR2.External_Options;
