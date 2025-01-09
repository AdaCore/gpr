--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.External_Options is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.Ext_Opt.Clear;
   end Clear;

   -----------
   -- Fetch --
   -----------

   function Fetch (Self   : Object;
                   Action : Action_Class;
                   Lang   : Language_Id) return GPR2.Containers.Value_List
   is
      Result : GPR2.Containers.Value_List;
   begin
      if Self.Ext_Opt.Contains (Action) then
         if Lang /= No_Language
           and then Self.Ext_Opt (Action).Contains (No_Language)
         then
            Result.Append_Vector (Self.Ext_Opt (Action) (No_Language));
         end if;

         if Self.Ext_Opt (Action).Contains (Lang) then
            Result.Append_Vector (Self.Ext_Opt (Action) (Lang));
         end if;
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

end GPR2.Build.External_Options;
