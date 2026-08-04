--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.External_Options is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object;
                    Action : Action_Class := All_Actions) is
   begin
      for Orig in Origin loop
         if Action = All_Actions then
            Self.Ext_Opt (Orig).Clear;
         elsif Self.Ext_Opt (Orig).Contains (Action) then
            Self.Ext_Opt (Orig) (Action).Clear;
         end if;
      end loop;
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
      --  Append the options coming from the project files first, then those
      --  coming from the command line, so that the latter are emitted last and
      --  override the former when switches conflict.

      for Orig in Origin loop
         if Self.Ext_Opt (Orig).Contains (Action) then
            if Lang /= No_Language
              and then Self.Ext_Opt (Orig) (Action).Contains (No_Language)
            then
               Result.Append_Vector
                 (Self.Ext_Opt (Orig) (Action) (No_Language));
            end if;

            if Self.Ext_Opt (Orig) (Action).Contains (Lang) then
               Result.Append_Vector (Self.Ext_Opt (Orig) (Action) (Lang));
            end if;
         end if;
      end loop;

      return Result;
   end Fetch;

   --------------
   -- Register --
   --------------

   procedure Register (Self   : in out Object;
                       Action : Action_Class;
                       Lang   : Language_Id;
                       Option : String;
                       From   : Origin := Command_Line)
   is
      use GPR2.Containers;
   begin
      if not Self.Ext_Opt (From).Contains (Action) then
         Self.Ext_Opt (From).Insert (Action, Empty_Lang_Value_List_Map);
      end if;

      if not Self.Ext_Opt (From) (Action).Contains (Lang) then
         Self.Ext_Opt (From) (Action).Insert
           (Lang, Value_Type_List.Empty_Vector);
      end if;

      Self.Ext_Opt (From) (Action) (Lang).Append (Option);
   end Register;

end GPR2.Build.External_Options;
