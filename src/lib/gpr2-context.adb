--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Context is

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Self      : Object;
      Externals : Containers.Name_List) return Context.Binary_Signature
   is
      Position : Context.Key_Value.Cursor;
      P_Ctx    : Context.Object;
   begin
      --  Compute the project's own context. That is, the context based only on
      --  the project's external variables.

      for E of Externals loop
         Position := Self.Find (E);

         if Context.Key_Value.Has_Element (Position) then
            P_Ctx.Include
              (Context.Key_Value.Key (Position),
               Context.Key_Value.Element (Position));
         end if;
      end loop;

      if P_Ctx.Is_Empty then
         return Context.Default_Signature;

      else
         declare
            C : MD5.Context;
         begin
            for E in P_Ctx.Iterate loop
               MD5.Update (C, String (Key_Value.Key (E)));
               MD5.Update (C, "=");
               MD5.Update (C, Key_Value.Element (E));
               MD5.Update (C, ";");
            end loop;

            return MD5.Digest (C);
         end;
      end if;
   end Signature;

end GPR2.Context;
