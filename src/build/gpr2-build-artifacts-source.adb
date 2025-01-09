--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GPR2.Build.Source;

package body GPR2.Build.Artifacts.Source is

   function Create
     (View  : GPR2.Project.View.Object;
      Name  : Simple_Name;
      Index : Unit_Index := No_Index) return Object
   is
      Src : constant Build.Source.Object := View.Visible_Source (Name);
   begin
      if not Src.Is_Defined then
         return Undefined;
      end if;

      return (Name  => +Name,
              View  => Src.Owning_View,
              Path  => Src.Path_Name,
              Index => Index);
   end Create;

   -----------------
   -- Unserialize --
   -----------------

   overriding procedure Unserialize
     (Ctxt : GPR2.Project.View.Object;
      S    : String;
      Val  : out Object)
   is
      Idx   : Natural := Ada.Strings.Fixed.Index (S, "" & Sep);
      Index : Unit_Index;

   begin
      if Idx in S'Range then
         Index := Unit_Index'Value (S (Idx + 1 .. S'Last));
      else
         Index := No_Index;
         Idx := S'Last + 1;
      end if;

      Val := Create
        (Ctxt,
         Simple_Name (S (S'First .. Idx - 1)),
         Index);
   end Unserialize;

end GPR2.Build.Artifacts.Source;
