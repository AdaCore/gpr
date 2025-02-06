--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

package body GPR2.Build.Artifacts.Source is

   ------------
   -- Create --
   ------------

   function Create
     (View  : GPR2.Project.View.Object;
      Name  : Simple_Name;
      Index : Unit_Index := No_Index) return Object
   is
      Ambiguous : Boolean;
      Src       : constant Build.Source.Object :=
                    View.Visible_Source (Name, Ambiguous);
   begin
      if not Src.Is_Defined or else Ambiguous then
         return Undefined;
      end if;

      return (Name  => +Name,
              View  => Src.Owning_View,
              Path  => Src.Path_Name,
              Index => Index);
   end Create;

   function Create
     (Src   : GPR2.Build.Source.Object;
      Index : Unit_Index := No_Index) return Object is
   begin
      return (Name  => +Src.Path_Name.Simple_Name,
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
      Idx       : Natural := Ada.Strings.Fixed.Index (S, "" & Sep);
      Index     : Unit_Index;
      Ambiguous : Boolean;

   begin
      if Idx in S'Range then
         Index := Unit_Index'Value (S (Idx + 1 .. S'Last));
      else
         Index := No_Index;
         Idx := S'Last + 1;
      end if;

      Val := Create
        (Ctxt.Visible_Source
           (Simple_Name (S (S'First .. Idx - 1)), Ambiguous),
         Index);
   end Unserialize;

end GPR2.Build.Artifacts.Source;
