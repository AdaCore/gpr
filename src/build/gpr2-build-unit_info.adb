--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Unit_Info is

   ------------
   -- Create --
   ------------

   function Create
     (Unit_Name     : Optional_Name_Type;
      Index         : Unit_Index;
      Kind          : Unit_Kind;
      Separate_Name : Optional_Name_Type := No_Name;
      Dependencies  : GPR2.Containers.Name_Set :=
                        GPR2.Containers.Name_Type_Set.Empty_Set;
      Parsed        : Boolean := False) return Object
   is
      Folded_Name : constant Optional_Name_Type :=
        Optional_Name_Type (To_Upper (Unit_Name));
      Folded_Separate : constant Optional_Name_Type :=
        Optional_Name_Type (To_Upper (Separate_Name));
   begin
      return (Name_Len      => Folded_Name'Length,
              Separate_Len  => Folded_Separate'Length,
              Kind          => Kind,
              Index         => Index,
              Is_Parsed     => Parsed,
              Name          => Folded_Name,
              Separate_Name => Folded_Separate,
              Dependencies  => Dependencies);
   end Create;

   ----------------------
   -- Set_Parsed_State --
   ----------------------

   procedure Set_Parsed_State (Self : in out Object; State : Boolean) is
   begin
      Self.Is_Parsed := State;
   end Set_Parsed_State;

end GPR2.Build.Unit_Info;
