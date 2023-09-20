--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Tree;
with GPR2.Build.Object_Info.Parser.Registry;

package body GPR2.Build.Object_Info is

   ---------------------------------
   -- Context_Clause_Dependencies --
   ---------------------------------

   procedure Context_Clause_Dependencies
     (Self   : Object;
      Part   : Main_CU_Part := CU_Body;
      Action : access procedure (Ref : Source_Reference.Identifier.Object))
   is
   begin
      --  ??? Ensure either ALI or source is parsed
      for D of Self.Includes (Part) loop
         Action (D);
      end loop;
   end Context_Clause_Dependencies;

   ------------
   -- Create --
   ------------

   function Create
     (View             : Project.View.Object;
      Path             : Path_Name.Object;
      Timestamp        : Ada.Calendar.Time;
      Language         : Language_Id;
      Is_Overloaded    : Boolean := False)
      return Object
   is
      LI_Suffix : constant Filename_Type :=
                    View.Tree.Dependency_Suffix (Language);
      LI_Path   : Path_Name.Object;
   begin
      if LI_Suffix /= "" then
         LI_Path :=
           Path.Containing_Directory.Compose (Path.Base_Filename & LI_Suffix);
      end if;

      return (View         => View,
              Path         => Path,
              LI_Path      => LI_Path,
              Language     => Language,
              Timestamp    => Timestamp,
              others       => <>);
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   procedure Dependencies
     (Self   : Object;
      Action : access procedure
        (Sfile     : Filename_Type;
         Unit_Name : Name_Type;
         Kind      : CU_Part;
         Stamp     : Ada.Calendar.Time))
   is
   begin
      --  ??? Ensure the ALI file is parsed
      for D of Self.Dependencies loop
         Action (D.Sfile, D.Unit_Name, D.Unit_Kind, D.Stamp);
      end loop;
   end Dependencies;

   -------------------
   -- Ensure_Parsed --
   -------------------

   function Ensure_Parsed
     (Self     : in out Object;
      Backends : Backend_Set) return Boolean
   is
   begin
      for BK in Backends'Range loop
         if Backends (BK)
           and then Self.Parsed /= BK
           and then Parser.Registry.Exists (Self.Language, BK)
         then
            Parser.Registry.Get (Self.Language, BK).Compute
              (Self, Self.View.Tree.Log_Messages.all);
            exit when Self.Parsed = BK;
         end if;
      end loop;

      return Self.Parsed /= None;
   end Ensure_Parsed;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object;
      Backends : Backend_Set := All_Backends) is
   begin
      for BK in Backends'Range loop
         --  ??? Check Is_Parsed status and ask parser whether the info is
         --  up-to-date

         if Backends (BK)
           and then Object_Info.Parser.Registry.Exists (Self.Language, BK)
         then
            Object_Info.Parser.Registry.Get (Self.Language, BK).Compute
              (Self, Messages);

            exit when Self.Is_Parsed;
         end if;
      end loop;
   end Parse;

   ---------------
   -- Separates --
   ---------------

   function Separates (Self : Object) return Separate_Maps.Map is
   begin
      return Result : Separate_Maps.Map do
         for Sep of Self.CU.Separates loop
            Result.Include (-Sep.Sub_Unit, -Sep.Source);
         end loop;
      end return;
   end Separates;

end GPR2.Build.Object_Info;
