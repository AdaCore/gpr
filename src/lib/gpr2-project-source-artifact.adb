------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2018, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body GPR2.Project.Source.Artifact is

   ------------
   -- Create --
   ------------

   function Create
     (Source              : GPR2.Project.Source.Object;
      Object              : Path_Name.Object;
      Dependency          : Path_Name.Object := Path_Name.Undefined;
      Preprocessed_Source : Path_Name.Object := Path_Name.Undefined)
      return Artifact.Object is
   begin
      return Artifact.Object'
        (Source           => Source,
         Object           => Object,
         Dependency       => Dependency,
         Preprocessed_Src => Preprocessed_Source);
   end Create;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Artifact.Object) return Path_Name.Object is
   begin
      return Self.Dependency;
   end Dependency;

   ----------
   -- List --
   ----------

   function List (Self : Object) return Path_Name.Set.Object is
      Result : Path_Name.Set.Object;
   begin
      if Self.Has_Object_Code then
         Result.Append (Self.Object_Code);

         declare
            Name : constant Name_Type :=
                     Self.Source.Source.Path_Name.Simple_Name;
            Dir  : constant Optional_Name_Type :=
                     Optional_Name_Type
                       (Self.Source.View.Object_Directory.Value);
         begin
            Result.Append (Path_Name.Create_File (Name & ".stdout", Dir));
            Result.Append (Path_Name.Create_File (Name & ".stderr", Dir));
         end;
      end if;

      if Self.Has_Dependency then
         Result.Append (Self.Dependency);
      end if;

      if Self.Has_Preprocessed_Source then
         Result.Append (Self.Preprocessed_Source);
      end if;

      return Result;
   end List;

   -----------------
   -- Object_Code --
   -----------------

   function Object_Code
     (Self : Artifact.Object) return Path_Name.Object is
   begin
      return Self.Object;
   end Object_Code;

   -------------------------
   -- Preprocessed_Source --
   -------------------------

   function Preprocessed_Source (Self : Object) return Path_Name.Object is
   begin
      return Self.Preprocessed_Src;
   end Preprocessed_Source;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Project.Source.Object is
   begin
      return Self.Source;
   end Source;

end GPR2.Project.Source.Artifact;
