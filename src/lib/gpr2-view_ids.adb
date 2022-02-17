------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with Ada.Strings.Hash;

package body GPR2.View_Ids is

   use type GPR2.Context.Context_Kind;

   ROOT_VIEWS_PREFIX    : constant Character          := '<';
   AGGR_VIEWS_PREFIX    : constant Character          := '$';
   SPECIAL_VIEWS_PREFIX : constant Character          := '!';
   EXTENDED_PREFIX      : constant Character          := ':';

   RUNTIME_IMAGE        : constant Name_Type          := "runtime";
   CONFIG_IMAGE         : constant Name_Type          := "config";

   -------
   -- < --
   -------

   function "<" (Self : View_Id; Other : View_Id) return Boolean is
   begin
      return Image (Self) < Image (Other);
   end "<";

   -------
   -- = --
   -------

   overriding function "=" (Self : View_Id; Other : View_Id) return Boolean is
   begin
      if Self.Kind /= Other.Kind then
         return False;
      elsif Self.Kind = Project_Id then
         return Self.Id = Other.Id
           and then Self.Context = Other.Context
           and then Self.Extending = Other.Extending;
      else
         return True;
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create
     (Project_File : GPR2.Path_Name.Object;
      Context      : GPR2.Context.Context_Kind := Root;
      Extending    : View_Id := Undefined)
      return View_Id
   is
      Id_Str : Unbounded_String;
   begin
      Append (Id_Str, GPR2.Path_Name.To_OS_Case (Project_File.Value));

      return (Kind      => Project_Id,
              Id        => Id_Str,
              Context   => Context,
              Extending => (if Is_Defined (Extending)
                            then To_Unbounded_String (String
                              (Image (Extending)))
                            else Null_Unbounded_String));
   end Create;

   ----------
   -- Hash --
   ----------

   function Hash (Self : View_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Image (Self)));
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Self : View_Id) return Optional_Name_Type is
   begin
      case Defined_Id_Kind (Self.Kind) is
         when Config_Id  => return SPECIAL_VIEWS_PREFIX & CONFIG_IMAGE;
         when Runtime_Id => return SPECIAL_VIEWS_PREFIX & RUNTIME_IMAGE;
         when Project_Id =>
            declare
               Extending_Suffix : constant Optional_Name_Type :=
                                    (if Length (Self.Extending) = 0
                                     then ""
                                     else EXTENDED_PREFIX &
                                       Name_Type (To_String (Self.Extending)));
            begin
               if Self.Context = Root then
                  return ROOT_VIEWS_PREFIX &
                    Name_Type (To_String (Self.Id)) &
                    Extending_Suffix;
               else
                  return AGGR_VIEWS_PREFIX &
                    Name_Type (To_String (Self.Id)) &
                    Extending_Suffix;
               end if;
            end;
      end case;
   end Image;

end GPR2.View_Ids;
