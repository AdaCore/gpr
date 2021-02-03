------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

   ROOT_VIEWS_PREFIX    : constant Character := '%';
   AGGR_VIEWS_PREFIX    : constant Character := '#';
   SPECIAL_VIEWS_PREFIX : constant Character := '@';

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
         return Self.Id = Other.Id and then Self.Context = Other.Context;
      else
         return True;
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create
     (Project_File : GPR2.Path_Name.Object;
      Context      : GPR2.Context.Context_Kind := Root)
      return View_Id
   is
      Id_Str : Unbounded_String;
   begin
      if not Project_File.Is_Defined then
         raise View_Id_Error with "cannot create view id from empty path";
      end if;

      if not Project_File.Has_Dir_Name then
         raise View_Id_Error with "cannot creaste view id from relative path";
      end if;

      Append (Id_Str, Project_File.Value);

      return (Kind    => Project_Id,
              Id      => Id_Str,
              Context => Context);
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
      case Self.Kind is
         when Null_Id    => return "";
         when Config_Id  => return SPECIAL_VIEWS_PREFIX & "config";
         when Runtime_Id => return SPECIAL_VIEWS_PREFIX & "runtime";
         when Project_Id =>
            if Self.Context = Root then
               return ROOT_VIEWS_PREFIX & Name_Type (To_String (Self.Id));
            else
               return AGGR_VIEWS_PREFIX & Name_Type (To_String (Self.Id));
            end if;
      end case;
   end Image;

   ------------
   -- Import --
   ------------

   function Import (Str : Optional_Name_Type) return View_Id is
   begin
      if Str'Length = 0 then
         return (Kind => Null_Id);

      else
         declare
            Prefix : constant Character := Str (Str'First);
            Id     : constant String :=
                       String (Str (Str'First + 1 .. Str'Last));

         begin
            case Prefix is
               when SPECIAL_VIEWS_PREFIX =>
                  if Id = "config" then
                     return (Kind => Config_Id);

                  elsif Id = "runtime" then
                     return (Kind => Config_Id);

                  else
                     raise View_Id_Error with "invalid view id image";
                  end if;

               when ROOT_VIEWS_PREFIX =>
                  return (Kind    => Project_Id,
                          Id      => To_Unbounded_String (String (Str)),
                          Context => Root);

               when AGGR_VIEWS_PREFIX =>
                  return (Kind    => Project_Id,
                          Id      => To_Unbounded_String (String (Str)),
                          Context => Aggregate);

               when others =>
                  raise View_Id_Error with "invalid view id image";
            end case;
         end;
      end if;
   end Import;

   --------------------
   -- Is_Valid_Image --
   --------------------

   function Is_Valid_Image (Str : Optional_Name_Type) return Boolean is
   begin
      if Str'Length = 0 then
         return True;

      elsif Str (Str'First) = SPECIAL_VIEWS_PREFIX then
         return Str = SPECIAL_VIEWS_PREFIX & "config"
           or else Str = SPECIAL_VIEWS_PREFIX & "runtime";

      else
         return Str (Str'First) = ROOT_VIEWS_PREFIX
           or else Str (Str'First) = AGGR_VIEWS_PREFIX;
      end if;
   end Is_Valid_Image;

end GPR2.View_Ids;
