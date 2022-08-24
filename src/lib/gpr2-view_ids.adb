--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Hash;

package body GPR2.View_Ids is

   use type GPR2.Context.Context_Kind;

   ROOT_VIEWS_PREFIX    : constant Character  := '<';
   AGGR_VIEWS_PREFIX    : constant Character  := '$';
   SPECIAL_VIEWS_PREFIX : constant Character  := '!';
   EXTENDED_PREFIX      : constant Character  := '>';

   UNDEFINED_IMAGE      : constant Value_Type := "";
   RUNTIME_IMAGE        : constant Value_Type := "runtime";
   CONFIG_IMAGE         : constant Value_Type := "config";

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
      if not Project_File.Is_Defined then
         raise View_Id_Error with "cannot create view id from empty path";
      end if;

      if not Project_File.Has_Dir_Name then
         raise View_Id_Error with "cannot creaste view id from relative path";
      end if;

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

   function Image (Self : View_Id) return Value_Type is
   begin
      case Self.Kind is
         when Null_Id    => return UNDEFINED_IMAGE;
         when Config_Id  => return SPECIAL_VIEWS_PREFIX & CONFIG_IMAGE;
         when Runtime_Id => return SPECIAL_VIEWS_PREFIX & RUNTIME_IMAGE;
         when Project_Id =>
            declare
               Extending_Suffix : constant Value_Type :=
                                    (if Length (Self.Extending) = 0
                                     then ""
                                     else EXTENDED_PREFIX &
                                      Value_Type (To_String (Self.Extending)));
            begin
               if Self.Context = Root then
                  return ROOT_VIEWS_PREFIX &
                    Value_Type (To_String (Self.Id)) &
                    Extending_Suffix;
               else
                  return AGGR_VIEWS_PREFIX &
                    Value_Type (To_String (Self.Id)) &
                    Extending_Suffix;
               end if;
            end;
      end case;
   end Image;

   ------------
   -- Import --
   ------------

   function Import (Name : Value_Type) return View_Id
   is
      Prefix        : Character;
      Id            : Value_Type renames
                        Name (Name'First + 1 .. Name'Last);
      Ext_Delimiter : Natural;
      Context       : GPR2.Context.Context_Kind;

   begin
      if Name = UNDEFINED_IMAGE then
         return (Kind => Null_Id);
      end if;

      Prefix := Name (Name'First);

      if Prefix = SPECIAL_VIEWS_PREFIX then
         if Id = CONFIG_IMAGE then
            return (Kind => Config_Id);
         elsif Id = RUNTIME_IMAGE then
            return (Kind => Runtime_Id);
         else
            raise View_Id_Error with "Invalid view id image";
         end if;
      end if;

      if Prefix = ROOT_VIEWS_PREFIX then
         Context := Root;

      elsif Prefix = AGGR_VIEWS_PREFIX then
         Context := Aggregate;

      else
         raise View_Id_Error with "invalid view id image";
      end if;

      Ext_Delimiter := 0;

      for J in Id'Range loop
         if Id (J) = EXTENDED_PREFIX then
            Ext_Delimiter := J;
            exit;
         end if;
      end loop;

      if Ext_Delimiter = 0 then
         return (Kind      => Project_Id,
                 Id        => To_Unbounded_String (String (Id)),
                 Context   => Context,
                 Extending => Null_Unbounded_String);
      else
         return (Kind      => Project_Id,
                 Id        => To_Unbounded_String (String
                                (Id (Id'First .. Ext_Delimiter - 1))),
                 Context   => Context,
                 Extending => To_Unbounded_String (String
                                (Id (Ext_Delimiter + 1 .. Id'Last))));
      end if;
   end Import;

   --------------------
   -- Is_Valid_Image --
   --------------------

   function Is_Valid_Image (Name : Value_Type) return Boolean is
   begin
      if Name'Length = 0 then
         return True;

      elsif Name (Name'First) = SPECIAL_VIEWS_PREFIX then
         return Name = SPECIAL_VIEWS_PREFIX & CONFIG_IMAGE
           or else Name = SPECIAL_VIEWS_PREFIX & RUNTIME_IMAGE;

      else
         return Name (Name'First) = ROOT_VIEWS_PREFIX
           or else Name (Name'First) = AGGR_VIEWS_PREFIX;
      end if;
   end Is_Valid_Image;

end GPR2.View_Ids;
