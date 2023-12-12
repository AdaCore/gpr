--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.String_Builders;

package body GPR2.Build.Artifact_Ids is

   Separator : constant Character := ';';

   ---------
   -- "<" --
   ---------

   function "<" (Id, Other : Artifact_Id) return Boolean is
      use type View_Ids.View_Id;
   begin
      return Id.View_Id < Other.View_Id
        and then Id.Class < Other.Class
        and then Id.Path < Other.Path;
   end "<";

   ------------
   -- Create --
   ------------

   function Create
     (Class : Artifact_Class; View : GPR2.View_Ids.View_Id; Path : Value_Type)
      return Artifact_Id
   is
   begin
      return (Path_Len  => Path'Length,
              View_Id   => View,
              Class     => Class,
              Path      => Path);
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Id : Artifact_Id) return Value_Type
   is
      Result : GNATCOLL.String_Builders.String_Builder;
      use GNATCOLL.String_Builders;
   begin
      Append (Result, Image (Id.Class));
      Append (Result, Separator);
      Append (Result, View_Ids.Image (Id.View_Id));
      Append (Result, Separator);
      Append (Result, Id.Path);
      return Value_Type (As_String (Result));
   end Image;

   ------------
   -- Import --
   ------------

   function Import (Image : Value_Type) return Artifact_Id is
      Start  : Natural := Image'First;
      Token  : Natural := 1;
      Class  : Artifact_Class;

   begin
      Start := Image'First;

      for J in Image'Range loop
         if Image (J) = Separator then
            if Token = 1 then
               Class := +Optional_Name_Type (Image (Start .. J - 1));

            elsif Token = 2 then

               return Create
                 (Class,
                  View_Ids.Import (Image (Start .. J - 1)),
                  Image (J + 1 .. Image'Last));
            end if;

            Token := Token + 1;
            Start := J + 1;
         end if;
      end loop;

      return Artifact_Ids.Undefined;
   end Import;

   --------------------
   -- Is_Valid_Image --
   --------------------

   function Is_Valid_Image (Image : Value_Type) return Boolean
   is
      Cnt_Sep : Natural := 0;
   begin
      for J in Image'Range loop
         if Image (J) = Separator then
            if J = Image'First or else J = Image'Last then
               return False;
            elsif J < Image'Last and then Image (J + 1) = Separator then
               return False;
            end if;

            Cnt_Sep := Cnt_Sep + 1;

            if Cnt_Sep > 2 then
               return False;
            end if;
         end if;
      end loop;

      return Cnt_Sep = 2;
   end Is_Valid_Image;

end GPR2.Build.Artifact_Ids;
