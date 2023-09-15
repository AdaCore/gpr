--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;
with Ada.Strings.Fixed;
with GNAT.Formatted_String;

package body GPR2.Source_Reference is

   ------------
   -- Column --
   ------------

   function Column (Self : Object) return Positive is
   begin
      return Self.Column;
   end Column;

   ------------
   -- Create --
   ------------

   function Create
     (Filename     : Path_Name.Full_Name;
      Line, Column : Natural) return Object'Class is
   begin
      return Object'(Line, Column, To_Unbounded_String (Filename));
   end Create;

   --------------
   -- Filename --
   --------------

   function Filename (Self : Object) return Path_Name.Full_Name is
   begin
      return To_String (Self.Filename);
   end Filename;

   ------------
   -- Format --
   ------------

   function Format (Self : Object; Full_Path_Name : Boolean := False)
     return String is

      use GNAT.Formatted_String;

      function Simple_Name (S : String) return String;
      --  Handle possible pseudo files

      -----------------
      -- Simple_Name --
      -----------------

      function Simple_Name (S : String) return String is
         Start : Natural := Ada.Strings.Fixed.Index (S, "<ram>");
      begin
         if Start = 0 then
            Start := S'First;
         else
            Start := Start + 5;
         end if;

         return Directories.Simple_Name (S (Start .. S'Last));
      end Simple_Name;

      Filename : constant String :=
                   (if Full_Path_Name
                    then To_String (Self.Filename)
                    else Simple_Name (To_String (Self.Filename)));

   begin

      if Self.Has_Source_Reference then
         declare
            Format : constant Formatted_String := +"%s:%d:%02d";
         begin
            return -(Format & Filename & Self.Line & Self.Column);
         end;

      else
         declare
            Format : constant Formatted_String := +"%s";
         begin
            return -(Format & Filename);
         end;
      end if;
   end Format;

   ----------
   -- Line --
   ----------

   function Line (Self : Object) return Positive is
   begin
      return Self.Line;
   end Line;

end GPR2.Source_Reference;
