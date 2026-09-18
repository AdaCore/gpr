--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


package body GPR2.Build.Artifacts.Files is

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (Self : Object) return String is
      Path : constant String := Self.Path.String_Value;
   begin
      if not On_Windows then
         --  A backslash is a valid character in a Unix file name

         return Path;
      end if;

      --  Store '/' separators, to avoid escaping them in the JSON.
      --  Unserialize restores the native form via Path_Name.Create_File.

      return Result : String := Path do
         for C of Result loop
            if C = '\' then
               C := '/';
            end if;
         end loop;
      end return;
   end Serialize;

   -----------------
   -- Unserialize --
   -----------------

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object)
   is
      pragma Unreferenced (Chk);
   begin
      if not On_Windows then
         Val := (Path => Path_Name.Create_File (Filename_Type (Repr)));
         return;
      end if;

      if Repr'Length >= 2
        and then Repr (Repr'First .. Repr'First + 1) = "//"
      then
         declare
            Path : Filename_Type := Filename_Type (Repr);
         begin
            --  GNAT checks for a UNC prefix before converting separators.

            Path (Path'First .. Path'First + 1) := "\\";
            Val := (Path => Path_Name.Create_File (Path));
         end;
      else
         Val := (Path => Path_Name.Create_File (Filename_Type (Repr)));
      end if;
   end Unserialize;

end GPR2.Build.Artifacts.Files;
