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
      Val := (Path => Path_Name.Create_File (Filename_Type (Repr)));
   end Unserialize;

end GPR2.Build.Artifacts.Files;
