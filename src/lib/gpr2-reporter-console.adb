--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;
with Interfaces.C_Streams;

with GNAT.OS_Lib;

package body GPR2.Reporter.Console is

   procedure Set_Binary_Mode
     (FD : GNAT.OS_Lib.File_Descriptor; To_Binary : Boolean);
   --  Switch FD to binary mode (To_Binary => True) or back to text mode. On
   --  platforms without end-of-line translation (e.g. Unix) this is a no-op.

   ------------
   -- Create --
   ------------

   function Create (Verbosity           : Verbosity_Level := Regular;
                    User_Verbosity      : User_Verbosity_Level := Unset;
                    Use_Full_Pathname   : Boolean := False;
                    Level_Report_Format : Level_Format := Long) return Object
   is
   begin
      return
        (Verbosity      => Verbosity,
         Full_Path      => Use_Full_Pathname,
         Level_Fmt      => Level_Report_Format,
         User_Verbosity => User_Verbosity);
   end Create;

   ---------------------
   -- Internal_Report --
   ---------------------

   overriding procedure Internal_Report
     (Self    : in out Object;
      Message : GPR2.Message.Object;
      Binary  : Boolean := False)
   is
      use Ada.Text_IO;

      Content : constant String :=
                  Message.Format (Self.Full_Path, Self.Level_Fmt);

      Stream : constant File_Type :=
                 (case Message.Level is
                   when Error | Warning => Current_Error,
                   when others          =>
                    (if Message.To_Stderr
                     then Current_Error
                     else Current_Output));
   begin
      if Binary then

         --  Binary mode: write the content verbatim so that any end-of-line
         --  already present in it (e.g. the \r\n of a captured command
         --  output) is preserved as-is, with no \r doubling. The line
         --  terminator that Internal_Report normally appends is still emitted
         --  in text mode, so it gets the platform end-of-line (\r\n on
         --  Windows). It is only added when the content does not already end
         --  with a newline, to avoid an extra blank line.

         declare
            use GNAT.OS_Lib;

            FD : constant File_Descriptor :=
              (case Message.Level is
                 when Error | Warning => Standerr,
                 when others          =>
                   (if Message.To_Stderr then Standerr else Standout));
         begin
            Flush (Stream);
            Set_Binary_Mode (FD, To_Binary => True);
            Put (Stream, Content);
            Flush (Stream);

            --  Back to text mode for the terminating newline if one does not
            --  already exists.

            Set_Binary_Mode (FD, To_Binary => False);

            if Content'Length = 0 or else Content (Content'Last) /= ASCII.LF
            then
               New_Line (Stream);
            end if;
         end;
      else
         Put_Line (Stream, Content);
      end if;
   end Internal_Report;

   ---------------------
   -- Set_Binary_Mode --
   ---------------------

   procedure Set_Binary_Mode
     (FD : GNAT.OS_Lib.File_Descriptor; To_Binary : Boolean)
   is
      use Interfaces.C_Streams;
   begin
      if To_Binary then
         set_binary_mode (int (FD));
      else
         set_text_mode (int (FD));
      end if;
   end Set_Binary_Mode;

   -----------------------
   -- Set_Full_Pathname --
   -----------------------

   procedure Set_Full_Pathname
     (Self : in out Object; Use_Full_Pathname : Boolean)
   is
   begin
      Self.Full_Path := Use_Full_Pathname;
   end Set_Full_Pathname;

   -----------------------------
   -- Set_Level_Report_Format --
   -----------------------------

   procedure Set_Level_Report_Format
     (Self : in out Object; Level_Report_Format : Level_Format)
   is
   begin
      Self.Level_Fmt := Level_Report_Format;
   end Set_Level_Report_Format;

   ------------------------
   -- Set_User_Verbosity --
   ------------------------

   procedure Set_User_Verbosity
     (Self : in out Object;
      Verbosity : User_Verbosity_Level) is
   begin
      Self.User_Verbosity := Verbosity;
   end Set_User_Verbosity;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (Self : in out Object; Verbosity : Verbosity_Level)
   is
   begin
      Self.Verbosity := Verbosity;
   end Set_Verbosity;

   --------------------
   -- User_Verbosity --
   --------------------

   overriding
   function User_Verbosity (Self : Object) return User_Verbosity_Level is
   begin
      return Self.User_Verbosity;
   end User_Verbosity;

   ---------------
   -- Verbosity --
   ---------------

   overriding function Verbosity (Self : Object) return Verbosity_Level is
   begin
      return Self.Verbosity;
   end Verbosity;

end GPR2.Reporter.Console;
