--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Calendar.Conversions; use Ada.Calendar.Conversions;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body Test_Assert is
   package IO renames Ada.Text_IO;
   package UTF renames Ada.Strings.UTF_Encoding.Wide_Strings;

   procedure Put_Indented (Indent_Columns : Natural; Lines : String);
   --  Put Lines on the standard output. This also indents all but the first
   --  line with Indent_Column spaces.

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Success  : Boolean;
       Msg      : String := "";
       Location : String := SI.Source_Location) is
   begin
      --  Start with an indicator about test status so that it is easy to
      --  quickly spot failing tests.

      if Success then
         IO.Put ("OK  ");
      else
         IO.Put ("FAIL");
         Final_Status := 1;
      end if;

      --  Then tell where the failure happened, and add the given message (if
      --  any).

      IO.Put (" " & Location);
      if Msg'Length > 0 then
         IO.Put (" " & Msg);
      end if;
      IO.New_Line;

      Assert_Count := Assert_Count + 1;
   end Assert;

   ------------------
   -- Put_Indented --
   ------------------

   procedure Put_Indented (Indent_Columns : Natural; Lines : String) is
      Starting_Line : Boolean := False;
   begin
      for C of Lines loop
         if C = ASCII.LF then
            Starting_Line := True;
         elsif Starting_Line then
            Starting_Line := False;
            IO.Put ((1 .. Indent_Columns => ' '));
         end if;
         IO.Put (C);
      end loop;
   end Put_Indented;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Left, Right : String;
       Msg         : String := "";
       Location    : String := SI.Source_Location)
   is
      Success : constant Boolean := Left = Right;

      Expected_Prefix : constant String := "expected: ";
      Got_Prefix      : constant String := "got:      ";
      Indent          : constant Natural := Expected_Prefix'Length;

   begin
      Assert (Success, Msg, Location);
      if not Success then
         if Right'Length > 0 then
            IO.Put (Expected_Prefix);
            Put_Indented (Indent, Right);
            IO.New_Line;
         else
            IO.Put_Line ("expected empty string");
         end if;

         if Left'Length > 0 then
            IO.Put (Got_Prefix);
            Put_Indented (Indent, Left);
            IO.New_Line;
         else
            IO.Put_Line ("got empty string");
         end if;
      end if;
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Left        : Wide_String;
       Right       : UTF8.UTF_8_String;
       Msg         : String := "";
       Location    : String := SI.Source_Location)
   is
      UTF_Left : constant UTF8.UTF_8_String := UTF.Encode (Left);
   begin
      Assert (UTF_Left, Right, Msg, Location);
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Left        : Integer;
       Right       : Integer;
       Msg         : String := "";
       Location    : String := SI.Source_Location)
   is
      Success : constant Boolean := Left = Right;
   begin
      Assert (Success, Msg, Location);
      if not Success then
         IO.Put_Line ("expected: " & Right'Img);
         IO.Put_Line ("got:      " & Left'Img);
      end if;
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
      (Left, Right : VFS.Virtual_File;
       Msg         : String := "";
       Location    : String := SI.Source_Location)
   is
      use type VFS.Virtual_File;
      Success : constant Boolean := Left = Right;
   begin
      Assert (Success, Msg, Location);
      if not Success then
         IO.Put_Line ("expected: " & VFS.Display_Full_Name (Right));
         IO.Put_Line ("got:      " & VFS.Display_Full_Name (Left));
      end if;
   end Assert;

   ---------------------
   -- Assert_Inferior --
   ---------------------

   procedure Assert_Inferior
      (Left     : Time;
       Right    : Time;
       Msg      : String := "";
       Location : String := SI.Source_Location)
   is
      Success : constant Boolean := Left < Right;
   begin
      Assert (Success, Msg, Location);
      if not Success then
         IO.Put_Line ("left:  " & Image (Left) &
                      " (" & To_Unix_Nano_Time (Left)'Img & ")");
         IO.Put_Line ("right: " & Image (Right) &
                      " (" & To_Unix_Nano_Time (Right)'Img & ")");
      end if;
   end Assert_Inferior;

   ------------
   -- Report --
   ------------

   function Report return Natural is
   begin
      if Final_Status = 0 then
         IO.Put_Line ("<=== TEST PASSED ===>");
      else
         IO.Put_Line ("<=== TEST FAILED ===>");
      end if;
      return Final_Status;
   end Report;

end Test_Assert;
