--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Holders;
with Ada.Tags;
with Ada.Text_IO;

package body GPR2.Message.Reporter is

   package Holders is new Ada.Containers.Indefinite_Holders
     (Object'Class);

   Default      : Text_Reporter;
   The_Reporter : Holders.Holder := Holders.To_Holder (Default);

   ---------------------
   -- Active_Reporter --
   ---------------------

   function Active_Reporter return Object'Class is
   begin
      return The_Reporter.Element;
   end Active_Reporter;

   --------------------------------
   -- Configure_Default_Reporter --
   --------------------------------

   procedure Configure_Default_Reporter
     (Use_Full_Pathname   : Boolean       := False;
      Level_Report_Format : Level_Format  := Long)
   is
      use type Ada.Tags.Tag;
   begin
      if The_Reporter.Element'Tag = Text_Reporter'Tag then
         The_Reporter := Holders.To_Holder
           (Text_Reporter'
              (Full_Path => Use_Full_Pathname,
               Level_Fmt => Level_Report_Format));
      end if;
   end Configure_Default_Reporter;

   -----------------------
   -- Register_Reporter --
   -----------------------

   procedure Register_Reporter (Instance : Object'Class) is
   begin
      The_Reporter := Holders.To_Holder (Instance);
   end Register_Reporter;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (Self : Text_Reporter; Message : GPR2.Message.Object)
   is
      use Ada.Text_IO;
   begin
      Put_Line
        ((case Message.Level is
            when Information | Lint => Current_Output,
            when Error | Warning    => Current_Error),
         Message.Format (Self.Full_Path, Self.Level_Fmt));
   end Report;

end GPR2.Message.Reporter;
