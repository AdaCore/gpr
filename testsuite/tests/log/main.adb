------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Log;
with GPR2.Message;

procedure Main is
   use GPR2;

   procedure Display_Status (Log : GPR2.Log.Object);
   --  Display log status

   --------------------
   -- Display_Status --
   --------------------

   procedure Display_Status (Log : GPR2.Log.Object) is
   begin
      New_Line;
      Put_Line ("Log is empty: " & Boolean'Image (Log.Is_Empty));

      Put_Line
        ("Has_Element (info) "
           & Boolean'Image (Log.Has_Element (Information => True,
                                             Warning     => False,
                                             Error       => False,
                                             Read        => False,
                                             Unread      => True)));
      Put_Line
        ("Has_Element (warn) "
           & Boolean'Image (Log.Has_Element (Information => False,
                                             Warning     => True,
                                             Error       => False,
                                             Read        => False,
                                             Unread      => True)));
      Put_Line
        ("Has_Element (error) "
           & Boolean'Image (Log.Has_Element (Information => False,
                                             Warning     => False,
                                             Error       => True,
                                             Read        => False,
                                             Unread      => True)));
      Put_Line
        ("Has_Element (read) "
           & Boolean'Image (Log.Has_Element (Information => True,
                                             Warning     => True,
                                             Error       => True,
                                             Read        => True,
                                             Unread      => False)));
      Put_Line
        ("Has_Element (unread) "
           & Boolean'Image (Log.Has_Element (Information => True,
                                             Warning     => True,
                                             Error       => True,
                                             Read        => False,
                                             Unread      => True)));
   end Display_Status;

   Log   : GPR2.Log.Object;
   Count : Natural;

begin
   Put_Line ("Log is empty: " & Boolean'Image (Log.Is_Empty));

   Log.Append (Message.Create (Message.Warning, "test warning"));
   Log.Append (Message.Create (Message.Error, "test error"));
   Log.Append (Message.Create (Message.Information, "test information"));

   Display_Status (Log);

   --  Read error

   Count := 0;

   for E in Log.Iterate (Error       => True,
                         Warning     => False,
                         Information => False,
                         Read        => False,
                         Unread      => True)
   loop
      null;
   end loop;

   Put_Line ("Count (err) " & Natural'Image (Count));

   Display_Status (Log);

   --  Read warning

   Count := 0;

   for E in Log.Iterate (Error       => False,
                         Warning     => True,
                         Information => False,
                         Read        => False,
                         Unread      => True)
   loop
      null;
   end loop;

   Put_Line ("Count (warn) " & Natural'Image (Count));

   Display_Status (Log);

   --  Read information

   Count := 0;

   for E in Log.Iterate (Error       => False,
                         Warning     => False,
                         Information => True,
                         Read        => False,
                         Unread      => True)
   loop
      null;
   end loop;

   Put_Line ("Count (warn) " & Natural'Image (Count));

   Display_Status (Log);
end Main;
