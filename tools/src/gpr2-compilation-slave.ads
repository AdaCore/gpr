------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  A slave is a compilation node on the network. It is composed of a host name
--  and a connection port. Slaves are recorded into the registry.

with GNAT.Sockets;

package GPR2.Compilation.Slave is

   type Object is tagged private;

   Undefined : constant Object;

   function Create
     (Host : Name_Type;
      Port : GNAT.Sockets.Port_Type) return Object;
   --  Create a new slave

   function Host (Self : Object) return Name_Type;
   --  Returns the host part of the slave

   function Port (Self : Object) return GNAT.Sockets.Port_Type;
   --  Returns the port of the slave

private

   type Object is tagged record
      Host : Unbounded_String;
      Port : GNAT.Sockets.Port_Type;
   end record;

   Undefined : constant Object  :=
                 (Port => GNAT.Sockets.Port_Type'Last, others => <>);

   function Host (Self : Object) return Name_Type is
      (Name_Type (To_String (Self.Host)));

   function Port (Self : Object) return GNAT.Sockets.Port_Type is (Self.Port);

end GPR2.Compilation.Slave;
