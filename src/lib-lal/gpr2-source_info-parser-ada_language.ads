------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

package GPR2.Source_Info.Parser.Ada_Language
  with Elaborate_Body
is

   Language : aliased constant Name_Type := "Ada";

   type Object is new Parser.Object
     (Language => Language'Unrestricted_Access,
      Kind     => Source) with null record;

   overriding procedure Compute
     (Self   : not null access Object;
      Data   : in out Source_Info.Object'Class;
      Source : GPR2.Source.Object'Class;
      LI     : Path_Name.Object'Class    := GPR2.Path_Name.Undefined;
      View   : Project.View.Object'Class := Project.View.Undefined);
   --  Setup Data with the information from parsing Ada source file

   procedure Unregister;
   --  Unregister the Ada_Language source parser

end GPR2.Source_Info.Parser.Ada_Language;
