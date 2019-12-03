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

--  Knowledge base xml parsing

private package GPR2.Project.Configuration.KB.Parsing is

   procedure Parse_Knowledge_Base
     (Self     : in out Object;
      Location : GPR2.Path_Name.Object;
      Flags    : Parsing_Flags)
     with Pre => Self.Is_Defined and then Location.Is_Defined
                 and then Location.Exists;
   --  Parses xml file(s) into KB contents

   procedure Add
     (Self     : in out Object;
      Flags    : Parsing_Flags;
      Content  : Value_Not_Empty)
     with Pre => Self.Is_Defined;
   --  Implementation of Knowledge_Base.Add

end GPR2.Project.Configuration.KB.Parsing;
