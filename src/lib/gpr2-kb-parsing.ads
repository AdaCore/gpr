------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

--  Knowledge base xml parsing

private package GPR2.KB.Parsing is

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

   function Parse_Default_Knowledge_Base (Flags : Parsing_Flags) return Object;
   --  Implementation of Knowledge_Base.Create_Default

end GPR2.KB.Parsing;
