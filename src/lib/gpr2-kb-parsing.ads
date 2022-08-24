--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
