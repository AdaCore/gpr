--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Knowledge base xml parsing

with GPR2.Environment;

private package GPR2.KB.Parsing is

   procedure Parse_Knowledge_Base
     (Self        : in out Object;
      Location    : GPR2.Path_Name.Object;
      Flags       : Parsing_Flags;
      Environment : GPR2.Environment.Object)
     with Pre => Self.Is_Defined and then Location.Is_Defined
                 and then Location.Exists;
   --  Parses xml file(s) into KB contents

   procedure Add
     (Self        : in out Object;
      Flags       : Parsing_Flags;
      Content     : Value_Not_Empty;
      Environment : GPR2.Environment.Object)
     with Pre => Self.Is_Defined;
   --  Implementation of Knowledge_Base.Add

   function Parse_Default_Knowledge_Base
     (Flags : Parsing_Flags;
      Environment : GPR2.Environment.Object) return Object;
   --  Implementation of Knowledge_Base.Create_Default

end GPR2.KB.Parsing;
