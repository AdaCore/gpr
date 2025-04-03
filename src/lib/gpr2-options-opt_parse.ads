--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides facilities for parsing the
--  command line options for GPR2, when using GNATCOLL.Opt_Parse.
--
--  It's meant to be used this way:
--
--      --  First declare the Parser for your application, as you
--      --  normally would with GNATCOLL.Opt_Parse. Something like:
--
--      Parser : Argument_Parser :=
--        Create_Argument_Parser (Help => "Tool to frobnize the foos");
--
--      --  (declare your arguments, too)
--
--      --  Then instanciate this package:
--      package GPR_Args is new GPR2.Options.Opt_Parse.Args (Parser => Parser);
--
--      --  Call Parser.Parse - this will parse your application arguments
--      --  and the project-relate arguments.
--      if not Parser.Parse then
--          ... -- Handle the error
--      end if;
--
--      --  To obtain the computed GPR2 Options, call Parse_GPR2_Options:
--      declare
--         Opt : constant GPR2.Options.Object := GPR_Args.Parsed_GPR2_Options;
--      begin
--         ... your code here.
--      end;

with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;

package GPR2.Options.Opt_Parse is

   generic
      Parser : in out GNATCOLL.Opt_Parse.Argument_Parser;
   package Args is
      function Parsed_GPR2_Options return GPR2.Options.Object;
      --  Returns the GPR2.Options.Object that represents the parsed
      --  GPR2 options. This function is only valid after Parser.Parse
      --  has been called.
   end Args;

end GPR2.Options.Opt_Parse;
