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
--
--      --  Now you can call Parse_GPR2_Options to process all project-specific
--      --  switches.
--      declare
--         Options            : GPR2.Options.Object;
--         Unparsed_Arguments : GNATCOLL.Opt_Parse.XString_Vector;
--      begin
--         --  This call will parse both your application arguments and the
--         --  project-relate arguments.
--         if not Parser.Parse (Unknown_Arguments => Unparsed_Arguments) then
--             ... -- Handle the error
--         end if;
--
--         --   To obtain the computed GPR2 Options, call Parse_GPR2_Options:
--         if GPR_Args.Parse_GPR2_Options
--           (Arguments => Unparsed_Arguments,
--            Options   => Options)
--         then
--            --  At this point, Options is initialized and ready to use
--            ...
--         else
--            --  Handle the error
--            Put_Line ("Could not parse the GPR options.");
--            OS_Exit (1);
--         end if;
--       end;
--
--   Note that no error will be printed if you have remaining unparsed
--   arguments at the end of your chain. You might want to handle this,
--   with something like this:
--
--      for X of Unparsed_Arguments loop
--         Put_Line ("Unrecognized argument " & X.To_String);
--      end loop;
--
--      if not Unparsed_Arguments.Is_Empty then
--         --  In case of unrecognized arguments, print the help and exit
--         Put_Line (Parser.Help);
--         OS_Exit (1);
--      end if;

with GNATCOLL.Opt_Parse;    use GNATCOLL.Opt_Parse;

package GPR2.Options.Opt_Parse is

   generic
      Parser : in out GNATCOLL.Opt_Parse.Argument_Parser;
   package Args is
      function Parse_GPR2_Options
        (Arguments : in out GNATCOLL.Opt_Parse.XString_Vector;
         Options   : out GPR2.Options.Object) return Boolean;
      --  Parse GPR2 options passed in Arguments. The function returns True if
      --  the parsing was successful, False otherwise. In case of success,
      --  Options contains the ready-to-use GPR2.Options object.
      --  Upon exit, Arguments contains the remaining arguments that were not
      --  parsed by this function.
   end Args;

end GPR2.Options.Opt_Parse;
