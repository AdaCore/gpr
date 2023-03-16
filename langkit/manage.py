#! /usr/bin/env python

import os.path

from os import listdir
from datetime import date

# Set the environment
from env import setenv
setenv()

from langkit.libmanage import ManageScript


class Manage(ManageScript):

    def __init__(self):
        super(Manage, self).__init__()

    def create_context(self, args):
        # Keep these import statements here so that they are executed only
        # after the coverage computation actually started.
        from langkit.compile_context import CompileCtx
        from language.parser import gpr_grammar
        from language.parser.lexer import gpr_lexer

        ctx = CompileCtx(lang_name='Gpr',
                         lexer=gpr_lexer,
                         grammar=gpr_grammar,
                         lib_name='Gpr_Parser',
                         default_charset='iso-8859-1',
                         verbosity=args.verbosity,
                         case_insensitive=True,
                         standalone=True)

        ctx.post_process_ada = add_ada_copyright
        ctx.post_process_c = add_c_copyright

        return ctx


def add_ada_copyright(source):
    return ada_header + source


def add_c_copyright(source):
    return c_header + source


current_year = date.today().year

ada_header = """
--
--  Copyright (C) 2019-{}, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

""".format(current_year)

c_header = """
/*
--  Copyright (C) 2019-{}, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

""".format(current_year)

if __name__ == '__main__':
    m = Manage()
    m.run()
