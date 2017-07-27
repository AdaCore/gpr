from   quex.input.code.core  import CodeTerminal, \
                                               CodeTerminal_NULL
from   quex.engine.analyzer.state.core  import Processor
from   quex.engine.analyzer.state.entry import Entry
import quex.engine.state_machine.index  as     index
from   quex.engine.misc.tools                import typed

from   types import FunctionType
from   copy  import copy

#__________________________________________________________________________
#
# TerminalState:
#                    .-------------------------------------------------.
#  .-----.           |                                                 |
#  | 341 |--'accept'--> input_p = position[2]; --->---+---------.      |
#  '-----'           |  set terminating zero;         |         |      |
#  .-----.           |                                |    .---------. |
#  | 412 |--'accept'--> column_n += length  ------>---+    | pattern | |
#  '-----'           |  set terminating zero;         |    |  match  |--->
#  .-----.           |                                |    | actions | |
#  | 765 |--'accept'--> line_n += 2;  ------------>---'    '---------' |
#  '-----'           |  set terminating zero;                          |
#                    |                                                 |
#                    '-------------------------------------------------'
# 
# A terminal state prepares the execution of the user's pattern match 
# actions and the start of the next analysis step. For this, it computes
# line and column numbers, sets terminating zeroes in strings and resets
# the input pointer to the position where the next analysis step starts.
#__________________________________________________________________________
class Terminal(Processor):
    @typed(Name=(str,unicode), Code=CodeTerminal)
    def __init__(self, Code, Name=""):
        Processor.__init__(self, index.get(), Entry())
        self.__incidence_id = None
        self.__code         = Code
        self.__name         = Name

    def incidence_id(self):
        return self.__incidence_id

    def set_incidence_id(self, IncidenceId):
        assert self.__incidence_id is None
        self.__incidence_id = IncidenceId

    @typed(Name=(str,unicode))
    def set_name(self, Name):
        self.__name = Name

    def name(self):
        return self.__name

    def code(self, TheAnalyzer):
        return self.__code.get_code()

    def pure_code(self):
        return self.__code.get_pure_code()

    def requires_lexeme_terminating_zero_f(self):
        return self.__code.requires_lexeme_terminating_zero_f()

    def requires_lexeme_begin_f(self):
        return self.__code.requires_lexeme_begin_f()

class TerminalGenerated(Terminal):
    @typed(Name=(str,unicode), GeneratorFunction=FunctionType, Data=dict, Code=list)
    def __init__(self, GeneratorFunction, Data, Name="", PrefixCode=None):
        Terminal.__init__(self, CodeTerminal_NULL, Name)
        self.__generator   = GeneratorFunction
        self.__data        = Data
        self.__prefix_code = PrefixCode

    def code(self, TheAnalyzer):
        result = copy(self.__prefix_code)
        result.extend(
            self.__generator(self.__data, TheAnalyzer)
        )
        return result


