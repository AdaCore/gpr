import quex.engine.misc.error        as     error
from quex.engine.misc.file_in        import skip_whitespace, \
                                            check, \
                                            read_identifier
from quex.engine.state_machine.core  import StateMachine
from quex.input.code.base import SourceRef
from quex.input.regular_expression.exception                  import RegularExpressionException

from copy import deepcopy

__debug_recursion_depth  = -1
__debug_output_enabled_f = False # True / False 

class PatternShorthand:
    def __init__(self, Name="", StateMachine="", SourceReference=None, RE=""):
        assert StateMachine.__class__.__name__ == "StateMachine"

        self.name               = Name
        self.__state_machine    = StateMachine
        if SourceReference is None: SourceReference = SourceRef()
        self.sr                 = SourceReference
        self.regular_expression = RE

    def get_state_machine(self):
        return self.__state_machine.clone()

    def get_character_set(self):
        if len(self.__state_machine.states) != 2: return None
        t  = self.__state_machine.states[self.__state_machine.init_state_index].target_map
        db = t.get_map()
        if len(db) != 1: return None
        return deepcopy(db[db.keys()[0]])

def __snap_until(stream, ClosingDelimiter, OpeningDelimiter=None):
     """Cuts the first letters of the utf8_string until an un-backslashed
        Delimiter occurs.
     """
     cut_string = ""  
     backslash_f = False
     open_bracket_n = 1 
     while 1 + 1 == 2:
        letter = stream.read(1)
        if letter == "": 
            raise RegularExpressionException("Unable to find closing delimiter '%s'" % ClosingDelimiter)

        cut_string += letter    

        if letter == "\\": 
            backslash_f = not backslash_f       
            continue
            
        elif letter == ClosingDelimiter and not backslash_f: 
            if open_bracket_n == 1: cut_string = cut_string[:-1]; break
            open_bracket_n -= 1

        elif letter == OpeningDelimiter and not backslash_f: 
            # NOTE: if OpeningDelimiter is None, then this can never be the case!
            open_bracket_n += 1

        # if a backslash would have appeared, we would have 'continue'd (see above)
        backslash_f = False    
     else:
        raise RegularExpressionException("Unable to find closing delimiter '%s'" % ClosingDelimiter)
   
     return cut_string

def __debug_print(msg, msg2="", msg3=""):
    global __debug_recursion_depth
    if not __debug_output_enabled_f: return
    if type(msg2) != str: msg2 = repr(msg2)
    if type(msg3) != str: msg3 = repr(msg3)
    txt = "##" + "  " * __debug_recursion_depth + msg + " " + msg2 + " " + msg3
    txt = txt.replace("\n", "\n    " + "  " * __debug_recursion_depth)
    print txt
    
def __debug_exit(result, stream):
    global __debug_recursion_depth
    __debug_recursion_depth -= 1

    if __debug_output_enabled_f: 
        pos = stream.tell()
        txt = stream.read()
        stream.seek(pos)    
        __debug_print("##exit: [%s], remainder = \"%s\"" % (type(result), txt))
        
    return result

def __debug_entry(function_name, stream):
    global __debug_recursion_depth
    __debug_recursion_depth += 1

    if __debug_output_enabled_f: 
        pos = stream.tell()
        txt = stream.read()
        stream.seek(pos)    
        __debug_print("##entry: %s, remainder = \"%s\"" % (function_name, txt))

def snap_replacement(stream, PatternDict, StateMachineF=True):
    """Snaps a predefined pattern from the input string and returns the resulting
       state machine.
    """ 
    skip_whitespace(stream)
    pattern_name = read_identifier(stream)  
    if pattern_name == "":
        raise RegularExpressionException("Pattern replacement expression misses identifier after '{'.")
    skip_whitespace(stream)

    if not check(stream, "}"):
        raise RegularExpressionException("Pattern replacement expression misses closing '}' after '%s'." \
                                         % pattern_name)

    error.verify_word_in_list(pattern_name, PatternDict.keys(),
                             "Specifier '%s' not found in any preceeding 'define { ... }' section." % pattern_name, 
                             stream)

    reference = PatternDict[pattern_name]
    assert reference.__class__ == PatternShorthand

    # The replacement may be a state machine or a number set
    if StateMachineF:
        # Get a cloned version of state machine
        state_machine = reference.get_state_machine()
        assert isinstance(state_machine, StateMachine)

        # It is essential that state machines defined as patterns do not 
        # have origins. Otherwise, the optimization of patterns that
        # contain pattern replacements might get confused and can
        # not find all optimizations.
        assert state_machine.has_origins() == False
            
        # A state machine, that contains pre- or post- conditions cannot be part
        # of a replacement. The addition of new post-contexts would mess up the pattern.
        ## if state_machine.has_pre_or_post_context():
        ##    error.log("Pre- or post-conditioned pattern was used in replacement.\n" + \
        ##              "Quex's regular expression grammar does not allow this.", stream)
            
        return state_machine

    else:
        # Get a cloned version of character set
        character_set = reference.get_character_set()
        if character_set is None:
            error.log("Replacement in character set expression must be a character set.\n"
                      "Specifier '%s' relates to a pattern state machine." % pattern_name, stream)

        if character_set.is_empty():
            error.log("Referenced character set '%s' is empty.\nAborted." % pattern_name, stream)

        return character_set

