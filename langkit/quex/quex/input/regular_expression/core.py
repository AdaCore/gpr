import quex.engine.misc.error               as     error
from   quex.engine.misc.file_in             import EndOfStreamException 
from   quex.engine.state_machine.core       import StateMachine 
from   quex.input.regular_expression.exception                       import RegularExpressionException
import quex.blackboard                      as     blackboard
import quex.input.regular_expression.engine as     regex
from   StringIO import StringIO

def parse(Txt_or_File, AllowNothingIsFineF=False):

    pattern, dummy = __parse(Txt_or_File, AllowNothingIsFineF=AllowNothingIsFineF)

    return pattern

def parse_character_string(Txt_or_File, Terminator=None):
    return __parse(Txt_or_File, StateMachine.get_number_sequence, "character string", Terminator)

def parse_character_set(Txt_or_File, Terminator=None):
    return __parse(Txt_or_File, StateMachine.get_number_set, "character set", Terminator)

def __parse(Txt_or_File, ExtractFunction=None, Name=None, Terminator=None, 
            AllowNothingIsFineF=False):

    if Txt_or_File.__class__ in [file, StringIO]:
        sh = Txt_or_File
    else:
        sh = StringIO(Txt_or_File)

    # (*) Parse the pattern => A Pattern object
    start_position = sh.tell()
    try:
        pattern = regex.do(sh, blackboard.shorthand_db, 
                           AllowNothingIsNecessaryF = AllowNothingIsFineF,
                           SpecialTerminator        = Terminator)

    except RegularExpressionException, x:
        sh.seek(start_position)
        error.log("Regular expression parsing:\n" + x.message, sh)

    except EndOfStreamException:
        sh.seek(start_position)
        error.error_eof("regular expression", sh)

    # (*) Extract the object as required 
    if ExtractFunction is not None:
        result = ExtractFunction(pattern.sm)

        if pattern.has_pre_or_post_context() or result is None:
            sh.seek(start_position)
            pattern_str = pattern.pattern_string().strip()
            txt = "Regular expression '%s' cannot be interpreted as plain %s." % (pattern_str, Name) 
            if len(pattern_str) != 0 and pattern_str[-1] == Terminator:
                txt += "\nMissing delimiting whitespace ' ' between the regular expression and '%s'.\n" % Terminator
            error.log(txt, sh)
    else:
        result = None

    return pattern, result

