import quex.input.regular_expression.core         as     regular_expression
from   quex.input.files.parser_data.counter       import ParserDataLineColumn, \
                                                         ParserDataIndentation
from   quex.input.code.base            import SourceRef
import quex.engine.misc.error          as     error
from   quex.engine.misc.file_in        import check, \
                                              check_or_die, \
                                              skip_whitespace, \
                                              read_identifier, \
                                              read_integer

def parse_line_column_counter(fh):
    result = __parse(fh, ParserDataLineColumn(SourceRef.from_FileHandle(fh)))
    result.finalize()
    return result

def parse_indentation(fh):
    result = __parse(fh, ParserDataIndentation(SourceRef.from_FileHandle(fh)), 
                     IndentationSetupF=True)
    result.finalize()
    return result

def __parse_definition_head(fh, result):

    if check(fh, "\\default"): 
        error.log("'\\default' has been replaced by keyword '\\else' since quex 0.64.9!", fh)
    elif check(fh, "\\else"): 
        pattern = None
    else:                      
        pattern = regular_expression.parse(fh)

    skip_whitespace(fh)
    check_or_die(fh, "=>", " after character set definition.")

    skip_whitespace(fh)
    identifier = read_identifier(fh, OnMissingStr="Missing identifier for indentation element definition.")
    error.verify_word_in_list(identifier, result.identifier_list, 
                        "Unrecognized specifier '%s'." % identifier, fh)
    skip_whitespace(fh)

    return pattern, identifier, SourceRef.from_FileHandle(fh)

def __parse(fh, result, IndentationSetupF=False):
    """Parses pattern definitions of the form:
   
          [ \t]                                       => grid 4;
          [:intersection([:alpha:], [\X064-\X066]):]  => space 1;

       In other words the right hand side *must* be a character set.
    """

    # NOTE: Catching of EOF happens in caller: parse_section(...)
    #
    while 1 + 1 == 2:
        skip_whitespace(fh)
        if check(fh, ">"): 
            break
        
        # A regular expression state machine
        pattern, identifier, sr = __parse_definition_head(fh, result)
        if pattern is None and IndentationSetupF:
            error.log("Keyword '\\else' cannot be used in indentation setup.", fh)

        # '__parse_definition_head()' ensures that only identifiers mentioned in 
        # 'result' are accepted. 
        if not IndentationSetupF:
            value = read_value_specifier(fh, identifier, 1)
            result.specify(identifier, pattern, value, sr)
        else:
            result.specify(identifier, pattern, sr)

        if not check(fh, ";"):
            error.log("Missing ';' after '%s' specification." % identifier, fh)

    return result

def read_value_specifier(fh, Keyword, Default=None):
    skip_whitespace(fh)
    value = read_integer(fh)
    if value is not None:     return value

    # not a number received, is it an identifier?
    variable = read_identifier(fh)
    if   variable != "":      return variable
    elif Default is not None: return Default

    error.log("Missing integer or variable name after keyword '%s'." % Keyword, fh) 

