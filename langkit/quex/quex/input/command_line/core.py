import sys
import os
sys.path.insert(0, os.environ["QUEX_PATH"])

from   quex.blackboard                    import setup
from   quex.input.command_line.GetPot     import GetPot
import quex.input.command_line.code_generation as code_generation
import quex.input.command_line.query      as      query     
from   quex.input.setup                   import SETUP_INFO,               \
                                                 SetupParTypes,            \
                                                 NotificationDB
from   quex.input.code.base               import SourceRef
import quex.engine.misc.error             as     error
from   quex.engine.misc.file_operations   import open_file_or_die
from   quex.engine.misc.file_in           import get_integer_parameter_value
                                                 

def do(argv):
    """RETURNS: True,  if CODE GENERATION needs to happen.
                False, if NOTHING remains to be done.
    """        
    global setup
    location_list = __extra_option_extend_argv(argv)

    query_f, command_line = argv_interpret(argv)

    if command_line is None: return False
    elif not query_f:        code_generation.prepare(command_line, argv)
    else:                    query.run(command_line, argv)

    __extra_option_message(location_list)
    return not query_f

def __extra_option_extend_argv(argv):
    """Checks for source files mentioned in the command line. Some may
    contain sections that extend the command line. If so, the command line
    options are parsed and added to 'argv'.

    Details in '__extra_option_extract_from_file()'.
    """
    extra_location_list = []
    try:    
        idx = argv.index("--token-class-file")
        if idx + 1 < len(argv): idx += 1
        else:                   idx  = None
    except: 
        idx = None 

    if idx is None:
        # No file with extra command line options.
        return

    extra_argv, extra_location_list = __extra_option_extract_from_file(argv[idx])
    if extra_argv is None: 
        # No extra option in file. 
        return

    argv.extend(extra_argv)
    return extra_location_list

def __extra_option_extract_from_file(FileName):
    """Extract an option section from a given file. The quex command line 
       options may be given in a section surrounded by '<<<QUEX-OPTIONS>>>'
       markers. For example:

           <<<QUEX-OPTIONS>>>
              --token-class-file      Common-token
              --token-class           Common::Token
              --token-id-type         uint32_t
              --buffer-element-type   uint8_t
              --lexeme-null-object    ::Common::LexemeNullObject
              --foreign-token-id-file Common-token_ids
           <<<QUEX-OPTIONS>>>

       This function extracts those options and builds a new 'argv' array, i.e.
       an array of strings are if they would come from the command line.
    """
    MARKER = "<<<QUEX-OPTIONS>>>"
    fh     = open_file_or_die(FileName)

    while 1 + 1 == 2:
        line = fh.readline()
        if line == "":
            return None, [] # Simply no starting marker has been found
        elif line.find(MARKER) != -1: 
            pos = fh.tell()
            break

    result = []
    location_list = []

    line_n = 0
    while 1 + 1 == 2:
        line_n += 1
        line    = fh.readline()
        if line == "":
            fh.seek(pos)
            error.log("Missing terminating '%s'." % MARKER, fh)

        if line.find(MARKER) != -1: 
            break
        
        idx = line.find("-")
        if idx == -1: continue
        options = line[idx:].split()

        location_list.append((SourceRef(FileName, line_n), options))
        result.extend(options)

    if len(result) == 0: return None, location_list

    return result, location_list

def __extra_option_message(ExtraLocationList):
    if ExtraLocationList is None:
        return
    elif NotificationDB.message_on_extra_options in setup.suppressed_notification_list:
        return
    elif len(ExtraLocationList) == 0:
        return

    sr = ExtraLocationList[0][0]
    error.log("Command line arguments from inside files:", sr, NoteF=True)
    for sr, option in ExtraLocationList:
        if len(option) < 2: option_str = option[0]
        else:               option_str = reduce(lambda x, y: "%s %s" % (x.strip(), y.strip()), option)
        error.log("%s" % option_str, sr, NoteF=True)
    error.log("", sr, NoteF=True, SuppressCode=NotificationDB.message_on_extra_options)

def argv_interpret(argv):
    """RETURNS:
         QueryF -- True, if quex is run in query mode.
                   False, if it is run in code generation mode.
         Setup  -- information about the command line.
    """
    command_line = GetPot(argv, SectionsEnabledF=False)
    query_f      = None
    command_line.disable_loop()
    for variable_name, info in SETUP_INFO.items():

        if type(info) != list:  # Parameter not set on command line?
            continue            # => skip.

        command_line.reset_cursor()
        if not command_line.search(info[0]): continue

        query_f = argv_is_query_option(command_line, info[0], variable_name, query_f)

        if   info[1] == SetupParTypes.FLAG:
            value = argv_catch_flag(command_line, info[0], None)

        elif info[1] == SetupParTypes.NEGATED_FLAG:
            value = argv_catch_negated_flag(command_line, info[0], None)

        elif info[1] == SetupParTypes.INT_LIST:
            value = argv_catch_int_list(command_line, variable_name, info[0], [])

        elif info[1] == SetupParTypes.LIST:
            value = argv_catch_list(command_line, info[0], [])

        elif isinstance(info[1], (int, long)):
            value = argv_catch_int(command_line, info[0], info[1])
        else:
            value = argv_catch_string(command_line, info[0], info[1])

        setup.set(variable_name, info[1], value)

    # Handle unidentified command line options.
    argv_ufo_detections(command_line)

    return query_f, command_line

def argv_is_query_option(Cl, Option, Name, PrevQueryF):
    """Determines whether the setup parameter is a parameter related to 
    queries (or to code generation). If a mixed usage is detected an 
    error is issued.

    RETURN: query flag

    The query flag is the same as QueryF, except for one case: when QueryF
    was None (unset) and the option appeared on the command line. Then, the 
    return value tells whether the option was a query flag or not.

    ERROR: If there are mixed options, i.e. query flags and code generation
    flags appear at the same time.
    """
    query_f = (Name.find("query_") == 0)

    if   PrevQueryF is None:    return query_f
    elif PrevQueryF == query_f: return query_f

    # If debug exception is enabled, do not trigger errror
    if Cl.search(SETUP_INFO["_debug_exception_f"][0]): return query_f

    error.log("Mixed options: query and code generation mode.\n"
              "The option(s) '%s' cannot be combined with preceeding options." \
              % str(SETUP_INFO[Name][0])[1:-1].replace("'",""))

def argv_catch_flag(Cl, Option, Default):
    """RETURNS: True -- if option is found on command line.
                False -- else.
    """
    return Cl.search(Option)

def argv_catch_negated_flag(Cl, Option, Default):
    """RETURNS: True -- if option is NOT found on command line.
                False -- else.
    """
    return not Cl.search(Option)

def argv_catch_int(Cl, Option, Default):
    """RETURNS: Integer for the given variable name.
    """
    return get_integer_parameter_value(str(Option)[1:-1], Cl.follow("%i" % Default, Option))

def argv_catch_int_list(Cl, VariableName, Option, Default):
    """RETURNS: list of integers built from the list of no-minus followers of 
    the given option.
    """
    return [
        get_integer_parameter_value(str(Option)[1:-1], x)
        for x in argv_catch_list(Cl, Option, Default)
    ]

def argv_catch_list(Cl, Option, Default):
    """Catch the list of no-minus followers of the given option. Multiple
    occurrencies of Option are considered.

    RETURNS: list of no-minus followers.
    """
    result = []
    while 1 + 1 == 2:
        if not Cl.search(Option):
            break

        the_list = Cl.nominus_followers(Option)
        if len(the_list) == 0:
            error.log("Option %s\nnot followed by anything." % str(Option)[1:-1])

        for x in the_list:
            if x not in result: result.append(x)
    return result

def argv_catch_string(Cl, Option, Type):
    Cl.reset_cursor()
    value = Cl.follow("##EMPTY##", Option)
    if value == "##EMPTY##":
        if Type == SetupParTypes.OPTIONAL_STRING:
            value = ""
        else:
            error.log("Option %s\nnot followed by anything." % str(Option)[1:-1])
    return value

def argv_ufo_detections(Cl):
    """Detects unidentified command line options.
    """
    known_option_list = []
    for info in SETUP_INFO.itervalues():
        if type(info) != list: continue
        known_option_list.extend(info[0])

    ufo_list = Cl.unidentified_options(known_option_list)
    if not ufo_list: return

    option_str = "".join("%s\n" % ufo_list)
    error.log("Following command line options are unknown to current version of quex:\n" \
              + option_str, 
             SuppressCode=NotificationDB.error_ufo_on_command_line_f)
