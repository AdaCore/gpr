#! /usr/bin/env python
# Quex is  free software;  you can  redistribute it and/or  modify it  under the
# terms  of the  GNU Lesser  General  Public License  as published  by the  Free
# Software Foundation;  either version 2.1 of  the License, or  (at your option)
# any later version.
# 
# This software is  distributed in the hope that it will  be useful, but WITHOUT
# ANY WARRANTY; without even the  implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the  GNU Lesser General Public License for more
# details.
# 
# You should have received a copy of the GNU Lesser General Public License along
# with this  library; if not,  write to the  Free Software Foundation,  Inc., 59
# Temple Place, Suite 330, Boston, MA 02111-1307 USA
#
################################################################################
#
import quex.engine.misc.error              as     error
from   quex.engine.misc.file_operations    import open_file_or_die
from   quex.engine.misc.file_in            import EndOfStreamException, \
                                                  check, \
                                                  os, \
                                                  parse_identifier_assignment, \
                                                  read_identifier, \
                                                  read_integer, \
                                                  skip_whitespace
import quex.input.files.mode               as mode
import quex.input.files.token_type         as token_type
import quex.input.files.code_fragment      as code_fragment
import quex.input.regular_expression.core  as regular_expression
from   quex.input.regular_expression.auxiliary  import PatternShorthand
from   quex.input.setup                    import NotificationDB
from   quex.output.cpp.token_id_maker      import TokenInfo, prepare_default_standard_token_ids
from   quex.input.code.base     import SourceRef
from   quex.input.code.core     import CodeUser
from   quex.blackboard                     import setup as Setup, Lng
import quex.blackboard                     as     blackboard
from   quex.input.regular_expression.exception                      import RegularExpressionException

def do(file_list):
    if len(file_list) == 0 and not Setup.token_class_only_f: 
        error.log("No input files.")

    # If a foreign token-id file was presented even the standard token ids
    # must be defined there.
    if not Setup.token_id_foreign_definition:
        prepare_default_standard_token_ids()

    for file in file_list:
        fh = open_file_or_die(file, CodecCheckF=True)

        # read all modes until end of file
        try:
            while 1 + 1 == 2:
                parse_section(fh)
        except EndOfStreamException:
            pass
        except RegularExpressionException, x:
            error.log(x.message, fh)
        
    if blackboard.token_type_definition is None:
        parse_default_token_definition()

    return blackboard.mode_description_db

default_token_type_definition_triggered_by_mode_definition_f = False

def parse_section(fh):
    global default_token_type_definition_triggered_by_mode_definition_f

    # NOTE: End of File is supposed to be reached when trying to read a new
    #       section. Thus, the end-of-file catcher does not encompass the beginning.
    position = fh.tell()
    skip_whitespace(fh)
    word = read_identifier(fh, OnMissingStr="Missing section title")

    error.verify_word_in_list(word, blackboard.all_section_title_list, 
                        "Unknown quex section '%s'" % word, fh)
    try:
        # (*) determine what is defined
        #
        #     -- 'mode { ... }'     => define a mode
        #     -- 'start = ...;'     => define the name of the initial mode
        #     -- 'header { ... }'   => define code that is to be pasted on top
        #                              of the engine (e.g. "#include<...>")
        #     -- 'body { ... }'     => define code that is to be pasted in the class' body
        #                              of the engine (e.g. "public: int  my_member;")
        #     -- 'init { ... }'     => define code that is to be pasted in the class' constructors
        #                              of the engine (e.g. "my_member = -1;")
        #     -- 'define { ... }'   => define patterns shorthands such as IDENTIFIER for [a-z]+
        #     -- 'repeated_token_id = QUEX_TKN_ ...;' => enables token repetition, defines
        #                                                the token id to be repeated.
        #     -- 'token { ... }'    => define token ids
        #     -- 'token_type { ... }'  => define a customized token type
        #
        if word in blackboard.fragment_db.keys():
            element_name = blackboard.fragment_db[word]
            fragment     = code_fragment.parse(fh, word, AllowBriefTokenSenderF=False)        
            blackboard.__dict__[element_name] = fragment
            return

        elif word == "start":
            mode_name = parse_identifier_assignment(fh)
            if mode_name == "":
                error.log("Missing mode_name after 'start ='", fh)

            elif not blackboard.initial_mode.sr.is_void():
                error.log("start mode defined more than once!", fh, DontExitF=True)
                error.log("previously defined here", blackboard.initial_mode.sr)
             
            blackboard.initial_mode = CodeUser(mode_name, SourceRef.from_FileHandle(fh))
            return

        elif word == "repeated_token":
            blackboard.token_repetition_token_id_list = parse_token_id_definitions(fh, NamesOnlyF=True)
            for token_name in blackboard.token_repetition_token_id_list:
                error.verify_word_in_list(token_name[len(Setup.token_id_prefix):],
                                    blackboard.token_id_db.keys(),
                                    "Token ID '%s' not yet defined." % token_name, fh,
                                    ExitF=False, 
                                    SuppressCode=NotificationDB.warning_repeated_token_not_yet_defined)
            return
            
        elif word == "define":
            parse_pattern_name_definitions(fh)
            return

        elif word == "token":       
            if Setup.token_id_foreign_definition:
                error.log("Token id file '%s' has been specified.\n" \
                          % Setup.token_id_foreign_definition_file \
                          + "All token ids must be specified there. Section 'token'\n" \
                          + "is not allowed.", fh)

            parse_token_id_definitions(fh)
            return

        elif word == "token_type":       

            if Setup.token_class_file != "":
                error.log("Section 'token_type' is intended to generate a token class.\n" \
                          + "However, the manually written token class file '%s'" \
                          % repr(Setup.token_class_file) \
                          + "has been specified on the command line.", 
                          fh)
       
            if blackboard.token_type_definition is None:
                blackboard.token_type_definition = token_type.parse(fh)
                return

            # Error case:
            if default_token_type_definition_triggered_by_mode_definition_f:
                error.log("Section 'token_type' must appear before first mode definition.", 
                          fh)
            else:
                error.log("Section 'token_type' has been defined twice.", 
                          fh, DontExitF=True)
                error.log("Previously defined here.",
                          blackboard.token_type_definition.sr)
            return

        elif word == "mode":
            # When the first mode is parsed then a token_type definition must be 
            # present. If not, the default token type definition is considered.
            if blackboard.token_type_definition is None:
                parse_default_token_definition()
                default_token_type_definition_triggered_by_mode_definition_f = True

            mode.parse(fh)
            return

        else:
            # This case should have been caught by the 'verify_word_in_list' function
            assert False

    except EndOfStreamException:
        fh.seek(position)
        error.error_eof(word, fh)

def parse_pattern_name_definitions(fh):
    """Parses pattern definitions of the form:
   
          WHITESPACE  [ \t\n]
          IDENTIFIER  [a-zA-Z0-9]+
          OP_PLUS     "+"
          
       That means: 'name' whitespace 'regular expression' whitespace newline.
       Comments can only be '//' nothing else and they have to appear at the
       beginning of the line.
       
       One regular expression can have more than one name, but one name can 
       only have one regular expression.
    """
    skip_whitespace(fh)
    if not check(fh, "{"):
        error.log("define region must start with opening '{'.", fh)

    while 1 + 1 == 2:
        skip_whitespace(fh)

        if check(fh, "}"): 
            return
        
        # -- get the name of the pattern
        skip_whitespace(fh)
        pattern_name = read_identifier(fh, OnMissingStr="Missing identifier for pattern definition.")

        if blackboard.shorthand_db.has_key(pattern_name):
            error.log("Second definition of pattern '%s'.\n" % pattern_name + \
                      "Pattern names must be unique.", fh)

        skip_whitespace(fh)

        if check(fh, "}"): 
            error.log("Missing regular expression for pattern definition '%s'." % \
                      pattern_name, fh)

        # A regular expression state machine
        # (No possible transformation into a particular codec whatever.
        #  the state machines are transformed once, after they are expanded
        #  as patterns in a mode.)
        pattern = regular_expression.parse(fh, AllowNothingIsFineF = True) 

        if pattern.has_pre_or_post_context():
            error.log("Pattern definition with pre- and/or post-context.\n" + \
                      "Pre- and Post-Contexts can only be defined inside mode definitions.", 
                      fh)
        state_machine = pattern.sm

        blackboard.shorthand_db[pattern_name] = \
                PatternShorthand(pattern_name, state_machine, 
                                 SourceRef.from_FileHandle(fh), pattern.pattern_string())

def parse_token_id_definitions(fh, NamesOnlyF=False):
    """NamesOnlyF == True: Allow only definition of names, no numeric values 
                           may be assigned to it.

       'NamesOnlyF' indicates that data is not written to the global 
       'token_id_db'. Then only a list of names is returned.
    """
    # NOTE: Catching of EOF happens in caller: parse_section(...)
    #
    prefix       = Setup.token_id_prefix
    prefix_plain = Setup.token_id_prefix_plain # i.e. without name space included

    if NamesOnlyF: 
        result = set()

    skip_whitespace(fh)
    if not check(fh, "{"):
        error.log("Missing opening '{' for after 'token' section identifier.", 
                  fh)

    while check(fh, "}") == False:
        skip_whitespace(fh)

        candidate = read_identifier(fh, TolerantF=True, OnMissingStr="Missing valid token identifier.")

        # -- check the name, if it starts with the token prefix paste a warning
        suspicious_prefix = None
        if len(prefix) != 0 and candidate.find(prefix) == 0:       
            suspicious_prefix = prefix
        elif len(prefix_plain) != 0 and candidate.find(prefix_plain) == 0: 
            suspicious_prefix = prefix_plain

        if suspicious_prefix is not None:
            error.warning("Token identifier '%s' starts with token prefix '%s'.\n" \
                      % (candidate, suspicious_prefix) \
                      + "Token prefix is mounted automatically. This token id appears in the source\n" \
                      + "code as '%s%s'." \
                      % (prefix, candidate), \
                      fh, 
                      SuppressCode=NotificationDB.warning_token_id_prefix_appears_in_token_id_name)

        skip_whitespace(fh)

        if NamesOnlyF:
            result.add(prefix + candidate)
            if check(fh, ";") == False:
                error.log("Missing ';' after token identifier '%s'.\n" \
                          % candidate, fh)
            continue

        # Parse a possible numeric value after '='
        numeric_value = None
        if check(fh, "="):
            skip_whitespace(fh)
            numeric_value = read_integer(fh)
            if numeric_value is None:
                error.log("Missing number after '=' for token identifier '%s'." % candidate, 
                          fh)

        if check(fh, ";") == False:
            error.log("Missing ';' after token identifier '%s'." % candidate, 
                      fh)

        if not NamesOnlyF:
            ti = TokenInfo(candidate, numeric_value, 
                           SourceReference=SourceRef.from_FileHandle(fh))
            blackboard.token_id_db[candidate] = ti

    if NamesOnlyF:
        return sorted(list(result))
    else:
        return # Changes are applied to 'blackboard.token_id_db'

def parse_default_token_definition():
    sub_fh = open_file_or_die(os.environ["QUEX_PATH"] 
                              + Lng["$code_base"] 
                              + Lng["$token-default-file"])
    parse_section(sub_fh)
    sub_fh.close()
