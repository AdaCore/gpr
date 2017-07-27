import quex.engine.misc.error            as     error
from   quex.engine.misc.file_in          import EndOfStreamException, \
                                                check, \
                                                check_or_die, \
                                                read_integer, \
                                                read_namespaced_name, \
                                                read_until_closing_bracket, \
                                                skip_whitespace
from   quex.output.cpp.token_id_maker    import TokenInfo 
from   quex.input.files.token_id_file    import cut_token_id_prefix
import quex.blackboard                   as     blackboard
from   quex.blackboard                   import setup as Setup, \
                                                Lng
from   quex.input.code.base   import SourceRef
from   quex.input.setup                  import NotificationDB
import quex.input.regular_expression.snap_backslashed_character as snap_backslashed_character
from   quex.engine.codec_db.unicode.parser     import ucs_property_db
from   quex.engine.misc.utf8                  import __read_one_utf8_code_from_stream
from   quex.input.code.core   import CodeUser 

def parse(fh, CodeFragmentName, 
          ErrorOnFailureF=True, AllowBriefTokenSenderF=True, ContinueF=True):
    """RETURNS: An object of class CodeUser containing
                line number, filename, and the code fragment.

                None in case of failure.
    """
    assert type(ErrorOnFailureF)        == bool
    assert type(AllowBriefTokenSenderF) == bool

    skip_whitespace(fh)

    word = fh.read(2)
    if len(word) >= 1 and word[0] == "{":
        if len(word) > 1: fh.seek(-1, 1) # unput the second character
        return __parse_normal(fh, CodeFragmentName)

    elif AllowBriefTokenSenderF and word == "=>":
        return __parse_brief_token_sender(fh, ContinueF)

    elif not ErrorOnFailureF:
        fh.seek(-2,1)
        return None
    else:
        error.log("Missing code fragment after %s definition." % CodeFragmentName, 
                  fh)

def __parse_normal(fh, code_fragment_name):
    code   = read_until_closing_bracket(fh, "{", "}")
    return CodeUser(code, SourceRef.from_FileHandle(fh))

def __read_token_identifier(fh):
    """Parses a token identifier that may contain a namespace specification.

       Returns "", if no valid specification could be found.
    """
    identifier, name_space_list, dummy = read_namespaced_name(fh, "token identifier")
    if identifier == "": return ""
    if len(name_space_list) == 0: return identifier
    return reduce(lambda x, y: x + "::" + y, name_space_list + [identifier])

def __parse_brief_token_sender(fh, ContinueF):
    # shorthand for { self.send(TKN_SOMETHING); QUEX_SETTING_AFTER_SEND_CONTINUE_OR_RETURN(); }
    
    
    position = fh.tell()
    try: 
        skip_whitespace(fh)
        position = fh.tell()

        code = __parse_token_id_specification_by_character_code(fh)
        if code != -1: 
            code = __create_token_sender_by_character_code(fh, code)
        else:
            skip_whitespace(fh)
            identifier = __read_token_identifier(fh)
            skip_whitespace(fh)
            if identifier in ["GOTO", "GOSUB", "GOUP"]:
                code = __create_mode_transition_and_token_sender(fh, identifier)
            else:
                code = __create_token_sender_by_token_name(fh, identifier)
                check_or_die(fh, ";")

        if len(code) != 0: 
            if ContinueF: code += "QUEX_SETTING_AFTER_SEND_CONTINUE_OR_RETURN();\n"
            return CodeUser(code, SourceRef.from_FileHandle(fh))
        else:
            return None

    except EndOfStreamException:
        fh.seek(position)
        error.error_eof("token", fh)

def read_character_code(fh):
    # NOTE: This function is tested with the regeression test for feature request 2251359.
    #       See directory $QUEX_PATH/TEST/2251359.
    pos = fh.tell()
    
    start = fh.read(1)
    if start == "":  
        fh.seek(pos); return -1

    elif start == "'": 
        # read an utf-8 char an get the token-id
        # Example: '+'
        if check(fh, "\\"):
            # snap_backslashed_character throws an exception if 'backslashed char' is nonsense.
            character_code = snap_backslashed_character.do(fh, ReducedSetOfBackslashedCharactersF=True)
        else:
            character_code = __read_one_utf8_code_from_stream(fh)

        if character_code is None:
            error.log("Missing utf8-character for definition of character code by character.", 
                      fh)

        elif fh.read(1) != '\'':
            error.log("Missing closing ' for definition of character code by character.", 
                      fh)

        return character_code

    if start == "U":
        if fh.read(1) != "C": fh.seek(pos); return -1
        # read Unicode Name 
        # Example: UC MATHEMATICAL_MONOSPACE_DIGIT_FIVE
        skip_whitespace(fh)
        ucs_name = __read_token_identifier(fh)
        if ucs_name == "": fh.seek(pos); return -1
        # Get the character set related to the given name. Note, the size of the set
        # is supposed to be one.
        character_code = ucs_property_db.get_character_set("Name", ucs_name)
        if type(character_code) in [str, unicode]:
            error.verify_word_in_list(ucs_name, ucs_property_db["Name"].code_point_db,
                                      "The string %s\ndoes not identify a known unicode character." % ucs_name, 
                                      fh)
        elif type(character_code) not in [int, long]:
            error.log("%s relates to more than one character in unicode database." % ucs_name, 
                      fh) 
        return character_code

    fh.seek(pos)
    character_code = read_integer(fh)
    if character_code is not None: return character_code

    # Try to interpret it as something else ...
    fh.seek(pos)
    return -1               

def __parse_function_argument_list(fh, ReferenceName):
    argument_list = []
    position = fh.tell()
    try:
        # Read argument list
        if check(fh, "(") == False:
            return []

        text = ""
        while 1 + 1 == 2:
            tmp = fh.read(1)
            if   tmp == ")": 
                break
            elif tmp in ["(", "[", "{"]:
                closing_bracket = {"(": ")", "[": "]", "{": "}"}[tmp]
                text += tmp + read_until_closing_bracket(fh, tmp, closing_bracket) + closing_bracket
            elif tmp == "\"":
                text += tmp + read_until_closing_bracket(fh, "", "\"", IgnoreRegions = []) + "\"" 
            elif tmp == "'":
                text += tmp + read_until_closing_bracket(fh, "", "'", IgnoreRegions = []) + "'" 
            elif tmp == ",":
                argument_list.append(text)
                text = ""
            elif tmp == "":
                fh.seek(position)
                error.error_eof("argument list for %s" % ReferenceName, fh)
            else:
                text += tmp

        if text != "": argument_list.append(text)

        argument_list = map(lambda arg:    arg.strip(), argument_list)
        argument_list = filter(lambda arg: arg != "",   argument_list)
        return argument_list

    except EndOfStreamException:
        fh.seek(position)
        error.error_eof("token", fh)

def __parse_token_id_specification_by_character_code(fh):
    character_code = read_character_code(fh)
    if character_code == -1: return -1
    check_or_die(fh, ";")
    return character_code

def __create_token_sender_by_character_code(fh, CharacterCode):
    # The '--' will prevent the token name from being printed
    prefix_less_token_name = "UCS_0x%06X" % CharacterCode
    token_id_str           = "0x%06X" % CharacterCode 
    blackboard.token_id_db["--" + prefix_less_token_name] = \
            TokenInfo(prefix_less_token_name, CharacterCode, None, 
                      SourceRef.from_FileHandle(fh)) 
    return "self_send(%s);\n" % token_id_str

def token_id_db_verify_or_enter_token_id(fh, TokenName):
    global Setup

    prefix_less_TokenName = cut_token_id_prefix(TokenName, fh)

    # Occasionally add token id automatically to database
    if not blackboard.token_id_db.has_key(prefix_less_TokenName):
        # DO NOT ENFORCE THE TOKEN ID TO BE DEFINED, BECAUSE WHEN THE TOKEN ID
        # IS DEFINED IN C-CODE, THE IDENTIFICATION IS NOT 100% SAFE.
        if TokenName in blackboard.token_id_db.keys():
            msg  = "Token id '%s' defined implicitly.\n" % TokenName
            msg += "'%s' has been defined in a token { ... } section!\n" % \
                   (Setup.token_id_prefix + TokenName)
            msg += "Token ids in the token { ... } section are automatically prefixed."
            error.warning(msg, fh, 
                          SuppressCode=NotificationDB.warning_usage_of_undefined_token_id_name)
        else:
            # Warning is posted later when all implicit tokens have been
            # collected. See "token_id_maker.__propose_implicit_token_definitions()"
            blackboard.token_id_implicit_list.append((prefix_less_TokenName, 
                                                      SourceRef.from_FileHandle(fh)))

        # Enter the implicit token id definition in the database
        blackboard.token_id_db[prefix_less_TokenName] = \
                TokenInfo(prefix_less_TokenName, None, None, 
                          SourceRef.from_FileHandle(fh)) 

def __create_token_sender_by_token_name(fh, TokenName):
    assert type(TokenName) in [str, unicode]

    # Enter token_id into database, if it is not yet defined.
    token_id_db_verify_or_enter_token_id(fh, TokenName)

    # Parse the token argument list
    argument_list = __parse_function_argument_list(fh, TokenName)

    # Create the token sender
    explicit_member_names_f = False
    for arg in argument_list:
        if arg.find("=") != -1: explicit_member_names_f = True

    assert blackboard.token_type_definition is not None, \
           "A valid token_type_definition must have been parsed at this point."

    if not explicit_member_names_f:
        # There are only two allowed cases for implicit token member names:
        #  QUEX_TKN_XYZ(Lexeme)     --> call take_text(Lexeme, LexemeEnd)
        #  QUEX_TKN_XYZ(Begin, End) --> call to take_text(Begin, End)
        if   len(argument_list) == 2:
            return "QUEX_NAME_TOKEN(take_text)(self_write_token_p(), &self, (%s), (%s));\n" % \
                   (argument_list[0], argument_list[1]) + \
                   "self_send(%s);\n" % (TokenName)

        elif len(argument_list) == 1:
            if argument_list[0] == "Lexeme":
                return "QUEX_NAME_TOKEN(take_text)(self_write_token_p(), &self, self.buffer._lexeme_start_p, self.buffer._input_p);\n" \
                       "self_send(%s);\n" % (TokenName)
            elif argument_list[0] == "LexemeNull":
                return "QUEX_NAME_TOKEN(take_text)(self_write_token_p(), &self, LexemeNull, LexemeNull);\n" \
                       "self_send(%s);\n" % (TokenName)
            else:
                error.log("If one unnamed argument is specified it must be 'Lexeme'\n"          + \
                          "or 'LexemeNull'. Found '%s'.\n" % argument_list[0]                     + \
                          "To cut parts of the lexeme, please, use the 2 argument sender, e.g.\n" + \
                          "QUEX_TKN_MY_ID(Lexeme + 1, LexemeEnd - 2);\n"                             + \
                          "Alternatively, use named parameters such as 'number=...'.", 
                          fh)

        elif len(argument_list) == 0:
            return "self_send(%s);\n" % TokenName

        else:
            error.log("Since 0.49.1, there are only the following brief token senders that can take\n"
                      "unnamed token arguments:\n"
                      "     one argument:   'Lexeme'   =>  token.take_text(..., LexemeBegin, LexemeEnd);\n"
                      "     two arguments:  Begin, End =>  token.take_text(..., Begin, End);\n"
                      + "Found: " + repr(argument_list)[1:-1] + ".", fh)

        # Returned from Function if implicit member names

    member_value_pairs = map(lambda x: x.split("="), argument_list)
    txt = ""
    for member, value in member_value_pairs:
        if value == "":
            error.log("One explicit argument name mentioned requires all arguments to\n"  + \
                      "be mentioned explicitly. Value '%s' mentioned without argument.\n"   \
                      % member, fh)

        if Setup.token_class_file != "":
            error.log("Member assignments in brief token senders are inadmissible\n" + \
                      "with manually written token classes. User provided file '%s'.\n" % Setup.token_class_file + \
                      "Found member assignment: '%s' = '%s'." % (member, value), fh)
        else:
            member_name = member.strip()
            error.verify_word_in_list(member_name, blackboard.token_type_definition.get_member_db(), 
                                      "No member:   '%s' in token type description." % member_name, fh)
            idx = value.find("Lexeme")
            if idx != -1:
                if idx != 0 and value[idx-1] == "(":
                    pass
                else:
                    error.log("Assignment of token member '%s' with 'Lexeme' directly being involved. The\n" % member_name + 
                              "'Lexeme' points into the text buffer and it is not owned by the token object.\n"
                              "\n"
                              "Proposals:\n\n"
                              "   (1) Use '(Lexeme)', i.e. surround 'Lexeme' by brackets to indicate\n"
                              "       that you are aware of the danger. Do this, if at the end of the\n"
                              "       process, the member can be assumed to relate to an object that\n"
                              "       is not directly dependent anymore on 'Lexeme'. This is particularly\n"
                              "       true if the member is of type 'std::string'. Its constructor\n"
                              "       creates a copy of the zero terminated string.\n\n"
                              "   (2) Use token senders without named arguments, for example\n"
                              "          \"%s(Lexeme+1, LexemeEnd-2)\"\n" % TokenName + 
                              "          \"%s(Lexeme)\"\n" % TokenName + 
                              "       These token senders create a copy of the lexeme and let the token\n"
                              "       own it.", fh)

            access = blackboard.token_type_definition.get_member_access(member_name)
            txt += "self_write_token_p()->%s = %s;\n" % (access, value.strip())


    # Box the token, stamp it with an id and 'send' it
    txt += "self_send(%s);\n" % TokenName
    return txt

def __create_mode_transition_and_token_sender(fh, Op):
    assert Op in ["GOTO", "GOSUB", "GOUP"]

    position     = fh.tell()
    target_mode  = ""
    token_sender = ""
    if check(fh, "("):
        skip_whitespace(fh)
        if Op != "GOUP":
            target_mode = __read_token_identifier(fh)
            skip_whitespace(fh)

        if check(fh, ")"):
            token_sender = ""

        elif Op == "GOUP" or check(fh, ","):
            skip_whitespace(fh)
            token_name = __read_token_identifier(fh)
            skip_whitespace(fh)

            if check(fh, ","):
                error.log("Missing opening '(' after token name specification.\n" 
                          "Note, that since version 0.50.1 the syntax for token senders\n"
                          "inside brief mode transitions is like:\n\n"
                          "     => GOTO(MYMODE, QUEX_TKN_MINE(Argument0, Argument1, ...));\n", 
                          fh)

            token_sender = __create_token_sender_by_token_name(fh, token_name) 

            if check(fh, ")") == False:
                error.log("Missing closing ')' or ',' after '%s'." % Op, 
                          fh)

        else:
            fh.seek(position)
            error.log("Missing closing ')' or ',' after '%s'." % Op, fh)

    if check(fh, ";") == False:
        error.log("Missing ')' or ';' after '%s'." % Op, fh)

    if Op in ["GOTO", "GOSUB"] and target_mode == "": 
        error.log("Op %s requires at least one argument: The target mode." % Op, 
                  fh)

    # Code for mode change
    if   Op == "GOTO":  txt = Lng.MODE_GOTO(target_mode)
    elif Op == "GOSUB": txt = Lng.MODE_GOSUB(target_mode)
    else:                    txt = Lng.MODE_GOUP()

    # Code for token sending
    txt += token_sender

    return txt

