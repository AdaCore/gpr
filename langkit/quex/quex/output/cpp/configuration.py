from   quex.engine.misc.file_operations import get_file_content_or_die
from   quex.engine.misc.file_in         import get_include_guard_extension, \
                                               make_safe_identifier

from   quex.engine.misc.string_handling import blue_print
from   quex.blackboard  import setup as Setup, E_IncidenceIDs, Lng
import quex.blackboard  as blackboard
from   quex.DEFINITIONS import QUEX_PATH, QUEX_VERSION
import time

def do(ModeDescriptionDB):
    IndentationSupportF = blackboard.required_support_indentation_count()
    BeginOfLineSupportF = blackboard.required_support_begin_of_line()

    

    LexerClassName = Setup.analyzer_class_name

    ConfigurationTemplateFile =(  QUEX_PATH \
                                + Lng["$code_base"] \
                                + "/analyzer/configuration/TXT").replace("//","/")

    txt = get_file_content_or_die(ConfigurationTemplateFile)

    # -- check if exit/entry handlers have to be active
    entry_handler_active_f = False
    exit_handler_active_f = False
    for mode in ModeDescriptionDB.values():
        entry_handler_active_f |= mode.incidence_db.has_key(E_IncidenceIDs.MODE_ENTRY)
        exit_handler_active_f  |= mode.incidence_db.has_key(E_IncidenceIDs.MODE_EXIT)

    # Buffer filler converter (0x0 means: no buffer filler converter)
    converter_new_str = "#   define QUEX_SETTING_BUFFER_FILLERS_CONVERTER_NEW " 
    if Setup.converter_user_new_func != "": 
        converter_new_str += Setup.converter_user_new_func + "()"
    else: 
        converter_new_str = "/* " + converter_new_str + " */"

    # Token repetition support
    token_repeat_test_txt = ""
    for token_id_str in blackboard.token_repetition_token_id_list:
        token_repeat_test_txt += "TokenID == %s || " % token_id_str
    if token_repeat_test_txt != "":
        token_repeat_test_txt = token_repeat_test_txt[:-3]
    else:
        token_repeat_test_txt = "false"

    if Setup.analyzer_derived_class_name != "":
        analyzer_derived_class_name = Setup.analyzer_derived_class_name
    else:
        analyzer_derived_class_name = Setup.analyzer_class_name

    txt = __switch(txt, "QUEX_OPTION_COLUMN_NUMBER_COUNTING",        Setup.count_column_number_f)        
    txt = __switch(txt, "QUEX_OPTION_COMPUTED_GOTOS",                False)
    txt = __switch(txt, "QUEX_OPTION_CONVERTER_ICONV",               Setup.converter_iconv_f)
    txt = __switch(txt, "QUEX_OPTION_CONVERTER_ICU",                 Setup.converter_icu_f)
    txt = __switch(txt, "QUEX_OPTION_INCLUDE_STACK",                 Setup.include_stack_support_f)
    txt = __switch(txt, "QUEX_OPTION_LINE_NUMBER_COUNTING",          Setup.count_line_number_f)      
    txt = __switch(txt, "QUEX_OPTION_POST_CATEGORIZER",              Setup.post_categorizer_f)
    txt = __switch(txt, "QUEX_OPTION_RUNTIME_MODE_TRANSITION_CHECK", Setup.mode_transition_check_f)
    txt = __switch(txt, "QUEX_OPTION_STRING_ACCUMULATOR",            Setup.string_accumulator_f)
    txt = __switch(txt, "QUEX_OPTION_TOKEN_POLICY_QUEUE",            Setup.token_policy == "queue")
    txt = __switch(txt, "QUEX_OPTION_TOKEN_POLICY_SINGLE",           Setup.token_policy == "single")
    txt = __switch(txt, "QUEX_OPTION_TOKEN_REPETITION_SUPPORT",      token_repeat_test_txt != "false")
    txt = __switch(txt, "QUEX_OPTION_USER_MANAGED_TOKEN_MEMORY",     Setup.token_memory_management_by_user_f)
    txt = __switch(txt, "__QUEX_OPTION_BIG_ENDIAN",                  Setup.buffer_byte_order == "big")
    txt = __switch(txt, "__QUEX_OPTION_CONVERTER_HELPER",            Setup.converter_helper_required_f)
    txt = __switch(txt, "__QUEX_OPTION_CONVERTER",                   Setup.converter_f)
    txt = __switch(txt, "QUEX_OPTION_INDENTATION_TRIGGER",           IndentationSupportF)     
    txt = __switch(txt, "__QUEX_OPTION_LITTLE_ENDIAN",               Setup.buffer_byte_order == "little")
    txt = __switch(txt, "__QUEX_OPTION_ON_ENTRY_HANDLER_PRESENT",    entry_handler_active_f)
    txt = __switch(txt, "__QUEX_OPTION_ON_EXIT_HANDLER_PRESENT",     exit_handler_active_f)
    txt = __switch(txt, "__QUEX_OPTION_PLAIN_C",                     Setup.language.upper() == "C")
    txt = __switch(txt, "__QUEX_OPTION_SUPPORT_BEGIN_OF_LINE_PRE_CONDITION", BeginOfLineSupportF)
    txt = __switch(txt, "__QUEX_OPTION_SYSTEM_ENDIAN",               Setup.byte_order_is_that_of_current_system_f)
    txt = __switch(txt, "QUEX_OPTION_BUFFER_BASED_ANALYZIS",         Setup.buffer_based_analyzis_f)
    txt = __switch(txt, "__QUEX_OPTION_ENGINE_RUNNING_ON_CODEC",     Setup.buffer_codec.name != "unicode")

    # -- token class related definitions
    token_descr = blackboard.token_type_definition

    # -- name of the character codec
    codec_name = make_safe_identifier(Setup.buffer_codec.name).lower()

    # Setup.buffer_element_size can be '-1'. This signals then that 
    # sizeof(QUEX_TYPE_CHARACTER) needs to be used. A numeric value 
    # is required here.
    character_size_str = "%i" % Setup.buffer_element_size

    def namespace(NameSpaceList):
        result = Lng.NAMESPACE_REFERENCE(NameSpaceList, TrailingDelimiterF=False)

        if len(result) == 0: return ""

        assert Setup.language.upper() != "C++" or len(result) > 2, \
               "Error while generating namespace reference '%s'" % result

        return result

    txt = blue_print(txt, 
            [
             ["$$BUFFER_LIMIT_CODE$$",          "0x%X" % Setup.buffer_limit_code],
             ["$$QUEX_SETTING_CHARACTER_CODEC$$", codec_name],
             ["$$INCLUDE_GUARD_EXTENSION$$",    get_include_guard_extension(Lng.NAMESPACE_REFERENCE(Setup.analyzer_name_space) + "__" + Setup.analyzer_class_name)],
             ["$$INITIAL_LEXER_MODE_ID$$",      "QUEX_NAME(ModeID_%s)" % blackboard.initial_mode.get_pure_text()],
             ["$$LEXER_BUILD_DATE$$",           time.asctime()],
             ["$$LEXER_CLASS_NAME$$",           LexerClassName],
             ["$$LEXER_CLASS_NAME_SAFE$$",      Setup.analyzer_name_safe],
             ["$$LEXER_DERIVED_CLASS_NAME$$",   analyzer_derived_class_name],
             ["$$MAX_MODE_CLASS_N$$",           repr(len(ModeDescriptionDB))],
             ["$$NAMESPACE_MAIN$$",             namespace(Setup.analyzer_name_space)],
             ["$$NAMESPACE_MAIN_CLOSE$$",       Lng.NAMESPACE_CLOSE(Setup.analyzer_name_space).replace("\n", "\\\n")],
             ["$$NAMESPACE_MAIN_OPEN$$",        Lng.NAMESPACE_OPEN(Setup.analyzer_name_space).replace("\n", "\\\n")],
             ["$$NAMESPACE_TOKEN$$",            namespace(token_descr.name_space)],
             ["$$NAMESPACE_TOKEN_CLOSE$$",      Lng.NAMESPACE_CLOSE(token_descr.name_space).replace("\n", "\\\n")],
             ["$$NAMESPACE_TOKEN_OPEN$$",       Lng.NAMESPACE_OPEN(token_descr.name_space).replace("\n", "\\\n")],
             ["$$PATH_TERMINATION_CODE$$",      "0x%X" % Setup.path_limit_code],
             ["$$QUEX_SETTING_BUFFER_FILLERS_CONVERTER_NEW$$", converter_new_str],
             ["$$QUEX_TYPE_CHARACTER$$",        Setup.buffer_element_type],
             ["$$QUEX_SETTING_CHARACTER_SIZE$$", character_size_str],
             ["$$QUEX_NAMESPACE_LEXEME_NULL_OPEN$$",   Lng.NAMESPACE_OPEN(Setup.lexeme_null_namespace).replace("\n", "\\\n")],
             ["$$QUEX_NAMESPACE_LEXEME_NULL_CLOSE$$",  Lng.NAMESPACE_CLOSE(Setup.lexeme_null_namespace).replace("\n", "\\\n")],
             ["$$QUEX_LEXEME_NULL$$",                  Setup.lexeme_null_full_name_cpp],
             ["$$QUEX_LEXEME_NULL_SAFE$$",             Setup.lexeme_null_name_safe],
             ["$$QUEX_LEXEME_NULL_IN_ITS_NAMESPACE$$", Setup.lexeme_null_name],
             ["$$QUEX_VERSION$$",               QUEX_VERSION],
             ["$$TOKEN_CLASS$$",                token_descr.class_name],
             ["$$TOKEN_CLASS_NAME_SAFE$$",      token_descr.class_name_safe],
             ["$$TOKEN_COLUMN_N_TYPE$$",        token_descr.column_number_type.get_pure_text()],
             ["$$TOKEN_ID_TYPE$$",              token_descr.token_id_type.get_pure_text()],
             ["$$TOKEN_LINE_N_TYPE$$",          token_descr.line_number_type.get_pure_text()],
             ["$$TOKEN_PREFIX$$",               Setup.token_id_prefix],
             ["$$TOKEN_QUEUE_SAFETY_BORDER$$",  repr(Setup.token_queue_safety_border)],
             ["$$TOKEN_QUEUE_SIZE$$",           repr(Setup.token_queue_size)],
             ["$$TOKEN_REPEAT_TEST$$",          token_repeat_test_txt],
             ["$$USER_LEXER_VERSION$$",         Setup.user_application_version_id],
             ])

    return txt

def __switch(txt, Name, SwitchF):
    if SwitchF: txt = txt.replace("$$SWITCH$$ %s" % Name, "#define    %s" % Name)
    else:       txt = txt.replace("$$SWITCH$$ %s" % Name, "/* #define %s */" % Name)
    return txt
