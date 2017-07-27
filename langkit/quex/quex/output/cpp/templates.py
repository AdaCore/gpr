from   quex.engine.analyzer.door_id_address_label import DoorID, \
                                                         dial_db
from   quex.engine.misc.interval_handling              import NumberSet
from   operator import itemgetter
#

def __nice(SM_ID): 
    return repr(SM_ID).replace("L", "")

#________________________________________________________________________________
# C++
#

def _local_variable_definitions(VariableDB):
    if len(VariableDB) == 0: return ""

    def __group_by_condition(VariableDB):
        """Groups variables by their conditional compilation macro.

        RETURNS: A map to a pair of lists:
            
                 macro name ---> (ifdef_list, ifndef_list) 

        The 'ifdef_list' is the list of variables which is defined IF the macro
        is defined. The variables in 'ifndef_list' are defined if the macro is 
        NOT defined.
        """
        result = {}
        for variable in VariableDB.itervalues():
            variable_list = result.get(variable.condition)
            if variable_list is None: 
                variable_list              = [[], []]
                result[variable.condition] = variable_list

            if not variable.condition_negated_f: variable_list[0].append(variable)
            else:                                variable_list[1].append(variable)

        return result

    def __code(txt, variable):
        variable_type = variable.variable_type
        variable_init = variable.initial_value
        variable_name = variable.name

        if variable.element_n is not None: 
            if variable.element_n != 0:
                variable_name += "[%s]" % repr(variable.element_n)
                if variable_type.find("QUEX_TYPE_GOTO_LABEL") != -1: 
                    variable_name = "(" + variable_name + ")"
            else:
                variable_type += "*"
                variable_init  = ["0x0"]

        if variable_init is None: 
            value = ["/* un-initilized */"]
        else:
            if type(variable_init) != list: variable_init = [ variable_init ]
            value = [" = "] + variable_init

        txt.append("    %s%s %s%s" % \
                   (variable_type, 
                    " " * (30-len(variable_type)), variable_name, 
                    " " * (30 - len(variable_name))))
        txt.extend(value)
        txt.append(";\n")

    # L   = max(map(lambda info: len(info[0]), VariableDB.keys()))
    txt       = []
    for raw_name, variable in sorted(VariableDB.items()):
        if variable.priority_f == False: continue

        if variable.condition is not None:
            if variable.condition_negated_f == False: 
                txt.append("#   ifdef %s\n"  % variable.condition)
            else:
                txt.append("#   ifndef %s\n" %  variable.condition)

        __code(txt, variable)

        if variable.condition is not None:
            txt.append("#   endif /* %s */\n" % variable.condition)

        del VariableDB[variable.name]

    grouped_variable_list = __group_by_condition(VariableDB)
    unconditioned_name_set = set([])
    for condition, groups in sorted(grouped_variable_list.iteritems()):
        if condition is not None: continue
        for variable in groups[0]:
            unconditioned_name_set.add(variable.name)

    for condition, groups in sorted(grouped_variable_list.iteritems()):

        condition_group, negated_condition_group = groups

        if condition is None:
            for variable in condition_group:
                __code(txt, variable)
        else:
            if len(condition_group) != 0:
                txt.append("#   ifdef %s\n"  % condition)

                for variable in condition_group:
                    if variable.name in unconditioned_name_set: continue
                    __code(txt, variable)

            if len(negated_condition_group) != 0:
                if len(condition_group) != 0: txt.append("#   else /* not %s */\n" % condition)
                else:                         txt.append("#   ifndef %s\n"         % condition)

                for variable in negated_condition_group:
                    if variable.name in unconditioned_name_set: continue
                    __code(txt, variable)

            txt.append("#   endif /* %s */\n" % condition)
            
    return "".join(txt)
         
_terminal_state_prolog  = """
    /* (*) Terminal states _______________________________________________________
     *
     * States that implement actions of the 'winner patterns.                     */
"""

__function_signature = """
__QUEX_TYPE_ANALYZER_RETURN_VALUE  
QUEX_NAME($$STATE_MACHINE_NAME$$_analyzer_function)(QUEX_TYPE_ANALYZER* me) 
{
    /* NOTE: Different modes correspond to different analyzer functions. The 
     *       analyzer functions are all located inside the main class as static
     *       functions. That means, they are something like 'globals'. They 
     *       receive a pointer to the lexical analyzer, since static members do
     *       not have access to the 'this' pointer.                          */
#   if defined(QUEX_OPTION_TOKEN_POLICY_SINGLE)
    register QUEX_TYPE_TOKEN_ID __self_result_token_id 
           = (QUEX_TYPE_TOKEN_ID)__QUEX_SETTING_TOKEN_ID_UNINITIALIZED;
#   endif
"""

comment_on_post_context_position_init_str = """
    /* Post context positions do not have to be reset or initialized. If a state
     * is reached which is associated with 'end of post context' it is clear what
     * post context is meant. This results from the ways the state machine is 
     * constructed. Post context position's live cycle:
     *
     * (1)   unitialized (don't care)
     * (1.b) on buffer reload it may, or may not be adapted (don't care)
     * (2)   when a post context begin state is passed, then it is **SET** (now: take care)
     * (2.b) on buffer reload it **is adapted**.
     * (3)   when a terminal state of the post context is reached (which can only be reached
     *       for that particular post context), then the post context position is used
     *       to reset the input position.                                              */
"""

def _analyzer_function(StateMachineName, Setup, variable_definitions, 
                       function_body, ModeNameList=[]):
    """EngineClassName = name of the structure that contains the engine state.
                         if a mode of a complete quex environment is created, this
                         is the mode name. otherwise, any name can be chosen. 
       SingleModeAnalyzerF = False if a mode for a quex engine is to be created. True
                           if a stand-alone lexical engine is required (without the
                           complete mode-handling framework of quex).
    """              
    Lng = Setup.language_db
    SingleModeAnalyzerF = Setup.single_mode_analyzer_f

    mode_definition_str   = ""
    mode_undefinition_str = ""
    if len(ModeNameList) != 0 and not SingleModeAnalyzerF: 
        L = max(map(lambda name: len(name), ModeNameList))
        mode_definition_str = "".join(
            "#   define %s%s    (QUEX_NAME(%s))\n" % (name, " " * (L- len(name)), name)
            for name in ModeNameList
        )
        mode_undefinition_str = "".join(
            "#   undef %s\n" % name 
            for name in ModeNameList
        )

    function_signature_str = __function_signature.replace("$$STATE_MACHINE_NAME$$", 
                                                          StateMachineName)
    txt = [
        "#include <quex/code_base/temporary_macros_on>\n",
        function_signature_str,
        # 
        # Macro definitions
        #
        "#   ifdef     self\n",
        "#       undef self\n",
        "#   endif\n",
        "#   define self (*((QUEX_TYPE_ANALYZER*)me))\n",
        "/*  'QUEX_GOTO_STATE' requires 'QUEX_LABEL_STATE_ROUTER' */\n",
        "#   define QUEX_LABEL_STATE_ROUTER %s\n" % dial_db.get_label_by_door_id(DoorID.global_state_router()),
        mode_definition_str,
        Lng.LEXEME_MACRO_SETUP(),
        #
        variable_definitions,
        #
        comment_on_post_context_position_init_str,
        "#   if    defined(QUEX_OPTION_AUTOMATIC_ANALYSIS_CONTINUATION_ON_MODE_CHANGE) \\\n",
        "       || defined(QUEX_OPTION_ASSERTS)\n",
        "    me->DEBUG_analyzer_function_at_entry = me->current_analyzer_function;\n",
        "#   endif\n",
        #
        # Entry to the actual function body
        #
        "%s\n" % Lng.LABEL(DoorID.global_reentry()),
        "    %s\n" % Lng.LEXEME_START_SET(),
        "    QUEX_LEXEME_TERMINATING_ZERO_UNDO(&me->buffer);\n",
    ]

    txt.extend(function_body)

    # -- prevent the warning 'unused variable'
    txt.extend([ 
        "\n",                                                                                             
        "    __quex_assert_no_passage();\n", 
        "\n",                                                                                             
        "    /* Following labels are referenced in macros. It cannot be detected\n"
        "     * whether the macros are applied in user code or not. To avoid compiler.\n"
        "     * warnings of unused labels, they are referenced in unreachable code.   */\n"
        "    %s /* in RETURN                */\n" % Lng.GOTO(DoorID.return_with_on_after_match()),
        "    %s /* in CONTINUE              */\n" % Lng.GOTO(DoorID.continue_with_on_after_match()),
        "    %s /* in CONTINUE and skippers */\n" % Lng.GOTO(DoorID.continue_without_on_after_match()),
        "#   if ! defined(QUEX_OPTION_COMPUTED_GOTOS)\n",
        "    %s /* in QUEX_GOTO_STATE       */\n" % Lng.GOTO(DoorID.global_state_router()),
        "#   endif\n",
        "\n",
        "    /* Prevent compiler warning 'unused variable'.                           */\n",
        "    (void)QUEX_LEXEME_NULL;\n",                                    
        "    (void)QUEX_NAME_TOKEN(DumpedTokenIdObject);\n",                
        "    /* target_state_index and target_state_else_index appear when \n",
        "     * QUEX_GOTO_STATE is used without computed goto-s.                      */\n",
        "    (void)target_state_index;\n",
        "    (void)target_state_else_index;\n",
        #
        # Macro undefinitions
        # 
        lexeme_macro_clean_up,
        mode_undefinition_str,
        "#   undef self\n",
        "#   undef QUEX_LABEL_STATE_ROUTER\n",
        "}\n",
        "#include <quex/code_base/temporary_macros_off>\n",
    ])
    return txt

lexeme_macro_setup = """
    /* Lexeme setup: 
     *
     * There is a temporary zero stored at the end of each lexeme, if the action 
     * references to the 'Lexeme'. 'LexemeNull' provides a reference to an empty
     * zero terminated string.                                                    */
#if defined(QUEX_OPTION_ASSERTS)
#   define Lexeme       QUEX_NAME(access_Lexeme)((const char*)__FILE__, (size_t)__LINE__, &me->buffer)
#   define LexemeBegin  QUEX_NAME(access_LexemeBegin)((const char*)__FILE__, (size_t)__LINE__, &me->buffer)
#   define LexemeL      QUEX_NAME(access_LexemeL)((const char*)__FILE__, (size_t)__LINE__, &me->buffer)
#   define LexemeEnd    QUEX_NAME(access_LexemeEnd)((const char*)__FILE__, (size_t)__LINE__, &me->buffer)
#else
#   define Lexeme       (me->buffer._lexeme_start_p)
#   define LexemeBegin  Lexeme
#   define LexemeL      $$LEXEME_LENGTH$$
#   define LexemeEnd    $$INPUT_P$$
#endif

#define LexemeNull      (&QUEX_LEXEME_NULL)
"""

lexeme_macro_clean_up = """
#   undef Lexeme
#   undef LexemeBegin
#   undef LexemeEnd
#   undef LexemeNull
#   undef LexemeL
"""

__return_if_queue_full_or_simple_analyzer = """
#   ifndef __QUEX_OPTION_PLAIN_ANALYZER_OBJECT
#   ifdef  QUEX_OPTION_TOKEN_POLICY_QUEUE
    if( QUEX_NAME(TokenQueue_is_full)(&self._token_queue) ) {
        return;
    }
#   else
    if( self_token_get_id() != __QUEX_SETTING_TOKEN_ID_UNINITIALIZED) {
        return __self_result_token_id;
    }
#   endif
#   endif
"""
__return_if_mode_changed = """
    /*  If a mode change happened, then the function must first return and
     *  indicate that another mode function is to be called. At this point, 
     *  we to force a 'return' on a mode change. 
     *
     *  Pseudo Code: if( previous_mode != current_mode ) {
     *                   return 0;
     *               }
     *
     *  When the analyzer returns, the caller function has to watch if a mode 
     *  change occurred. If not it can call this function again.             */
#   if    defined(QUEX_OPTION_AUTOMATIC_ANALYSIS_CONTINUATION_ON_MODE_CHANGE) \
       || defined(QUEX_OPTION_ASSERTS)
    if( me->DEBUG_analyzer_function_at_entry != me->current_analyzer_function ) 
#   endif
    { 
#       if defined(QUEX_OPTION_AUTOMATIC_ANALYSIS_CONTINUATION_ON_MODE_CHANGE)
        self_token_set_id(__QUEX_SETTING_TOKEN_ID_UNINITIALIZED);
        __QUEX_PURE_RETURN;
#       elif defined(QUEX_OPTION_ASSERTS)
        QUEX_ERROR_EXIT("Mode change without immediate return from the lexical analyzer.");
#       endif
    }
"""

def reentry_preparation(Lng, PreConditionIDList, OnAfterMatchCode):
    """Reentry preperation (without returning from the function."""
    # (*) Unset all pre-context flags which may have possibly been set
    unset_pre_context_flags_str = Lng.PRE_CONTEXT_RESET(PreConditionIDList)

    if OnAfterMatchCode is not None:
        on_after_match_str = Lng.SOURCE_REFERENCED(OnAfterMatchCode)
    else:
        on_after_match_str = ""

    txt = [ 
        "\n%s\n"  % Lng.LABEL(DoorID.return_with_on_after_match()), 
        Lng.COMMENT("RETURN -- after executing 'on_after_match' code."),
        on_after_match_str,
        "    %s\n\n" % Lng.PURE_RETURN,
        #
        "\n%s\n" % Lng.LABEL(DoorID.continue_with_on_after_match()), 
        Lng.COMMENT("CONTINUE -- after executing 'on_after_match' code."),
        on_after_match_str,
        #
        "\n%s\n" % Lng.LABEL(DoorID.continue_without_on_after_match()),
        Lng.COMMENT("CONTINUE -- without executing 'on_after_match' (e.g. on FAILURE)."), "\n",
        #
        __return_if_queue_full_or_simple_analyzer, "\n",
        __return_if_mode_changed, "\n",
        #
        unset_pre_context_flags_str,
        "\n%s\n" % Lng.GOTO(DoorID.global_reentry()), 
    ]

    return txt

def __frame_of_all(Code, Setup):
    # namespace_ref   = Lng.NAMESPACE_REFERENCE(Setup.analyzer_name_space)
    # if len(namespace_ref) > 2 and namespace_ref[:2] == "::":  namespace_ref = namespace_ref[2:]
    # if len(namespace_ref) > 2 and namespace_ref[-2:] == "::": namespace_ref = namespace_ref[:-2]
    # "using namespace " + namespace_ref + ";\n"       + \

    implementation_header_str = ""
    if Setup.language == "C":
        implementation_header_str += "#if defined(__QUEX_OPTION_CONVERTER_HELPER)\n"
        implementation_header_str += "#    include \"%s\"\n" % Setup.get_file_reference(Setup.output_buffer_codec_header_i)
        implementation_header_str += "#else\n"
        implementation_header_str += "#    include \"quex/code_base/converter_helper/from-unicode-buffer.i\"\n"
        implementation_header_str += "#endif\n"
        implementation_header_str += "#include <quex/code_base/analyzer/headers.i>\n"
        implementation_header_str += "#include <quex/code_base/analyzer/C-adaptions.h>\n"

    lexeme_null_definition = ""
    if Setup.external_lexeme_null_object == "":
        # LexemeNull has been defined elsewhere.
        lexeme_null_definition = "QUEX_TYPE_CHARACTER  QUEX_LEXEME_NULL_IN_ITS_NAMESPACE = (QUEX_TYPE_CHARACTER)0;\n"

    return "".join(["/* #include \"%s\"*/\n" % Setup.get_file_reference(Setup.output_header_file),
                    implementation_header_str,
                    "QUEX_NAMESPACE_MAIN_OPEN\n",
                    lexeme_null_definition,
                    Code,
                    "QUEX_NAMESPACE_MAIN_CLOSE\n"])                     

def __get_if_in_character_set(ValueList):
    assert type(ValueList) == list
    assert len(ValueList) > 0
    txt = "if( "
    for value in ValueList:
        txt += "input == %i || " % value

    txt = txt[:-3] + ") {\n"
    return txt

def __get_if_in_interval(TriggerSet):
    assert TriggerSet.__class__.__name__ == "Interval"
    assert TriggerSet.size() >= 2

    if TriggerSet.size() == 2:
        return "if( input == %i || input == %i ) {\n" % (TriggerSet.begin, TriggerSet.end - 1)
    else:
        return "if( input >= %i && input < %i ) {\n" % (TriggerSet.begin, TriggerSet.end)

def __condition(txt, CharSet):
    first_f = True
    for interval in CharSet.get_intervals(PromiseToTreatWellF=True):
        if first_f: first_f = False
        else:       txt.append(" || ")

        if interval.end - interval.begin == 1:
            txt.append("(C) == 0x%X"                % interval.begin)
        elif interval.end - interval.begin == 2:
            txt.append("(C) == 0x%X || (C) == 0x%X" % (interval.begin, interval.end - 1))
        else:
            txt.append("(C) <= 0x%X && (C) < 0x%X" % (interval.begin, interval.end))

def __indentation_add(Info):
    # (0) If all involved counts are single spaces, the 'counting' can be done
    #     easily by subtracting 'end - begin', no adaption.
    indent_txt = " " * 16
    if Info.homogeneous_spaces():
        return ""

    def __do(txt, CharSet, Content):
        txt.append(indent_txt + "if( ")
        __condition(txt, CharSet)
        txt.append(" ) { ")
        txt.append(Content)
        txt.append(" }\\\n")

    txt       = []
    spaces_db = {} # Sort same space counts together
    grid_db   = {} # Sort same grid counts together
    for name, count_parameter in Info.count_db.items():
        count         = count_parameter.get()
        character_set = Info.character_set_db[name].get()
        if count == "bad": continue
        # grid counts are indicated by negative integer for count.
        if count >= 0:
            spaces_db.setdefault(count, NumberSet()).unite_with(character_set)
        else:
            grid_db.setdefault(count, NumberSet()).unite_with(character_set)

    for count, character_set in spaces_db.items():
        __do(txt, character_set, "(I) += %i;" % count)

    for count, character_set in grid_db.items():
        __do(txt, character_set, "(I) += (%i - ((I) %% %i));" % (abs(count), abs(count)))

    return "".join(txt)

def __indentation_check_whitespace(Info):
    all_character_list = map(lambda x: x.get(), Info.character_set_db.values())
    assert len(all_character_list) != 0

    number_set = all_character_list[0]
    for character_set in all_character_list[1:]:
        number_set.unite_with(character_set)

    txt = []
    __condition(txt, number_set)
    return "".join(txt)

def __get_switch_block(VariableName, CaseCodePairList):
    txt = [0, "switch( %s ) {\n" % VariableName]
    next_i = 0
    L = len(CaseCodePairList)
    CaseCodePairList.sort(key=itemgetter(0))
    for case, code in CaseCodePairList: 
        next_i += 1
        txt.append(1)
        case_label = "0x%X" % case
        if next_i != L and CaseCodePairList[next_i][1] == code:
            txt.append("case %s: %s\n" % (case_label, " " * (7 - len(case_label))))
        else:
            txt.append("case %s: %s" % (case_label, " " * (7 - len(case_label))))
            if type(code) == list: txt.extend(code)
            else:                  txt.append(code)
            txt.append("\n")
            
    txt.append(0)
    txt.append("}\n")
    return txt


