from   quex.output.core.variable_db  import variable_db
from   quex.output.core.skipper.common         import get_character_sequence, \
                                                           get_on_skip_range_open, \
                                                           line_column_counter_in_loop
import quex.engine.state_machine.index              as     sm_index
from   quex.engine.analyzer.door_id_address_label   import __nice, \
                                                           dial_db
from   quex.engine.misc.string_handling             import blue_print
from   quex.engine.misc.tools                            import typed
from   quex.blackboard                              import Lng

def do(Data, TheAnalyzer):

    OpeningSequence = Data["opener_sequence"]
    CloserSequence  = Data["closer_sequence"]
    CloserPattern   = Data["closer_pattern"]
    ModeName        = Data["mode_name"]
    OnSkipRangeOpen = Data["on_skip_range_open"]
    DoorIdAfter     = Data["door_id_after"]

    return get_skipper(OpeningSequence, CloserSequence, CloserPattern, ModeName, OnSkipRangeOpen, DoorIdAfter) 

#def new_skipper():
#    #CloserSequence = transform()
#    #OpeningSequence = transform()
#    character_set   = NumberSet(CloserSequence[0])
#    character_set.add(OpeningSequence[0])
#    character_set.complement()
#
#    txt = "/* Assert sizeof(buffer) >= len(CloserSequence) + 2 */\n"
#
#    end_sequence_check_adr = index.get()
#    end_sequence_label     = get_label("$entry", end_sequence_check_adr, U=True) 
#
#    implementation_type, \
#    loop_txt,            \
#    entry_action,        \
#    exit_action          = LoopGenerator.do(Mode.counter_db, 
#                             IteratorName = "me->buffer._input_p",
#                             OnContinue   = [ 1, "continue;" ],
#                             OnExit       = [ 1, "goto %s;" % end_sequence_label ],
#                             CharacterSet = character_set, 
#                             ReloadF      = True)
#
#    end_sequence_txt = get_end_sequence(OpeningSequence, CloserSequence)

#def new_end_sequence():
#    txt.append("%s:\n" % EndSequenceLabel)
#    txt.append("/* Reload if necessary */\n")
#    txt.append("/* If fail --> skipped until end of file. */\n")
#    txt.append(Lng.CHARACTER_BEGIN_P_SET())
#
#    common_sequence = common(OpeningSequence, CloserSequence)
#    for chunk in common_sequence:
#        pass # Code if(chunk)
#
#    # 'if i == OpeningSequence[i]' --> continue with opening sequence
#    for chunk in common_sequence:
#        pass # Code if(chunk)
#
#    # 'if i == CloserSequence[i]' --> continue with closing sequence

template_str = """
    Skipper$$SKIPPER_INDEX$$_Opener_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Opener;
    Skipper$$SKIPPER_INDEX$$_Closer_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Closer;
    /* text_end                           = QUEX_NAME(Buffer_text_end)(&me->buffer); */
$$LC_COUNT_COLUMN_N_POINTER_DEFINITION$$

$$ENTRY$$
    QUEX_BUFFER_ASSERT_CONSISTENCY(&me->buffer);
    __quex_assert(QUEX_NAME(Buffer_content_size)(&me->buffer) >= $$OPENER_LENGTH$$ );

    /* NOTE: If _input_p == end of buffer, then it will drop out immediately out of the
     *       loop below and drop into the buffer reload procedure.                      */

    /* Loop eating characters: Break-out as soon as the First Character of the Delimiter
     * (FCD) is reached. Thus, the FCD plays also the role of the Buffer Limit Code. There
     * are two reasons for break-out:
     *    (1) we reached a limit (end-of-file or buffer-limit)
     *    (2) there was really the FCD in the character stream
     * This must be distinguished after the loop was exited. But, during the 'swallowing' 
     * we are very fast, because we do not have to check for two different characters.  */
    while( 1 + 1 == 2 ) {
        $$INPUT_GET$$ 
        if( input == QUEX_SETTING_BUFFER_LIMIT_CODE ) {
            $$GOTO_RELOAD$$
        }
        if( input == *Skipper$$SKIPPER_INDEX$$_Closer_it ) {
            ++Skipper$$SKIPPER_INDEX$$_Closer_it;
            if( Skipper$$SKIPPER_INDEX$$_Closer_it == Skipper$$SKIPPER_INDEX$$_CloserEnd ) {
                if( counter == 0 ) {
                    /* NOTE: The initial state does not increment the input_p. When it detects that
                     * it is located on a buffer border, it automatically triggers a reload. No 
                     * need here to reload the buffer. */
                    $$INPUT_P_INCREMENT$$ /* Now, BLC cannot occur. See above. */
                    $$LC_COUNT_END_PROCEDURE$$
                    /* No need for re-entry preparation. Acceptance flags and modes are untouched after skipping. */
                    $$GOTO_AFTER_END_OF_SKIPPING$$ /* End of range reached. */
                }
                --counter;
                Skipper$$SKIPPER_INDEX$$_Opener_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Opener;
                Skipper$$SKIPPER_INDEX$$_Closer_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Closer;
                goto CONTINUE_$$SKIPPER_INDEX$$;
            }
        } else {
            Skipper$$SKIPPER_INDEX$$_Closer_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Closer;
        }
        if( input == *Skipper$$SKIPPER_INDEX$$_Opener_it ) {
            ++Skipper$$SKIPPER_INDEX$$_Opener_it;
            if( Skipper$$SKIPPER_INDEX$$_Opener_it == Skipper$$SKIPPER_INDEX$$_OpenerEnd ) {
                ++counter;
                Skipper$$SKIPPER_INDEX$$_Opener_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Opener;
                Skipper$$SKIPPER_INDEX$$_Closer_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Closer;
                goto CONTINUE_$$SKIPPER_INDEX$$;
            }
        } else {
            Skipper$$SKIPPER_INDEX$$_Opener_it = (QUEX_TYPE_CHARACTER*)Skipper$$SKIPPER_INDEX$$_Opener;
        }
CONTINUE_$$SKIPPER_INDEX$$:
$$LC_COUNT_IN_LOOP$$
        $$INPUT_P_INCREMENT$$ /* Now, BLC cannot occur. See above. */
    }

$$RELOAD$$:
    QUEX_BUFFER_ASSERT_CONSISTENCY_LIGHT(&me->buffer);
    /* -- When loading new content it is checked that the beginning of the lexeme
     *    is not 'shifted' out of the buffer. In the case of skipping, we do not care about
     *    the lexeme at all, so do not restrict the load procedure and set the lexeme start
     *    to the actual input position.                                                    */
    $$MARK_LEXEME_START$$

$$LC_COUNT_BEFORE_RELOAD$$
    if( QUEX_NAME(Buffer_is_end_of_file)(&me->buffer) == false ) {
        QUEX_NAME(buffer_reload_forward)(&me->buffer, (QUEX_TYPE_CHARACTER_POSITION*)position,
                                         PositionRegisterN);
        /* Recover '_input_p' from lexeme start 
         * (inverse of what we just did before the loading) */
        $$INPUT_P_TO_LEXEME_START$$
        /* After reload, we need to increment _input_p. That's how the game is supposed to be played. 
         * But, we recovered from lexeme start pointer, and this one does not need to be incremented. */
        /* text_end = QUEX_NAME(Buffer_text_end)(&me->buffer); */
$$LC_COUNT_AFTER_RELOAD$$
        QUEX_BUFFER_ASSERT_CONSISTENCY(&me->buffer);
        $$GOTO_ENTRY$$ /* End of range reached.             */
    }
    /* Here, either the loading failed or it is not enough space to carry a closing delimiter */
    $$INPUT_P_TO_LEXEME_START$$
    $$ON_SKIP_RANGE_OPEN$$
"""

@typed(OpenerSequence=[int], CloserSequence=[int])
def get_skipper(OpenerSequence, CloserSequence, CloserPattern, ModeName, OnSkipRangeOpen, DoorIdAfter):
    assert len(OpenerSequence) >= 1
    assert len(CloserSequence) >= 1
    assert OpenerSequence != CloserSequence

    skipper_index   = sm_index.get()
    skipper_door_id = dial_db.new_door_id(skipper_index)

    opener_str, opener_comment_str = get_character_sequence(OpenerSequence)
    opener_length = len(OpenerSequence)
    closer_str, closer_comment_str = get_character_sequence(CloserSequence)
    closer_length = len(CloserSequence)

    variable_db.require("reference_p", Condition="QUEX_OPTION_COLUMN_NUMBER_COUNTING")
    variable_db.require("counter")
    variable_db.require_array("Skipper%i_Opener", Initial="{ %s }" % opener_str, ElementN=opener_length, Index = skipper_index)
    variable_db.require("Skipper%i_OpenerEnd", "Skipper%i_Opener + (ptrdiff_t)%i" % (skipper_index, opener_length), Index = skipper_index) 
    variable_db.require("Skipper%i_Opener_it", "0x0", Index = skipper_index) 
    variable_db.require_array("Skipper%i_Closer", Initial="{ %s }" % closer_str, ElementN=closer_length, Index = skipper_index) 
    variable_db.require("Skipper%i_CloserEnd", "Skipper%i_Closer + (ptrdiff_t)%i" % (skipper_index, closer_length), Index = skipper_index) 
    variable_db.require("Skipper%i_Closer_it", "0x0", Index = skipper_index) 

    reference_p_def = "    __QUEX_IF_COUNT_COLUMNS(reference_p = QUEX_NAME(Buffer_tell_memory_adr)(&me->buffer));\n"
    before_reload   = "    __QUEX_IF_COUNT_COLUMNS_ADD((size_t)(QUEX_NAME(Buffer_tell_memory_adr)(&me->buffer)\n" + \
                      "                                - reference_p));\n" 
    after_reload    = "        __QUEX_IF_COUNT_COLUMNS(reference_p = QUEX_NAME(Buffer_tell_memory_adr)(&me->buffer));\n"

    if CloserSequence[-1] == ord('\n'):
        end_procedure  = "       __QUEX_IF_COUNT_LINES_ADD((size_t)1);\n"
        end_procedure += "       __QUEX_IF_COUNT_COLUMNS_SET((size_t)1);\n"
    else:
        end_procedure = "        __QUEX_IF_COUNT_COLUMNS_ADD((size_t)(QUEX_NAME(Buffer_tell_memory_adr)(&me->buffer)\n" + \
                        "                                    - reference_p));\n" 

    reload_door_id     = dial_db.new_door_id()
    on_skip_range_open = get_on_skip_range_open(OnSkipRangeOpen, CloserPattern, NestedF=True)

    code_str = blue_print(template_str, [
                   ["$$SKIPPER_INDEX$$",   __nice(skipper_index)],
                   #
                   ["$$OPENER_LENGTH$$",                  "%i" % opener_length],
                   ["$$INPUT_P_INCREMENT$$",              Lng.INPUT_P_INCREMENT()],
                   ["$$INPUT_P_DECREMENT$$",              Lng.INPUT_P_DECREMENT()],
                   ["$$INPUT_GET$$",                      Lng.ACCESS_INPUT()],
                   ["$$IF_INPUT_EQUAL_DELIMITER_0$$",     Lng.IF_INPUT("==", "Skipper$$SKIPPER_INDEX$$[0]")],
                   ["$$ENDIF$$",                          Lng.END_IF()],
                   ["$$ENTRY$$",                          Lng.LABEL(skipper_door_id)],
                   ["$$RELOAD$$",                         dial_db.get_label_by_door_id(reload_door_id)],
                   ["$$GOTO_AFTER_END_OF_SKIPPING$$",     Lng.GOTO(DoorIdAfter)], 
                   ["$$GOTO_RELOAD$$",                    Lng.GOTO(reload_door_id)],
                   ["$$INPUT_P_TO_LEXEME_START$$",        Lng.INPUT_P_TO_LEXEME_START()],
                   # When things were skipped, no change to acceptance flags or modes has
                   # happend. One can jump immediately to the start without re-entry preparation.
                   ["$$GOTO_ENTRY$$",                     Lng.GOTO(skipper_door_id)],
                   ["$$MARK_LEXEME_START$$",              Lng.LEXEME_START_SET()],
                   ["$$ON_SKIP_RANGE_OPEN$$",             on_skip_range_open],
                   #
                   ["$$LC_COUNT_COLUMN_N_POINTER_DEFINITION$$", reference_p_def],
                   ["$$LC_COUNT_IN_LOOP$$",                     line_column_counter_in_loop()],
                   ["$$LC_COUNT_END_PROCEDURE$$",               end_procedure],
                   ["$$LC_COUNT_BEFORE_RELOAD$$",               before_reload],
                   ["$$LC_COUNT_AFTER_RELOAD$$",                after_reload],
               ])

    return [ code_str ]


