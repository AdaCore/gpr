from   quex.engine.analyzer.door_id_address_label  import DoorID, \
                                                          dial_db
from   quex.blackboard import Lng

from   operator import itemgetter

def do(StateRouterInfoList):
    """Create code that allows to jump to a state based on an integer value.
    """

    # NOTE: Do not use 'IfDoorIdReferencedCode' because the state router may 
    #       possibly not be tagged as 'gotoed', event if it is used.
    prolog = "#   ifndef QUEX_OPTION_COMPUTED_GOTOS\n" \
             "    __quex_assert_no_passage();\n"       \
             "%s\n" % Lng.LABEL(DoorID.global_state_router()) 
    epilog = "#   endif /* QUEX_OPTION_COMPUTED_GOTOS */\n"
    
    if len(StateRouterInfoList) == 0: code = []
    else:                             code = __get_code(StateRouterInfoList)

    result = [ prolog ] 
    result.extend(code)
    result.append(epilog)
    return result

def __get_code(StateRouterInfoList):
    # It is conceivable, that 'last_acceptance' is never set to a valid 
    # terminal. Further, there might be solely the init state. In this
    # case the state router is void of states. But, the terminal router
    # requires it to be defined --> define a dummy state router.
    if len(StateRouterInfoList) == 0:
        return ["    QUEX_ERROR_EXIT(\"Entered section of empty state router.\");\n"]

    txt = ["    switch( target_state_index ) {\n" ]

    done_set = set([])
    for index, code in sorted(StateRouterInfoList, key=itemgetter(0)):
        if index in done_set: continue
        done_set.add(index)
        txt.append("        case %i: { " % index)
        if type(code) == list: txt.extend(code)
        else:                  txt.append(code)
        txt.append("}\n")

    txt.append("\n")
    txt.append("        default:\n")
    txt.append("            __QUEX_STD_fprintf(stderr, \"State router: index = %i\\n\", (int)target_state_index);\n")
    txt.append("            QUEX_ERROR_EXIT(\"State router: unknown index.\\n\");\n")
    txt.append("    }\n")

    return txt

def get_info(StateIndexList):
    """
    NOTE: At least a 'dummy' state router is always equired so that 'goto
    QUEX_STATE_ROUTER;' does not reference a non-existing label. Then, we
    return an empty text array.

    <fschaef9/13y10m15d: "Still true with current dial_db implementation?">
    """
    if len(StateIndexList) == 0: return []

    # Make sure, that for every state the 'drop-out' state is also mentioned
    result = [None] * len(StateIndexList)
    for i, index in enumerate(StateIndexList):
        assert type(index) != str
        if index >= 0:
            # Transition to state entry
            label      = dial_db.get_label_by_address(index)
        else:
            assert False, "Is this still an issue?"
            # Transition to a templates 'drop-out'
            door_id    = DoorID.drop_out(- index)
            label      = dial_db.get_label_by_door_id(door_id)

        result[i] = (index, "goto %s; " % label)
        dial_db.mark_label_as_gotoed(label)

    return result
