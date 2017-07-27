from   quex.blackboard  import Lng

def do(txt, StateIndex, DropOut, TheAnalyzer, DefineLabelF=True, MentionStateIndexF=True):
    
    if MentionStateIndexF:
        txt.append(1)
        txt.append("__quex_debug_drop_out(%i);\n" % StateIndex)

    txt.extend(
        Lng.GOTO(TheAnalyzer.drop_out_DoorID(StateIndex))
    )
    return 


