from quex.blackboard import setup as Setup, \
                            Lng

def do(Interval, Target, IndentF=False):
    global Setup
    global Lng

    if   hasattr(Target, "code"): txt = Target.code()
    elif type(Target) == long:    txt = [ Lng.GOTO_ADDRESS(Target) ]
    else:                         txt = [ Target ]

    if Interval is not None and Setup.comment_transitions_f: 
        txt.append(Lng.COMMENT(Interval.get_utf8_string()))

    return txt

