import quex.engine.state_machine.algorithm.beautifier as beautifier
import quex.engine.state_machine.algebra.reverse      as reverse
import quex.engine.state_machine.algebra.cut_begin    as cut_begin

def do(SM_A, SM_B):
    Ar        = beautifier.do(reverse.do(SM_A))
    Br        = beautifier.do(reverse.do(SM_B))
    cut_Ar_Br = cut_begin.do(Ar, Br)
    return reverse.do(cut_Ar_Br)
