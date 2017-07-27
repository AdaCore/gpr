import quex.engine.state_machine.algorithm.beautifier     as beautifier
import quex.engine.state_machine.algebra.reverse          as reverse
import quex.engine.state_machine.algebra.complement_begin as complement_begin

def do(SM_A, SM_B):
    """Find a state machine that stops right before the state machine 'SM_B'.
    If there is a lexeme 'l' (lowercase L) in SM_A:

                       l = [x0, x1, ... xj, xk, ... xN ]

    and '[xk ... xN]' is a lexeme from L(SM_B). The 'rcut(SM_A, SM_B)' shall
    only match 
                       '[x0, x1, ... xj]'. 
                       
    All lexemes 'l' translate into lexemes 's' in reverse(SM_A):

                       s = [xN, ... xk, xj, ... x1, x0 ]

    lexemes in SM_B translate into reverse(SM_B) as

                       t = [xN, ... xk]

    The 'cut' operation cut(reverse(SM_A), reverse(SM_B)) delivers

                       u = [ xj, ... x1, x0 ]

    Then, the 'reverse(cut(reverse(SM_A), reverse(SM_B)))' delivers

                       u = [ x0, x1, ... xj ]

    as desired for all lexemes in SM_A that end with something that 
    matches SM_B.
                       
    (C) Frank-Rene Schaefer
    """
    Ar        = beautifier.do(reverse.do(SM_A))
    Br        = beautifier.do(reverse.do(SM_B))
    cut_Ar_Br = complement_begin.do(Ar, Br)
    return reverse.do(cut_Ar_Br)
