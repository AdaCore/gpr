import quex.engine.state_machine.algebra.intersection as intersection
import quex.engine.state_machine.algebra.complement   as complement

def do(A, B): 
    A_and_B     = intersection.do([A, B])
    not_A_and_B = complement.do(A_and_B)

    # Difference: It only remains in A what is not in A and B.
    return intersection.do([A, not_A_and_B])
