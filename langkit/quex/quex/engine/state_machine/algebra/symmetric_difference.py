import quex.engine.state_machine.algebra.union        as union
import quex.engine.state_machine.algebra.intersection as intersection
import quex.engine.state_machine.algebra.difference   as difference

def do(SM_List):
    """Result: A state machine that matches what is matched by one of the
               state machines but by no other.

       Formula:

                       difference(union(All), intersection(All))

    """
    # Difference: It only remains in A what is not in A and B.
    tmp0 = union.do(SM_List)
    tmp1 = intersection.do(SM_List)
    return difference.do(tmp0, tmp1)

