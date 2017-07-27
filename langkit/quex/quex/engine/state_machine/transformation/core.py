# (C) Frank-Rene Schaefer
from   quex.engine.state_machine.core                  import StateMachine
import quex.engine.state_machine.algorithm.beautifier  as beautifier
from   quex.blackboard                                 import setup as Setup

from   quex.engine.misc.tools import typed, \
                                flatten_list_of_lists

@typed(SmIn=(StateMachine,None))
def do_state_machine(SmIn):
    """Transforms a given state machine from 'Unicode Driven' to another
       character encoding type.
    
       RETURNS: 
       [0] Transformation complete (True->yes, False->not all transformed)
       [1] Transformed state machine. It may be the same as it was 
           before if there was no transformation actually.

       It is ensured that the result of this function is a DFA compliant
       state machine.
    """
    if SmIn is None: return True, None
    assert SmIn.is_DFA_compliant()

    # BEFORE: Forgive characters not in source range. What comes out is 
    #         important. It is checked in 'transform()' of the Pattern.
    complete_f, sm_out = Setup.buffer_codec.transform(SmIn)

    # AFTER: Whatever happend, the transitions in the state machine MUST
    #        lie in the drain_set.
    sm_out.assert_range(Setup.buffer_codec.drain_set)

    if sm_out.is_DFA_compliant(): return complete_f, sm_out
    else:                         return complete_f, beautifier.do(sm_out)

def do_set(number_set, TrafoInfo, fh=-1):
    """RETURNS: True  transformation successful
                False transformation failed, number set possibly in inconsistent state!
    """
    return TrafoInfo.transform_NumberSet(number_set)

def do_character(Character, TrafoInfo, fh=-1):
    """The current implementation is, admitably, not very fast. 
    Improve upon detection of speed issues.

    RETURNS: A list of integers which represent the character in the 
             given TrafoInfo.
    """
    return TrafoInfo.transform_Number(Character) 

def do_sequence(Sequence, TrafoInfo=None, fh=-1):
    if TrafoInfo is None:
        TrafoInfo = Setup.buffer_codec

    return flatten_list_of_lists(
        do_character(x, TrafoInfo, fh)
        for x in Sequence
    )



