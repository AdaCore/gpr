import quex.output.core.state.transition_map.solution            as     solution
from   quex.blackboard                                                import setup as Setup

def do(txt, TM):
    """Implement the given transition map 'TM'. That is for a given 'input' 
    determine in what interval it lies. Depending on that interval execute
    associated code. 

                TM = list of (interval, effect)

    In particular, the 'effect' describes the transition to another state.
    
    RETURNS: Code that implements the map.
    """
    #__________________________________________________________________________
    #
    #   NOT:      TM.prune(min, supremum) !!
    #
    # Pruning at this point in time would mean that possible transitions are 
    # cut. As a consequence whole branches of the the state machine may be 
    # unreachable! Such things must have been clarified before!
    #__________________________________________________________________________
    _assert_consistency(TM)

    structure = solution.do(TM)
    # (*) Bisection until other solution is more suitable.
    #     (This may include 'no bisectioning')
    _implement(txt, structure)

def _implement(txt, structure):
    """Creates code for state transitions from this state. This function is very
       similar to the function creating code for a 'NumberSet' condition 
       (see 'interval_handling').
    
       Writes code that does a mapping according to 'binary search' by
       means of if-else-blocks.
    """
    global Lng

    # Potentially Recursive
    #txt.append(E_TextOp.INDENT)
    txt.extend(structure.implement())
    #txt.append(E_TextOp.DEDENT)

def _assert_consistency(TM):
    """Check consistency of the given transition map.

    IMPORTANT: 

    The transition map MUST NOT exceed the range given by the buffer element
    type! Otherwise, it would mean that transitions wer cut off! This may lead
    to undefined reference labels etc. Such things must have been clarified
    before the call of the transition map coder!
    
    ABORTS: In case that the given transition map is not properly setup.
    """
    assert TM is not None
    assert len(TM) != 0

    # The transition map MUST be designed to cover exactly the range of 
    # of possible values given by the buffer element type!
    TM.assert_boundary(Setup.buffer_codec.drain_set.minimum(), 
                       Setup.buffer_codec.drain_set.supremum()) 
