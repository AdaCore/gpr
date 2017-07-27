import quex.engine.state_machine.algorithm.nfa_to_dfa            as nfa_to_dfa
import quex.engine.state_machine.algorithm.hopcroft_minimization as hopcroft

def do(SM):
    """Construct a state machine which is equivalent to SM and is:

       -- DFA compliant, i.e. without epsilon transitions and no two
              transitions to the same target.
       -- Hopcroft-minimized.
    """
    result = nfa_to_dfa.do(SM)
    hopcroft.do(result, CreateNewStateMachineF=False)

    assert result.is_DFA_compliant()
    return result
