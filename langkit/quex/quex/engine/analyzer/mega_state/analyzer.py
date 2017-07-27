# (C) 2013 Frank-Rene Schaefer
import quex.engine.analyzer.mega_state.template.core    as     template_analyzer
from   quex.engine.analyzer.mega_state.template.state   import TemplateState
import quex.engine.analyzer.mega_state.path_walker.core as     path_analyzer
from   quex.engine.analyzer.mega_state.target           import TargetByStateKey
from   quex.blackboard                                  import setup as Setup, \
                                                               E_Compression

def do(TheAnalyzer):
    """MegaState Analysis _____________________________________________________

    Normal states are potentially absorbed by MegaState-s which represent more
    than one single state at once.

    The setting 'Setup.compression_type_list' defines what type of algorithms
    have to be executed in to construct MegaStates (if any at all). Consider
    'core.py' in this directory for further reading.
    ___________________________________________________________________________

    NOTE: MegaState-s apply some 'mechanics' for implementing the state which
          they represent. However, the TransitionMap-s of other states are 
          not effected. They remain targetting the same DoorID-s.

    Example: 

          ( 1 )--- 'a' -->[Door0]-->( 2 )--- 'b' ---> ( 3 )
                                    /                 / 
          ( 4 )--- 'c' -->[Door1]--'                 /
                                                    /
          ( 5 )--- 'd' -->[Door2]-->( 6 )--- 'e' --'

    After implementing 2 and 6 in a MegaState:

                         MegaState
                          .--------------------.
          ( 1 )--- 'a' -->[Door0] ... [state=2?]--- 'b' ---> ( 3 )
                          |                    |    /        / 
          ( 4 )--- 'c' -->[Door1] ... [state=2?]---'        /
                          |                    |           /
          ( 5 )--- 'd' -->[Door2] ... [state=6?]--- 'e' --'
                          '--------------------'

    So, from outside, there is no observable change in behavior. Other states
    do not 'feel' that there is a MegaState.
    ___________________________________________________________________________
    """ 
    assert len(Setup.compression_type_list) != 0
    mega_state_list = []

    # The 'remainder' keeps track of states which have not yet been
    # absorbed into a MegaState.
    remainder = set(TheAnalyzer.state_db.iterkeys())
    remainder.remove(TheAnalyzer.init_state_index)

    for ctype in Setup.compression_type_list:
        # -- MegaState-s by Path-Compression
        if ctype in (E_Compression.PATH, E_Compression.PATH_UNIFORM):
            new_mega_state_list = path_analyzer.do(TheAnalyzer, ctype, remainder)
    
        # -- MegaState-s by Template-Compression
        elif ctype in (E_Compression.TEMPLATE, E_Compression.TEMPLATE_UNIFORM):
            new_mega_state_list = template_analyzer.do(TheAnalyzer, Setup.compression_template_min_gain, 
                                                       ctype, remainder)
        else:
            assert False

        for mega_state in new_mega_state_list:
            mega_state.finalize(TheAnalyzer, ctype)
            mega_state.assert_consistency(ctype, remainder, TheAnalyzer)

        # -- Track the remaining not-yet-absorbed states
        for mega_state in new_mega_state_list:
            remainder.difference_update(mega_state.implemented_state_index_set())

        mega_state_list.extend(new_mega_state_list)

    # Only now: We enter the MegaState-s into the 'state_db'. If it was done before,
    # the MegaState-s might try to absorb each other.
    TheAnalyzer.add_mega_states(mega_state_list)

    for mega_state in mega_state_list:
        if isinstance(mega_state, TemplateState):
            ## TargetByStateKey.rejoin_uniform_schemes(mega_state.transition_map)
            TargetByStateKey.assign_scheme_ids(mega_state.transition_map)

    return

