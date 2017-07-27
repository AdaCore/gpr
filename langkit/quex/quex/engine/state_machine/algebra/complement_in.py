import quex.engine.state_machine.check.special              as special
import quex.engine.state_machine.algorithm.beautifier       as beautifier
import quex.engine.state_machine.construction.sequentialize as sequentialize
import quex.engine.state_machine.construction.repeat        as repeat
import quex.engine.state_machine.algebra.complement_begin   as complement_begin

def do(SM_A, SM_B):
    """\NotIn{P Q} = \NotBegin{P \Any*(Q+)}
    """
    all_star      = repeat.do(special.get_any(), min_repetition_n=0)
    sm_b_repeated = repeat.do(SM_B, min_repetition_n=1)

    tmp = sequentialize.do([all_star, sm_b_repeated], 
                           MountToFirstStateMachineF=True, 
                           CloneRemainingStateMachinesF=True)

    tmp = beautifier.do(tmp)

    # There might be many paths which have no hope to reach acceptance
    tmp.clean_up()

    return complement_begin.do(SM_A, tmp)

# Following was just a trie:
#
#def DELETE_do(SM_A, SM_B):
#    SM_A_init_state_index_backup = SM_A.init_state_index
#    sm_list = []
#    for state_index in SM_A.states.iterkeys():
#        if state_index != SM_A.init_state_index:
#            starting_sm = clone_until_state(SM_A, state_index, SM_B.init_state_index)
#
#            debug_clone = deepcopy(starting_sm)
#            link_si = index.map_state_combination_to_index((state_index, SM_B.init_state_index))
#            debug_clone.states[link_si] = State()
#            print "#starting_sm:", debug_clone
#        else:
#            print "#starting_sm:", None
#            starting_sm = None
#
#        # Start analysis from 'state_index' in the new clone
#        walker = complement_begin.WalkAlong(SM_A, SM_B, starting_sm)
#        walker.do((state_index, SM_B.init_state_index))
#        walker.result.clean_up()
#        print "#result:", walker.result
#        sm_list.append(walker.result)
#
#    return intersection.do(sm_list)
#
#def clone_until_state(SM, StateIndex, CorrespondentInitStateIndex):
#    """It must be made sure, that the cloned state 'StateIndex' fits with 
#    the 'SM_B' init state. That is necessary, so that the walker can 'go'
#    to it propperly.
#    """
#    done_set             = set([StateIndex])
#    result               = StateMachine(AcceptanceF=SM.get_init_state().is_acceptance())
#    replacement_db       = { 
#        SM.init_state_index: result.init_state_index,
#        StateIndex:          index.map_state_combination_to_index((StateIndex, CorrespondentInitStateIndex)), 
#    }
#    def get_new_index(db, Index):
#        result        = db.get(Index)
#
#        if result is None: 
#            result    = index.get()
#            db[Index] = result
#        return result
#
#    work_list            = [ SM.init_state_index ]
#    while len(work_list) != 0:
#        state_index = work_list.pop()
#        done_set.add(state_index)
#
#        state                          = SM.states[state_index]
#        new_state_index                = get_new_index(replacement_db, state_index)
#        new_state                      = State(AcceptanceF = state.is_acceptance())
#        result.states[new_state_index] = new_state
#
#        tm = state.target_map.get_map()
#        for target_index, trigger_set in tm.iteritems():
#            new_state.add_transition(trigger_set.clone(), get_new_index(replacement_db, target_index))
#
#        work_list.extend(i for i in tm.iterkeys() if i not in done_set)
#
#    return result 
#
#
