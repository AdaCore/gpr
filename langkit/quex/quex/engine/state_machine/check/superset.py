from   quex.engine.state_machine.core            import StateMachine
import quex.engine.state_machine.algebra.reverse as reverse

class Checker:
    def __init__(self, SuperSM, CandidateSM):
        assert isinstance(SuperSM, StateMachine),     SuperSM.__class__.__name__
        assert isinstance(CandidateSM, StateMachine), CandidateSM.__class__.__name__

        self.sub   = CandidateSM
        self.super = SuperSM
        self.visited_state_index_set = set()

    def do(self):
        """RETURNS: 
        
             True  - if SuperSM matches all the patterns that CandidateSM
                     can match. 
             False - if not 

           In other words, SuperSM is a 'Super StateMachine' of Candidate, if
           the set of patterns matched by 'CandidateSM' a subset of the set of
           patterns matched by 'SuperSM'.                            
        """
        return self.__dive(self.sub.init_state_index, [self.super.init_state_index])

    def __dive(self, SubSM_StateIndex, SuperSM_StateIndexList):
        """SubSM_StateIndex:       refers to a state in the alleged subset state machine.

           SuperSM_StateIndexList: list of states in the 'super set' state machine that
                                   was reached by the same trigger set as SubSM_StateIndex.      
                                   They are the set of states that can 'shadow' the current
                                   state indexed by 'SubSM_StateIndex'.
        """
        # (*) Determine the states behind the indices
        sub_state        = self.sub.states[SubSM_StateIndex]
        super_state_list = map(lambda index: self.super.states[index], SuperSM_StateIndexList)
        #     Bookkeeping
        self.visited_state_index_set.add(SubSM_StateIndex)
        #     Union of all triggers were the 'shadowing' super states trigger.
        #     (For speed considerations, keep it in prepared, so it does not have to 
        #      be computed each time it is required.)
        super_trigger_set_union_db = dict(
            (index, self.super.states[index].target_map.get_trigger_set_union())
            for index in SuperSM_StateIndexList
        )

        # (*) Here comes the condition:
        #
        #     For every trigger (event) in the 'sub sm state' that triggers to a follow-up state
        #     there must be pendant triggering from the shadowing 'super sm states'.
        #
        #     If a trigger set triggers to an 'acceptance' state, then all shadowing 'super sm states' 
        #     must trigger to an 'acceptance' state. Thus, saying that the 'super sm' also recognizes
        #     the pattern that was reached until here can be matched by the 'super set sm'. If not
        #     all shadowing state machines would trigger on the trigger set to an acceptance state,
        #     this means that there is a path to an acceptance state in 'subset sm' that the 'super
        #     sm' has no correspondance. Thus, then the claim to be a super set state machine can
        #     be denied.
        #
        for target_index, trigger_set in sub_state.target_map.get_map().iteritems():
            target_state = self.sub.states[target_index]

            # (*) Require that all shadowing states in the 'super sm' trigger to a valid
            #     target state on all triggers in the trigger set. 
            #     
            #     This is true, if the union of all trigger sets of a shadowing 'super state'
            #     covers the trigger set. It's not true, if not. Thus, use set subtraction:
            for index in SuperSM_StateIndexList:
                if trigger_set.difference(super_trigger_set_union_db[index]).is_empty() == False:
                    return False

            # (*) Collect the states in the 'super set sm' that can be reached via the 'trigger_set'
            super_target_state_index_set = set()
            for super_state in super_state_list:
                super_target_state_index_set.update(super_state.target_map.get_resulting_target_state_index_list(trigger_set))

            # (*) The acceptance condition: 
            if target_state.is_acceptance():
                # (*) Require that all target states in 'super sm' reached by 'trigger_set' are 
                #     acceptance states, otherwise the alleged 'sub sm' has found a pattern which
                #     is matched by it and which is not matched by 'super sm'. Thus, the claim 
                #     that the alleged 'sub sm' is a sub set state machine can be repudiated.
                for index in super_target_state_index_set:
                    if self.super.states[index].is_acceptance() == False: return False

            # (*) No need to go along loops, do not follow paths to states already visited.
            if target_index not in self.visited_state_index_set:
                if self.__dive(target_index, super_target_state_index_set) == False: return False

        # If the condition held for all sub-pathes of all trigger_sets then we can report
        # that the currently investigated sub-path supports the claim that 'sub sm' is a
        # sub set state machine of 'super sm'.
        return True

def do(A, B):
    """RETURNS: True  - if A == SUPERSET of B
                False - if not
    """
    if isinstance(A, StateMachine):
        assert isinstance(B, StateMachine)
        return Checker(A, B).do()

    assert not isinstance(B, StateMachine)
    # (*) Core Pattern ________________________________________________________
    #
    #     (including the mounted post context, if there is one).
    #
    # NOTE: Post-conditions do not change anything, since they match only when
    #       the whole lexeme has matched (from begin to end of post condition).
    #       Post-conditions only tell something about the place where the 
    #       analyzer returns after the match.
    superset_f = Checker(A.sm, B.sm).do()

    if not superset_f: return False

    # NOW: For the core state machines it holds: 
    #
    #                      'core(A)' matches a super set of 'core(B)'.
    #

    # (*) Pre-Condition _______________________________________________________
    #
    if not A.has_pre_context(): 
        # core(A) is a superset of core(B). 
        # A is not restricted. B may be (who cares).
        # => A can match more than B.
        return True

    # NOW: Acceptance of A is restricted by a pre-context.
    #
    if not B.has_pre_context(): 
        # A is restricted by pre-context, B is not.
        # => B can match things that A cannot. 
        return False

    # NOW: A is restricted by pre-context. 
    #      B is restricted by pre-context. 
    #
    #      For A to be a superset of B, A must be less or equally restricted than B.
    #
    #                 pre(B) is a superset of pre(A) 
    # 
    if B.pre_context_trivial_begin_of_line_f:
        if not A.pre_context_trivial_begin_of_line_f:
            # pre(A) can never be a subset of pre(B)
            return False
        else:
            # pre(A) = pre(B) which fulfills the condition
            return True

    # IMPORTANT: The pre-contexts must be mounted at this point!
    #            Call to '.mount_pre_context_sm()' must preceed this function.
    assert A.pre_context_sm is not None
    assert B.pre_context_sm is not None

    # NOW: B is a 'real' pre-context not only a 'begin-of-line'
    #
    # Decision about "pre(A) is subset of pre(B)" done by Checker
    if not A.pre_context_trivial_begin_of_line_f:
        A_pre_sm = A.pre_context_sm
    else:
        # A contains only 'begin-of-line'. Note, however, that 
        # -- newline definition may include '\r\n' so inversion is 
        #    required. 
        A_pre_sm = reverse.do(StateMachine.from_sequence("\n"))
        ## NOT: -- at this point in time we are dealing with transformed 
        ##         machines. So this has also to be transformed.
        ## complete_f, A_pre_sm = transformation.do_state_machine(A_pre_sm)

    return Checker(B.pre_context_sm, A_pre_sm).do()

def do_list(SuperPattern_List, AllegedSubPattern):
    for super_sm in SuperPattern_List:
        if do(super_sm, AllegedSubPattern) == True: return True
    return False
