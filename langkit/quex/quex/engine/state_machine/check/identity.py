from quex.blackboard import E_PreContextIDs
from quex.engine.state_machine.core import StateMachine

class Checker:
    def __init__(self, SM0, SM1):
        """Checks whether the set of patterns matched by SM0 is identical to the
           set of patterns matched by SM1.

           RETURNS: 'True'  if so,
                    'False' if not.
        """
        self.sm1 = SM1
        self.sm0 = SM0
        self.visited_state_index_db = {}

    def do(self):
        return self.__dive(self.sm1.init_state_index, [self.sm0.init_state_index])

    def __dive(self, SM1_StateIndex, SM0_StateIndexList):
        """SM1_StateIndex:     state index in SM1

           SM0_StateIndexList: list of states in the 'sm0 set' state machine that
                               was reached by the same trigger set as SM1_StateIndex.      
                               They are the set of states that can 'mimik' the current
                               state indexed by 'SM1_StateIndex'.
        """
        # (*) Determine the states behind the indices
        sm1_state      = self.sm1.states[SM1_StateIndex]
        sm0_state_list = map(lambda index: self.sm0.states[index], SM0_StateIndexList)
        #     Bookkeeping
        self.visited_state_index_db[SM1_StateIndex] = True
        #     Union of all triggers were the 'mimiking' sm0 states trigger.
        #     (For speed considerations, keep it in prepared, so it does not have to 
        #      be computed each time it is required.)
        sm0_trigger_set_union_db = {} 
        for index in SM0_StateIndexList:
            sm0_trigger_set_union_db[index] = self.sm0.states[index].target_map.get_trigger_set_union()

        sm1_trigger_set_union = sm1_state.target_map.get_trigger_set_union()

        # (*) Here comes the condition:
        #
        #     For every trigger (event) in the 'sm1 state' that triggers to a follow-up state
        #     there must be pendant triggering from the mimiking 'sm0 states'.
        #
        #     That is: 
        #     -- No 'mimiking sm0 state' is allowed to trigger on something beyond
        #        the trigger_set present on sm1, and vice versa.
        for index in SM0_StateIndexList:
            if not sm0_trigger_set_union_db[index].is_equal(sm1_trigger_set_union): 
                return False

        #     -- All 'mimiking sm0 states' must trigger on the given trigger_set to 
        #        a subsequent state of the same 'type' as the 'sm1 state'.
        for target_index, trigger_set in sm1_state.target_map.get_map().items():
            target_state = self.sm1.states[target_index]

            # (*) Collect the states in the 'sm0' that can be reached via the 'trigger_set'
            sm0_target_state_index_list = []
            for sm0_state in sm0_state_list:
                for index in sm0_state.target_map.get_resulting_target_state_index_list(trigger_set):
                    if index in sm0_target_state_index_list: continue
                    sm0_target_state_index_list.append(index)

            # (*) If there is one single state in the collection of follow-up states in sm0
            #     that has not the same type as the target state, then 'sm0' and 'sm1' are 
            #     not identical.
            if not self.__correspondance(target_state, sm0_target_state_index_list): 
                return False

            # (*) No need to go along loops, do not follow paths to states already visited.
            if not self.visited_state_index_db.has_key(target_index):
                if self.__dive(target_index, sm0_target_state_index_list) == False: return False

        # If the condition held for all sub-pathes of all trigger_sets then we can reports
        # that the currently investigated sub-path supports the claim that 'sm1 sm' is a
        # sub set state machine of 'sm0 sm'.
        return True

    def __correspondance(self, S1, S0List):
        """Checks whether all states in SList are of the same type as S0. 
           (With respect to the criteria of out algorithm.)
        """
        for index in S0List:
            S0 = self.sm0.states[index] # core of the 'sm0' state

            if       S0.is_acceptance() \
                 !=  S1.is_acceptance():                                      return False
            elif    (S0.pre_context_id() == E_PreContextIDs.NONE) \
                 != (S1.pre_context_id() == E_PreContextIDs.NONE):            return False
            elif    (S0.pre_context_id() == E_PreContextIDs.BEGIN_OF_LINE) \
                 != (S1.pre_context_id() == E_PreContextIDs.BEGIN_OF_LINE):   return False
            elif     S0.input_position_store_f() \
                 !=  S1.input_position_store_f():                             return False
            elif     S0.input_position_restore_f() \
                 !=  S1.input_position_restore_f():                           return False

        return True

def do(Pattern0, Pattern1):
    if isinstance(Pattern0, StateMachine):
        assert isinstance(Pattern1, StateMachine)
        return Checker(Pattern0, Pattern1).do()

    assert not isinstance(Pattern1, StateMachine)

    # Check whether Pattern0 and Pattern1 are identical, i.e they match exactly the same patterns 
    # and provide exactly the same behavior of the lexical analyzer.
    identity_f = Checker(Pattern0.sm, Pattern1.sm).do()

    if not identity_f: return False
    # NOTE: Post-conditions are handled in the identity check already.
    #
    # Pre-Condition: 
    #
    #       If only one state machine is pre-conditioned, then they are not identical
    if (Pattern0.pre_context_sm_to_be_inverted is not None) != (Pattern1.pre_context_sm_to_be_inverted is not None): 
        return False
    else:
        if Pattern0.pre_context_sm_to_be_inverted is not None:
            # Both are pre-conditioned by state machine
            assert Pattern1.pre_context_sm_to_be_inverted != -1
            # Pre-condition by 'begin of line' excludes other pre-conditions.
            assert not Pattern0.pre_context_trivial_begin_of_line_f
            assert not Pattern1.pre_context_trivial_begin_of_line_f
            return Checker(Pattern0.pre_context_sm_to_be_inverted, Pattern1.pre_context_sm_to_be_inverted).do()

    # Here: None Pattern0 and Pattern1 is dependent on pre-context state machine
    if  Pattern0.pre_context_trivial_begin_of_line_f != Pattern1.pre_context_trivial_begin_of_line_f: 
        return False
    else:
        if Pattern0.pre_context_trivial_begin_of_line_f:
            # Both are pre-conditioned by 'begin of line' => judgement remains as above
            assert Pattern1.pre_context_trivial_begin_of_line_f
            # Pre-condition by 'begin of line' excludes other pre-conditions.
            assert Pattern0.pre_context_sm_to_be_inverted is None
            assert Pattern1.pre_context_sm_to_be_inverted is None
            # Here: identity_f == True
            return True

    # Here: None Pattern0 and Pattern1 is dependent on pre-context by 'begin of line

    # If there is no pre-condition at all, then the old judgement holds
    return True

