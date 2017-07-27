# (C) 2005-2011 Frank-Rene Schaefer
import quex.engine.state_machine.index as     state_machine_index
from   quex.engine.state_machine.core  import StateMachine, State
from   itertools   import islice, ifilter, chain
from   collections import defaultdict

class HopcroftMinization:
    """Combine sets of states that are equivalent. 

       Example:
                      .-- 'a' --->( 2 )--- 'a' --.           
                     /                            \ 
                  ( 1 )                         (( 3 ))
                     \                            /
                      '-- 'b' --->( 4 )--- 'a' --'

        Where (( 3 )) is an acceptance state. Triggers exactly the same as 

                  ( 1 )--- [ab] --->( 2 )--- 'a' -->(( 3 ))

        Therefore state 2 and 4 from the first machine can be combined and one
        state can be spared. In practical applications the Hopcroft Minization
        can tremendously reduce the size of a state machine.

        The mathematics behind it can be found at 
        
                    http://en.wikipedia.org/wiki/DFA_minimization

        The algorithm below adapted the hopcroft algorithm in order to cope
        with the large trigger sets.

        (1) Basic Idea of Hopcroft Minimization _______________________________

            (i) Define the initial state sets:

                States that accept patterns --> each in the corresponding acceptance state set
                Non-acceptance states       --> all together in one state set.

                All states are marked 'to do', i.e. to be considered.

            (ii) Loop over all 'to do' state sets:

                 if all states in a state set trigger to the same target state sets,
                 then they are equivalent. 

                 if then, the target state set is 'done', the state set can be 
                 considered done. (done = delete from 'to do').

                 if not, the state set needs to be split. Add the new one to to do list.

             (iii) Goto (ii) until to do list is empty.


        (2) Frank-Rene Schaefer's Adaptations _________________________________

        In many cases, already the difference in target_sets reveals that a
        state set needs to be split. The term 'homogenous' is defined for the
        algorithm below:

             homogenous: A state set is homogenous, if and only if all states in the
                         state set trigger to target states that belong to the
                         same state set.

        The split is then separated into two repeated phases:

                 (1) Split until all non-homogenous state sets disappear.
                 (2) Split all states in the current to do list.
                 Repeat
    """
    def __init__(self, StateMachine):
        self.sm = StateMachine

        # (*) Helper Mappings:
        #     map: state index --> index of the state set that contains it
        self.map = {} 
        #    from_map: target index --> from what states the target state is entered
        self.from_map = dict([(i, []) for i in self.sm.states.iterkeys()])
        for origin_index, state in self.sm.states.iteritems():
            for target_index in state.target_map.get_map().iterkeys():
                self.from_map[target_index].append(origin_index)
        #    to_map: state_index --> list of target states
        self.to_map = dict([(i, self.sm.states[i].target_map.get_map()) for i in self.sm.states.keys()])

        # (*) Initial split 
        #     --> initial state_set_list
        self.__todo           = set([])  # state sets to be investigated
        self.__non_homogenous = set([])  # suspects to trigger to different target state sets
        self.state_set_list = []
        self.initial_split()

        # (*) Run Adapted Hopcroft Minimization
        #     Split until only equivalent state sets are present
        self.run()

    def run(self):
        # It is conceivable (and appears in practise) that target state sets 
        # which are undone point to each other in loops. Therefore, they can never
        # be marked undone. Check against this by means of a change flag.
        change_f = True
        while len(self.__todo) != 0 and change_f:
            # (A) Phase: Split all state sets that are non-homogenous, 
            #            i.e. trigger to different target state sets.
            while len(self.__non_homogenous) != 0:
                for i in list(self.__non_homogenous):
                    self.pre_split(i)

            # (B) Phase: Split all state sets (which are 'homogenous')
            #            if they trigger to target state sets with different trigger sets.
            change_f = False
            for i in list(sorted(self.__todo, key=lambda i: len(self.state_set_list[i]), reverse=True)): 
                # (See also below after split()) 
                # if i in self.__non_homogenous: continue

                if self.split(i): 
                    change_f = True           
                    # Without a split, there can be no new non-homogenous anyway.
                    if len(self.__non_homogenous) != 0: break

            # Double Check: All non-homogenouss must be on todo-list.
            assert self.__non_homogenous.issubset(self.__todo)
            
    def pre_split(self, StateSetIndex):
        """Separate state_set into two state sets:

           (1) The set of states that have the same target state sets,
               as a arbitrarily chosen prototype := 'homogenous'.
           (2) Those who differ from the prototype.

           The state set that matches the prototype can be considered 
           'homogenous' since it triggers for sure to the same target
           state sets. The prototype is packed into a separate state
           set. The other states remain in the old state set. 

           If there was a prototype that separated the set then the
           what does not match the prototype is possibly not-homogenous.

           RETURNS: True  Split happened.
                          The old state set is possible non-homogenous.
                    False No split happened.
                          The old state set and the prototype are
                          identical.
        """
        assert StateSetIndex in self.__todo
        assert StateSetIndex in self.__non_homogenous
        state_set = self.state_set_list[StateSetIndex]
        assert len(state_set) > 1

        # NOTE: Previous experiments have shown, that the split does **not** become
        #       faster, if all non-homogenous states are identified at once. The overhead
        #       of the book-keeping is much higher than the benefit. Also, statistics
        #       have shown that a very large majority of cases results in one or two
        #       state sets only.

        def get_target_state_set_list(StateIndex):
            target_list = self.to_map[StateIndex].iterkeys()
            return set([self.map[i] for i in target_list])

        state_set = self.state_set_list[StateSetIndex]
        prototype = get_target_state_set_list(state_set[0])

        match_set = [ state_set[0] ] 
        for state_index in islice(state_set, 1, None):
            if prototype == get_target_state_set_list(state_index): 
                match_set.append(state_index)

        # NOW: match_set = homogenous
        #      remainder = possibly not homogenous

        # To split, or not to split ...
        if len(match_set) == len(state_set): 
            self.non_homogenous_remove(StateSetIndex)
            return False 

        # The match set is now extracted from the original state set

        # -- Delete all states that are put into other sets from the original state set
        # -- Add the new set to the state_set_list
        # -- Sets of size == 1: 1. are homogenous (trigger all to the same target state sets).
        #                       2. cannot be split further => done.
        self.__extract(StateSetIndex, state_set, match_set)
        # (by default, the new set is not added to non_homogenous, which is true.)

        # -- Neither the new, nor the old state set can be labeled as 'done' because
        #    the exact transitions have not been investigated, yet. The only exception
        #    if len(state_set) == 1, because then it cannot be split anyway.
        #    if len(match_set) == 1, it is not added to the todo list by __add_state_set().
        # -- Again, even if the target state sets of the prototype is 'done' it is
        #    not guaranteed, that all states in the matching set transit on the same
        #    transitions to the done target sets.
        if len(state_set) == 1: 
            self.non_homogenous_remove(StateSetIndex)
            self.todo_remove(StateSetIndex)

        # -- The state set is split, thus state sets triggering to it may be non-homogenous
        #    (this may include recursive states, so this has to come after anything above)
        self.__check_nh_dependencies(chain(match_set, state_set))

    def split(self, StateSetIndex):
        """RETURNS:  False   if StateSet does not need to be split up any further.
                     True    if the state set requires a split.
        """
        pass#assert StateSetIndex in self.__todo
        state_set = self.state_set_list[StateSetIndex]
        #
        N         = len(state_set)
        pass#assert N != 0, "State set of size '0'. List = " + repr(self.state_set_list)
        # only one state in state set => no change possible
        if N == 1: 
            self.todo_remove(StateSetIndex)
            return False    

        def normalized_map(DB):
            result = {}
            for target_index, trigger_map in DB.iteritems():
                # Target in terms of equivalent state sets
                target = self.map[target_index]
                entry  = result.get(target)
                if entry is None: entry = trigger_map 
                else:             entry = entry.union(trigger_map)
                result[target] = entry
            return result

        # -- choose one arbitrary state (for example state 0) as a prototype
        #    which is compared against the remaining states in the state set.
        prototype_i          = state_set[0]
        prototype_map        = normalized_map(self.to_map[prototype_i])
        equivalent_state_set = [ prototype_i ] 

        if len(prototype_map) == 0:
            # If there are no target states, then there can be no split.
            # The state set is done.
            self.todo_remove(StateSetIndex)
            return False

        # Since all state sets are homogenized at the entry of this function
        # It can be assumed that the prototype contains all target_set indices
        # Loop over all remaining states from state set
        for state_index in islice(state_set, 1, None):
            state_map = normalized_map(self.to_map[state_index])
            for target in prototype_map.iterkeys():
                if not prototype_map[target].is_equal(state_map[target]): break
            else:
                equivalent_state_set.append(state_index)

        # The transitions have been investigated. Now, we can judge whether 
        # a state set is done. If all target state sets are done, then 
        # no further split can happen and the state set itself is done.
        all_done_f = self.__todo.isdisjoint(prototype_map.keys())  

        if len(equivalent_state_set) == len(state_set): 
            self.non_homogenous_remove(StateSetIndex)
            if all_done_f: self.todo_remove(StateSetIndex)
            return False

        new_index = self.__extract(StateSetIndex, state_set, equivalent_state_set)

        # -- The state set is split, thus state sets triggering to it may be non-homogenous
        #    (this may include recursive states, even the equivalent state set, 
        #     so this has to come after anything above)
        self.__check_nh_dependencies(chain(state_set, equivalent_state_set))

        if len(state_set) == 1: 
            self.non_homogenous_remove(StateSetIndex)
            self.todo_remove(StateSetIndex)
        else:
            self.non_homogenous_add(StateSetIndex)

        if all_done_f and len(equivalent_state_set) != 1: 
            # If the new state state is of size one, it has not been added to 'todo' list
            self.todo_remove(new_index)

        return True

    def initial_split(self):
        """Generates initial sets of states. After the initial split, state
        sets are only split further and never combined. The initial split 
        separate states that must never, ever, be combined. The knowledge about
        combinability lies where the operations are known. The function

                   .hopcroft_combinability_key()

        determines a key for a state. Two states have the same key, if and only
        if they are combinable during the initial split of hopcroft minimization.
        """
        distinguisher_db = defaultdict(list)
        for state_index, state in self.sm.states.iteritems():
            key = state.single_entry.hopcroft_combinability_key()
            distinguisher_db[key].append(state_index)

        for state_set in distinguisher_db.itervalues():
            i = self.__add_state_set(state_set)
            if len(state_set) != 1: self.non_homogenous_add(i)

    def todo_add(self, StateSetIndex):
        self.__todo.add(StateSetIndex)

    def todo_remove(self, StateSetIndex):
        assert StateSetIndex not in self.__non_homogenous
        self.__todo.remove(StateSetIndex)

    def non_homogenous_remove(self, StateSetIndex):
        if StateSetIndex in self.__non_homogenous: 
            self.__non_homogenous.remove(StateSetIndex)

    def non_homogenous_add(self, StateSetIndex):
        self.__non_homogenous.add(StateSetIndex)

    def __extract(self, MotherIdx, mother_state_set, NewStateSet):
        # -- Delete states from the mother set
        for state_index in NewStateSet:
            del mother_state_set[mother_state_set.index(state_index)]

        # (if all was to be extracted then the mother state set could remain
        #  and the new state set be represented by the mother_state_set)
        assert len(mother_state_set) != 0

        # -- Create a new state set in state_set_list
        return self.__add_state_set(NewStateSet)

    def __check_nh_dependencies(self, StateSetIterable):
        """If a state set is split, then every state set that triggered to it may be non-homogenous
           origin_state_list = list of states that trigger to states inside mother_state_set.

           PROPOSAL:

            if len(StateSet) == 1:
                # (*) This is the short version of the algorithm above, if there is only one state
                origin_state_list = self.from_map[StateSet[0]]
                if len(origin_state_list) != 1:
                    considerable_set  = set([self.map[i] for i in origin_state_list])
                    for state_set_index in ifilter(lambda x: len(self.state_set_list[x]) != 1, considerable_set):
                        self.non_homogenous_add(state_set_index)
                else:
                    # (*) This is the short version, if there is only one accept command
                    state_set_index = self.map[origin_state_list[0]]
                    if len(self.state_set_list[state_set_index]) != 1:
                        self.non_homogenous_add(state_set_index)
        """
        # (*) This is the full general version of the dependency check
        considerable_set = set()
        for state_index in StateSetIterable:
            origin_state_list = self.from_map[state_index]
            considerable_set.update([self.map[i] for i in origin_state_list])

        # Exception: state set is of size 'one', then it cannot be non-homogenous
        for state_set_index in ifilter(lambda x: len(self.state_set_list[x]) != 1, considerable_set):
            # A state_set cannot be 'done' if one of the target state_sets is not done
            # Such a case, would be caught by the assert inside 'non_homogenous_add'.
            self.non_homogenous_add(state_set_index)

    def __add_state_set(self, NewStateSet):
        """Add a new state set to the pool. Return the 'index' of the
           newly added member of the pool. If the state set if of size > 1
           then it is added to the todo-list. If not, it cannot be split
           and therefore is already done.
        """
        assert len(NewStateSet) != 0

        new_index = len(self.state_set_list)

        self.state_set_list.append(NewStateSet)

        # Register for each state in NewStateSet its containing set
        for state_index in NewStateSet:
            self.map[state_index] = new_index

        # Any state set with more than one state needs to be investigated
        if len(NewStateSet) != 1: self.todo_add(new_index)

        return new_index

def do(SM, CreateNewStateMachineF=True, Class_StateMachine=StateMachine, Class_State=State):
    """Reduces the number of states according to equivalence classes of states. It starts
       with two sets: 
       
            (1) the set of acceptance states, 
                -- these states need to be split again according to their acceptance id.
                   Acceptance of state machine A is not equal to acceptance of 
                   state machine B.
            (2) the set of non-acceptance states.
       
       Whenever one finds a state in a state set that triggers on the same characters to 
       a different state set, the set has to be split in two sets of states:

           -- the set of states that trigger on trigger 'X' to state set 'K'
           -- the set of states that trigger on trigger 'X' to another state set

       The original state set is replaced by the two new ones. This algorithm is 
       repeated until the state sets do not change anymore.
    """        
    result = HopcroftMinization(SM)

    if CreateNewStateMachineF: return create_state_machine(SM, result, Class_StateMachine, Class_State)
    else:                      return adapt_state_machine(SM, result)

def create_state_machine(SM, Result, Class_StateMachine, Class_State):
    # If all states are of size one, this means, that there were no states that
    # could have been combined. In this case a simple copy of the original
    # state machine will do.
    if len(filter(lambda state_set: len(state_set) != 1, Result.state_set_list)) == 0:
        return SM.clone()
    
    # Define a mapping from the state set to a new target state index
    #
    # map:  state_set_index  --->  index of the state that represents it
    #
    map_new_state_index = dict([(i, state_machine_index.get()) for i in xrange(len(Result.state_set_list))])
                
    # The state set that contains the initial state becomes the initial state of 
    # the new state machine.   
    state_set_containing_initial_state_i = Result.map[SM.init_state_index]
    new_init_state_index                 = map_new_state_index[state_set_containing_initial_state_i]

    result = StateMachine(new_init_state_index)

    # Ensure that each target state index has a state inside the state machine
    # Build up the state machine out of the state sets
    for state_set_idx, state_set in enumerate(Result.state_set_list):

        new_state_index = map_new_state_index[state_set_idx]

        # Merge all operations of states which are combined into one.
        result.states[new_state_index] = Class_State.from_state_iterable(
                                                SM.states[i] for i in state_set)

    for state_set_idx, state_set in enumerate(Result.state_set_list):
        # The prototype: States in one set behave all equivalent with respect to target state sets
        # thus only one state from the start set has to be considered.      
        prototype    = SM.states[state_set[0]]
        representative = result.states[map_new_state_index[state_set_idx]]

        # The representative must have all transitions that the prototype has
        for target_state_index, trigger_set in prototype.target_map.get_map().iteritems():
            target_state_set_index = Result.map[target_state_index]
            target_index           = map_new_state_index[target_state_set_index]
            representative.add_transition(trigger_set, target_index)

    return result    

def adapt_state_machine(sm, Result):
    # If all states are of size one, this means, that there were no states that
    # could have been combined. In this case nothing is to be done.
    for dummy in (state_set for state_set in Result.state_set_list if len(state_set) != 1):
        break
    else:
        return sm
    
    # We know, that all states in a state set are equivalent. Thus, all but one
    # of each set can be thrown away.
    replacement_dict = {}
    for state_set in Result.state_set_list:
        if len(state_set) == 1: 
            continue

        # Merge all core information of the states inside the state set.
        # Do not delete the init state
        if sm.init_state_index in state_set: prototype_i = sm.init_state_index
        else:                                prototype_i = state_set[0]  

        # The prototype takes over the role of all
        sm.states[prototype_i].single_entry.merge_list(
            sm.states[i].single_entry for i in state_set if i != prototype_i)

        replacement_dict.update(
            (i, prototype_i) for i in state_set if i != prototype_i)

    # Replace the indices of the thrown out states
    for state_idx in replacement_dict.iterkeys():
        del sm.states[state_idx]

    for state in sm.states.itervalues():
        state.target_map.replace_target_indices(replacement_dict)

    return sm    

