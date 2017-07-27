# (C) 2010-2012 Frank-Rene Schaefer
from   quex.engine.analyzer.mega_state.template.state     import TemplateState, \
                                                                 PseudoTemplateState
from   quex.engine.analyzer.mega_state.template.candidate import TemplateStateCandidate

from   quex.blackboard import E_Compression

from   itertools       import ifilter, islice
from   operator        import attrgetter

def do(TheAnalyzer, MinGain, CompressionType, AvailableStateIndexList):
    """TEMPLATE COMPRESSION ____________________________________________________

    The 'template compression' algorithm tries to combine the transition maps of
    multiple similar states into a single transition map. The difference in the
    transition maps is dealt with by an adaption table. For example the three
    states

         .---- 'a' --> 2        .---- 'a' --> 2        .---- 'a' --> 2
         |                      |                      |
       ( A )-- 'e' --> 0      ( B )-- 'e' --> 1      ( C )-- 'e' --> 2
         |                      |                      |
         `---- 'x' --> 5        `---- 'x' --> 5        `---- 'y' --> 5

    can be combined into a single template state

                         .----- 'a' --> 2 
                         |               
                      ( T1 )--- 'e' --> Target0 
                         |\               
                         \  `-- 'x' --> Target1
                          \
                           `--- 'y' --> Target2

    where the targets Target0, Target1, and Target2 are adapted. If the template
    has to mimik state A then Target0 needs to be 1, Target1 is 5, and Target2
    is 'drop-out'. The adaptions can be stored in a table:

                                A     B     C
                       Target0  0     1     2
                       Target1  5     5     drop
                       Target2  drop  drop  5

    The columns in the table tell how the template behaves if it operates on
    behalf of a certain state--here A, B, and C. Practically, a state_key is
    associated with each state, e.g. 0 for state A, 1 for state B, and 2 for
    state C.  Thus, a state that is implemented in a template is identified by
    'template index' and 'state key', i.e.

            templated state <--> (template index, state_key)

    The combination of multiple states reduces memory consumption. The
    efficiency increases with the similarity of the transition maps involved.
    The less differences there are in the trigger intervals, the less additional
    intervals need to be added. The less differences there are in target states,
    the less information needs to be stored in adaption tables.

    RESULT ______________________________________________________________________

    The result of analyzis of template state compression is:

              A list of 'TemplateState' objects. 

    A TemplateState carries:

     -- A trigger map, i.e. a list of intervals together with target state
        lists to which they trigger. If there is only one associated target
        state, this means that all involved states trigger to the same target
        state.

     -- A list of involved states. A state at position 'i' in the list has
        the state key 'i'. It is the key into the adaption table mentioned
        above.

    ALGORITHM __________________________________________________________________

    Not necessarily all states can be combined efficiently with each other.
    This class supports an algorithm which finds successively best combinations
    and stops when no further useful combinations can be found. That is, it
    combines always pairs of states, where a state may be an AnalyzerState or a
    TemplateState. The algorithm works on two containers:

    elect_db

       The list of 'elects', i.e. AnalyzerState-s and TemplateState-s which
       survived the selection process so far.

    candidate_list

       A list of TemplateStateCandidate-s. A candidate represents the possible
       combination of two states from the 'elects'. A candidate contains
       information about the possible gain which could be expected from
       combining two particular states from the 'elect_db'.

    To support the homogeneity of the algorithm all AnalyzerState-s are
    translated into PseudoTemplateState-s prior the analysis procedure.

    COMBINATION GAIN VALUE ____________________________________________________

    The measurement of the gain value happens inside a TemplateStateCandidate. 
    It is a function of the similarity of the states. In particular the entries, 
    the drop_outs, and the transition map is considered. 
    ___________________________________________________________________________
    """
    assert CompressionType in (E_Compression.TEMPLATE, E_Compression.TEMPLATE_UNIFORM)
    assert isinstance(MinGain, (int, long, float))

    elect_db       = ElectDB(TheAnalyzer, AvailableStateIndexList)
    candidate_list = CandidateList(elect_db, 
                                   UniformityF = (CompressionType == E_Compression.TEMPLATE_UNIFORM),
                                   MinGain     = float(MinGain))

    while 1 + 1 == 2:
        # -- Try to get the best candidates for combination
        best = candidate_list.pop_best()
        if best is None: break

        # -- Construct the 'elect' from the best combination candidate.
        elect = TemplateState(best)

        # -- The two states combined in 'elect' can now be removed.
        del elect_db[best.state_a.index]
        del elect_db[best.state_b.index]

        # -- Generate new possible combinations with the new elect
        candidate_list.update(elect_db, elect)

        # -- Add new elect to the elects. 
        #    (After 'update' to avoid combination with itself)
        elect_db[elect.index] = elect

    return list(elect_db.iterable_template_states())

class CandidateList(list):
    """________________________________________________________________________

    Maintain list of possible state combinations into a TemplateState. States
    to be combined can be AnalyzerState-s (i.e. PseudoTemplateState-s) or 
    TemplateState-s. For each possible combination a 'gain' needs to be computed.
    This happens during the construction of a 'TemplateStateCandidate'. This
    list maintains all candidates that provide a minimum gain in a sorted 
    order. Thus '.pop_best()' allows to get the best possible combination.
    If '.pop_best()'. returns None, then there is no combination candidate that
    provides the minimum gain.
    ___________________________________________________________________________
    """
    def __init__(self, TheElectDB, UniformityF, MinGain):
        """Compute TemplateStateCandidate-s for each possible combination of 
           two states in the '__elect_db'. If the gain of a combination is less 
           that 'self.__min_gain' then it is not considered.
        """
        self.__min_gain              = MinGain
        self.__uniformity_required_f = UniformityF
        state_list = TheElectDB.values()
        L          = len(state_list)

        def bad_company_announcement(A, B):
            A.bad_company_add(B.index)  # Make a note, that it makes not sense
            B.bad_company_add(A.index)  # to try ony of the two again together.

        # Pre-allocate the result array to avoid frequent allocations
        #
        # NOTE: L * (L - 1) is always even, i.e. dividable by 2.
        #       Proof:
        #       (a) L even = k * 2:     -> k * 2 ( k * 2 - 1 )            = k * k * 4 - k * 2
        #                                = even - even = even
        #       (b) L odd  = k * 2 + 1: -> (k * 2 + 1) * ( k * 2 + 1 - 1) = k * k * 4 + k * 2
        #                                = even + even = even
        # 
        #       => division by two without remainder 
        MaxSize = (L * (L - 1)) / 2
        result  = [None] * MaxSize
        n       = 0
        for i, i_state in enumerate(state_list):
            for k_state in islice(state_list, i + 1, None):

                if     self.__uniformity_required_f                                               \
                   and (   i_state.uniform_DropOut           != k_state.uniform_DropOut           \
                        or i_state.uniform_entry_OpList != k_state.uniform_entry_OpList): 
                    bad_company_announcement(i_state, k_state)
                    continue

                candidate = TemplateStateCandidate(i_state, k_state)

                if candidate.gain >= self.__min_gain:
                    result[n] = candidate
                    n += 1
                else:
                    bad_company_announcement(i_state, k_state)

        if n != MaxSize:
            del result[n:]

        # Sort according to delta cost
        self.extend(result)
        self.sort(key=attrgetter("gain")) # 'best' must be at end

    def update(self, TheElectDB, NewElect):
        """Adapt the CandidateList to include candidates of combinations with
           the NewElect.
        """
        assert isinstance(NewElect, TemplateState)
        assert NewElect.index not in TheElectDB    # Avoid combination with self.

        # Avoid extensive 'appends' by single allocation (see initial computation)
        MaxIncrease = len(TheElectDB) 
        n           = len(self)
        MaxSize     = len(self) + MaxIncrease
        self.extend([None] * MaxIncrease)
        ImplementedStateIndexSet = NewElect.implemented_state_index_set()

        for state in TheElectDB.itervalues():
            if self.__uniformity_required_f:
                # Rely on __eq__ operator (used '=='). '!=' uses __ne__ 
                if   not (state.drop_out == NewElect.drop_out):    continue
                elif not (state.entry.is_uniform(NewElect.entry)): continue


            # Do not try to combine states that have proven to be 'bad_company'.
            if       state.index in NewElect.bad_company():                                  continue
            elif not NewElect.bad_company().isdisjoint(state.implemented_state_index_set()): continue
            elif not state.bad_company().isdisjoint(ImplementedStateIndexSet):               continue
            # IMPOSSIBLE: NewElect.index in state.bad_company() 
            #             because when 'state' was created, 'NewElect' did not exist.
            candidate = TemplateStateCandidate(NewElect, state)

            if candidate.gain >= self.__min_gain:
                self[n] = candidate
                n += 1
            else:
                # Mention the states for which the other does not combine properly
                state.bad_company_add(NewElect.index)
                NewElect.bad_company_add(state.index)

        if n != MaxSize:
            del self[n:]

        self.sort(key=attrgetter("gain")) # 'best' must be at end

    def pop_best(self):
        """Determines the two states that result in the greatest gain if they are 
        combined into a TemplateState. 

        If no combination has a "gain >= self.__min_gain", then None is
        returned. This is ensured, by not letting any entry enter the
        CandidateList, where 'gain < self.__min_gain'.

        RETURNS: TemplateStateCandidate if combination of states with the 
                                        greatest gain. 
                 None, if there is no more.
        """
        if len(self) == 0: return None

        # (*) The entry with the highest gain is at the tail of the list.
        best = self.pop()

        # (*) Remove any TemplateStateCandidate that combines 'i' or 'k' which
        #     are now implemented by 'best'. No other candidate that combines
        #     'i' and 'k' shall get a chance.
        # 
        # If 'i' or 'k' refer to an AnalyzerState, then any combination where 'i'
        # or 'k' is involved is removed from the candidate list. The 'best', let it 
        # have state index 'p', is the only state that contains now 'i' and 'k'. Any
        # state, with index 'q', that combines with it does not contain 'i' and 
        # 'k'. And, on the event of combining 'p' and other 'q' all other combinations
        # related to 'p' are deleted, thus all other combinations that contain
        # 'i' and 'k' are impossible.
        #
        # => The consideration of 'implemented_state_index_set' is not necessary.
        self.__delete_references(best.state_a.index, best.state_b.index)

        return best

    def __delete_references(self, I, K):
        """Delete all related entries in the 'CandidateList' that relate to
        states I and K. This function is used after the decision has been made
        that I and K are combined into a TemplateState. None of them can be
        combined with another state anymore.
        """

        done_set = (I, K)
        i        = len(self) - 1
        while i >= 0:
            entry = self[i]
            if entry.state_a.index in done_set or entry.state_b.index in done_set:
                del self[i]
            i -= 1

        return 

class ElectDB(dict):
    """________________________________________________________________________

    Database of states which are either AnalyzerState-s (i.e.
    PseudoTemplateState-s) from the original state machine, or TemplateState-s
    which implement two or more of such AnalyzerState-s. 
    
    Whenever two states are combined into one TemplateState, the two
    implemented states themselves are removed from the ElectDB and the
    implementing TemplateState is entered.
    ___________________________________________________________________________
    """
    def __init__(self, TheAnalyzer, AvailableStateIndexList):
        """At the beginning all AnalyzerState-s are elected (and they remain
        elected if they cannot be efficiently combined). However, some states
        may NOT be considered as they are:
        
           NOT: -- states with empty transition maps.
                -- states which are no longer available for implementation.
                -- the init-state..
        """
        StateDB        = TheAnalyzer.state_db
        InitStateIndex = TheAnalyzer.init_state_index
        # x[0] = state_index, x[1] = state
        condition = lambda x:     x[0] in AvailableStateIndexList \
                              and x[1].transition_map is not None \
                              and len(x[1].transition_map) != 0   \
                              and x[1].index != InitStateIndex 

        # Represent AnalyzerState-s by PseudoTemplateState-s so they behave
        # uniformly with TemplateState-s.
        result = dict((state_index, PseudoTemplateState(state, TheAnalyzer.drop_out)) \
                      for state_index, state in ifilter(condition, StateDB.iteritems()) )

        self.update(result)

    def iterable_template_states(self):
        for state in self.itervalues():
            if isinstance(state, TemplateState):
                yield state


