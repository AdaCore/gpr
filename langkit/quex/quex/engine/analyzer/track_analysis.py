"""
________________________________________________________________________________
Path Analysis

For each state in the state machine (virtually) all paths are considered which
guide through it. The analysis is concerned with acceptance and input position
storage and restorage. A path corresponds to a possible sequence of state
transition as consequence of a sequence of appearing characters.

The main result is a map that tells what consequences a path has in a specific 
state, i.e.:

    trace_db:        state index --> consequences of paths

As a by-product a 'path_element_db' is developed which tells for a given 
state what states lie on the path to it, i.e.

    path_element_db: state index --> indices of states which lie
                                     on the path to that state.
________________________________________________________________________________
EXPLANATION:

Each path is related to a list of 'AcceptCondition' objects. An
AcceptCondition consists of the following main members:

    .acceptance_id           -- Pattern that can be accepted.
    
    .pre_context_id          -- PreContext required for acceptance.
    
    .accepting_state_index   -- State where the last acceptance of 'acceptance_id' 
                                appeared.
    
    .positioning_state_index -- State where the input position had to be 
                                stored.

    .transition_n_since_positioning -- ... self explanatory

Different acceptance_id-s may be involved in the same state, 

  (i)  because the state may be reached through different paths or 

  (ii) because the winning pattern may depend on what pre-context is 
       fulfilled. 
  
The position storing state is involved when 

  (i)  a post-context needs to reset the input pointer to the end of 
       the core lexeme or 

  (ii) an acceptance detected on a previous state is winning. In that
       case the positioning state index is equal to the accepting
       state index.

The eventualities of a path is represented by a list of AcceptCondition 
objects. It behaves like a list where sorting order tells the precedence 
of winners. The first pattern where the pre_context is fulfilled wins.

     AcceptSequence: [ 
          #               length       acceptance_id   pre_context_id   ...
          AcceptCondition(67,          2,               ...)
          AcceptCondition(32,          1,               ...)
          AcceptCondition(7,           None,            ...)
     ]

Note, that length has preceedence over acceptance_id. For this reason, greater
acceptance_id-s may have precedence of higher once--if it matches a longer
lexeme.

Since there are potentially mutiple paths to a state, there is a list of
lists of AcceptCondition objects. Thus the mapping:

    map:  state index --> consequences of paths

is represented by 

    map:  state index --> list(list(AcceptCondition)) 
________________________________________________________________________________

Based on this information on AcceptCondition-s requirements on entry and 
drop_out behaviors of a state can be derived. This is done by module 'core.py'.
________________________________________________________________________________
NOTE:

It is possible, that not all paths are walked along. If a state is reached 
though a different path but with the same consequence as another path through
that state, then the further investigation of that path is aborted. All 
possible informations have been gathered, no need to investigate further.
________________________________________________________________________________
(C) 2010-2014 Frank-Rene Schaefer
ABSOLUTELY NO WARRANTY
________________________________________________________________________________
"""
from   quex.engine.state_machine.state.single_entry import SeAccept, SeStoreInputPosition
from   quex.engine.misc.tree_walker        import TreeWalker
from   quex.engine.analyzer.paths_to_state import PathsToState
from   quex.blackboard                     import E_IncidenceIDs, \
                                                  E_PreContextIDs, \
                                                  E_TransitionN

from   itertools   import izip
from   copy        import copy
from   zlib        import crc32


def do(SM, ToDB):
    """RETURNS: Acceptance trace database:

                map: state_index --> PathsToState

    ___________________________________________________________________________
    This function walks down almost each possible path trough a given state
    machine.  During the process of walking down the paths it develops for each
    state its list of _Trace objects.
    ___________________________________________________________________________
    IMPORTANT:

    There is NO GUARANTEE that the paths from acceptance to 'state_index' or
    the paths from input position storage to 'state_index' are complete! The 
    calling algorithm must walk these paths on its own.

    This is due to a danger of exponential complexity with certain setups. Any
    path analysis is dropped as soon as a state is reached with an equivalent
    history.
    ___________________________________________________________________________
    """
    def print_path(x):
        print x.state_index, " ",
        if x.parent is not None: print_path(x.parent)
        else:                    print

    class TraceFinder(TreeWalker):
        """Determines _Trace objects for each state. The heart of this function is
           the call to '_Trace.next_step()' which incrementally develops the 
           acceptance and position storage history of a path.

           Recursion Terminal: When state has no target state that has not yet been
                               handled in the 'path' in the same manner. That means,
                               that if a state appears again in the path, its trace
                               must be different or the recursion terminates.
        """
        def __init__(self, state_machine, ToDB):
            self.sm              = state_machine
            self.empty_list      = []
            self.to_db           = ToDB
            self.result          = dict((i, []) for i in self.sm.states.iterkeys())
            self.path            = []
            # Each state is a 'on the path to itself', i.e. it holds
            # 'i in path_element_db[i]'.
            self.path_element_db = dict((i,set([i])) for i in self.sm.states.iterkeys())
            TreeWalker.__init__(self)

        def on_enter(self, Args):
            PreviousTrace = Args[0]
            StateIndex    = Args[1]
            # print "#sp:", StateIndex
            # print "#pt:", PreviousTrace

            # (*) Update the information about the 'trace of acceptances'
            State = self.sm.states[StateIndex]

            if len(self.path) == 0: trace = _Trace(self.sm.init_state_index)
            else:                   trace = PreviousTrace.next_step(StateIndex, State) 

            target_index_list = self.to_db[StateIndex]
            for state_index in self.path:
                self.path_element_db[StateIndex].update(self.path)

            # (*) Recursion Termination:
            #
            # If a state has been analyzed before with the same trace as result,  
            # then it is not necessary dive into deeper investigations again. All
            # of its successor paths have been walked along before. This catches
            # two scenarios:
            #   
            # (1) Loops: A state is reached through a loop and nothing 
            #            changed during the walk through the loop since
            #            the last passing.
            # 
            #            There may be connected loops, so it is not sufficient
            #            to detect a loop and stop.
            #
            # (2) Knots: A state is be reached through different branches.
            #            However, the traces through those branches are
            #            indifferent in their positioning and accepting 
            #            behavior. Only one branch needs to consider the
            #            subsequent states.
            #
            #     (There were cases where this blew the computation time
            #      see bug-2257908.sh in $QUEX_PATH/TEST).
            # 
            existing_trace_list = self.result.get(StateIndex) 
            if len(existing_trace_list) != 0:
                end_of_road_f = (len(target_index_list) == 0)
                for pioneer in existing_trace_list:
                    if not trace.is_equivalent(pioneer, end_of_road_f): 
                        continue
                    if trace.has_parent(pioneer):
                        # Loop detected -- Continuation unnecessary. 
                        # Nothing new happened since last passage.
                        # If trace was not equivalent, the loop would have to be stepped through again.
                        #if StateIndex == 6: print "#has parent", pioneer
                        return None
                    else:
                        # Knot detected -- Continuation abbreviated.
                        # A state is reached twice via two separate paths with
                        # the same positioning_states and acceptance states. The
                        # analysis of subsequent states on the path is therefore
                        # complete. Almost: There is now alternative paths from
                        # store to restore that must added later on.
                        #if StateIndex == 6: print "#knot", pioneer
                        return None

            # (*) Mark the current state with its acceptance trace
            self.result[StateIndex].append(trace)

            # (*) Add current state to path
            self.path.append(StateIndex)

            # (*) Recurse to all (undone) target states. 
            return [(trace, target_i) for target_i in target_index_list ]

        def on_finished(self, Args):
            # self.done_set.add(StateIndex)
            self.path.pop()

    trace_finder = TraceFinder(SM, ToDB)
    trace_finder.do((None, SM.init_state_index))

    result = dict((key, PathsToState(trace_list)) 
                  for key, trace_list in trace_finder.result.iteritems())
    return result, trace_finder.path_element_db

class _Trace(object):
    """
    An object of this class documents the impact of actions that happen
    along ONE specific path from the init state to a specific state. 
    ___________________________________________________________________________
    EXPLANATION:

    During a path from the init state to 'this state', the following things
    may happen or may have happened:

         -- The input position has been stored in a position register
            (for post context management or on accepting a pattern).

         -- A pattern has been accepted. Acceptance may depend on a
            pre-context being fulfilled.

    Storing the input position can be a costly operation. If the length of
    the path from storing to restoring can be determined from the number of
    transitions, then it actually does not have to be stored. Instead, it
    can be obtained by 'input position -= transition number since
    positioning.' In any case, the restoring of an input position is
    triggered by an acceptance event.

    Acceptance of a pattern occurs, if one drops out of a state, i.e. there
    are no further transitions possible. Later analysis will focus on these
    acceptance events. They are stored in a sorted member '.acceptance_trace'.

    The sort order of the acceptance trace reflects the philosophy of
    'longest match'. That, is that the last acceptance along a path has a
    higher precedence than an even higher prioritized pattern before. 
    Actually, all patterns without any pre-context remove any _AcceptInfo
    object that preceded along the path.

    For further analysis, this class provides:

         .acceptance_trace -- Sorted list of information about acceptances.

    During the process of building path traces, the function

         .next_step(...)

    is called. It assumes that the current object represents the path trace
    before 'this state'. Based on the given arguments to this function it 
    modifies itself so that it represents the trace for 'this_state'.

    ___________________________________________________________________________
    EXAMPLE:
    
    
        ( 0 )----->(( 1 ))----->( 2 )----->(( 3 ))----->( 4 ) ....
                    8 wins                 pre 4 -> 5 wins                    
                                           pre 3 -> 7 wins

    results in _Trace objects for the states as follows:

        State 0: has no acceptance trace, only '(no pre-context, failure)'.
        State 1: (pattern 8 wins, input position = current)
        State 2: (pattern 8 wins, input position = current - 1)
        State 3: (if pre context 4 fulfilled: 5 wins, input position = current)
                 (if pre context 3 fulfilled: 7 wins, input position = current)
                 (else,                       8 wins, input position = current - 2)
        State 4: (if pre context 4 fulfilled: 5 wins, input position = current - 1)
                 (if pre context 3 fulfilled: 7 wins, input position = current - 1)
                 (else,                       8 wins, input position = current - 3)
        ...
    ___________________________________________________________________________
    """
    __slots__ = ("__acceptance_trace",  # List of _AcceptInfo objects
                 "__storage_db",        # Map: acceptance_id --> _StoreInfo objects
                 "__parent", 
                 "__state_index", 
                 "__equivalence_hash", 
                 "__equivalence_hint", 
                 "__acceptance_trace_len",
                 "__storage_db_len")

    def __init__(self, InitStateIndex=None, HashF=True):
        if InitStateIndex is None: # 'None' --> call from '.clone()'
            self.__acceptance_trace = [] 
        else:
            self.__acceptance_trace = [ 
                  _AcceptInfo(PreContextID        = E_PreContextIDs.NONE, 
                             AcceptanceID         = E_IncidenceIDs.MATCH_FAILURE, 
                             AcceptingStateIndex  = InitStateIndex, 
                             PathSincePositioning = [InitStateIndex], 
                             TransitionNSincePositioning = 0)              
            ]
        self.__storage_db       = {}
        self.__state_index      = InitStateIndex
        self.__parent           = None
        self.__equivalence_hash = None
        self.__equivalence_hint = None
        if HashF:
            self.__compute_equivalence_hash()

    def reproduce(self, StateIndex):
        """Reproduce: Clone + update for additional StateIndex in the path."""
        result = _Trace(HashF=False) # We compute 'hash' later.
        result.__acceptance_trace = [ x.reproduce(StateIndex) for x in self.__acceptance_trace ]
        result.__storage_db       = dict(( (i, x.reproduce(StateIndex)) 
                                         for i, x in self.__storage_db.iteritems() 
                                    ))
        result.__state_index      = StateIndex
        result.__parent           = self

        result.__compute_equivalence_hash()
        return result

    def next_step(self, StateIndex, State):
        """The present object of _Trace represents the history of events 
        along a path from the init state to the state BEFORE 'this state'.

        Applying the events of 'this state' on the current history results
        in a _Trace object that represents the history of events until
        'this state'.

        RETURNS: Altered clone of the present object.
        """
        # Some experimenting has shown that the number of unnecessary cloning,
        # i.e.  when there would be no change, is negligible. The fact that
        # '.path_since_positioning()' has almost always to be adapted,
        # makes selective cloning meaningless. So, it is done the safe way:
        # (update .path_since_positioning during 'reproduction'.)
        result = self.reproduce(StateIndex) # Always clone. 

        # (2) Update '.__acceptance_trace' and '.__storage_db' according to occurring
        #     acceptances and store-input-position events.
        #     Origins must be sorted with the highest priority LAST, so that they will
        #     appear on top of the acceptance trace list.
        for cmd in sorted(State.single_entry.get_iterable(SeAccept), 
                          key=lambda x: x.acceptance_id(), reverse=True):
            # Acceptance 
            result.__acceptance_trace_add_at_front(cmd, StateIndex)

        for cmd in sorted(State.single_entry.get_iterable(SeStoreInputPosition), 
                          key=lambda x: x.acceptance_id(), reverse=True):
            # Store Input Position Information
            result.__storage_db[cmd.acceptance_id()] = _StoreInfo([StateIndex], 0)

        assert len(result.__acceptance_trace) >= 1
        result.__compute_equivalence_hash()
        return result

    def __acceptance_trace_add_at_front(self, Op, StateIndex):
        """Assume that the 'Op' belongs to a state with index 'StateIndex' that
           comes after all states on the before considered path.
           Assume that the 'Op' talks about 'acceptance'.
        """
        # If there is an unconditional acceptance, it dominates all previous 
        # occurred acceptances (philosophy of longest match).
        if Op.pre_context_id() == E_PreContextIDs.NONE:
            del self.__acceptance_trace[:]

        # Input Position Store/Restore
        acceptance_id = Op.acceptance_id()
        if Op.restore_position_register_f():
            # Restorage of Input Position (Post Contexts): refer to the 
            # input position at the time when it was stored.
            entry                          = self.__storage_db[acceptance_id]
            path_since_positioning         = entry.path_since_positioning
            transition_n_since_positioning = entry.transition_n_since_positioning
        else:
            # Normally accepted patterns refer to the input position at 
            # the time of acceptance.
            path_since_positioning         = [ StateIndex ]
            transition_n_since_positioning = 0

        # Reoccurring information about an acceptance overwrites previous occurrences.
        for entry_i in (i for i, x in enumerate(self.__acceptance_trace) \
                        if x.acceptance_id == acceptance_id):
            del self.__acceptance_trace[entry_i]
            # From the above rule, it follows that there is only one entry per acceptance_id.
            break

        entry = _AcceptInfo(Op.pre_context_id(), acceptance_id,
                            AcceptingStateIndex         = StateIndex, 
                            PathSincePositioning        = path_since_positioning, 
                            TransitionNSincePositioning = transition_n_since_positioning) 

        # Insert at the beginning, because what comes last has the highest
        # priority.  (Philosophy of longest match). The calling function must
        # ensure that for one step on the path, the higher prioritized patterns
        # appear AFTER the lower prioritized ones.
        self.__acceptance_trace.insert(0, entry)

    @property 
    def state_index(self):  
        return self.__state_index

    @property
    def parent(self): 
        return self.__parent

    def has_parent(self, Candidate):
        parent = self.__parent
        while parent is not None:
            if id(parent) == id(Candidate): return True
            parent = parent.parent
        return False

    @property
    def acceptance_trace(self):
        return self.__acceptance_trace

    @property
    def storage_db(self):
        return self.__storage_db

    def get(self, PreContextID):
        """RETURNS: _AcceptInfo object for a given PreContextID."""
        for entry in self.__acceptance_trace:
            if entry.pre_context_id == PreContextID: return entry
        return None

    def __compute_equivalence_hash(self):
        """Computes a numeric value 'self.__equivalence_hash' to identify the
           current setting of the object.  This hash value may be used in a
           necessary condition to compare for 'equivalence' with another
           object. That is, if the hash values of two objects are different,
           the objects MUST be different. If they are the same, a detailed
           check must investigate if they are equivalent or not. See function
           'is_equivalent()' which assumes that the '__equivalence_hash' has been
           computed.
        """
        # New Faster Try: (Must be Double Checked!)
        #        eq = hash(0x5a5a5a5a)
        #        for x in self.__acceptance_trace:
        #            eq ^= hash(x.acceptance_id)
        #            eq ^= hash(x.accepting_state_index)
        #            eq ^= hash(x.positioning_state_index)
        #            eq ^= hash(x.transition_n_since_positioning)
        #
        #        for acceptance_id, info in sorted(self.__storage_db.iteritems()):
        #            eq ^= hash(x.loop_f)
        #            eq ^= hash(x.transition_n_since_positioning)
        data = []
        for x in self.__acceptance_trace:
            if isinstance(x.acceptance_id, long):                  data.append(x.acceptance_id)
            elif x.acceptance_id == E_IncidenceIDs.MATCH_FAILURE:  data.append(0x5A5A5A5A)
            else:                                                  data.append(0xA5A5A5A5)
            if isinstance(x.accepting_state_index, long):          data.append(x.accepting_state_index)
            else:                                                  data.append(0x5B5B5B5B)
            if isinstance(x.positioning_state_index, long):        data.append(x.positioning_state_index)
            else:                                                  data.append(0x5C5C5C5C)
            if isinstance(x.transition_n_since_positioning, long): data.append(x.transition_n_since_positioning)
            else:                                                  data.append(0x5D5D5D5D)

        for acceptance_id, info in sorted(self.__storage_db.iteritems()):
            if info.loop_f:                                             data.append(0x48484848)
            elif isinstance(info.transition_n_since_positioning, long): data.append(info.transition_n_since_positioning)
            else:                                                       data.append(0x4D4D4D4D)

        self.__equivalence_hash = crc32(str(data))
        # HINT: -- One single acceptance on current state.
        #       -- No restore of position from previous states.
        #       => Store the acceptance_id of the winning pattern.
        # 
        # This hint may be used for a SUFFICENT condition to determine 
        # equivalence, IF the state has no subsequent transitions. Because,
        # then there is no restore involved and the __storage_db can be
        # neglected.
        if len(self.__acceptance_trace) == 1:
            x = self.__acceptance_trace[0]
            if     x.transition_n_since_positioning == 0               \
               and x.positioning_state_index == x.accepting_state_index:
                # From:    transition_n_since_positioning == 0
                # Follows: x.accepting_state_index == current state index
                self.__equivalence_hint = x.acceptance_id
            else:
                self.__equivalence_hint = None
        else:
            self.__equivalence_hint = None
        self.__acceptance_trace_len = len(self.__acceptance_trace)
        self.__storage_db_len       = len(self.__storage_db)

    def is_equivalent(self, Other, EndOfRoadF=False):
        """This function determines whether the path trace described in Other is
           equivalent to this trace. 
        """
        if self.__equivalence_hash != Other.__equivalence_hash:           return False

        if self.__equivalence_hint is not None:
            if self.__equivalence_hint == Other.__equivalence_hint:       return True

        if   self.__acceptance_trace_len != Other.__acceptance_trace_len: return False
        elif self.__storage_db_len       != Other.__storage_db_len:       return False

        for x, y in izip(self.__acceptance_trace, Other.__acceptance_trace):
            if   x.acceptance_id                  != y.acceptance_id:                  return False
            elif x.accepting_state_index          != y.accepting_state_index:          return False
            elif x.positioning_state_index        != y.positioning_state_index:        return False
            elif x.transition_n_since_positioning != y.transition_n_since_positioning: return False

        # When there are no further transitions to other states, then no restore
        # may happen. Then, considering '__storage_db' is not necessary.
        if not EndOfRoadF:
            for x_pattern_id, x in self.__storage_db.iteritems():
                y = Other.__storage_db.get(x_pattern_id)
                if   y is None:                                              return False
                elif x.loop_f                  != y.loop_f:                  return False
                elif x.positioning_state_index != y.positioning_state_index: return False

        #print "## self.acceptance:", self.__acceptance_trace
        #print "## self.storage:", self.__storage_db
        #print "## Other.acceptance:", Other.__acceptance_trace
        #print "## self.storage:", self.__storage_db
        #print "## Other.storage:", Other.__storage_db
        #print "#TRUE", Other
        return True

    def __eq__(self, Other):
        if self.__acceptance_trace != Other.__acceptance_trace: return False
        if len(self.__storage_db)  != len(Other.__storage_db):  return False

        for acceptance_id, entry in self.__storage_db.iteritems():
            other_entry = Other.__storage_db.get(acceptance_id)
            if other_entry is None:                                return False
            if not entry.is_equal(Other.__storage_db[acceptance_id]): return False
        
        return True

    def __ne__(self, Other):
        return not (self == Other)

    def __repr__(self):
        return "".join([repr(x) for x in self.__acceptance_trace]) + "".join([repr(x) for x in self.__storage_db.iteritems()])

class _StoreInfo(object):
    """
    Informs about a 'positioning action' that happened during the walk
    along a specific path from init state to 'this state'. 
    
    Used in function '_Trace.next_step()'.
    ___________________________________________________________________________
    EXPLANATION:

    A 'positioning action' is the storage of the current input position 
    into a dedicated position register. Objects of class '_StoreInfo'
    are stored in dictionaries where the key represents the pattern-id
    is at the same time the identifier of the position storage register.
    (Note, later the position register is remapped according to required
     entries.)

    'This state' means the state where the trace lead to. 

    The member '.path_since_positioning' gets one more state index appended
    at each transition along a path. 
    
    If a loop is detected '.transition_n_since_positioning' returns
    'E_TransitionN.VOID'.

    The member '.positioning_state_index' is the state where the positioning
    happend. If there is a loop along the path from '.positioning_state_index'
    to 'this state, then the '.transition_n_since_positioning' is set to 
    'E_TransitionN.VOID' (see comment above).

    ___________________________________________________________________________
    .path_since_positioning  -- List of indices of states which have been
                                passed from the storage of input position
                                to this state.
    ___________________________________________________________________________
    """
    __slots__ = ('path_since_positioning', '__transition_n_since_positioning', '__loop_f')
    def __init__(self, PathSincePositioning, TransitionNSincePositioning=None):
        self.path_since_positioning = PathSincePositioning
        if TransitionNSincePositioning is None:
            if len(PathSincePositioning) != len(set(PathSincePositioning)):
                self.__loopf                          = True 
                self.__transition_n_since_positioning = E_TransitionN.VOID
            else:
                self.__loop_f                         = False
                self.__transition_n_since_positioning = len(PathSincePositioning) - 1
        else:
            if TransitionNSincePositioning == E_TransitionN.VOID:
                self.__loop_f                         = True
            else:
                self.__loop_f                         = False
            self.__transition_n_since_positioning = TransitionNSincePositioning

    def reproduce(self, StateIndex):
        """Reproduce: Clone + update for additional StateIndex in the path."""
        path_since_positioning         = copy(self.path_since_positioning)
        transition_n_since_positioning = self.get_transition_n_since_positioning_update(StateIndex)
        path_since_positioning.append(StateIndex)

        return _StoreInfo(path_since_positioning, transition_n_since_positioning)

    def get_transition_n_since_positioning_update(self, StateIndex):
        """RETURNS: Value of 'transition_n_since_positioning' when 'StateIndex'
                    is put on the path.
        """
        if      StateIndex in self.path_since_positioning \
            and self.__transition_n_since_positioning != E_TransitionN.LEXEME_START_PLUS_ONE:
            return E_TransitionN.VOID

        elif isinstance(self.__transition_n_since_positioning, (int, long)):
            return self.__transition_n_since_positioning + 1

        else:
            return self.__transition_n_since_positioning

    @property
    def loop_f(self):                         
        # NOT: return self.__transition_n_since_positioning == E_TransitionN.VOID
        #      because the comparison is much slower, then returning simply a boolean.
        # THIS FUNCTION MAY BE CALLED EXTENSIVELY!
        return self.__loop_f

    @property
    def transition_n_since_positioning(self): 
        return self.__transition_n_since_positioning

    @property
    def positioning_state_index(self):
        return self.path_since_positioning[0]

    def is_equal(self, Other):
        return     self.transition_n_since_positioning == Other.transition_n_since_positioning \
               and self.positioning_state_index        == Other.positioning_state_index

    def __repr__(self):
        txt = ["---\n"]
        txt.append("    .path_since_positioning         = %s\n" % repr(self.path_since_positioning))
        return "".join(txt)

class _AcceptInfo(_StoreInfo):
    """
    Information about the acceptance and input position storage behavior in 
    a state which is a result of events that happened before on a specific path 
    from the init state to 'this_state'.
    ___________________________________________________________________________
    EXPLANATION:
    
    Acceptance of a pattern is something that occurs in case that the 
    state machine can no further proceed on a given input (= philosophy
    of 'longest match'), i.e. on 'drop-out'. '_AcceptInfo' objects tell 
    about the acceptance of a particular pattern (given by '.acceptance_id').
    
    .acceptance_id              -- Identifies the pattern that is concerned.
                             
    .pre_context_id          -- if E_PreContextIDs.NONE, then '.acceptance_id' is
                                always accepted. If not, then the pre-context
                                must be checked before the pattern can be 
                                accepted.
                             
    .accepting_state_index   -- Index of the state that caused the acceptance 
                                of the pattern somewhere before on the path.
                                It may, as well be 'this state'.
    
    [from _StoreInfo]

    .path_since_positioning  -- List of indices of states which have been
                                passed from the storage of input position
                                to this state.
    ___________________________________________________________________________
    """
    __slots__ = ("pre_context_id", 
                 "acceptance_id", 
                 "accepting_state_index") 

    def __init__(self, PreContextID, AcceptanceID, 
                 AcceptingStateIndex, PathSincePositioning, 
                 TransitionNSincePositioning): 
        self.pre_context_id        = PreContextID
        self.acceptance_id            = AcceptanceID
        self.accepting_state_index = AcceptingStateIndex

        if self.acceptance_id == E_IncidenceIDs.MATCH_FAILURE:
            transition_n_since_positioning = E_TransitionN.LEXEME_START_PLUS_ONE
        else:
            transition_n_since_positioning = TransitionNSincePositioning

        _StoreInfo.__init__(self, PathSincePositioning, transition_n_since_positioning)

    def reproduce(self, StateIndex):
        """Reproduce: Clone + update for additional StateIndex in the path."""
        path_since_positioning         = copy(self.path_since_positioning)
        transition_n_since_positioning = self.get_transition_n_since_positioning_update(StateIndex)
        path_since_positioning.append(StateIndex)
        result = _AcceptInfo(self.pre_context_id, 
                            self.acceptance_id, 
                            self.accepting_state_index, 
                            path_since_positioning, 
                            transition_n_since_positioning) 
        return result

    def index_of_last_acceptance_on_path_since_positioning(self):
        L = len(self.path_since_positioning)
        return L - 1 - self.path_since_positioning[::-1].index(self.accepting_state_index)

    @property
    def positioning_state_index(self):
        return self.path_since_positioning[0]

    def is_equal(self, Other):
        if   self.pre_context_id                 != Other.pre_context_id:                 return False
        elif self.acceptance_id                     != Other.acceptance_id:                     return False
        elif self.accepting_state_index          != Other.accepting_state_index:          return False
        elif self.transition_n_since_positioning != Other.transition_n_since_positioning: return False
        elif self.positioning_state_index        != Other.positioning_state_index:        return False
        return True

    def __eq__(self, Other):
        return self.is_equal(Other)

    def __repr__(self):
        txt = ["---\n"]
        txt.append("    .pre_context_id                 = %s\n" % repr(self.pre_context_id))
        txt.append("    .acceptance_id                     = %s\n" % repr(self.acceptance_id))
        txt.append("    .transition_n_since_positioning = %s\n" % repr(self.transition_n_since_positioning))
        txt.append("    .accepting_state_index          = %s\n" % repr(self.accepting_state_index))
        txt.append("    .positioning_state_index        = %s\n" % repr(self.positioning_state_index))
        txt.append(_StoreInfo.__repr__(self))
        return "".join(txt)


