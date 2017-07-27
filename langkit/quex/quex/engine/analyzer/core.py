"""
_______________________________________________________________________________

This module produces an object of class Analyzer. It is a representation of an
analyzer state machine (object of class StateMachine) that is suited for code
generation. In particular, track analysis results in 'decorations' for states
which help to implement efficient code.

_______________________________________________________________________________
EXPLANATION:

Formally an Analyzer consists of a set of states that are related by their
transitions. Each state is an object of class AnalyzerState and has the
following components:

    * entry:          actions to be performed at the entry of the state.

    * input:          what happens to get the next character.

    * transition_map: a map that tells what state is to be entered 
                      as a reaction to the current input character.

There's global 'drop-out' state which has entries from each state. Its entries
represents what happens if a state's transition mape fails to match any input 
character.

For administrative purposes, other data such as the 'state_index' is stored
along with the AnalyzerState object.

The goal of track analysis is to reduce the run-time effort of the lexical
analyzer. In particular, acceptance and input position storages may be spared
depending on the constitution of the state machine.

_______________________________________________________________________________
(C) 2010-2013 Frank-Rene Schaefer
ABSOLUTELY NO WARRANTY
_______________________________________________________________________________
"""

import quex.engine.analyzer.track_analysis        as     track_analysis
from   quex.engine.analyzer.paths_to_state        import PathsToState
import quex.engine.analyzer.optimizer             as     optimizer
from   quex.engine.analyzer.state.entry           import Entry
from   quex.engine.analyzer.state.core            import Processor, \
                                                         AnalyzerState, \
                                                         ReloadState
import quex.engine.analyzer.state.drop_out        as     drop_out
from   quex.engine.analyzer.state.entry_action    import TransitionAction
from   quex.engine.operations.operation_list                  import Op, \
                                                         OpList
import quex.engine.analyzer.mega_state.analyzer   as     mega_state_analyzer
import quex.engine.analyzer.position_register_map as     position_register_map
import quex.engine.analyzer.engine_supply_factory as     engine

from   quex.engine.state_machine.core               import StateMachine
from   quex.engine.state_machine.state.single_entry import SeAccept      

from   quex.engine.misc.tools                            import typed
from   quex.blackboard  import setup as Setup, \
                               E_IncidenceIDs, \
                               E_TransitionN, \
                               E_PreContextIDs, \
                               E_Op, \
                               E_StateIndices

from   collections      import defaultdict
from   itertools        import imap
from   operator         import attrgetter

def do(SM, EngineType=engine.FORWARD, 
       ReloadStateExtern=None, OnBeforeReload=None, OnAfterReload=None):

    # Generate Analyzer from StateMachine
    analyzer = Analyzer.from_StateMachine(SM, EngineType, ReloadStateExtern)
    # Optimize the Analyzer
    analyzer = optimizer.do(analyzer)

    # DoorID-s required by '.prepare_for_reload()'
    analyzer.prepare_DoorIDs()

    # Prepare the reload BEFORE mega state compression!
    # (Null-operation, in case no reload required.)
    # TransitionMap:              On BufferLimitCode --> ReloadState
    # ReloadState.door of state:  OnBeforeReload
    #                             prepare goto on reload success and reload fail
    # State.door of ReloadState:  OnAfterReload (when reload was a success).
    for state in analyzer.state_db.itervalues():
        # Null-operation, in case no reload required.
        state.prepare_for_reload(analyzer, OnBeforeReload, OnAfterReload) 

    # [Optional] Combination of states into MegaState-s.
    if len(Setup.compression_type_list) != 0:
        mega_state_analyzer.do(analyzer)
        # Prepare Reload:
        # (Null-operation, in case no reload required.)
        # TransitionMap:                  On BufferLimitCode --> ReloadState
        # ReloadState.door of mega state: Router to doors of implemented states.
        for state in analyzer.mega_state_list:
            state.prepare_again_for_reload(analyzer) 

    # AnalyzerState.transition_map:    Interval --> DoorID
    # MegaState.transition_map:        Interval --> TargetByStateKey
    #                               or Interval --> DoorID
    return analyzer

class Analyzer:
    """A representation of a pattern analyzing StateMachine suitable for
       effective code generation.
    """
    def __init__(self, EngineType, InitStateIndex):
        self.__engine_type      = EngineType
        self.__init_state_index = InitStateIndex
        self.__state_db         = {}
        self.drop_out           = Processor(E_StateIndices.DROP_OUT, Entry())
        self.__state_machine_id = None

    @classmethod
    @typed(SM=StateMachine, EngineType=engine.Base)
    def from_StateMachine(cls, SM, EngineType, ReloadStateExtern=None):
        """ReloadStateExtern is only to be specified if the analyzer needs
        to be embedded in another one.
        """
        result = cls(EngineType, SM.init_state_index)
        result._prepare_state_information(SM)
        result._prepare_reload_state(ReloadStateExtern, EngineType)
        result._prepare_entries_and_drop_out(EngineType, SM)
        return result

    @typed(SM=StateMachine)
    def _prepare_state_information(self, SM):
        self.__acceptance_state_index_list = SM.get_acceptance_state_index_list()
        self.__state_machine_id            = SM.get_id()

        # (*) From/To Databases
        #
        #     from_db:  state_index --> states from which it is entered.
        #     to_db:    state_index --> states which it enters
        #
        self.__to_db   = SM.get_to_db()
        self.__from_db = SM.get_from_db()

        # (*) Prepare AnalyzerState Objects
        self.__state_db.update(
            (state_index, self.prepare_state(state, state_index))
            for state_index, state in SM.states.iteritems()
        )

        self.__mega_state_list          = []
        self.__non_mega_state_index_set = set(state_index for state_index in SM.states.iterkeys())

    def _prepare_reload_state(self, ReloadStateExtern, EngineType):
        if ReloadStateExtern is None:
            self.reload_state          = ReloadState(EngineType=EngineType)
            self.reload_state_extern_f = False
        else:
            self.reload_state          = ReloadStateExtern
            self.reload_state_extern_f = True

    def _prepare_entries_and_drop_out(self, EngineType, SM):
        if not EngineType.requires_detailed_track_analysis():
            for state_index, state in SM.states.iteritems():
                if not self.state_db[state_index].transition_map.has_drop_out(): continue
                cl = EngineType.create_DropOut(state)
                self.drop_out.entry.enter_OpList(E_StateIndices.DROP_OUT, 
                                                      state_index, cl)
                                                      
            self.__position_register_map = None

        else:
            # (*) PathTrace database, Successor database
            self.__trace_db, \
            path_element_db    = track_analysis.do(SM, self.__to_db)

            # (*) Drop Out Behavior
            #     The PathTrace objects tell what to do at drop_out. From this, the
            #     required entry actions of states can be derived.
            acceptance_storage_db, \
            position_storage_db    = self.configure_all_drop_outs()

            # (*) Entry Behavior
            #     Implement the required entry actions.
            self.configure_all_entries(acceptance_storage_db, position_storage_db,
                                       path_element_db)

            if EngineType.requires_position_register_map():
                # (*) Position Register Map (Used in 'optimizer.py')
                self.__position_register_map = position_register_map.do(self)
            else:
                self.__position_register_map = None
        return 

    def add_mega_states(self, MegaStateList):
        """Add MegaState-s into the analyzer and remove the states which are 
        represented by them.
        """
        for mega_state in MegaStateList:
            state_index_set = mega_state.implemented_state_index_set()
            for state_index in state_index_set:
                self.remove_state(state_index)

        self.__mega_state_list          = MegaStateList
        self.__non_mega_state_index_set = set(self.__state_db.iterkeys())

        self.__state_db.update(
           (mega_state.index, mega_state) for mega_state in MegaStateList
        )

    def remove_state(self, StateIndex):
        if StateIndex in self.__non_mega_state_index_set:
            self.__non_mega_state_index_set.remove(StateIndex)
        del self.__state_db[StateIndex]

    @property
    def mega_state_list(self):             return self.__mega_state_list
    @property
    def non_mega_state_index_set(self):    return self.__non_mega_state_index_set
    @property
    def trace_db(self):                    return self.__trace_db
    @property
    def state_db(self):                    return self.__state_db
    @property
    def init_state_index(self):            return self.__init_state_index
    @property
    def position_register_map(self):       return self.__position_register_map
    @property
    def state_machine_id(self):            return self.__state_machine_id
    @property
    def engine_type(self):                 return self.__engine_type
    @property
    def acceptance_state_index_list(self): return self.__acceptance_state_index_list
    @property
    def to_db(self):
        """Map: state_index --> list of states which can be reached starting from state_index."""
        return self.__to_db
    @property
    def from_db(self):
        """Map: state_index --> list of states which which lie on a path to state_index."""
        return self.__from_db

    def iterable_target_state_indices(self, StateIndex):
        for i in self.__state_db[StateIndex].map_target_index_to_character_set.iterkeys():
            yield i
        yield None

    def prepare_state(self, OldState, StateIndex):
        """REQUIRES: 'self.init_state_forward_f', 'self.engine_type', 'self.__from_db'.
        """
        state = AnalyzerState.from_State(OldState, StateIndex, self.engine_type)

        cmd_list = []
        if self.engine_type.is_BACKWARD_PRE_CONTEXT():
            cmd_list.extend(
                 Op.PreContextOK(cmd.acceptance_id()) 
                 for cmd in OldState.single_entry.get_iterable(SeAccept)
            )

        if state.transition_map is None and False: 
            # NOTE: We need a way to disable this exception for PathWalkerState-s(!)
            #       It's safe, not to allow it, in general.
            #------------------------------------------------------------------------
            # If the state has no further transitions then the input character does 
            # not have to be read. This is so, since without a transition map, the 
            # state immediately drops out. The drop out transits to a terminal. 
            # Then, the next action will happen from the init state where we work
            # on the same position. If required the reload happens at that moment,
            # NOT before the empty transition block.
            #
            # This is not true for Path Walker States, so we offer the option 
            # 'ForceInputDereferencingF'
            assert StateIndex != self.init_state_index # Empty state machine! --> impossible

            if self.engine_type.is_FORWARD(): cmd_list.append(Op.InputPIncrement())
            else:                             cmd_list.append(Op.InputPDecrement())
        else:
            if self.engine_type.is_FORWARD(): cmd_list.extend([Op.InputPIncrement(), Op.InputPDereference()])
            else:                             cmd_list.extend([Op.InputPDecrement(), Op.InputPDereference()])

        ta = TransitionAction(OpList.from_iterable(cmd_list))

        # NOTE: The 'from reload transition' is implemented by 'prepare_for_reload()'
        for source_state_index in self.__from_db[StateIndex]: 
            assert source_state_index != E_StateIndices.BEFORE_ENTRY
            state.entry.enter(StateIndex, source_state_index, ta.clone())

        if StateIndex == self.init_state_index:
            if self.engine_type.is_FORWARD():
                ta = TransitionAction(OpList(Op.InputPDereference()))
            state.entry.enter_state_machine_entry(self.__state_machine_id, 
                                                  StateIndex, ta)

        return state

    def prepare_DoorIDs(self):
        """Assign DoorID-s to transition actions and relate transitions to DoorID-s.
        """
        for state in self.__state_db.itervalues():
            state.entry.categorize(state.index)
        self.drop_out.entry.categorize(E_StateIndices.DROP_OUT)

        for state in self.__state_db.itervalues():
            assert state.transition_map is not None
            state.transition_map = state.transition_map.relate_to_DoorIDs(self, state.index)

        return 

    def drop_out_DoorID(self, StateIndex):
        """RETURNS: DoorID of the drop-out catcher for the state of the given
                    'StateIndex'
                    None -- if there is no drop out for the given state.
        """
        drop_out_door_id = self.drop_out.entry.get_door_id(E_StateIndices.DROP_OUT, StateIndex)
        #assert drop_out_door_id is not None
        return drop_out_door_id
                                      
    def init_state(self):
        return self.state_db[self.init_state_index]

    def get_action_at_state_machine_entry(self):
        return self.init_state().entry.get_action(self.init_state_index, 
                                                  E_StateIndices.BEFORE_ENTRY)

    def get_depth_db(self):
        """Determine a database which tells about the minimum distance to the initial state.

            map: state_index ---> min. number of transitions from the initial state.

        """
        depth_db = { self.__init_state_index: 0, }

        work_set   = set(self.__state_db.keys())
        work_set.remove(self.__init_state_index)
        last_level = set([ self.__init_state_index ])
        level_i    = 1
        while len(work_set):
            len_before = len(work_set)
            this_level = set()
            for state_index in last_level:
                for i in self.iterable_target_state_indices(state_index):
                    if   i not in work_set: continue
                    elif i in depth_db:     continue 
                    depth_db[i] = level_i
                    this_level.add(i)
                    work_set.remove(i)
            assert len_before != len(work_set), "There are orphaned states!" 
            last_level = this_level
            level_i += 1

        return depth_db

    def last_acceptance_variable_required(self):
        """If one entry stores the last_acceptance, then the 
           correspondent variable is required to be defined.
        """
        if not self.__engine_type.is_FORWARD(): 
            return False
        for entry in imap(lambda x: x.entry, self.__state_db.itervalues()):
            if entry.has_command(E_Op.Accepter): return True
        return False

    @typed(TraceList=PathsToState)
    def drop_out_configure(self, StateIndex, TraceList, acceptance_storage_db, position_storage_db):
        """____________________________________________________________________
        Every analysis step ends with a 'drop-out'. At this moment it is
        decided what pattern has won. Also, the input position pointer must be
        set so that it indicates the right location where the next step starts
        the analysis. 

        Consequently, a drop-out action contains two elements:

         -- Acceptance Checker: Dependent on the fulfilled pre-contexts a
            winning pattern is determined. 

            If acceptance depends on stored acceptances, a request is raised at
            each accepting state that is has to store its acceptance in
            variable 'last_acceptance'.

         -- Terminal Router: Dependent on the accepted pattern the input
            position is modified and the terminal containing the pattern
            action is entered.

            If the input position is restored from a position register, then
            the storing states are requested to store the input position.

        _______________________________________________________________________
        HINT:

        A state may be reached via multiple paths. For each path there is a
        separate PathTrace. Each PathTrace tells what has to happen in the
        state depending on the pre-contexts being fulfilled or not (if there
        are even any pre-context patterns).
        _______________________________________________________________________
        """
        # (*) Acceptance Checker
        accept_sequence = TraceList.uniform_acceptance_sequence()
        if accept_sequence is not None:
            # (i) Uniform Acceptance Pattern for all paths through the state.
            # 
            #     Use one trace as prototype. No related state needs to store
            #     acceptance at entry. 
            accepter = Op.Accepter() # Accepter content
            for x in accept_sequence:
                accepter.content.add(x.pre_context_id, x.acceptance_id)
                # No further checks necessary after unconditional acceptance
                if     x.pre_context_id == E_PreContextIDs.NONE \
                   and x.acceptance_id  != E_IncidenceIDs.MATCH_FAILURE: break
        else:
            # (ii) Non-Uniform Acceptance Patterns
            #
            # Different paths to one state result in different acceptances. 
            # There is only one way to handle this:
            #
            # -- The acceptance must be stored in the state where it occurs, 
            # -- and it must be restored here.
            #
            accepter = None # Only rely on 'last_acceptance' being stored.

            # Dependency: Related states are required to store acceptance at state entry.
            for accepting_state_index in TraceList.accepting_state_index_list():
                acceptance_storage_db[accepting_state_index].append(StateIndex)

            # Later, a function will use the '__require_acceptance_storage_db' to 
            # implement the acceptance storage.

        # (*) Terminal Router
        terminal_router = Op.RouterByLastAcceptance()
        for x in TraceList.positioning_info():
            terminal_router.content.add(x.acceptance_id, x.transition_n_since_positioning)

            if x.transition_n_since_positioning != E_TransitionN.VOID: continue

            # Request the storage of the position from related states.
            for state_index in x.positioning_state_index_set:
                position_storage_db[state_index].append(
                        (StateIndex, x.pre_context_id, x.acceptance_id))

            # Later, a function will use the 'position_storage_db' to implement
            # the position storage.

        return drop_out.get_OpList(accepter, terminal_router)

    def configure_all_drop_outs(self):
        acceptance_storage_db = defaultdict(list)
        position_storage_db   = defaultdict(list)
        for state_index, trace_list in self.__trace_db.iteritems():
            if not self.state_db[state_index].transition_map.has_drop_out(): continue
            cl = self.drop_out_configure(state_index, trace_list, 
                                         acceptance_storage_db,
                                         position_storage_db)
            self.drop_out.entry.enter_OpList(E_StateIndices.DROP_OUT, 
                                                  state_index, cl)
        return acceptance_storage_db, position_storage_db

    def configure_all_entries(self, acceptance_storage_db, position_storage_db, PathElementDb):
        """DropOut objects may rely on acceptances and input positions being 
           stored. This storage happens at state entries.
           
           Function 'drop_out_configure()' registers which states have to store
           the input position and which ones have to store acceptances. These
           tasks are specified in the two members:

                 require_acceptance_storage_db
                 position_storage_db

           It is tried to postpone the storing as much as possible along the
           state paths from store to restore. Thus, some states may not have to
           store, and thus the lexical analyzer becomes a little faster.
        """
        self.implement_required_acceptance_storage(acceptance_storage_db)
        self.implement_required_position_storage(position_storage_db, PathElementDb)

    def implement_required_acceptance_storage(self, acceptance_storage_db):
        """
        Storing Acceptance / Postpone as much as possible.
        
        The stored 'last_acceptance' is only needed at the first time
        when it is restored. So, we could walk along the path from the 
        accepting state to the end of the path and see when this happens.
        
        Critical case:
        
          State V --- "acceptance = A" -->-.
                                            \
                                             State Y ----->  State Z
                                            /
          State W --- "acceptance = B" -->-'
        
        That is, if state Y is entered from state V is shall store 'A'
        as accepted, if it is entered from state W is shall store 'B'.
        In this case, we cannot walk the path further, and say that when
        state Z is entered it has to store 'A'. This would cancel the
        possibility of having 'B' accepted here. There is good news:
        
        ! During the 'drop_out_configure()' the last acceptance is restored    !
        ! if and only if there are at least two paths with differing           !
        ! acceptance patterns. Thus, it is sufficient to consider the restore  !
        ! of acceptance in the drop_out as a terminal condition.               !

        EXCEPTION:

        When a state is reached that is part of '__dangerous_positioning_state_set'
        then it is not safe to assume that all sub-paths have been considered.
        The acceptance must be stored immediately.
        """
        # Not Postponed: Collected acceptances to be stored in the acceptance states itself.
        #
        # Here, storing Acceptance cannot be deferred to subsequent states, because
        # the first state that restores acceptance is the acceptance state itself.
        #
        # (1) Restore only happens if there is non-uniform acceptance. See 
        #     function 'drop_out_configure(...)'. 
        # (2) Non-uniform acceptance only happens, if there are multiple paths
        #     to the same state with different trailing acceptances.
        # (3) If there was an absolute acceptance, then all previous trailing 
        #     acceptance were deleted (longest match). This contradicts (2).
        #
        # (4) => Thus, there are only pre-contexted acceptances in such a state.
        #
        # It is possible that a deferred acceptance are already present in the doors. But, 
        # since they all come from trailing acceptances, we know that the acceptance of
        # this state preceeds (longest match). Thus, all the acceptances we add here 
        # preceed the already mentioned ones. Since they all trigger on lexemes of the
        # same length, the only precendence criteria is the acceptance_id.
        # 
        for state_index in acceptance_storage_db.iterkeys():
            entry = self.__state_db[state_index].entry
            # Only the trace content that concerns the current state is filtered out.
            # It should be the same for all traces through 'state_index'
            prototype = self.__trace_db[state_index].get_any_one()
            for x in sorted(prototype, key=attrgetter("acceptance_id", "pre_context_id")):
                if x.accepting_state_index != state_index: 
                    continue
                entry.add_Accepter_on_all(x.pre_context_id, x.acceptance_id)

    def acceptance_storage_post_pone_do(self, StateIndex, PatternId):
        pass 

    def acceptance_storage_post_pone_can_be_delegate(self, StateIndex, PatternId, MotherAcceptSequence):
        pass
            
    def implement_required_position_storage(self, position_storage_db, PathElementDb):
        """
        Store Input Position / Postpone as much as possible.

        Before we do not reach a state that actually restores the position, it
        does make little sense to store the input position. 

                         Critical Point: Loops and Forks

        If a loop is reached then the input position can no longer be determined
        by the transition number. The good news is that during 'drop_out_configure'
        any state that has undetermined positioning restores the input position.
        Thus 'restore_position_f(register)' is enough to catch this case.
        """
        for state_index, info_list in position_storage_db.iteritems():
            target_state_index_list = self.__to_db[state_index]
            for end_state_index, pre_context_id, acceptance_id in info_list:
                # state_index      --> state that stores the input position
                # end_state_index  --> state that stores the input position
                # pre_context_id   --> pre_context which is concerned
                # acceptance_id       --> pattern which is concerned
                # Only consider target states which guide to the 'end_state_index'.
                index_iterable = (i for i in target_state_index_list 
                                    if i in PathElementDb[end_state_index])
                for target_index in index_iterable: # target_state_index_list: # index_iterable:
                    if target_index == state_index: # Position is stored upon entry in *other*
                        continue                    # state--not the state itself. 
                    entry = self.__state_db[target_index].entry

                    entry.add_StoreInputPosition(StateIndex       = target_index, 
                                                 FromStateIndex   = state_index, 
                                                 PreContextID     = pre_context_id, 
                                                 PositionRegister = acceptance_id, 
                                                 Offset           = 0)

    def is_init_state_forward(self, StateIndex):
        return StateIndex == self.init_state_index and self.engine_type.is_FORWARD()     
                
    def __iter__(self):
        for x in self.__state_db.values():
            yield x

    def __repr__(self):
        # Provide some type of order that is oriented towards the content of the states.
        # This helps to compare analyzers where the state identifiers differ, but the
        # states should be the same.
        def order(X):
            side_info = 0
            if len(X.transition_map) != 0: side_info = max(trigger_set.size() for trigger_set, t in X.transition_map)
            return (len(X.transition_map), side_info, X.index)

        txt = [ repr(state) for state in sorted(self.__state_db.itervalues(), key=order) ]
        return "".join(txt)

