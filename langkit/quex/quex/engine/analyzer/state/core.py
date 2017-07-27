from   quex.engine.state_machine.state.core        import State
from   quex.engine.analyzer.door_id_address_label  import dial_db
from   quex.engine.analyzer.transition_map         import TransitionMap
from   quex.engine.analyzer.state.entry            import Entry
from   quex.engine.analyzer.state.entry_action     import TransitionAction
from   quex.engine.analyzer.door_id_address_label  import DoorID
from   quex.engine.operations.operation_list                   import OpList, Op
from   quex.engine.misc.tools import typed
from   quex.blackboard  import setup as Setup, \
                               E_IncidenceIDs, \
                               E_StateIndices

class Processor(object):
    __slots__ = ("_index", "entry")
    def __init__(self, StateIndex, TheEntry=None):
        self._index = StateIndex
        self.entry  = TheEntry
    
    @property
    def index(self):            return self._index
    def set_index(self, Value): assert isinstance(Value, long); self._index = Value

#__________________________________________________________________________
#
# AnalyzerState:
# 
#                  AnalyzerState
#                  .--------------------------------------------.
#  .-----.         |                                  .---------|
#  | 341 |--'load'--> Entry  ----->-----.             |tm(input)| 
#  '-----'         |  actions           |             |  'a' ------> 313
#  .-----.         |                    '             |  'c' ------> 142
#  | 412 |--'load'--> Entry  ---> input = *input_p -->|  'p' ------> 721
#  '-----'         |  actions           .             |  'q' ------> 313
#  .-----.         |                    |             |  'x' ------> 472
#  | 765 |--'load'--> Entry  --->-------'             |  'y' ------> 812
#  '-----'         |  actions                         |- - - - -|
#                  |                                  |drop out ---> 
#                  |                                  '---------|
#                  '--------------------------------------------'
#
# The entry actions depend on the state from which the state is entered.
# Next, the input pointer is dereferenced and 'input' is assigned. Based
# on the value of 'input' a subsequent state is targetted. The relation
# between 'input' and the target state is given by the 'TransitionMap'.
# If no state transition is possible, then 'drop out actions' are executed.
#__________________________________________________________________________
class AnalyzerState(Processor):
    __slots__ = ("map_target_index_to_character_set", 
                 "transition_map") 

    @typed(StateIndex=(int,long), TheTransitionMap=TransitionMap)
    def __init__(self, StateIndex, TheTransitionMap):
        # Empty transition maps are reported as 'None'
        Processor.__init__(self, StateIndex, Entry())
        self.map_target_index_to_character_set = None
        self.transition_map                    = TheTransitionMap

    @staticmethod
    def from_LinearState(S):
        if EntryRecipe is not None:
            self.entry.enter_OpList(S.entry_transition_id,
                                         S.recipe.get_entry_OpList())
        self.on_drop_out = Recipe.get_drop_out_OpList()

    @staticmethod
    def from_MouthState(S):
        for transition_id, recipe in S.entry_db.iteritems():
            self.entry.enter_OpList(transition_id,
                                         recipe.get_entry_OpList())
        self.on_drop_out = S.recipe.get_drop_out_OpList()

    @staticmethod
    def from_State(SM_State, StateIndex, EngineType):
        assert isinstance(SM_State, State)
        assert SM_State.target_map.is_DFA_compliant()
        assert isinstance(StateIndex, (int, long))

        x = AnalyzerState(StateIndex, TransitionMap.from_TargetMap(SM_State.target_map))

        # (*) Transition
        # Currently, the following is only used for path compression. If the alternative
        # is implemented, then the following is no longer necessary.
        x.map_target_index_to_character_set = SM_State.target_map.get_map()

        return x

    def prepare_for_reload(self, TheAnalyzer, BeforeReloadOpList=None, 
                           AfterReloadOpList=None):
        """Prepares state for reload. Reload procedure

            .- State 'X' ---.               
            |               |                .-- Reloader------. 
            | BUFFER LIMIT  |              .----------------.  |
            | CODE detected ------->-------| Door from X:   |  |
            |               |              | Actions before |  |
            |               |              | reload.        |  |
            |               |              '----------------'  |
            |               |                |        |        |
            |               |                |  reload buffer> |
            |    .----------------.          |        |        |
            |    | Door for       |---<-------(good)--*        |
            |    | RELOAD SUCCESS:|          |        |        |
            |    | Actions after  |          |      (bad)      |
            |    | Reload.        |          '------- |--------'
            |    '----------------'                   |
            |               |                .----------------.
            '---------------'                | Door for       |
                                             | RELOAD FAILURE |
                                             '----------------'
                                   
        (1) Create 'Door for RELOAD SUCCESS'. 
        (2) Determine 'Door for RELOAD FAILURE'.
        (3) Create 'Door from X' in Reloader.
        (4) Adapt state X's transition map, so that:
              BUFFER LIMIT CODE --> reload procedure.
        """
        assert self.transition_map is not None
        assert BeforeReloadOpList is None or isinstance(BeforeReloadOpList, OpList)
        assert AfterReloadOpList  is None or isinstance(AfterReloadOpList, OpList)

        if not TheAnalyzer.engine_type.subject_to_reload():
            # Engine type does not require reload => no reload. 
            return

        elif self.transition_map.is_only_drop_out():
            # If the state drops out anyway, then there is no need to reload.
            # -- The transition map is not adapted.
            # -- The reloader is not instrumented to reload for that state.
            return                      

        assert self.index in TheAnalyzer.state_db
        reload_state = TheAnalyzer.reload_state
        assert reload_state.index in (E_StateIndices.RELOAD_FORWARD, 
                                      E_StateIndices.RELOAD_BACKWARD)

        # (1) Door for RELOAD SUCCESS
        #
        after_cl = []
        if TheAnalyzer.engine_type.is_FORWARD(): after_cl.append(Op.InputPIncrement())
        else:                                    after_cl.append(Op.InputPDecrement())
        after_cl.append(Op.InputPDereference())
        if AfterReloadOpList is not None:
            after_cl.extend(AfterReloadOpList)

        self.entry.enter_OpList(self.index, reload_state.index, OpList.from_iterable(after_cl))
        self.entry.categorize(self.index) # Categorize => DoorID is available.
        on_success_door_id = self.entry.get_door_id(self.index, reload_state.index)

        # (2) Determin Door for RELOAD FAILURE
        #
        if TheAnalyzer.is_init_state_forward(self.index):
            on_failure_door_id = DoorID.incidence(E_IncidenceIDs.END_OF_STREAM)
        else:
            on_failure_door_id = TheAnalyzer.drop_out_DoorID(self.index)

        # (3) Create 'Door from X' in Reloader
        assert on_failure_door_id != on_success_door_id
        reload_door_id = reload_state.add_state(self.index, 
                                                on_success_door_id, 
                                                on_failure_door_id, 
                                                BeforeReloadOpList)

        # (4) Adapt transition map: BUFFER LIMIT CODE --> reload_door_id
        #
        self.transition_map.set_target(Setup.buffer_limit_code, reload_door_id)
        return

    def get_string_array(self, InputF=True, EntryF=True, TransitionMapF=True, DropOutF=True):
        txt = [ "State %s:\n" % repr(self.index).replace("L", "") ]
        # if InputF:         txt.append("  .input: move position %s\n" % repr(self.input))
        if EntryF:         txt.append("  .entry:\n"); txt.append(repr(self.entry))
        if TransitionMapF: txt.append("  .transition_map:\n")
        txt.append("\n")
        return txt

    def get_string(self, InputF=True, EntryF=True, TransitionMapF=True, DropOutF=True):
        return "".join(self.get_string_array(InputF, EntryF, TransitionMapF, DropOutF))

    def __repr__(self):
        return self.get_string()

#__________________________________________________________________________
#
# ReloadState:
#                  .--------------------------------------------.
#  .-----.         |                                            |
#  | 341 |--'load'--> S = 341;                                  |
#  '-----'         |  F = DropOut(341); --.            .----------> goto S
#  .-----.         |                      |           / success |
#  | 412 |--'load'--> S = 412;            |          /          |
#  '-----'         |  F = DropOut(412); --+--> Reload           |
#  .-----.         |                      |          \          |
#  | 765 |--'load'--> S = 765;            |           \ failure |
#  '-----'         |  F = DropOut(765); --'            '----------> goto F
#                  |                                            |
#                  '--------------------------------------------'
# 
# The entry of the reload state sets two variables: The address where to
# go if the reload was successful and the address where to go in case that
# the reload fails. 
#__________________________________________________________________________
class ReloadState(Processor):
    def __init__(self, EngineType):
        if EngineType.is_FORWARD(): index = E_StateIndices.RELOAD_FORWARD
        else:                       index = E_StateIndices.RELOAD_BACKWARD
        Processor.__init__(self, index, Entry())

    def remove_states(self, StateIndexSet):
        self.entry.remove_transition_from_states(StateIndexSet)

    def absorb(self, OtherReloadState):
        # Do not absorb RELOAD_FORWARD into RELOAD_BACKWARD, and vice versa.
        assert self.index == OtherReloadState.index
        self.entry.absorb(OtherReloadState.entry)

    def add_state(self, StateIndex, OnSuccessDoorId, OnFailureDoorId, BeforeReload=None):
        """Adds a state from where the reload state is entered. When reload is
        done it jumps to 'OnFailureDoorId' if the reload failed and to 'OnSuccessDoorId'
        if the reload succeeded.

        RETURNS: DoorID into the reload state. Jump to this DoorID in order
                 to trigger the reload for the state given by 'StateIndex'.
        """
        assert BeforeReload is None or isinstance(BeforeReload, OpList) 
        # Before reload: prepare after reload, the jump back to the reloading state.
        before_cl = OpList(Op.PrepareAfterReload(OnSuccessDoorId, OnFailureDoorId))
        if BeforeReload is not None:
            # May be, add additional commands
            before_cl = before_cl.concatinate(BeforeReload)

        # No two transitions into the reload state have the same OpList!
        # No two transitions can have the same DoorID!
        # => it is safe to assign a new DoorID withouth .categorize()
        ta         = TransitionAction(before_cl)
        # Assign a DoorID (without categorization) knowing that no such entry
        # into this state existed before.
        ta.door_id = dial_db.new_door_id(self.index)

        assert not self.entry.has_transition(self.index, StateIndex) # Cannot be in there twice!
        self.entry.enter(self.index, StateIndex, ta)

        return ta.door_id

    def add_mega_state(self, MegaStateIndex, StateKeyRegister, Iterable_StateKey_Index_Pairs, 
                       TheAnalyzer):
        """Implement a router from the MegaState-s door into the Reloader to
        the doors of the implemented states. 
        
                        Reload State
                       .--------------------------------- - -  -
                     .--------------.    on state-key
          reload --->| MegaState's  |       .---.
                     | Reload Door  |------>| 0 |-----> Reload Door of state[0]
                     '--------------'       | 1 |-----> Reload Door of state[1]
                       |                    | : |
                       :                    '---'
                       '--------------------------------- - -  -

        """
        def DoorID_provider(state_index):
            door_id = self.entry.get_door_id(self.index, state_index)
            if door_id is None:
                # The state implemented in the MegaState did not have a 
                # transition to 'ReloadState'. Thus, it was a total drop-out.
                # => Route to the state's drop-out.
                door_id = TheAnalyzer.drop_out_DoorID(state_index)
            return door_id

        cmd = Op.RouterOnStateKey(
            StateKeyRegister, MegaStateIndex,
            Iterable_StateKey_Index_Pairs,
            DoorID_provider
        )

        ta         = TransitionAction(OpList(cmd))
        # Assign a DoorID (without categorization) knowing that no such entry
        # into this state existed before.
        ta.door_id = dial_db.new_door_id(self.index)

        assert not self.entry.has_transition(self.index, MegaStateIndex) # Cannot be in there twice!
        self.entry.enter(self.index, MegaStateIndex, ta)

        return ta.door_id

