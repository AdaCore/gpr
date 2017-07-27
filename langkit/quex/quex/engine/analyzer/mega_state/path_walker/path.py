from   quex.engine.analyzer.state.core            import AnalyzerState
from   quex.engine.analyzer.door_id_address_label import DoorID
from   quex.engine.analyzer.transition_map        import TransitionMap       

from   quex.engine.misc.tools                       import UniformObject

from   quex.blackboard                         import E_Compression, E_StateIndices

from   collections import namedtuple

from   copy import deepcopy

class CharacterPathStep(namedtuple("CharacterPathStep_tuple", ("state_index", "trigger"))):
    """See also class 'CharacterPath' where the role of the CharacterPathStep
       is explained further.

       The CharacterPathStep contains information about one single step
       along the CharacterPath. It tells from what state the step starts

                        .source_state_index   .trigger     
                        .---.                                        .---.
                        | 1 |------------------ 'g' ---------------->| 3 |
                        '---'                                        '---'

       The '1' is the '.state_index' of the current CharacterPathStep on the
       CharacterPath. The step from state 1 to 3 is described by:

                current.state_index =  1   # Index of the state where the step starts
                current.trigger     = 'g'  # The triggering character

                next.state_index    =  3   # Index of the state where the step goes
   
       That is, the full information about a step containing the 'from state', the 
       'to state', and the 'trigger' by which the transition is made, can only be 
       observed when the CharacterPathStep plays its role in a CharacterPath.

       Special Case: 
       
          .trigger = None

          => Terminal, no further transition. '.state_index' is the first state 
             behind the path.
    """
    def __new__(self, StateIndex, TransitionChar):
        assert isinstance(StateIndex, long)
        assert TransitionChar is None or isinstance(TransitionChar, (int, long))
        return super(CharacterPathStep, self).__new__(self, StateIndex, TransitionChar)

    def __repr__(self):
        return ".state_index = %s;\n" \
               ".trigger     = '%s';\n" \
               % (self.state_index, self.trigger)

class TransitionMapData(object):
    __slots__ = ("transition_map", "wildcard_char")

    def __init__(self, TheTransitionMap, TransitionCharacter):
        if TheTransitionMap is None: return # Called from '.clone()'
        self.wildcard_char  = deepcopy(TransitionCharacter)
        self.transition_map = TheTransitionMap.clone()
        self.transition_map.set_target(self.wildcard_char, E_StateIndices.VOID)

    def clone(self):
        result = TransitionMapData(None, None)
        result.transition_map = self.transition_map.clone()
        result.wildcard_char  = deepcopy(self.wildcard_char)
        return result

    def plug_this(self, WildCardPlug):
        if WildCardPlug == -1: return
        assert self.wildcard_char is not None
        assert self.transition_map.get_target(self.wildcard_char) == E_StateIndices.VOID
        self.transition_map.set_target(self.wildcard_char, WildCardPlug)
        self.wildcard_char = None

    def finalize(self):
        # Ensure that there is no wildcard in the transition map
        if self.wildcard_char is None: return

        self.transition_map.smoothen(self.wildcard_char)
        self.wildcard_char = None

    def get_string(self):
        L   = max(len(x[0].get_utf8_string()) for x in self.transition_map)
        def stringify(interval, target_door_id):
            interval_str  = interval.get_utf8_string()
            return "   %s%s -> %s\n" % (interval_str, " " * (L - len(interval_str)), target_door_id)

        return "".join( 
           stringify(interval, target_door_id) 
           for interval, target_door_id in self.transition_map
        )

class CharacterPath(object):
    """________________________________________________________________________

    Represents identified path in the state machine such as:


       ( 1 )--- 'a' -->( 2 )--- 'b' -->( 3 )--- 'c' -->( 4 )--- 'd' -->( 5 )

    where the remaining transitions in each state match (except for the last).

    During analysis, the CharacterPath acts as a container which also stores
    information about the transition_map. The TransitionMapData object
    maintains information about possible transition_map matches with other
    states.

    ___________________________________________________________________________
    MEMBERS:
  
    .step_list: (list of CharacterPathStep-s)

       .------------------. .------------------.       .--------------------.
       |.state_index = 1  | |.state_index = 2  |       |.state_index = 4711 |
       |.trigger     = 'a'| |.trigger     = 'b'| . . . |.trigger     = None |
       '------------------' '------------------'       '--------------------'

       The last element of the '__step_list' contains the terminal. The 
       terminal state is not implemented by the path walker. It is the first
       state entered after the path walker has finished. Its role is further
       indicated by a trigger of 'None'.
   
    .uniform_entry_OpList:

      This object  keeps track about the actions to  be executed upon entry
      into a state on the path and upon drop-out of a state on on the path. If 
      any of them is the same for all paths on the state it contains an object
      which is not 'None'. In case it is not uniform 'None' is contained.

    .transition_map_data

     maintains data about the common transition map of all states. 
    ___________________________________________________________________________
    """
    __slots__ = ("__step_list",       
                 "transition_map_data", 
                 "uniform_entry_OpList") 

    def __init__(self, StartState, TheTransitionMap, TransitionCharacter):
        if StartState is None: return # Only for Clone

        assert isinstance(StartState,          AnalyzerState)
        assert isinstance(TransitionCharacter, (int, long)), "Found '%s'" % TransitionCharacter
        assert isinstance(TheTransitionMap,    TransitionMap)

        # uniform_entry_OpList: 
        #    The entry into the StartState happens from outside.
        #    => A 'state_key' is assigned (the path_iterator)
        #       and it is not part of the iteration loop.
        #    => The StartState's entry IS NOT subject to uniformity considerations.
        self.uniform_entry_OpList = UniformObject()

        self.__step_list = [ CharacterPathStep(StartState.index, TransitionCharacter) ]

        self.transition_map_data = TransitionMapData(TheTransitionMap, TransitionCharacter)

    def clone(self):
        result = CharacterPath(None, None, None)

        result.uniform_entry_OpList       = self.uniform_entry_OpList.clone()
        result.__step_list                     = [ x for x in self.__step_list ] # CharacterPathStep are immutable
        result.transition_map_data             = self.transition_map_data.clone()
        return result

    def extended_clone(self, PreviousTerminal, TransitionCharacter, TransitionMapWildCardPlug):
        """Append 'State' to path:

        -- add 'State' to the sequence of states on the path.

        -- absorb the 'State's' drop-out and entry actions into the path's 
           drop-out and entry actions.
        
        If 'TransitionCharacter' is None, then the state is considered a
        terminal state. Terminal states are not themselves implemented inside a
        PathWalkerState. Thus, their entry and drop out information does not
        have to be absorbed.
        """
        assert    TransitionCharacter is not None
        assert    isinstance(TransitionMapWildCardPlug, DoorID) \
               or TransitionMapWildCardPlug == -1

        result = self.clone()

        # OpList upon Entry to State
        # (TriggerIndex == 0, because there can only be one transition from
        #                     one state to the next on the path).
        prev_step         = self.__step_list[-1]
        entry_OpList = PreviousTerminal.entry.get_command_list(PreviousTerminal.index, 
                                                                              prev_step.state_index,
                                                                              TriggerId=0)
        assert entry_OpList is not None

        result.uniform_entry_OpList <<= entry_OpList

        result.__step_list.append(CharacterPathStep(PreviousTerminal.index, TransitionCharacter))

        result.transition_map_data.plug_this(TransitionMapWildCardPlug)

        return result

    def uniformity_with_predecessor(self, State):
        """Check whether the entry of the last state on the path executes the
           same OpList as the entry to 'State'. 
        """
        # This function is supposed to be called only when uniformity is required.
        # If so, then the path must be in a uniform state at any time. 
        assert self.uniform_entry_OpList.is_uniform()

        # Check on 'Entry' for what is done along the path.
        #
        # OpList upon Entry to State
        # (TriggerIndex == 0, because there can only be one transition from
        #                     one state to the next on the path).
        prev_step    = self.__step_list[-1]
        command_list = State.entry.get_command_list(State.index, prev_step.state_index, 
                                                              TriggerId=0)
        assert command_list is not None

        if not self.uniform_entry_OpList.fit(command_list): 
            return False

        return True

    def has_wildcard(self):
        return self.transition_map_data.wildcard_char is not None

    @property
    def step_list(self):
        return self.__step_list

    @property
    def transition_map(self):
        return self.transition_map_data.transition_map

    def finalize(self, TerminalStateIndex):
        self.__step_list.append(CharacterPathStep(TerminalStateIndex, None))
        self.transition_map_data.finalize()

    def contains_state(self, StateIndex):
        """Is 'StateIndex' on the path(?). This includes the terminal."""
        for dummy in (x for x in self.__step_list if x.state_index == StateIndex):
            return True
        return False

    def implemented_state_index_set(self):
        return set(x.state_index for x in self.__step_list[:-1])

    def state_index_set_size(self):
        return len(self.__step_list) - 1

    def state_index_set(self):
        assert len(self.__step_list) > 1
        return set(x.state_index for x in self.__step_list[:-1])

    def __repr__(self):
        return self.get_string()

    def get_string(self, NormalizeDB=None):
        # assert NormalizeDB is None, "Sorry, I guessed that this was no longer used."
        def norm(X):
            assert isinstance(X, (int, long)) or X is None
            return X if NormalizeDB is None else NormalizeDB[X]

        skeleton_txt = self.transition_map_data.get_string()

        sequence_txt = ""
        for x in self.__step_list[:-1]:
            sequence_txt += "(%i)--'%s'-->" % (x.state_index, chr(x.trigger))
        sequence_txt += "[%i]" % norm(self.__step_list[-1].state_index)

        return "".join(["start    = %i;\n" % norm(self.__step_list[0].state_index),
                        "path     = %s;\n" % sequence_txt,
                        "skeleton = {\n%s}\n"  % skeleton_txt, 
                        "wildcard = %s;\n" % repr(self.transition_map_data.wildcard_char is not None)])


    def assert_consistency(self, TheAnalyzer, CompressionType):
        assert len(self.__step_list) >= 2

        # If uniformity was required, it must have been maintained.
        if CompressionType == E_Compression.PATH_UNIFORM:
            assert self.uniform_entry_OpList.is_uniform()

        # If entry command list is claimed to be uniform
        # => then all states in path must have this particular drop-out (except for the first).
        if self.uniform_entry_OpList.is_uniform():
            prev_state_index = self.__step_list[0].state_index
            for state in (TheAnalyzer.state_db[s.state_index] for s in self.__step_list[1:-1]):
                command_list = state.entry.get_command_list(state.index, prev_state_index, TriggerId=0)
                assert command_list == self.uniform_entry_OpList.content
                prev_state_index = state.index
        return

