# (C) 2010-2014 Frank-Rene Schaefer
from   quex.engine.operations.operation_list                         import Op
from   quex.engine.analyzer.mega_state.core              import MegaState, \
                                                                StateKeyIndexDB
from   quex.engine.analyzer.mega_state.path_walker.find  import DropOutConsideration_cmp, \
                                                                DropOutConsideration_relate
import quex.engine.state_machine.index                   as     index
from   quex.engine.misc.tools                            import UniformObject
from   quex.blackboard                                    import E_Compression, E_Op

from   itertools  import izip

class PathWalkerState(MegaState):
    """________________________________________________________________________
    A path walker state is a state that can walk along one or more paths
    with the same remaining transition map. Objects of this class are the basis
    for code generation.

                    path ['w', 'h', 'i', 'l', 'e', PTC]
                    path ['f', 'o', 'r', PTC]
                    path ['f', 'u', 'n', 'c', 't', 'i', 'o', 'n', PTC]
                    path ['p', 'r', 'i', 'n', 't', PTC]
           path_iterator ---->--'                    
                    path ['s', 't', 'r', 'u', 'c', 't', PTC]

                    .-------------------------.
                    | path_iterator = path[0] |
                    '-------------------------'
                                |
                                |<-----------------------------------.
                                |                                    |
                   .-----------'''--------------.    true   .-----------------.
                  / *input_p == *path_iterator ? \----------| ++path_iterator |
                  \______________________________/          | ++input_p       |
                                |                           '-----------------'
                                |
                      .------------------------.
                      |                        |----- [a-h] ----> state 21
                      |                        |----- [j]   ----> state 185
                      | transition_map(*input) |----- 'o'   ----> state 312
                      |                        |----- [p-z] ----> state 21
                      |                        |----- [a-h] ----> state 21
                      '------------------------'

    The 'group()' function in 'path_walker.core.py' develops a set of path
    walkers for a set of given CharacterPath list.
    ___________________________________________________________________________
    """
    def __init__(self, FirstPath, TheAnalyzer):
        my_index       = index.get()
        ski_db         = StateKeyIndexDB([x.state_index for x in FirstPath.step_list],
                                         IgnoredListIndex=len(FirstPath.step_list)-1)
        MegaState.__init__(self, my_index, FirstPath.transition_map, ski_db)

        # Uniform OpList along entries on the path (optional)
        self.uniform_entry_OpList = FirstPath.uniform_entry_OpList.clone()

        self.__path_list = [ FirstPath.step_list ]

        # Following is set by 'finalize()'.
        self.__finalized = None # <-- ._finalize_content()

    @property
    def door_id_sequence_list(self):
        return self.__finalized.door_id_sequence_list

    @property
    def uniform_door_id(self):
        """At any step along the path commands may be executed upon entry
           into the target state. If those commands are uniform, then this
           function returns a OpList object of those uniform commands.

           RETURNS: None, if the commands at entry of the states on the path
                          are not uniform.
        """
        return self.__finalized.uniform_door_id.content

    @property
    def uniform_terminal_door_id(self):
        """RETURNS: DoorID -- if all paths which are involved enter the same 
                               terminal state through the same entry door.
                    None   -- if not.
        """
        return self.__finalized.uniform_terminal_door_id.content

    @property
    def path_list(self):          
        assert type(self.__path_list) == list
        return self.__path_list

    def accept(self, Path, TheAnalyzer, CompressionType):
        """Checks whether conditions of absorbing the Path are met, and if
        so then the Path is absorbed. 

        RETURNS: False -- Path does not fit the PathWalkerState.
                 True  -- Path can be walked by PathWalkerState and has been 
                          accepted.
        """
        if not self.__can_absorb_path(Path, TheAnalyzer, CompressionType):
            return False

        self.__absorb_path(Path, TheAnalyzer)
        return True

    def __can_absorb_path(self, Path, TheAnalyzer, CompressionType):
        """Check whether a path can be walked along with the given PathWalkerState.
        For this, the following has to hold:

            -- The transition_maps must match.
            -- If uniformity is required, the entries and drop-outs must 
               be uniform with the existing onces.
        """
        if not self.transition_map.is_equal(Path.transition_map, DropOutConsideration_cmp): 
            return False

        if CompressionType == E_Compression.PATH_UNIFORM:
            if not self.uniform_entry_OpList.fit(Path.uniform_entry_OpList):
                return False

        return True

    def __absorb_path(self, Path, TheAnalyzer):
        """-- Absorb the state sequence of the path.
           -- Absorb the Entry/DropOut information.
        """

        # (Meaningful paths consist of more than one state and a terminal.)
        assert len(Path.step_list) > 2
        self.__path_list.append(Path.step_list)

        # (1) Absorb the state sequence of the path.
        #
        new_state_index_list = [x.state_index for x in Path.step_list]
        terminal_index       = len(new_state_index_list) - 1
        # Assert: A state cannot be implemented on two different paths.
        assert self.ski_db.not_implemented_yet(new_state_index_list[:terminal_index])
        self.ski_db.extend(new_state_index_list, IgnoredListIndex=terminal_index)

        # (2) Absorb Entry/DropOut Information
        #
        self.uniform_entry_OpList <<= Path.uniform_entry_OpList

        return True

    def _finalize_entry_OpLists(self):
        """If a state is entered from outside the path walker, then the 'state_key',
        respectively, the 'path_iterator' needs to be set. During the walk along
        a path, the 'path_iterator' is simply incremented--and this happens in the
        code generated for the path walker (later on).

        NOTE: Here, it must be ensured that the DoorID-s for entries from 
              outside remain the same! This way, any external transition map
              may remain the same.
        """
        # Entries along the path: PathIterator Increment
        #                         ... but this is handled better by the code generator.
        # Entries from outside:   PathIteratorSet
        for path_id, step_list in enumerate(self.__path_list):
            prev_state_index = None
            # Terminal is not element of path => consider only 'step_list[:-1]'
            for offset, step in enumerate(step_list[:-1]):
                # Inside transition:     'prev_state.index --> step.state_index'
                # All other transitions: '       *         --> step.state_index'
                # are transitions from states outside the path.
                state_key = offset
                # Update sets inside transition's 'door_id = None' and adds
                # the transition to 'transition_reassignment_candidate_list'.
                self.entry.action_db_update(From           = prev_state_index, 
                                            To             = step.state_index, 
                                            FromOutsideOp = Op.PathIteratorSet(self.index, path_id, state_key),
                                            FromInsideOp  = None)

                prev_state_index = step.state_index

        # Make sure, that the OpList-s on the paths are organized and
        # assigned with new DoorID-s. 
        assert len(self.entry.transition_reassignment_candidate_list) > 0

    def _finalize_transition_map(self, TheAnalyzer):
        """All drop-outs of this path walker enter a common door in the drop-out
        catcher. There, they are routed to the drop-outs for the current state
        which the path walker representes. The current state is given by the 
        state key.
        """
        # Any drop-out in the transition map must become a 'goto path walker's i
        # drop-out'. In the path walker's drop-out it is routed to the drop-out of
        # the state which it currently represented.
        drop_out_door_id = TheAnalyzer.drop_out_DoorID(self.index)
        self.transition_map.adapt_targets(drop_out_door_id, DropOutConsideration_relate)
        self.transition_map.combine_adjacents()

    def _finalize_content(self, TheAnalyzer):
        self.__finalized = FinalizedContent(self, TheAnalyzer)

    def get_Trigger_DoorID_by_state_key(self, StateKey):
        # Find (transition char, DoorID) for given StateKey
        i      = 0
        offset = 0
        for i in xrange(len(self.path_list)):
            end = offset + len(self.path_list[i]) - 1
            if StateKey < end: 
                break
            offset = end + 1

        step_i  = StateKey - offset
        return self.path_list[i][step_i].trigger, self.door_id_sequence_list[i][step_i]

    def _get_target_by_state_key(self, Begin, End, TargetScheme, StateKey):
        """In a PathWalkerState's transition map, the targets are DoorID-s. They
        do not depend on a StateKey. The ones which depend on a state key are 
        are the ones on a path.
        """
        # First, look if the character lies on the path. If not rely on the
        # transition map' target DoorID as it is.
        if End - Begin == 1:
            trigger, door_id = self.get_Trigger_DoorID_by_state_key(StateKey)
            if Begin == trigger:
                return door_id

        return TargetScheme

    def _assert_consistency(self, CompressionType, RemainingStateIndexSet, TheAnalyzer):            
        # If uniform_entry_OpList is claimed, then the DoorID must be 
        # the same along all paths--and vice versa.
        assert    (self.uniform_door_id is not None) \
               == self.uniform_entry_OpList.is_uniform()

        # If uniformity was required, then it must have been maintained.
        if CompressionType == E_Compression.PATH_UNIFORM:
            assert self.uniform_door_id is not None
            assert self.uniform_entry_OpList.is_uniform()

        # The door_id_sequence_list corresponds to the path_list.
        assert len(self.door_id_sequence_list) == len(self.path_list)
        for door_id_sequence, step_list in izip(self.door_id_sequence_list, self.path_list):
            # Path entry is not element of door_id_sequence => '-1'
            assert len(door_id_sequence) == len(step_list) - 1 

        # A OpList at a door can at maximum contain 1 path iterator command!
        for action in self.entry.itervalues():
            path_iterator_cmd_n = 0
            for cmd in action.command_list:
                if cmd.id != E_Op.PathIteratorSet: continue
                path_iterator_cmd_n += 1
                assert path_iterator_cmd_n < 2

class FinalizedContent(object):
    __slots__ = ("uniform_door_id",
                 "uniform_terminal_door_id",
                 "door_id_sequence_list")

    def __init__(self, PWState, TheAnalyzer):
        self.uniform_door_id          = UniformObject()
        self.uniform_terminal_door_id = UniformObject()
        self.door_id_sequence_list    = []
        for step_list in PWState.path_list:
            # Meaningful paths consist of more than one state and a terminal. 
            assert len(step_list) > 2

            door_id_sequence = self.__determine_door_id_sequence(step_list, TheAnalyzer, PWState)

            self.door_id_sequence_list.append(door_id_sequence)

        return

    def __determine_door_id_sequence(self, step_list, TheAnalyzer, PWState):
        """Determines the sequence of DoorID-s for a given 'step_list' and
        adapts 'uniform_door_id', and 'uniform_terminal_door_id'.
        """
        # -- States on path
        #    (entries are considered from the second state on path on)
        door_id_sequence = []
        prev_step        = step_list[0]
        action_db        = PWState.entry
        for step in step_list[1:-1]:
            # (Recall: there is only one transition (from, to) => TriggerId == 0)
            door_id = action_db.get_door_id(step.state_index, prev_step.state_index, TriggerId=0)

            # Every DoorID on the path must be a newly-assigned one to this PathWalkerState.
            assert door_id.state_index == PWState.index

            door_id_sequence.append(door_id)
            self.uniform_door_id <<= door_id

            prev_step = step

        # -- Terminal
        step      = step_list[-1] 

        #! A terminal of one path cannot be element of another path of the
        #! same PathWalkerState. This might cause huge trouble!
        #! (Ensured by the function '.accept(Path)')
        # assert step.state_index not in PWState.implemented_state_index_set()

        action_db = TheAnalyzer.state_db[step.state_index].entry
        door_id   = action_db.get_door_id(step.state_index, prev_step.state_index, TriggerId=0)

        door_id_sequence.append(door_id)
        self.uniform_terminal_door_id <<= door_id

        return door_id_sequence

