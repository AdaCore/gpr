"""PATH COMPRESSION: _______________________________________________________

Path compression tries to find traces of single character transitions inside a
state machine.  A sequence of single character transitions is called a 'path'.
Instead of implementing each state individually the states of a path are 
implemented in one MegaState, i.e. a 'PathWalkerState'.

                  
            path ['p', 'r', 'i', 'n', 't', PTC]
                         |
   path_iterator ---->---'                    

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

As shown in the above figure, a PathWalkerState implements the states on the
path by 'path walking'.  That is, it compares the current input with the
current character on the path.  If it fits, it continues on the path.  If it
does not, then it enters the PathWalkerState's transition map.  The transition 
map of all AnalyzerState-s absorbed by a PathWalkerState must be the same.

EXPLANATION: __________________________________________________________________

For path compression, traits of single character transitions are identified
while the remaining transitions of the involved states are the same (or covered
by what is triggered by the current path element). This type of compression is
useful in languages that contain keywords. Consider for example a state
machine, containing the key-word 'for':


( 0 )-- 'f' --->( 1 )-- 'o' --->( 2 )-- 'r' -------->(( 3 ))--( [a-z] )-.
   \               \               \                                    |
    \               \               \                                   |
     `--([a-eg-z])---`--([a-np-z])---`--([a-qs-z])-->(( 4 ))<-----------'


The states 0, 1, and 2 can be implemented by a PathWalkerState.  It consists of
a common transition map which is preceded by a single character check.  The
single character check changes along a fixed path: the sequence of characters
'f', 'o', 'r'.  This is shown in the following pseudo-code:

  /* Character sequence of path is stored in array. */
  character array[4] = { 'f', 'o', 'r', PTC, };

  0: p = &array[0]; goto PATH_WALKER_1;
  1: p = &array[1]; goto PATH_WALKER_1;
  2: p = &array[2]; goto PATH_WALKER_1;

  PATH_WALKER_1:
     /* Single Character Check */
     if   input == *p: ++p; goto PATH_WALKER_1;
     elif input == PTC:     goto StateAfterPath;
     elif *p == 0:          goto STATE_3;

     /* Common Transition Map: The 'skeleton transition map' */
     if   x < 'a': drop out
     elif x > 'z': drop out
     else:         goto STATE_4

The array with the character sequence ends with a path terminating character
PTC.  It acts similar to the 'terminating zero' in classical C Strings. This
way it can be detected when to trigger to the terminal state, i.e. the first
state after the path. For a state to be part of the path, it must hold that 

            'state.transition_map - transition character' 
    matches 'path.transition_map'

where 'transition character' is the character on which it transits to its
subsequent state. The 'transition character' is a 'wildcard', because it 
is handled in the pathwalker's head and not as part of the path walker's
body (if state_key = this state).

Each state which is part of a path, can be identified by the identifier of the
PathWalkerState + the position on the path (+ the id of the path, if a
PathWalkerState walks more than one path), i.e.

     state on path <--> (PathWalkerState.index, path iterator position)

Assume that in the above example the path is the 'PATH_WALKER_1' and the
character sequence is given by an array:

     path_1_sequence = { 'f', 'o', 'r', PTC };
         
then the three states 0, 1, 2 are identified as follows

         STATE_0  <--> (PATH_WALKER_1, path_1_sequence)
         STATE_1  <--> (PATH_WALKER_1, path_1_sequence + 1)
         STATE_2  <--> (PATH_WALKER_1, path_1_sequence + 2)

In the MegaState terminology, the path iterator position acts as a 'state_key'
which identifies the state the pathwalker current represents.  The
PathWalkerState implements dedicated entry doors for each state on a path,
where the path iterator (~ state  key) is set to the appropriate position.

(C) 2009-2013 Frank-Rene Schaefer
"""
import quex.engine.analyzer.mega_state.path_walker.find  as     find
from   quex.engine.analyzer.mega_state.path_walker.state import PathWalkerState
from   quex.blackboard                                   import E_Compression

from   copy        import copy

def do(TheAnalyzer, CompressionType, AvailableStateIndexSet):
    """________________________________________________________________________
    RETURNS: 
    
          list( PathWalkerState )

    NOTE: _____________________________________________________________________

          Inheritance:

                 AnalyzerState <----- MegaState <------- PathWalkerState

    A MegaState contains the '.implemented_state_index_set' which tells about
    what states have actually been implemented by the PathWalkerState.
    """
    assert CompressionType in [E_Compression.PATH, E_Compression.PATH_UNIFORM]
    assert isinstance(AvailableStateIndexSet, set)

    # (*) Find all single character transitions (paths) inside TheAnalyzer's 
    #     state machine.
    path_list = find.do(TheAnalyzer, CompressionType, AvailableStateIndexSet)

    # (*) Select paths, so that a maximum of states is implemented by path walkers.
    path_list = select(path_list)
    path_list_assert_consistency(path_list, TheAnalyzer, AvailableStateIndexSet, CompressionType)

    # (*) Group paths
    #    
    #     Different paths may have the same common transition map. If this is
    #     the case, they can be implemented by the same PathWalkerState.
    return group(path_list, TheAnalyzer, CompressionType)

def select(path_list):
    """The CharacterPath-s which have been found may intersect. However, a 
    state can only appear in one distinct path. If a set of paths intersect
    the longest of them is chosen. 

    RETURNS: list( CharacterPath ) 

    where the 'best' possible configuration of character paths is chosen.
    """
    class BelongDB(dict):
        """belong_db: 
          
                      state_index --> paths of which state_index is part. 
        """
        def __init__(self, PathList):
            self.path_list = [path for path in PathList] # clone.

            for i, path in enumerate(self.path_list):
                for state_index in path.implemented_state_index_set():
                    entry = self.get(state_index)
                    if entry is not None: entry.add(i)
                    else:                 self[state_index] = set([i])
            return

        def iterpaths(self):
            for i, path in enumerate(self.path_list):
                yield i, path

        def compute_value(self, path_i):
            """What happens if 'path' is chosen?

            -- Gain: Multiple states are implemented by a single PathWalkerState.
                    
                  gain = len(implemented states by path)

            -- Cost: If one of the states of the path intersects with another
                     path, then the other path cannot be implemented.

                  cost = number of states present in intersecting paths if those
                         states have no other paths which implements them.
            ___________________________________________________________________
            RETURNS: 
            
                     (gain - cost, extra_key, extra_key)

            where 'gain - cost' gives the value of the path. The other two
            extra keys do not relate to the value.  They identify the path
            itself and are provided as a means to make the selection process
            deterministic.
            """
            # -- The states implemented by path
            path            = self.path_list[path_i]
            implemented_set = path.implemented_state_index_set()

            # -- Forbidden are those paths which have a state in common with 'path'
            # -- Those states implemented in forbidden paths become endangered.
            #    I.e. they might not to be implemented by a PathWalkerState.
            lost_path_list               = []
            remaining_path_list          = []
            remaining_implementation_set = set()
            for other_path_i, other_path in self.iterpaths():
                if   path_i == other_path_i: continue

                # Terminal may be present in another path (exclude it from consideration)
                if implemented_set.isdisjoint(other_path.implemented_state_index_set()):
                    remaining_path_list.append(other_path)
                    remaining_implementation_set.update(path.implemented_state_index_set())
                else:
                    lost_path_list.append(other_path)

            lost_implementation_set = set()
            for lost_path in lost_path_list:
                # A state of a lost path is not implemented by path walking, if
                # it is not implemented by the 'path' itself or by one of the 
                # non-intersecting paths of 'path'.
                lost_implementation_set.update(
                         lost_path.implemented_state_index_set() \
                       - implemented_set                         \
                       - remaining_implementation_set            \
                )

            cost = - len(lost_implementation_set)
            gain =   len(implemented_set)

            # EXTRA COMPARISON KEYS, so that sorting becomes DETERMINISTIC in 
            # in case that two paths provide the SAME GAIN. Clearly, these keys
            # are no functional necessity.
            #
            #  -- The 'negative' triggers, so that lower triggers sort higher.
            extra_key_0 = tuple(- x.trigger for x in path.step_list[:-1])
            #  -- The number of the first state on the path.
            extra_key_1 = - path.step_list[0].state_index
            return (gain - cost, extra_key_0, extra_key_1)

    def get_best_path(AvailablePathList):
        """The best path is the path that brings the most gain. The 'gain'
        of a path is a function of the number of states it implements minus
        the states that may not be implemented because other intersecting
        paths cannot be chosen anymore.

        RETURN: [0] winning path
                [1] list of indices of paths which would be no longer 
                    available, because the winning path intersects.
        """
        belong_db  = BelongDB(AvailablePathList)

        max_value  = None
        max_length = None
        winner_i   = None
        for i, path in belong_db.iterpaths():
            # INPORTANT: Consider 'length == length' so that other criteria
            #            can trigger which support deterministic solutions.
            if max_value is not None and len(path.step_list) < max_length: 
                continue # No chance

            value = belong_db.compute_value(i)

            if max_value is None or max_value < value:
                max_value  = value
                winner_i   = i
                max_length = len(belong_db.path_list[i].step_list)

        return winner_i


    result    = []
    work_list = copy(path_list)
    while len(work_list):
        work_list.sort(key=lambda p: - p.state_index_set_size())

        elect_i = get_best_path(work_list)

        # Copy path from 'work_list' to 'result', 
        # BEFORE list content is changed.
        elect = work_list[elect_i]
        del work_list[elect_i]
        result.append(elect)

        dropped_list    = []
        implemented_set = elect.implemented_state_index_set()
        for i, path in enumerate(work_list):
            if implemented_set.isdisjoint(path.implemented_state_index_set()): continue
            dropped_list.append(i)

        # Delete all paths which are impossible, if 'elect_i' is chosen.
        # Reverse sort of list indices (greater first) 
        # => simply 'del work_list[i]' 
        for i in sorted(dropped_list, reverse=True):
            del work_list[i]

        # None of the remaining paths shall have states in common with the elect.
        for path in work_list:
            assert elect.implemented_state_index_set().isdisjoint(path.implemented_state_index_set())

    return result

def group(CharacterPathList, TheAnalyzer, CompressionType):
    """Different character paths may be walked down by the same path walker.
    This function groups the given list of CharacterPath-s and assigns them to
    PathWalkerState-s.  
    """
    path_walker_list = []
    # (*) Collect path walkers into groups of similar ones
    for path in CharacterPathList:
        for path_walker in path_walker_list:
            # Set-up the walk in an existing PathWalkerState
            if path_walker.accept(path, TheAnalyzer, CompressionType): 
                break
        else:
            # Create a new PathWalkerState
            path_walker = PathWalkerState(path, TheAnalyzer)
            path_walker_list.append(path_walker)

    return path_walker_list

def path_list_assert_consistency(path_list, TheAnalyzer, AvailableStateIndexList, CompressionType):
    # None of the paths shall contain a state which is not available for compression.
    # No state shall be implemented by more than one path.
    remainder = set(AvailableStateIndexList)
    for path in path_list:
        assert path.implemented_state_index_set().issubset(remainder)
        remainder.difference_update(path.implemented_state_index_set())

    # A state shall not be implemented on two paths. It was the task of 
    # 'select()' to make an optimal choice.
    appeared_state_index_set = set()
    for path in path_list:
        delta = len(path.step_list) - 1
        size_before = len(appeared_state_index_set)
        appeared_state_index_set.update(path.implemented_state_index_set())
        size_after  = len(appeared_state_index_set)
        assert size_after - size_before == delta

    # Each path must be consistent in itself
    for path in path_list:
        path.assert_consistency(TheAnalyzer, CompressionType)
