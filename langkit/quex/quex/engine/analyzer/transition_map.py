from   quex.engine.misc.interval_handling              import Interval
from   quex.engine.misc.tools                          import r_enumerate
from   quex.engine.analyzer.mega_state.target     import TargetByStateKey
from   quex.engine.analyzer.door_id_address_label import DoorID
from   quex.engine.analyzer.state.entry_action    import TransitionID
from   quex.blackboard                            import E_StateIndices, \
                                                         setup as Setup

from   itertools   import izip
from   collections import defaultdict
import sys

class TransitionMap(list):
    """________________________________________________________________________
    A TransitionMap is a sorted list of pairs:

                [ [ interval_0, target_0],
                  [ interval_1, target_1],
                  ...
                  [ interval_N, target_N] ]

    where the intervals are sorted and non-overlapping. Drop-out intervals 
    are best associated with a target == *DROP_OUT.

    NOTE: len(self) == 0 indicates: empty transition map.
    ___________________________________________________________________________
    """
    @classmethod
    def from_TargetMap(cls, TM):
        """Derives a TransitionMap object from a target map of a 'normal' state
        machine state.
        """
        result = cls()
        if (TM is not None) and (not TM.is_empty()): 
            for target, character_set in TM.get_map().iteritems():
                result.extend(
                    (interval.clone(), target) 
                    for interval in character_set.get_intervals(PromiseToTreatWellF=True)
                )
            assert len(result) != 0 # Empty target maps have been handled before
            result.sort()

        result.fill_gaps(E_StateIndices.DROP_OUT, 
                         Setup.buffer_codec.drain_set.minimum(),  
                         Setup.buffer_codec.drain_set.supremum())
        result.assert_boundary(Setup.buffer_codec.drain_set.minimum(), 
                               Setup.buffer_codec.drain_set.supremum()) 
        result.assert_continuity()
        return result

    @classmethod
    def from_iterable(cls, Iterable, TheTargetFactory=None):
        result = cls()
        if TheTargetFactory is None:
            result.extend((interval.clone(), target) 
                          for interval, target in Iterable)
        else:
            result.extend((interval.clone(), TheTargetFactory(target)) 
                          for interval, target in Iterable)
        return result

    def clone(self):
        result = self.from_iterable(self)
        return result

    @staticmethod
    def izip(TransitionMapA, TransitionMapB):
        """Produces an iterable over two transition maps at once. The borders in the
        zipped transition map consist of a superset of all borders of transition
        map 'A' and 'B'. Whenever a border hits a new interval is notified. 

                YIELDS:  begin, end, a_target, b_target

        Interval [begin, end) is homogenous in the zipped transition map, i.e.
        inside this interval 'A' triggers to 'a_target' and 'B' to 'b_target'.  

        NOTE: The name is derived from python's 'itertools.izip'.
        """
        LenA = len(TransitionMapA)
        LenB = len(TransitionMapB)
        assert LenA != 0 
        assert LenB != 0 
        TransitionMapA.assert_boundary(TransitionMapB[0][0].begin, 
                                       TransitionMapB[-1][0].end)

        i                = 0 # iterator over TransitionMapA
        k                = 0 # iterator over TransitionMapB
        i_itvl, i_target = TransitionMapA[0]
        k_itvl, k_target = TransitionMapB[0]
        begin            = i_itvl.begin
        end_of_both      = TransitionMapA[-1][0].end
        prev_end         = begin 
        # Intervals in trigger map are always adjacent, so the '.begin' member is
        # not accessed.
        while not (i == LenA - 1 and k == LenB - 1):
            end    = min(i_itvl.end, k_itvl.end)

            yield prev_end, end, i_target, k_target

            prev_end  = end

            if   i_itvl.end == k_itvl.end: 
                i += 1; i_itvl, i_target = TransitionMapA[i]
                k += 1; k_itvl, k_target = TransitionMapB[k]
            elif i_itvl.end <  k_itvl.end: 
                i += 1; i_itvl, i_target = TransitionMapA[i]
            else:                          
                k += 1; k_itvl, k_target = TransitionMapB[k]

        # Treat the last trigger interval
        if prev_end != end_of_both:
            yield prev_end, end_of_both, TransitionMapA[-1][1], TransitionMapB[-1][1]
        return

    def relate_to_DoorIDs(self, TheAnalyzer, StateIndex):
        """Creates a transition_map that triggers to DoorIDs instead of target states.
        """
        def relate(Target):
            if Target == E_StateIndices.DROP_OUT:
                te     = TheAnalyzer.drop_out.entry
            elif  Target == E_StateIndices.RELOAD_FORWARD \
               or Target == E_StateIndices.RELOAD_BACKWARD:
                te = TheAnalyzer.reload_state.entry
            else:
                te = TheAnalyzer.state_db[Target].entry

            door_id = te.get_door_id(StateIndex=Target, FromStateIndex=StateIndex)
            assert door_id is not None, \
                    "State %s: No DoorID for { from: %s to: %s }\nentry: %s" % \
                    (Target, StateIndex, Target, [tid for tid, ta in te.iteritems()])
            return door_id
        
        return self.__class__.from_iterable(self, relate)

    def relate_to_TargetByStateKeys(self, StateIndex, DropOutCatcher):
        """ASSUME: The transition map targets DoorID-s. 
        
        Then the internal DoorID-s are translated into TargetByStateKey objects.
        """
        def relate(TargetDoorId):
            transition_id = TransitionID(TargetDoorId.state_index, StateIndex, TriggerId=0)
            door_id       = TargetDoorId
            return TargetByStateKey.from_transition(transition_id, door_id)

        return self.__class__.from_iterable(self, relate)

    def adapt_targets(self, helper_object, adapt_this):
        """Adapts targets in the transition map independent on the intervals. 
        
        'adapt' is a function which:
           -- receives the target of an interval as argument. 
           -- returns 'None' if the value of the target shall not be changed. 
           -- if it returns a value which 'is not None', then this value
              is implemented as the replacement for the target.
        """
        for i, info in enumerate(self):
            interval, target = info
            new_target = adapt_this(helper_object, target)
            if new_target is None: continue
            self[i] = (interval, new_target)

    def contains_DoorIDs(self, DoorIdSet):
        for i, info in self:
            assert isinstance(info, DoorID), "%s%s" % (info.__class__, info)
            if info in DoorIdSet: return True
        return False

    def has_drop_out(self):
        for interval, target in self:
            if   target == E_StateIndices.DROP_OUT: return True
            elif not hasattr(target, "drop_out_f"): continue
            elif target.drop_out_f():               return True
        return False

    def is_only_drop_out(self):
        """RETURNS: True  -- if there is only one target which is 'DROP-OUT'.
                    False -- if there are transitions to other states.
        """
        assert len(self) != 0
        if len(self) != 1: return False
        assert isinstance(self[0][1], (TargetByStateKey, DoorID))
        return self[0][1].drop_out_f()

    def is_equal(self, Other, EqualCmp=lambda x,y: x==y):
        if len(self) != len(Other):        return False
        for x, y in izip(self, Other):
            if   x[0] != y[0]:             return False  # Interval
            elif not EqualCmp(x[1], y[1]): return False  # Target
        return True

    def is_empty(self):
        if   len(self) == 0: return True
        elif len(self) == 1: return self[0][1].drop_out_f()
        else:                return False

    def match_with_wildcard(self, Other, ExceptionCharacter=None, EqualCmp=lambda x,y: x==y):
        """Determines whether the transition map matches Other. If 'self' 
        contains a transition to 'E_StateIndices.VOID', then a wild card
        may be applied. A transition of Other to 'ExceptionTarget' on 
        'ExceptionCharacter' is ignored as being different.

        RETURNS: 
         
        int > 0, the character that the wild card shall take so that the
                 path's transition map matches the TransitionMap.

        - 1,     if path's transition map and TransitionMap match anyway 
                 and no wild card plug is necessary.

        None,    if there is no way that the path's transition map and the
                 TransitionMap could match.
        """
        wildcard_target = -1
        for begin, end, a_target, b_target in TransitionMap.izip(self, Other):
            if EqualCmp(a_target, b_target): continue    

            # Here: Mismatching targets on same character interval.

            if   end - begin != 1:             # No help for size > one character.
                return None
            elif begin == ExceptionCharacter:  # Difference on 'ExceptionCharacter' ignored                         
                continue
            elif wildcard_target == -1 and a_target == E_StateIndices.VOID: 
                wildcard_target = b_target     # Wildcard used to plug hole
                continue
            else:                              # No exception, no wildcard plug -> misfit                               
                return None

        # Here: The transition maps match, but possibly require the use of a wildcard.
        return wildcard_target

    def get_target(self, Character):
        i = TransitionMap.bisect(self, Character)
        if i is None: return None
        return self[i][1]

    @staticmethod
    def get_target_statistics(List):
        """RETURNS: [0] Most often appearing target.
                    [1] Number of different targets.
        """
        db = defaultdict(int)
        for interval, target in List:
            db[target] += interval.size()

        max_n      = -1
        max_target = None
        for target, n in db.iteritems():
            if n <= max_n: continue
            max_n      = n
            max_target = target
        return max_target, len(db)

    @staticmethod
    def get_size_of_range_other_targets(List, Target):
        """Sum up the ranges of intervals that target a target that is
        different from 'Target'.
        """
        return sum(interval.size() for interval, target in List
                   if target != Target
        )

    def set_target(self, Character, NewTarget):
        """Set the target in the transition map for a given 'Character'.
        """
        # Find the index of the interval which contains 'Character'
        i = TransitionMap.bisect(self, Character)
        if i is None:
            self.insert(0, (Interval(Character), NewTarget))
            self.sort()
            return

        # Split the found interval, if necessary, so that the map
        # contains 'Character' --> 'NewTarget'.
        interval, target = self[i]
        assert interval.size() > 0

        new_i = None

        if target == NewTarget: 
            return # Nothing to be done

        elif interval.size() == 1:
            self[i] = (interval, NewTarget)
            new_i   = i

        elif Character == interval.end - 1:
            self.insert(i+1, (Interval(Character), NewTarget))
            interval.end -= 1
            new_i         = i + 1

        elif Character == interval.begin:
            self.insert(i, (Interval(Character), NewTarget))
            interval.begin += 1
            new_i           = i

        else:
            self.insert(i+1, (Interval(Character), NewTarget))
            self.insert(i+2, (Interval(Character+1, interval.end), target))
            interval.end = Character 
            new_i        = i + 1

        # Combine adjacent intervals which trigger to the same target.
        self.combine_adjacents(new_i)
        self.assert_continuity()
        return

    def index(self, Character):
        """Searches for interval that contains 'Character' and returns
           the index in the transition map to the corresponding entry.

           RETURNS: None      -- Character not found
                    index > 0 -- where interval 'self[i][0]' contains Character.
        """
        return TransitionMap.bisect(self, Character)

    @staticmethod
    def bisect(List, Character):
        """Searches for Character in transition map by bisectioning.
        
        RETURNS: None -- if Character has not been found in List.
                 N    -- The index of the interval in which the Character
                         can be found.
        """
        lower = 0
        upper = len(List)
        if upper == 0:
            return None

        while upper - lower > 1:
            i = (upper + lower) >> 1
            if   List[i][0].begin >  Character: upper = i
            elif List[i][0].end   <= Character: lower = i
            else:                                         return i

        if     Character >= List[lower][0].begin \
           and Character <  List[lower][0].end:
            return lower

        return None

    def combine_adjacents(self, Index=None):
        """If two adjacent intervals have the same target, then combine both
           into a single one. If 'Index' is specified only the neighbours of
           self[i] are considered.
        """
        L = len(self) 
        if L == 0: return

        if Index is None:
            range_iterable = reversed(xrange(L))
        else:
            assert Index >= 0  and Index < L
            range_iterable = (
                    idx for idx in (Index + 1, Index, Index - 1)
                        if idx >= 0 and idx < L
            )

        prev_interval, prev_target = self[range_iterable.next()]
        for i in range_iterable:
            interval, target = self[i]
            if interval.end == prev_interval.begin and prev_target == target:
                interval.end = prev_interval.end
                del self[i+1]
            else:
                prev_target   = target
            prev_interval = interval

    def smoothen(self, Character):
        """Replaces a single character transition by a transition of its adjacent 
           intervals.
        """
        i = TransitionMap.bisect(self, Character)
        assert i is not None
        assert self[i][0].size() == 1

        L = len(self)
        if i > 0: 
            if i < L - 1 and self[i+1][1] == self[i-1][1]:
                self[i-1][0].end = self[i+1][0].end
                del self[i:i+2]
            else:
                self[i-1][0].end = Character
                del self[i]

        elif i < L:
            self[i+1][0].end = Character + 1
            del self[i]
        else:
            assert False

    def sort(self):
        list.sort(self, key=lambda x: (x[0].begin, x[0].end))

        # double check -- no overlapping!
        if len(self) != 0:
            prev_interval = self[0][0]
            for interval, target in self[1:]:
                assert interval.begin >= prev_interval.end

    def clean_up(self):
        """NOTE: 'clean_up()' does not mean that there are no gaps! For that
                 consider '.combine_adjacents()' or '.fill_gaps()'
        """
        self.sort()
        self.combine_adjacents()

    def fill_gaps(self, Target, Begin, End):
        """Fill gaps in the transition map. 
        Begin -- begin of range of intervals
        End   -- end of range of intervals.
        """
        size  = len(self)
        if size == 0:
            self.append((Interval(Begin, End), Target))
            return

        assert self[0][0].begin >= Begin, "FAILED: begin %x >= %s" % (self[0][0].begin, Begin)
        assert self[-1][0].end  <= End,   "FAILED: end   %x <= %s" % (self[-1][0].end, End)

        # If outer borders are lacking, then add them
        if self[0][0].begin != Begin: 
            self.insert(0, (Interval(Begin, self[0][0].begin), Target))
            size += 1

        if self[-1][0].end != End: 
            self.append((Interval(self[-1][0].end, End), Target))
            size += 1

        # Fill gaps between the intervals
        prev_end    = self[0][0].end
        prev_target = self[0][1]
        i           = 1
        while i < size:
            interval = self[i][0]
            target   = self[i][1]

            assert interval.begin != interval.end
            assert interval.begin >= prev_end, \
                   "tm: %s\n" % self.get_string() + \
                   "i: %i\n" % i + \
                   "int,target: %s, %s\n" % (interval, target) + \
                   "prev_end, prev_target: %s, %s\n" % (prev_end, prev_target)

            if prev_end == interval.begin: 
                if prev_target == target:
                    # (*) Combine two intervals which have the same target
                    self[i-1][0].end = interval.end
                    del self[i]
                    size -= 1
                else:
                    i += 1
                prev_end    = interval.end
                prev_target = target

            elif prev_target != Target:
                # (*) Gap detected, targets differ => fill!
                self.insert(i, (Interval(prev_end, interval.begin), Target))
                size += 1
                # prev_target = same;
                # prev_end    = same; 
                # NOT: i+=1, because need to check for combination with next interval.

            else:
                # (*) Gap detected, targets equal => extend!
                self[i-1][0].end = interval.begin
                prev_target = Target
                prev_end    = interval.begin
                # NOT: i+=1, because need to check for combination with next interval.

    def prune(TriggerMap, Begin, End):
        """Cut out any element in the trigger map which lies beyong [Begin:End)
        """
        L = len(TriggerMap)

        # Iterate from 'low' to 'high'
        begin_i = None
        for i, info in enumerate(TriggerMap):
            interval, target = info
            if interval.end <= Begin: continue

            # Found an interval that intersects with 'Begin' line
            if interval.begin < Begin:
                interval.begin = Begin
            begin_i = i
            break

        if   begin_i is None: del TriggerMap[:] # No element is in range
        elif begin_i != 0:    del TriggerMap[:begin_i]

        # Iterate from 'high' to 'low'
        end_i = None
        for i, info in r_enumerate(TriggerMap):
            interval, target = info
            if interval.begin >= End: continue

            # Found an interval that intersects with 'End' line
            if interval.end > End:
                interval.end = End 

            end_i = i
            break

        if   end_i is None: del TriggerMap[:] # No element in range
        elif end_i != L-1:  del TriggerMap[end_i+1:]

        # Delete all intervals which have become empty
        i    = 0
        size = len(TriggerMap)
        while i < size:
            if TriggerMap[i][0].begin == TriggerMap[i][0].end:
                del TriggerMap[i]
                size -= 1
            else:
                i += 1

        return

    def assert_no_empty_action(self):
        for interval, action in self:
            assert action is not None
            assert not isinstance(action, list) or len(action) != 0

    def assert_continuity(self, StrictF=True):
        """StrictF => Assume that adjacent intervals have been combined. 

           Tests that all intervals appear in a sorted manner.

           NOTE: 'assert_adjacency' is stronger than 'assert_continuity'. 
                 It is not necessary to test for both. Rather decide what
                 level needs to be asserted.
        """
        if len(self) == 0:
            return 

        iterable                   = self.__iter__()
        prev_interval, prev_target = iterable.next()
        # No 'empty' intervals
        assert prev_interval.end > prev_interval.begin

        for interval, target in iterable:
            assert interval.end > interval.begin        # No empty intervals
            assert interval.begin >= prev_interval.end  # Intervals appear sorted, 
            if interval.begin == prev_interval.end and StrictF:
                # If the touch, require the target is different
                assert target != prev_target, "%s" % self.get_string()
            prev_interval = interval
            prev_target   = target

    def assert_adjacency(self, TotalRangeF=False, ChangeF=False):
        """Check that the trigger map consist of sorted adjacent intervals 
           This assumption is critical because it is assumed that for any isolated
           interval the bordering intervals have bracketed the remaining cases!
        """
        if len(self) == 0: 
            assert not TotalRangeF
            return

        if TotalRangeF: 
            assert self[0][0].begin == Setup.buffer_codec.drain_set.minimum()
            assert self[-1][0].end  == Setup.buffer_codec.drain_set.supremum()

        iterable    = self.__iter__()
        info        = iterable.next()
        prev_end    = info[0].end
        prev_target = info[1]

        for interval, target in iterable:
            assert interval.end > interval.begin        # No empty intervals

            # Intervals are adjacent!
            assert interval.begin == prev_end, \
                   "interval.begin: 0x%X != prev_end: 0x%X" % (interval.begin, prev_end)
            # Interval size > 0! 
            assert interval.end   > interval.begin, \
                   "interval.end: %x <= interval.begin: 0x%X" % (interval.end, interval.begin)
            if ChangeF:
                assert target != prev_target 

            prev_end    = interval.end
            prev_target = target

        # If we reach here, then everything is OK.
        return

    def assert_boundary(self, Begin, End):
        assert len(self) != 0
        assert self[0][0].begin == Begin, \
               "begin: %s != %s" % (self[0][0].begin, Begin)
        assert self[-1][0].end  == End, \
               "end:   %s != %s" % (self[-1][0].end, End)

    def replace_target(self, Original, Replacement):
        for i, info in enumerate(self):
            if info[1] == Original:
                self[i] = (info[0], Replacement)

    def get_string(self, Option="utf8", IntervalF=True):
        assert Option in ("hex", "dec", "utf8")
        def get(X):
            if   X == sys.maxint:   return "+oo"
            elif X == - sys.maxint: return "-oo"
            return "%i" % X
        if len(self) == 0:
            return "   <empty>"
        L = max(len(x[0].get_string(Option)) for x in self)
        txt = []
        for interval, target in self:
            if IntervalF: interval_str = interval.get_string(Option)
            else:         interval_str = "[%s:%s)" % (get(interval.begin), get(interval.end))
            txt.append("   %s%s %s\n" % (interval_str, " " * (L - len(interval_str)), target))
        return "".join(txt)

