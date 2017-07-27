from   quex.engine.misc.interval_handling import NumberSet, Interval
from   quex.blackboard               import E_Border, setup as Setup
from   operator import attrgetter

class TargetMap:
    """Members:

       __db:      map: target index --> NumberSet that triggers to target

       __epsilon_target_index_list: list of target states that are entered via epsilon 
                                    transition.
    """
    def __init__(self, DB=None, ETIL=None):
        if DB is None: self.__db = {}   
        else:          self.__db = DB
        if ETIL is None: self.__epsilon_target_index_list = [] 
        else:            self.__epsilon_target_index_list = ETIL 
        ## OPTIMIZATION OPTION: Store the trigger map in a 'cache' variable. This, however,
        ## requires that all possible changes to the database need to annulate the cache value.
        ## self.__DEBUG_trigger_map = None

    def clone(self, ReplDbStateIndex=None):
        if ReplDbStateIndex is None:
            db   = dict((tsi, trigger_set.clone()) 
                        for tsi, trigger_set in self.__db.iteritems())
            etil = list(self.__epsilon_target_index_list)
        else:
            db   = dict((ReplDbStateIndex[tsi], trigger_set.clone()) 
                        for tsi, trigger_set in self.__db.iteritems())
            etil = list(ReplDbStateIndex[tsi] for tsi in self.__epsilon_target_index_list)
        return TargetMap(DB=db, ETIL=etil) 

    def clear(self, TriggerMap=None):
        if TriggerMap is not None:
            assert isinstance(TriggerMap, dict)
            self.__db = TriggerMap
        else:
            # Do not set default value 'TriggerMap={}' since this creates the same
            # default object for all calls of this function.
            self.__db = {}

        self.__epsilon_target_index_list = [] # array.array("l", [])

    def is_empty(self):
        return len(self.__db) == 0 and len(self.__epsilon_target_index_list) == 0

    def is_DFA_compliant(self):
        """Checks if the current state transitions are DFA compliant, i.e. it
           investigates if trigger sets pointing to different targets intersect.
           RETURNS:  True  => OK
                    False => Same triggers point to different target. This cannot
                             be part of a deterministic finite automaton (DFA).
        """
        # DFA's do not have epsilon transitions
        if len(self.__epsilon_target_index_list) != 0: return False

        # check whether trigger sets intersect
        all_trigger_sets = NumberSet()
        for trigger_set in self.__db.itervalues():
            if all_trigger_sets.has_intersection(trigger_set): 
                return False
            else:
                all_trigger_sets.unite_with(trigger_set)

        return True

    def add_epsilon_target_state(self, TargetStateIdx):
        if TargetStateIdx not in self.__epsilon_target_index_list:
            self.__epsilon_target_index_list.append(TargetStateIdx)

    def add_transition(self, Trigger, TargetStateIdx): 
        """Adds a transition according to trigger and target index.
           RETURNS: The target state index (may be created newly).
        """
        assert type(TargetStateIdx) == long \
               or TargetStateIdx is None, "%s" % TargetStateIdx.__class__.__name__
        assert Trigger.__class__ in (int, long, list, Interval, NumberSet) or Trigger is None

        if Trigger is None: # This is a shorthand to trigger via the remaining triggers
            Trigger = self.get_trigger_set_union().get_complement(Setup.buffer_codec.source_set)
        elif type(Trigger) == long: Trigger = Interval(int(Trigger), int(Trigger+1))
        elif type(Trigger) == int:  Trigger = Interval(Trigger, Trigger+1)
        elif type(Trigger) == list: Trigger = NumberSet(Trigger, ArgumentIsYoursF=True)

        if Trigger.__class__ == Interval:  
            if self.__db.has_key(TargetStateIdx): 
                self.__db[TargetStateIdx].add_interval(Trigger)
            else:
                self.__db[TargetStateIdx] = NumberSet(Trigger, ArgumentIsYoursF=True)
        else:
            if self.__db.has_key(TargetStateIdx): 
                self.__db[TargetStateIdx].unite_with(Trigger)
            else:
                self.__db[TargetStateIdx] = Trigger

        return TargetStateIdx

    def delete_transitions_to_target(self, TargetIdx):
        if self.__db.has_key(TargetIdx):
            del self.__db[TargetIdx]
        self.delete_epsilon_target_state(TargetIdx)

    def delete_epsilon_target_state(self, TargetStateIdx):
        if TargetStateIdx in self.__epsilon_target_index_list:
            del self.__epsilon_target_index_list[self.__epsilon_target_index_list.index(TargetStateIdx)]

    def delete_transitions_on_empty_trigger_sets(self):
        for target_index, trigger_set in self.__db.items():
            if trigger_set.is_empty(): del self.__db[target_index]

    def get_trigger_set_union(self):
        result = NumberSet()
        for trigger_set in self.__db.itervalues():
            result.unite_with(trigger_set)
        return result

    def get_drop_out_trigger_set_union(self):
        """This function returns the union of all trigger sets that do not
           transit to any target.
        """
        return self.get_trigger_set_union().get_complement(Setup.buffer_codec.source_set)

    def get_epsilon_target_state_index_list(self):
        return self.__epsilon_target_index_list

    def get_non_epsilon_target_state_index_list(self):
        return self.__db.keys()

    def iterable_target_state_indices(self):
        for state_index in self.__db.iterkeys():
            yield state_index
        for state_index in self.__epsilon_target_index_list:
            yield state_index
        yield None

    def get_target_state_index_list(self):
        """Union of target states that can be reached either via epsilon transition
           or 'real' transition via character.
        """
        result = set(self.__db.iterkeys())
        result.update(self.__epsilon_target_index_list)
        return list(result)

    def get_resulting_target_state_index(self, Trigger):
        """This function makes sense for DFA's"""
        for target_index, trigger_set in self.__db.items():
            if trigger_set.contains(Trigger): return target_index
        return None

    def get_resulting_target_state_index_list(self, Trigger):
        result = []
        if Trigger.__class__.__name__ == "NumberSet":
            for target_index, trigger_set in self.__db.items():
                if trigger_set.has_intersection(Trigger) and target_index not in result:
                    result.append(target_index) 

        else:
            for target_index, trigger_set in self.__db.items():
                if trigger_set.contains(Trigger) and target_index not in result:
                    result.append(target_index) 

        if len(self.__epsilon_target_index_list) != 0:
            for target_index in self.__epsilon_target_index_list:
                if target_index not in result:
                    result.append(self.__epsilon_target_index_list)

        return result

    def get_map(self):
        return self.__db

    def get_trigger_set_line_up(self):
        ## WATCH AND SEE THAT WE COULD CACHE HERE AND GAIN A LOT OF SPEED during construction
        ## if self.__dict__.has_key("NONSENSE"): 
        ##    self.NONSENSE += 1
        ##    print "## called", self.NONSENSE
        ## else:
        ##    self.NONSENSE = 1
        """Lines the triggers up on a 'time line'. A target is triggered by
           certain characters that belong to 'set' (trigger sets). Those sets
           are imagined as ranges on a time line. The 'history' is described
           by means of history items. Each item tells whether a range begins
           or ends, and what target state is reached in that range.

           [0, 10] [90, 95] [100, 200] ---> TargetState0
           [20, 89]                    ---> TargetState1
           [96, 99] [201, 205]         ---> TargetState2

           results in a 'history':

           0:  begin of TargetState0
           10: end of   TargetState0
           11: begin of DropOut
           20: begin of TargetState1
           89: end of   TargetState1
           90: begin of TargetState0
           95: end of   TargetState0
           96: begin of TargetState2
           99: end of   TargetState2
           100 ...
           
        """
        # (*) create a 'history', i.e. note down any change on the trigger set combination
        #     (here the alphabet plays the role of a time scale)
                
        history = []
        # NOTE: This function only deals with non-epsilon triggers. Empty
        #       ranges in 'history' are dealt with in '.get_trigger_map()'. 
        for target_idx, trigger_set in self.__db.iteritems():
            interval_list = trigger_set.get_intervals(PromiseToTreatWellF=True)
            for interval in interval_list: 
                # add information about start and end of current interval
                history.append(history_item(interval.begin, E_Border.BEGIN, target_idx))
                history.append(history_item(interval.end, E_Border.END, target_idx))

        # (*) sort history according to position
        history.sort(key=attrgetter("position"))

        return history      

    def get_trigger_set_to_target(self, TargetIdx):
        """Returns all triggers that lead to target 'TargetIdx'. If a trigger 'None' is returned
           it means that the epsilon transition triggers to target state. If the TargetIndex is 
           omitted the set of all triggers, except the epsilon triggers, are returned.
        """
        if self.__db.has_key(TargetIdx): return self.__db[TargetIdx]
        else:                            return NumberSet()

    def replace_target_indices(self, ReplacementDict):
        new_db = {}
        for target_idx, trigger_set in self.__db.iteritems():
            # In case of no entry in the ReplacementDict, then
            # the old target index remains.
            new_idx = ReplacementDict.get(target_idx, target_idx)
            entry = new_db.get(new_idx)
            if entry is not None: entry.unite_with(trigger_set)
            else:                 new_db[new_idx] = trigger_set.clone()

        # By assigning a new_db, the old one is left for garbage collection
        self.__db = new_db

        for i in xrange(len(self.__epsilon_target_index_list)):
            target_idx     = self.__epsilon_target_index_list[i] 
            new_idx = ReplacementDict.get(target_idx)
            if new_idx is None: continue
            self.__epsilon_target_index_list[i] = new_idx

    def transform(self, TrafoInfo):
        """Transforms all related NumberSets from Unicode to a given Target 
           Encoding according to the given TransformationInfo. The 
           TransformationInfo is a list of elements consisting of 

           [ SourceInterval_Begin, SourceInterval_End, TargetInterval_Begin ]

           For example an element '[ 32, 49, 256 ]' means that all characters 
           from 32 to 48 are transformed into 256 to 372. The function assumes
           that the entries are sorted with respect to SourceInterval_Begin.

           RETURNS: True  transformation of all elements successful.
                    False transformation was not possible for all elements.

           NOTE: If 'False' is returned, for safety, it may be necessary to 
                 do a 'delete_orphaned_states()' operation, because states
                 may have been deleted from the map.
        """
        complete_f = True
        for target, number_set in self.__db.items(): # NOT '.iteritems()'
            assert not number_set.is_empty()
            if number_set.transform(TrafoInfo): 
                assert not number_set.is_empty()
                continue
            complete_f = False
            if number_set.is_empty():
                del self.__db[target]

        # All code points which are not available in the drain's range must
        # trigger an 'on_codec_error'.
        return complete_f
            
    def has_one_of_triggers(self, CharacterCodeList):
        assert type(CharacterCodeList) == list
        for code in CharacterCodeList:
            if self.has_trigger(code): return True
        return False

    def has_trigger(self, CharCode):
        assert type(CharCode) == int
        if self.get_resulting_target_state_index(CharCode) is None: return False
        else:                                                       return True

    def has_target(self, TargetState):
        if self.__db.has_key(TargetState):                    return True
        elif TargetState in self.__epsilon_target_index_list: return True
        else:                                                 return False

    def get_string(self, FillStr, StateIndexMap, Option="utf8"):
        # print out transitionts
        sorted_transitions = self.get_map().items()
        sorted_transitions.sort(lambda a, b: cmp(a[1].minimum(), b[1].minimum()))

        msg = ""
        # normal state transitions
        for target_state_index, trigger_set in sorted_transitions:
            if Option == "utf8": trigger_str = trigger_set.get_utf8_string()
            else:                trigger_str = trigger_set.get_string(Option)
            if StateIndexMap is None: target_str = "%05i" % target_state_index
            else:                     target_str = "%05i" % StateIndexMap[target_state_index]
                
            msg += "%s == %s ==> %s\n" % (FillStr, trigger_str, target_str)

        # epsilon transitions
        if len(self.__epsilon_target_index_list) != 0:
            txt_list = map(lambda ti: "%05i" % StateIndexMap[ti], self.__epsilon_target_index_list)
            msg += "%s ==<epsilon>==> " % FillStr 
            for txt in txt_list:
                msg += txt + ", "
            if len(txt_list) != 0: msg = msg[:-2]
        else:
            msg += "%s" % FillStr

        msg += "\n"

        return msg

    def get_graphviz_string(self, OwnStateIdx, StateIndexMap, Option="utf8"):
        assert Option in ["hex", "utf8"]
        sorted_transitions = self.get_map().items()
        sorted_transitions.sort(lambda a, b: cmp(a[1].minimum(), b[1].minimum()))

        msg = ""
        # normal state transitions
        for target_state_index, trigger_set in sorted_transitions:
            if Option == "utf8": trigger_str = trigger_set.get_utf8_string()
            else:                trigger_str = trigger_set.get_string(Option)
            if StateIndexMap is None: target_str  = "%i" % target_state_index
            else:                     target_str  = "%i" % StateIndexMap[target_state_index]
            msg += "%i -> %s [label =\"%s\"];\n" % (OwnStateIdx, target_str, trigger_str.replace("\"", ""))

        # epsilon transitions
        for ti in self.__epsilon_target_index_list:
            if StateIndexMap is None: target_str = "%i" % int(ti) 
            else:                     target_str = "%i" % int(StateIndexMap[ti]) 
            msg += "%i -> %s [label =\"<epsilon>\"];\n" % (OwnStateIdx, target_str)

        # Escape backslashed characters
        return msg.replace("\\", "\\\\")

class history_item(object):
    """To be used by: member function 'get_trigger_set_line_up(self)'
    """
    __slots__ = ('position', 'change', 'target_idx')
    def __init__(self, Position, ChangeF, TargetIdx):
        self.position   = Position
        self.change     = ChangeF
        self.target_idx = TargetIdx 
        
    def __repr__(self):         
        if self.change == E_Border.BEGIN: ChangeStr = "begin"
        else:                             ChangeStr = "end"
        return "%i: %s %s" % (self.position, ChangeStr, self.target_idx)

    def __eq__(self, Other):
        return     self.position   == Other.position \
               and self.change     == Other.change   \
               and self.target_idx == Other.target_idx 
