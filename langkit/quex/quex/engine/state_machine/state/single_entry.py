from quex.engine.misc.tools               import typed
from quex.engine.operations.se_operations import SeOp, \
                                                 SeAccept, \
                                                 SeStoreInputPosition
from quex.blackboard import E_PreContextIDs

class SingleEntry(object):
    __slots__ = ('__list')

    def __init__(self, CloneF=False):
        if not CloneF: self.__list = []

    @staticmethod
    def from_iterable(Iterable):
        result = SingleEntry()
        result.set(Iterable)
        return result

    def clone(self, ReplDbPreContext=None, ReplDbAcceptance=None):
        return SingleEntry.from_iterable(
            x.clone(ReplDbPreContext=ReplDbPreContext,
                    ReplDbAcceptance=ReplDbAcceptance) 
            for x in self.__list)

    def find(self, OpClass):
        for cmd in self.__list:
            if cmd.__class__ == OpClass: return cmd
        return None

    def get_iterable(self, OpClass):
        for cmd in self.__list:
            if cmd.__class__ == OpClass: yield cmd
        
    def has(self, Op):
        for candidate in self.__list:
            if candidate == Op: return True
        return False

    @typed(Op=SeOp)
    def add(self, Op):
        if self.has(Op): return
        self.__list.append(Op.clone())

    def add_Op(self, OpClass):
        cmd = self.find(OpClass)
        if cmd is not None: return
        self.add(OpClass())

    def merge(self, Other):
        assert isinstance(Other, SingleEntry)
        self.__list.extend(
            cmd.clone() for cmd in Other.__list if not self.has(cmd)
        )

    def merge_list(self, OpIterableIterable):
        for origin_iterable in OpIterableIterable:
            self.merge(origin_iterable)

    def set(self, OpList):
        self.clear()
        self.__list.extend(
            cmd.clone() for cmd in OpList if not self.has(cmd)
        )
        
    def remove_Op(self, OpClass):
        L = len(self.__list)
        for i in xrange(L-1, -1, -1):
            cmd = self.__list[i]
            if cmd.__class__ == OpClass: del self.__list[i]

    def has_acceptance_id(self, AcceptanceID):
        for cmd in self.get_iterable(SeAccept):
            if cmd.acceptance_id() == AcceptanceID:
                return True
        return False

    def has_begin_of_line_pre_context(self):
        for cmd in self.get_iterable(SeAccept):
            if cmd.pre_context_id() == E_PreContextIDs.BEGIN_OF_LINE:
                return True
        return False

    def clear(self):
        del self.__list[:]

    def delete_dominated(self):
        """Simplification to make Hopcroft Minimization more efficient. The first unconditional
        acceptance makes any lower prioritized acceptances meaningless. 

        This function is to be seen in analogy with the function 'get_acceptance_detector'. 
        Except for the fact that it requires the 'end of core pattern' markers of post
        conditioned patterns. If the markers are not set, the store input position commands
        are not called properly, and when restoring the input position bad bad things happen 
        ... i.e. segmentation faults.
        """
        # NOTE: Acceptance origins sort before non-acceptance origins
        min_acceptance_id = None
        for cmd in self.get_iterable(SeAccept):
            if min_acceptance_id is None or min_acceptance_id > cmd.acceptance_id():
                min_acceptance_id = cmd.acceptance_id()

        # Delete any SeAccept command where '.acceptance_id() > min_acceptance_id'
        for i in xrange(len(self.__list)-1, -1, -1):
            cmd = self.__list[i]
            if cmd.__class__ == SeAccept and cmd.acceptance_id() > min_acceptance_id:
                del self.__list[i]

    def hopcroft_combinability_key(self):
        """Two states have the same hopcroft-combinability key, if and only if
        they are combinable during the initial state split in the hopcroft
        minimization. Criteria:
        
        (1) Acceptance states of a different acceptance schemes. The
            decision making about the winning pattern must be the same for all
            states of a state set that is possibly combined into one single
            state. 
        
            In particular, non-acceptance states can never be combined with
            acceptance states.
        
        (2) Two states of the same pattern where one stores the input position
            and the other not, cannot be combined. Otherwise, the input
            position would be stored in unexpected situations.

        The approach is the following: For each investigated behavior a a tuple
        of numbers can be derived that describes it uniquely. The tuple of all
        tuples is used during the hopcroft minimization to distinguish between
        combinable states and those that are not.
        """
        # Before the track analysis, the acceptance in a state is simple
        # given by its precedence, i.e. its acceptance id. Thus, the sorted
        # sequence of acceptance ids identifies the acceptance behavior.
        acceptance_info = tuple(sorted(x.acceptance_id() 
                                       for x in self.get_iterable(SeAccept)))

        # The storing of input positions in registers is independent of its
        # position in the command list (as long as it all happens before the increment
        # of the input pointer, of course).
        #
        # The sorted list of position storage registers where positions are stored
        # is a distinct description of the position storing behavior.
        store_info = tuple(sorted(x.acceptance_id() 
                                  for x in self.get_iterable(SeStoreInputPosition)))

        result = (acceptance_info, store_info)
        return result

    def get_string(self, OpalStatesF=True):
        if   not self.__list:       return "\n"
        elif len(self.__list) == 1: return "%s\n" % self.__list[0]

        def key(X):
            if   X.__class__ == SeAccept:              
                return (0, X.acceptance_id(), X.pre_context_id(), X.restore_position_register_f())
            elif X.__class__ == SeStoreInputPosition: 
                return (1, X.acceptance_id())
            else:
                assert False

        return "%s\n" % reduce(lambda x, y: "%s, %s" % (x, y), sorted(self.__list, key=key))

    def __iter__(self):
        for x in self.__list:
            yield x

    def __len__(self):
        return len(self.__list)

