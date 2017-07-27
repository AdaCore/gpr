from  quex.blackboard import E_R, \
                             E_Compression

class RouterOnStateKeyContent:
    """_________________________________________________________________________

    Implements:

        switch( path_iterator - base ) {
            0: goto DoorID(drop out of state '0');
            1: goto DoorID(drop out of state '1');
            2: goto DoorID(drop out of state '2');
            ...
        }
    ___________________________________________________________________________
    """
    def __init__(self):
        self.mega_state_index = -1
        self.__list = []

    def configure(self, CompressionType, MegaStateIndex, IterableStateKeyStateIndexPairs, DoorID_provider):
        self.register = {
            E_Compression.PATH:             E_R.PathIterator,
            E_Compression.PATH_UNIFORM:     E_R.PathIterator,
            E_Compression.TEMPLATE:         E_R.TemplateStateKey,
            E_Compression.TEMPLATE_UNIFORM: E_R.TemplateStateKey,
        }[CompressionType]
        self.mega_state_index = MegaStateIndex
        self.__list = [
            (state_key, DoorID_provider(state_index))
            for state_key, state_index in IterableStateKeyStateIndexPairs
        ]

    def clone(self):
        result = RouterOnStateKeyContent()
        result.__list = [ deepcopy(x) for x in self.__list ]
        return result
    
    # Require '__hash__' and '__eq__' to be element of a set.
    def __hash__(self): 
        xor_sum = 3 * self.mega_state_index
        for i, x in enumerate(self.__list):
            xor_sum ^= i * hash(x)
        return xor_sum

    def __eq__(self, Other):
        if   not isinstance(Other, self.__class__):           return False
        elif self.mega_state_index != Other.mega_state_index: return False
        return self.__list == Other.__list

    def __ne__(self, Other):
        return not (self == Other)

    def __iter__(self):
        for x in self.__list:
            yield x

    def __str__(self):
        txt = [ "on last_acceptance:\n" ]
        txt.extend(str(x) for x in self.__list)
        return "".join(txt)

    def __len__(self):
        return len(self.__list)
