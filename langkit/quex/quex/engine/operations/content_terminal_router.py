from  quex.engine.operations.content_accepter import repr_acceptance_id
from  quex.blackboard import E_TransitionN, \
                             E_IncidenceIDs, \
                             E_PostContextIDs
                             
from copy import deepcopy

class RouterContentElement(object):
    """Objects of this class shall be elements to build a router to the terminal
       based on the setting 'last_acceptance', i.e.

            switch( last_acceptance ) {
                case  45: input_p -= 3;                   goto TERMINAL_45;
                case  43:                                 goto TERMINAL_43;
                case  41: input_p -= 2;                   goto TERMINAL_41;
                case  33: input_p = lexeme_start_p - 1;   goto TERMINAL_33;
                case  22: input_p = position_register[2]; goto TERMINAL_22;
            }

       That means, the 'router' actually only tells how the positioning has to happen
       dependent on the acceptance. Then it goes to the action of the matching pattern.
       Following elements are provided:

        .acceptance_id    Terminal to be targeted (what was accepted).

                         == -1   --> goto terminal 'failure', nothing matched.
                         >= 0    --> goto terminal given by '.terminal_id'

        .positioning     Adaption of the input pointer, before the terminal is entered.

                         >= 0    
                                   input_p -= .positioning 

                            This is only possible if the number of transitions
                            since acceptance is determined before run time.

                         == E_TransitionN.VOID 
                         
                                   input_p = position[.position_register]

                            Restore a stored input position from its register.
                            A loop appeared on the path from 'store input
                            position' to here.

                         == E_TransitionN.LEXEME_START_PLUS_ONE 
                         
                                   input_p = lexeme_start_p + 1

                            Case of failure (actually redundant information).
    """
    __slots__ = ("acceptance_id", "positioning", "position_register")

    def __init__(self, AcceptanceID, TransitionNSincePositioning):
        assert    TransitionNSincePositioning == E_TransitionN.VOID \
               or TransitionNSincePositioning == E_TransitionN.LEXEME_START_PLUS_ONE \
               or TransitionNSincePositioning == E_TransitionN.IRRELEVANT \
               or TransitionNSincePositioning >= 0
        assert    AcceptanceID in E_IncidenceIDs \
               or AcceptanceID >= 0

        self.acceptance_id     = AcceptanceID
        self.positioning       = TransitionNSincePositioning
        self.position_register = AcceptanceID                 # May later be adapted.

    def replace(self, PositionRegisterMap):
        """Replace the '.position_register' with the correspondance provided
        in PositionRegisterMap. This is only relevant, if the position register
        is actually used. That is, if the position is not VOID, then no 
        replacement needs to take place.
        """
        if self.positioning != E_TransitionN.VOID: return
        self.position_register = PositionRegisterMap[self.position_register]

    def __hash__(self):
        return       hash(self.acceptance_id) \
               + 2 * hash(self.positioning) \
               + 5 * hash(self.position_register)

    def __eq__(self, Other):
        return     self.acceptance_id     == Other.acceptance_id   \
               and self.positioning       == Other.positioning     \
               and self.position_register == Other.position_register

    def __ne__(self, Other):
        return not (self == Other)

    def get_string(self):
        if self.acceptance_id == E_IncidenceIDs.MATCH_FAILURE: assert self.positioning == E_TransitionN.LEXEME_START_PLUS_ONE
        else:                                                  assert self.positioning != E_TransitionN.LEXEME_START_PLUS_ONE

        if self.positioning != 0:
            return "%s goto %s;" % (repr_positioning(self.positioning, self.position_register), 
                                    repr_acceptance_id(self.acceptance_id))
        else:
            return "goto %s;"    % (repr_acceptance_id(self.acceptance_id))

    def __str__(self):
        return "case %s: %s" % (repr_acceptance_id(self.acceptance_id, PatternStrF=False),
                                self.get_string())
        
class RouterContent:
    """_________________________________________________________________________

    RouterContent: Contains a list of pairs that indicates where to go based
                   on the setting of the acceptance register. 

          [0]  if   last_acceptance == FAILURE: goto TERMINAL_FAILURE;
          [1]  elif last_acceptance == 5:       input_p = Position[5]; goto TERMINAL_5;
          [2]  else:                            goto TERMINAL_Pattern56
    
    Requires: RouterContentElement which stores the elements of the list.

    E_R.AcceptanceRegister: Read.
    E_R.InputP:             Possibly write.
    E_R.ThreadOfControl:    Write (jumps to terminals). 
    ___________________________________________________________________________
    """
    def __init__(self):
        self.__list = []

    def clone(self):
        result = RouterContent()
        result.__list = [ deepcopy(x) for x in self.__list ]
        return result
    
    def add(self, Value, Address):
        self.__list.append(RouterContentElement(Value, Address))

    def replace(self, PositionRegisterMap):
        """Replaces position register indices with the onces given in the map.
        That is, for each pair (key, value) in the 'PositionRegisterMap' replace
        any occurrence of 'position_register=key' with 'position_register=value'.
        """
        for element in self.__list:
            if element.positioning != E_TransitionN.VOID: continue
            element.position_register = PositionRegisterMap[element.position_register]

    # Require '__hash__' and '__eq__' to be element of a set.
    def __hash__(self): 
        xor_sum = 0
        for i, x in enumerate(self.__list):
            xor_sum ^= i * hash(x)
        return xor_sum

    def __eq__(self, Other):
        if   not isinstance(Other, RouterContent):    return False
        elif len(self.__list) != len(Other.__list):   return False

        for x, y in zip(self.__list, Other.__list):
            if x != y: return False

        return True

    def __ne__(self, Other):
        return not (self == Other)

    def __iter__(self):
        for x in self.__list:
            assert isinstance(x, RouterContentElement)
            yield x

    def __str__(self):
        txt = [ "on last_acceptance:\n" ]
        def case_str(AcceptanceId):
            msg = "%s" % AcceptanceId
            if msg == "MATCH_FAILURE": return "Failure"
            else:                      return msg
        txt.extend("case %s: %s\n" % (case_str(x.acceptance_id), x.get_string()) for x in self.__list)
        return "".join(txt)

    def __len__(self):
        return len(self.__list)

def repr_positioning(Positioning, PositionRegisterID):
    if   Positioning == E_TransitionN.VOID: 
        return "pos = %s;" % repr_position_register(PositionRegisterID)
    elif Positioning == E_TransitionN.LEXEME_START_PLUS_ONE: 
        return "pos = lexeme_start_p + 1; "
    elif Positioning > 0:   
        return "pos -= %i; " % Positioning
    elif Positioning == 0:  
        return ""
    else: 
        assert False

def repr_position_register(Register):
    if Register == E_PostContextIDs.NONE: return "position[Acceptance]"
    else:                                 return "position[PostContext_%i] " % Register


