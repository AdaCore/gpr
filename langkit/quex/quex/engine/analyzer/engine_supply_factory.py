from   quex.engine.analyzer.door_id_address_label import DoorID
from   quex.engine.operations.operation_list                  import OpList, \
                                                         Op
from   quex.blackboard  import E_InputActions, \
                               setup as Setup

class Base:
    def is_FORWARD(self):                  return False
    def is_BACKWARD_PRE_CONTEXT(self):     return False
    def is_BACKWARD_INPUT_POSITION(self):  return False
    def is_CHARACTER_COUNTER(self):        return False

    def requires_detailed_track_analysis(self):      return False

    def subject_to_reload(self):
        # No engine type is subject to 'reload', if the setup imposes
        # buffer based analysis.
        if Setup.buffer_based_analyzis_f:  return False
        # Ask derived type whether by principal, they require reload.
        return self._principally_subject_to_reload()

    def _principally_subject_to_reload(self): return True
    def requires_position_register_map(self):        return False

    def direction_str(self):               return None

    def input_action(self, InitStateF):    assert False

    def create_DropOut(self, SM_State):                assert False

class Class_FORWARD(Base):
    def __init__(self):
        pass

    def is_FORWARD(self):                  
        return True
    def requires_detailed_track_analysis(self):
        """DropOut and Entry require construction beyond what is accomplished 
           by constructor of 'AnalyzerState'. Storing of positions need to be
           optimized.
        """
        return True
    def requires_position_register_map(self):
        """For acceptance positions may need to be stored. For this a 
        'position register map' is developped which may minimize the 
        number of registers.
        """
        return True

    def input_action(self, InitStateF):
        if InitStateF: return E_InputActions.DEREF
        else:          return E_InputActions.INCREMENT_THEN_DEREF

    def direction_str(self): 
        return "FORWARD"

    def create_DropOut(self, SM_State):                          
        # DropOut and Entry interact and require sophisticated analysis
        # => See "Analyzer.get_drop_out_object(...)"
        return None 

class Class_CHARACTER_COUNTER(Class_FORWARD):
    def is_CHARACTER_COUNTER(self): return True

    def _principally_subject_to_reload(self): 
        """Characters to be counted are only inside the lexeme. The lexeme must 
        be entirely in the buffer. Thus, no reload is involved.
        """
        return False

    def create_DropOut(self, SM_State):                          
        return None

class Class_BACKWARD_PRE_CONTEXT(Base):
    def is_BACKWARD_PRE_CONTEXT(self):     
        return True

    def direction_str(self): 
        return "BACKWARD"

    def input_action(self, InitStateF):
        return E_InputActions.DECREMENT_THEN_DEREF

    def create_DropOut(self, SM_State):                        
        return OpList(Op.GotoDoorId(DoorID.global_end_of_pre_context_check()))

class Class_BACKWARD_INPUT_POSITION(Base):
    def __init__(self, IncidenceIdOnBehalfOfWhichBipdOperates):
        self.__incidence_id_of_bipd = IncidenceIdOnBehalfOfWhichBipdOperates

    def is_BACKWARD_INPUT_POSITION(self):  
        return True

    def incidence_id_of_bipd(self):
        return self.__incidence_id_of_bipd

    def _principally_subject_to_reload(self): 
        """When going backwards, this happens only along a lexeme which must
        be entirely in the buffer. Thus, no reload is involved.
        """
        return False

    def direction_str(self): 
        return None

    def input_action(self, InitStateF):
        return E_InputActions.DECREMENT_THEN_DEREF

    def create_DropOut(self, SM_State):                          
        if SM_State.is_acceptance():
            incidence_id = self.__incidence_id_of_bipd
            return OpList(
                Op.QuexDebug('pattern %i: backward input position detected\\n' % incidence_id),
                Op.InputPIncrement(), 
                Op.GotoDoorId(DoorID.bipd_return(incidence_id))
            )
        else:
            return OpList(
                Op.QuexAssertNoPassage()
            )

FORWARD                 = Class_FORWARD()
CHARACTER_COUNTER       = Class_CHARACTER_COUNTER()
BACKWARD_PRE_CONTEXT    = Class_BACKWARD_PRE_CONTEXT()
# NOT: BACKWARD_INPUT_POSITION = Class_BACKWARD_INPUT_POSITION() --> Each one is different
#                                use Class_BACKWARD_INPUT_POSITION(x)
