#from   quex.input.files.parser_data.counter           import ParserDataLineColumn
from   quex.input.code.base                import SourceRef_VOID, \
                                                             SourceRef
from   quex.engine.state_machine.core                 import StateMachine
from   quex.engine.state_machine.character_counter    import CountInfo
import quex.engine.state_machine.construction.setup_post_context   as     setup_post_context
import quex.engine.state_machine.construction.setup_pre_context    as     setup_pre_context
import quex.engine.state_machine.transformation.core  as     transformation
import quex.engine.state_machine.algorithm.beautifier as     beautifier
import quex.engine.state_machine.algebra.reverse      as     reverse
import quex.engine.misc.error                         as     error
#                                                         
from   quex.engine.misc.tools               import typed

class Pattern(object):
    __slots__ = ("__sr", # Source Reference (filename, line_n)
                 "__sm", 
                 "__post_context_f", 
                 "__post_context_sm",
                 "__bipd_sm_to_be_inverted",        "__bipd_sm", 
                 "__pre_context_sm_to_be_inverted", "__pre_context_sm", 
                 "__pre_context_begin_of_line_f", 
                 "__post_context_end_of_line_f", 
                 "__pattern_string",
                 "__count_info", 
                 "__alarm_transformed_f")
    @typed(CoreSM=StateMachine, BeginOfLineF=bool, EndOfLineF=bool, AllowNothingIsNecessaryF=bool, Sr=SourceRef)
    def __init__(self, CoreSM, PreContextSM=None, PostContextSM=None, 
                 BeginOfLineF=False, EndOfLineF=False, Sr=SourceRef_VOID, 
                 PatternString="",
                 AllowNothingIsNecessaryF=False):
        assert PreContextSM is None or isinstance(PreContextSM, StateMachine)
        Pattern.check_initial(CoreSM, 
                              BeginOfLineF, PreContextSM, 
                              EndOfLineF, PostContextSM, 
                              Sr,
                              AllowNothingIsNecessaryF)

        self.__pattern_string = PatternString
        self.__sr             = Sr

        # (*) Setup the whole pattern
        self.__sm                         = CoreSM
        self.__post_context_sm            = PostContextSM
        self.__post_context_end_of_line_f = EndOfLineF
        assert self.__sm is not None

        # -- [optional] post contexts
        self.__post_context_f = (PostContextSM is not None)

        #    Backward input position detection requires an inversion of the 
        #    state machine. This can only be done after the (optional) codec
        #    transformation. Thus, a non-inverted version of the state machine
        #    is maintained until the transformation is done.
        self.__bipd_sm_to_be_inverted = None
        self.__bipd_sm                = None

        # -- [optional] pre contexts
        #
        #    Same as for backward input position detection holds for pre-contexts.
        self.__pre_context_sm_to_be_inverted = PreContextSM
        self.__pre_context_sm                = None

        # All state machines must be DFAs
        if not self.__sm.is_DFA_compliant(): 
            self.__sm  = beautifier.do(self.__sm)

        if         self.__pre_context_sm_to_be_inverted is not None \
           and not self.__pre_context_sm_to_be_inverted.is_DFA_compliant(): 
            self.__pre_context_sm_to_be_inverted = beautifier.do(self.__pre_context_sm_to_be_inverted)

        # Detect the trivial pre-context
        self.__pre_context_begin_of_line_f = BeginOfLineF
        
        # The line/column count information can only be determined when the 
        # line/column count database is present. Thus, it is delayed.
        self.__count_info = None

        # Ensure, that the pattern is never transformed twice
        self.__alarm_transformed_f = False

        self.__validate(Sr)
    
    @staticmethod
    def from_character_set(CharacterSet):
        return Pattern(StateMachine.from_character_set(CharacterSet), 
                       PatternString="<character set>")

    @property
    def sr(self):             return self.__sr

    @typed(Sr=SourceRef)
    def set_source_reference(self, Sr):
        self.__sr = Sr

    def pattern_string(self): return self.__pattern_string

    def set_pattern_string(self, Value): self.__pattern_string = Value

    def incidence_id(self):   return self.__sm.get_id()

    def set_incidence_id(self, Id):
        self.__sm.set_id(Id)

    #@typed(CsLC=ParserDataLineColumn)
    def prepare_count_info(self, CsLC, CodecTrafoInfo):                
        """Perform line/column counting on the core pattern, i.e. the pattern
        which is not concerned with the post context. The counting happens 
        on a UNICODE state machine--not on a possibly transformed codec state
        machine.

        IDEA: It is done in here, so that it may be done once, even for derived 
              modes--whenever the pattern's count needs to be determined. 
        """
        # Make sure that a pattern is NOT transformed before!
        assert self.__alarm_transformed_f == False
        assert self.__count_info is None           

        # If the pre-context is 'trivial begin of line', then the column number
        # starts counting at '1' and the column number may actually be set
        # instead of being added.
        self.__count_info = CountInfo.from_StateMachine(self.__sm, CsLC, 
                                   self.pre_context_trivial_begin_of_line_f, 
                                   CodecTrafoInfo)
        return self.__count_info

    def count_info(self):                          
        """RETURN information for line and column number counting. 

        The information needs to be prepared before the codec-specific transformation
        -- relying on function 'prepare_count_info()'.
        """
        #assert self.__count_info is not None
        return self.__count_info

    @property
    def sm(self):                                  return self.__sm
    @property
    def pre_context_sm_to_be_inverted(self):       return self.__pre_context_sm
    @property
    def pre_context_sm(self):                      return self.__pre_context_sm
    @property
    def bipd_sm(self):                             return self.__bipd_sm
    @property
    def pre_context_trivial_begin_of_line_f(self): 
        if self.__pre_context_sm_to_be_inverted is not None:
            return False
        return self.__pre_context_begin_of_line_f

    def has_pre_context(self): 
        return    self.__pre_context_begin_of_line_f \
               or self.__pre_context_sm_to_be_inverted is not None \
               or self.__pre_context_sm                is not None
    def has_post_context(self):   
        return self.__post_context_f
    def has_pre_or_post_context(self):
        return self.has_pre_context() or self.has_post_context()

    def mount_pre_context_sm(self):
        if    self.__pre_context_sm_to_be_inverted is None \
          and self.__pre_context_begin_of_line_f == False:
            return

        self.__pre_context_sm = setup_pre_context.do(self.__sm, 
                                                     self.__pre_context_sm_to_be_inverted, 
                                                     self.__pre_context_begin_of_line_f)

    def mount_post_context_sm(self):
        self.__sm,     \
        self.__bipd_sm_to_be_inverted = setup_post_context.do(self.__sm, 
                                                              self.__post_context_sm, 
                                                              self.__post_context_end_of_line_f, 
                                                              self.__sr)

        if self.__bipd_sm_to_be_inverted is None: 
            return

        if         self.__bipd_sm_to_be_inverted is not None \
           and not self.__bipd_sm_to_be_inverted.is_DFA_compliant(): 
            self.__bipd_sm_to_be_inverted = beautifier.do(self.__bipd_sm_to_be_inverted)

        self.__bipd_sm = beautifier.do(reverse.do(self.__bipd_sm_to_be_inverted))

    def cut_character_list(self, CharacterList):
        """Characters can only be cut, if transformation is done and 
        pre- and bipd are mounted.
        """
        def my_error(Name, Pattern):
            error.log("Pattern becomes empty after deleting signal character '%s'." % Name,
                      Pattern.sr)

        for character, name in CharacterList:
            for sm in [self.__sm, self.__pre_context_sm, self.__post_context_sm]:
                if sm is None: continue
                sm.delete_transitions_on_number(character)
                sm.clean_up()
                if sm.is_empty(): 
                    my_error(name, self)

    def transform(self, TrafoInfo):
        """Transform state machine if necessary."""
        # Make sure that a pattern is never transformed twice
        assert self.__alarm_transformed_f == False
        self.__alarm_transformed_f = True

        # Transformation MUST be called before any pre-context or bipd
        # is mounted.
        assert self.__pre_context_sm is None
        assert self.__bipd_sm        is None

        # Currently, the incidence_id is equivalent to the state machine id. The
        # transformation may generate a new state machine, but the incidence id
        # must remain the same! This will not be necessary, if state machines 
        # do not carray ids any longer.
        backup_incidence_id = self.incidence_id()
        c0, self.__sm                            = transformation.do_state_machine(self.__sm)
        c1, self.__pre_context_sm_to_be_inverted = transformation.do_state_machine(self.__pre_context_sm_to_be_inverted)
        c2, self.__post_context_sm               = transformation.do_state_machine(self.__post_context_sm)
        self.set_incidence_id(backup_incidence_id)

        # Only if all transformations have been complete, then the transformation
        # can be considered complete.
        return c0 and c1 and c2

    def __validate(self, Sr):
        # (*) It is essential that state machines defined as patterns do not 
        #     have origins.
        if self.__sm.has_origins():
            error.log("Regular expression parsing resulted in state machine with origins.\n" + \
                      "Please, log a defect at the projects website quex.sourceforge.net.\n", Sr)

        # (*) Acceptance states shall not store the input position when they are 'normally'
        #     post-conditioned. Post-conditioning via the backward search is a different 
        #     ball-game.
        acceptance_f = False
        for state in self.__sm.states.values():
            if state.is_acceptance(): 
                acceptance_f = True
            if     state.input_position_store_f() \
               and state.is_acceptance():
                error.log("Pattern with post-context: An irregularity occurred.\n" + \
                          "(end of normal post-contexted core pattern is an acceptance state)\n" 
                          "Please, log a defect at the projects website quex.sourceforge.net.", Sr)

        if acceptance_f == False:
            error.log("Pattern has no acceptance state and can never match.\n" + \
                      "Aborting generation process.", Sr)

        self.check_consistency()

    def __repr__(self):
        return self.get_string(self)

    def get_string(self, NormalizeF=False, Option="utf8"):
        assert Option in ["utf8", "hex"]

        msg = self.__sm.get_string(NormalizeF, Option)
            
        if   self.__pre_context_sm is not None:
            msg += "pre-context = "
            msg += self.__pre_context_sm.get_string(NormalizeF, Option)           

        elif self.__pre_context_sm_to_be_inverted is not None:
            msg += "pre-context to be inverted = "
            msg += self.__pre_context_sm_to_be_inverted.get_string(NormalizeF, Option)           

        if self.__bipd_sm is not None:
            msg += "post-context backward input position detector = "
            msg += self.__bipd_sm.get_string(NormalizeF, Option)           

        elif self.__bipd_sm_to_be_inverted is not None:
            msg += "post-context backward input position detector to be inverted = "
            msg += self.__bipd_sm_to_be_inverted.get_string(NormalizeF, Option)           

        return msg

    def check_consistency(self):
        return     self.sm.is_DFA_compliant() \
               and (self.pre_context_sm is None or self.pre_context_sm.is_DFA_compliant()) \
               and (self.pre_context_sm is None or not self.pre_context_sm.has_orphaned_states()) \
               and (not self.sm.has_orphaned_states())

    @staticmethod
    def check_initial(core_sm, 
                      begin_of_line_f, pre_context, 
                      end_of_line_f,   post_context, 
                      Sr, 
                      AllowNothingIsNecessaryF):

        def check(Name, Sm, Sr):
            if Sm is None: 
                return

            elif Sm.has_orphaned_states(): 
                error.log("Orphaned state(s) detected in %spattern (optimization lack).\n" % Name + \
                          "Please, log a defect at the projects website quex.sourceforge.net.\n"    + \
                          "Orphan state(s) = " + repr(Sm.get_orphaned_state_index_list()), 
                          Sr, DontExitF=True)
            elif Sm.is_empty():
                error.log("Empty %spattern." % Name, Sr)

            elif not AllowNothingIsNecessaryF:
                # 'Nothing is necessary' cannot be accepted. 
                # See the discussion in the module "quex.output.cpp.core".
                Pattern.detect_path_of_nothing_is_necessary(Sm,  Name.strip(),  post_context_f, Sr)

        post_context_f = (post_context is not None)
        for name, sm in [("pre-context ", pre_context), ("", core_sm), ("post-context ", post_context)]:
            check(name, sm, Sr)

    @staticmethod
    def detect_path_of_nothing_is_necessary(sm, Name, PostContextPresentF, fh):
        assert Name in ["", "pre-context", "post-context"]
        if sm is None: 
            return
        elif not sm.get_init_state().is_acceptance(): 
            return
        if len(Name) == 0: name_str = "core pattern"
        else:              name_str = Name

        msg = "The %s contains in a 'nothing is necessary' path in the state machine.\n"   \
              % name_str                                                                     + \
              "This means, that without reading a character the analyzer drops into\n"   + \
              "an acceptance state. "

        msg += { 
            "":
                "The analyzer would then stall.",

            "pre-context":
                "E.g., pattern 'x*/y/' means that zero or more 'x' are a pre-\n"             + \
                "condition for 'y'. If zero appearances of 'x' are enough, then obviously\n" + \
                "there is no pre-context for 'y'! Most likely the author intended 'x+/y/'.",

            "post-context":
                "A post context where nothing is necessary is superfluous.",
        }[Name]

        if Name != "post-context" and PostContextPresentF:
            msg += "\n"                                                          \
                   "Note: A post context does not change anything to that fact." 

        error.log(msg, fh)

