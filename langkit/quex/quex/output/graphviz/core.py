from quex.engine.misc.file_operations                   import write_safely_and_close
from quex.engine.state_machine.engine_state_machine_set import EngineStateMachineSet
from quex.engine.misc.tools                             import typed
from quex.input.regular_expression.construct            import Pattern           

from quex.blackboard                         import setup as Setup

class Generator(EngineStateMachineSet):
    @typed(PatternList=[Pattern])
    def __init__(self, PatternList, StateMachineName):
        self.state_machine_name = StateMachineName
        EngineStateMachineSet.__init__(self, PatternList)

    def do(self, Option="utf8"):
        """Prepare output in the 'dot' language, that graphviz uses."""
        assert Option in ["utf8", "hex"]

        self.__do(self.sm, self.state_machine_name + ".dot", Option)

        if self.pre_context_sm is not None:
            file_name = "%s-pre-context.dot" % self.state_machine_name
            self.pre_context_file_name = file_name
            self.__do(self.pre_context_sm, file_name, Option)

        if len(self.bipd_sm_db) != 0:
            self.backward_detector_file_name = []
            for sm in self.bipd_sm_db.itervalues():
                file_name = "%s_%i.dot" % (self.state_machine_name, sm.get_id())
                self.backward_detector_file_name.append(file_name)
                self.__do(sm, file_name, Option)

    def __do(self, state_machine, FileName, Option="utf8"):
        dot_code = state_machine.get_graphviz_string(NormalizeF=Setup.normalize_f, Option=Option)
        write_safely_and_close(FileName, dot_code)
    

