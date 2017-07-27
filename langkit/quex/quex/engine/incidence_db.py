from   quex.engine.misc.tools import typed
from   quex.engine.analyzer.terminal.factory           import TerminalFactory
from   quex.input.code.base                 import CodeFragment, \
                                                              SourceRef_DEFAULT
from   quex.input.code.core                 import CodeTerminal
from   quex.blackboard import standard_incidence_db, \
                              E_IncidenceIDs, \
                              E_TerminalType


class IncidenceDB(dict):
    """Database of CodeFragments related to 'incidences'.
    ---------------------------------------------------------------------------

                      incidence_id --> [ CodeFragment ]

    If the 'mode_option_info_db[option_name]' mentions that there can be 
    no multiple definitions or if the options can be overwritten than the 
    list of OptionSetting-s must be of length '1' or the list does not exist.

    ---------------------------------------------------------------------------
    """
    terminal_type_db = {
        E_IncidenceIDs.MATCH_FAILURE:   E_TerminalType.MATCH_FAILURE,
        E_IncidenceIDs.END_OF_STREAM:   E_TerminalType.END_OF_STREAM,
        # NOT: E_IncidenceIDs.SKIP_RANGE_OPEN: E_TerminalType.SKIP_RANGE_OPEN
        # Otherwise, it would try to make terminals for that in 'extract_terminal_db()'
    }
    mandatory_list = [
        E_IncidenceIDs.MATCH_FAILURE, 
        E_IncidenceIDs.END_OF_STREAM,
        E_IncidenceIDs.SKIP_RANGE_OPEN,
        E_IncidenceIDs.INDENTATION_BAD,
    ]
    # When webbed into a state machine, certain incidences may not be changed, 
    # because there address is used all over the place.
    unmutable_incidence_ids = set([
        E_IncidenceIDs.INDENTATION_HANDLER
    ])

    @staticmethod
    def from_BaseModeSequence(BaseModeSequence):
        """Collects the content of the 'incidence_db' member of this mode and
        its base modes. 

        RETURNS:      map:    incidence_id --> [ CodeFragment ]
        """
        assert len(BaseModeSequence) > 0

        # Special incidences from 'standard_incidence_db'
        result = IncidenceDB()
        for incidence_name, info in standard_incidence_db.iteritems():
            incidence_id, comment = info
            code = None
            for mode_descr in BaseModeSequence:
                code_fragment = mode_descr.incidence_db.get(incidence_name)
                if   code_fragment is None:         continue
                elif code_fragment.is_whitespace(): continue

                if code is None: code = code_fragment.get_code()
                else:            code.extend(code_fragment.get_code())

            if code is not None:
                result[incidence_id] = CodeFragment(code)

        # Make sure, that all mandatory incidences are implemented!
        mode_name = BaseModeSequence[-1].name
        for incidence_id in IncidenceDB.mandatory_list:
            if incidence_id in result: continue
            result[incidence_id] = IncidenceDB.__default_code_fragment(incidence_id, 
                                                                       mode_name)

        return result

    def __setitem__(self, Key, Value):
        dict.__setitem__(self, Key, Value)

    @staticmethod
    def __default_code_fragment(IncidenceId, ModeName):
        if IncidenceId == E_IncidenceIDs.MATCH_FAILURE:
            txt =   "QUEX_ERROR_EXIT(\"\\n    Match failure in mode '%s'.\\n\"\n" % ModeName \
                  + "                \"    No 'on_failure' section provided for this mode.\\n\"\n" \
                  + "                \"    Proposal: Define 'on_failure' and analyze 'Lexeme'.\\n\");\n"

        elif IncidenceId == E_IncidenceIDs.END_OF_STREAM:
            txt = 'self_send(__QUEX_SETTING_TOKEN_ID_TERMINATION);\n'
                # NOT: "Lng.PURE_RETURN" because the terminal end of stream 
                #      exits anyway immediately--after 'on_after_match'.

        elif IncidenceId == E_IncidenceIDs.SKIP_RANGE_OPEN:
            txt =   'QUEX_ERROR_EXIT("\\nLexical analyzer mode \'%s\':\\n"\n' % ModeName \
                  + '                "End of file occurred before closing skip range delimiter!\\n"\n' \
                  + '                "The \'on_skip_range_open\' handler has not been specified.");'
        elif IncidenceId == E_IncidenceIDs.INDENTATION_BAD:
            txt =   'QUEX_ERROR_EXIT("Mode \'%s\': bad indentation character detected!\\n"' % ModeName \
                  + '                "No \'on_indentation_bad\' handler has been specified.\\n");'
        else:
            assert False

        return CodeFragment(txt, SourceRef_DEFAULT)

    @typed(factory=TerminalFactory)
    def extract_terminal_db(self, factory, ReloadRequiredF):
        """SpecialTerminals: END_OF_STREAM
                             FAILURE
                             CODEC_ERROR
                             ...
        """
        result = {}
        for incidence_id, code_fragment in self.iteritems():
            if incidence_id not in IncidenceDB.terminal_type_db: 
                continue
            elif   incidence_id == E_IncidenceIDs.END_OF_STREAM \
               and not ReloadRequiredF:
                continue
            terminal_type = IncidenceDB.terminal_type_db[incidence_id]
            code_terminal = CodeTerminal(code_fragment.get_code())
            assert terminal_type not in result
            terminal = factory.do(terminal_type, code_terminal)
            terminal.set_incidence_id(incidence_id)
            result[incidence_id] = terminal

        return result

    def get_CodeTerminal(self, IncidenceId):
        if IncidenceId not in self:
            return CodeTerminal([""])
        else:
            return CodeTerminal(self[IncidenceId].get_code(), LexemeRelevanceF=True)

    def get_text(self, IncidenceId):
        code_fragment = self.get(IncidenceId)
        if code_fragment is None: return ""
        else:                     return "".join(code_fragment.get_code())

    def default_indentation_handler_f(self):
        return not (   self.has_key(E_IncidenceIDs.INDENTATION_ERROR) \
                    or self.has_key(E_IncidenceIDs.INDENTATION_INDENT)   \
                    or self.has_key(E_IncidenceIDs.INDENTATION_DEDENT)   \
                    or self.has_key(E_IncidenceIDs.INDENTATION_N_DEDENT) \
                    or self.has_key(E_IncidenceIDs.INDENTATION_NODENT))

