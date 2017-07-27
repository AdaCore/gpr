#! /usr/bin/env python
#
# (C) Frank-Rene Schaefer
# ABSOLUTELY NO WARRANTY
from   quex.DEFINITIONS                   import QUEX_PATH, QUEX_CODEC_DB_PATH
import quex.engine.codec_db.parser        as     parser
import quex.engine.misc.error             as     error
from   quex.engine.misc.file_operations   import get_file_content_or_die, \
                                                 open_file_or_die
from   quex.engine.misc.interval_handling import Interval, NumberSet
from   quex.engine.misc.tools             import typed

import os

_supported_codec_list              = []
_supported_codec_list_plus_aliases = []

def get_supported_codec_list(IncludeAliasesF=False):
    assert type(IncludeAliasesF) == bool

    global _supported_codec_list
    if len(_supported_codec_list) != 0: 
        if IncludeAliasesF: return _supported_codec_list_plus_aliases
        else:               return _supported_codec_list

    file_name = QUEX_PATH + "/quex/engine/codec_db/database/00-SUPPORTED.txt"
    content   = get_file_content_or_die(file_name)

    _supported_codec_list = content.split()
    _supported_codec_list.sort()
    codec_db_list = parser.get_codec_list_db()
    for codec_name, aliases_list, dummy in codec_db_list:
        if codec_name in _supported_codec_list: 
            _supported_codec_list_plus_aliases.extend(filter(lambda x: x != "", aliases_list))
        
    _supported_codec_list_plus_aliases.sort()
    if IncludeAliasesF: return _supported_codec_list_plus_aliases
    else:               return _supported_codec_list

def get_supported_language_list(CodecName=None):
    if CodecName is None:
        result = []
        for record in parser.get_codec_list_db():
            for language in record[2]:
                if language not in result: 
                    result.append(language)
        result.sort()
        return result
    else:
        for record in parser.get_codec_list_db():
            if record[0] == CodecName: return record[2]
        return []

def get_codecs_for_language(Language):
    result = []
    for record in parser.get_codec_list_db():
        codec = record[0]
        if codec not in get_supported_codec_list(): continue
        if Language in record[2]: 
            result.append(record[0])
    if len(result) == 0:
        error.verify_word_in_list(Language, get_supported_language_list(),
                            "No codec found for language '%s'." % Language)
    return result

#______________________________________________________________________________
#
# CodecTransformationInfo(list):
#
# Provides the information about the relation of character codes in a particular 
# coding to unicode character codes. It is provided in the following form:
#
#   # Codec Values                 Unicode Values
#   [ (Source0_Begin, Source0_End, TargetInterval0_Begin), 
#     (Source1_Begin, Source1_End, TargetInterval1_Begin),
#     (Source2_Begin, Source2_End, TargetInterval2_Begin), 
#     ... 
#   ]
#
# .name           = Name of the codec.
# .file_name      = Name of file where the codec was taken from.
# .source_set     = NumberSet of unicode code points which have a representation 
#                   the given codec.
# .drain_set      = NumberSet of available code points in the given codec.
#
# NOTE: If the content of the file was not a valid codec transformation info,
#       then the following holds:
#
#       .source_set = .drain_set = None
#______________________________________________________________________________
class CodecInfo:
    def __init__(self, Name, SourceSet, DrainSet):
        self.name       = Name
        self.source_set = SourceSet
        self.drain_set  = DrainSet

    def variable_character_sizes_f(self):
        return False

    def transform(self, sm):
        """Cut any number that is not in drain_set from the transition trigger
        sets. Possible orphaned states are deleted.
        """
        complete_f         = True
        orphans_possible_f = False
        for state in sm.states.itervalues():
            target_map = state.target_map.get_map()
            for target_index, number_set in target_map.items():
                if self.drain_set.is_superset(number_set): continue
                complete_f = False
                number_set.intersect_with(self.drain_set)
                if number_set.is_empty(): 
                    del target_map[target_index]
                    orphans_possible_f = True

        if orphans_possible_f:
            sm.delete_orphaned_states()

        return complete_f, sm

    def transform_NumberSet(self, number_set):
        return self.drain_set.intersection(number_set)

    def transform_Number(self, number):
        if self.drain_set.contains(number): return [ Interval(number) ]
        return None

    def homogeneous_chunk_n_per_character(self, CharacterSet):
        return 1 # In non-dynamic character codecs each chunk element is a character

    def homogeneous_chunk_n_per_character_in_state_machine(self, SM):
        return 1

class CodecDynamicInfo(CodecInfo):
    def __init__(self, Name, ImplementingModule):
        CodecInfo.__init__(self, 
                    Name,
                    ImplementingModule.get_unicode_range(), 
                    ImplementingModule.get_codec_element_range())
        self.module = ImplementingModule

    def variable_character_sizes_f(self):
        return True

    def transform(self, sm):
        sm = self.module.do(sm)
        return True, sm

    @typed(number_set=NumberSet)
    def transform_NumberSet(self, number_set):
        result = self.module.do_set(number_set)
        assert result is not None, \
               "Operation 'number set transformation' failed.\n" + \
               "The given number set results in a state sequence not a single transition."
        return result

    def transform_Number(self, number):
        result = self.transform_NumberSet(NumberSet(number))
        if result is None: return None
        else:              return result.get_intervals(PromiseToTreatWellF=True)

    def homogeneous_chunk_n_per_character(self, CharacterSet):
        """Consider a given state machine (pattern). If all characters involved in the 
        state machine require the same number of chunks (2 bytes) to be represented 
        this number is returned. Otherwise, 'None' is returned.

        RETURNS:   N > 0  number of chunks (2 bytes) required to represent any character 
                          in the given state machine.
                   None   characters in the state machine require different numbers of
                          chunks.
        """
        return self.module.homogeneous_chunk_n_per_character(CharacterSet)

    def homogeneous_chunk_n_per_character_in_state_machine(self, SM):
        chunk_n = None
        for state in SM.states.itervalues():
            for number_set in state.target_map.get_map().itervalues():
                candidate_chunk_n = self.module.homogeneous_chunk_n_per_character(number_set)
                if   candidate_chunk_n is None:    return None
                elif chunk_n is None:              chunk_n = candidate_chunk_n
                elif chunk_n != candidate_chunk_n: return None
        return chunk_n

class CodecTransformationInfo(CodecInfo, list):
    def __init__(self, Codec=None, FileName=None, ExitOnErrorF=True):
        assert Codec is not None or FileName is not None

        if FileName is not None:
            file_name  = os.path.basename(FileName)
            file_name, dumped_ext = os.path.splitext(file_name)
            codec_name = file_name.replace(" ", "_").replace("\t", "_").replace("\n", "_")
            file_name  = FileName
        else:
            codec_name = _get_distinct_codec_name_for_alias(Codec)
            file_name  = QUEX_CODEC_DB_PATH + "/%s.dat" % codec_name

        source_set, drain_set = self.__load(file_name, ExitOnErrorF)
        CodecInfo.__init__(self, codec_name, source_set, drain_set)

    def transform(self, sm):
        """RETURNS: True  transformation for all states happend completely.
                    False transformation may not have transformed all elements because
                          the target codec does not cover them.
        """
        complete_f         = True
        orphans_possible_f = False
        for state in sm.states.itervalues():
            L = len(state.target_map.get_map())
            if not state.target_map.transform(self):
                complete_f = False
                if L != len(state.target_map.get_map()):
                    orphans_possible_f = True

        # If some targets have been deleted from target maps, then a orphan state 
        # deletion operation is necessary.
        if orphans_possible_f:
            sm.delete_orphaned_states()

        return complete_f, sm

    def transform_NumberSet(self, number_set):
        return number_set.transform(self)

    def transform_Number(self, number):
        result = self.transform_NumberSet(NumberSet(number)).get_intervals()
        if result is None: return None
        else:              return result.get_intervals(PromiseToTreatWellF=True)

    def __load(self, FileName, ExitOnErrorF):
        fh = open_file_or_die(FileName, "rb")
        source_set, drain_set, error_str = parser.do(self, fh)

        if error_str is not None:
            error.log(error_str, fh, DontExitF=not ExitOnErrorF)
            self.__set_invalid() # Transformation is not valid.

        return source_set, drain_set

    def __set_invalid(self):
        list.clear(self)                  
        self.source_set = None
        self.drain_set  = None

def _get_distinct_codec_name_for_alias(CodecAlias, FH=-1):
    """Arguments FH and LineN correspond to the arguments of error.log."""
    assert len(CodecAlias) != 0

    for record in parser.get_codec_list_db():
        if CodecAlias in record[1] or CodecAlias == record[0]: 
            return record[0]

    error.verify_word_in_list(CodecAlias, get_supported_codec_list(), 
                        "Character encoding '%s' unknown to current version of quex." % CodecAlias,
                        FH)

def get_supported_unicode_character_set(CodecAlias=None, FileName=None):
    """RETURNS:

       NumberSet of unicode characters which are represented in codec.
       None, if an error occurred.

       NOTE: '.source_set' is None in case an error occurred while constructing
             the CodecTransformationInfo.
    """
    return CodecTransformationInfo(CodecAlias, FileName, ExitOnErrorF=False).source_set

