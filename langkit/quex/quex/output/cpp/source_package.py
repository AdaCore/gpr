from quex.engine.misc.file_operations import open_file_or_die, \
                                             write_safely_and_close 
from quex.blackboard          import setup as Setup, \
                                     Lng

import os.path as path
import os
from   quex.DEFINITIONS  import QUEX_PATH

# Search for related files by:
dummy = """
find quex/code_base \
     -path "*.svn*"        -or -path "*TEST*" -or -name tags      \
     -or -name "TXT*"      -or -name "*.txt"  -or -name "*.sw?"   \
     -or -path "*DESIGN*"  -or -name "*.7z"   -or -name "*ignore" \
     -or -name "*DELETED*" -or -name .        -or -name "*_body"  \
     -or -name "[1-9]"     -or -name "circle" -or -name "*.o"     \
     -or -name "*.exe"     -prune  \
     -or -type f -print | sort
"""

base = """
/asserts
/aux-string
/aux-string.i
/definitions
/include-guard-undef
/bom
/bom.i
/MemoryManager
/MemoryManager.i
/single.i
/multi.i
/temporary_macros_off
/temporary_macros_on
"""

base_compatibility = """
/compatibility/iconv-argument-types.h
/compatibility/stdint.h
/compatibility/stdbool-pseudo.h
/compatibility/stdbool.h
/compatibility/win/borland_stdint.h
/compatibility/win/msc_stdint.h
/compatibility/win/msc_stdint.h
"""

base_buffer = """
/buffer/asserts
/buffer/Buffer
/buffer/Buffer_debug
/buffer/Buffer_debug.i
/buffer/Buffer.i
/buffer/MemoryPositionMimiker
"""

base_analyzer = """
/analyzer/C-adaptions.h
/analyzer/Mode
/analyzer/Mode.i
/analyzer/asserts
/analyzer/asserts.i
/analyzer/configuration/derived
/analyzer/configuration/undefine
/analyzer/configuration/validation
/analyzer/headers
/analyzer/headers.i
/analyzer/EngineMemento_body
/analyzer/Engine_body
/analyzer/member/basic
/analyzer/member/basic.i
/analyzer/member/buffer-access
/analyzer/member/buffer-access.i
/analyzer/member/constructor
/analyzer/member/constructor.i
/analyzer/member/misc
/analyzer/member/misc.i
/analyzer/member/mode-handling
/analyzer/member/mode-handling.i
/analyzer/member/navigation
/analyzer/member/navigation.i
/analyzer/member/on_indentation.i
/analyzer/member/token-receiving
/analyzer/member/token-receiving.i
/analyzer/member/token-sending
/analyzer/member/token-sending-undef.i
"""

analyzer_accumulator = """
/analyzer/Accumulator
/analyzer/Accumulator.i
"""

analyzer_counter = """
/analyzer/Counter
/analyzer/Counter.i
"""

analyzer_post_categorizer = """
/analyzer/PostCategorizer
/analyzer/PostCategorizer.i
"""

analyzer_include_stack = """
/analyzer/member/include-stack
/analyzer/member/include-stack.i
"""

token_policy = "/token/TokenPolicy"

token_queue = """
/token/TokenQueue
/token/TokenQueue.i
"""

token_default_C = "/token/CDefault.qx"
token_default_Cpp = "/token/CppDefault.qx"

buffer_filler = """
/buffer/BufferFiller
/buffer/BufferFiller.i
/buffer/InputPolicy
"""

buffer_filler_plain = """
/buffer/plain/BufferFiller_Plain
/buffer/plain/BufferFiller_Plain.i
"""

buffer_filler_converter = """
/buffer/converter/BufferFiller_Converter
/buffer/converter/BufferFiller_Converter.i
/buffer/converter/Converter
"""

buffer_filler_iconv = """
/buffer/converter/iconv/Converter_IConv
/buffer/converter/iconv/Converter_IConv.i
/buffer/converter/iconv/special_headers.h
"""

buffer_filler_icu = """
/buffer/converter/icu/Converter_ICU
/buffer/converter/icu/Converter_ICU.i
/buffer/converter/icu/special_headers.h
"""

converter_helper = """
/converter_helper/common.h
/converter_helper/identity
/converter_helper/identity.i
/converter_helper/generator/declarations.g
/converter_helper/generator/implementations.gi
/converter_helper/generator/string-converter.gi
/converter_helper/generator/character-converter-to-char-wchar_t.gi
"""

converter_helper_unicode = """
/converter_helper/from-unicode-buffer
/converter_helper/from-unicode-buffer.i
"""

converter_helper_utf8 = """
/converter_helper/from-utf8
/converter_helper/from-utf8.i
"""
converter_helper_utf16 = """
/converter_helper/from-utf16
/converter_helper/from-utf16.i
"""
converter_helper_utf32 = """
/converter_helper/from-utf32
/converter_helper/from-utf32.i
"""

def do():
    # Analyzer base file list (required by any analyzer)
    txt =   base                   \
          + base_compatibility     \
          + base_buffer            \
          + base_analyzer          \
          + token_policy           

    # Buffer Filler ___________________________________________________________
    # 
    # The instance that is responsible for filling a buffer with content
    BufferFillerF = True # Change this once we have 'buffer only' modes

    if BufferFillerF:
        txt += buffer_filler
        if Setup.converter_f:
            txt += buffer_filler_converter
            if   Setup.converter_icu_f:                   txt += buffer_filler_icu
            elif Setup.converter_iconv_f:                 txt += buffer_filler_iconv
            elif len(Setup.converter_user_new_func) != 0: pass
            else:                                         assert False
        else:
            txt += buffer_filler_plain

    # if Setup.converter_helper_required_f:
    txt +=   converter_helper       \
           + converter_helper_utf8  \
           + converter_helper_utf16 \
           + converter_helper_utf32 

    # if Setup.buffer_codec.name == "unicode": 
    txt += converter_helper_unicode

    if Setup.token_policy == "queue":
        txt += token_queue

    if Setup.token_class_file != "":
        if   Setup.language == "C":   txt += token_default_C
        elif Setup.language == "C++": txt += token_default_Cpp


    if Setup.string_accumulator_f:                               txt += analyzer_accumulator
    if Setup.count_column_number_f or Setup.count_line_number_f: txt += analyzer_counter 
    if Setup.post_categorizer_f:                                 txt += analyzer_post_categorizer 
    if Setup.include_stack_support_f:                            txt += analyzer_include_stack

    __copy_files(txt)

def __copy_files(FileTxt):

    input_directory  = QUEX_PATH               
    output_directory = Setup.output_directory 

    file_list = map(lambda x: Lng["$code_base"] + x.strip(), FileTxt.split())

    # Ensure that all directories exist
    directory_list = []
    for file in file_list:
        directory = path.dirname(output_directory + file)
        if directory in directory_list: continue
        directory_list.append(directory)

    # Sort directories according to length --> create parent directories before child
    for directory in sorted(directory_list, key=len):
        if os.access(directory, os.F_OK) == True: continue
        # Create also parent directories, if required
        os.makedirs(directory)

    for file in file_list:
        input_file  = input_directory + file
        output_file = output_directory + file
        # Copy
        content     = open_file_or_die(input_file, "rb").read()
        write_safely_and_close(output_file, content)

