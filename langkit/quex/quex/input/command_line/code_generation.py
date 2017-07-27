import quex.input.command_line.validation   as     validation
from   quex.input.setup                     import global_extension_db,      \
                                                   global_character_type_db, \
                                                   command_line_args_defined, \
                                                   command_line_arg_position, \
                                                   E_Files
from   quex.input.files.token_type          import TokenTypeDescriptorManual
from   quex.input.files.token_id_file       import parse as token_id_file_parse
from   quex.output.core.dictionary import db as output_language_db
import quex.engine.state_machine.transformation.utf8_state_split  as utf8_state_split      
import quex.engine.state_machine.transformation.utf16_state_split as utf16_state_split      
from   quex.engine.misc.file_in             import read_namespaced_name
import quex.engine.misc.error               as     error 


from   quex.blackboard import setup as Setup, E_Compression
import quex.blackboard as blackboard

from   operator import itemgetter
import re
import sys
import os

def prepare(command_line, argv):
    """RETURN:  True, if process needs to be started.
                False, if job is done.
    """
    global Setup

    # (*) Classes and their namespace
    __setup_analyzer_class(Setup)
    __setup_token_class(Setup)
    __setup_token_id_prefix(Setup)
    __setup_lexeme_null(Setup)       # Requires 'token_class_name_space'

    # (*) Output programming language        
    Setup.language = Setup.language.upper()
    error.verify_word_in_list(Setup.language, output_language_db.keys(),
                              "Programming language '%s' is not supported." % Setup.language)
    Setup.language_db  = output_language_db[Setup.language]
    Setup.extension_db = global_extension_db[Setup.language]

    # Is the output file naming scheme provided by the extension database
    # (Validation must happen immediately)
    if Setup.extension_db.has_key(Setup.output_file_naming_scheme) == False:
        error.log("File extension scheme '%s' is not provided for language '%s'.\n" \
                  % (Setup.output_file_naming_scheme, Setup.language) + \
                  "Available schemes are: %s." % repr(Setup.extension_db.keys())[1:-1])

    if Setup.buffer_byte_order == "<system>": 
        Setup.buffer_byte_order                      = sys.byteorder 
        Setup.byte_order_is_that_of_current_system_f = True
    else:
        Setup.byte_order_is_that_of_current_system_f = False

    Setup.buffer_element_specification_prepare()

    if   Setup.buffer_codec_name == "utf8":  module = utf8_state_split
    elif Setup.buffer_codec_name == "utf16": module = utf16_state_split
    else:                                    module = None
    Setup.buffer_codec_prepare(Setup.buffer_codec_name, 
                               Setup.buffer_codec_file, module)

    # AFTER: Setup.buffer_codec_prepare() !!!
    if Setup.language not in ["DOT"]:
        prepare_file_names(Setup)

    type_info = global_character_type_db.get(Setup.buffer_element_type)
    if     type_info is not None and len(type_info) >= 4 \
       and type_info[3] != -1 and Setup.buffer_element_size != -1 \
       and type_info[3] != Setup.buffer_element_size:
        error.log("\nBuffer element type ('--bet' or '--buffer-element-type') was set to '%s'.\n" \
                  % Setup.buffer_element_type \
                  + "It is well known to be of size %s[byte]. However, the buffer element size\n" \
                  % type_info[3] \
                  + "('-b' or '--buffer-element-type') was specified as '%s'.\n\n" \
                  % Setup.buffer_element_size \
                  + "Quex can continue, but the result is questionable.\n", \
                  DontExitF=True)

    Setup.converter_f = False
    if Setup.converter_iconv_f or Setup.converter_icu_f or len(Setup.converter_user_new_func) != 0:
        Setup.converter_f = True

    # The only case where no converter helper is required is where ASCII 
    # (Unicode restricted to [0, FF] is used.
    Setup.converter_helper_required_f = True
    if Setup.converter_f == False and Setup.buffer_element_size == 1 and Setup.buffer_codec.name == "unicode":
        Setup.converter_helper_required_f = False

    validation.do(Setup, command_line, argv)

    if Setup.converter_ucs_coding_name == "": 
        if global_character_type_db.has_key(Setup.buffer_element_type):
            if Setup.buffer_byte_order == "little": index = 1
            else:                                   index = 2
            Setup.converter_ucs_coding_name = global_character_type_db[Setup.buffer_element_type][index]

    if len(Setup.token_id_foreign_definition) != 0: 
        if len(Setup.token_id_foreign_definition) > 3: 
            error.log("Option '--foreign-token-id-file' received > 3 followers.\n"
                      "Found: %s" % str(Setup.token_id_foreign_definition)[1:-1])
        if len(Setup.token_id_foreign_definition) > 1:
            Setup.token_id_foreign_definition_file_region_begin_re = \
                    __compile_regular_expression(Setup.token_id_foreign_definition[1], "token id region begin")
        if len(Setup.token_id_foreign_definition) > 2:
            Setup.token_id_foreign_definition_file_region_end_re = \
                    __compile_regular_expression(Setup.token_id_foreign_definition[2], "token id region end")
        Setup.token_id_foreign_definition_file = \
                Setup.token_id_foreign_definition[0]

        CommentDelimiterList = [["//", "\n"], ["/*", "*/"]]
        token_id_file_parse(Setup.token_id_foreign_definition_file, 
                            CommentDelimiterList)

    # (*) Compression Types
    compression_type_list = []
    for name, ctype in [("compression_template_f",         E_Compression.TEMPLATE),
                        ("compression_template_uniform_f", E_Compression.TEMPLATE_UNIFORM),
                        ("compression_path_f",             E_Compression.PATH),
                        ("compression_path_uniform_f",     E_Compression.PATH_UNIFORM)]:
        if command_line_args_defined(command_line, name):
            compression_type_list.append((command_line_arg_position(name), ctype))

    compression_type_list.sort(key=itemgetter(0))
    Setup.compression_type_list = map(lambda x: x[1], compression_type_list)

    # (*) return Setup ___________________________________________________________________
    return True

def __compile_regular_expression(Str, Name):
    tmp = Str.replace("*", "\\*")
    tmp = tmp.replace("?", "\\?")
    tmp = tmp.replace("{", "\\{")
    tmp = tmp.replace("}", "\\}")
    try:
        return re.compile(tmp)
    except:
        error.log("Invalid %s: %s" % (Name, Str))

def __setup_analyzer_class(Setup):
    """ X0::X1::X2::ClassName --> analyzer_class_name = ClassName
                                  analyzer_name_space = ["X0", "X1", "X2"]
        ::ClassName --> analyzer_class_name = ClassName
                        analyzer_name_space = []
        ClassName --> analyzer_class_name = ClassName
                      analyzer_name_space = ["quex"]
    """
    if Setup.analyzer_class.find("::") == -1:
        Setup.analyzer_class = "quex::%s" % Setup.analyzer_class

    Setup.analyzer_class_name, \
    Setup.analyzer_name_space, \
    Setup.analyzer_name_safe   = \
         read_namespaced_name(Setup.analyzer_class, 
                              "analyzer class (options -o, --analyzer-class)")

    if Setup.show_name_spaces_f:
        print "Analyzer: {"
        print "     class_name:  %s;" % Setup.analyzer_class_name
        print "     name_space:  %s;" % repr(Setup.analyzer_name_space)[1:-1]
        print "     name_prefix: %s;" % Setup.analyzer_name_safe   
        print "}"

    Setup.analyzer_derived_class_name,       \
    Setup.analyzer_derived_class_name_space, \
    Setup.analyzer_derived_class_name_safe = \
         read_namespaced_name(Setup.analyzer_derived_class_name, 
                              "derived analyzer class (options --derived-class, --dc)",
                              AllowEmptyF=True)

def __setup_lexeme_null(Setup):
    if len(Setup.external_lexeme_null_object) != 0:
        lexeme_null_object = Setup.external_lexeme_null_object
        default_name_space = Setup.analyzer_name_space
    elif Setup.token_class_only_f:
        lexeme_null_object = "LexemeNullObject"
        default_name_space = Setup.token_class_name_space
    else:
        lexeme_null_object = "LexemeNullObject"
        default_name_space = Setup.analyzer_name_space

    if lexeme_null_object.find("::") == -1:
        # By default, Setup the token in the analyzer's namespace
        if len(Setup.analyzer_name_space) != 0:
            name_space = reduce(lambda x, y: "%s::%s" % (x, y), default_name_space)
        else:
            name_space = ""
        lexeme_null_object = "%s::%s" % (name_space, lexeme_null_object)

    Setup.lexeme_null_name,        \
    Setup.lexeme_null_namespace,   \
    Setup.lexeme_null_name_safe  = \
         read_namespaced_name(lexeme_null_object, 
                              "lexeme null object (options --lexeme-null-object, --lno)")
    Setup.lexeme_null_full_name_cpp = "::" 
    for name in Setup.lexeme_null_namespace:
        Setup.lexeme_null_full_name_cpp += name + "::"
    Setup.lexeme_null_full_name_cpp += Setup.lexeme_null_name

def __setup_token_class(Setup):
    """ X0::X1::X2::ClassName --> token_class_name = ClassName
                                  token_name_space = ["X0", "X1", "X2"]
        ::ClassName --> token_class_name = ClassName
                        token_name_space = []
        ClassName --> token_class_name = ClassName
                      token_name_space = analyzer_name_space
    """
    if Setup.token_class.find("::") == -1:
        # By default, Setup the token in the analyzer's namespace
        if len(Setup.analyzer_name_space) != 0:
            analyzer_name_space = reduce(lambda x, y: "%s::%s" % (x, y), Setup.analyzer_name_space)
        else:
            analyzer_name_space = ""
        Setup.token_class = "%s::%s" % (analyzer_name_space, Setup.token_class)

    # Token classes and derived classes have the freedom not to open a namespace,
    # thus no check 'if namespace == empty'.
    Setup.token_class_name,       \
    Setup.token_class_name_space, \
    Setup.token_class_name_safe = \
         read_namespaced_name(Setup.token_class, 
                              "token class (options --token-class, --tc)")

    if Setup.show_name_spaces_f:
        print "Token: {"
        print "     class_name:  %s;" % Setup.token_class_name
        print "     name_space:  %s;" % repr(Setup.token_class_name_space)[1:-1]
        print "     name_prefix: %s;" % Setup.token_class_name_safe   
        print "}"

    if Setup.token_class_file != "":
        blackboard.token_type_definition = \
                TokenTypeDescriptorManual(Setup.token_class_file,
                                          Setup.token_class_name,
                                          Setup.token_class_name_space,
                                          Setup.token_class_name_safe,
                                          Setup.token_id_type)

    #if len(Setup.token_class_name_space) == 0:
    #    Setup.token_class_name_space = deepcopy(Setup.analyzer_name_space)

def __setup_token_id_prefix(Setup):
    Setup.token_id_prefix_plain,      \
    Setup.token_id_prefix_name_space, \
    dummy                           = \
         read_namespaced_name(Setup.token_id_prefix, 
                              "token prefix (options --token-id-prefix)", 
                              AllowEmptyF=True)

    if len(Setup.token_id_prefix_name_space) != 0 and Setup.language.upper() == "C":
         error.log("Token id prefix cannot contain a namespaces if '--language' is set to 'C'.")

def prepare_file_names(Setup):
    # BEFORE file names can be prepared, determine the output directory!
    #
    # If 'source packaging' is enabled and no output directory is specified
    # then take the directory of the source packaging.
    if Setup.source_package_directory and not Setup.output_directory:
        Setup.output_directory = Setup.source_package_directory

    #__________________________________________________________________________
    if Setup.language in ["DOT"]:
        return

    Setup.output_file_stem = ""
    if Setup.analyzer_name_space != ["quex"]:
        for name in Setup.analyzer_name_space:
            Setup.output_file_stem += name + "_"
    Setup.output_file_stem += Setup.analyzer_class_name

    Setup.output_code_file          = __prepare_file_name("",               E_Files.SOURCE) 
    Setup.output_header_file        = __prepare_file_name("",               E_Files.HEADER)
    Setup.output_configuration_file = __prepare_file_name("-configuration", E_Files.HEADER)
    Setup.output_token_id_file      = __prepare_file_name("-token_ids",     E_Files.HEADER)
    Setup.output_token_class_file   = __prepare_file_name("-token",         E_Files.HEADER)
    if Setup.token_class_only_f == False:
        Setup.output_token_class_file_implementation = __prepare_file_name("-token",     E_Files.HEADER_IMPLEMTATION)
    else:
        Setup.output_token_class_file_implementation = __prepare_file_name("-token",     E_Files.SOURCE)

    if   Setup.buffer_codec.name == "utf8":
        Setup.output_buffer_codec_header   = "quex/code_base/converter_helper/from-utf8"
        Setup.output_buffer_codec_header_i = "quex/code_base/converter_helper/from-utf8.i"

    elif Setup.buffer_codec.name == "utf16":
        Setup.output_buffer_codec_header   = "quex/code_base/converter_helper/from-utf16"
        Setup.output_buffer_codec_header_i = "quex/code_base/converter_helper/from-utf16.i"

    elif Setup.buffer_codec.name == "utf32":
        Setup.output_buffer_codec_header   = "quex/code_base/converter_helper/from-utf32"
        Setup.output_buffer_codec_header_i = "quex/code_base/converter_helper/from-utf32.i"

    elif Setup.buffer_codec.name != "unicode":
        # Note, that the name may be set to 'None' if the conversion is utf8 or utf16
        # See Internal engine character encoding'
        Setup.output_buffer_codec_header = \
            __prepare_file_name("-converter-%s" % Setup.buffer_codec.name, E_Files.HEADER)
        Setup.output_buffer_codec_header_i = \
            __prepare_file_name("-converter-%s" % Setup.buffer_codec.name, E_Files.HEADER_IMPLEMTATION)
    else:
        Setup.output_buffer_codec_header   = "quex/code_base/converter_helper/from-unicode-buffer"
        Setup.output_buffer_codec_header_i = "quex/code_base/converter_helper/from-unicode-buffer.i"

def __prepare_file_name(Suffix, ContentType):
    global Setup
    assert ContentType in E_Files

    # Language + Extenstion Scheme + ContentType --> name of extension
    ext = Setup.extension_db[Setup.output_file_naming_scheme][ContentType]

    file_name = Setup.output_file_stem + Suffix + ext

    if not Setup.output_directory: return file_name
    else:                          return os.path.normpath(Setup.output_directory + "/" + file_name)

