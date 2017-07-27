from   quex.blackboard                          import setup as Setup, \
                                                       Lng
import quex.output.cpp.source_package           as source_package
#
import quex.input.files.core                    as quex_file_parser
from   quex.input.files.mode                    import determine_start_mode, Mode
import quex.input.files.consistency_check       as     consistency_check
#
from   quex.engine.analyzer.door_id_address_label  import dial_db
from   quex.engine.misc.file_operations            import write_safely_and_close
#
import quex.output.cpp.core                     as cpp_generator
import quex.output.cpp.token_id_maker           as token_id_maker
import quex.output.cpp.token_class_maker        as token_class_maker
import quex.output.cpp.analyzer_class           as analyzer_class
import quex.output.cpp.configuration            as configuration 
import quex.output.cpp.mode_classes             as mode_classes
import quex.output.cpp.codec_converter_helper   as codec_converter_helper 
import quex.output.graphviz.core                as grapviz_generator

import quex.blackboard                          as blackboard

from   operator import attrgetter

def do():
    """Generates state machines for all modes. Each mode results into 
       a separate state machine that is stuck into a virtual function
       of a class derived from class 'quex_mode'.
    """
    if Setup.language == "DOT": 
        return do_plot()

    mode_description_db = quex_file_parser.do(Setup.input_mode_files)

    # (*) Generate the token ids
    #     (This needs to happen after the parsing of mode_db, since during that
    #      the token_id_db is developed.)
    if Setup.external_lexeme_null_object != "":
        # Assume external implementation
        token_id_header                        = None
        function_map_id_to_name_implementation = ""
    else:
        token_id_header                        = token_id_maker.do(Setup) 
        function_map_id_to_name_implementation = token_id_maker.do_map_id_to_name_function()

    # (*) [Optional] Make a customized token class
    class_token_header, \
    class_token_implementation = token_class_maker.do(function_map_id_to_name_implementation)

    if Setup.token_class_only_f:
        write_safely_and_close(blackboard.token_type_definition.get_file_name(), 
                                 do_token_class_info() \
                               + class_token_header)
        write_safely_and_close(Setup.output_token_class_file_implementation,
                               class_token_implementation)
        write_safely_and_close(Setup.output_token_id_file, token_id_header)
        Lng.straighten_open_line_pragmas(Setup.output_token_id_file)
        Lng.straighten_open_line_pragmas(Setup.output_token_class_file_implementation)
        Lng.straighten_open_line_pragmas(blackboard.token_type_definition.get_file_name())
        return

    # (*) implement the lexer mode-specific analyser functions
    #     During this process: mode_description_db --> mode_db
    function_analyzers_implementation, \
    mode_db                            = analyzer_functions_get(mode_description_db)

    # (*) Implement the 'quex' core class from a template
    # -- do the coding of the class framework
    configuration_header    = configuration.do(mode_db)
    analyzer_header         = analyzer_class.do(mode_db)
    analyzer_implementation = analyzer_class.do_implementation(mode_db) + "\n"
    mode_implementation     = mode_classes.do(mode_db)

    # (*) [Optional] Generate a converter helper
    codec_converter_helper_header, \
    codec_converter_helper_implementation = codec_converter_helper.do()
    
    # Implementation (Potential Inline Functions)
    if class_token_implementation is not None:
         analyzer_implementation += class_token_implementation + "\n" 

    # Engine (Source Code)
    engine_txt =   Lng.ENGINE_TEXT_EPILOG()               + "\n" \
                 + mode_implementation                    + "\n" \
                 + function_analyzers_implementation      + "\n" \
                 + function_map_id_to_name_implementation + "\n" 

    # (*) Write Files ___________________________________________________________________
    if codec_converter_helper_header is not None:
        write_safely_and_close(Setup.output_buffer_codec_header,   
                               codec_converter_helper_header) 
        write_safely_and_close(Setup.output_buffer_codec_header_i, 
                               codec_converter_helper_implementation) 

    if token_id_header is not None:
        write_safely_and_close(Setup.output_token_id_file, token_id_header)

    write_safely_and_close(Setup.output_configuration_file, configuration_header)

    if Setup.language == "C":
        engine_txt     += analyzer_implementation
    else:
        analyzer_header = analyzer_header.replace("$$ADDITIONAL_HEADER_CONTENT$$", 
                                                  analyzer_implementation)

    write_safely_and_close(Setup.output_header_file, analyzer_header)
    write_safely_and_close(Setup.output_code_file,   engine_txt)

    if class_token_header is not None:
        write_safely_and_close(blackboard.token_type_definition.get_file_name(), 
                               class_token_header)

    Lng.straighten_open_line_pragmas(Setup.output_header_file)
    Lng.straighten_open_line_pragmas(Setup.output_code_file)
    if not blackboard.token_type_definition.manually_written():
        Lng.straighten_open_line_pragmas(blackboard.token_type_definition.get_file_name())

    if Setup.source_package_directory != "":
        source_package.do()

def analyzer_functions_get(ModeDB):
    code = []

    # (*) Get list of modes that are actually implemented
    #     (abstract modes only serve as common base)
    mode_name_list = ModeDB.keys()  

    for name, mode_descr in ModeDB.iteritems():        
        dial_db.clear()

        # -- Generate 'Mode' from 'ModeDescriptions'
        mode = Mode(mode_descr)
        blackboard.mode_db[name] = mode

        if not mode.is_implemented(): continue

        txt_analyzer = cpp_generator.do(mode, mode_name_list)
        txt_counter  = cpp_generator.do_default_counter(mode)

        code.extend(txt_counter)
        code.extend(txt_analyzer)

    code.append(do_comment_pattern_action_pairs(blackboard.mode_db.itervalues()))

    if not Setup.token_class_only_f:
        determine_start_mode(blackboard.mode_db)

    # (*) perform consistency check on newly generated mode_db
    consistency_check.do(blackboard.mode_db)

    # generate frame for analyser code
    return cpp_generator.frame_this("".join(code)), blackboard.mode_db

def do_plot():
    mode_description_db = quex_file_parser.do(Setup.input_mode_files)

    for mode_descr in mode_description_db.itervalues():        
        mode = Mode(mode_descr)
        # -- some modes only define event handlers that are inherited
        if len(mode.pattern_list) == 0: continue

        plotter = grapviz_generator.Generator(mode.pattern_list,
                                              StateMachineName = mode.name)
        plotter.do(Option=Setup.character_display)

def do_token_class_info():
    info_list = [
        "  --token-id-prefix       %s" % Setup.token_id_prefix,
        "  --token-class-file      %s" % Setup.output_token_class_file,
        "  --token-class           %s" % Setup.token_class,
        "  --token-id-type         %s" % Setup.token_id_type,
        "  --buffer-element-type   %s" % Setup.buffer_element_type,
        "  --lexeme-null-object    %s" % Setup.lexeme_null_full_name_cpp,
        "  --foreign-token-id-file %s" % Setup.output_token_id_file,
    ]
    print "info: Analyzers using this token class must be generated with"
    print "info:"
    for line in info_list:
        print "info:    %s" % line
    print "info:"
    print "info: Header: \"%s\"" % blackboard.token_type_definition.get_file_name() 
    print "info: Source: \"%s\"" % Setup.output_token_class_file_implementation

    comment = ["<<<QUEX-OPTIONS>>>\n"]
    for line in info_list:
        if line.find("--token-class-file") != -1: continue
        comment.append("%s\n" % line)
    comment.append("<<<QUEX-OPTIONS>>>")
    return Lng.ML_COMMENT("".join(comment), IndentN=0)

def do_comment_pattern_action_pairs(ModeIterable):
    """Write some comment on the pattern action pairs of all modes.
    """
    if not Setup.comment_mode_patterns_f:
        return ""

    txt = "".join(
        mode.get_documentation()
        for mode in sorted(ModeIterable, key=attrgetter("name"))
    )
    comment = Lng.ML_COMMENT("BEGIN: MODE PATTERNS\n" + \
                             txt                      + \
                             "\nEND: MODE PATTERNS")
    return comment 

def blackboard_mode_db_setup(ModeDescrDb):
    """Takes all ModeDescription-s from ModeDescrDb and generates Mode objects
    out of them. 

    RESULT: blackboard.mode_db containing appropriate Mode objects.
    """
    def enter(Name, ModeDescr):
        mode = Mode(mode_descr)  # -- Generate 'Mode' from 'ModeDescriptions'
        blackboard.mode_db[name] = mode
        return mode

    for name, mode_descr in ModeDescrDb.iteritems():
        enter(name, mode_descr)

    if not Setup.token_class_only_f:
        determine_start_mode(blackboard.mode_db)

    # (*) perform consistency check on newly generated mode_db
    consistency_check.do(blackboard.mode_db)
    return 
