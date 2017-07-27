#! /usr/bin/env python
import quex.engine.misc.error             as     error
import quex.engine.misc.file_in           as     file_in
from   quex.engine.misc.file_operations   import get_propperly_slash_based_file_name
from   quex.engine.misc.enum              import Enum
from   quex.engine.misc.interval_handling import NumberSet
import quex.engine.codec_db.core          as     codec_db
from   quex.DEFINITIONS                   import QUEX_PATH

import os  
import sys

E_Files = Enum("HEADER", 
               "HEADER_IMPLEMTATION", 
               "SOURCE", 
               "_DEBUG_Files")

class QuexSetup:
    def __init__(self, SetupInfo):
        self.init(SetupInfo)
        self.buffer_codec_prepare("unit-test") # Default: be prepared for unit tests
        self.__buffer_element_specification_done_f = False

    def init(self, SetupInfo):
        for key, entry in SetupInfo.items():
            if type(entry) != list:                         
                default_value = entry
            elif entry[1] in SetupParTypes:
                # The following is supposed to break, in case a paramater type
                # appears that is not handled. => detect missing default value setup
                default_value = {
                    SetupParTypes.LIST:            [],
                    SetupParTypes.INT_LIST:        [],
                    SetupParTypes.FLAG:            False,
                    SetupParTypes.NEGATED_FLAG:    True,
                    SetupParTypes.OPTIONAL_STRING: None,  # "" indicates no follower string
                }[entry[1]]
            else:                                           
                default_value = entry[1]

            self.__dict__[key] = default_value

        # Default values, maybe overiden later on.
        self.language_db  = None
        self.extension_db = None
        self.buffer_codec = None
        self.compression_type_list = []

        file_in.specify_setup_object(self)

    def set(self, Name, Type, Value):
        if Type in (SetupParTypes.LIST, SetupParTypes.INT_LIST):
            prev = self.__dict__.get(Name)
            if prev in SetupParTypes: self.__dict__[Name] = Value
            else:                     prev.extend(Value)
        else:
            self.__dict__[Name] = Value

    def buffer_element_specification_prepare(self):
        global global_character_type_db
        if self.buffer_element_size == "wchar_t":
            error.log("Since Quex version 0.53.5, 'wchar_t' can no longer be specified\n"
                      "with option '--buffer-element-size' or '-bes'. Please, specify\n"
                      "'--buffer-element-type wchar_t' or '--bet'.")

        if self.buffer_element_type == "wchar_t":
            self.converter_ucs_coding_name = "WCHAR_T"

        # (*) Determine buffer element type and size (in bytes)
        if self.buffer_element_size == -1:
            if global_character_type_db.has_key(self.buffer_element_type):
                self.buffer_element_size = global_character_type_db[self.buffer_element_type][3]
            elif self.buffer_element_type == "":
                self.buffer_element_size = 1
            else:
                # Buffer element type is not identified in 'global_character_type_db'.
                # => here Quex cannot know its size on its own.
                self.buffer_element_size = -1

        if self.buffer_element_type == "":
            if self.buffer_element_size in [1, 2, 4]:
                self.buffer_element_type = { 
                    1: "uint8_t", 2: "uint16_t", 4: "uint32_t",
                }[self.buffer_element_size]
            elif self.buffer_element_size == -1:
                pass
            else:
                error.log("Buffer element type cannot be determined for size '%i' which\n" \
                          % self.buffer_element_size + 
                          "has been specified by '-b' or '--buffer-element-size'.")

        self.__buffer_element_specification_done_f = True

    def buffer_codec_prepare(self, BufferCodecName, BufferCodecFileName=None, Module=None):
        """Determines: Setup.buffer_codec_name
                       Setup.buffer_codec
        """
        assert    BufferCodecName == "unit-test" \
               or self.__buffer_element_specification_done_f == True

        if   BufferCodecName in ("utf8", "utf16"):
            assert Module is not None
            result = codec_db.CodecDynamicInfo(BufferCodecName, Module)
        elif BufferCodecFileName:
            os.path.splitext(os.path.basename(BufferCodecFileName))
            try: 
               os.path.splitext(os.path.basename(BufferCodecFileName))
            except:
                error.log("cannot interpret string following '--codec-file'")
            result = codec_db.CodecTransformationInfo(FileName=BufferCodecFileName)
        elif BufferCodecName == "unicode":
            # (Still, 'icu' or 'iconv' may provide converted content, but ...) 
            # If the internal buffer is 'unicode', then the pattern's state 
            # machines are not converted. The requirement for the pattern's
            # range is the same as for the 'buffer element chunks'.
            result = codec_db.CodecInfo("unicode", 
                                NumberSet.from_range(0, self.get_character_value_limit()), 
                                NumberSet.from_range(0, self.get_character_value_limit()))
        elif BufferCodecName == "unit-test":
            result = codec_db.CodecInfo("unicode", 
                                NumberSet.from_range(-sys.maxint, sys.maxint),
                                NumberSet.from_range(-sys.maxint, sys.maxint))

        else:
            result = codec_db.CodecTransformationInfo(BufferCodecName)

        self.buffer_codec = result
        #print "#Setup.name", self.buffer_codec.name
        #print "#Setup.buffer_codec.source_set", self.buffer_codec.source_set
        #print "#Setup.buffer_codec.drain_set ", self.buffer_codec.drain_set

    def get_character_value_limit(self):
        """A buffer element is a chunk of memory of the size of the granularity
        of which the input pointer increases. For fixed size codecs, such as
        ASCII or UCS32, the BUFFER ELEMENT VALUE LIMIT is exactly the same as
        the CHARACTER VALUE LIMIT. 

        However, for dynamic sized codecs, such as UTF8 or UTF16, they are
        different. In UTF8, the input pointer increments by one byte on each
        state transition. However, a character may consist out of multiple
        bytes. The buffer element value limit is 256, but the character value
        limit is the whole range.
        
        
        RETURNS: Integer = supremum of possible character range, i.e.
                           one character behind the last possible.

                 sys.maxint, if no such limit exists.
        """
        buffer_element_size = self.buffer_element_size

        if buffer_element_size == -1: return sys.maxint

        try:
            result = 256 ** buffer_element_size
        except:
            error.log("Error while trying to compute 256 to the 'buffer-element-size' (%i bytes)\n"   \
                      % buffer_element_size + \
                      "Adapt \"--buffer-element-size\" or \"--buffer-element-type\",\n"       + \
                      "or specify '--buffer-element-size-irrelevant' to ignore the issue.")

        if result > sys.maxint: return sys.maxint
        else:                   return result

    def set_all_character_set_UNIT_TEST(self, Begin, End):
        self.buffer_codec.source_set = NumberSet.from_range(Begin, End)

    def get_file_reference(self, FileName):
        """When a source package is specified, then it must be given
           with 'relative coordinates' to the source package directory.
           
           if 'SourcePackager':
               $QUEX_PATH/quex/code_base --> source-package-dir/quex/code_base
               .  (current dir)          --> source-package-dir     
        """
        def clean(X):
            return get_propperly_slash_based_file_name(X)

        code_base_directory = self.language_db["$code_base"]
        # The starting backslash must be assumed for many things ...
        assert code_base_directory[0] == "/"

        # If the source packager is active, then everything becomes relative
        # to the new source package directory.
        if not self.source_package_directory: 
            return clean(FileName)

        full_file_name          = clean(os.path.realpath(FileName))
        # Ensure that all directories end with '/'. The 'clean' will omit double slashes.
        full_source_package_dir = clean(os.path.realpath(self.source_package_directory) + "/")
        full_code_base_dir      = clean(os.path.realpath(QUEX_PATH + code_base_directory) + "/")

        ##if FileName.find("CppDefault.qx") != -1:
        ##    print "## filename           = ", FileName
        ##    print "## full filename      = ", full_file_name
        ##    print "## source package dir = ", full_source_package_dir
        ##    print "## full_code_base_dir = ", full_code_base_dir

        idx = full_file_name.find(full_code_base_dir)
        if idx != -1:
            ## print "##Found"
            ## print "## source package directory = ", self.source_package_directory
            ## print "## languages' code_base     = ", self.language_db["$code_base"]
            ## print "## local file               = ", idx, len(full_code_base_dir), \
            ##                                        full_file_name[idx + len(full_code_base_dir):]

            # The starting '/' must be deleted from codebase, since otherwise
            # it will be considered as an absolute path under Unix.
            result = clean(   code_base_directory[1:]
                           + "/" + full_file_name[idx + len(full_code_base_dir):])
            ## print "## result = ", result
            return result

        elif self.source_package_directory and self.output_directory == self.source_package_directory:
            # If we are in the process of 'source packaging' and no explicit output
            # directory is specified, then the base directory is deleted from the FileName.
            idx = full_file_name.find(full_source_package_dir)
            if idx == 0: 
                return clean(full_file_name[len(full_source_package_dir):])

        return clean(FileName)

SetupParTypes = Enum("LIST", "INT_LIST", "FLAG", "NEGATED_FLAG", "STRING", "OPTIONAL_STRING")

SETUP_INFO = {         
    # [Name in Setup]                 [ Flags ]                                [Default / Type]
    "_debug_exception_f":             [["--debug-exception"],                  SetupParTypes.FLAG], 
    "analyzer_class":                 [["-o", "--analyzer-class"],             "Lexer"],    
    "analyzer_derived_class_file":    [["--derived-class-file"],               ""],
    "analyzer_derived_class_name":    [["--derived-class", "--dc"],            ""],
    "buffer_codec_name":                   [["--codec"],                            "unicode"],
    "buffer_codec_file":              [["--codec-file"],                       ""],
    "buffer_limit_code":              [["--buffer-limit"],                     0x0],
    "buffer_element_size":            [["--buffer-element-size", "-b", "--bes"], -1],  # [Bytes]
    "buffer_element_type":            [["--buffer-element-type", "--bet"],     ""],
    "buffer_based_analyzis_f":        [["--buffer-based", "--bb"],             SetupParTypes.FLAG],
    "buffer_byte_order":              [["--endian"],                           "<system>"],
    "comment_state_machine_f":        [["--comment-state-machine"],            SetupParTypes.FLAG],
    "comment_transitions_f":          [["--comment-transitions"],              SetupParTypes.FLAG],
    "comment_mode_patterns_f":        [["--comment-mode-patterns"],            SetupParTypes.FLAG],
    "compression_template_f":         [["--template-compression"],             SetupParTypes.FLAG],
    "compression_template_uniform_f": [["--template-compression-uniform"],     SetupParTypes.FLAG],
    "compression_template_min_gain":  [["--template-compression-min-gain"],    0],
    "compression_path_f":             [["--path-compression"],                 SetupParTypes.FLAG],
    "compression_path_uniform_f":     [["--path-compression-uniform"],         SetupParTypes.FLAG],
    "count_column_number_f":          [["--no-count-lines"],                   SetupParTypes.NEGATED_FLAG],
    "count_line_number_f":            [["--no-count-columns"],                 SetupParTypes.NEGATED_FLAG],
    "character_display":              [["--character-display"],                "utf8"],
    "path_limit_code":                [["--path-termination"],                 0x1],
    "dos_carriage_return_newline_f":  [["--no-DOS"],                           SetupParTypes.NEGATED_FLAG],
    "string_accumulator_f":           [["--no-string-accumulator", "--nsacc"], SetupParTypes.NEGATED_FLAG],
    "converter_iconv_f":              [["--iconv"],                            SetupParTypes.FLAG],
    "converter_icu_f":                [["--icu"],                              SetupParTypes.FLAG],
    "converter_user_new_func":        [["--converter-new", "--cn"],            ""],
    "converter_ucs_coding_name":      [["--converter-ucs-coding-name", "--cucn"], ""],
    "include_stack_support_f":        [["--no-include-stack", "--nois"],       SetupParTypes.NEGATED_FLAG],
    "input_mode_files":               [["-i"],                                 SetupParTypes.LIST],
    "suppressed_notification_list":   [["--suppress", "-s"],                   SetupParTypes.INT_LIST],
    "token_class_file":               [["--token-class-file"],                 ""],
    "token_class":                    [["--token-class", "--tc"],              "Token"],
    "token_class_only_f":             [["--token-class-only", "--tco"],           SetupParTypes.FLAG],
    "token_id_foreign_definition":    [["--foreign-token-id-file"],               SetupParTypes.LIST],  
    "token_id_foreign_definition_file_show_f": [["--foreign-token-id-file-show"], SetupParTypes.FLAG],
    "token_id_counter_offset":        [["--token-id-offset"],                10000],
    "token_id_type":                  [["--token-id-type"],                  "uint32_t"],
    "token_id_prefix":                [["--token-id-prefix"],                "QUEX_TKN_"],
    "token_queue_size":               [["--token-queue-size"],               64],
    "token_queue_safety_border":      [["--token-queue-safety-border"],      16],
    "token_policy":                   [["--token-policy", "--tp"],           "queue"],                
    "token_memory_management_by_user_f": [["--token-memory-management-by-user", "--tmmbu"], SetupParTypes.FLAG],
    "mode_transition_check_f":        [["--no-mode-transition-check"],       SetupParTypes.NEGATED_FLAG],
    "language":                       [["--language", "-l"],                 "C++"],
    "normalize_f":                    [["--normalize"],                      SetupParTypes.FLAG],
    "external_lexeme_null_object":    [["--lexeme-null-object", "--lno"],      ""],
    "output_file_naming_scheme":      [["--file-extension-scheme", "--fes"], ""],
    "post_categorizer_f":             [["--post-categorizer"],               SetupParTypes.FLAG],
    "output_directory":               [["--output-directory", "--odir"],     ""],
    "source_package_directory":       [["--source-package", "--sp"],         ""],
    "show_name_spaces_f":             [["--show-name-spaces", "--sns"],      SetupParTypes.FLAG],
    "single_mode_analyzer_f":         [["--single-mode-analyzer", "--sma"],  SetupParTypes.FLAG],
    "user_application_version_id":    [["--version-id"],                     "0.0.0-pre-release"],
    #
    "warning_on_outrun_f":            [["--warning-on-outrun", "--woo"],   SetupParTypes.FLAG],
    #
    # QUERY MODE:
    #
    "query_version_f":                [["--version", "-v"],               SetupParTypes.FLAG],
    "query_help_f":                   [["--help", "-h"],                  SetupParTypes.FLAG],
    "query_codec":                    [["--codec-info", "--ci"],          ""],
    "query_codec_file":               [["--codec-info-file", "--cif"],    ""], 
    "query_codec_language":           [["--codec-for-language", "--cil"], ""],
    "query_property":                 [["--property", "--pr"],            SetupParTypes.OPTIONAL_STRING],
    "query_set_by_property":          [["--set-by-property", "--sbpr"],   ""], 
    "query_set_by_expression":        [["--set-by-expression", "--sbe"],  ""],
    "query_property_match":           [["--property-match", "--prm"],     ""],
    "query_numeric_f":                [["--numeric", "--num"],            SetupParTypes.FLAG],
    "query_interval_f":               [["--intervals", "--itv"],          SetupParTypes.FLAG],
    "query_unicode_names_f":          [["--names"],                       SetupParTypes.FLAG],
    #
    #__________________________________________________________________________
    # Parameters not set on the command line:
    "byte_order_is_that_of_current_system_f":    True,
    "analyzer_class_name":                       None,
    "analyzer_name_space":                       None,
    "analyzer_name_safe":                        None,
    "analyzer_derived_class_name_space":         None,
    "analyzer_derived_class_name_safe":          None,
    "token_class_name":                          None,
    "token_class_name_space":                    None,
    "token_class_name_safe":                     None,
    "token_id_prefix_name_space":                None,
    "token_id_prefix_plain":                     None,   # i.e. without the namespace specified.
    "token_id_foreign_definition_file":                  "",
    "token_id_foreign_definition_file_region_begin_re":  None,
    "token_id_foreign_definition_file_region_end_re":    None,
    "output_buffer_codec_header_file":           None,
    "output_header_file":                        None,
    "output_configuration_file":                 None,
    "output_code_file":                          None,
    "output_token_id_file":                      None,
    "output_token_class_file_implementation":    None,
    "output_token_class_file":                   None,
    "language_db":                               None,
    "extension_db":                              None,
    "converter_helper_required_f":               True,
    "compression_type_list":                     None,
    #______________________________________________________________________________________________________
    #
    # DEPRECATED
    #______________________________________________________________________________________________________
    "XX_begin_of_stream_code":           [["--begin-of-stream"],                "0x19"],                  
    "XX_buffer_element_size":            [["--bytes-per-ucs-code-point"],       "1"],                  
    "XX_buffer_element_size2":           [["--bytes-per-trigger"],              -1],                  
    "XX_end_of_stream_code":             [["--end-of-stream"],                  "0x1A"],                  
    "XX_flex_engine_f":                  [["--flex-engine"],                    SetupParTypes.FLAG],      
    "XX_input_pattern_file":             [["-p", "--pattern-file"],             ""],                      
    "XX_input_token_id_db":              [["-t", "--token-id-db"],              SetupParTypes.LIST],
    "XX_leave_temporary_files_f":        [["--leave-tmp-files"],                SetupParTypes.FLAG],      
    "XX_plain_memory_f":                 [["--plain-memory"],                   SetupParTypes.FLAG],           
    "XX_std_istream_support_f":          [["--istream-support"],                SetupParTypes.FLAG],           
    "XX_yywrap_is_ok_f":                 [["--yywrap-is-ok"],                   SetupParTypes.FLAG],           
    "XX_input_token_sending_via_queue_f":[["--token-queue"],                    SetupParTypes.FLAG],           
    "XX_string_accumulator_f":           [["--string-accumulator", "--sacc"],   SetupParTypes.FLAG],  
    "XX_disable_token_queue_f":          [["--no-token-queue", "--ntq"],        SetupParTypes.FLAG],       
    "XX_disable_return_token_id_f":      [["--no-return-token-id"],             SetupParTypes.FLAG],  
    "XX_input_lexer_class_friends":      [["--friend-class"],                   SetupParTypes.LIST], 
    "XX_token_class_name":               [["--token-class-name"],               ""],             
    "XX_token_class_stringless_check_f": [["--token-type-no-stringless-check",  "--ttnsc"], SetupParTypes.NEGATED_FLAG], 
    "XX_token_id_counter_offset":        [["--token-offset"],                     "10000"],        
    "XX_token_id_termination":           [["--token-id-termination"],             "0"],            
    "XX_token_id_uninitialized":         [["--token-id-uninitialized"],           "1"],            
    "XX_token_id_indentation_error":     [["--token-id-indentation-error"],       "2"],            
    "XX_output_debug_f":                 [["--debug"],                            SetupParTypes.FLAG],
    "XX_plot_graphic_format":            [["--plot"],                             ""],
    "XX_plot_character_display":         [["--plot-character-display", "--pcd"],  "utf8"],
    "XX_plot_graphic_format_list_f":     [["--plot-format-list"],                 SetupParTypes.FLAG],
    "XX_compression_template_coef":      [["--template-compression-coefficient"], 1.0],
    "XX_token_id_prefix":                [["--token-prefix"],                     "QUEX_TKN_"],
    "XX_message_on_extra_options_f":     [["--no-message-on-extra-options"],      SetupParTypes.NEGATED_FLAG],
    "XX_error_on_dominated_pattern_f":      [["--no-error-on-dominated-pattern",      "--neodp"],   SetupParTypes.NEGATED_FLAG],
    "XX_error_on_special_pattern_same_f":   [["--no-error-on-special-pattern-same",   "--neosps"],  SetupParTypes.NEGATED_FLAG],
    "XX_error_on_special_pattern_outrun_f": [["--no-error-on-special-pattern-outrun", "--neospo"],  SetupParTypes.NEGATED_FLAG],
    "XX_error_on_special_pattern_subset_f": [["--no-error-on-special-pattern-subset", "--neospsu"], SetupParTypes.NEGATED_FLAG],
    "XX_warning_disabled_no_token_queue_f": [["--no-warning-on-no-token-queue"], SetupParTypes.FLAG],
    "XX_state_entry_analysis_complexity_limit": [["--state-entry-analysis-complexity-limit", "--seacl"], 1000],
    "XX_mode_files":                        [["--mode-files"], None],
    "XX_engine":                            [["--engine"], None],
    "XX_token_class_take_text_check_f":  [["--token-type-no-take_text-check",     "--ttnttc"], SetupParTypes.NEGATED_FLAG], 
}

class NotificationDB:
    # IMPORTANT: The notification ids are NOT supposed to changed between
    #            different versions. Otherwise, updates of quex may cause
    #            compatibility issues.
    #
    # Notification Name:                             Notification ID:
    token_id_ignored_files_report                    = 0
    message_on_extra_options                         = 1
    error_on_dominated_pattern                       = 2
    error_on_special_pattern_same                    = 3
    error_on_special_pattern_outrun                  = 4
    error_on_special_pattern_subset                  = 5
    warning_on_no_token_queue                        = 6
    warning_usage_of_undefined_token_id_name         = 7
    warning_repeated_token_not_yet_defined           = 8
    warning_token_id_prefix_appears_in_token_id_name = 9
    warning_codec_error_with_non_unicode             = 10
    warning_counter_setup_without_newline            = 11
    warning_counter_setup_without_else               = 12
    warning_default_newline_0A_impossible            = 13
    warning_default_newline_0D_impossible            = 14
    warning_on_no_token_class_take_text              = 15
    warning_on_no_warning_on_missing_take_text       = 16
    error_ufo_on_command_line_f                      = 17

DEPRECATED = { 
  "XX_input_pattern_file": 
     ("Write a 'pattern { ... }' section inside the mode files instead.\n" + \
      "Syntax of the 'pattern { ... }' section and the previous file syntax\n" + \
      "are backward compatible.", "0.9.x"),        
  "XX_input_token_id_db":
     ("Write a 'token { ... }' section inside the mode files instead.\n" + \
      "Syntax of the 'token { ... }' section and the previous file syntax\n" + \
      "are backward compatible.", "0.9.x"),        
  "XX_yywrap_is_ok_f":
     ("Since the mentioned version, the flex core engine is no longer supported. The\n" + \
      "flag makes only sense for flex core engines.", "0.13.1"),
  "XX_flex_engine_f":
     ("Since the mentioned version, the flex core engine is no longer supported. The\n" + \
      "flag makes only sense for flex core engines.", "0.13.1"),
  "XX_leave_temporary_files_f":
     ("Since the mentioned version, the flex core engine is no longer supported. The\n" + \
      "flag makes only sense for flex core engines.", "0.13.1"),
  "XX_plain_memory_f":                 
     ("Since the mentioned version, quex does no longer need the '--plain-memory' command\n" + \
      "line argument. The engine can be used with plain memory directly. Please, consider\n" + \
      "reading the documentation on this issue.", "0.31.1"),
  "XX_std_istream_support_f":
     ("The lexical analyzer has a flexible interface now, for both C++ istreams and FILE*\n" + \
      "so that rigid setting with this option is superfluous", "0.13.1"),
  "XX_begin_of_stream_code":
     ("Since the mentioned version, there is no need for end of stream and end of stream\n" + \
      "characters anymore. Options '--end-of-stream' and '--begin-of-stream' are no longer\n" + \
      "supported.", "0.25.2"),
  "XX_end_of_stream_code":
     ("Since the mentioned version, there is no need for end of stream and end of stream\n" + \
      "characters anymore. Options '--end-of-stream' and '--begin-of-stream' are no longer\n" + \
      "supported.", "0.25.2"),
  "XX_input_token_sending_via_queue_f":
     ("The token queue was temporarily turned off by default. Since 0.31.5 the token queue is again\n" + \
      "turned on by default, since the lexical analysers can be described much more natural. If you\n" + \
      "want to disable the token queue, please, use '--no-token-queue', or '--ntq'.", "0.31.5"),
  "XX_string_accumulator_f":
     ("The string accumulator was temporarily turned off by default. Since 0.31.5 the it is again\n" + \
      "turned on by default. If you want to disable the token queue, please, use '--no-string-accumulator',\n" + \
      "or '--nsacc'.", "0.31.5"),
  "XX_disable_token_queue_f":
     ("Since version 0.36.5 the flag '--no-token-queue' and '--ntq' have been deprecated.\n" + \
      "Use flag '--token-policy' or '--tp' instead.", "0.36.5"),     
  "XX_disable_return_token_id_f":      
     ("Flag --no-return-token-id is no longer supported. In recent versions of quex\n" + \
      "token-IDs are not passed as return values at all.", "0.37.1"), 
  "XX_input_lexer_class_friends":  
      ("Since version 0.46.3, friend classes are no longer defined on the command line. Please,\n"
       "use the 'body { ... }' section and fill be-'friend'-ing code there.", "0.46.3"),
  "XX_token_class_name":
      ("Command line option '--token-class-name' has been renamed to '--token-class'\n"
       "for uniformity.", "0.46.3"),
  "XX_token_class_stringless_check_f": 
      ("Command line options --token-type-no-stringless-check and --ttnsc are deprecated. Please,\n"
       "use --token-type-no-take_text-check or --ttnttc", 
       "0.48.1"), 
  "XX_buffer_element_size": 
      ("The command line option '--bytes-per-ucs-code-point' has been renamed to\n"
       "'--buffer-element-size'. The old name causes heavy confusion when it was\n"
       "used in combination with dynamic length codecs (option --codec).", "0.49.1"),
  "XX_buffer_element_size2": 
      ("The command line option '--bytes-per-trigger' has been renamed to\n"
       "'--buffer-element-size'. This naming was chose to harmonize with the\n"  
       "new command line option '--buffer-element-type'.", "0.54.1"),
  "XX_token_id_counter_offset":
      ("The command line option '--token-offset' has been replaced by '--token-id-offset'."
       "0.51.1"),
  "XX_token_id_termination":
      ("Option '--token-id-termination' is no longer supported.\n" \
       "Numeric value for token ids are no longer defined on the command line.\n" \
       "Numeric values for token ids can be defined in token sections, e.g.\n" \
       "    token {\n" \
       "       TERMINATION = 4711;\n"
       "    }", "0.51.1"),
  "XX_token_id_uninitialized":         
      ("Option '--token-id-uninitialized' is no longer supported.\n" \
       "Numeric value for token ids are no longer defined on the command line.\n" \
       "Numeric values for token ids can be defined in token sections, e.g.\n" \
       "    token {\n" \
       "       UNINITIALIZED = 4711;\n"
       "    }", "0.51.1"),
  "XX_token_id_indentation_error":     
      ("Option '--token-id-indentation-error' is no longer supported.\n"          \
       "Numeric value for token ids are no longer defined on the command line.\n" \
       "Numeric values for token ids can be defined in token sections, e.g.\n"    \
       "    token {\n"                                                            \
       "       INDENTATION_ERROR = 4711;\n"                                       \
       "    }", "0.51.1"),
  "XX_output_debug_f":
      ("Option '--debug' is no longer supported. Column and line number counting\n" \
       "is supported by the compile option '-DQUEX_OPTION_DEBUG_SHOW'.",            \
       "0.58.3"),
  "XX_plot_graphic_format":         
      ("Option '--plot' no longer supported, use '--language dot' for example\n" \
       "to generate source code for the plot utility 'graphviz'\n"               \
       "(See http://www.graphviz.org)", 
       "0.59.9"),
  "XX_plot_character_display": 
      ("Option '--plot-character-display' and '--pcd' are no longer supported.\n" \
       "Please, use '--character-display' instead.", 
       "0.59.9"), 
  "XX_plot_graphic_format_list_f":     
      ("Option '--plot-format-list' is no longer supported. Note, that since 0.59.9\n" \
       "Quex does no longer call the GraphViz utility directly. Use '--language dot'.\n",
       "0.59.9"),
  "XX_compression_template_coef":      
      ("Option '--template-compression-coefficient' has been replaced by \n" \
       "'--template-compression-min-gain' which tells the minimum estimated number of\n" \
       "bytes that can be spared before two states would be combined.",
       "0.60.1"),
  "XX_token_id_prefix":
      ("Command line option '--token-prefix' has been renamed to '--token-id-prefix'\n"
       "for the sake of precision in expression.", "0.62.1"),
  "XX_message_on_extra_options_f": 
      ("Option '--no-message-on-extra-options' has been replaced with '--suppress %s'" 
       % NotificationDB.message_on_extra_options, "0.64.3"),
  "XX_error_on_dominated_pattern_f":
      ("Option '--no-error-on-dominated-pattern' or '--neodp' has been replaced with '--suppress %s'"
       % NotificationDB.error_on_dominated_pattern, "0.64.3"),
  "XX_error_on_special_pattern_same_f":   
      ("Option '--no-error-on-special-pattern-same' or '--neosps' has been replaced with '--suppress %s'"
       % NotificationDB.error_on_special_pattern_same, "0.64.3"),
  "XX_error_on_special_pattern_outrun_f": 
      ("Option '--no-error-on-special-pattern-outrun' or '--neospo' has been replaced with '--suppress %s'"
       % NotificationDB.error_on_special_pattern_outrun, "0.64.3"),
  "XX_error_on_special_pattern_subset_f": 
      ("Option '--no-error-on-special-pattern-subset' or '--neospsu' has been replaced with '--suppress %s'"
       % NotificationDB.error_on_special_pattern_subset, "0.64.3"),
  "XX_warning_disabled_no_token_queue_f": 
      ("Option '--no-warning-on-no-token-queue' has been replaced with '--suppress %s'"
       % NotificationDB.warning_on_no_token_queue, "0.64.3"),
  "XX_state_entry_analysis_complexity_limit":
      ("Option '--state-entry-analysis-complexity-limit' is no longer necessary.\n"
       "The related algorithm has been improved.", "0.65.1"),
  "XX_mode_files":
      ("Option '--mode-files' is no longer supported, use '-i' instead.",
       "0.65.1"),
  "XX_engine":
      ("Option '--engine' is no longer supported, use '-o' or '--analyzer-class' instead.",
       "0.65.1"),
  "XX_token_class_take_text_check_f":  
      ("Option '--token-type-no-take_text-check' or '--ttnttc' is replaced by '--suppress %i'."
       % NotificationDB.warning_on_no_token_class_take_text,
       "0.65.1"),
}
 
global_character_type_db = {
        # Name:         Type:         LittleEndian     Big Endian       Bytes per 
        #                             Converter Name:  Converter Name:  engine character:
        "uint8_t":    [ "uint8_t",    "ASCII",         "ASCII",         1],
        "uint16_t":   [ "uint16_t",   "UCS-2LE",       "UCS-2BE",       2],
        "uint32_t":   [ "uint32_t",   "UCS-4LE",       "UCS-4BE",       4],
        "byte":       [ "byte",       "ASCII",         "ASCII",         1],
        "u8":         [ "u8",         "ASCII",         "ASCII",         1],
        "u16":        [ "u16",        "UCS-2LE",       "UCS-2BE",       2],
        "u32":        [ "u32",        "UCS-4LE",       "UCS-4BE",       4],
        "unsigned8":  [ "unsigned8",  "ASCII",         "ASCII",         1],
        "unsigned16": [ "unsigned16", "UCS-2LE",       "UCS-2BE",       2],
        "unsigned32": [ "unsigned32", "UCS-4LE",       "UCS-4BE",       4],
        "wchar_t":    [ "wchar_t",    "WCHAR_T",       "WCHAR_T",       -1],
}

global_extension_db = {
    "C++": {
        "": { 
              E_Files.SOURCE:              ".cpp",
              E_Files.HEADER:              "",
              E_Files.HEADER_IMPLEMTATION: ".i",
        },
        "++": { 
              E_Files.SOURCE:              ".c++",
              E_Files.HEADER:              ".h++",
              E_Files.HEADER_IMPLEMTATION: ".h++",
        },
        "pp": { 
              E_Files.SOURCE:              ".cpp",
              E_Files.HEADER:              ".hpp",
              E_Files.HEADER_IMPLEMTATION: ".hpp",
        },
        "cc": { 
              E_Files.SOURCE:              ".cc",
              E_Files.HEADER:              ".hh",
              E_Files.HEADER_IMPLEMTATION: ".hh",
        },
        "xx": { 
              E_Files.SOURCE:              ".cxx",
              E_Files.HEADER:              ".hxx",
              E_Files.HEADER_IMPLEMTATION: ".hxx",
        },
   },
    "C": {
        "": {
              E_Files.SOURCE:              ".c",
              E_Files.HEADER:              ".h",
              E_Files.HEADER_IMPLEMTATION: ".c",
        },
   },
    "DOT": {
        "": {
              E_Files.SOURCE:              ".dot",
              E_Files.HEADER:              None,
              E_Files.HEADER_IMPLEMTATION: None,
        }
   }
}

DOC = {
    "_debug_exception_f":             ("Verbose output on internal exception.", ""),
    "analyzer_class":                 ("Specify analyzer class with optional namespace.", ""),
    "analyzer_derived_class_file":    ("Name of file containing derived class.", ""),
    "analyzer_derived_class_name":    ("Name of derived class with optional namespace.", ""),
    "buffer_codec_name":              ("Buffer internal codec.", ""),
    "buffer_codec_file":              ("Codec file describing mapping to unicode code points.", ""),
    "buffer_limit_code":              ("Buffer limit code.", ""),
    "buffer_element_size":            ("Buffer element size.", ""),
    "buffer_element_type":            ("Buffer element type.", ""),
    "buffer_based_analyzis_f":        ("No reload required. Analysis is buffer based.", ""),
    "buffer_byte_order":              ("Byte order of buffer elements.", ""),
    "comment_state_machine_f":        ("Provide state machine description in comment of generated code.", ""),
    "comment_transitions_f":          ("Provided UTF8 representation of transition characters in comments of generated code.", ""),
    "comment_mode_patterns_f":        ("", ""),
    "compression_template_f":         ("Activate template compression.", ""),
    "compression_template_uniform_f": ("Activate template compression with constraint of uniformity.", ""),
    "compression_template_min_gain":  ("Specifies minimum gain for template compression.", ""),
    "compression_path_f":             ("Activate path compression.", ""),
    "compression_path_uniform_f":     ("Activate path compression with constraint of uniformity.", ""),
    "count_column_number_f":          ("Activate column number counting.", ""),
    "count_line_number_f":            ("Activate line number counting.", ""),
    "character_display":              ("", ""),
    "path_limit_code":                ("", ""),
    "dos_carriage_return_newline_f":  ("", ""),
    "string_accumulator_f":           ("", ""),
    "converter_iconv_f":              ("Use 'iconv' library for character conversions.", ""),
    "converter_icu_f":                ("Use 'icu' library for character conversions.", ""),
    "converter_user_new_func":        ("", ""),
    "converter_ucs_coding_name":      ("", ""),
    "include_stack_support_f":        ("", ""),
    "input_mode_files":               ("", ""),
    "token_class_file":               ("", ""),
    "token_class":                    ("", ""),
    "token_class_only_f":             ("", ""),
    "token_id_foreign_definition_file":  ("", ""),
    "token_id_counter_offset":        ("", ""),
    "token_id_type":                  ("", ""),
    "token_id_prefix":                ("", ""),
    "token_queue_size":               ("", ""),
    "token_queue_safety_border":      ("", ""),
    "token_policy":                   ("", ""),
    "token_memory_management_by_user_f": ("", ""),
    "mode_transition_check_f":        ("", ""),
    "language":                       ("", ""),
    "normalize_f":                    ("", ""),
    "external_lexeme_null_object":    ("", ""),
    "output_file_naming_scheme":      ("", ""),
    "post_categorizer_f":             ("", ""),
    "output_directory":               ("", ""),
    "source_package_directory":       ("", ""),
    "show_name_spaces_f":             ("", ""),
    "single_mode_analyzer_f":         ("", ""),
    "state_entry_analysis_complexity_limit": ("", ""),
    "user_application_version_id":           ("", ""),
    #
    "version_information":               ("", ""),
    "help":                              ("", ""),
    "warning_disabled_no_token_queue_f": ("", ""),
    "warning_on_outrun_f":               ("", ""),
}

def command_line_arg_position(ParameterName):
    arg_list = SETUP_INFO[ParameterName][0]
    min_position = 1e37
    for arg in arg_list:
        if arg not in sys.argv[1:]: continue
        position = sys.argv[1:].index(arg)
        if position < min_position: min_position = position
    return min_position

def command_line_args(ParameterName):
    return SETUP_INFO[ParameterName][0]

def command_line_args_defined(cl, ParameterName):
    cl.reset_cursor() # Necessary to capture all arguments
    return cl.search(command_line_args(ParameterName))

def command_line_args_string(ParameterName):
    args = command_line_args(ParameterName)
    if len(args) == 1: return "'%s'"          % args[0]
    if len(args) == 2: return "'%s' or '%s'" % (args[0], args[1])
    txt = ""
    for arg in args[:-1]:
        txt += "%s, " % arg
    return "%sor %s" % (txt, args[-1])

