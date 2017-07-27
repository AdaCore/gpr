from   quex.DEFINITIONS              import QUEX_PATH
import quex.input.command_line.query as     query
from   quex.input.setup              import SETUP_INFO, DEPRECATED, \
                                            global_character_type_db, \
                                            command_line_args_defined, \
                                            command_line_args_string, \
                                            command_line_args, \
                                            SetupParTypes, \
                                            NotificationDB
import quex.engine.codec_db.core     as     codec_db
import quex.engine.misc.error        as     error
from   quex.engine.misc.file_in      import is_identifier
import os.path

def do(setup, command_line, argv):
    """Does a consistency check for setup and the command line.
    """

    setup.output_directory = os.path.normpath(setup.output_directory)
    if setup.output_directory:
        # Check, if the output directory exists
        if os.access(setup.output_directory, os.F_OK) == False:
            error.log("The directory %s was specified for output, but does not exists." % setup.output_directory)
        if os.access(setup.output_directory, os.W_OK) == False:
            error.log("The directory %s was specified for output, but is not writeable." % setup.output_directory)

    # if the mode is '--language dot' => check character display options. 
    if setup.character_display not in ["hex", "utf8"]:
        error.log("Character display must be either 'hex' or 'utf8'.\nFound: '%s'" % 
                  setup.character_display)

    # ensure that options are not specified twice
    for parameter, info in SETUP_INFO.items():
        if type(info) != list: continue
        occurence_n = 0 
        for option in info[0]:
            occurence_n += argv.count(option)
        if occurence_n > 1 and info[1] not in (SetupParTypes.LIST, SetupParTypes.INT_LIST):
            error.log("Received more than one of the following options:\n" + \
                      "%s" % repr(info[0])[1:-1])

    # (*) Check for 'Depraceted' Options ___________________________________________________
    for name, info in DEPRECATED.items():
        command_line_options = SETUP_INFO[name][0]
        comment                   = info[0]
        depreciated_since_version = info[1]
        for option in command_line_options:
            if command_line.search(option):
                error.log("Command line option '%s' is ignored.\n" % option + \
                          comment + "\n" + \
                          "Last version of Quex supporting this option is version %s. Please, visit\n" % \
                          depreciated_since_version + \
                          "http://quex.org for further information.")
                          
    # (*) Check for 'Straying' Options ___________________________________________________
    options = []
    for key, info in SETUP_INFO.items():
        if type(info) != list: continue
        if key in DEPRECATED: continue
        if info[1] is not None: options.extend(info[0])
    options.sort(lambda a,b: cmp(a.replace("-",""), b.replace("-","")))

    ufos = command_line.unidentified_options(options)
    if len(ufos) != 0:
        error.log("Unidentified option(s) = " +  repr(ufos) + "\n" + \
                  __get_supported_command_line_option_description(options))

    if setup.analyzer_derived_class_name != "" and \
       setup.analyzer_derived_class_file == "":
            error.log("Specified derived class '%s' on command line, but it was not\n" % \
                      setup.analyzer_derived_class_name + \
                      "specified which file contains the definition of it.\n" + \
                      "use command line option '--derived-class-file'.\n")

    if setup.buffer_element_size not in [-1, 1, 2, 4]:
        error.log("The setting of '--buffer-element-size' (or '-b') can only be\n" 
                  "1, 2, or 4 (found %s)." % repr(setup.buffer_element_size))

    if setup.buffer_byte_order not in ["<system>", "little", "big"]:
        error.log("Byte order (option --endian) must be 'little', 'big', or '<system>'.\n" + \
                  "Note, that this option is only interesting for cross plattform development.\n" + \
                  "By default, quex automatically chooses the endian type of your system.")

    # Manually written token class requires token class name to be specified
    if setup.token_class_file != "" and command_line.search("--token-class", "--tc") == False:
        error.log("The use of a manually written token class requires that the name of the class\n"
                  "is specified on the command line via the '--token-class' option.")
    
    # Token queue
    if setup.token_policy != "queue" and command_line.search("--token-queue-size"):
        error.log("Option --token-queue-size determines a fixed token queue size. This makes\n" + \
                  "only sense in conjunction with '--token-policy queue'.\n")
    if setup.token_queue_size <= setup.token_queue_safety_border + 1:
        if setup.token_queue_size == setup.token_queue_safety_border: cmp_str = "equal to"
        else:                                                         cmp_str = "less than"
        error.log("Token queue size is %i is %s token queue safety border %i + 1.\n" % \
                  (setup.token_queue_size, cmp_str, setup.token_queue_safety_border) + 
                  "Set appropriate values with --token-queue-size and --token-queue-safety-border.")

    # Check that names are valid identifiers
    if len(setup.token_id_prefix_plain) != 0:
        __check_identifier(setup, "token_id_prefix_plain", "Token prefix")
    __check_identifier(setup, "analyzer_class_name", "Engine name")
    if setup.analyzer_derived_class_name != "": 
        __check_identifier(setup, "analyzer_derived_class_name", "Derived class name")
    
    __check_file_name(setup, "token_class_file",                 "file containing token class definition")
    __check_file_name(setup, "analyzer_derived_class_file",      "file containing user derived lexer class")
    __check_file_name(setup, "token_id_foreign_definition_file", "file containing user token ids", 0,
                      CommandLineOption=SETUP_INFO["token_id_foreign_definition"][0])
    __check_file_name(setup, "input_mode_files", "quex source file")

    # Check that not more than one converter is specified
    converter_n = 0
    if setup.converter_iconv_f:                 converter_n += 1
    if setup.converter_icu_f:                   converter_n += 1 
    if len(setup.converter_user_new_func) != 0: converter_n += 1
    if converter_n > 1:
        error.log("More than one character converter has been specified. Note, that the\n" + \
                  "options '--icu', '--iconv', and '--converter-new' (or '--cn') are\n"    + \
                  "to be used mutually exclusively.")
    if converter_n == 1 and setup.buffer_codec.name != "unicode":  
        # If the buffer codec is other than unicode, then no converter shall
        # be used to fill the buffer. Instead, the engine is transformed, so 
        # that it works directly on the codec.
        error.log("An engine that is to be generated for a specific codec cannot rely\n"      + \
                  "on converters. Do no use '--codec' together with '--icu', '--iconv', or\n" + \
                  "`--converter-new`.")

    # If a converter has been specified and no bytes-element-size has been specified,
    # it defaults to '1 byte' which is most likely not what is desired for unicode.
    if     converter_n == 1 \
       and setup.buffer_element_size == 1 \
       and not command_line_args_defined(command_line, "buffer_element_size") \
       and not command_line_args_defined(command_line, "buffer_element_type"):
        error.log("A converter has been specified, but the default buffer element size\n" + \
                  "is left to 1 byte. Consider %s or %s." \
                  % (command_line_args_string("buffer_element_size"),
                     command_line_args_string("buffer_element_type")))

    # If a user defined type is specified for 'engine character type' and 
    # a converter, then the name of the target type must be specified explicitly.
    if         setup.buffer_element_type != "" \
       and not global_character_type_db.has_key(setup.buffer_element_type) \
       and     setup.converter_ucs_coding_name == "" \
       and     converter_n != 0:
        tc = setup.buffer_element_type
        error.log("A character code converter has been specified. It is supposed to convert\n" + \
                  "incoming data into an internal buffer of unicode characters. The size of\n" + \
                  "each character is determined by '%s' which is a user defined type.\n" % tc  + \
                  "\n" + \
                  "Quex cannot determine automatically the name that the converter requires\n" +      \
                  "to produce unicode characters for type '%s'. It must be specified by the\n" % tc + \
                  "command line option %s." \
                  % command_line_args_string("converter_ucs_coding_name"))

    # Token transmission policy
    token_policy_list = ["queue", "single", "users_token", "users_queue"]
    if setup.token_policy not in token_policy_list:
        error.log("Token policy '%s' not supported. Use one of the following:\n" % setup.token_policy + \
                  repr(token_policy_list)[1:-1])
    elif setup.token_policy == "users_token":
        error.log("Token policy 'users_queue' has be deprecated since 0.49.1. Use\n"
                  "equivalent policy 'single'.")
    elif setup.token_policy == "users_queue":
        error.log("Token policy 'users_queue' has be deprecated since 0.49.1\n")

    # Internal engine character encoding
    def __codec_vs_buffer_element_size(CodecName, RequiredBufferElementSize):
        if   setup.buffer_codec.name   != CodecName:                 return
        elif setup.buffer_element_size == RequiredBufferElementSize: return

        if setup.buffer_element_size == -1: 
            msg_str = "undetermined (found type '%s')" % setup.buffer_element_type
        else:
            msg_str = "is not %i (found %i)" % (RequiredBufferElementSize, setup.buffer_element_size)

        error.log("Using codec '%s' while buffer element size %s.\n" % (CodecName, msg_str) + 
                  "Consult command line argument %s" \
                  % command_line_args_string("buffer_element_size"))

    if setup.buffer_codec.name != "unicode":
        if not setup.buffer_codec_file:
            error.verify_word_in_list(setup.buffer_codec_name,
                                      codec_db.get_supported_codec_list() + ["utf8", "utf16"],
                                      "Codec '%s' is not supported." % setup.buffer_codec.name)
        __codec_vs_buffer_element_size("utf8", 1)
        __codec_vs_buffer_element_size("utf16", 2)

    if setup.external_lexeme_null_object and setup.token_class_only_f:
        error.log("Specifying an external lexeme null object signalizes an\n"
                  "external token class implementation. The 'token class only\n"
                  "flag' generates a token class considered to be externally\n"
                  "shared. Both flags are mutually exclusive.")

    if setup.string_accumulator_f:
        error_n = NotificationDB.warning_on_no_token_class_take_text
        if error_n in setup.suppressed_notification_list: 
           error.warning("The warning upon missing 'take_text' in token type definition is de-\n"
                     + "activated by '--suppress %i'. This is dangerous, if there is a string\n" % error_n
                     + "accumulator. May be, use '--no-string-accumulator'.", -1,
                    SuppressCode=NotificationDB.warning_on_no_warning_on_missing_take_text)

def __check_identifier(setup, Candidate, Name):
    value = setup.__dict__[Candidate]
    if is_identifier(value): return

    CommandLineOption = ""
    if type(SETUP_INFO) == list:
        CommandLineOption = " (%s)" % str(SETUP_INFO[Candidate][0])[-1:1]

    error.log("%s must be a valid identifier%s.\n" % (Name, CommandLineOption) + \
              "Received: '%s'" % value)

def __get_supported_command_line_option_description(NormalModeOptions):
    txt = "OPTIONS:\n"
    for option in NormalModeOptions:
        txt += "    " + option + "\n"

    txt += "\nOPTIONS FOR QUERY MODE:\n"
    txt += query.get_supported_command_line_option_description()
    return txt

def __check_file_name(setup, Candidate, Name, Index=None, CommandLineOption=None):
    value             = setup.__dict__[Candidate]
    if len(value) == 0: return

    if CommandLineOption is None:
        CommandLineOption = command_line_args(Candidate)

    if Index is not None:
        if type(value) != list or len(value) <= Index: value = ""
        else:                                          value = value[Index]

    if type(value) == list:
        for name in value:
            if name != "" and name[0] == "-": 
                error.log("Quex refuses to work with file names that start with '-' (minus).\n"  + \
                          "Received '%s' for %s (%s)" % (value, name, repr(CommandLineOption)[1:-1]))
            if os.access(name, os.F_OK) == False:
                # error.log("File %s (%s)\ncannot be found." % (name, Name))
                error.log_file_not_found(name, Name)
    else:
        if value == "" or value[0] == "-":              return
        if os.access(value, os.F_OK):                   return
        if os.access(QUEX_PATH + "/" + value, os.F_OK): return
        if     os.access(os.path.dirname(value), os.F_OK) == False \
           and os.access(QUEX_PATH + "/" + os.path.dirname(value), os.F_OK) == False:
            error.log("File '%s' is supposed to be located in directory '%s' or\n" % \
                      (os.path.basename(value), os.path.dirname(value)) + \
                      "'%s'. No such directories exist." % \
                      (QUEX_PATH + "/" + os.path.dirname(value)))
        error.log_file_not_found(value, Name)

