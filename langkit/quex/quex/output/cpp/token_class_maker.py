# (C) 2005-2010 Frank-Rene Schaefer
# ABSOLUTELY NO WARANTY
from   quex.DEFINITIONS                   import QUEX_PATH
from   quex.engine.misc.file_in           import get_include_guard_extension
from   quex.engine.misc.file_operations   import open_file_or_die
from   quex.engine.misc.string_handling   import blue_print
import quex.blackboard                    as     blackboard
from   quex.blackboard                    import setup as Setup, Lng

import re

def do(MapTokenIDToNameFunctionStr):
    assert blackboard.token_type_definition is not None

    if blackboard.token_type_definition.manually_written():
        # User has specified a manually written token class
        return None, None

    txt, txt_i = _do(blackboard.token_type_definition)

    # Return declaration and implementation as two strings
    if Setup.token_class_only_f:
        txt   = clean_for_independence(txt)
        txt_i = clean_for_independence(txt_i)
        map_token_id_to_name_function_str =   \
                  get_helper_definitions() \
                + clean_for_independence(MapTokenIDToNameFunctionStr) \

    if Setup.language.upper() == "C++":
        # In C++ we do inline, so we can do everything in the header file
        header_txt         = "".join([txt, "\n", txt_i])
        implementation_txt = ""
    else:
        # In C, there's a separate file in any case
        header_txt         = txt
        implementation_txt = txt_i 

    if Setup.token_class_only_f: 
        implementation_txt +=  map_token_id_to_name_function_str \
                             + lexeme_null_implementation() 

    return header_txt, implementation_txt

def _do(Descr):
    # The following things must be ensured before the function is called
    assert Descr is not None
    assert Descr.__class__.__name__ == "TokenTypeDescriptor"

    ## ALLOW: Descr.get_member_db().keys() == empty

    TemplateFile = QUEX_PATH \
                   + Lng["$code_base"] \
                   + Lng["$token_template_file"]

    TemplateIFile = QUEX_PATH \
                   + Lng["$code_base"] \
                   + Lng["$token_template_i_file"]

    template_str   = open_file_or_die(TemplateFile, Mode="rb").read()
    template_i_str = open_file_or_die(TemplateIFile, Mode="rb").read()
    
    virtual_destructor_str = ""
    if Descr.open_for_derivation_f: virtual_destructor_str = "virtual "

    if Descr.copy is None:
        # Default copy operation: Plain Copy of token memory
        copy_str = "__QUEX_STD_memcpy((void*)__this, (void*)__That, sizeof(QUEX_TYPE_TOKEN));\n"
    else:
        copy_str = Lng.SOURCE_REFERENCED(Descr.copy)

    if Descr.take_text is None:
        take_text_str = "return true;\n" 
    else:
        take_text_str = Lng.SOURCE_REFERENCED(Descr.take_text)

    include_guard_extension_str = get_include_guard_extension(
                                        Lng.NAMESPACE_REFERENCE(Descr.name_space) 
                                        + "__" + Descr.class_name)

    # In case of plain 'C' the class name must incorporate the namespace (list)
    token_class_name = Descr.class_name
    if Setup.language == "C":
        token_class_name = Setup.token_class_name_safe

    converter_declaration_include,   \
    converter_implementation_include, \
    converter_string,                 \
    converter_wstring                 = __get_converter_configuration(include_guard_extension_str)

    extra_at_begin_str = lexeme_null_declaration()
    extra_at_end_str   = ""
    if Setup.token_class_only_f:
        extra_at_begin_str = QUEX_NAME_TOKEN_define_str % include_guard_extension_str \
                             + extra_at_begin_str
        extra_at_end_str   = QUEX_NAME_TOKEN_undef_str % include_guard_extension_str \
                             + extra_at_end_str

    namespace_open, namespace_close = __namespace_brackets()
    helper_variable_replacements = [
              ["$INCLUDE_CONVERTER_DECLARATION",    converter_declaration_include],
              ["$INCLUDE_CONVERTER_IMPLEMENTATION", converter_implementation_include],
              ["$CONVERTER_STRING",                 converter_string],
              ["$CONVERTER_WSTRING",                converter_wstring],
              ["$NAMESPACE_CLOSE",                  namespace_close],
              ["$NAMESPACE_OPEN",                   namespace_open],
              ["$TOKEN_CLASS",                      token_class_name],
    ]

    txt = blue_print(template_str, 
            [
              ["$$EXTRA_AT_BEGIN$$",  extra_at_begin_str],
              ["$$EXTRA_AT_END$$",    extra_at_end_str],
            ])
    txt = blue_print(txt,
             [
              ["$$BODY$$",                    Lng.SOURCE_REFERENCED(Descr.body)],
              ["$$CONSTRUCTOR$$",             Lng.SOURCE_REFERENCED(Descr.constructor)],
              ["$$COPY$$",                    copy_str],
              ["$$DESTRUCTOR$$",              Lng.SOURCE_REFERENCED(Descr.destructor)],
              ["$$DISTINCT_MEMBERS$$",        get_distinct_members(Descr)],
              ["$$FOOTER$$",                  Lng.SOURCE_REFERENCED(Descr.footer)],
              ["$$FUNC_TAKE_TEXT$$",          take_text_str],
              ["$$HEADER$$",                  Lng.SOURCE_REFERENCED(Descr.header)],
              ["$$INCLUDE_GUARD_EXTENSION$$", include_guard_extension_str],
              ["$$NAMESPACE_CLOSE$$",         Lng.NAMESPACE_CLOSE(Descr.name_space)],
              ["$$NAMESPACE_OPEN$$",          Lng.NAMESPACE_OPEN(Descr.name_space)],
              ["$$QUICK_SETTERS$$",           get_quick_setters(Descr)],
              ["$$SETTERS_GETTERS$$",         get_setter_getter(Descr)],
              ["$$TOKEN_REPETITION_N_GET$$",  Lng.SOURCE_REFERENCED(Descr.repetition_get)],
              ["$$TOKEN_REPETITION_N_SET$$",  Lng.SOURCE_REFERENCED(Descr.repetition_set)],
              ["$$UNION_MEMBERS$$",           get_union_members(Descr)],
              ["$$VIRTUAL_DESTRUCTOR$$",      virtual_destructor_str],
              ["$$TOKEN_CLASS_NAME_SAFE$$",   Descr.class_name_safe],
             ])

    txt   = blue_print(txt, helper_variable_replacements)

    if Setup.language.upper() != "C++" and Setup.token_class_only_f:
        extra_at_begin_str += local_strlen_str % (Descr.class_name_safe, Setup.buffer_element_type, Setup.buffer_element_type)

    txt_i = blue_print(template_i_str, 
            [
              ["$$EXTRA_AT_BEGIN$$",  extra_at_begin_str],
              ["$$EXTRA_AT_END$$",    extra_at_end_str],
            ])
    txt_i = blue_print(txt_i, 
                       [
                        ["$$CONSTRUCTOR$$",             Lng.SOURCE_REFERENCED(Descr.constructor)],
                        ["$$COPY$$",                    copy_str],
                        ["$$DESTRUCTOR$$",              Lng.SOURCE_REFERENCED(Descr.destructor)],
                        ["$$FOOTER$$",                  Lng.SOURCE_REFERENCED(Descr.footer)],
                        ["$$FUNC_TAKE_TEXT$$",          take_text_str],
                        ["$$TOKEN_CLASS_HEADER$$",      Setup.get_file_reference(blackboard.token_type_definition.get_file_name())],
                        ["$$INCLUDE_GUARD_EXTENSION$$", include_guard_extension_str],
                        ["$$NAMESPACE_OPEN$$",          Lng.NAMESPACE_OPEN(Descr.name_space)],
                        ["$$NAMESPACE_CLOSE$$",         Lng.NAMESPACE_CLOSE(Descr.name_space)],
                        ["$$TOKEN_REPETITION_N_GET$$",  Lng.SOURCE_REFERENCED(Descr.repetition_get)],
                        ["$$TOKEN_REPETITION_N_SET$$",  Lng.SOURCE_REFERENCED(Descr.repetition_set)],
                        ["$$TOKEN_CLASS_NAME_SAFE$$",   Descr.class_name_safe],
                       ])


    txt_i = blue_print(txt_i, helper_variable_replacements)

    return txt, txt_i

def get_distinct_members(Descr):
    # '0' to make sure, that it works on an empty sequence too.
    TL = Descr.type_name_length_max()
    NL = Descr.variable_name_length_max()
    txt = ""
    for name, type_code in Descr.distinct_db.items():
        txt += __member(type_code, TL, name, NL)
    #txt += Lng._SOURCE_REFERENCE_END()
    return txt

def get_union_members(Descr):
    # '0' to make sure, that it works on an empty sequence too.
    TL = Descr.type_name_length_max()
    NL = Descr.variable_name_length_max()
    if len(Descr.union_db) == 0: return ""
    
    txt = "    union {\n"
    for name, type_descr in Descr.union_db.items():
        if type(type_descr) == dict:
            txt += "        struct {\n"
            for sub_name, sub_type in type_descr.items():
                txt += __member(sub_type, TL, sub_name, NL, IndentationOffset=" " * 8)
            txt += "\n        } %s;\n" % name
        else:
            txt += __member(type_descr, TL, name, NL, IndentationOffset=" " * 4) + "\n"
    txt += "    } content;\n"
    #txt += Lng._SOURCE_REFERENCE_END()
    return txt

def __member(TypeCode, MaxTypeNameL, VariableName, MaxVariableNameL, IndentationOffset=""):
    my_def  = Lng._SOURCE_REFERENCE_BEGIN(TypeCode.sr)
    my_def += IndentationOffset
    my_def += Lng.CLASS_MEMBER_DEFINITION(TypeCode.get_pure_text(), MaxTypeNameL, 
                                          VariableName)
    my_def += Lng._SOURCE_REFERENCE_END(TypeCode.sr)
    return my_def

def get_setter_getter(Descr):
    """NOTE: All names are unique even in combined unions."""
    TL = Descr.type_name_length_max()
    NL = Descr.variable_name_length_max()
    variable_db = Descr.get_member_db()
    txt = ""
    for variable_name, info in variable_db.items():
        type_code = info[0]
        access    = info[1]
        type_str  = type_code.get_pure_text()
        txt += Lng._SOURCE_REFERENCE_BEGIN(type_code.sr)
        my_def = "    %s%s get_%s() const %s{ return %s; }" \
                 % (type_str,      " " * (TL - len(type_str)), 
                    variable_name, " " * ((NL + TL)- len(variable_name)), 
                    access)
        txt += my_def

        type_str = type_str.strip()
        type_str = type_str.replace("\t", " ")
        while type_str.find("  ") != -1:
            type_str = type_str.replace("  ", " ")
        if type_str not in ["char", "unsigned char", "singed char",
                            "short", "unsigned short", "singed short",
                            "int", "unsigned int", "singed int",
                            "long", "unsigned long", "singed long",
                            "float", "unsigned float", "singed float",
                            "double", "unsigned double", "singed double",
                            "uint8_t", "uint16_t", "uint32_t",
                            "int8_t", "int16_t", "int32_t",
                            "size_t", "uintptr_t", "ptrdiff_t"]:
            type_str += "&"

        txt += Lng._SOURCE_REFERENCE_BEGIN(type_code.sr)
        my_def = "    void%s set_%s(%s Value) %s{ %s = Value; }" \
               % (" " * (TL - len("void")), 
                  variable_name, type_str, " " * (NL + TL - (len(type_str) + len(variable_name))), 
                  access)
        txt += my_def

    txt += Lng._SOURCE_REFERENCE_END()
    return txt

def get_quick_setters(Descr):
    """NOTE: All names are unique even in combined unions."""
    variable_db         = Descr.get_member_db()
    used_signature_list = []

    def __quick_setter(ArgList, used_signature_list):
        """ArgList = [ [Name, Type], [Name, Type], ...]
         
           NOTE: There cannot be two signatures of the same type specification.
                 This is so, since functions are overloaded, have the same name
                 and only identify with their types.
        """
        signature = map(lambda x: x[1].get_pure_text(), ArgList)
        if signature in used_signature_list:
            return ""
        else:
            used_signature_list.append(signature)

        txt = "    void set(const QUEX_TYPE_TOKEN_ID ID, "
        i = -1
        for name, type_info in ArgList:
            i += 1
            type_str = type_info.get_pure_text()
            if type_str.find("const") != -1: type_str = type_str[5:]
            txt += "const %s& Value%i, " % (type_str, i)
        txt = txt[:-2]
        txt += ")\n    { "
        txt += "_id = ID; "
        i = -1
        for name, type_info in ArgList:
            i += 1
            txt += "%s = Value%i; " % (variable_db[name][1], i)
        txt += "}\n"

        return txt

    def __combined_quick_setters(member_db, used_signature_list, AllOnlyF=False):
        txt = ""
        member_list = member_db.items()
        if len(member_list) == 0: return ""

        # sort the members with respect to their occurence in the token_type section
        member_list.sort(lambda x, y: cmp(x[1].sr.line_n, y[1].sr.line_n))
        L = len(member_list)
        PresenceAll  = [ 1 ] * L
        if AllOnlyF: presence_map = PresenceAll
        else:        presence_map = [ 1 ] + [ 0 ] * (L - 1)  

        while 1 + 1 == 2:
            # build the argument list consisting of a permutation of distinct members
            arg_list = []
            for i in range(L):
                if presence_map[i]: arg_list.append(member_list[i])

            txt += __quick_setter(arg_list, used_signature_list)

            # increment the presence map
            if presence_map == PresenceAll:
                break
            for i in range(L):
                if presence_map[i] == 1: presence_map[i] = 0
                else:                    presence_map[i] = 1; break

        return txt

    txt = ""

    # (*) Quick setters for distinct members
    txt += __combined_quick_setters(Descr.distinct_db, used_signature_list)

    # (*) Quick setters for union members
    complete_f = True
    for name, type_info in Descr.union_db.items():
        if type(type_info) != dict: setter_txt = __quick_setter([[name, type_info]], used_signature_list)
        else:                       setter_txt = __combined_quick_setters(type_info, used_signature_list, AllOnlyF=True)
        if len(setter_txt) == 0: complete_f = False
        txt += setter_txt

    if not complete_f:
        txt = "   /* Not all members are accessed via quick-setters (avoid overload errors). */" \
              + txt

    return txt

def __get_converter_configuration(IncludeGuardExtension):
    token_descr = blackboard.token_type_definition

    if not Setup.converter_helper_required_f:
        declaration_include    = "#include <quex/code_base/converter_helper/identity>"
        implementation_include = "#include <quex/code_base/converter_helper/identity.i>"
        from_codec = "identical"

    elif Setup.buffer_codec.name in ["utf8", "utf16", "utf32"]:
        declaration_include    = "#include <quex/code_base/converter_helper/from-%s>"   % Setup.buffer_codec.name
        implementation_include = "#include <quex/code_base/converter_helper/from-%s.i>" % Setup.buffer_codec.name
        from_codec = Setup.buffer_codec.name

    elif Setup.buffer_codec.name == "unicode":
        declaration_include    = "#include <quex/code_base/converter_helper/from-unicode-buffer>"
        implementation_include = "#include <quex/code_base/converter_helper/from-unicode-buffer.i>"
        from_codec = Setup.buffer_codec.name

    else:
        declaration_include    = "#include \"%s\"" % \
                                           Setup.get_file_reference(Setup.output_buffer_codec_header)
        implementation_include = "#include \"%s\"" % \
                                           Setup.get_file_reference(Setup.output_buffer_codec_header_i)
        from_codec = Setup.buffer_codec.name

    if not Setup.token_class_only_f:
        string  = "QUEX_CONVERTER_STRING(%s,char)"  % from_codec
        wstring = "QUEX_CONVERTER_STRING(%s,wchar)" % from_codec

        return declaration_include, implementation_include, string, wstring

    # From Here One: 'Sharable Token Class Generation'
    if Setup.language.upper() == "C++":
        function_prefix       = Lng.NAMESPACE_REFERENCE(token_descr.name_space) 
        function_def_prefix   = ""
        function_def_prefix_0 = ""
        namespace_token_open  = Lng.NAMESPACE_OPEN(token_descr.name_space).replace("\n", " ")
        namespace_token_close = Lng.NAMESPACE_CLOSE(token_descr.name_space).replace("\n", " ")
    else:
        function_prefix       = token_descr.class_name_safe + " ##"
        function_def_prefix   = token_descr.class_name_safe + " ##"
        function_def_prefix_0 = token_descr.class_name_safe 
        namespace_token_open  = ""
        namespace_token_close = ""


    before = frame_begin                                    \
             % (IncludeGuardExtension,                      \
                function_def_prefix, function_def_prefix,   \
                function_prefix,     function_prefix,       \
                namespace_token_open, namespace_token_close,\
                Setup.buffer_element_type)
    after  = frame_end \
             % IncludeGuardExtension

    declaration_include    = "%s%s\n%s" \
                             % (before, declaration_include, after)
    implementation_include = "%s%s\n%s" \
                             % (before, implementation_include, after)

    # In C:   Function call and def prefix is the same
    # In C++: We are in the same namespace, no prefix, function_def_prefix is empty anyway.
    string  = "%s%s_to_char"  % (function_def_prefix_0, from_codec)
    wstring = "%s%s_to_wchar" % (function_def_prefix_0, from_codec)

    return declaration_include, implementation_include, string, wstring

QUEX_strlen_re                = re.compile("QUEX_NAME\\(strlen\\)", re.UNICODE)
QUEX_TYPE_CHARACTER_re        = re.compile("\\bQUEX_TYPE_CHARACTER\\b", re.UNICODE)
QUEX_LEXEME_NULL_re           = re.compile("\\bQUEX_LEXEME_NULL\\b", re.UNICODE)
QUEX_TYPE_ANALYZER_re         = re.compile("\\bQUEX_TYPE_ANALYZER\\b", re.UNICODE)
QUEX_TYPE_TOKEN_ID_re         = re.compile("\\bQUEX_TYPE_TOKEN_ID\\b", re.UNICODE)
QUEX_LexemeNullDeclaration_re = re.compile("QUEX_NAME\\(LexemeNullObject\\)", re.UNICODE)
QUEX_TYPE_CHARACTER_safe_re   = re.compile("\\$\\$quex_type_character\\$\\$", re.UNICODE)

def clean_for_independence(txt):
    token_descr = blackboard.token_type_definition

    global QUEX_MEMORY_FREE_re
    global QUEX_MEMORY_ALLOC_re
    global QUEX_strlen_re
    global QUEX_TYPE_CHARACTER_re
    global QUEX_TYPE_ANALYZER_re
    global QUEX_TYPE_TOKEN_ID_re
    global QUEX_LexemeNullDeclaration_re
    global QUEX_TYPE_CHARACTER_safe_re
    global QUEX_LEXEME_NULL_re

    txt = QUEX_TYPE_CHARACTER_re.sub(Setup.buffer_element_type, txt)
    txt = QUEX_TYPE_ANALYZER_re.sub("void", txt)
    txt = QUEX_TYPE_TOKEN_ID_re.sub(Setup.token_id_type, txt)
    txt = QUEX_LexemeNullDeclaration_re.sub(common_lexeme_null_str(), txt)
    # txt = QUEX_MEMORY_ALLOC_re.sub("malloc", txt)
    # txt = QUEX_MEMORY_FREE_re.sub("free", txt)
    txt = QUEX_TYPE_CHARACTER_safe_re.sub("QUEX_TYPE_CHARACTER", txt)
    txt = QUEX_strlen_re.sub("%s_strlen" % token_descr.class_name_safe, txt)
    txt = QUEX_LEXEME_NULL_re.sub(common_lexeme_null_str(), txt)

    # Delete any line references
    result = []
    for line in txt.splitlines():
        x = line.strip()
        if len(x) != 0 and x[0] == "#":
            x = x[1:].strip()
            if x.find("line") == 0: 
                continue
        result.append(line + "\n")
    return "".join(result)

def common_lexeme_null_str():
    token_descr = blackboard.token_type_definition
    if Setup.language.upper() == "C++": 
        # LexemeNull's namespace == token namespace, no explicit naming.
        return "LexemeNullObject"
    else:                               
        namespace_prefix = Lng.NAMESPACE_REFERENCE(token_descr.name_space) 
        return "%sLexemeNullObject" % namespace_prefix

def __namespace_brackets(DefineF=False):
    token_descr = blackboard.token_type_definition

    if Setup.language.upper() == "C++":
        open_str  = Lng.NAMESPACE_OPEN(token_descr.name_space).strip()
        close_str = Lng.NAMESPACE_CLOSE(token_descr.name_space).strip()
        if DefineF:
            open_str  = open_str.replace("\n", "\\\n")
            close_str = close_str.replace("\n", "\\\n")
        return open_str, close_str
    else:
        return "", ""

def lexeme_null_declaration():
    if Setup.token_class_only_f:
        namespace_open, namespace_close = __namespace_brackets()
        return "".join([
                    "%s\n" % namespace_open,
                    "extern %s  %s;\n" % (Setup.buffer_element_type, common_lexeme_null_str()),
                    "%s\n\n" % namespace_close,
                  ])
    else:
        # The following should hold in any both cases:
        return "".join([
                    "QUEX_NAMESPACE_LEXEME_NULL_OPEN\n",
                    "extern QUEX_TYPE_CHARACTER   QUEX_LEXEME_NULL_IN_ITS_NAMESPACE;\n" 
                    "QUEX_NAMESPACE_LEXEME_NULL_CLOSE\n",
                  ])

def lexeme_null_implementation():
    namespace_open, namespace_close = __namespace_brackets()

    return "".join([
                "%s\n" % namespace_open,
                "%s  %s = (%s)0;\n" % (Setup.buffer_element_type, common_lexeme_null_str(), Setup.buffer_element_type),
                "%s\n\n" % namespace_close,
              ])

local_strlen_str = """
QUEX_INLINE size_t 
%s_strlen(const %s* Str)
{
    const %s* iterator = Str;
    while( *iterator != 0 ) ++iterator; 
    return (size_t)(iterator - Str);
}

"""

QUEX_NAME_TOKEN_define_str = """
#if ! defined(QUEX_NAME_TOKEN)
#   if defined(__QUEX_OPTION_PLAIN_C)
#      define QUEX_NAME_TOKEN(NAME)   $$TOKEN_CLASS_NAME_SAFE$$_ ## NAME
#   else
#      define QUEX_NAME_TOKEN(NAME)   $TOKEN_CLASS_ ## NAME
#   endif
#   define __QUEX_SIGNAL_DEFINED_QUEX_NAME_TOKEN_%s
#endif
"""

QUEX_NAME_TOKEN_undef_str = """
#if defined(__QUEX_SIGNAL_DEFINED_QUEX_NAME_TOKEN_%s)
#   undef QUEX_NAME_TOKEN
#endif
"""

frame_begin = """
#if defined(QUEX_CONVERTER_CHAR_DEF)
#   undef  __QUEX_CONVERTER_CHAR_DEF
#   undef  __QUEX_CONVERTER_STRING_DEF
#   undef  QUEX_CONVERTER_CHAR_DEF
#   undef  QUEX_CONVERTER_STRING_DEF
#   undef  __QUEX_CONVERTER_CHAR
#   undef  __QUEX_CONVERTER_STRING
#   undef  QUEX_CONVERTER_CHAR
#   undef  QUEX_CONVERTER_STRING
#   undef  QUEX_NAMESPACE_MAIN_OPEN               
#   undef  QUEX_NAMESPACE_MAIN_CLOSE              
#   undef  $$quex_type_character$$
#   define __QUEX_SIGNAL_UNDEFINED_CONVERTER_MACROS_%s
#endif
#define    __QUEX_CONVERTER_CHAR_DEF(FROM, TO)    %sFROM ## _to_ ## TO ## _character
#define    __QUEX_CONVERTER_STRING_DEF(FROM, TO)  %sFROM ## _to_ ## TO
#define    QUEX_CONVERTER_CHAR_DEF(FROM, TO)      __QUEX_CONVERTER_CHAR_DEF(FROM, TO)
#define    QUEX_CONVERTER_STRING_DEF(FROM, TO)    __QUEX_CONVERTER_STRING_DEF(FROM, TO)
#define    __QUEX_CONVERTER_CHAR(FROM, TO)        %sFROM ## _to_ ## TO ## _character
#define    __QUEX_CONVERTER_STRING(FROM, TO)      %sFROM ## _to_ ## TO
#define    QUEX_CONVERTER_CHAR(FROM, TO)          __QUEX_CONVERTER_CHAR(FROM, TO)
#define    QUEX_CONVERTER_STRING(FROM, TO)        __QUEX_CONVERTER_STRING(FROM, TO)
#define    QUEX_NAMESPACE_MAIN_OPEN               %s
#define    QUEX_NAMESPACE_MAIN_CLOSE              %s
#define    $$quex_type_character$$                %s

#define __QUEX_INCLUDE_GUARD__CONVERTER_HELPER__TMP_DISABLED
"""

frame_end = """
#undef  __QUEX_INCLUDE_GUARD__CONVERTER_HELPER__TMP_DISABLED

#undef     __QUEX_CONVERTER_CHAR_DEF
#undef     __QUEX_CONVERTER_STRING_DEF
#undef     QUEX_CONVERTER_CHAR_DEF
#undef     QUEX_CONVERTER_STRING_DEF
#undef     __QUEX_CONVERTER_CHAR
#undef     __QUEX_CONVERTER_STRING
#undef     QUEX_CONVERTER_CHAR
#undef     QUEX_CONVERTER_STRING
#undef     QUEX_NAMESPACE_MAIN_OPEN               
#undef     QUEX_NAMESPACE_MAIN_CLOSE              
#undef     $$quex_type_character$$
#if defined(__QUEX_SIGNAL_UNDEFINED_CONVERTER_MACROS_%s)
#   define __QUEX_CONVERTER_CHAR_DEF    __QUEX_CONVERTER_CHAR_DEF_BACKUP
#   define __QUEX_CONVERTER_STRING_DEF  __QUEX_CONVERTER_STRING_DEF_BACKUP
#   define QUEX_CONVERTER_CHAR_DEF      QUEX_CONVERTER_CHAR_DEF_BACKUP
#   define QUEX_CONVERTER_STRING_DEF    QUEX_CONVERTER_STRING_DEF_BACKUP
#   define __QUEX_CONVERTER_CHAR        __QUEX_CONVERTER_CHAR_BACKUP
#   define __QUEX_CONVERTER_STRING      __QUEX_CONVERTER_STRING_BACKUP
#   define QUEX_CONVERTER_CHAR          QUEX_CONVERTER_CHAR_BACKUP
#   define QUEX_CONVERTER_STRING        QUEX_CONVERTER_STRING_BACKUP
#   define QUEX_NAMESPACE_MAIN_OPEN     QUEX_NAMESPACE_MAIN_OPEN_BACKUP               
#   define QUEX_NAMESPACE_MAIN_CLOSE    QUEX_NAMESPACE_MAIN_CLOSE_BACKUP              
#   define $$quex_type_character$$          $$quex_type_character$$_BACKUP
#endif
"""

helper_definitions = """
#if ! defined(__QUEX_OPTION_PLAIN_C)
#   define QUEX_NAME_TOKEN(NAME)       %s_ ## NAME
#   define QUEX_NAMESPACE_TOKEN_OPEN   %s
#   define QUEX_NAMESPACE_TOKEN_CLOSE  %s
#   define QUEX_NAMESPACE_LEXEME_NULL_OPEN    %s
#   define QUEX_NAMESPACE_LEXEME_NULL_CLOSE   %s
#else
#   define QUEX_NAME_TOKEN(NAME)       %s_ ## NAME
#   define QUEX_NAMESPACE_TOKEN_OPEN  
#   define QUEX_NAMESPACE_TOKEN_CLOSE 
#   define QUEX_NAMESPACE_LEXEME_NULL_OPEN     
#   define QUEX_NAMESPACE_LEXEME_NULL_CLOSE    
#endif
#define QUEX_TYPE_TOKEN_ID             %s
#include "%s"
#include "%s" 
"""
def get_helper_definitions():
    namespace_open, namespace_close = __namespace_brackets(DefineF=True)
    token_descr                     = blackboard.token_type_definition
    if len(Setup.token_id_foreign_definition_file) != 0:
        token_id_definition_file = Setup.token_id_foreign_definition_file
    else:
        token_id_definition_file = Setup.output_token_id_file

    ln_namespace_open  = Lng.NAMESPACE_OPEN(Setup.lexeme_null_namespace).replace("\n", "\\\n")
    ln_namespace_close = Lng.NAMESPACE_CLOSE(Setup.lexeme_null_namespace).replace("\n", "\\\n")

    return helper_definitions \
           % (token_descr.class_name, 
              namespace_open,         
              namespace_close,        
              ln_namespace_open,      
              ln_namespace_close,      
              token_descr.class_name_safe, 
              Setup.token_id_type,
              Setup.get_file_reference(Setup.output_token_class_file),
              token_id_definition_file)
                       

