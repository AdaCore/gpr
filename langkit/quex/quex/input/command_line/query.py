import sys

import quex.engine.misc.error                   as error
from   quex.engine.misc.utf8                    import map_unicode_to_utf8
from   quex.engine.misc.interval_handling       import NumberSet, Interval
from   quex.engine.codec_db.unicode.parser      import ucs_property_db

from   quex.input.regular_expression.exception  import RegularExpressionException

import quex.input.regular_expression.core       as regular_expression
from   quex.DEFINITIONS                         import QUEX_VERSION 
import quex.engine.codec_db.core                as codec_db

from   quex.blackboard import setup as Setup

OPTION_DB = {
        "--codec-info":         ["Information about supported characters of a codec."],
        "--codec-file-info":    ["Information about supported characters of a codec file."],
        "--codec-for-language": ["Lists possible codecs for a given language."],
        "--property":           ["Querying properties"],
        "--set-by-property":    ["Determining character set by property"],
        "--set-by-expression":  ["Determining character set by property"],
        "--property-match":     ["Find property values that match wildcards"],
        "--numeric":            ["Display sets numerically",  ["--set-by-property", "--set-by-expression"]],
        "--intervals":          ["Display sets by intervals", ["--set-by-property", "--set-by-expression"]],
        "--names":              ["Display unicode names",     ["--set-by-property", "--set-by-expression"]],
}

def run(cl, Argv):

    if   Setup.query_version_f: print_version(); return
    elif Setup.query_help_f:    print_help(); return

    # Regular Expressions extract the BufferLimitCode and the PathTerminatorCode
    # from the sets. So let us define them outside the normal range.
    backup_buffer_limit_code = Setup.buffer_limit_code
    backup_path_limit_code   = Setup.path_limit_code
    Setup.buffer_limit_code = -1
    Setup.path_limit_code   = -1

    try:
        if   Setup.query_codec:                __handle_codec(cl)
        elif Setup.query_codec_file:           __handle_codec_file(cl)
        elif Setup.query_codec_language:       __handle_codec_for_language(cl)
        elif Setup.query_property is not None: __handle_property(cl)
        elif Setup.query_set_by_property:      __handle_set_by_property(cl)
        elif Setup.query_set_by_expression:    __handle_set_by_expression(cl)
        elif Setup.query_property_match:       __handle_property_match(cl)
        else:
            assert False # No query option(s) !
    except RegularExpressionException, x:
        error.log(x.message)

    Setup.buffer_limit_code = backup_buffer_limit_code
    Setup.path_limit_code   = backup_path_limit_code
    return 

def get_supported_command_line_option_description():
    txt = ""
    for key, description in OPTION_DB.items():
        txt += "    " + key
        if len(description) >= 2: 
            txt += " (only with "
            txt += repr(description[1])[1:-1]
            txt += ")"
        txt += "\n"
    return txt

def search_and_validate(CL, Option):
    if CL.search(Option) == False: return False

    # Validate command line
    ufos = CL.unidentified_options(OPTION_DB.keys())
    if len(ufos) != 0:
        error.log("Unidentified option(s) = " +  repr(ufos) + "\n" + \
                  get_supported_command_line_option_description())
    return True

def __handle_codec(cl):
    codec_name = cl.follow("", "--codec-info")
    supported_codec_list = codec_db.get_supported_codec_list(IncludeAliasesF=True)

    if codec_name == "":
        txt      = "Missing argument after '--codec-info'. Supported codecs are:\n\n"
        line_txt = ""
        for name in supported_codec_list:
            line_txt += name + ", "
            if len(line_txt) > 50: txt += line_txt + "\n"; line_txt = ""
        txt += line_txt
        txt = txt[:-2] + "."
        error.log(txt)

    character_set = codec_db.get_supported_unicode_character_set(CodecAlias=codec_name)
    __display_set(character_set, cl)

    print
    print "Codec is designed for:"
    print repr(codec_db.get_supported_language_list(codec_name))[1:-1]

def __handle_codec_file(cl):
    file_name = cl.follow("", "--codec-file-info")
    character_set = codec_db.get_supported_unicode_character_set(FileName=file_name)
    __display_set(character_set, cl)

def __handle_codec_for_language(cl):
    language_name = cl.follow("", "--codec-for-language")

    supported_language_list = codec_db.get_supported_language_list()

    if language_name == "":
        txt      = "Missing argument after '--codec-for-language'. Supported languages are:\n\n"
        line_txt = ""
        for name in supported_language_list:
            line_txt += name + ", "
            if len(line_txt) > 50: txt += line_txt + "\n"; line_txt = ""
        txt += line_txt
        txt = txt[:-2] + "."
        error.log(txt)

    print "Possible Codecs: " + repr(codec_db.get_codecs_for_language(language_name))[1:-1]

def __handle_property(cl):
    if Setup.query_property == "":
        # no specific property => display all properties in the database
        sys.stderr.write("(please, wait for database parsing to complete)\n")
        ucs_property_db.init_db()
        print ucs_property_db.get_property_descriptions()

    else:
        # specific property => display information about it
        sys.stderr.write("(please, wait for database parsing to complete)\n")
        property = __get_property(Setup.query_property)
        if property is None: return True
        print property

def __handle_property_match(cl):
    property_follower = Setup.query_property_match
    if not property_follower: return

    sys.stderr.write("(please, wait for database parsing to complete)\n")

    fields = map(lambda x: x.strip(), property_follower.split("="))
    if len(fields) != 2:
        error.log("Wrong property setting '%s'." % property_follower)

    # -- determine name and value
    name                 = fields[0]
    wild_card_expression = fields[1]

    # -- get the property from the database
    property = __get_property(name)
    if property is None: 
        return True

    # -- find the character set for the given expression
    if property.type == "Binary":
        error.log("Binary property '%s' is not subject to value wild card matching.\n" % property.name)

    for value in property.get_wildcard_value_matches(wild_card_expression):
        print value

def __handle_set_by_property(cl):
    result = Setup.query_set_by_property

    # expect: 'property-name = value'
    if not result:
        return 

    sys.stderr.write("(please, wait for database parsing to complete)\n")
    fields = map(lambda x: x.strip(), result.split("="))
    if len(fields) not in [1, 2]:
        error.log("Wrong property setting '%s'." % result)

    # -- determine name and value
    name = fields[0]
    if len(fields) == 2: value = fields[1]
    else:                value = None

    # -- get the property from the database
    property = __get_property(name)
    if property is None: 
        return True

    # -- find the character set for the given expression
    if property.type == "Binary" and value is not None:
        error.log("Binary property '%s' cannot have a value assigned to it.\n" % property.name + \
                  "Setting ignored. Printing set of characters with the given property.")

    character_set = property.get_character_set(value)
    if character_set.__class__.__name__ != "NumberSet":
        error.log(character_set)

    __display_set(character_set, cl)

def __handle_set_by_expression(cl):
    pattern_str = Setup.query_set_by_expression
    if not pattern_str : return
    dummy, character_set = regular_expression.parse_character_set("[:" + pattern_str + ":]")
    __display_set(character_set, cl)

def __display_set(CharSet, cl):
    if Setup.query_numeric_f: display = "hex"
    else:                     display = "utf8"

    CharSet.intersect_with(NumberSet(Interval(0, 0x110000)))

    print "Characters:\n"
    if Setup.query_interval_f:
        __print_set_in_intervals(CharSet, display, 80)
    elif Setup.query_unicode_names_f:
        __print_set_character_names(CharSet, display, 80)
    else:
        __print_set_single_characters(CharSet, display, 80)

    print 
   
def __get_property(Name_or_Alias):
    
    ucs_property_db.init_db()
    property = ucs_property_db[Name_or_Alias]
    if property.__class__.__name__ != "PropertyInfo":
        print property
        if Name_or_Alias.find("=") != -1: 
            print "Use command line option `--set-by-property` to investigate property settings."
        if Name_or_Alias.find("(") != -1:
            print "Use command line option `--set-by-expression` to investigate character set operations."
        return None
    
    property.init_code_point_db()
    return property

def __print_set_in_intervals(CharSet, Display, ScreenWidth):
    assert Display in ["hex", "utf8"]

    interval_list = CharSet.get_intervals(PromiseToTreatWellF=True)

    txt = ""
    line_size = 0
    for interval in interval_list:
        interval_string        = interval.get_string(Display, "-") + ", "
        interval_string_length = len(interval_string)

        if line_size + interval_string_length > ScreenWidth:
            txt += "\n"
            line_size = 0
        else:
            line_size += interval_string_length
        txt += interval_string

    print txt

def __print_set_character_names(CharSet, Display, ScreenWidth):
    for interval in CharSet.get_intervals(PromiseToTreatWellF=True):
        for code_point in range(interval.begin, interval.end):
            print "%06X: %s" % (code_point, ucs_property_db.map_code_point_to_character_name(code_point))

class CharacterList:
    def __init__(self, CharacterSet):
        interval_list = CharacterSet.get_intervals(PromiseToTreatWellF=True)
        interval_list.sort(lambda x, y: cmp(x.begin, y.begin))


        self.__interval_list      = interval_list
        self.__interval_list_size = len(interval_list)

        if self.__interval_list_size == 0:
            self.__current_character  = None
            self.__current_interval_i = -1
        else:
            # No character below 0 --> take first interval with .end > 0
            for i in range(self.__interval_list_size):
                if self.__interval_list[i].end >= 0: break

            self.__current_character  = max(0, self.__interval_list[i].begin)
            self.__current_interval_i = i

    def is_empty(self):
        return self.__interval_list_size == 0

    def next(self):
        tmp = self.__current_character

        if tmp is None: return None

        # Prepare the character for the next call
        self.__current_character += 1
        if self.__current_character == self.__interval_list[self.__current_interval_i].end:
            self.__current_interval_i += 1
            if self.__current_interval_i == self.__interval_list_size:
                self.__current_character = None # End reached
            else:
                self.__current_character = self.__interval_list[self.__current_interval_i].begin

        # Return the character that is still now to treat
        return tmp

def __print_set_single_characters(CharSet, Display, ScreenWidth):
    assert Display in ["hex", "utf8"]

    if Display == "hex":
        CharactersPerLine = 8
        ColumnWidth       = 6
    else:
        CharactersPerLine = 32
        ColumnWidth       = 2

    # just to make sure ...

    character_list = CharacterList(CharSet)
    if character_list.is_empty():
        sys.stdout.write("<Result = Empty Character Set>\n")
        return

    # Avoid memory overflow for very large sets: get character by character 
    last_start_character_of_line = -1
    last_horizontal_offset       = 0
    while 1 + 1 == 2:
        character_code = character_list.next()
        if character_code is None: break

        start_character_of_line = character_code - character_code % CharactersPerLine
        horizontal_offset       = character_code - start_character_of_line

        if start_character_of_line > last_start_character_of_line + CharactersPerLine: 
            sys.stdout.write("\n...")
        if start_character_of_line != last_start_character_of_line:
            sys.stdout.write("\n%05X: " % start_character_of_line)
            last_horizontal_offset = 0

        sys.stdout.write(" " * ColumnWidth * (horizontal_offset - last_horizontal_offset - 1))

        if Display == "hex":
            sys.stdout.write("%05X " % character_code)
        else:
            if character_code >= 0x20:
                sys.stdout.write("%s " % map_unicode_to_utf8(character_code))
            else:
                sys.stdout.write("? ")

        last_start_character_of_line = start_character_of_line
        last_horizontal_offset       = horizontal_offset
        
def print_version():
    print "Quex - Fast Universal Lexical Analyzer Generator"
    print "Version " + QUEX_VERSION
    print "(C) Frank-Rene Schaefer"
    print "ABSOLUTELY NO WARRANTY"

def print_help():
    print "Quex - Fast Universal Lexical Analyzer Generator"
    print "Please, consult the quex documentation for further help, or"
    print "visit http://quex.org"
    print "(C) Frank-Rene Schaefer"
    print "ABSOLUTELY NO WARRANTY"

