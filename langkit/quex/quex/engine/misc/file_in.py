#! /usr/bin/env python
# Quex is  free software;  you can  redistribute it and/or  modify it  under the
# terms  of the  GNU Lesser  General  Public License  as published  by the  Free
# Software Foundation;  either version 2.1 of  the License, or  (at your option)
# any later version.
# 
# This software is  distributed in the hope that it will  be useful, but WITHOUT
# ANY WARRANTY; without even the  implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the  GNU Lesser General Public License for more
# details.
# 
# You should have received a copy of the GNU Lesser General Public License along
# with this  library; if not,  write to the  Free Software Foundation,  Inc., 59
# Temple Place, Suite 330, Boston, MA 02111-1307 USA
#
################################################################################
import quex.engine.misc.error       as     error

from   StringIO import StringIO
import os

__reference_to_setup = None
def specify_setup_object(TheSetup):
    global __reference_to_setup 
    __reference_to_setup = TheSetup
    error.specify_setup_object(TheSetup)

class EndOfStreamException(Exception):
    pass

temporary_files  = []

def skip_whitespace(fh):
    def __skip_until_newline(fh):
        tmp = ""
        while tmp != '\n':
            tmp = fh.read(1)
            if tmp == "": raise EndOfStreamException()

    while 1 + 1 == 2:
        tmp = fh.read(1)

        if   tmp.isspace(): continue
        elif tmp == "": raise EndOfStreamException()

        # -- character was not a whitespace character
        #    => is there a '//' or a '/*' -comment ?
        tmp2 = fh.read(1)
        if tmp2 == "": fh.seek(-1, 1); return

        tmp += tmp2
        if tmp == "//":   
            __skip_until_newline(fh)

        elif tmp == "/*":
            # skip until '*/'
            previous = " "
            while tmp != "":
                tmp = fh.read(1)
                if tmp == "": raise EndOfStreamException()
                if previous + tmp == "*/":
                    break
                previous = tmp
        else:
            # no whitespace, no comment --> real character
            fh.seek(-2, 1)
            return                

def is_identifier_start(character):
    if len(character) != 1:
        # It is theoretically possible that a character > 0x10000 arrives on a python
        # narrow build.
        error.log("The underlying python build cannot handle character '%s'." % character)

    char_value = ord(character)
    return    character == "_" \
           or (char_value >= ord('a') and char_value <= ord('z')) \
           or (char_value >= ord('A') and char_value <= ord('Z'))

def is_identifier_continue(character):
    if len(character) != 1:
        # It is theoretically possible that a character > 0x10000 arrives on a python
        # narrow build.
        error.log("The underlying python build cannot handle character '%s'." % character)

    char_value = ord(character)
    return    is_identifier_start(character) \
           or (char_value >= ord('0') and char_value <= ord('9'))

def is_identifier(identifier, TolerantF=False):
    if identifier == "": return False

    if TolerantF:
        # Do not require 'identifier start'
        if not is_identifier_continue(identifier[0]): return False
    else:
        if not is_identifier_start(identifier[0]): return False

    if len(identifier) == 1: return True

    for letter in identifier[1:]:
        if is_identifier_continue(letter) == False: return False

    return True

def read_identifier(fh, TolerantF=False, OnMissingStr=None):
    def __read(fh, TolerantF):
        txt = fh.read(1)
        if len(txt) == 0: return ""

        if TolerantF:
            if is_identifier_continue(txt) == False: fh.seek(-1, 1); return ""
        else:
            if is_identifier_start(txt) == False: fh.seek(-1, 1); return ""

        while 1 + 1 == 2:
            tmp = fh.read(1)
            if len(tmp) == 0: return txt

            if is_identifier_continue(tmp): txt += tmp
            else:                           fh.seek(-1, 1); return txt
    result = __read(fh, TolerantF)

    if len(result) == 0 and OnMissingStr is not None: 
        error.log(OnMissingStr, fh)
    return result

def find_end_of_identifier(Txt, StartIdx, L):
    for i in range(StartIdx, L):
        if not is_identifier_continue(Txt[i]): return i
    else:
        return L

def check_or_die(fh, What, Comment = "."):
    if not check(fh, What):
        error.log("Missing '%s'" % What + Comment, fh)

def parse_identifier_assignment(fh):
    # NOTE: Catching of EOF happens in caller
    check_or_die(fh, "=", " for assignment")

    skip_whitespace(fh)
    identifier = read_identifier(fh)

    check_or_die(fh, ";", " for assignment. Since quex version 0.33.5 this is required.")

    return identifier.strip()

def read_namespaced_name(FileHandle_or_String, Meaning, AllowEmptyF=False):
    string_f = False
    if isinstance(FileHandle_or_String, (str, unicode)):
        fh = StringIO(FileHandle_or_String)
        string_f = True
    else:
        fh = FileHandle_or_String

    # Overstep a starting '::' if present.
    if fh.read(2) != "::": fh.seek(-2, 1)

    # Parsing the namespace definition
    try:
        name_list  = [""]   # Signalize Empty by <<name_list[-1] == "">>
        while 1 + 1 == 2:
            skip_whitespace(fh)
            name = read_identifier(fh)
            if name == "": break

            name_list[-1] = name
            if not check(fh, "::"): break
            name_list.append("")
    except:
        pass

    # Error Check
    if len(name_list[-1]) == 0: # Empty, or last name missing?
        if not AllowEmptyF: 
            if string_f: fh = -1
            error.log("Missing identifier in %s name specification." % Meaning, fh)

    if string_f: 
        trailing_chars = fh.read()
        if len(trailing_chars) != 0:
            error.log("Trailing characters '%s' in '%s'\nfor %s name specification." % \
                      (trailing_chars, FileHandle_or_String, Meaning))

    return name_list[-1], name_list[:-1], reduce(lambda x, y: x + "_" + y, name_list)  

def get_text_line_n(Txt, Pos):
    line_n = 1
    for i in range(0, Pos):
        if Txt[i] == "\n": line_n += 1
    return line_n

def delete_comment(Content, Opener, Closer, LeaveNewlineDelimiter=False):
    # delete comment lines in C++ form
    new_content = ""
    prev_i      = 0
    while 1 + 1 == 2:
        i = Content.find(Opener, prev_i)
        if i == -1: 
            new_content += Content[prev_i:]
            break
        new_content += Content[prev_i:i]
        prev_i = i + len(Opener)
        i = Content.find(Closer, prev_i)
        if i == -1: break
        if LeaveNewlineDelimiter:
            new_content += "\n" * Content[prev_i:i+len(Closer)].count("\n")
        prev_i = i + len(Closer)

    return new_content

def get_number_base(fh):
    """Checks on the prefix of a number and determines the number base. When
    this function is done, 'fh' points to the beginning of the number to be
    parsed.

    RETURNS: [0] base
             [1] digits which are available in that codec
    """
    pos = fh.tell()
    decimal_digits = "0123456789"

    first = fh.read(1)
    if first == "" or first.isdigit() == False: 
        fh.seek(pos)
        return None, None
    elif first != "0":
        fh.seek(pos)
        return 10, decimal_digits

    second = fh.read(1)
    if second == "":  
        fh.seek(pos) 
        return 10, decimal_digits
    elif second.isdigit():
        fh.seek(-1, 1)
        return 10, decimal_digits
    elif second == ".":
        error.log("Decimal integer number cannot contain '.'.", fh)
    elif not second.isalpha():
        fh.seek(pos)
        return 10, decimal_digits

    try:
        return {
            "x": (16,       "0123456789abcdefABCDEF."),  #  '.' is allowed and means nothing
            "o": (8,        "01234567."),                #  '.' is allowed and means nothing
            "b": (2,        "01."),                      #  '.' is allowed and means nothing
            "r": ("roman",  "MCDXLIVmcdxliv"),
            "n": ("Napier", "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        }[second]
    except:
        error.log("Number format '0%s' is not supported by quex.\n" % second + \
                  "Use prefix '0x' for hexadecimal numbers.\n" + \
                  "           '0o' for octal numbers.\n"       + \
                  "           '0b' for binary numbers.\n"      + \
                  "           '0r' for roman numbers.\n"      + \
                  "           '0n' for Napier numbers.\n"      + \
                  "           and no prefix for decimal numbers.", fh)

def read_integer(fh):
    pos = fh.tell()

    base, digit_list = get_number_base(fh)
    if base is None: return None

    txt = ""
    while 1 + 1 == 2:
        tmp = fh.read(1)
        if   tmp == "": break
        elif tmp not in digit_list: fh.seek(-1, 1); break
        txt += tmp

    # If we drop out on a digit, then let us assume that the user just missed a point
    if tmp.isdigit() or (tmp in list("ABCDEFabcdef")):
        error.log("Digit '%s' cannot be part of an expression of base %s." % (tmp, base), fh)

    txt = txt.replace(".", "")
    if len(txt) == 0:
        if base in [2, 8, 16, "roman", "Napier"]:
            error.log("Missing digits after for number of base %s, found '%s'." % (str(base), tmp), fh)
        fh.seek(pos)
        return None

    # Octal, decimal, and hexadecimal numbers
    if   base in [2, 8, 10, 16]: return int(txt, base)
    elif base == "roman":        return __roman_number(txt, fh)
    elif base == "Napier":       return __napier_number(txt, fh)
    else:                        return __binary_number(txt, fh)

def __binary_number(Text, fh):
    result = 0
    for letter in Text:
        result <<= 1
        if letter == "1": result |= 1
    return result

def __roman_number(Text, fh):
    """Source: http://code.activestate.com -- Code Recipes 
               Recipe 81611 by Paul Winkler.
    """
    input = Text.upper() 
    # map of (numeral, value, maxcount) tuples
    roman_numeral_map = (('M',  1000, None), ('CM', 900, 1),
                         ('D',  500, 1),     ('CD', 400, 1),
                         ('C',  100, 3),     ('XC', 90, 1),
                         ('L',  50, 1),      ('XL', 40, 1),
                         ('X',  10, 3),      ('IX', 9, 1),
                         ('V',  5, 1),       ('IV', 4, 1), 
                         ('I',  1, 3))

    result, index = 0, 0
    for numeral, value, maxcount in roman_numeral_map:
        count = 0
        while input[index: index + len(numeral)] == numeral:
            count += 1 # how many of this numeral we have
            if maxcount is not None and count > maxcount:
                error.log("input 0r%s is not a valid roman numeral." % Text, fh)
            result += value
            index  += len(numeral)

    if index < len(input): # There are characters unaccounted for.
        error.log("input 0r%s is not a valid roman numeral." % Text, fh)

    return result

def __napier_number(Text, fh):
    def value(C): 
        Cv = ord(C)
        if Cv >= ord('a') and Cv <= ord('z'):
            return Cv - ord('a')
        elif Cv >= ord('A') and Cv <= ord('Z'):
            return Cv - ord('A')
        else:
            # Before this function is called, it must be safe to assume that 
            # the text contains a valid Napier number.
            assert False

    return sum(2**value(c) for c in Text)

def extract_identifiers_with_specific_prefix(Content, Prefix):
    L = len(Content)
    i = 0
    finding_list = []
    while 1 + 1 == 2:
        i = Content.find(Prefix, i)
        # not found?
        if i == -1: break
        # is it inside an identifier?
        if i != 0 and is_identifier_start(Content[i-1]): i += 1; continue
        end_i = find_end_of_identifier(Content, i, L)
        finding_list.append([Content[i:end_i], get_text_line_n(Content, i)])
        i = end_i
    return finding_list

def read_until_whitespace(fh):
    txt = ""
    previous_tmp = ""
    while 1 + 1 == 2:
        tmp = fh.read(1)
        if   tmp == "": 
            if txt == "": raise EndOfStreamException()
            else:         return txt

        elif tmp.isspace():                                fh.seek(-1, 1); return txt
        elif previous_tmp == "/" and (tmp in ["*",  "/"]): fh.seek(-2, 1); return txt[:-1]
        txt += tmp
        previous_tmp = tmp

def read_until_closing_bracket(fh, Opener, Closer,
                               IgnoreRegions = [ ['"', '"'],      # strings
                                                 ['\'', '\''],    # characters
                                                 ["//", "\n"],    # c++ comments
                                                 ["/*", "*/"] ],  # c comments
                               SkipClosingDelimiterF = True):                    
    """This function does not eat the closing bracket from the stream.
    """                                                             
    # print "# # read_until_closing_bracket: ", Opener, ", ", Closer, ", ", IgnoreRegions

    open_brackets_n = 1
    backslash_f     = False
    txt     = ""
    CacheSz = max(len(Opener), len(Closer))
    if len(IgnoreRegions) != 0: 
        # check for correct type, because this can cause terrible errors
        assert type(IgnoreRegions) == list

        for element in IgnoreRegions:                                    
            assert type(element) == list
                                                 
        CacheSz = max(map(lambda delimiter: len(delimiter[0]), IgnoreRegions) + [ CacheSz ])

    cache = ["\0"] * CacheSz

    def match_against_cache(Delimiter):
        """Determine whether the string 'Delimiter' is flood into the cache or not."""
        assert len(Delimiter) <= len(cache), \
               "error: read_until_closing_bracket() cache smaller than delimiter"

        # delimiter == "" means that it is, in fact, not a delimiter (disabled)    
        if Delimiter == "": return False
        L = len(Delimiter)
        i = -1
        for letter in Delimiter:
            i += 1
            if letter != cache[L-i-1]: return False
        return True

    # ignore_start_triggers = map(lamda x: x[0], IgnoreRegions)
    while 1 + 1 == 2:
        tmp = fh.read(1)
        txt += tmp
        cache.insert(0, tmp)  # element 0 last element flood into cache (current)
        cache.pop(-1)         # element N first element                 (oldest)

        if tmp == "":         
            raise EndOfStreamException()

        elif tmp == "\\":       
            backslash_f = not backslash_f   # every second backslash switches to 'non-escape char'
            continue

        if not backslash_f:
            if   match_against_cache(Opener):
                open_brackets_n += 1
            elif match_against_cache(Closer):
                open_brackets_n -= 1
                if open_brackets_n == 0: 
                    # stop accumulating text when the closing delimiter is reached. do not 
                    # append the closing delimiter to the text. 
                    txt = txt[:-len(Closer)]
                    break

        backslash_f = False

        for delimiter in IgnoreRegions:
            # If the start delimiter for ignored regions matches the strings recently in flooded into
            # the cache, then read until the end of the region that is to be ignored.
            if match_against_cache(delimiter[0]): 
                position = fh.tell()
                try:
                    txt += read_until_closing_bracket(fh, "", delimiter[1], IgnoreRegions=[]) 
                except:
                    fh.seek(position)
                           
                    error.log("Unbalanced '%s', reached end of file before closing '%s' was found." % \
                              (delimiter[0].replace("\n", "\\n"), delimiter[1].replace("\n", "\\n")), 
                              fh)

                txt += delimiter[1]
                # the 'ignore region info' may contain information about with what the
                # closing delimiter is to be replaced
                # flush the cache
                cache = ["\0"] * CacheSz
                break
                
    return txt

def read_until_character(fh, Character):
    """Backslash may disable terminating character."""
    backslash_n = 0
    txt         = ""

    # ignore_start_triggers = map(lamda x: x[0], IgnoreRegions)
    # TODO: incorporate "comment ignoring"
    while 1 + 1 == 2:
        tmp = fh.read(1)
        if   tmp == "": raise EndOfStreamException()
        elif tmp == "\\": backslash_n += 1
        else:
            backslash_n = 0
            if backslash_n % 2 != 1:
                if tmp == Character:
                    return txt
        txt += tmp

    return txt

def read_until_letter(fh, EndMarkers, Verbose=False):
    txt = ""
    while 1 + 1 == 2:
        tmp = fh.read(1)
        if tmp == "":   
            if Verbose: return "", -1
            else:       return ""

        if tmp in EndMarkers:
            if Verbose: return txt, EndMarkers.index(tmp)
            else:       return txt
        txt += tmp

def clean_up():
    # -- delete temporary files
    for file in temporary_files:
        os.system("rm %s" % file)

def make_safe_identifier(String, NoCodeF=True):
    txt = ""
    for letter in String:
        if len(letter) != 1:
            # It is theoretically possible that a character > 0x10000 arrives on a python
            # narrow build.
            error.log("The underlying python build cannot handle character '%s'." % letter)

        if letter.isalpha() or letter.isdigit() or letter == "_": txt += letter.upper()
        elif letter == ":":                                       txt += "_"
        elif NoCodeF:                                             txt += "_" 
        else:                                                     txt += "_x%x_" % ord(letter)
    return txt

def get_include_guard_extension(Filename):
    """Transforms the letters of a filename, so that they can appear in a C-macro."""
    return make_safe_identifier(Filename, NoCodeF=False)

def check_whitespace(fh):
    pos = fh.tell()
    skip_whitespace(fh)
    if pos == fh.tell(): return False
    else:                return True

def check(fh, Word):
    position = fh.tell()
    try:
        skip_whitespace(fh)
        dummy = fh.read(len(Word))
        if dummy == Word: return True
    except:
        pass
    fh.seek(position)
    return False

def get_integer_parameter_value(MemberName, ValueStr):
    if type(ValueStr) == int: 
        return ValueStr
    result = read_integer(StringIO(ValueStr))
    if result is None:
        error.log("Cannot convert '%s' into an integer for '%s'.\n" % (ValueStr, MemberName) + \
                  "Use prefix '0x' for hexadecimal numbers.\n" + \
                  "           '0o' for octal numbers.\n"       + \
                  "           '0b' for binary numbers.\n"      + \
                  "           '0r' for roman numbers.\n"      + \
                  "           and no prefix for decimal numbers.")
    return result

