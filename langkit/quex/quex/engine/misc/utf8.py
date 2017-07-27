import sys
from   copy import copy
import quex.engine.misc.error as error

import StringIO

utf8_char_db = {
    -sys.maxint:   "-oo",
    sys.maxint:    "oo",            
    ord(' '):      "' '",
    ord('\n'):     "'\\n'",
    ord('\t'):     "'\\t'",
    ord('\r'):     "'\\r'",
    ord('\a'):     "'\\a'",
    ord('\b'):     "'\\b'",
    ord('\f'):     "'\\f'",
    ord('\v'):     "'\\v'",
}

# The largest number which can be displayed in utf8 with this module.
UTF8_MAX     = 0x80000000L - 1

# Borders of the different ranges
UTF8_BORDERS = [ 0x80, 0x800, 0x10000, 0x00200000, 0x04000000L, 0x80000000L ] 

def unicode_to_utf8(UnicodeValue):
    if UnicodeValue < 0x80:
        return [ UnicodeValue, ]
    elif UnicodeValue < 0x800:
        # Bits: 5 + 6 
        return [ 0xC0 | ((UnicodeValue >> 6) & 0x1F),
                 0x80 | ((UnicodeValue     ) & 0x3F)]
    elif UnicodeValue < 0x10000:
        # Bits: 4 + 6 + 6
        return [ 0xE0 | ((UnicodeValue >> 12) & 0x0F),
                 0x80 | ((UnicodeValue >>  6) & 0x3F),
                 0x80 | ((UnicodeValue      ) & 0x3F)]
    elif UnicodeValue < 0x00200000:
        # Bits: 3 + 6 + 6 + 6
        return [ 0xF0 | ((UnicodeValue >> 18) & 0x07),
                 0x80 | ((UnicodeValue >> 12) & 0x3F),
                 0x80 | ((UnicodeValue >>  6) & 0x3F),
                 0x80 | ((UnicodeValue      ) & 0x3F)]
    elif UnicodeValue < 0x04000000L:
        # Bits: 2 + 6 + 6 + 6 + 6
        return [ 0xF0 | ((UnicodeValue >> 24) & 0x03),
                 0x80 | ((UnicodeValue >> 18) & 0x3F),
                 0x80 | ((UnicodeValue >> 12) & 0x3F),
                 0x80 | ((UnicodeValue >>  6) & 0x3F),
                 0x80 | ((UnicodeValue      ) & 0x3F)]
    elif UnicodeValue < 0x80000000L:
        # Bits: 1 + 6 + 6 + 6 + 6 + 6
        return [ 0xF0 | ((UnicodeValue >> 30) & 0x01),
                 0x80 | ((UnicodeValue >> 24) & 0x3F),
                 0x80 | ((UnicodeValue >> 18) & 0x3F),
                 0x80 | ((UnicodeValue >> 12) & 0x3F),
                 0x80 | ((UnicodeValue >>  6) & 0x3F),
                 0x80 | ((UnicodeValue      ) & 0x3F)]
    else:
        error.log("Unicode character 0x%8X > 0x7FFFFFFF detected. Cannot be handled." % UnicodeValue)

def unicode_to_pretty_utf8(Code):
    global utf8_char_db
    
    if   Code == -sys.maxint:         return "-oo"
    elif Code == sys.maxint:          return "oo"
    elif Code < 0 or Code > 0x10FFFF: return "<%i>" % Code

    if utf8_char_db.has_key(Code): return utf8_char_db[Code]
    elif Code < ord(' '):          return "\\" + repr(Code) #  from ' ' to '9' things are 'visible'
    else:
        char_str = "".join(["'"] + map(chr, unicode_to_utf8(Code)) + ["'"])
        return char_str

def utf8_to_unicode(ByteSequence):
    """Unfortunately, there is no elegant way to do the utf8-decoding 
       safely in libPython, since due to strange behavior of a python 
       narrow build a character >= 0x10000 may appear as a 2 byte string 
       and cannot be handled by 'ord' in python 2.x.

       Thus: 
              utf8d = codecs.getdecoder("utf-8")
              return ord(utf8d("".join(map(chr, ByteSequence)))[0])

       would be unsafe. That's why we do it by hand here
    """
    # Assume that the byte sequence is valid, thus a byte sequence of length 'N'
    # has a N - 1 leading ones in the header plus a zero. Remaining bits in the
    # header are therefore 8 - N. All other bytes in the sequence start with bits '10'
    # and contain 6 bits of useful payload.
    header_bit_n = 8 - len(ByteSequence)
    mask         = (1 << header_bit_n) - 1
    value = ByteSequence[0] & mask
    for byte in ByteSequence[1:]:
        value <<= 6
        value |=  (byte & 0x3F)   # blend off the highest two bits
        # The highest two bits in a follow byte in utf8 MUST be '10'. Thus:
        if (byte & 0xC0) != 0x80:
            error.log("Error in UTF8 encoded file. Inadmissible byte sequence detected. Found byte '%02X'" % byte)

    return value

def map_utf8_to_unicode(utf8_string_or_stream):
    """Maps the start of the 'utf8_string' to a single unicode character. 
    
       Returns: (string) the unicode of the letter and the remaining utf8-string with the first 
                         letters cut that represented the unicode letter.
                (StringIO) the unicode of the first utf8 coding in the stream. sets the stream
                           position to the first character after the utf8 code.         
    """    
    arg_type=utf8_string_or_stream.__class__.__name__
    assert arg_type in ["StringIO", "file", "str"]

    if    utf8_string_or_stream.__class__.__name__ == "StringIO" \
       or utf8_string_or_stream.__class__.__name__ == "file":
       return __read_one_utf8_code_from_stream(utf8_string_or_stream)
    elif utf8_string_or_stream.__class__.__name__ == "str":
       utf8_string = utf8_string_or_stream

    stream = StringIO.StringIO(utf8_string)
    ucs_character_code = __read_one_utf8_code_from_stream(stream)
    remainder = stream.read()  
    return ucs_character_code, remainder 
    
def map_n_utf8_to_unicode(utf8_string, N=-1, RemainderF=False):
    """Reads 'N' characters out of the string and creates an array
       of integers that represent the unicode characters contained.
       N=-1 => interpret the whole given string.

       Returns: unicode value and if RemainderF=True: remaining string.
    """   
    stream = StringIO.StringIO(utf8_string)
    result = []
    if N == 0: 
       if RemainderF: return result, utf8_string
       else:          return result

    if N != -1:
        tmp = __read_one_utf8_code_from_stream(stream)
        result.append(tmp)
        n   = 0
        while tmp is not None:
            n += 1
            if n >= N: break
            tmp = __read_one_utf8_code_from_stream(stream)
            if tmp is None: break
            result.append(tmp)

    else:
        tmp = __read_one_utf8_code_from_stream(stream)
        while tmp is not None:
            result.append(tmp)
            tmp = __read_one_utf8_code_from_stream(stream)

    if RemainderF: 
        remainder = stream.read()    
        return result, remainder
    else:          
        return result

def map_unicode_to_utf8(UCS_character_code):
    """Returns a utf8 string that correspond to the unicode character
       passed by UCS_character_code.
    """
    return "".join(map(chr, unicode_to_utf8(UCS_character_code)))

def __read_one_utf8_code_from_stream(char_stream):
    """Interprets the subsequent characters as a UTF-8 coded
    character.

    RETURNS: integer value of the unicode character.
             0xFF          in case of error.

             second parameter: True if character was backslashed,
                               False if not.
    """
    character = char_stream.read(1)
    if character == "": return None
    try:    head_char = ord(character)
    except: return None

    # (*) head characters for the the "utf-8 escape" are located in between
    #     0xC0 to 0xFD. If the chacter code falls out of this border no further
    #     consideration is required.
    if head_char < 0xC0 or head_char > 0xFD:
        return head_char
    elif  head_char > 0xFC:  # 1111.110..
        char_n = 5
    elif  head_char > 0xF8:  # 1111.10..
        char_n = 4
    elif  head_char > 0xF0:  # 1111.0..
        char_n = 3
    elif  head_char > 0xE0:  # 1110..
        char_n = 2
    elif  head_char > 0xC0:  # 110..
        char_n = 1
    
    # (*) read the bytes from the char_stream that are needed
    try:    bytes = [head_char] + map(lambda x: ord(x), char_stream.read(char_n))
    except:
        assert False, "utf8.py: could not read %i utf-follow bytes" % char_n
    
    return utf8_to_unicode(bytes)

def __bit_string_to_int(BitStr):
    """Transforms a string of the form '01001001' into an integer
    which represents this byte.

    TESTED <frs>
    """

    BitStr = BitStr.replace(".", "")
    BitArray = map(lambda x: x, BitStr)
    BitArray.reverse()

    n = 0
    sum = 0
    for bit in BitArray:
        if bit == "1": sum += 2**n
        n += 1

    return sum

def __int_to_bit_string(IntN, BitN):
    """Receives an integer and constructs a string containing 0s and 1s that 
       represent the integer binarily.
    """
    int_n = copy(IntN)

    sum = ""
    while int_n:
        if int_n & 0x1: sum = "1" + sum
        else:           sum = "0" + sum
        int_n = int_n >> 1
    
    # NOTE: this should not occur in our case (head character > 0xC0)
    if len(sum) < BitN: sum = "0" * (BitN - len(sum)) + sum
    
    return sum


