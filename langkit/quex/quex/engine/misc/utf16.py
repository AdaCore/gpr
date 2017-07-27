# import codecs

# utf16c = codecs.getencoder("utf-16be")

def unicode_to_utf16(UnicodeValue):
    """Do not do this by hand in order to have a 'reference' to double check
       whether otherwise hand coded values are correct.
    """
    if UnicodeValue < 0x10000:
        return [UnicodeValue,]
    else:
        UnicodeValue -= 0x10000
        x0 = (UnicodeValue >> 10)   | 0xD800
        x1 = (UnicodeValue & 0x3FF) | 0xDC00
        return [x0, x1]

def utf16_to_unicode(WordSeq):
    if len(WordSeq) == 1: return WordSeq[0]

    x0 = WordSeq[0] - 0xD800
    x1 = WordSeq[1] - 0xDC00

    return (x0 << 10) + x1 + 0x10000
