def trim(Str):
    """Deletes whitepspace borders of a string.
       
       Transforms: input  = "  hallo, du da   "
       into        output = "hallo, du da"
    """

    if Str == "": return ""
    L = len(Str)
    for i in range(L):
        if not Str[i].isspace():
            break
    else:
        # reached end of string --> empty string
        return ""

    for k in range(1, L-i+1):
        if not Str[L-k].isspace():
            break

    # note, if k = 1 then we would return Str[i:0]
    if L-i != 1:
        if k == 1:   return Str[i:]
        else:        return Str[i:-k + 1]
    else:            return Str[i:]

def blue_print(BluePrintStr, Replacements, CommonStart="$"):
    """Takes a string acting as blue print and replaces all
       replacements of the form r in Replacements:

           r[0] = original pattern
           r[1] = replacements

       Original pattern must start with '$'.
    """
    # -- sort the replacements so that long strings
    #    are replaced first
    Replacements.sort(lambda a, b: cmp(len(b[0]), len(a[0])))

    # -- the longest original
    L = len(Replacements[0][0])

    txt      = BluePrintStr
    result   = []
    prev_end = 0
    while 1 + 1 == 2:
        i = txt.find(CommonStart, prev_end)
        if i == -1: 
            result.append(txt[prev_end:])
            try:    
                return "".join(result)
            except: 
                for original, replacement in Replacements:
                    if not isinstance(replacement, (str, unicode)):
                        print "##", original, "  ->  ", repr(replacement.__class__.__name__)
                        print "##>>", replacement
                assert False


        for orig, replacement in Replacements:
            assert orig[0] == CommonStart[0]
            if txt.find(orig, i, i + L) == i: 
                result.append(txt[prev_end:i])
                result.append(replacement)
                prev_end = i + len(orig)
                break
        else:
            # Nothing matched the expression starting with '$' simply
            # continue as if nothing happend.
            result.append(txt[prev_end:i+1])
            prev_end  = i + 1
            pass

def tex_safe(Str):

    for letter in "_%&^#$":
        Str.replace(letter, "\\" + letter)

    return Str

def safe_string(String):
    def get(Letter):
        if Letter in ['\\', '"', '\n', '\t', '\r', '\a', '\v']: return "\\" + Letter
        else:                                                   return Letter 

    return "".join(get(letter) for letter in String)

def pretty_code(Code, Base=4):
    """-- Delete empty lines at the beginning
       -- Delete empty lines at the end
       -- Strip whitespace after last non-whitespace
       -- Propper Indendation based on Indentation Counts

       Base = Min. Indentation
    """
    class Info:
        def __init__(self, IndentationN, Content):
            self.indentation = IndentationN
            self.content     = Content
    info_list           = []
    no_real_line_yet_f  = True
    indentation_set     = set()
    for element in Code:
        for line in element.splitlines():
            line = line.rstrip() # Remove trailing whitespace
            if len(line) == 0 and no_real_line_yet_f: continue
            else:                                     no_real_line_yet_f = False

            content     = line.lstrip()
            if len(content) != 0 and content[0] == "#": indentation = 0
            else:                                       indentation = len(line) - len(content) + Base
            info_list.append(Info(indentation, content))
            indentation_set.add(indentation)

    # Discretize indentation levels
    indentation_list = list(indentation_set)
    indentation_list.sort()

    # Collect the result
    result              = []
    # Reverse so that trailing empty lines are deleted
    no_real_line_yet_f  = True
    for info in reversed(info_list):
        if len(info.content) == 0 and no_real_line_yet_f: continue
        else:                                             no_real_line_yet_f = False
        indentation_level = indentation_list.index(info.indentation)
        result.append("%s%s\n" % ("    " * indentation_level, info.content))

    return "".join(reversed(result))

