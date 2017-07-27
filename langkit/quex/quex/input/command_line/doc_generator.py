#Command Line Options
#====================
import os
import sys
import re
sys.path.insert(0, os.environ["QUEX_PATH"])

from   quex.input.setup import SETUP_INFO, SetupParTypes

class SectionHeader:
    def __init__(self, Title): self.title = Title
    def do(self, visitor):     return visitor.do_SectionHeader(self.title)

class Text:
    def __init__(self, Text): self.text = Text
    def do(self, visitor):    return visitor.do_Text(self.text)

class Note:
    def __init__(self, *Content): self.content_list = list(Content)
    def do(self, visitor):        return visitor.do_Note(self.content_list)

class Block:
    def __init__(self, Content, Language="plain"):
        self.content  = Content
        self.language = Language
    def do(self, visitor): return visitor.do_Block(self.content, self.language)

class Option:
    def __init__(self, Name, CallStr, *Paragraphs):
        self.name           = Name
        self.call_str       = CallStr
        self.paragraph_list = list(Paragraphs)
    def do(self, visitor):
        info = SETUP_INFO[self.name]
        option_list = info[0]
        default     = info[1]
        return visitor.do_Option(option_list, self.call_str, default, self.paragraph_list)

class Item:
    def __init__(self, Name, *Paragraphs):
        self.name           = Name
        self.paragraph_list = list(Paragraphs)
    def do(self, visitor):
        return visitor.do_Item(self.name, self.paragraph_list)

class List:
    def __init__(self, *Items):
        self.item_list = list(Items)
        
    def do(self, visitor):
        return visitor.do_List(self.item_list)

class Visitor:
    re_verbatim0 = re.compile("\\\\v{([^}]+)}([.,:!?])\s*")
    re_verbatim  = re.compile("\\\\v{([^}]+)}\s*")

    def __init__(self):
        self.nesting_level = -1

    def nesting_indent(self):
        return "    " * self.nesting_level

    def do(self, DescriptionList):
        def adapt(X):
            if isinstance(X, (str, unicode)): return Text(X)
            else:                             return X
        self.nesting_level += 1
        adapted = [ adapt(x) for x in DescriptionList ]
        result  = [ x.do(self) for x in adapted ]
        self.nesting_level -= 1
        return "".join(result)

    def do_SectionHeader(self, Title): assert False
    def do_Note(self, ContentList):          assert False
    def do_Text(self, Text):          assert False
    def do_Block(self, Content, Language):      assert False
    def do_Option(self, OptionList, CallStr, Default, ParagraphList):        assert False
    def do_Item(self, Name, ParagraphList):          assert False
    def do_List(self, ItemtList):  assert False

    def format_default(self, Default):
        if Default == "": return None

        if Default in SetupParTypes:
            if   Default == SetupParTypes.FLAG: 
                return "false (disabled)"
            elif Default == SetupParTypes.NEGATED_FLAG:
                return "true (not disabled)"
            elif Default == SetupParTypes.LIST:
                return "empty list"
            elif Default == SetupParTypes.INT_LIST:
                return "empty list"
            elif Default == SetupParTypes.OPTIONAL_STRING:
                return "empty string"
            elif Default == "":
                return "undefined"
            else:
                assert False, "%s" % Default
        return "%s" % Default

    def format_block(self, Text):
        self.nesting_level += 1
        def get_indentation(Line):
            i = 0
            for i, letter in enumerate(Line):
                if not letter.isspace(): break
            return i
                
        min_indentation = 1e37
        line_list       = []
        for line in Text.splitlines():
            line = line.replace("\t", "    ")
            # measure indentation
            indentation = get_indentation(line)
            if indentation < min_indentation: min_indentation = indentation
            line_list.append((indentation, line.strip()))

        # Indent everything starting with an indentation of 'Indentation'
        curr_indentation = 4 * self.nesting_level
        self.nesting_level -= 1
        return "".join(
            " " * (indentation - min_indentation + curr_indentation) + "%s\n" % line
            for indentation, line in line_list
        )

    def format_text(self, Text):
        """Separates paragraphs and reformats them so that they are
        properly indented.
        """
        def append(pl, p):
            if p: pl.append("".join(p))

        def clean(line):
            """Delete any prepending or trailing whitespace, let the only
            whitespace be ' '. Let all whitespace between words be ' '.
            """
            result = line.strip()
            result = result.replace("\n", " ")
            result = result.replace("\t", " ")
            while result.find("  ") != -1:
                result = result.replace("  ", " ")
            return result

        def add_line(p, line):
            p.append("%s " % clean(line))

        paragraph_list = []
        paragraph      = []
        for line in Text.splitlines():
            line = line.strip()
            if len(line) == 0: 
                append(paragraph_list, paragraph)
                paragraph = []
            else:              
                add_line(paragraph, line)

        append(paragraph_list, paragraph)
        return [
            self.__format_text(p)
            for p in paragraph_list
        ]
                
    def __format_text(self, X):
        """Replace text formatting markers by the language dependent
        markers. For that the following replacement strings must be
        defined.

        .re_verbatim_replace --> print in 're_verbatim_replace' for variable and function 
                      names.
        """
        assert X.find("\n") == -1
        result = X
        result = Visitor.re_verbatim0.sub(self.re_verbatim_replace0, result)
        result = Visitor.re_verbatim.sub(self.re_verbatim_replace, result)
        return result

class VisitorSphinx(Visitor):
    """Produces code for 'sphinx documentation system'
    """
    re_verbatim_replace0 = "``\\1``\\2 "
    re_verbatim_replace  = "``\\1`` "
    def __init__(self):
        Visitor.__init__(self)

    def do_SectionHeader(self, Title):
        return "%s\n%s\n\n" % (Title, "=" * len(Title))

    def do_Text(self, Text):          
        width  = 4 * self.nesting_level
        indent = self.nesting_indent()
        text   = [ indent ]
        for paragraph in self.format_text(Text):
            for word in paragraph.split(" "):
                text.append("%s " % word)
                if len(word) + width > 80:
                    text.append("\n")
                    text.append(indent)
                    width = 4 * self.nesting_level
                width += len(word)
            text.append("\n\n%s" % indent)
        return "".join(text)

    def do_Note(self, ContentList):
        self.nesting_level += 1
        content = self.do(ContentList)
        self.nesting_level -= 1
        return "%s.. note::\n\n%s\n" % (self.nesting_indent(), content)

    def do_Block(self, Content, Language):
        self.nesting_level += 1
        block = self.format_block(Content)
        self.nesting_level -= 1
        return "%s.. code-block:: %s\n\n%s\n\n" % (self.nesting_indent(), Language, block)

    def do_Option(self, OptionList, CallStr, Default, ParagraphList):
        options_str = reduce(lambda a, b: "%s, %s" % (a, b), OptionList)
        content     = self.do(ParagraphList)
        default     = self.format_default(Default)
        if default is not None: default = "Default: %s\n\n" % default
        else:                   default = ""
        if CallStr is not None: call_str = CallStr
        else:                   call_str = ""
        return "%s.. cmdoption:: %s %s\n\n%s\n\n%s" \
               % (self.nesting_indent(), options_str, call_str, content, default)

    def do_Item(self, Name, ParagraphList):
        content = self.do(ParagraphList)
        return "\n%s.. describe:: %s\n\n%s\n" % (self.nesting_indent(), Name, content)

    def do_List(self, ItemtList):
        content = "".join(
            "    %s* %s\n" % (self.nesting_indent(), "".join(self.format_text(content)))
            for content in ItemtList
        )
        # First '\n' is to annulate previous indentations
        return "\n%s\n" % content

class VisitorManPage(Visitor):
    re_verbatim_replace0 = "\n.BR \"\\1\" \\2\n"
    re_verbatim_replace  = "\n.B \"\\1\"\n"
    def __init__(self):
        Visitor.__init__(self)

    def do_SectionHeader(self, Title):
        return ".SS %s\n\n" % Title

    def do_Text(self, Text):          
        text   = [ ]
        for paragraph in self.format_text(Text):
            text.append(paragraph)
            text.append("\n\n")
        return "".join(text)

    def do_Note(self, ContentList):
        return self.do(ContentList)

    def do_Block(self, Content, Language):
        self.nesting_level += 1
        block = self.format_block(Content)
        self.nesting_level -= 1
        return "\n.nf\n\n%s\n\n\n.fi\n" % block

    def do_Option(self, OptionList, CallStr, Default, ParagraphList):
        options_str = reduce(lambda a, b: "%s, %s" % (a, b), OptionList)
        #options_str = options_str.replace("--", "\\-\\^\\-")
        #options_str = options_str.replace("-", "\\-")
        content     = self.do(ParagraphList)
        default     = self.format_default(Default)
        if default is not None: default = "\.RS\nDefault: %s\n.RE\n\n" % default
        else:                   default = ""
        if CallStr is not None: call_str = CallStr
        else:                   call_str = ""
        return ".TP\n.BI \"%s %s\"\n\n.RS\n%s\n.RE\n\n%s\n" \
               % (options_str, call_str, content, default)

    def do_Item(self, Name, ParagraphList):
        content = self.do(ParagraphList)
        if content and content[0] != "\n": 
            content = "\n%s" % content
        return "\n.RS\n.IP %s%s\n.RE\n" % (Name, content)

    def do_List(self, ItemtList):
        content = "".join(
            " * %s\n" % "".join(self.format_text(content))
            for content in ItemtList
        )
        # First '\n' is to annulate previous indentations
        return "\n.Bl -bullet\n%s\n.El\n" % content


