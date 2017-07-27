import quex.engine.misc.error          as     error
from   quex.engine.misc.file_in        import EndOfStreamException, \
                                              skip_whitespace, \
                                              check_or_die, \
                                              read_identifier, \
                                              read_namespaced_name, \
                                              check, \
                                              read_until_letter
from   quex.input.code.core            import CodeUser, \
                                              CodeUser_NULL
from   quex.input.code.base            import SourceRef, \
                                              CodeFragment, \
                                              SourceRef_VOID
import quex.input.files.code_fragment  as     code_fragment
from   quex.input.setup                import E_Files, \
                                              NotificationDB
from   quex.blackboard                 import setup as Setup, \
                                              Lng

token_type_code_fragment_db = { 
        "constructor":    CodeUser_NULL, 
        "destructor":     CodeUser_NULL,
        "copy":           None, 
        "body":           CodeUser_NULL,
        "header":         CodeUser_NULL,
        "footer":         CodeUser_NULL,
        "take_text":      None,
        "repetition_set": CodeUser_NULL,
        "repetition_get": CodeUser_NULL,
        }

class TokenTypeDescriptorCore:
    """Object used during the generation of the TokenTypeDescriptor."""
    def __init__(self, Core=None):
        if Core is None:
            self._file_name                = Setup.output_token_class_file
            self._file_name_implementation = Setup.output_token_class_file_implementation
            if Setup.token_class_name.find("::") != -1:
                Setup.token_class_name,       \
                Setup.token_class_name_space, \
                Setup.token_class_name_safe = \
                        read_namespaced_name(Setup.token_class_name, 
                                             "token class (options --token-class, --tc)")
            self.class_name            = Setup.token_class_name
            self.class_name_safe       = Setup.token_class_name_safe
            self.name_space            = Setup.token_class_name_space
            self.open_for_derivation_f      = False
            self.token_contains_token_id_f  = True
            self.token_id_type         = CodeUser("size_t", SourceRef())
            self.column_number_type    = CodeUser("size_t", SourceRef())
            self.line_number_type      = CodeUser("size_t", SourceRef())

            self.distinct_db = {}
            self.union_db    = {}

            for name, default_value in token_type_code_fragment_db.iteritems():
                self.__dict__[name] = default_value

        else:
            self._file_name                = Core._file_name
            self._file_name_implementation = Core._file_name_implementation
            self.class_name            = Core.class_name
            self.class_name_safe       = Core.class_name_safe
            self.name_space            = Core.name_space
            self.open_for_derivation_f      = Core.open_for_derivation_f
            self.token_contains_token_id_f  = Core.token_contains_token_id_f
            self.token_id_type         = Core.token_id_type
            self.column_number_type    = Core.column_number_type
            self.line_number_type      = Core.line_number_type

            self.distinct_db           = Core.distinct_db
            self.union_db              = Core.union_db

            for name in token_type_code_fragment_db.keys():
                self.__dict__[name] = Core.__dict__[name]
            
    def set_file_name(self, FileName):
        self._file_name = FileName
        ext = Lng[Setup.language].extension_db[Setup.output_file_naming_scheme][E_Files.HEADER_IMPLEMTATION]
        self._file_name_implementation = FileName + ext

    def __repr__(self):
        txt = ""
        if self._file_name != "": 
            txt += "file name: '%s'\n" % self._file_name
        txt += "class:     '%s'\n" % self.class_name
        if self.open_for_derivation_f: 
            txt += "           (with virtual destructor)\n"
        if self.token_contains_token_id_f == False:
            txt += "           (token id not part of token object)\n"
        txt += "namespace: '%s'\n" % repr(self.name_space)[1:-1]
        txt += "type(token_id)      = %s\n" % self.token_id_type.get_text()
        txt += "type(column_number) = %s\n" % self.column_number_type.get_text()
        txt += "type(line_number)   = %s\n" % self.line_number_type.get_text()

        txt += "distinct members {\n"
        # '0' to make sure, that it works on an empty sequence too.
        L = self.distinct_members_type_name_length_max()
        for name, type_code in self.distinct_db.items():
            txt += "    %s%s %s\n" % (type_code.get_text(), " " * (L - len(type_code.get_text())), name)
        txt += "}\n"
        txt += "union members {\n"

        # '0' to make sure, that it works on an empty sequence too.
        L = self.union_members_type_name_length_max()
        for name, type_descr in self.union_db.items():
            if type(type_descr) == dict:
                txt += "    {\n"
                for sub_name, sub_type in type_descr.items():
                    txt += "        %s%s %s\n" % \
                           (sub_type.get_text(), 
                            " " * (L - len(sub_type.get_text())-4), 
                            sub_name)
                txt += "    }\n"
            else:
                txt += "    %s%s %s\n" % \
                       (type_descr.get_text(), 
                        " " * (L - len(type_descr.get_text())), 
                        name)
        txt += "}\n"

        # constructor / copy / destructor
        if not self.constructor.is_whitespace():
            txt += "constructor {\n"
            txt += Lng.SOURCE_REFERENCED(self.constructor)
            txt += "}"
        
        if self.copy is not None:
            txt += "copy {\n"
            txt += Lng.SOURCE_REFERENCED(self.copy)
            txt += "}"

        if not self.destructor.is_whitespace():
            txt += "destructor {\n"
            txt += Lng.SOURCE_REFERENCED(self.destructor)
            txt += "}"

        if not self.body.is_whitespace():
            txt += "body {\n"
            txt += Lng.SOURCE_REFERENCED(self.body)
            txt += "}"

        return txt

    def manually_written(self):
        return False

class TokenTypeDescriptor(TokenTypeDescriptorCore):
    """The final product."""
    def __init__(self, Core, SourceReference=SourceRef_VOID):
        assert isinstance(Core, TokenTypeDescriptorCore)
        TokenTypeDescriptorCore.__init__(self, Core)

        self.sr = SourceReference 

        # (*) Max length of variables etc. for pretty printing
        max_length = 0
        for type_descr in self.union_db.values():
            if type(type_descr) == dict:
                length = 4 + max([0] + map(lambda x: len(x.get_text()), type_descr.values()))
            else:
                length = len(type_descr.get_text())
            if length > max_length: max_length = length
        self.__union_members_type_name_length_max = max_length

        max_length = 0
        for name, type_descr in self.union_db.items():
            if type(type_descr) == dict:
                length = 4 + max([0] + map(lambda x: len(x), type_descr.keys()))
            else:
                length = len(name)
            if length > max_length: max_length = length
        self.__union_members_variable_name_length_max = max_length

        # 
        self.__distinct_members_type_name_length_max = \
               max([0] + map(lambda x: len(x.get_text()), self.distinct_db.values()))
        self.__distinct_members_variable_name_length_max = \
               max([0] + map(lambda x: len(x), self.distinct_db.keys()))
        self.__type_name_length_max = \
               max(self.__distinct_members_type_name_length_max,
                   self.__union_members_type_name_length_max)
        self.__variable_name_length_max = \
               max(self.__distinct_members_variable_name_length_max,
                   self.__union_members_variable_name_length_max)

        # (*) Member DB: [member name] --> [type info, access info]
        db = {}
        for name, type_code in self.distinct_db.items():
            db[name] = [type_code, name]
        for name, type_descr in self.union_db.items():
            if type(type_descr) == dict:
                for sub_name, sub_type in type_descr.items():
                    db[sub_name] = [sub_type, "content." + name + "." + sub_name]
            else:
                db[name] = [type_descr, "content." + name]
        self.__member_db = db

    def get_file_name(self):
        return self._file_name

    def get_file_name_implementation(self):
        return self._file_name_implementation

    def type_name_length_max(self):
        return self.__type_name_length_max

    def variable_name_length_max(self):
        return self.__variable_name_length_max

    def distinct_members_type_name_length_max(self):
        return self.__distinct_members_type_name_length_max

    def distinct_members_variable_name_length_max(self):
        return self.__distinct_members_variable_name_length_max

    def union_members_type_name_length_max(self):
        return self.__union_members_type_name_length_max

    def union_members_variable_name_length_max(self):
        return self.__union_members_variable_name_length_max

    def has_member(self, MemberName):
        return self.__member_db.has_key(MemberName)

    def get_member_db(self):
        return self.__member_db

    def get_member_access(self, MemberName):
        assert self.__member_db.has_key(MemberName), \
               "Member database does not provide member name '%s'.\n" % MemberName + \
               "Available: " + repr(self.__member_db.keys())
        return self.__member_db[MemberName][1]

    def consistency_check(self):
        # Is 'take_text' section defined
        if self.take_text is not None: return

        error.warning(_warning_msg, self.sr,
                      SuppressCode=NotificationDB.warning_on_no_token_class_take_text)

class TokenTypeDescriptorManual:
    """Class to mimik as 'real' TokenTypeDescriptor as defined in 
       quex.input.files.token_type.py. Names and functions must remain
       as they are for compatibility.
    """
    def __init__(self, FileName, ClassName, NameSpace, ClassNameSafe, TokenIDType):
        self.__file_name       = FileName
        self.class_name        = ClassName
        self.name_space        = NameSpace
        self.class_name_safe   = ClassNameSafe

        self.column_number_type = CodeFragment("size_t")
        self.line_number_type   = CodeFragment("size_t")
        self.token_id_type      = CodeFragment(TokenIDType)

    def get_file_name(self):
        return self.__file_name

    def manually_written(self):
        return True

TokenType_StandardMemberList = ["column_number", "line_number", "id"]

__data_name_index_counter = -1
def data_name_index_counter_get():
    global __data_name_index_counter
    __data_name_index_counter += 1
    return __data_name_index_counter

def parse(fh):
    descriptor = TokenTypeDescriptorCore()

    if not check(fh, "{"):
        error.log("Missing opening '{' at begin of token_type definition", fh)

    already_defined_list = []
    position             = fh.tell()
    sr_begin             = SourceRef.from_FileHandle(fh)
    result               = True
    while result == True:
        try: 
            # x = fh.tell(); fh.seek(x)
            result = parse_section(fh, descriptor, already_defined_list)
        except EndOfStreamException:
            fh.seek(position)
            error.error_eof("token_type", fh)

        
    if not check(fh, "}"):
        fh.seek(position)
        error.log("Missing closing '}' at end of token_type definition.", fh);

    result = TokenTypeDescriptor(descriptor, sr_begin)
    if     len(result.get_member_db()) == 0       \
       and result.class_name == "Token"           \
       and result.token_id_type.sr.is_void()      \
       and result.column_number_type.sr.is_void() \
       and result.line_number_type.sr.is_void():
        error.log("Section 'token_type' does not define any members, does not\n" + \
                  "modify any standard member types, nor does it define a class\n" + \
                  "different from 'Token'.", fh)

    result.consistency_check()
    return result

def parse_section(fh, descriptor, already_defined_list):
    pos = fh.tell()
    try: 
        return __parse_section(fh, descriptor, already_defined_list)
    except EndOfStreamException:
        fh.seek(pos)
        error.error_eof("token_type", fh)

def __parse_section(fh, descriptor, already_defined_list):
    global token_type_code_fragment_db
    assert type(already_defined_list) == list

    SubsectionList = ["name", "file_name", "standard", "distinct", "union", "inheritable", "noid"] \
                      + token_type_code_fragment_db.keys()

    position = fh.tell()
    skip_whitespace(fh)
    word = read_identifier(fh)
    if word == "":
        fh.seek(position)
        if check(fh, "}"): 
            fh.seek(position) 
            return False
        error.log("Missing token_type section ('standard', 'distinct', or 'union').", fh)

    error.verify_word_in_list(word, SubsectionList, 
                        "Subsection '%s' not allowed in token_type section." % word, 
                        fh)

    if word == "name":
        if not check(fh, "="):
            error.log("Missing '=' in token_type 'name' specification.", fh)
        descriptor.class_name, descriptor.name_space, descriptor.class_name_safe = read_namespaced_name(fh, "token_type")
        if not check(fh, ";"):
            error.log("Missing terminating ';' in token_type 'name' specification.", fh)

    elif word == "inheritable":
        descriptor.open_for_derivation_f = True
        check_or_die(fh, ";")

    elif word == "noid":
        descriptor.token_contains_token_id_f = False;
        check_or_die(fh, ";")

    elif word == "file_name":
        if not check(fh, "="):
            error.log("Missing '=' in token_type 'file_name' specification.", fh)
        descriptor.set_file_name(read_until_letter(fh, ";"))
        if not check(fh, ";"):
            error.log("Missing terminating ';' in token_type 'file_name' specification.", fh)

    elif word in ["standard", "distinct", "union"]:
        if   word == "standard": parse_standard_members(fh, word, descriptor, already_defined_list)
        elif word == "distinct": parse_distinct_members(fh, word, descriptor, already_defined_list)
        elif word == "union":    parse_union_members(fh, word, descriptor, already_defined_list)

        if not check(fh, "}"):
            fh.seek(position)
            error.log("Missing closing '}' at end of token_type section '%s'." % word, fh);

    elif word in token_type_code_fragment_db.keys():
        fragment     = code_fragment.parse(fh, word, AllowBriefTokenSenderF=False)        
        descriptor.__dict__[word] = fragment

    else: 
        assert False, "This code section section should not be reachable because 'word'\n" + \
                      "was checked to fit in one of the 'elif' cases."

    return True
            
def parse_standard_members(fh, section_name, descriptor, already_defined_list):
    if not check(fh, "{"):
        error.log("Missing opening '{' at begin of token_type section '%s'." % section_name, fh);

    position = fh.tell()

    while 1 + 1 == 2:
        try: 
            result = parse_variable_definition(fh) 
        except EndOfStreamException:
            fh.seek(position)
            error.error_eof("standard", fh)

        if result is None: return
        type_code_fragment, name = result[0], result[1]

        __validate_definition(type_code_fragment, name,
                              already_defined_list, StandardMembersF=True)

        if   name == "id":            descriptor.token_id_type      = type_code_fragment
        elif name == "column_number": descriptor.column_number_type = type_code_fragment
        elif name == "line_number":   descriptor.line_number_type   = type_code_fragment
        else:
            assert False # This should have been caught by the variable parser function

        already_defined_list.append([name, type_code_fragment])

def parse_distinct_members(fh, section_name, descriptor, already_defined_list):
    if not check(fh, "{"):
        error.log("Missing opening '{' at begin of token_type section '%s'." % section_name, fh);

    result = parse_variable_definition_list(fh, "distinct", already_defined_list)
    if result == {}: 
        error.log("Missing variable definition in token_type 'distinct' section.", fh)
    descriptor.distinct_db = result

def parse_union_members(fh, section_name, descriptor, already_defined_list):
    if not check(fh, "{"):
        error.log("Missing opening '{' at begin of token_type section '%s'." % section_name, fh);

    result = parse_variable_definition_list(fh, "union", already_defined_list, 
                                                         GroupF=True)
    if result == {}: 
        error.log("Missing variable definition in token_type 'union' section.", fh)
    descriptor.union_db = result

def parse_variable_definition_list(fh, SectionName, already_defined_list, GroupF=False):
    position = fh.tell()

    db = {}
    while 1 + 1 == 2:
        try: 
            result = parse_variable_definition(fh, GroupF=True, already_defined_list=already_defined_list) 
        except EndOfStreamException:
            fh.seek(position)
            error.error_eof(SectionName, fh)

        if result is None: 
            return db

        # The type_descriptor can be:
        #  -- a UserCodeFragment with a string of the type
        #  -- a dictionary that contains the combined variable definitions.
        type_descriptor = result[0]

        # If only one argument was returned it was a 'struct' that requires
        # an implicit definition of the struct that combines the variables.
        if len(result) == 1: name = "data_" + repr(data_name_index_counter_get())
        else:                name = result[1]

        db[name] = type_descriptor

        if len(result) == 1:
            assert type(type_descriptor) == dict
            # In case of a 'combined' definition each variable needs to be validated.
            for sub_name, sub_type in type_descriptor.items():
                __validate_definition(sub_type, sub_type, already_defined_list, 
                                      StandardMembersF=False)

                already_defined_list.append([sub_name, sub_type])
        else:
            assert type_descriptor.__class__ == CodeUser
            __validate_definition(type_descriptor, name, already_defined_list, 
                                  StandardMembersF=False)
            already_defined_list.append([name, type_descriptor])

def parse_variable_definition(fh, GroupF=False, already_defined_list=[]):
    """PURPOSE: Parsing of a variable definition consisting of 'type' and 'name.
                Members can be mentioned together in a group, which means that
                they can appear simultaneously. Possible expresions are

                (1) single variables:

                              name0 : type;
                              name1 : type[32];
                              name2 : type*;

                (2) combined variables

                              {
                                  sub_name0 : type0;
                                  sub_name1 : type[64];
                                  sub_name2 : type1*;
                              }

       ARGUMENTS: 

        'GroupF'               allows to have 'nested variable groups' in curly brackets

        'already_defined_list' informs about variable names that have been already
                               chosen. It is only used for groups.

       RETURNS:
                 None        on failure to pass a variable definition.
                 array       when a single variable definition was found. 
                                array[0] = UserCodeFragment containing the type. 
                                array[1] = name of the variable.
                 dictionary  if it was a combined variable definition. The dictionary
                               maps: (variable name) ---> (UserCodeFragment with type)
    
    """
    position = fh.tell()

    skip_whitespace(fh)
    name_str = read_identifier(fh)
    if name_str == "":
        if not GroupF or not check(fh, "{"): 
            fh.seek(position); 
            return None
        sub_db = parse_variable_definition_list(fh, "Concurrent union variables", already_defined_list)
        if not check(fh, "}"): 
            fh.seek(position)
            error.log("Missing closing '}' after concurrent variable definition.", fh)
        return [ sub_db ]

    else:
        name_str = name_str.strip()
        if not check(fh, ":"): error.log("Missing ':' after identifier '%s'." % name_str, fh)
        
        if fh.read(1).isspace() == False:
            error.log("Missing whitespace after ':' after identifier '%s'.\n" % name_str \
                    + "The notation has to be: variable-name ':' type ';'.", fh)

        type_str, i = read_until_letter(fh, ";", Verbose=True)
        if i == -1: error.log("missing ';'", fh)
        type_str = type_str.strip()

        return [ CodeUser(type_str, SourceRef.from_FileHandle(fh)), name_str ]

def __validate_definition(TheCodeFragment, NameStr, 
                          AlreadyMentionedList, StandardMembersF):
    if StandardMembersF:
        error.verify_word_in_list(NameStr, TokenType_StandardMemberList, 
                            "Member name '%s' not allowed in token_type section 'standard'." % NameStr, 
                            TheCodeFragment.sr)

        # Standard Members are all numeric types
        if    TheCodeFragment.contains_string(Lng.Match_string) \
           or TheCodeFragment.contains_string(Lng.Match_vector) \
           or TheCodeFragment.contains_string(Lng.Match_map):
            type_str = TheCodeFragment.get_text()
            error.log("Numeric type required.\n" + \
                      "Example: <token_id: uint16_t>, Found: '%s'\n" % type_str, 
                      TheCodeFragment.sr)
    else:
        if NameStr in TokenType_StandardMemberList:
            error.log("Member '%s' only allowed in 'standard' section." % NameStr,
                      TheCodeFragment.sr)

    for candidate in AlreadyMentionedList:
        if candidate[0] != NameStr: continue 
        error.log("Token type member name '%s' defined twice." % NameStr,
                  TheCodeFragment.sr, DontExitF=True)
        error.log("Previously defined here.",
                  candidate[1].sr)

_warning_msg = \
"""Section token_type does not contain a 'take_text' section. It would be
necessary if the analyzer uses the string accumulator."""

