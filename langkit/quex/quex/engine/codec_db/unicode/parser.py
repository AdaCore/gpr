import os
import sys

sys.path.insert(0, os.environ["QUEX_PATH"])

from   quex.DEFINITIONS              import QUEX_PATH
import quex.engine.misc.error        as     error

from   quex.engine.misc.interval_handling import Interval, NumberSet

import re
import fnmatch
from   copy import deepcopy

unicode_db_directory = QUEX_PATH + "/quex/engine/codec_db/unicode/database"
comment_deleter_re   = re.compile("#[^\n]*")

def open_data_base_file(Filename):
    try: 
        fh = open(unicode_db_directory + "/" + Filename, "rb")
    except:
        error.log("Fatal---Unicode Database File '%s' not found!\n" % Filename + \
                  "QUEX_PATH='%s'\n" % QUEX_PATH + \
                  "Unicode Database Directory: '%s'" % unicode_db_directory)
    return fh

def parse_table(Filename, IntervalColumnList=[], NumberColumnList=[], NumberListColumnList=[], CommentF=False):
    """Columns in IntervalColumnList   --> converted to Interval() objects
                  NumberColumnList     --> converted to integers (hex numbers)
                  NumberListColumnList --> converted to integer list (hex numbers)
    """
    fh = open_data_base_file(Filename)

    record_set = []
    for line in fh.readlines():
        comment_idx = line.find("#")
        comment     = None
        if comment_idx != -1: 
            comment = line[comment_idx+1:]
            line    = line[:comment_idx]

        if line == "" or line.isspace(): 
            continue

        # append content to record set
        cells = map(lambda x: x.strip(), line.split(";"))

        for i in IntervalColumnList:
            fields = cells[i].split("..")    # range: value0..value1
            assert len(fields) in [1, 2]

            if len(fields) == 2: 
               begin = int("0x" + fields[0], 16)
               end   = int("0x" + fields[1], 16) + 1
            else:
               begin = int("0x" + fields[0], 16)
               end   = int("0x" + fields[0], 16) + 1
            cells[i] = Interval(begin, end)

        for i in NumberColumnList:
            cells[i] = int("0x" + cells[i], 16)

        for i in NumberListColumnList:
            nl = []
            for n in cells[i].split():
                nl.append(int("0x" + n, 16))
            cells[i] = nl

        # Sometimes, the comment is useful
        if CommentF: 
            cells.append(comment)

        record_set.append(cells)

    # There is no need to decouple here, since the record_set is created 
    # each time that the function is called.
    return record_set

def convert_table_to_associative_map(table, ValueColumnIdx, ValueType, KeyColumnIdx):
    """Produces a dictionary that maps from 'keys' to NumberSets. The 
       number sets represent the code points for which the key (property)
       is valid.

       ValueColumnIdx: Column that contains the character code interval or
                       string to which one wishes to map.

       KeyColmnIdx:   Column that contains the 'key' to be used for the map

       self.db = database to contain the associative map.
    """

    db = {}
    if ValueType == "NumberSet":
        for record in table:
            key   = record[KeyColumnIdx].strip()
            key   = key.replace(" ", "_")
            value = record[ValueColumnIdx]

            if type(value) == int: value = Interval(value)

            db.setdefault(key, NumberSet()).quick_append_interval(value, SortF=False)

    elif ValueType == "number" or ValueType == "string":
        for record in table:
            key   = record[KeyColumnIdx].strip()
            key   = key.replace(" ", "_")
            value = record[ValueColumnIdx]
            db[key] = value
    else:
        raise BaseException("ValueType = '%s' unknown.\n" % ValueType)

    # if the content was a number set, it might be simplified, try it.
    if ValueType == "NumberSet":
        for key, number_set in db.items():
            number_set.clean()

    return db

def load_db(DB_Filename, ValueType, ValueColumnIdx, KeyColumnIdx):
    """Loads a database contained in file 'DB_Filename'. Creates a python dictionary
       that maps from a string (contained in column KeyColumnIdx) to a number set
       or a single number (contained in column ValueColumnIdx).

       NOTE: The 'key' maybe for example the property value. The 
             'value' is the number set it points to. This maybe
             confusing.
    """
    if   ValueType == "NumberSet": 
        table = parse_table(DB_Filename, IntervalColumnList=[ValueColumnIdx])
    elif ValueType == "number":    
        table = parse_table(DB_Filename, NumberColumnList=[ValueColumnIdx])

    db = convert_table_to_associative_map(table, ValueColumnIdx, ValueType, KeyColumnIdx)

    return db

class PropertyInfo:
    def __init__(self, Name, Alias, Type, RelatedPropertyInfoDB):
        """Alias = short form of Name or Value.
        """
        self.name  = Name
        self.alias = Alias
        self.type  = Type
        self.alias_to_name_map = {}   # map value alias to value
        #                             # NOTE: Not all values may have aliases!
        self.code_point_db     = None # map value (not alias) to number set or number
        self.related_property_info_db = RelatedPropertyInfoDB
        # Some values may be based on combinations of values. For those, the 
        # following maps are required.
        self.alias_to_alias_combination_db = {}
        self.name_to_alias_map             = {}


    def __repr__(self):
        assert self.type in ["Binary", "Catalog", "Enumerated", "String", "Miscellaneous", "Numeric"], \
               "self.type = " + repr(self.type)

        txt  = "NAME          = '%s'\n" % self.name
        txt += "ALIAS         = '%s'\n" % self.alias
        txt += "TYPE          = '%s'\n" % self.type
        if self.type == "Binary":
            txt += "VALUE_ALIASES = (Binary has no values)\n"
        else:
            txt += "VALUE_ALIASES = {\n    "
            txt += self.get_value_list_help(sys.maxint).replace(", ", ",\n    ")
            txt += "\n}\n" 
        return txt

    def get_character_set(self, Value=None):
        """Returns the character set that corresponds to 'Property==Value'.
           'Value' can be a property value or a property value alias.
           For binary properties 'Value' must be None.
        """
        assert self.type != "Binary" or Value is None

        def get_value_combination(CmbAlias):
            result = []
            for alias in self.alias_to_alias_combination_db[CmbAlias]:
                name = self.alias_to_name_map.get(alias)
                if name is None:
                    return "Unicode database error: no name related to alias '%s'" % alias
                result.append(name)
            return result

        if self.type != "Binary" and Value is None:
            return "Property '%s' requires a value setting.\n" % self.name + \
                   "Possible Values: " + \
                   self.get_value_list_help()

        if self.code_point_db is None:
            self.init_code_point_db()

        if self.type == "Binary": 
            # Decouple, since we refer to an internal database
            return deepcopy(self.code_point_db)

        adapted_value = Value.replace(" ", "_")

        if   self.code_point_db.has_key(adapted_value): 
            # 'value' is present as name in the code point database
            value = adapted_value

        elif Value in self.alias_to_name_map.keys():
            # 'value' is present as alias in code pointer database
            value = self.alias_to_name_map[adapted_value]

        elif Value in self.alias_to_alias_combination_db.keys():
            # 'value' is present as a combination of aliases
            value = get_value_combination(adapted_value)

        elif self.name_to_alias_map.has_key(adapted_value):
            # The value was a combination of values
            value = get_value_combination(self.name_to_alias_map[adapted_value])

        else:
            # -- WILDCARD MATCH: Results in a list of property values  
            character_set = self.__wildcard_value_match(adapted_value)
            if character_set is None:
                return "Property '%s' cannot have a value or value alias '%s'.\n" % (self.name, Value) + \
                       "Possible Values: " + \
                       self.get_value_list_help()
            # No need to decouple, since character is not a reference to
            # internal database (for safety, do it)
            return deepcopy(character_set)

        if type(value) == list:
            result = NumberSet()
            for element in value:
                if element == "Unassigned": continue
                entry = self.code_point_db.get(element)
                if entry is None:
                    return "%s/%s is not supported by Unicode database." % (self.name, repr(element))
                result.unite_with(entry)
        else:
            result = self.code_point_db.get(value)
            if result is None:
                return "%s/%s is not supported by Unicode database." % (self.name, repr(value))

        # Reference to internal database --> decouple with 'deepcopy'
        return deepcopy(result)

    def init_code_point_db(self):

        if self.alias in ["na", "na1", "nv", "gc", "bc", "isc"]:
            # Name
            # Unicode 1 Name 
            # Numeric Value
            # General Category
            # Bidi Class
            self.related_property_info_db.load_UnicodeData()
            return
        
        if self.type == "Catalog":
            if   self.alias == "blk":
                self.code_point_db = load_db("Blocks.txt",     "NumberSet", 0, 1)
            elif self.alias == "age":
                self.code_point_db = load_db("DerivedAge.txt", "NumberSet", 0, 1)
            elif self.alias == "sc":
                self.code_point_db = load_db("Scripts.txt",    "NumberSet", 0, 1)
            else:
                return


        elif self.type == "Binary":

            if self.alias in ["AHex", "Bidi_C", "Dash", "Dep", "Dia",
                    "Ext", "Hex", "Hyphen", "IDSB", "IDST", "Ideo", "Join_C",
                    "LOE", "NChar", "OAlpha", "ODI", "OGr_Ext", "OIDC", "OIDS",
                    "OLower", "OMath", "OUpper", "Pat_Syn", "Pat_WS", "QMark",
                    "Radical", "SD", "STerm", "Term", "UIdeo", "VS", "WSpace"]:

                filename = "PropList.txt"

            elif self.alias == "Bidi_M":

                filename = "extracted/DerivedBinaryProperties.txt"

            elif self.alias in ["Alpha", "DI", "Gr_Base", "Gr_Ext",
                    "Gr_Link", "IDC", "IDS", "Math", "Lower", "Upper", "XIDC", "XIDS" ]:

                filename = "DerivedCoreProperties.txt"

            elif self.alias == "Comp_Ex":

                filename = "DerivedNormalizationProps.txt"

            elif self.alias == "CE":

                self.related_property_info_db.load_Composition_Exclusion()
                return

            else:
                return
                   
            self.related_property_info_db.load_binary_properties(filename)

        elif self.type == "Enumerated":
            try:
                filename = {
                        "Numeric_Type":              "extracted/DerivedNumericType.txt",
                        "Joining_Type":              "extracted/DerivedJoiningType.txt",
                        "Joining_Group":             "extracted/DerivedJoiningGroup.txt",
                        "Word_Break":                "auxiliary/WordBreakProperty.txt",
                        "Sentence_Break":            "auxiliary/SentenceBreakProperty.txt",
                        "Grapheme_Cluster_Break":    "auxiliary/GraphemeBreakProperty.txt",
                        "Hangul_Syllable_Type":      "HangulSyllableType.txt",
                        "Line_Break":                "extracted/DerivedLineBreak.txt",
                        "Decomposition_Type":        "extracted/DerivedDecompositionType.txt",
                        "East_Asian_Width":          "extracted/DerivedEastAsianWidth.txt",
                        "Canonical_Combining_Class": "extracted/DerivedCombiningClass.txt",
                    }[self.name]
            except:
                print "warning: no database file for property `%s'." % self.name
                return

            self.code_point_db = load_db(filename, "NumberSet", 0, 1)

        elif self.type == "Miscellaneous":
            pass # see first check

    def get_value_list_help(self, MaxN=20, OpeningBracket="", ClosingBracket=""):
        if self.code_point_db is None:
            self.init_code_point_db()

        the_list = self.code_point_db.keys()
        n = min(len(the_list), MaxN)
        selection = the_list[:n]
        selection.sort()

        txt = ""
        alias_name_pair_list = self.alias_to_name_map.items()
        for element in selection:
            if element == "": continue
            txt += OpeningBracket + element 
            for alias, name in alias_name_pair_list:
                if element == name: 
                    txt += "(%s)" % alias
                    break
            txt += ClosingBracket + ", "

        if n != len(the_list):  txt += "... (%i more)" % (len(the_list) - n)
        else:                   txt = txt[:-2] + "."

        return txt 

    def get_wildcard_value_matches(self, WildCardValue):
        """Does not consider value aliases!"""
        value_candidates = self.code_point_db.keys()
        match_value_list = fnmatch.filter(value_candidates, WildCardValue)
        match_value_list.sort()
        # No need to decouple, match_value_list is generated new for each call.
        return match_value_list

    def __wildcard_value_match(self, WildCardValue):
        result = NumberSet()

        value_list = self.get_wildcard_value_matches(WildCardValue)
        if len(value_list) == 0: 
            return None

        for value in value_list:
            result.unite_with(NumberSet(self.code_point_db[value]))

        # No decoupling, since result is computed each fresh and new
        return result

class PropertyInfoDB:
    def __init__(self):
        self.property_name_to_alias_map = {}  # map: property alias to property name
        self.db = {}                          # map: property alias to property information

        self.__code_point_to_name_db = {}

    def __getitem__(self, PropertyName):
        if self.db == {}: self.init_db()

        if PropertyName in self.db.keys(): 
            return self.db[PropertyName]
        elif PropertyName in self.property_name_to_alias_map.keys():
            return self.db[self.property_name_to_alias_map[PropertyName]]
        else: 
            return "<unknown property or alias '%s'>" % PropertyName

    def get_property_value_matches(self, PropertyName, Value):
        assert Value is not None

        if self.db == {}: self.init_db()

        property = self[PropertyName]
        if not isinstance(property, PropertyInfo):
            txt = property
            txt += "Properties: " + self.get_property_names()
            return txt

        if property.type == "Binary":
            if Value is not None:
                return "Binary property '%s' cannot have a value.\n" % PropertyName + \
                       "Received '%s = %s'." % (PropertyName, Value)

        return property.get_wildcard_value_matches(Value)

    def get_character_set(self, PropertyName, Value=None):
        """Returns the character set that corresponds to 'Property==Value'.

           'Property' can be a property name or a property alias.
           'Value'    can be a property value or a property value alias.
                      For binary properties 'Value' must be None.

           RETURNS: NumberSet in case of success.
                    str       in case an error occurred. String describes the problem.
        """
        if self.db == {}: self.init_db()

        property = self[PropertyName]
        if not isinstance(property, PropertyInfo):
            assert type(property) in [str, unicode]
            txt = property
            txt += "Properties: " + self.get_property_names()
            return txt

        if property.type == "Binary":
            if Value is not None:
                return "Binary property '%s' cannot have a value.\n" % PropertyName + \
                       "Received '%s = %s'." % (PropertyName, Value)

        elif Value is None:
            return "Non-Binary property '%s' must have a value.\n" % PropertyName + \
                   "Expected something like '%s = Value'.\n" % PropertyName + \
                   "Possible Values: " + \
                   property.get_value_list_help()

        return property.get_character_set(Value)

    def init_db(self):
        self.__parse_property_name_alias_and_type()
        self.__parse_property_value_and_value_aliases()

    def __parse_property_name_alias_and_type(self):
        fh = open_data_base_file("PropertyAliases.txt")

        # -- skip anything until the first line that contains '======'
        line = fh.readline()
        while line != "":
            if line.find("# ==================") != -1: break
            line = fh.readline()

        property_type = "none"
        for line in fh.readlines():
            line = line.strip()
            if line != "" and line[0] == "#" and line.find("Properties") != -1:
                property_type = line.split()[1]
                continue
            
            line = comment_deleter_re.sub("", line)
            if line.isspace() or line == "": continue
            # append content to record set
            fields = map(lambda x: x.strip(), line.split(";"))
            property_alias = fields[0]
            property_name  = fields[1]

            self.db[property_alias] = PropertyInfo(property_name, property_alias, property_type, self)
            self.property_name_to_alias_map[property_name] = property_alias

    def __parse_property_value_and_value_aliases(self):
        """NOTE: Function __parse_property_name_alias_and_type() **must be called**
                 before this function.
        """
        assert self.db != {}
        table = parse_table("PropertyValueAliases.txt", CommentF=True)

        for row in table:
            property_alias       = row[0].strip()
            property_value_alias = row[1].strip()
            property_value       = row[2].replace(" ", "_").strip()

            # if property db has been parsed before, this shall not fail
            property_info = self.db[property_alias]

            # The 'General Category' property is different, in the sense that
            # important information may be stored in comments.
            if property_alias == "gc" and row[-1] is not None:
                combination = map(lambda x: x.strip(), row[-1].split("|"))
                property_info.alias_to_alias_combination_db[property_value_alias] = combination
                property_info.name_to_alias_map[property_value]                   = property_value_alias
            else:
                property_info.alias_to_name_map[property_value_alias] = property_value

    def load_binary_properties(self, DB_Filename):
        # property descriptions working with 'property names'
        db = load_db(DB_Filename, "NumberSet", 0, 1)

        for key, number_set in db.items():

            if self.property_name_to_alias_map.has_key(key): 
                property_name_alias = self.property_name_to_alias_map[key]
            else:
                property_name_alias = key

            property = self.db[property_name_alias]

            if property.type != "Binary": continue

            property.code_point_db = number_set

    def load_Composition_Exclusion(self):
        # Column 0 contains what is interesting ...
        table = parse_table("CompositionExclusions.txt", NumberColumnList=[0])

        number_set = NumberSet()
        for row in table:
           begin = row[0]
           number_set.quick_append_interval(Interval(begin, begin + 1))
        number_set.clean()    

        self.db["CE"].code_point_db = number_set

    def load_UnicodeData(self):
        fh = open_data_base_file("UnicodeData.txt")

        # some rows contain aliases, so they need to get converted into values
        property_general_category = self.db["gc"]
        property_bidi_class       = self.db["bc"]

        def convert(Property, ValueAlias):
            """Convert specified ValueAlias to Value of the given property."""
            if Property.alias_to_name_map.has_key(ValueAlias):
                return Property.alias_to_name_map[ValueAlias]
            return ValueAlias

        names_db            = {}
        general_category_db = {}
        bidi_class_db       = {}
        numeric_value_db    = {}
        names_uc1_db        = {}
        iso_comment_db      = {}

        for line in fh.readlines():
            if line.find("#") != -1: line = line[:line.find("#")]
            if line == "" or line.isspace(): continue

            x = line.split(";")

            code_point       = int("0x" + x[0].strip(), 16)    # CodePointIdx       = 0
            name             = x[1].strip().replace(" ", "_")  # NameIdx            = 1
            general_category = x[2].strip().replace(" ", "_")  # GeneralCategoryIdx = 2
            general_category = convert(property_general_category, general_category)
            bidi_class       = x[4].strip().replace(" ", "_")  # BidiClassIdx       = 4
            bidi_class       = convert(property_bidi_class, bidi_class)
            numeric_value    = x[6].strip()                    # NumericValueIdx    = 6
            uc1_name         = x[10].strip().replace(" ", "_") # NameUC1Idx         = 10
            iso_comment      = x[11].strip().replace(" ", "_") # ISO_CommentIdx     = 11

            names_db[name]                                                            = code_point
            general_category_db.setdefault(general_category, NumberSet()).quick_append_value(code_point)
            bidi_class_db.setdefault      (bidi_class,       NumberSet()).quick_append_value(code_point)
            numeric_value_db.setdefault   (numeric_value,    NumberSet()).quick_append_value(code_point)
            names_uc1_db[uc1_name]                                                    = code_point
            iso_comment_db[iso_comment]                                               = str(code_point)

        self.db["na"].code_point_db  = names_db             # Name
        self.db["gc"].code_point_db  = general_category_db  # General Category
        self.db["bc"].code_point_db  = bidi_class_db        # BidiClass
        self.db["nv"].code_point_db  = numeric_value_db     # Numeric Value
        self.db["na1"].code_point_db = names_uc1_db         # Name Unicode 1
        self.db["isc"].code_point_db = iso_comment_db       # ISO_Comment

    def map_code_point_to_character_name(self, CodePoint):
        if self.db["na"].code_point_db is None:
            self.load_UnicodeData()
        if len(self.__code_point_to_name_db) == 0:
            for key, value in self.db["na"].code_point_db.items():
                self.__code_point_to_name_db[value] = key
            for key, value in self.db["na1"].code_point_db.items():
                if self.__code_point_to_name_db.has_key(value): continue
                self.__code_point_to_name_db[value] = key

        return self.__code_point_to_name_db.get(CodePoint, "UCS 0x%06X" % CodePoint)

    def get_property_descriptions(self):
        item_list = self.db.items()

        L  = max(map(lambda property: len(property.name), self.db.values()))
        La = max(map(lambda property: len(property.alias), self.db.values()))
        Lt = max(map(lambda property: len(property.type), self.db.values()))

        txt = "# Abbreviation, Name, Type\n"
        item_list.sort(lambda a, b: cmp(a[0], b[0]))
        for key, property in item_list:
            txt += "%s, %s%s, %s%s" % \
                    (property.alias, " " * (La - len(property.alias)),
                     property.name, " " * (L - len(property.name)),
                     property.type)
            property.init_code_point_db()
            if property.code_point_db is None: 
               txt += ", " + " " * (Lt - len(property.type)) + "<unsupported>" 

            txt += "\n"

        return txt

    def get_property_names(self, BinaryOnlyF=False):
        if self.db == {}:
            self.init_db()

        alias_list = self.db.keys()
        alias_list.sort(lambda a, b: cmp(self.db[a], self.db[b]))

        txt = ""
        for alias in alias_list:
            if BinaryOnlyF and self.db[alias].type != "Binary": continue
            txt += self.db[alias].name + "(%s)" % alias
            txt += ", "

        return txt 

    def get_documentation(self):
        binary_property_list     = []
        non_binary_property_list = []

        for property in self.db.values():
            if property.type == "Binary": binary_property_list.append(property)
            else:                         non_binary_property_list.append(property)


        def list_to_string(the_list):
            the_list.sort(lambda a, b: cmp(a.name, b.name))
            txt = ""
            for property in the_list:
                txt += property.name + "(%s), " % property.alias
            return txt

        txt  = "Binary Properties::\n\n"
        txt += "    " + list_to_string(binary_property_list)
        txt += "\n\n"
        txt += "Non-Binary Properties::\n\n"
        txt += "    " + list_to_string(non_binary_property_list)
        txt += "\n\n"
        txt += "--------------------------------------------------------------\n"
        txt += "\n\n"
        txt += "Property settings:\n"
        txt += "\n\n"
        for property in non_binary_property_list:
            if property.type == "Binary": continue

            txt += "%s::\n\n" % property.name

            property.init_code_point_db()
            if   property.code_point_db is None: 
                txt += "    (not supported)\n" 

            elif property.name in ["Name", "Unicode_1_Name"]:
                txt += "    (see Unicode Standard Literature)\n"

            else:
                value_txt = property.get_value_list_help(270, OpeningBracket="$$", ClosingBracket="$$")
                txt += "    " + value_txt + "\n"

            txt += "\n"


        return txt




ucs_property_db = PropertyInfoDB()

if __name__ == "__main__":
    ucs_property_db.init_db()
    ################################################################################
    # NOTE: Do not delete this. It is used to generate documentation automatically.
    ################################################################################
    print ucs_property_db.get_documentation()

    # print ucs_property_db.db["bc"].get_value_list_help()
    # print ucs_property_db.get_character_set("Block",  "Arabic")
    # print ucs_property_db.get_character_set("Age",    "5.0")
    # print ucs_property_db.get_character_set("Script", "Greek")
    # print "%X" % names_db["LATIN SMALL LETTER CLOSED REVERSED EPSILON"]
    # print ucs_property_db.get_character_set("White_Space")

    #print ucs_property_db.get_property_descriptions()


