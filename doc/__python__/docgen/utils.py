#!/usr/bin/env python3

from docgen.conf import attribute_key_value_translation


class obj_attribute:
    def __init__(self, package, name, descr, defi):
        self.package = package
        self.name = name
        self.descr = descr
        self.defi = defi
        if self.defi["value"] == "LIST" and self.defi["value_is_set"]:
            self.defi["value"] = "SET"

    def get_name(self):
        return self.name

    def get_descr(self):
        return self.descr

    def get_def(self):
        return self.defi

    def get_def_str(self):
        def append(field):
            val = attribute_key_value_translation[field, self.defi[field]]
            if val != "":
                def_str.append(val)

        def_str = []

        append("value")

        if self.defi["value"] == "SET":
            append("value_case_sensitive")

        append("index_type")
        append("builtin")
        append("index_optional")
        append("config_concatenable")
        if self.package == "Project_Level":
            append("inherit_from_extended")

        return ", ".join(def_str)


class obj_category:
    def __init__(self, name, to_display):
        self.name = name
        self.to_display = to_display

    def get_name(self):
        return self.name

    def get_to_display(self):
        return self.to_display


class obj_package:
    def __init__(self, name, descr, to_display):
        self.name = name
        self.descr = descr
        self.to_display = to_display

    def get_name(self):
        return self.name

    def get_descr(self):
        return self.descr

    def get_to_display(self):
        return self.to_display


class attribute_dict:
    def __init__(self):
        self.attributes = {"attributes": []}

    def generator(self):
        for a in self.attributes["attributes"]:
            yield a["attribute_info"]

    def __attributes_exists__(self, a_info):
        a_list = [a["attribute_info"].get_name() for a in self.attributes["attributes"]]
        if a_info.get_name() in a_list:
            return True
        return False

    def add_attributes(self, a_info):
        if not self.__attributes_exists__(a_info):
            self.attributes["attributes"].append({"attribute_info": a_info})

    def add_hierarchy(self, a_info):
        self.add_attributes(a_info)


class category_dict:
    def __init__(self):
        self.categories = {"categories": []}

    def generator(self):
        for c in self.categories["categories"]:
            a = c["attributes"].generator()
            yield c["category_info"], a

    def __category_exists__(self, c_info):
        c_list = [c["category_info"].get_name() for c in self.categories["categories"]]
        if c_info.get_name() in c_list:
            return True
        return False

    def add_category(self, c_info):
        if not self.__category_exists__(c_info):
            self.categories["categories"].append({"category_info": c_info})

    def add_hierarchy(self, c_info, a_info):
        self.add_category(c_info)
        for c in self.categories["categories"]:
            if c["category_info"].get_name() == c_info.get_name():
                if "attributes" not in c:
                    c["attributes"] = attribute_dict()
                c["attributes"].add_hierarchy(a_info)


class package_dict:
    def __init__(self):
        self.packages = {"packages": []}

    def generator(self):
        for p in self.packages["packages"]:
            c = p["categories"].generator()
            yield p["package_info"], c

    def __package_exists__(self, p_info):
        plist = [p["package_info"].get_name() for p in self.packages["packages"]]
        if p_info.get_name() in plist:
            return True
        return False

    def add_package(self, p_info):
        if not self.__package_exists__(p_info):
            self.packages["packages"].append({"package_info": p_info})

    def add_hierarchy(self, p_info, c_info, a_info):
        self.add_package(p_info)
        for p in self.packages["packages"]:
            if p["package_info"].get_name() == p_info.get_name():
                if "categories" not in p:
                    p["categories"] = category_dict()
                p["categories"].add_hierarchy(c_info, a_info)
