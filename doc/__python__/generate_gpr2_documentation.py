#!/usr/bin/env python3

import argparse
import json
import logging
import os.path
import sys
import time

from docgen.conf import substrings_to_escape
from docgen.conf import attribute_key_value_translation
from docgen.conf import complementary_files
from docgen.conf import display_options
from docgen.conf import logging_options
from docgen.utils import obj_package
from docgen.utils import obj_category
from docgen.utils import obj_attribute
from docgen.utils import package_dict


def process_jsons(json_def, json_conf, input_filename):
    my_pca_dict = package_dict()

    # package/Attribute generator to iterate through input_pa json file
    def pa_generator(packages):
        for p in packages:
            for a in p["attributes"]:
                yield p, a

    # Get the attribute category in input_c json file from a package
    # and attribute of input_pa json file
    def get_attribute_category(package, attribute):
        res = []
        for p in json_conf["packages"]:
            for c in p["categories"]:
                for a in c["attributes"]:
                    if (
                        package == p["package_name"]
                        and attribute == a["attribute_name"]
                    ):
                        res.append(c)
        return res

    # Get the package option to be displayed or not
    def get_package_to_display(package):
        res = []
        for p in json_conf["packages"]:
            if package["package_name"] == p["package_name"]:
                res.append(p["package_to_display"])

        if len(res) == 0:
            logging.error(
                f"package {package['package_name']} "
                + f"was not found in {complementary_files['Config_File']}"
            )
            sys.exit(1)

        return res[0]

    my_generator = pa_generator(json_def["packages"])
    for p, a in my_generator:

        package_to_be_displayed = get_package_to_display(p)
        attribute_category = get_attribute_category(
            p["package_name"], a["attribute_name"]
        )

        # if the package/Attribute does not have a category set in the config file
        # it is skipped for the rest of the document generation.
        if len(attribute_category) == 0:
            logging.error(
                f"{p['package_name']}.{a['attribute_name']} "
                + "does not have any category defined in "
                + f"{complementary_files['Config_File']}"
            )
            sys.exit(1)
        # Same treatment if a package/Attribute has more than one category.
        elif len(attribute_category) != 1:
            logging.error(
                f"{p['package_name']}.{a['attribute_name']} "
                + "has more than one category defined in "
                + f"{complementary_files['Config_File']}"
            )
            sys.exit(1)
        # Otherwise, it is added to top level object for it to be generated
        else:
            my_package = obj_package(
                p["package_name"], p["package_descr"], package_to_be_displayed
            )
            my_category = obj_category(
                attribute_category[0]["category_name"],
                attribute_category[0]["category_to_display"],
            )
            my_attribute = obj_attribute(
                p["package_name"],
                a["attribute_name"],
                a["attribute_descr"],
                a["attribute_def"],
            )
            # If the package and category is not meant to be displayed
            # there is no need to add the package/Category/Attribute
            # into the main object.
            if my_package.get_to_display() and my_category.get_to_display():
                if my_attribute.get_descr() == "":
                    logging.error(
                        f"{p['package_name']}.{a['attribute_name']} "
                        + f"has an empty description in {input_filename}"
                    )
                    sys.exit(1)
                my_pca_dict.add_hierarchy(my_package, my_category, my_attribute)
            else:
                logging.debug(
                    f"{my_package.get_name()}.{my_attribute.get_name()} "
                    + "will not appear in the documentation because"
                    + "it is set to hidden"
                )

    return my_pca_dict


def generate(data, file):
    # Map to ensure Packages and Categories are only displayed once.
    displayed_pc = {}

    def generate_introduction(file):
        title = "Attributes"
        file.write(f".. index:: attribute\n\n.. _{title}:\n\n")
        file.write(f"{title}\n" + "-" * len(title) + "\n\n")

        with open(complementary_files["Intro_File"], "r") as intro_file:
            for line in intro_file:
                file.write(line)

        file.write("\n\n")

    def write_pca(package, category, attribute, file):

        # Ensure each character from substrings_to_escape config are escaped
        # to be Sphinx compliant.
        def escape_substring_from_descr(descr):
            for subs in substrings_to_escape:
                if subs in descr:
                    descr = descr.replace(" " + subs, " \\" + subs)
            return descr

        tmp_descr = attribute.get_descr()

        package_text_name = " ".join(package.get_name().split("_")) + " Attributes"
        if package.get_name() != "Project_Level":
            package_text_name = "Package " + package_text_name

        # If the package is not in the map, it is not displayed already
        if not package.get_name() in displayed_pc:
            package_cat_name = f"_{package.get_name()}_Attributes"
            if package.get_name() != "Project_Level":
                package_cat_name = "_Package" + package_cat_name
            file.write(
                f".. {package_cat_name}:\n\n"
                + f"{package_text_name}\n"
                + "^" * len(package_text_name)
                + "\n\n"
            )
            # Insert the package into the map, it will not be displayed anymore
            displayed_pc[package.get_name()] = True

        attr_indent = ""
        if category.get_name() != "Default":
            attr_indent = "  "

        # If the category is not in the map, it is not displayed already
        if not package.get_name() + "." + category.get_name() in displayed_pc:
            if category.get_name() != "Default":
                file.write(f"* **{category.get_name()}**\n\n")
            # Insert the category into the map, it will not be displayed anymore
            displayed_pc[package.get_name() + "." + category.get_name()] = True

        file.write(f"{attr_indent}.. index:: Attributes - {package_text_name}; "
                   + f"{attribute.get_name()}\n\n")
        file.write(f"{attr_indent}* **{attribute.get_name()}**: ")
        if display_options["Short_Def"]:
            file.write(attribute.get_def_str())
        file.write("\n\n")

        if display_options["Whole_Def"]:
            file.write(f"{attr_indent}  .. code-block:: attribute_definition\n\n")

            key_max_len = max(len(x_) for x_ in attribute.get_def())
            value_max_len = max(
                len(y_) for x_, y_ in attribute.get_def().items() if isinstance(y_, str)
            )

            for key, value in attribute.get_def().items():
                file.write(
                    f"{attr_indent}     {key.title()} " + " " * (key_max_len - len(key))
                )

                if not isinstance(value, dict):
                    file.write(
                        f": {str(value).title()}"
                        + " " * (value_max_len - len(str(value)))
                        + f" -> {attribute_key_value_translation[key, value]}\n"
                    )
                else:
                    sub_key_max_len = max(len(x) for x in value)
                    file.write(
                        " " * value_max_len
                        + f"   -> {attribute_key_value_translation[key]}\n"
                    )

                    for sub_key, sub_value in value.items():
                        file.write(
                            f"{attr_indent}     "
                            + " " * (key_max_len - sub_key_max_len)
                            + f"{sub_key.title()} "
                            + " " * (sub_key_max_len - len(sub_key))
                            + f": {str(sub_value).title()}\n"
                        )

            file.write("\n")

        tmp_descr = escape_substring_from_descr(tmp_descr)

        while len(tmp_descr) > 0:
            length = min(len(tmp_descr), display_options["Max_Length"])
            if length == display_options["Max_Length"]:
                length = min(
                    tmp_descr[:length].rfind(" "), display_options["Max_Length"]
                )
            file.write(f"{attr_indent}  {tmp_descr[:length]}\n")
            tmp_descr = tmp_descr[length + 1 :]

        file.write("\n")

    with open(file, "w") as sfile:
        generate_introduction(sfile)
        p_generator = data.generator()
        for p, c_generator in p_generator:
            for c, a_generator in c_generator:
                for a in a_generator:
                    write_pca(p, c, a, sfile)


def main(args=None):
    i_filename = ""
    o_filename = ""

    whole_time = time.perf_counter()
    logging.basicConfig(
        level=logging_options["Level"], format=logging_options["Format"]
    )

    prog_description = (
        "Generates a reStructuredFile from GPRdoc output for " + "gprtools attributes"
    )
    parser = argparse.ArgumentParser(description=prog_description)

    parser.add_argument("--input", required=True, help="GPRdoc json output")
    parser.add_argument("--output", required=True, help="reStructuredText file")
    try:
        args = vars(parser.parse_args())
        i_filename = args["input"]
        o_filename = args["output"]

        if not os.path.isfile(i_filename):
            logging.error("Input file does not exists !")
            sys.exit(1)
        elif not os.path.exists(os.path.dirname(o_filename)):
            logging.error("Output file directory does not exists !")
            sys.exit(1)

    except argparse.ArgumentError as error:
        logging.error(error)
        sys.exit(1)

    def open_json_file(json_file):
        with open(json_file, "r") as read_file:
            data = json.load(read_file)
        return data

    generate(
        process_jsons(
            open_json_file(i_filename),
            open_json_file(complementary_files["Config_File"]),
            i_filename,
        ),
        o_filename,
    )
    logging.info(
        "Documentation generated in %.2f ms"
        % ((time.perf_counter() - whole_time) * 1000)
    )


if __name__ == "__main__":
    main()
