#!/bin/env python3

import os
import subprocess
import time
import json
import tempfile


MAX_COMMAND_LENGTH = 32768
UNITS_NUMBER = 1000


def create_pkg_files(n):
    src_directory = os.path.join("data", "src")
    os.makedirs(src_directory, exist_ok=True)

    simple_body_template = """package body Pkg_{index} is
   procedure P (I : in out Integer) is
   begin
      I := I + 1;
   end P;
end Pkg_{index};
"""
    complex_body_template = """with Pkg_{index_plus1};
with Pkg_{index_plus2};
with Pkg_{index_plus3};

package body Pkg_{index} is
   procedure P (I : in out Integer) is
   begin
      I := I + 1;
   end P;
end Pkg_{index};
"""

    spec_template = """package Pkg_{index} is
   procedure P (I : in out Integer);
end Pkg_{index};
"""

    for i in range(1, n + 1):
        body_name = f"{src_directory}/pkg_{i}.adb"
        spec_name = f"{src_directory}/pkg_{i}.ads"

        with open(body_name, "w") as ada_file:
            if i < n - 2:
                ada_file.write(
                    complex_body_template.format(
                        index=i,
                        index_plus1=i + 1,
                        index_plus2=i + 2,
                        index_plus3=i + 3,
                    )
                )
            else:
                ada_file.write(
                    simple_body_template.format(
                        index=i,
                        index_plus1=i + 1,
                        index_plus2=i + 2,
                        index_plus3=i + 3,
                    )
                )

        with open(spec_name, "w") as ada_file:
            ada_file.write(spec_template.format(index=i))


def create_main_file():
    src_directory = os.path.join("data", "src")
    os.makedirs(src_directory, exist_ok=True)

    ada_template = """with Pkg_1;

    function Main return Integer is
    I : Integer := 0;
begin
    Pkg_1.P (I);
    return 4;
end Main;
"""

    main_file = os.path.join(src_directory, "main.adb")
    with open(main_file, "w") as fd:
        fd.write(ada_template)


def compile_with_gprbuild():
    project_file = os.path.join("data", "main.gpr")
    build_command = f"gprbuild -P {project_file} -j1"
    result = subprocess.run(
        build_command,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )

    if result.returncode != 0:
        print(f"Error building {project_file}:")
        print(result.stderr.decode("utf-8"))


def compile_with_gpr2build():
    build_command = os.path.join("gpr2build", "bin", "main")
    result = subprocess.run(
        build_command,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )

    if result.returncode != 0:
        print("Error building " + os.path.join("data", "main.gpr") + ":")
        print(result.stderr.decode("utf-8"))


def benchmark(description, function, args=None):
    print(description)
    start_time = time.time()
    if args is not None:
        function(args)
    else:
        function()
    end_time = time.time()
    total_time = end_time - start_time

    print(f"Execution time: {total_time:.4f} seconds")
    return total_time


def clean_dir_content(directory):
    try:
        for filename in os.listdir(directory):
            file_path = os.path.join(directory, filename)

            if os.path.isfile(file_path):
                os.remove(file_path)
            else:
                print(f"{file_path} is not a regular file. Failed to remove it")

    except OSError as e:
        print(f"Error: {directory} - {e}")


def check_produced_bin():
    result = subprocess.run(
        os.path.join("data", "obj", "main"),
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )

    if result.returncode != 4:
        print(f"Error running main:")
        print(result.stderr.decode("utf-8"))


def create_response_file(arguments, index):
    response_file_path = os.path.join(
        tempfile.gettempdir(), f"gcc_response_{index}.rsp"
    )

    with open(response_file_path, "w") as f:
        f.write(arguments + "\n")
    return response_file_path


def update_long_commands(commands):
    processed_commands = []

    for index, command in enumerate(commands):
        if len(command) > MAX_COMMAND_LENGTH:
            if command[0:3] != "gcc":
                print(
                    "[Warning]: long command different from gcc. Can not use response files for it"
                )
                processed_commands.append(command)
            else:
                response_file_path = create_response_file(
                    command[4:], index
                )  # Skip the 'gcc' part

                # Modify the command to use the response file
                modified_command = command[0:3] + " " + f"@{response_file_path}"
                processed_commands.append(modified_command)
        else:
            processed_commands.append(command)

    return processed_commands


def commands_to_execute():
    print("Obtaining commands to execute. It may take several minutes")
    result = subprocess.run(
        os.path.join("gpr2build", "bin", "main") + " --json",
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )

    if result.returncode != 0:
        print(f"Error running gpr2build/bin/main:")
        print(result.stderr.decode("utf-8"))

    commands = []
    jobs_json_path = os.path.join("data", "obj", "jobs.json")
    with open(jobs_json_path, "r") as file:
        json_content = json.load(file)
        commands = [obj["command"] for obj in json_content if "command" in obj]

    return update_long_commands(commands)


def execute_commands(commands):
    # gnatbind comand requires to be in the obj directory: generated sources
    # are created in the current directory.
    os.chdir(os.path.join("data", "obj"))
    for command in commands:
        result = subprocess.run(
            command,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )

        if result.returncode != 0:
            print(f"Error running " + command)
            print(result.stderr.decode("utf-8"))
            return
    os.chdir(os.path.join("..", ".."))


def build_gpr2build():
    project_file = os.path.join("gpr2build", "main.gpr")
    build_command = f"gprbuild -P {project_file}"
    result = subprocess.run(
        build_command,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
    )

    if result.returncode != 0:
        print(f"Error building {project_file}:")
        print(result.stderr.decode("utf-8"))


if __name__ == "__main__":
    build_gpr2build()
    clean_dir_content(os.path.join("data", "src"))
    create_pkg_files(UNITS_NUMBER)
    create_main_file()

    base_time = benchmark(
        "Compiling Ada files with explicit calls to compiler",
        execute_commands,
        commands_to_execute(),
    )
    check_produced_bin()
    clean_dir_content(os.path.join("data", "obj"))

    time_gpr2 = benchmark(
        "Compiling Ada files with gpr2build", compile_with_gpr2build
    )
    check_produced_bin()
    clean_dir_content(os.path.join("data", "obj"))

    time_gpr1 = benchmark(
        "Compiling Ada files with gprbuild", compile_with_gprbuild
    )
    check_produced_bin()
    clean_dir_content(os.path.join("data", "obj"))

    time_overhead = (time_gpr2 - base_time) / base_time * 100
    print(f"gpr2build overhead: {time_overhead:.1f}%")

    time_overhead = (time_gpr1 - base_time) / base_time * 100
    print(f"gprbuild overhead: {time_overhead:.1f}%")
    clean_dir_content(os.path.join("data", "src"))
