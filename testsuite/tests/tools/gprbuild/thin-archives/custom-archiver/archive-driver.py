import subprocess
import sys

# The archiver accepts switches unknown to GPRbuild, including an option
# whose separate argument contains spaces.
assert sys.argv[2:5] == [
    "--custom-thin", "--custom-label", "argument with spaces"
], sys.argv
sys.exit(subprocess.call(["ar", sys.argv[1], "--thin", *sys.argv[5:]]))
