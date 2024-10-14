import os
from testsuite_support.builder_and_runner import (
    BuilderAndRunner, GPRLS, GPRCONFIG
)

bnr = BuilderAndRunner()


os.environ.pop("GPR_CONFIG", None)

bnr.call([GPRLS, "-Pp", "-F"])
bnr.call([GPRLS, "-Pp", "--target=foo", "-F"])
bnr.call([GPRLS, "-Pp", "--target=foo", "--RTS=bar", "-F"])
bnr.call([GPRLS, "-Pp", "--RTS=bar", "-F"])

os.environ["GPR_CONFIG"] = "conf"

bnr.call([GPRLS, "-Pp", "-F"])
bnr.call([GPRLS, "-Pp", "--target=foo", "-F"])
bnr.call([GPRLS, "-Pp", "--target=foo", "--RTS=bar", "-F"])
bnr.call([GPRLS, "-Pp", "--RTS=bar", "-F"])

os.environ["GPR_CONFIG"] = "conf.cgpr"

bnr.call([GPRLS,"-Pp", "-F"])

os.environ.pop("GPR_CONFIG")

bnr.call([GPRCONFIG, "--batch", "--config=ada", "-o", "light.cgpr"])
bnr.call([GPRLS, "-Pp", "--RTS=light", "-F"])
