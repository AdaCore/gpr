echo "gprbuild -f -u -P prj.gpr"
gprbuild -f -u -P prj.gpr
echo "gprbuild -f -u -P prj.gpr pkg.ads"
gprbuild -f -u -P prj.gpr pkg.ads
gprbuild -f -q -P prj2.gpr
chmod -w main.ali
echo "gprbuild -f -u -P prj2.gpr"
gprbuild -f -u -P prj2.gpr
echo "gprbuild -f -u -P prj2.gpr main.adb"
gprbuild -f -u -P prj2.gpr main.adb
gprclean -r -q prj.gpr
gprclean -r -q prj2.gpr
