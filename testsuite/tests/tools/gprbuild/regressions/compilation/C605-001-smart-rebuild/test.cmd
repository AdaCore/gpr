gprbuild -q "-Pprj"
gnatmake -q dela
dela
cp pkg.adbnew pkg.adb
gprbuild "-Pprj"
