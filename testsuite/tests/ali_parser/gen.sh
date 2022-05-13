PATH=~/sbx/gpr/x86_64-linux/gprbuild/install/bin:$PATH gprbuild -Pprj -XGNATVersion=wave -f -v

for v in 5.04a1 7.2.2 7.3.2; do
    PATH=~/sbx/gpr/x86_64-linux/gprbuild/install/bin:~/sbx/gpr/x86_64-linux/stable-gnat-${v}/install/bin:~/sbx/gpr/x86_64-linux/gprbuild/install/bin/:$PATH \
        gprbuild -Pprj -XGNATVersion=${v} -f -c -v
done

rm -f obj/*/b__* obj/*/*.std* obj/*/*.o obj/*/*.bexch obj/*/*.cgpr obj/*/main
