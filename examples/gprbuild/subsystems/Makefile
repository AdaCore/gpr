all: default.cgpr
	gprbuild -p -Pc_main $(GPRBUILDFLAGS)
	gprbuild -p -Pada_main $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -r -Pc_main
	gprclean -r -Pada_main
	$(RM) default.cgpr

run: all
	./ada_main
	./c_main

.PHONY: all clean run
