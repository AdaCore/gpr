all: default.cgpr
	gprbuild -Panimals $(GPRBUILDFLAGS)
	gprbuild -Pexcept $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C++

clean: default.cgpr
	gprclean -Panimals
	gprclean -Pexcept
	$(RM) default.cgpr

run: all
	./ada_main
	./cpp_main
	./main

.PHONY: all clean run
