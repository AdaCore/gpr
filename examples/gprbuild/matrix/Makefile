all: default.cgpr
	gprbuild -p -Pmatrix $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C --config Fortran

clean: default.cgpr
	gprclean -Pmatrix
	$(RM) default.cgpr

run: all
	./obj/main

.PHONY: all clean run
