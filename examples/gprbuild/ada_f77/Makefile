all: default.cgpr
	gprbuild -p -Pft $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config Fortran

clean: default.cgpr
	gprclean -r -Pft
	$(RM) default.cgpr

run: all
	./fobj/ess

.PHONY: all clean run
