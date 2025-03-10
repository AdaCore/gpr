all: default.cgpr
	gprbuild -p -Pnamings -XC1=case1 $(GPRBUILDFLAGS)
	gprbuild -p -Pnamings -XC1=case2 $(GPRBUILDFLAGS)
	gprbuild -p -Pnamings -XC1=case3 $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -Pnamings -XC1=case1
	gprclean -Pnamings -XC1=case2
	gprclean -Pnamings -XC1=case3
	$(RM) default.cgpr

run: all
	./main-case1
	./main-case2
	./main-case3

.PHONY: all clean run
