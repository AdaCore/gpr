all: default.cgpr
	gprbuild -p -Pmain0 $(GPRBUILDFLAGS)
	gprbuild -p -Pmain1 $(GPRBUILDFLAGS)
	gprbuild -p -Pmain2 $(GPRBUILDFLAGS)
	gprbuild -p -Pmain3 $(GPRBUILDFLAGS)
	gprbuild -p -Pmain4 $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -r -Pmain0
	gprclean -r -Pmain1
	gprclean -r -Pmain2
	gprclean -r -Pmain3
	gprclean -r -Pmain4
	$(RM) default.cgpr

run: all
	./main0
	./main1
	PATH="lib2:$$PATH" ./main2
	PATH="lib3:$$PATH" ./main3
	PATH="lib2:$$PATH" ./main4

.PHONY: all clean run
