all: default.cgpr
	gprbuild -p -Pmain -XLIB=static $(GPRBUILDFLAGS)
	gprbuild -p -Pmain -XLIB=dynamic $(GPRBUILDFLAGS)


default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -r -Pmain -XLIB=static
	gprclean -r -Pmain -XLIB=dynamic
	$(RM) default.cgpr

run: all
	./obj/main
	PATH="../libraries/lib2:$$PATH" obj1/main

.PHONY: all clean run
