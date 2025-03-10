all: ../subsystems/default.cgpr
	gprbuild -p -Pnew_ada_main --config=../subsystems/default.cgpr $(GPRBUILDFLAGS)

../subsystems/default.cgpr:
	$(MAKE) -C ../subsystems default.cgpr

clean: ../subsystems/default.cgpr
	gprclean -r -Pnew_ada_main --config=../subsystems/default.cgpr
	$(MAKE) -C ../subsystems clean


run: all
	./ada_main

.PHONY: all clean run
