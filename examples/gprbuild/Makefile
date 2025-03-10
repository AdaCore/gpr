%:
	@echo 'Unknown target. Usage:'
	@echo 'make [all]                   build all examples'
	@echo 'make run                     run all examples'
	@echo 'make clean                   clean all examples'
	@echo 'make GPRBUILDFLAGS=-v        add options for gprbuild'
	@echo 'make ada_cpp/all matrix/all  build two specific examples'

subdirectories := $(wildcard */)
targets_all   := $(subdirectories:=all)
targets_clean := $(subdirectories:=clean)
targets_run   := $(subdirectories:=run)

all  : $(targets_all)
clean: $(targets_clean)
run  : $(targets_run)
$(targets_all) $(targets_clean) $(targets_run):
	$(MAKE) -C$(subst /, ,$@)

.PHONY: all clean run $(targets_all) $(targets_clean) $(targets_run)
