%:
	@echo 'Unknown target. Usage:'
	@echo 'make [all]                build all Rust examples'
	@echo 'make run                  run all Rust examples'
	@echo 'make clean                clean all Rust examples'
	@echo 'make greeter/all          build one specific example'

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
