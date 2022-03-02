.PHONY: doc build build-release

doc:
	@echo "Build GPR2 C Binding"
	@echo ""
	@echo "  build:         build the shared library with debugging information"
	@echo "  build-release: build  the shared library for production"
	@echo "  clean:         clean build artifacts"

build:
	gprbuild -j0 -Pgpr2_c_binding -XGPR2_BUILD=debug

build-release:
	gprbuild -j0 -Pgpr2_c_binding -XGPR2_BUILD=release

clean:
	rm -rf build


