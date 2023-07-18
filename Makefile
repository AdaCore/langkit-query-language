BUILD_MODE=dev
export BUILD_MODE

ifeq ($(OS),Windows_NT)
  SOEXT=.dll
else
  SOEXT=.so
endif

PROCS=0
PREFIX=install
PYTHON=python
BUILD_DIR=/undefined
LKQL_DIR=$(BUILD_DIR)/lkql
GPRBUILD=gprbuild -j$(PROCS) -p -XBUILD_MODE=$(BUILD_MODE)
GPRINSTALL=gprinstall --prefix=$(PREFIX) -p -XBUILD_MODE=$(BUILD_MODE)
BUILD_FOR_JIT=false

ifeq ($(BUILD_FOR_JIT),true)
  MANAGE_ARGS=--build-dir=$(LKQL_DIR) --build-mode=$(BUILD_MODE) \
    --library-types=relocatable
else
  MANAGE_ARGS=--build-dir=$(LKQL_DIR) --build-mode=$(BUILD_MODE) \
    --library-types=static
endif

ADDITIONAL_MANAGE_ARGS=

# WARNING: Note that for some reason parallelizing the build still doesn't work
all: lkql lkql_checker lalcheck doc lkql_jit lkql_native_jit

lkql: build/bin/liblkqllang_parse

doc: lkql
	cd user_manual && make clean html

lkql_checker: lkql
	gprbuild -P lkql_checker/lkql_checker.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

lalcheck: lkql
	gprbuild -P lkql_checker/lalcheck.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

build/bin/liblkqllang_parse: lkql/language/parser.py lkql/language/lexer.py
	lkql/manage.py make -P --pass-on="emit railroad diagrams" --enable-build-warnings --build-mode=$(BUILD_MODE) --enable-java

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql_checker clean_lkql clean_lkql_jit

clean_lkql:
	rm lkql/build -rf

clean_lkql_checker:
	gprclean -P lkql_checker/lkql_checker.gpr

clean_lkql_jit:
	cd lkql_jit && mvn clean

lkql_jit: lkql
	cd lkql_jit && mvn package install

lkql_native_jit: lkql
	cd lkql_jit && mvn package install -P native-all

.PHONY: lkql_checker

automated:
	rm -rf "$(PREFIX)"
	mkdir -p "$(PREFIX)/share" "$(PREFIX)/share/examples" "$(PREFIX)/lib"
	$(PYTHON) lkql/manage.py make $(MANAGE_ARGS) $(ADDITIONAL_MANAGE_ARGS)
	$(GPRBUILD) -Plkql_checker/lkql_checker.gpr -largs -s
	$(GPRBUILD) -Plkql_checker/lalcheck.gpr -largs -s
	$(GPRBUILD) -Plkql/liblkqllang_encapsulated -XLIBRARY_TYPE=static-pic -largs -s
	$(GPRINSTALL) --mode=usage -Plkql_checker/lkql_checker.gpr
	$(GPRINSTALL) --mode=usage -Plkql_checker/lalcheck.gpr
	$(GPRINSTALL) --mode=usage -P$(LKQL_DIR)/mains.gpr
	cp -pr lkql_checker/share/lkql "$(PREFIX)/share"
	cp -pr lkql_checker/share/examples "$(PREFIX)/share/examples/gnatcheck"
	cp -p lkql_repl.py "$(PREFIX)/bin"
	cp -pr "$(BUILD_DIR)/lkql/python" "$(PREFIX)/lib"
	cp -p lkql/encapsulated/*$(SOEXT) "$(PREFIX)/lib/python/liblkqllang"

automated-cov:
	rm -rf "$(PREFIX)" "$(BUILD_DIR)"
	mkdir -p "$(PREFIX)/share/lkql" "$(LKQL_DIR)"
	$(PYTHON) lkql/manage.py make $(MANAGE_ARGS) $(ADDITIONAL_MANAGE_ARGS) --coverage
	$(PYTHON) lkql/manage.py install $(MANAGE_ARGS) $(PREFIX)
	# Build and install the lkql_checker program. Instrument it first.
	# Note that we just copy the sources to the build directory since
	# "gnatcov instrument" does not support build tree relocation.
	cp -pr lkql_checker "$(BUILD_DIR)"
	gnatcov instrument "-P$(BUILD_DIR)/lkql_checker/lkql_checker.gpr" \
	  --level=stmt --no-subprojects --dump-trigger=atexit \
	  -XBUILD_MODE=$(BUILD_MODE)
	$(GPRBUILD) "-P$(BUILD_DIR)/lkql_checker/lkql_checker.gpr" \
	  --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts
	$(GPRINSTALL) --mode=dev "-P$(BUILD_DIR)/lkql_checker/lkql_checker.gpr"
	cp -pr lkql_checker/share/lkql "$(PREFIX)/share"
	# Ship coverage data files for liblkqllang and lkql_checker so that the
	# testsuite can use them.
	cp -p "$(LKQL_DIR)/obj/instr/sids/"*.sid "$(PREFIX)/lib/liblkqllang.static"
	mkdir -p "$(PREFIX)/lib/lkql_checker"
	cp -p "$(BUILD_DIR)/lkql_checker/obj/$(BUILD_MODE)/"*.sid \
	  "$(PREFIX)/lib/lkql_checker"

