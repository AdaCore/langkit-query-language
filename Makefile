BUILD_MODE=dev
export BUILD_MODE

ifeq ($(OS),Windows_NT)
  SOEXT=.dll
else
  SOEXT=.so
endif

PROCS=0 PREFIX=install
PYTHON=python
MAVEN=mvn
NPM_INSTALL_CACHE=true
NPMRC=
BUILD_DIR=/undefined
LKQL_DIR=$(BUILD_DIR)/lkql
IMPACTDB_DIR=/undefined
GPRBUILD=gprbuild -j$(PROCS) -p -XBUILD_MODE=$(BUILD_MODE)
GPRINSTALL=gprinstall --prefix=$(PREFIX) -p -XBUILD_MODE=$(BUILD_MODE)
BUILD_FOR_JIT=false
LKM=$(PYTHON) -m langkit.scripts.lkm

ifeq ($(BUILD_FOR_JIT),true)
  MANAGE_ARGS=--build-dir=$(LKQL_DIR) --build-mode=$(BUILD_MODE) \
    --library-types=relocatable
else
  MANAGE_ARGS=--build-dir=$(LKQL_DIR) --build-mode=$(BUILD_MODE) \
    --library-types=static
endif

ADDITIONAL_MANAGE_ARGS=
MAVEN_ARGS=-Dconfig.npmInstallCache=$(NPM_INSTALL_CACHE) -Dconfig.npmrc=$(NPMRC) -Dconfig.python=$(PYTHON)

# WARNING: Note that for some reason parallelizing the build still doesn't work
all: lkql gnatcheck build_lkql_native_jit doc

lkql: build/bin/liblkqllang_parse

doc: build_lkql_native_jit
	cd user_manual && make clean html
	cd lkql_checker/doc && make generate html-all

impacts:
	PYTHONPATH=$(IMPACTDB_DIR) ./utils/impact-db_impacts_gen.py $(IMPACTDB_DIR)

format:
	gnatformat -P lkql_checker/gnatcheck.gpr --no-subprojects
	$(MAVEN) -f lkql_jit spotless:apply $(MAVEN_ARGS)

gnatcheck: lkql
	gnatformat -P lkql_checker/gnatcheck.gpr --no-subprojects --check
	gprbuild -P lkql_checker/gnatcheck.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

build/bin/liblkqllang_parse: lkql/lkql.lkt
	$(LKM) make -c lkql/langkit.yaml \
	--pass-on="emit railroad diagrams" \
	--enable-java \
	--maven-executable $(MAVEN) \
	--build-mode=$(BUILD_MODE) \
	$(ADDITIONAL_MANAGE_ARGS)

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql clean_lkql_jit

clean_lkql:
	rm lkql/build -rf

clean_lkql_jit:
	cd lkql_jit && $(MAVEN) clean

build_lkql_jit: lkql
	$(MAVEN) -f lkql/build/java/ install
	$(MAVEN) -f lkql_jit/ clean install $(MAVEN_ARGS)

build_lkql_native_jit: lkql
	$(MAVEN) -f lkql/build/java/ install
	$(MAVEN) -f lkql_jit/ clean install -P native,$(BUILD_MODE) $(MAVEN_ARGS)

.PHONY: lkql_checker

automated:
	rm -rf "$(PREFIX)"
	mkdir -p "$(PREFIX)/share" "$(PREFIX)/share/examples" "$(PREFIX)/lib"
	$(LKM) make -c lkql/langkit.yaml $(MANAGE_ARGS) $(ADDITIONAL_MANAGE_ARGS)
	gnatformat -P lkql_checker/gnatcheck.gpr --check --no-subprojects
	$(GPRBUILD) -Plkql_checker/gnatcheck.gpr -largs -s
	$(GPRINSTALL) --mode=usage -Plkql_checker/gnatcheck.gpr
	$(GPRINSTALL) --mode=usage -P$(LKQL_DIR)/mains.gpr
	cp -pr lkql_checker/share/lkql "$(PREFIX)/share"
	cp -pr lkql_checker/share/examples "$(PREFIX)/share/examples/gnatcheck"

automated-cov:
	rm -rf "$(PREFIX)" "$(BUILD_DIR)"
	mkdir -p "$(PREFIX)/share/lkql" "$(LKQL_DIR)"
	$(LKM) make -c lkql/langkit.yaml $(MANAGE_ARGS) $(ADDITIONAL_MANAGE_ARGS) --coverage
	$(LKM) install -c lkql/langkit.yaml $(MANAGE_ARGS) $(PREFIX)
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
