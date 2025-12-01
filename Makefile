BUILD_MODE=dev
PROCS=0
PREFIX=install
PYTHON=python
MAVEN=mvn
NPM_INSTALL_CACHE=true
NPMRC=
BUILD_DIR=/undefined
LKQL_DIR=$(BUILD_DIR)/lkql
GPRBUILD=gprbuild -j$(PROCS) -p -XBUILD_MODE=$(BUILD_MODE)
GPRINSTALL=gprinstall --prefix=$(PREFIX) -p -XBUILD_MODE=$(BUILD_MODE)
BUILD_FOR_JIT=false
LKM=$(PYTHON) -m langkit.scripts.lkm
KP_JSON=lkql_checker/share/lkql/kp/kp.json

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

doc:
	cd user_manual && make clean html
	cd lkql_checker/doc && make generate html-all

impacts:
	[ -f "$(KP_JSON)" ] || "$(PYTHON)" "./utils/impact-db_impacts_gen.py"

format:
	gnatformat -P lkql_checker/lkql_checker.gpr --no-subprojects
	$(MAVEN) -f lkql_jit spotless:apply $(MAVEN_ARGS)

gnatcheck: lkql impacts
	gprbuild -P lkql_checker/lkql_checker.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

build/bin/liblkqllang_parse: lkql/lkql.lkt
	$(LKM) make -c lkql/langkit.yaml \
	--pass-on="emit railroad diagrams" \
	--enable-java \
	--maven-executable $(MAVEN) \
	--build-mode=$(BUILD_MODE) \
	$(ADDITIONAL_MANAGE_ARGS)

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql_jit clean_lkql_checker clean_lkql

clean_lkql:
	rm lkql/build -rf

clean_lkql_jit:
	$(MAVEN) -f lkql_jit clean $(MAVEN_ARGS)

clean_lkql_checker:
	cd lkql_checker && gprclean
	[ -f $(KP_JSON) ] && rm $(KP_JSON)

build_lkql_native_jit: lkql
	$(MAVEN) -f lkql/build/java/ install
	$(MAVEN) -f lkql_jit/ clean package -P native,$(BUILD_MODE) $(MAVEN_ARGS)

.PHONY: lkql_checker

automated:
	rm -rf "$(PREFIX)"
	mkdir -p "$(PREFIX)/share" "$(PREFIX)/share/examples" "$(PREFIX)/lib"
	$(LKM) make -c lkql/langkit.yaml $(MANAGE_ARGS) $(ADDITIONAL_MANAGE_ARGS)
	$(GPRBUILD) -Plkql_checker/lkql_checker.gpr -largs -s
	$(GPRINSTALL) --mode=usage -Plkql_checker/lkql_checker.gpr
	$(GPRINSTALL) --mode=usage -P$(LKQL_DIR)/mains.gpr
	cp -pr lkql_checker/share/lkql "$(PREFIX)/share"
	cp -pr lkql_checker/share/examples "$(PREFIX)/share/examples/gnatcheck"
