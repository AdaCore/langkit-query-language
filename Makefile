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
LKM=$(PYTHON) -m langkit.scripts.lkm
KP_JSON=lkql_checker/share/lkql/kp/kp.json
ADDITIONAL_LKM_ARGS=
LKM_ARGS=--build-mode=$(BUILD_MODE) --library-types=relocatable --maven-executable $(MAVEN) $(ADDITIONAL_LKM_ARGS)
MAVEN_ARGS=-Dconfig.npmInstallCache=$(NPM_INSTALL_CACHE) -Dconfig.npmrc=$(NPMRC) -Dconfig.python=$(PYTHON)

all: liblkqllang lkql_jit lkql_checker doc

liblkqllang:
	$(LKM) make -c lkql/langkit.yaml \
	--pass-on="emit railroad diagrams" \
	--disable-java \
	$(LKM_ARGS)

install_lkql_java_bindings: liblkqllang
	$(MAVEN) -f lkql/build/java/ install $(MAVEN_ARGS)

lkql_jit: install_lkql_java_bindings
	$(MAVEN) -f lkql_jit/ clean package -P native,$(BUILD_MODE) $(MAVEN_ARGS)

lkql_checker: liblkqllang impacts
	gprbuild -P lkql_checker/lkql_checker.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

doc:
	cd user_manual && make clean html
	cd lkql_checker/doc && make generate html-all

impacts:
	[ -f "$(KP_JSON)" ] || "$(PYTHON)" "./utils/impact-db_impacts_gen.py"

format:
	gnatformat -P lkql_checker/lkql_checker.gpr --no-subprojects
	$(MAVEN) -f lkql_jit spotless:apply $(MAVEN_ARGS)

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql_jit clean_lkql_checker clean_liblkqllang

clean_liblkqllang:
	rm lkql/build -rf

clean_lkql_jit:
	$(MAVEN) -f lkql_jit clean $(MAVEN_ARGS)

clean_lkql_checker:
	cd lkql_checker && gprclean
	[ -f $(KP_JSON) ] && rm $(KP_JSON)

.PHONY: liblkqllang install_lkql_java_bindings lkql_jit gnatcheck doc impacts format test clean_liblkqllang clean_lkql_jit clean_gnatcheck

automated:
	rm -rf "$(PREFIX)"
	mkdir -p "$(PREFIX)/share" "$(PREFIX)/share/examples" "$(PREFIX)/lib"
	$(LKM) make -c lkql/langkit.yaml $(LKM_ARGS)
	$(GPRBUILD) -Plkql_checker/lkql_checker.gpr -largs -s
	$(GPRINSTALL) --mode=usage -Plkql_checker/lkql_checker.gpr
	$(GPRINSTALL) --mode=usage -P$(LKQL_DIR)/mains.gpr
	cp -pr lkql_checker/share/lkql "$(PREFIX)/share"
	cp -pr lkql_checker/share/examples "$(PREFIX)/share/examples/gnatcheck"
