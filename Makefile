BUILD_MODE=dev
PROCS=0
LANGKIT_PYTHON=python
MAVEN=mvn
GPRBUILD=gprbuild -j$(PROCS) -p -XBUILD_MODE=$(BUILD_MODE)
LKM="$(LANGKIT_PYTHON)" -m langkit.scripts.lkm
ADDITIONAL_LKM_ARGS=
LKM_ARGS=--build-mode=$(BUILD_MODE) --library-types=relocatable --maven-executable $(MAVEN) -j$(PROCS) $(ADDITIONAL_LKM_ARGS)
ADDITIONAL_MAVEN_ARGS=
MAVEN_ARGS=-Dconfig.python="$(LANGKIT_PYTHON)" $(ADDITIONAL_MAVEN_ARGS)

all: liblkqllang lkql_jit

liblkqllang:
	$(LKM) make -c lkql/langkit.yaml \
	--pass-on="emit railroad diagrams" \
	--disable-java \
	$(LKM_ARGS)

install_lkql_java_bindings: liblkqllang
	"$(MAVEN)" -f lkql/build/java/ install $(MAVEN_ARGS)

lkql_jit: install_lkql_java_bindings
	"$(MAVEN)" -f lkql_jit/ clean package -P native,$(BUILD_MODE) $(MAVEN_ARGS)

format:
	"$(MAVEN)" -f lkql_jit spotless:apply $(MAVEN_ARGS)

test:
	testsuite/testsuite.py -j$(PROCS) -Edtmp

clean: clean_lkql_jit clean_liblkqllang

clean_liblkqllang:
	rm lkql/build -rf

clean_lkql_jit:
	"$(MAVEN)" -f lkql_jit clean $(MAVEN_ARGS)

.PHONY: liblkqllang install_lkql_java_bindings lkql_jit format test clean_liblkqllang clean_lkql_jit
