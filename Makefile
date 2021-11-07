BUILD_MODE=dev
PREFIX=install

all: lkql lkql_checker lalcheck doc
lkql: build/bin/liblkqllang_parse

automated:
	lkql/manage.py make -P --pass-on="emit railroad diagrams" --enable-build-warnings --build-mode=prod
	gprbuild -j0 -P lkql_checker/lkql_checker.gpr -p -XBUILD_MODE=prod
	gprbuild -j0 -P lkql_checker/lalcheck.gpr -p -XBUILD_MODE=prod
	gprinstall --prefix=$(PREFIX) --mode=usage -p -Plkql_checker/lkql_checker.gpr -XBUILD_MODE=prod
	gprinstall --prefix=$(PREFIX) --mode=usage -p -Plkql_checker/lalcheck.gpr -XBUILD_MODE=prod

automated-cov:
	lkql/manage.py make -P --pass-on="emit railroad diagrams" --enable-build-warnings --build-mode=prod --coverage --library-types=static
	lkql/manage.py install --build-mode=prod --coverage --library-types=static $(PREFIX)
	gnatcov instrument -Plkql_checker/lkql_checker.gpr --level=stmt --no-subprojects --dump-trigger=atexit
	gprbuild -j0 -p -Plkql_checker/lkql_checker.gpr -XBUILD_MODE=prod --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full
	gprinstall --prefix=$(PREFIX) --mode=dev -p -Plkql_checker/lkql_checker.gpr -XBUILD_MODE=prod
	# Ship coverage data files for liblkqllang and lkql_checker so that the
	# testsuite can use them.
	cp -p lkql/build/obj/instr/sids/*.sid $(PREFIX)/lib/liblkqllang.static
	mkdir -p $(PREFIX)/lib/lkql_checker
	cp -p lkql_checker/obj/prod/*.sid $(PREFIX)/lib/lkql_checker

doc:
	cd user_manual && make clean html

lkql_checker:
	gprbuild -P lkql_checker/lkql_checker.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

lalcheck:
	gprbuild -P lkql_checker/lalcheck.gpr -p $(GPR_ARGS) -XBUILD_MODE=$(BUILD_MODE)

build/bin/liblkqllang_parse: lkql/language/parser.py lkql/language/lexer.py
	lkql/manage.py make -P --pass-on="emit railroad diagrams" --enable-build-warnings --build-mode=$(BUILD_MODE)

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql_checker clean_lkql

clean_lkql:
	rm lkql/build -rf

clean_lkql_checker:
	gprclean -P lkql_checker/lkql_checker.gpr

.PHONY: lkql_checker
