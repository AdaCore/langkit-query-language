all: lkql lkql_checker doc
lkql: build/bin/liblkqllang_parse

doc:
	cd user_manual && make clean html

lkql_checker:
	gprbuild -P lkql_checker/lkql_checker.gpr -p $(GPR_ARGS)

build/bin/liblkqllang_parse: lkql/language/parser.py lkql/language/lexer.py
	lkql/manage.py make -P --pass-on="emit railroad diagrams"

test:
	testsuite/testsuite.py -Edtmp

clean: clean_lkql_checker clean_lkql

clean_lkql:
	rm lkql/build -rf

clean_lkql_checker:
	gprclean -P lkql_checker/lkql_checker.gpr

.PHONY: lkql_checker
