all: lkql lkql_checker
lkql: build/bin/liblkqllang_parse

lkql_checker:
	gprbuild -P lkql_checker/lkql_checker.gpr -p

build/bin/liblkqllang_parse: lkql/language/parser.py lkql/language/lexer.py
	lkql/manage.py make -P

clean: clean_lkql_checker clean_lkql

clean_lkql:
	rm build -rf

clean_lkql_checker:
	gprclean -P lkql_checker/lkql_checker.gpr

.PHONY: lkql_checker
