all: lkql lkql_checker
lkql: build/bin/liblkqllang_parse

lkql_checker:
	gprbuild -P lkql_checker/lkql_checker.gpr -p

build/bin/liblkqllang_parse: lkql/language/parser.py lkql/language/lexer.py
	rm build -rf # We add that because of the lexer invalidation bug
	lkql/manage.py --no-langkit-support make -P

clean: clean_lkql_checker clean_lkql

clean_lkql:
	rm build -rf

clean_lkql_checker:
	gprclean -P lkql_checker/lkql_checker.gpr

.PHONY: lkql_checker
