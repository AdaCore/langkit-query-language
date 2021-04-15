" Vim syntax file
" Language: lkql

if exists("b:current_syntax")
  finish
endif

syn keyword lkqlKeyword let select val match import nextgroup=lkqlEntity skipwhite
syn keyword lkqlKeyword fun selector nextgroup=lkqlFunction skipwhite
syn match   lkqlFunction	"\h\w*" display contained
syn match   lkqlEntity	"\h\w*" display contained
syn keyword lkqlKeyword when rec for skip is in true false if else then not null
syn match   lkqlQualifier "\<[A-Z]\w*"

syn keyword lkqlQuantifier no all any *

syn region  lkqlString  start=+"+ skip=+\\"+ end=+"+
syn region  lkqlChar  start=+'+ skip=+\\"+ end=+'+
syn match   lkqlLiteral "\d\+" display
syn match   lkqlComment "#.*$"
syn match   lkqlBlockString +|".*$+
syn match   lkqlOperator "=="
syn match   lkqlOperator "!="
syn match   lkqlOperator "*"
syn match   lkqlOperator "-"
syn match   lkqlOperator "/"
syn match   lkqlOperator "&"
syn match   lkqlOperator "<-"
syn match   lkqlOperator "=>"
syn match   lkqlOperator "!!"
syn match   lkqlOperator "<"
syn match   lkqlOperator ">"
syn match   lkqlOperator ">="
syn match   lkqlOperator "<="
syn match   lkqlOperator "="
syn match   lkqlOperator "@"

hi def link lkqlKeyword     Statement
hi def link lkqlQualifier   Identifier
hi def link lkqlQuantifier  Identifier
hi def link lkqlString      String
hi def link lkqlChar        String
hi def link lkqlOperator    Special
hi def link lkqlFunction    Define
hi def link lkqlEntity      Function
hi def link lkqlToken       Define
hi def link lkqlGrammarRule Define
hi def link lkqlLiteral     Number
hi def link lkqlComment     Comment
hi def link lkqlBlockString  String
