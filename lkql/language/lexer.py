from langkit.lexer import Lexer, LexerToken, Literal, WithText, WithSymbol, Pattern, Literal,\
                          Ignore


class Token(LexerToken):
    Identifier = WithSymbol()
    String = WithText()
    Integer = WithText()

    Query = WithSymbol()
    When = WithSymbol()
    Is = WithSymbol()
    Print = WithSymbol()
    TrueLit = WithSymbol()
    FalseLit = WithSymbol()

    Dot = WithText()
    Eq = WithText()
    EqEq = WithText()
    And = WithText()
    Or = WithText()
    Plus = WithText()
    Minus = WithText()
    Mul = WithText()
    Div = WithText()
    LPar = WithText()
    RPar = WithText()


lkql_lexer = Lexer(Token)
lkql_lexer.add_rules(
    (Pattern(r"[ \t\n\r]"),                                 Ignore()),
    (Literal("."),                                          Token.Dot),
    (Literal("="),                                          Token.Eq),
    (Literal("=="),                                         Token.EqEq),
    (Literal("&&"),                                         Token.And),
    (Literal("||"),                                         Token.Or),
    (Literal("+"),                                          Token.Plus),
    (Literal("-"),                                          Token.Minus),
    (Literal("*"),                                          Token.Mul),
    (Literal("/"),                                          Token.Div),
    (Literal("("),                                          Token.LPar),
    (Literal(")"),                                          Token.RPar),
    (Literal("query"),                                      Token.Query),
    (Literal("when"),                                       Token.When),
    (Literal("is"),                                         Token.Is),
    (Literal("print"),                                      Token.Print),
    (Literal("true"),                                       Token.TrueLit),
    (Literal("false"),                                      Token.FalseLit),
    (Pattern("[0-9]+"),                                     Token.Integer),
    (Pattern("[a-z][A-Za-z0-9]*"),                          Token.Identifier),
    (Pattern("\"[^\"]*\""),                                 Token.String)
)
