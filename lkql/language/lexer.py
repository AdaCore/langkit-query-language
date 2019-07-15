from langkit.lexer import Lexer, LexerToken, WithText, WithSymbol, Pattern,\
                            Literal, Ignore, WithTrivia


class Token(LexerToken):
    Identifier = WithSymbol()
    KindName = WithSymbol()
    String = WithText()
    Integer = WithText()

    Let = WithSymbol()
    QueryTok = WithSymbol()
    When = WithSymbol()
    Val = WithSymbol()
    Fun = WithSymbol()
    Selector = WithSymbol()
    Match = WithSymbol()
    Rec = WithSymbol()
    Skip = WithSymbol()
    Is = WithSymbol()
    In = WithSymbol()
    TrueLit = WithSymbol()
    FalseLit = WithSymbol()
    If = WithSymbol()
    Then = WithSymbol()
    Else = WithSymbol()
    Not = WithSymbol()
    Null = WithSymbol()
    Prototype = WithSymbol()
    AstNode = WithSymbol()

    Dot = WithText()
    QuestionDot = WithText()
    Coma = WithText()
    SemiCol = WithText()
    Colon = WithText()
    UnderScore = WithText()
    Eq = WithText()
    EqEq = WithText()
    Neq = WithText()
    ExclExcl = WithText()
    Lt = WithText()
    LEq = WithText()
    Gt = WithText()
    GEq = WithText()
    And = WithText()
    Or = WithText()
    Plus = WithText()
    Minus = WithText()
    Mul = WithText()
    Div = WithText()
    Amp = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrack = WithText()
    RBrack = WithText()
    LCurl = WithText()
    RCurl = WithText()
    At = WithText()
    Pipe = WithText()
    LArrow = WithText()
    RArrow = WithText()
    BigRArrow = WithText()
    Box = WithText()

    Comment = WithTrivia()

    Builtin = WithText()


lkql_lexer = Lexer(Token)
lkql_lexer.add_rules(
    (Pattern(r"[ \t\n\r]"),                                 Ignore()),
    (Literal("."),                                          Token.Dot),
    (Literal("?."),                                         Token.QuestionDot),
    (Literal(","),                                          Token.Coma),
    (Literal(";"),                                          Token.SemiCol),
    (Literal(":"),                                          Token.Colon),
    (Literal("_"),                                          Token.UnderScore),
    (Literal("="),                                          Token.Eq),
    (Literal("=="),                                         Token.EqEq),
    (Literal("!="),                                         Token.Neq),
    (Literal("!!"),                                         Token.ExclExcl),
    (Literal("<"),                                          Token.Lt),
    (Literal("<="),                                         Token.LEq),
    (Literal(">"),                                          Token.Gt),
    (Literal(">="),                                         Token.GEq),
    (Literal("&&"),                                         Token.And),
    (Literal("||"),                                         Token.Or),
    (Literal("+"),                                          Token.Plus),
    (Literal("-"),                                          Token.Minus),
    (Literal("*"),                                          Token.Mul),
    (Literal("/"),                                          Token.Div),
    (Literal("&"),                                          Token.Amp),
    (Literal("("),                                          Token.LPar),
    (Literal(")"),                                          Token.RPar),
    (Literal("["),                                          Token.LBrack),
    (Literal("]"),                                          Token.RBrack),
    (Literal("{"),                                          Token.LCurl),
    (Literal("}"),                                          Token.RCurl),
    (Literal("@"),                                          Token.At),
    (Literal("|"),                                          Token.Pipe),
    (Literal("<-"),                                         Token.LArrow),
    (Literal("->"),                                         Token.RArrow),
    (Literal("=>"),                                         Token.BigRArrow),
    (Literal("<>"),                                         Token.Box),
    (Literal("BUILTIN_DECL"),                               Token.Builtin),
    (Literal("let"),                                        Token.Let),
    (Literal("query"),                                      Token.QueryTok),
    (Literal("when"),                                       Token.When),
    (Literal("val"),                                        Token.Val),
    (Literal("fun"),                                        Token.Fun),
    (Literal("selector"),                                   Token.Selector),
    (Literal("match"),                                      Token.Match),
    (Literal("rec"),                                        Token.Rec),
    (Literal("skip"),                                       Token.Skip),
    (Literal("is"),                                         Token.Is),
    (Literal("in"),                                         Token.In),
    (Literal("true"),                                       Token.TrueLit),
    (Literal("false"),                                      Token.FalseLit),
    (Literal("if"),                                         Token.If),
    (Literal("else"),                                       Token.Else),
    (Literal("then"),                                       Token.Then),
    (Literal("not"),                                        Token.Not),
    (Literal("null"),                                       Token.Null),
    (Literal("prototype"),                                  Token.Prototype),
    (Literal("astnode"),                                    Token.AstNode),
    (Pattern("[0-9]+"),                                     Token.Integer),
    (Pattern("[a-z][A-Za-z0-9_]*"),                         Token.Identifier),
    (Pattern("[A-Z][A-Za-z_]*(.list)?"),                    Token.KindName),
    (Pattern("\"[^\"]*\""),                                 Token.String),
    (Pattern(r"//(.?)+"),                                   Token.Comment),
)
