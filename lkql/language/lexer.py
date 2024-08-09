from langkit.lexer import (
    Lexer, LexerToken, WithText, WithSymbol, Pattern, Literal, WithTrivia
)


class Token(LexerToken):
    Identifier = WithSymbol()
    UpperIdentifier = WithSymbol()
    String = WithText(tm_scope="string.quoted.double")
    Integer = WithText(tm_scope="constant.numeric", tm_bounded=True)

    SelectTok = WithSymbol(tm_scope="keyword.other.query", tm_bounded=True)
    FromTok = WithSymbol(tm_scope="keyword.other.query", tm_bounded=True)
    ThroughTok = WithSymbol(tm_scope="keyword.other.query", tm_bounded=True)
    When = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Match = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Val = WithSymbol(tm_scope="storage.type", tm_bounded=True)
    Fun = WithSymbol(tm_scope="storage.type", tm_bounded=True)
    Selector = WithSymbol(tm_scope="storage.type", tm_bounded=True)
    Rec = WithSymbol()
    For = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Is = WithSymbol(tm_scope="keyword.operator", tm_bounded=True)
    In = WithSymbol(tm_scope="keyword.operator", tm_bounded=True)
    TrueLit = WithSymbol(tm_scope="constant.language", tm_bounded=True)
    FalseLit = WithSymbol(tm_scope="constant.language", tm_bounded=True)
    If = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Then = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Else = WithSymbol(tm_scope="keyword.control", tm_bounded=True)
    Not = WithSymbol(tm_scope="keyword.operator", tm_bounded=True)
    Null = WithSymbol(tm_scope="constant.language", tm_bounded=True)
    Import = WithSymbol(tm_scope="keyword.control", tm_bounded=True)

    Dot = WithText()
    DotDotDot = WithText()
    Question = WithText()
    Coma = WithText()
    SemiCol = WithText()
    Colon = WithText()
    UnderScore = WithText()
    Eq = WithText()
    EqEq = WithText(tm_scope="keyword.operator")
    Neq = WithText(tm_scope="keyword.operator")
    ExclExcl = WithText()
    Lt = WithText(tm_scope="keyword.operator")
    LEq = WithText(tm_scope="keyword.operator")
    Gt = WithText(tm_scope="keyword.operator")
    GEq = WithText(tm_scope="keyword.operator")
    And = WithText(tm_scope="keyword.operator")
    Or = WithText(tm_scope="keyword.operator")
    Plus = WithText(tm_scope="keyword.operator")
    Minus = WithText(tm_scope="keyword.operator")
    Mul = WithText(tm_scope="keyword.operator")
    Div = WithText(tm_scope="keyword.operator")
    Amp = WithText(tm_scope="keyword.operator")
    LPar = WithText(start_tm_rule="Parented", tm_scope="punctuation.paren.open", content_tm_patterns=["#all_rules"])
    RPar = WithText(end_tm_rule="Parented", tm_scope="punctuation.paren.close")
    LBrack = WithText(start_tm_rule="Bracketed", tm_scope="punctuation.brack.open", content_tm_patterns=["#all_rules"])
    RBrack = WithText(end_tm_rule="Bracketed", tm_scope="punctuation.brack.close")
    LCurl = WithText(start_tm_rule="Curled", tm_scope="punctuation.curl.open", content_tm_patterns=["#all_rules"])
    RCurl = WithText(end_tm_rule="Curled", tm_scope="punctuation.curl.close")
    At = WithText()
    Pipe = WithText(tm_scope="keyword.operator")
    BigRArrow = WithText(tm_scope="keyword.operator")
    SubBlockLiteral = WithText(tm_scope="comment.block.documentation")

    Comment = WithTrivia(tm_scope="comment.line.number-sign")
    Whitespace = WithTrivia()


lkql_lexer = Lexer(Token)
lkql_lexer.add_rules(
    (Pattern(r"[ \t\n\r]"),            Token.Whitespace),
    (Literal("..."),                   Token.DotDotDot),
    (Literal("?"),                     Token.Question),
    (Literal("."),                     Token.Dot),
    (Literal(","),                     Token.Coma),
    (Literal(";"),                     Token.SemiCol),
    (Literal(":"),                     Token.Colon),
    (Literal("!!"),                    Token.ExclExcl),
    (Literal("_"),                     Token.UnderScore),
    (Literal("=>"),                    Token.BigRArrow),
    (Literal("=="),                    Token.EqEq),
    (Literal("!="),                    Token.Neq),
    (Literal("<="),                    Token.LEq),
    (Literal(">="),                    Token.GEq),
    (Literal("<"),                     Token.Lt),
    (Literal(">"),                     Token.Gt),
    (Literal("="),                     Token.Eq),
    (Literal("and"),                   Token.And),
    (Literal("or"),                    Token.Or),
    (Literal("+"),                     Token.Plus),
    (Literal("-"),                     Token.Minus),
    (Literal("*"),                     Token.Mul),
    (Literal("/"),                     Token.Div),
    (Literal("&"),                     Token.Amp),
    (Literal("("),                     Token.LPar),
    (Literal(")"),                     Token.RPar),
    (Literal("{"),                     Token.LCurl),
    (Literal("}"),                     Token.RCurl),
    (Literal("["),                     Token.LBrack),
    (Literal("]"),                     Token.RBrack),
    (Literal("@"),                     Token.At),
    (Pattern(r"\|\"(.?)+"),            Token.SubBlockLiteral),
    (Literal("|"),                     Token.Pipe),
    (Literal("select"),                Token.SelectTok),
    (Literal("from"),                  Token.FromTok),
    (Literal("through"),               Token.ThroughTok),
    (Literal("when"),                  Token.When),
    (Literal("val"),                   Token.Val),
    (Literal("fun"),                   Token.Fun),
    (Literal("import"),                Token.Import),
    (Literal("selector"),              Token.Selector),
    (Literal("match"),                 Token.Match),
    (Literal("rec"),                   Token.Rec),
    (Literal("for"),                   Token.For),
    (Literal("is"),                    Token.Is),
    (Literal("in"),                    Token.In),
    (Literal("true"),                  Token.TrueLit),
    (Literal("false"),                 Token.FalseLit),
    (Literal("if"),                    Token.If),
    (Literal("else"),                  Token.Else),
    (Literal("then"),                  Token.Then),
    (Literal("not"),                   Token.Not),
    (Literal("null"),                  Token.Null),
    (Pattern("[0-9]+"),                Token.Integer),
    (Pattern("[a-z][A-Za-z0-9_]*"),    Token.Identifier),
    (Pattern("[A-Za-z][A-Za-z0-9_]*"), Token.UpperIdentifier),
    (Pattern(r'"(\\.|[^"])*"'),        Token.String),
    (Pattern(r"#(.?)+"),               Token.Comment),
)
