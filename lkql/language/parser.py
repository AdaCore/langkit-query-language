from langkit.dsl import ASTNode, abstract, Field
from langkit.parsers import Grammar, Or, List, Pick
from lexer import Token

@abstract
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """
    pass


@abstract
class Op(LKQLNode):
    """
    Base class for operators.
    """
    enum_node = True
    alternatives = ['plus', 'minus', 'mul', 'div', 'and', 'or', 'eq']


class BinOp(LKQLNode):
    """
    Binary operation.
    """
    left = Field()
    op = Field()
    right = Field()


class Assign(LKQLNode):
    """
    Assign expression.
    """
    identifier = Field()
    value = Field()


class PrintStmt(LKQLNode):
    """
    `print` built-in.
    """
    value = Field()

class BoolLiteral(LKQLNode):
    """
    Booean literal
    """
    enum_node = True
    alternatives = ['true', 'false']

class Identifier(LKQLNode):
    """
    Regular identifier.
    """
    token_node = True

class Integer(LKQLNode):
    """
    Integer literal.
    """
    token_node = True

class Number(LKQLNode):
    """
    Decimal number literal.
    """
    token_node = True

class StringLiteral(LKQLNode):
    """
    String literal.
    """
    token_node = True

class DotAccess(LKQLNode):
    """
    Access to a node's field using dot notaion.
    """
    receiver = Field()
    member = Field()


lkql_grammar = Grammar('main_rule')
G = lkql_grammar
lkql_grammar.add_rules(
    main_rule=List(Or(G.statement, G.expr)),

    statement = Or(G.assign,
                   G.print_stmt),

    print_stmt = PrintStmt(Token.Print, Token.LPar, G.expr, Token.RPar),

    expr = Or(Pick(Token.LPar, G.expr, Token.RPar),
              BinOp(G.expr,
                    Or(Op.alt_and(Token.And),
                       Op.alt_or(Token.Or),
                       Op.alt_eq(Token.EqEq)),
                    G.plus_expr),
              G.plus_expr,
              G.assign),


    plus_expr = Or(BinOp(G.plus_expr,
                         Or(Op.alt_plus(Token.Plus),
                            Op.alt_minus(Token.Minus)),
                         G.prod_expr),
                   G.prod_expr),

    prod_expr = Or(BinOp(G.prod_expr,
                         Or(Op.alt_mul(Token.Mul),
                            Op.alt_div(Token.Div)),
                         G.value_expr),
                   G.value_expr),

    value_expr = Or(G.identifier,
                    G.number,
                    G.string_literal,
                    G.bool_literal,
                    G.integer,
                    G.assign,
                    G.dot_access),

    assign = Assign(G.identifier, Token.Eq, G.expr),

    identifier = Identifier(Token.Identifier),

    integer = Integer(Token.Integer),

    number = Number(Token.Number),

    bool_literal = Or(BoolLiteral.alt_true(Token.TrueLit),
                      BoolLiteral.alt_false(Token.FalseLit)),

    string_literal = StringLiteral(Token.String),

    dot_access = DotAccess(G.identifier, Token.Dot, G.identifier, Token.LPar, Token.RPar)
)
