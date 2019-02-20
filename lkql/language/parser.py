from langkit.dsl import ASTNode, abstract, Field
from langkit.parsers import Grammar, Or, List, Pick
from lexer import Token

@abstract
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """
    pass


class Op(LKQLNode):
    enum_node = True
    alternatives = ['plus', 'minus', 'mul', 'div']


class BinOp(LKQLNode):
    left = Field()
    op = Field()
    right = Field()


class Assign(LKQLNode):
    identifier = Field()
    value = Field()


class PrintStmt(LKQLNode):
    value = Field()

class BoolLiteral(LKQLNode):
    enum_node = True
    alternatives = ['true', 'false']

class Identifier(LKQLNode):
    token_node = True

class Integer(LKQLNode):
    token_node = True

class Number(LKQLNode):
    token_node = True

class StringLiteral(LKQLNode):
    token_node = True


lkql_grammar = Grammar('main_rule')
G = lkql_grammar
lkql_grammar.add_rules(
    main_rule=List(Or(G.statement, G.expr)),

    statement = Or(G.assign,
                   G.print_stmt),

    print_stmt = PrintStmt(Token.Print, Token.LPar, G.expr, Token.RPar),

    expr = Or(Pick(Token.LPar, G.expr, Token.RPar),
              BinOp(G.expr,
                  Or(Op.alt_plus(Token.Plus),
                     Op.alt_minus(Token.Minus),
                     Op.alt_mul(Token.Mul),
                     Op.alt_div(Token.Div)),
                  G.expr),
              G.identifier,
              G.number,
              G.string_literal,
              G.assign),

    assign = Assign(G.identifier, Token.Eq, G.expr),

    identifier = Identifier(Token.Identifier),

    integer = Integer(Token.Integer),

    number = Number(Token.Number),

    string_literal = StringLiteral(Token.String)
)
