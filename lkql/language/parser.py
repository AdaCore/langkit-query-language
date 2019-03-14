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
class Expr(LKQLNode):
    """
    Root node class for LKQL expressions.
    """
    pass


@abstract
class Op(LKQLNode):
    """
    Base class for operators.
    """
    enum_node = True
    alternatives = [
        'plus', 'minus', 'mul', 'div', 'and', 'or', 'eq', 'neq', 'concat'
    ]


class BoolLiteral(Expr):
    """
    Booean literal
    """
    enum_node = True
    alternatives = ['true', 'false']


class Identifier(Expr):
    """
    Regular identifier.
    """
    token_node = True


class Integer(Expr):
    """
    Integer literal.
    """
    token_node = True


class StringLiteral(Expr):
    """
    String literal.
    """
    token_node = True


class BinOp(Expr):
    """
    Binary operation.
    """
    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)


class Assign(Expr):
    """
    Assign expression.
    An assignment associates a name with a value, and returns Unit.

    For instance::
       let message = "Hello World"
    """
    identifier = Field(type=Identifier)
    value = Field(type=Expr)


class PrintStmt(Expr):
    """
    `print` built-in.
    """
    value = Field(type=Expr)


class DotAccess(Expr):
    """
    Access to a node's field using dot notation.
    """
    receiver = Field(type=Expr)
    member = Field(type=Identifier)


class Query(Expr):
    """
    AST query.

    This corresponds to a 'query <identifier> when condition' block,
    where 'identifier' is a regular identifier bound to the "current" node and
    'condition' is a predicate.

    Queries are implicitly run from the root of the AST and return the list of
    children nodes that matches the condition.

    For instance::
       let classesNamedA = query n when n is ClassDecl &&
                                        n.Identifier == "A"

    """
    binding = Field(type=Identifier)
    when_clause = Field(type=Expr)


class IsClause(Expr):
    """
    Check a node's kind using the 'is' keyword.
    """
    node_expr = Field(type=Expr)
    kind_name = Field(type=Identifier)


class InClause(Expr):
    """
    Check that a list contains a given value using the 'in' keyword
    """
    value_expr = Field(type=Expr)
    list_expr = Field(type=Expr)


class Indexing(Expr):
    """
    Access to the nth element of a List or String

    For instance::
       values[0]
    """
    collection_expr = Field(type=Expr)
    index_expr = Field(type=Expr)


lkql_grammar = Grammar('main_rule')
G = lkql_grammar
lkql_grammar.add_rules(
    main_rule=List(Or(G.statement, G.expr, G.query)),

    statement=Or(G.assign,
                 G.print_stmt),

    print_stmt=PrintStmt(Token.Print, Token.LPar, G.expr, Token.RPar),

    query=Query(Token.Query, G.identifier, Token.When, G.expr),

    expr=Or(BinOp(G.expr,
                  Or(Op.alt_and(Token.And),
                     Op.alt_or(Token.Or)),
                  G.comp_expr),
            G.comp_expr),

    comp_expr=Or(IsClause(G.comp_expr, Token.Is, G.kind_name),
                 InClause(G.comp_expr, Token.In, G.expr),
                 BinOp(G.comp_expr,
                       Or(Op.alt_eq(Token.EqEq),
                          Op.alt_neq(Token.Neq),
                          Op.alt_concat(Token.Amp)),
                       G.plus_expr),
                 G.plus_expr),

    plus_expr=Or(BinOp(G.plus_expr,
                       Or(Op.alt_plus(Token.Plus),
                          Op.alt_minus(Token.Minus)),
                       G.prod_expr),
                 G.prod_expr),

    prod_expr=Or(BinOp(G.prod_expr,
                       Or(Op.alt_mul(Token.Mul),
                          Op.alt_div(Token.Div)),
                       G.value_expr),
                 G.value_expr),

    value_expr=Or(DotAccess(G.value_expr, Token.Dot, G.identifier),
                  G.assign,
                  Indexing(G.value_expr, Token.LBrack, G.expr, Token.RBrack),
                  G.identifier,
                  G.string_literal,
                  G.bool_literal,
                  G.integer,
                  Pick(Token.LPar, G.expr, Token.RPar)),

    assign=Assign(Token.Let, G.identifier, Token.Eq, Or(G.expr, G.query)),

    identifier=Identifier(Token.Identifier),

    kind_name=Identifier(Token.KindName),

    integer=Integer(Token.Integer),

    bool_literal=Or(BoolLiteral.alt_true(Token.TrueLit),
                    BoolLiteral.alt_false(Token.FalseLit)),

    string_literal=StringLiteral(Token.String),
)
