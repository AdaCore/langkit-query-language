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


@abstract
class NodePattern(LKQLNode):
    """
    Root node class for node patterns
    """
    pass


class BindingNodePattern(NodePattern):
    """
    Node pattern comprising only a binding identifier

    For instance::
       let decls = query o ...
    """
    binding = Field(type=Identifier)


class KindNodePattern(NodePattern):
    """
    Node pattern comprising only a kind name

    For instance::
       let decls = query ObjectDecl ...
    """
    identifier = Field(type=Identifier)


class FullNodePattern(NodePattern):
    """
    Complete node pattern of the form: binding @ KindName

    For instance::
       let decls = query o@ObjectDecl ...
    """
    binding_pattern = Field(type=BindingNodePattern)
    kind_pattern = Field(type=KindNodePattern)


@abstract
class SelectorPattern(LKQLNode):
    """
    Root node for selector patterns
    """
    pass


class NamedSelector(SelectorPattern):
    """
    Selector comprising only a selector name.
    Used to specify the relationship between the node being queried and some
    other nodes.

    For instance::
       query p [children] ObjectDecl ...
    """
    name = Field(type=Identifier)


class QuantifiedSelector(SelectorPattern):
    """
    Selector of the form: [quantifier selector_name].
    The supported quantifiers are: some, all

    For instance::
       query p [all children] ObjectDecl ...
    """
    quantifier = Field(type=Identifier)
    selector = Field(type=SelectorPattern)


@abstract
class QueryPattern(Expr):
    """
    Root node class for query patterns
    """
    pass


class NodeQueryPattern(QueryPattern):
    """
    A query pattern of the form: node_pattern

    For instance::
       let decls = query ObjectDecls when ...
    """
    queried_node = Field(type=NodePattern)


class FullQueryPattern(NodeQueryPattern):
    """
    A query pattern of the form: node_pattern selector_pattern node_pattern

    For instance::
       let withAspects = query ObjectDecl [child] AspectAssoc when ...
    """
    selector = Field(type=SelectorPattern)
    related_node = Field(type=NodePattern)


class Query(Expr):
    """
    Query without filtering predicate.

    For instance::
       let withAspects = query ObjectDecl [child] AspectAssoc
    """
    pattern = Field(type=QueryPattern)


class FilteredQuery(Query):
    """
    Query with a filtering predicate.

    For instance::
       let classesNamedA = query cls@ClassDecl when cls.identifier == "A"
    """
    predicate = Field(type=Expr)


lkql_grammar = Grammar('main_rule')
G = lkql_grammar
# noinspection PyTypeChecker
lkql_grammar.add_rules(
    main_rule=List(Or(G.statement, G.expr, G.query)),

    statement=Or(G.assign,
                 G.print_stmt),

    print_stmt=PrintStmt(Token.Print, Token.LPar, G.expr, Token.RPar),

    query=Or(FilteredQuery(Token.QueryTok,
                           G.query_pattern,
                           Token.When,
                           G.expr),
             Query(Token.QueryTok, G.query_pattern)),

    query_pattern=Or(FullQueryPattern(G.node_pattern,
                                      G.selector_pattern,
                                      G.node_pattern),
                     NodeQueryPattern(G.node_pattern)),

    node_pattern=Or(G.full_node_pattern,
                    G.binding_node_pattern,
                    G.kind_node_pattern),

    full_node_pattern=FullNodePattern(G.binding_node_pattern,
                                      Token.At,
                                      G.kind_node_pattern),

    binding_node_pattern=BindingNodePattern(G.identifier),

    kind_node_pattern=KindNodePattern(G.kind_name),

    selector_pattern=Pick(Token.LBrack, G.selector, Token.RBrack),

    selector=Or(G.quantified_selector,
                G.named_selector),

    named_selector=NamedSelector(G.identifier),

    quantified_selector=QuantifiedSelector(G.identifier, G.named_selector),

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
