from langkit.parsers import (
    Grammar, Or, List, Pick, Opt, Cut as c, Null
)
from langkit.dsl import (
    T, ASTNode, abstract, Field, AbstractField, has_abstract_list, synthetic
)
from langkit.expressions import (
    Entity, Self, String, No, langkit_property, AbstractKind, AbstractProperty,
    Let, If
)
import langkit.expressions as dsl_expr
from langkit.expressions import String as S
from langkit.envs import add_to_env_kv, EnvSpec
from language.lexer import Token, lkql_lexer as L


@abstract
class LkqlNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """
    pass


class DeclAnnotation(LkqlNode):
    """
    Compile time annotation attached to a declaration. For the moment, only
    used for @checker annotation in lkql_checker.
    """
    name = Field(type=T.Identifier)
    arguments = Field(type=T.Arg.list)

    @langkit_property(public=True)
    def arg_with_name(name=T.Symbol):
        """
        Find argument with name "name"
        """
        return Entity.arguments.find(lambda a: a.name.symbol == name)


@abstract
class Declaration(LkqlNode):
    """
    Root node class for LKQL declarations.
    """

    annotation = Field(type=DeclAnnotation)

    doc = AbstractProperty(
        type=T.BaseStringLiteral, public=True,
        doc="Return the documentation for this declaration"
    )


@abstract
class Expr(LkqlNode):
    """
    Root node class for LKQL expressions.
    """
    pass


class TopLevelList(LkqlNode.list):
    """
    Holder for the top-level environment
    """
    pass


@abstract
class Op(LkqlNode):
    """
    Base class for operators.
    """
    enum_node = True
    alternatives = [
        'plus', 'minus', 'mul', 'div', 'and', 'or', 'eq', 'neq', 'concat',
        'lt', 'leq', 'gt', 'geq', 'not'
    ]


@abstract
class Literal(Expr):
    """
    Base class for literals
    """
    pass


class BoolLiteral(Literal):
    """
    Boolean literal
    """
    enum_node = True
    alternatives = ['true', 'false']


class Identifier(Expr):
    """
    Regular identifier.
    """
    token_node = True

    @langkit_property(return_type=T.Symbol, public=True)
    def sym():
        """
        Return the symbol for this identifier.
        """
        return Self.symbol


class IntegerLiteral(Literal):
    """
    Integer literal.
    """
    token_node = True


@abstract
class BaseStringLiteral(Literal):
    """
    Base class for string literals, both single & multi line.
    """
    pass


class StringLiteral(BaseStringLiteral):
    """
    String literal.
    """
    token_node = True


class BlockStringLiteral(BaseStringLiteral):
    """
    Node containing documentation for a given entity.
    """
    docs = Field(type=T.SubBlockLiteral.list)


class UnitLiteral(Literal):
    """
    Literal representing the unit value.
    """
    pass


class NullLiteral(Literal):
    """
    Literal representing a null node.
    """
    token_node = True


class SubBlockLiteral(LkqlNode):
    """
    Wrapper for a SubBlockLiteral token.
    """
    pass


class IfThenElse(Expr):
    """
    Expression of the form: ``if CONDITION then EXPR1 else EXPR2``
    """
    condition = Field(type=Expr)
    then_expr = Field(type=Expr)
    else_expr = Field(type=Expr)


class Unwrap(Expr):
    """
    Unwrapping of a nullable node using the !! operator.
    """
    node_expr = Field(type=Expr)


@abstract
class Arg(LkqlNode):
    """
    Base class for arguments
    """

    @langkit_property(return_type=Identifier, public=True)
    def name():
        """
        Return the argument's name, or an empty String if the argument has no
        name.
        """
        return No(Identifier)

    @langkit_property(return_type=T.Bool, public=True)
    def has_name():
        """
        Return whether the argument has a name.
        """
        return dsl_expr.Not(Self.name.is_null)

    @langkit_property(return_type=Expr, public=True,
                      kind=AbstractKind.abstract)
    def expr():
        """
        Return the argument's expression.
        """
        pass


class ExprArg(Arg):
    """
    Argument that consists of an expression
    """
    value_expr = Field(type=Expr)

    @langkit_property()
    def expr():
        return Self.value_expr


class NamedArg(Arg):
    """
    Named argument of the form: ``name=expression``

    For instance::

       add(x=20, y=22)
    """
    arg_name = Field(type=Identifier)
    value_expr = Field(type=Expr)

    @langkit_property()
    def expr():
        return Self.value_expr

    @langkit_property()
    def name():
        return Self.arg_name


@synthetic
class SynthNamedArg(NamedArg):
    """
    Synthetic NamedArg node
    """
    pass


class ParameterDecl(Declaration):
    """
    Base class for parameters
    """

    param_identifier = Field(type=Identifier)
    type_annotation = Field(type=Identifier)
    default_expr = Field(type=Expr)

    @langkit_property()
    def doc():
        return No(T.BaseStringLiteral)

    @langkit_property(return_type=T.Identifier, public=True)
    def identifier():
        """
        Return the identifier of the parameter.
        """
        return Self.param_identifier

    @langkit_property(return_type=T.String, public=True)
    def name():
        """
        Return the name of the parameter.
        """
        return Self.param_identifier.text


class BinOp(Expr):
    """
    Binary operation.
    """
    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)


class UnOp(Expr):
    """
    Unary operator (+/-)
    """
    op = Field(type=Op)
    operand = Field(type=Expr)


class RelBinOp(BinOp):
    """
    Relational (produces boolean) binary operator.
    """
    pass


class ArithBinOp(BinOp):
    """
    Arithmetic binary operator.
    """
    pass


class Unpack(LkqlNode):
    """
    Qualifier for unpack operator ("*").
    """
    enum_node = True
    qualifier = True


class ValDecl(Declaration):
    """
    Value declaration
    Associates a name with a value.

    For instance::

       val message = "Hello World"
    """
    doc_node = Field(type=T.BaseStringLiteral)
    identifier = Field(type=Identifier)
    value = Field(type=Expr)

    @langkit_property()
    def doc():
        return Self.doc_node


class DotAccess(Expr):
    """
    Access to a node's field using dot notation.
    """
    receiver = Field(type=Expr)
    member = Field(type=Identifier)


class SafeAccess(DotAccess):
    """
    Access to a field of a nullable node using the ``?.`` operator
    """
    pass


class InClause(Expr):
    """
    Check that a list contains a given value using the ``in`` keyword
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


class SafeIndexing(Indexing):
    """
    Safe indexing. Returns null if the value doesn't exist.
    """
    pass


@abstract
class BasePattern(LkqlNode):
    """
    Root node class for patterns.
    """
    pass


class FilteredPattern(BasePattern):
    """
    Pattern with a filtering predicate, of the form:
    ``<pattern> when <predicate>``

    For instance::

       o@ObjectDecl when o.children.length == 3
    """
    pattern = Field(type=BasePattern)
    predicate = Field(type=Expr)


@abstract
class ValuePattern(BasePattern):
    """
    Root node class for patterns that filter values.
    (As opposed to patterns that only bind values to a given name without
    doing any kind of filtering)
    """
    pass


class BindingPattern(BasePattern):
    """
    Pattern comprising a binding name and a value pattern.

    For instance::

       o@ObjectDecl
    """

    binding = Field(type=Identifier)
    value_pattern = Field(type=BasePattern)


class IsClause(Expr):
    """
    Check that a node matches a given pattern
    """
    node_expr = Field(type=Expr)
    pattern = Field(type=BasePattern)


class NullPattern(ValuePattern):
    """
    Null pattern. Will only match the null node.
    """
    pass


class UniversalPattern(ValuePattern):
    """
    Universal pattern that matches any value.

    For instance::

       let declParent = query * [children(depth==1)] BasicDecl
    """
    pass


class RegexPattern(ValuePattern):
    """
    Pattern that considers the value as text and matches it against the
    given regular expression.
    """
    token_node = True


class BoolPattern(ValuePattern):
    """
    Pattern to match on booleans.
    """
    enum_node = True
    alternatives = ['true', 'false']


class IntegerPattern(ValuePattern):
    """
    Pattern to match on integers.
    """
    token_node = True


class TuplePattern(ValuePattern):
    """
    Pattern to match on tuples.
    """
    patterns = Field(type=BasePattern.list)


class ListPattern(ValuePattern):
    """
    Pattern to match on lists.
    """
    patterns = Field(type=BasePattern.list)


class SplatPattern(ValuePattern):
    """
    Pattern to match any remaining number of elements in a list pattern.
    """
    binding = Field(type=Identifier)


class ObjectPatternAssoc(LkqlNode):
    """
    Object pattern assoc in an object pattern:
    ``label: <pattern>``
    """
    name = Field(type=Identifier)
    pattern = Field(type=BasePattern)


class ObjectPattern(ValuePattern):
    """
    Pattern to match on objects.
    """
    patterns = Field(type=LkqlNode.list)


class ParenPattern(ValuePattern):
    """
    A parenthesized pattern.
    """
    pattern = Field(type=BasePattern)


class OrPattern(ValuePattern):
    """
    Pattern that matches if any of its subpatterns matches.

    For instance::

        let value_decls = select ObjectDecl or ParamSpec
    """
    left = Field(type=BasePattern)
    right = Field(type=BasePattern)


class NotPattern(ValuePattern):
    """
    Pattern that matches if its inner pattern doesn't match.

    For instance::

       let non_objects = select not ObjectDecl
    """
    pattern = Field(type=BasePattern)


@abstract
class QueryKind(LkqlNode):
    """
    Base class for operators.
    """
    enum_node = True
    alternatives = ['all', 'first']


class Query(Expr):
    """
    Query against a pattern.
    this kind of expression will return every AST node that matches the given
    pattern.

    For instance::

       let withAspects = [from <node>] [through <selector>]
                         select ObjectDecl [child] AspectAssoc
    """

    unpack_from = Field(type=Unpack)
    from_expr = Field(type=Expr)
    through_expr = Field(type=Expr)
    query_kind = Field(type=QueryKind)
    pattern = Field(type=BasePattern)


class ListCompAssoc(LkqlNode):
    """
    Arrow association of the form: id <- expr.
    This construction is meant to be used a part of a list comprehension
    """

    binding_name = Field(type=Identifier)
    coll_expr = Field(type=Expr)


class ListLiteral(Expr):
    """
    List literal of the form: ``[ expr1, expr2, ..., exprn ]``
    """

    exprs = Field(type=Expr.list)


class ListComprehension(Expr):
    """
    List comprehension of the form:
    ``[ expr | generator1, generator2, ...,  opt(guard)]``
    """

    expr = Field(type=Expr)
    generators = Field(type=ListCompAssoc.list)
    guard = Field(type=Expr)


class ObjectLiteral(Expr):
    """
    Object literal of the form:
    ``{label: value, ..., labeln: valuen}``
    """
    assocs = Field(type=T.ObjectAssoc.list)


class ObjectAssoc(LkqlNode):
    """
    Object assoc in an object literal:
    ``label: <value>``
    """
    name = Field(type=Identifier)
    expr = Field(type=Expr)


class AtObjectAssoc(LkqlNode):
    """
    At object literal association of the form:
    ``label opt(: <value>)``
    """
    name = Field(type=Identifier)
    expr = Field(type=Expr)


class AtObjectLiteral(Expr):
    """
    Object literal with @ in front of it:
    ``@{ label1, label2: value }``
    """
    assocs = Field(type=AtObjectAssoc.list)


@abstract
class BlockBody(LkqlNode):
    """
    Root node for block expression block before steps
    """
    pass


class BlockBodyDecl(BlockBody):
    """
    Node for an expression in a block expression body
    """

    decl = Field(type=Declaration)


class BlockBodyExpr(BlockBody):
    """
    Node for a declaration in a block expression body
    """

    expr = Field(type=Expr)


class BlockExpr(Expr):
    """
    Expression of the form: ``val id = value; expr; expr``

    For instance::

        {
           val x = 40;
           val y = 2;
           print(x + y);
           x + y
        }
    """

    body = Field(type=BlockBody.list)
    expr = Field(type=Expr)


@abstract
class BaseFunction(Expr):
    """
    Base class for a function expression.
    """
    parameters = Field(type=ParameterDecl.list)
    doc_node = Field(type=T.BaseStringLiteral)
    body_expr = Field(type=Expr)

    @langkit_property(return_type=T.String, public=True)
    def profile():
        """
        Return a text profile for this function.
        """
        return Self.parent.match(
            lambda d=FunDecl: S("fun ")
            .concat(d.name.text).concat(S("("))
            .concat(Self.parameters.text).concat(S(")")),

            lambda _: S("lambda ")
            .concat(S("(")).concat(Self.parameters.text).concat(S(")"))
        )

    @langkit_property(return_type=T.BaseStringLiteral, public=True)
    def doc():
        """
        Return documentation associated to this function object.
        """
        return Self.match(
            lambda n=T.NamedFunction: n.doc_node,
            lambda _: dsl_expr.No(T.BaseStringLiteral)
        )

    @langkit_property(return_type=T.Int, public=True)
    def arity():
        """
        Return the number of parameters of the function
        """
        return Self.parameters.length

    @langkit_property(return_type=ParameterDecl, public=True)
    def find_parameter(name=T.String):
        """
        Return the parameter associated with the given name, if any.
        """
        return Self.parameters.find(lambda p: p.param_identifier.text == name)

    @langkit_property(return_type=T.Bool, public=True)
    def has_parameter(name=T.String):
        """
        Return whether the function has a parameter with the given name.
        """
        return dsl_expr.Not(Self.find_parameter(name).is_null)

    @langkit_property(return_type=ParameterDecl.entity.array, public=True)
    def default_parameters():
        """
        Return the defaults parameters of the function, if any.
        """
        return Entity.parameters.filter(
            lambda p: dsl_expr.Not(p.default_expr.is_null)
        )


class NamedFunction(BaseFunction):
    """
    Function expression that is part of a named function declaration (see
    ``FunDecl``).
    """


class AnonymousFunction(BaseFunction):
    """
    Anonymous function expression.
    """
    pass


class FunDecl(Declaration):
    """
    Function definition

    For instance::

       fun add(x, y) = x + y
    """

    name = Field(type=Identifier)
    fun_expr = Field(type=NamedFunction)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))

    @langkit_property()
    def doc():
        return Self.fun_expr.doc


class Safe(LkqlNode):
    """
    Qualifier for safe accesses.
    """
    enum_node = True
    qualifier = True


class FunCall(Expr):
    """
    Function call.

    For instance::

       add(2, 40)
    """

    name = Field(type=Expr)
    has_safe = Field(type=Safe)
    arguments = Field(type=Arg.list)

    @langkit_property(return_type=T.Int, public=True)
    def arity():
        """
        Return the number of arguments of the function call
        """
        return Self.arguments.length

    @langkit_property(return_type=T.Bool, public=True)
    def is_actual_call():
        """
        Return whether this is a call to an actual function (instead of a
        syntactic construct that looks like a function call bu isn't one).
        """
        return dsl_expr.Not(Self.parent.is_a(NodePatternProperty))

    @langkit_property(return_type=T.Bool, public=True)
    def is_builtin_call():
        """
        Return whether this is a call to a built-in property.
        """
        return (Self.name.text == String("print")) | \
               (Self.name.text == String("debug")) | \
               (Self.name.text == String("list"))

    @langkit_property(return_type=Expr)
    def expr_for_arg(name=T.String):
        return Let(
            lambda x=Self.arguments.find(lambda a: a.cast(T.NamedArg).then(
                lambda na: na.arg_name.text == name
            )):
            If(x.is_null, No(Expr), x.expr)
        )

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def depth_expr():
        """
        Return the expression associated to the 'expr' argument, if any.
        """
        return Self.expr_for_arg(String('depth'))

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def max_depth_expr():
        """
        If the 'max_depth' arg is set and 'depth' is not set, return the
        expression for 'max_depth'. If 'depth' is set return its expression.
        If neither 'depth' or 'max_depth' is set, return a null expression.
        """
        return If(Self.depth_expr.is_null,
                  Self.expr_for_arg(String('max_depth')),
                  Self.depth_expr)

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def min_depth_expr():
        """
        If the 'min_depth' arg is set and 'depth' is not set, return the
        expression for 'min_depth'. If 'depth' is set return its expression.
        If neither 'depth' or 'min_depth' is set, return a null expression.
        """
        return If(Self.depth_expr.is_null,
                  Self.expr_for_arg(String('min_depth')),
                  Self.depth_expr)


class RecExpr(Expr):
    """
    Expression that is only valid in a selector. Describes the next actions of
    a selector.
    """
    recurse_unpack = Field(type=Unpack)
    recurse_expr = Field(type=Expr)
    result_unpack = Field(type=Unpack)
    result_expr = Field(type=Expr)


class SelectorArm(LkqlNode):
    """
    Represents one case of a selector

    For instance::

       | o@ObjectDecl => o.image
    """

    pattern = Field(type=BasePattern)
    expr = Field(type=Expr)


class SelectorDecl(Declaration):
    """
    Ast selector, describing a subtree
    """
    name = Field(type=Identifier)
    doc_node = Field(type=T.BaseStringLiteral)
    arms = Field(type=SelectorArm.list)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))

    @langkit_property()
    def doc():
        return Self.doc_node


class SelectorCall(LkqlNode):
    """
    Root node for selector patterns
    """

    quantifier = Field(type=Identifier)
    binding = Field(type=Identifier)
    selector_call = Field(type=Expr)

    @langkit_property(return_type=T.String, public=True)
    def quantifier_name():
        """
        Return the selector's quantifier name.
        If the name hasn't been explicitly specified, the default quantifier
        name is 'all'.
        """
        return If(Self.quantifier.is_null,
                  String("all"),
                  Self.quantifier.text)

    @langkit_property(return_type=T.Identifier, public=True)
    def binding_name():
        """
        Return the binding name associated with this selector call, if any.
        """
        return Self.binding


@abstract
class NodePattern(ValuePattern):
    """
    Root node class for node patterns
    """
    pass


class NodeKindPattern(NodePattern):
    """
    Pattern of the form: KindName
    """
    kind_name = Field(type=Identifier)


@abstract
class NodePatternDetail(LkqlNode):
    """
    Access to a field, property or selector inside a node pattern.
    """
    pass


class NodePatternField(NodePatternDetail):
    """
    Access to a field in a node pattern.
    """
    identifier = Field(type=Identifier)
    expected_value = Field(type=BasePattern)


class NodePatternProperty(NodePatternDetail):
    """
    Access to a property in a node pattern.
    """
    call = Field(type=FunCall)
    expected_value = Field(type=BasePattern)


class NodePatternSelector(NodePatternDetail):
    """
    Use of a selector in a node pattern
    """
    call = Field(type=SelectorCall)
    pattern = Field(type=BasePattern)


class ExtendedNodePattern(NodePattern):
    """
    Node pattern of the form:

    ``KindName(field=val, prop() is val, any selector is Pattern)``

    For instance::

        ObjectDecl(children: AspectAssoc)
    """
    node_pattern = Field(type=ValuePattern)
    details = Field(type=NodePatternDetail.list)


class MatchArm(LkqlNode):
    """
    Represents one case of a 'match'.
    """

    pattern = Field(type=BasePattern)
    expr = Field(type=Expr)


class Match(Expr):
    """
    Match expression, returns the expression associated with the first pattern
    that matches the 'matched_val' field, or unit if no pattern matches.

    For instance::

       match nodes[0]
           | ObjectDecl => true
           | *          => false
    """
    matched_val = Field(type=Expr)
    arms = Field(type=MatchArm.list)

    @langkit_property(return_type=Expr, public=True)
    def nth_expression(n=(T.Int, 0)):
        """
        Return the expression associated with the nth selector arm.
        """
        return Self.arms.at(n - 1).expr()

    @langkit_property(return_type=BasePattern.entity.array, public=True)
    def patterns():
        """
        Return a list of the patterns that appear n the match
        expressions's arms.
        """
        return Self.arms.map(lambda x: x.pattern.as_entity)


class Import(LkqlNode):
    """
    Import.
    """

    name = Field(type=Identifier)


class Tuple(Expr):
    """
    Tuple expression.
    """
    exprs = Field(type=Expr.list)


lkql_grammar = Grammar('main_rule')
G = lkql_grammar

# noinspection PyTypeChecker
lkql_grammar.add_rules(
    main_rule=List(
        Or(G.import_clause, G.decl, G.expr),
        list_cls=TopLevelList, empty_valid=True
    ),

    import_clause=Import("import", G.id),

    query=Or(
        Query(
            "from", Unpack("*"), G.expr,
            Opt("through", Or(G.expr)),
            "select", c(), 
            Or(QueryKind.alt_first(L.Identifier(match_text="first")),
               QueryKind.alt_all()), 
            G.pattern
        ),
        Query(
            Null(Unpack), Null(G.expr),
            Opt("through", Or(G.expr)),
            "select", c(), 
            Or(QueryKind.alt_first(L.Identifier(match_text="first")),
               QueryKind.alt_all()), 
            G.pattern
        ),
    ),

    or_pattern=Or(
        OrPattern(G.pattern, "|", G.or_pattern),
        G.pattern
    ),

    pattern=Or(
        FilteredPattern(G.value_pattern, "when", G.expr),
        G.value_pattern
    ),

    object_pattern_assoc=ObjectPatternAssoc(G.id, ":", G.pattern),

    value_pattern=Or(
        ExtendedNodePattern(

            Or(UniversalPattern("*"),
               NodeKindPattern(G.upper_id),
               ParenPattern("(", G.or_pattern, ")")),

            Pick("(", c(), List(G.pattern_arg, sep=","), ")")
        ),
        NodeKindPattern(G.upper_id),
        UniversalPattern("*"),
        NullPattern("null"),
        G.regex_pattern,
        NotPattern("not", G.value_pattern),
        G.bool_pattern,
        G.integer_pattern,
        G.list_pattern,
        G.object_pattern,
        BindingPattern(G.id, Opt("@", G.value_pattern)),
        ParenPattern("(", G.or_pattern, ")"),
        G.tuple_pattern
    ),

    regex_pattern=RegexPattern(Token.String),

    bool_pattern=Or(
        BoolPattern.alt_true("true"),
        BoolPattern.alt_false("false"),
    ),

    splat_pattern=SplatPattern(Opt(G.id, "@"), "..."),

    integer_pattern=IntegerPattern(Token.Integer),
    list_pattern=ListPattern(
        "[",
        List(G.splat_pattern | G.value_pattern, sep=","), "]"
    ),
    object_pattern=ObjectPattern(
        "{",
        List(G.object_pattern_assoc | G.splat_pattern, sep=",", empty_valid=False),
        "}"
    ),
    tuple_pattern=TuplePattern("(", List(G.value_pattern, sep=","), ")"),

    pattern_arg=Or(
        NodePatternSelector(G.selector_call, ":", G.or_pattern),
        NodePatternField(G.id, ":", c(), G.or_pattern),
        NodePatternProperty(G.fun_call, ":", c(), G.or_pattern)
    ),

    selector_call=SelectorCall(
        Identifier(Or(
            Token.Identifier(match_text="any"),
            Token.Identifier(match_text="all")
        )),
        Opt(Pick(G.id, "@")),
        # NOTE: we use value_expr here because we don't want to include
        # comp_expr in the valid expressions for selectors, which will
        # clash, because in ``any parent is bla`` it would parse
        # ``parent is bla`` as a ``IsClause``.
        G.value_expr
    ),

    objectlit=ObjectLiteral(
        "{",
        List(G.object_assoc, empty_valid=True, sep=","),
        "}"
    ),
    object_assoc=ObjectAssoc(G.object_key, ":", G.expr),

    at_object_lit=AtObjectLiteral(
        "@", "{",
        List(G.at_object_assoc, empty_valid=True, sep=","),
        "}"
    ),
    at_object_assoc=AtObjectAssoc(
        G.object_key,
        Opt(":", G.expr)
    ),

    object_key=Or(
        G.id,
        G.upper_id
    ),

    listcomp=ListComprehension(
        "[",
        G.expr, "for",
        List(ListCompAssoc(G.id, "in", G.expr),
             sep=",", empty_valid=False),
        Opt("if", G.expr),
        "]"
    ),

    listlit=ListLiteral("[", List(G.expr, empty_valid=True, sep=","), "]"),

    decl=Or(G.fun_decl,
            G.selector_decl,
            G.val_decl),

    expr=Or(
        BinOp(
            G.expr,
            Or(Op.alt_and("and"), Op.alt_or("or")),
            G.not_expr
        ),
        G.not_expr,
    ),

    not_expr=Or(
        UnOp(Op.alt_not("not"), G.comp_expr),
        G.comp_expr
    ),

    comp_expr=Or(
        IsClause(G.comp_expr, "is", G.pattern),
        InClause(G.comp_expr, "in", G.expr),
        RelBinOp(
            G.comp_expr,
            Or(Op.alt_eq("=="),
               Op.alt_neq("!="),
               Op.alt_lt("<"),
               Op.alt_leq("<="),
               Op.alt_gt(">"),
               Op.alt_geq(">=")),
            G.plus_expr
        ),
        G.plus_expr
    ),

    plus_expr=Or(

        ArithBinOp(G.plus_expr,
                   Or(Op.alt_plus("+"), Op.alt_minus("-")),
                   G.prod_expr),

        BinOp(G.plus_expr, Op.alt_concat("&"), G.prod_expr),

        G.prod_expr
    ),

    prod_expr=Or(

        ArithBinOp(G.prod_expr,
                   Or(Op.alt_mul("*"), Op.alt_div("/")),
                   G.value_expr),

        G.unop

    ),

    unop=Or(
        UnOp(Or(Op.alt_plus("+"), Op.alt_minus("-")), G.value_expr),
        G.value_expr,
    ),

    value_expr=Or(
        Unwrap(G.value_expr, "!!"),

        DotAccess(G.value_expr, ".", c(), G.id),
        SafeAccess(G.value_expr, "?.", c(), G.id),
        Indexing(G.value_expr, "[", c(), G.expr, "]"),
        SafeIndexing(G.value_expr, "?[", c(), G.expr, "]"),
        G.selector_expr,
        FunCall(
            G.value_expr, Safe("?"),
            "(", c(), List(G.arg, empty_valid=True, sep=","), ")"
        ),
        G.term
    ),

    term=Or(
        G.query,
        G.listcomp,
        G.listlit,
        G.objectlit,
        G.at_object_lit,
        G.match,
        G.id,
        G.string_literal,
        G.block_string_literal,
        G.bool_literal,
        G.unit_literal,
        NullLiteral("null"),
        G.integer,
        G.anonymous_function,
        Pick("(", G.expr, ")"),
        G.if_then_else,
        G.block_expr,
        G.tuple_expr,
    ),

    tuple_expr=Tuple(
        "(", List(G.expr, sep=","), ")"
    ),

    anonymous_function=AnonymousFunction(
        "(", List(G.param, empty_valid=True, sep=","), ")",
        "=>", Null(G.doc_node), G.expr
    ),

    block_expr=BlockExpr(
        "{",
        List(
            Or(BlockBodyDecl(G.decl, ";"), BlockBodyExpr(G.expr, ";")),
            empty_valid=True
        ),
        c(),
        G.expr,
        "}"
    ),

    val_decl=ValDecl(
        Opt(G.decl_annotation),
        Opt(G.doc_node),
        "val", c(), G.id, "=", G.expr
    ),

    fun_decl=FunDecl(
        Opt(G.decl_annotation),
        "fun", c(), G.id,
        NamedFunction(
            "(", List(G.param, empty_valid=True, sep=","), ")",
            "=", Opt(G.doc_node), G.expr
        )
    ),

    fun_call=FunCall(
        G.id, Safe("?"), "(", c(), G.arg_list, ")"
    ),

    arg_list=List(G.arg, empty_valid=True, sep=","),

    selector_decl=SelectorDecl(
        Opt(G.decl_annotation),
        "selector", c(),
        G.id,
        Opt(G.doc_node),
        List(G.selector_arm, empty_valid=False)
    ),

    selector_arm=SelectorArm(
        "|",
        G.pattern,
        "=>", G.expr
    ),

    selector_expr=Or(
        RecExpr("rec", "(", Unpack("*"), G.expr, ",", Unpack("*"), G.expr, ")"),
        RecExpr("rec", "(", Unpack("*"), G.expr, Null(Unpack), Null(G.expr), ")")
    ),

    match=Match("match", G.expr, List(G.match_arm, empty_valid=False)),

    match_arm=MatchArm("|", G.pattern, "=>", G.expr),

    if_then_else=IfThenElse("if", G.expr, "then", G.expr, "else", G.expr),

    id=Identifier(Token.Identifier),
    upper_id=Identifier(Token.UpperIdentifier),
    integer=IntegerLiteral(Token.Integer),

    bool_literal=Or(BoolLiteral.alt_true("true"),
                    BoolLiteral.alt_false("false")),

    string_literal=StringLiteral(Token.String),
    block_string_literal=BlockStringLiteral(List(
        SubBlockLiteral(Token.SubBlockLiteral), empty_valid=False)
    ),

    unit_literal=UnitLiteral("(", ")"),

    arg=Or(NamedArg(G.id, "=", G.expr), ExprArg(G.expr)),

    param=ParameterDecl(
        Null(G.decl_annotation),
        G.id, Opt(":", G.id), Opt("=", G.expr)
    ),

    decl_annotation=DeclAnnotation("@", G.id, Opt("(", G.arg_list, ")")),

    doc_node=Or(G.string_literal, G.block_string_literal),

)
