from langkit.parsers import (
    Grammar, Or, List, Pick, Opt, NoBacktrack as c, Null
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
@has_abstract_list
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """

    @langkit_property(public=True, return_type=T.Symbol.array,
                      external=True, uses_envs=False, uses_entity_info=False)
    def interp_complete():
        """
        Complete from node.
        """
        pass

    @langkit_property(public=True, return_type=T.Bool,
                      external=True, uses_envs=False, uses_entity_info=False)
    def interp_init_from_project(project_file=T.String):
        """
        Context method.

        Initialize the interpreter with given project file.

        TODO: add other project options
        """
        pass

    @langkit_property(public=True, return_type=T.Symbol,
                      external=True, uses_envs=False, uses_entity_info=False)
    def interp_eval():
        """
        Eval the given node and return the result of the evaluation as a
        string.
        """
        pass


class DeclAnnotation(LKQLNode):
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
class Declaration(LKQLNode):
    """
    Root node class for LKQL declarations.
    """

    annotation = Field(type=DeclAnnotation)

    doc = AbstractProperty(
        type=T.BaseStringLiteral, public=True,
        doc="Return the documentation for this declaration"
    )


@abstract
class Expr(LKQLNode):
    """
    Root node class for LKQL expressions.
    """
    pass


class TopLevelList(LKQLNode.list):
    """
    Holder for the top-level environment
    """
    pass


@abstract
class Op(LKQLNode):
    """
    Base class for operators.
    """
    enum_node = True
    alternatives = [
        'plus', 'minus', 'mul', 'div', 'and', 'or', 'eq', 'neq', 'concat',
        'lt', 'leq', 'gt', 'geq'
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


class SubBlockLiteral(LKQLNode):
    """
    Wrapper for a SubBlockLiteral token.
    """
    pass


class IfThenElse(Expr):
    """
    Expression of the form: if CONDITION then EXPR1 else EXPR2
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
class Arg(LKQLNode):
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
    Named argument of the form: name=expression

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


class Not(Expr):
    """
    Negation of a boolean value.
    """
    value = Field(type=Expr)


class BinOp(Expr):
    """
    Binary operation.
    """
    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)


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


class Unpack(Expr):
    """
    Unpacking operator, written '*'.

    For instance::
       \\*listValue
    """
    collection_expr = Field(type=Expr)


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
    Access to a field of a nullable node using the ?. operator
    """
    pass


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
class BasePattern(LKQLNode):
    """
    Root node class for patterns.
    """

    @langkit_property(return_type=T.Identifier, public=True,
                      kind=AbstractKind.abstract)
    def binding_name():
        """
        Return the pattern's binding name.
        Return an empty String if the pattern doesn't contain a binding name.
        """
        pass

    @langkit_property(return_type=T.Bool, public=True)
    def has_binding():
        """
        Return whether the node pattern contains a binding name.
        """
        return dsl_expr.Not(Self.binding_name.is_null)

    @langkit_property(return_type=T.ValuePattern, public=True)
    def value_part():
        """
        Return the value pattern contained in the pattern , if any.
        """
        return No(ValuePattern)


@abstract
class UnfilteredPattern(BasePattern):
    """
    Pattern without a filtering predicate.
    """
    pass


class FilteredPattern(BasePattern):
    """
    Pattern with a filtering predicate, of the form: pattern when predicate

    For instance::
       o@ObjectDecl when o.children.length == 3
    """
    pattern = Field(type=UnfilteredPattern)
    predicate = Field(type=Expr)

    @langkit_property()
    def binding_name():
        return Self.pattern.binding_name

    @langkit_property()
    def value_part():
        return Self.pattern.value_part()


@abstract
class ValuePattern(UnfilteredPattern):
    """
    Root node class for patterns that filter values.
    (As opposed to patterns that only bind values to a given name without
    doing any kind of filtering)
    """

    @langkit_property()
    def binding_name():
        return dsl_expr.No(T.Identifier)

    @langkit_property()
    def value_part():
        return Self


class BindingPattern(UnfilteredPattern):
    """
    Pattern comprising a binding name and a value pattern.

    For instance::
       o : ObjectDecl
    """

    binding = Field(type=Identifier)
    value_pattern = Field(type=ValuePattern)

    @langkit_property()
    def binding_name():
        return Self.binding


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
    pattern = Field(type=ValuePattern)


@abstract
class QueryKind(LKQLNode):
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
       let withAspects = query ObjectDecl [child] AspectAssoc
    """

    from_expr = Field(type=Expr)
    query_kind = Field(type=QueryKind)
    pattern = Field(type=BasePattern)


class ListCompAssoc(LKQLNode):
    """
    Arrow association of the form: id <- expr.
    This construction is meant to be used a part of a list comprehension
    """

    binding_name = Field(type=Identifier)
    coll_expr = Field(type=Expr)


class ListLiteral(Expr):
    """
    List literal of the form:
        [ expr1, expr2, ..., exprn ]
    """

    exprs = Field(type=Expr.list)


class ListComprehension(Expr):
    """
    List comprehension of the form:
        [ expr | generator1, generator2, ...,  opt(guard)]
    """

    expr = Field(type=Expr)
    generators = Field(type=ListCompAssoc.list)
    guard = Field(type=Expr)


class BlockExpr(Expr):
    """
    Expression of the form: val id = value; expr

    For instance::
    {
       val x = 40;
       val y = 2;
       x + y
    }
    """

    vals = Field(type=Declaration.list)
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


class Safe(LKQLNode):
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


class SelectorExprMode(LKQLNode):
    """
    Modes for selector values:
        - default: add the value to the result set
        - rec: add the value to the result set and call the selector
               recursively
        - skip: call the selector recursively without adding the value to the
                result set
    """
    enum_node = True

    alternatives = ['default', 'rec', 'skip']


class SelectorExpr(LKQLNode):
    """
    Expression appearing in the right part of a selector arm.
    """
    mode = Field(type=SelectorExprMode)
    expr = Field(type=Expr)


class SelectorArm(LKQLNode):
    """
    Represents one case of a selector

    For instance::
       | o@ObjectDecl => o.image
    """

    pattern = Field(type=BasePattern)
    exprs_list = Field(type=SelectorExpr.list)


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

    @langkit_property(return_type=SelectorExpr.list, public=True)
    def nth_expressions(n=(T.Int, 0)):
        """
        Return the selector expressions associated with the nth selector arm.
        """
        return Self.arms.at(n - 1).exprs_list

    @langkit_property(return_type=BasePattern.entity.array, public=True)
    def patterns():
        """
        Return a list of the patterns that appear n the selector's arms.
        """
        return Self.arms.map(lambda x: x.pattern.as_entity)


class SelectorCall(LKQLNode):
    """
    Root node for selector patterns
    """

    quantifier = Field(type=Identifier)
    binding = Field(type=Identifier)
    selector_identifier = Field(type=Identifier)
    args = Field(type=NamedArg.list)

    @langkit_property(return_type=T.String, public=True)
    def name():
        """
        Return the name of the selector.
        """
        return Self.selector_identifier.text

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

    @langkit_property(return_type=Expr)
    def expr_for_arg(name=T.String):
        return Let(
            lambda x=Self.args.find(lambda a: a.arg_name.text == name):
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
        If the 'min_depth' arg is set and 'depth" is not set, return the
        expression for 'min_depth'. If 'depth' is set return its expression.
        If neither 'depth' or 'min_depth' is set, return a null expression.
        """
        return If(Self.depth_expr.is_null,
                  Self.expr_for_arg(String('min_depth')),
                  Self.depth_expr)


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
class DetailValue(LKQLNode):
    """
    Root node class for pattern data values.
    Pattern data values can be expressions or patterns.
    """
    pass


class DetailExpr(DetailValue):
    """
    Expression pattern data value.
    """
    expr_value = Field(type=Expr)


class DetailPattern(DetailValue):
    """
    Pattern pattern data value
    """
    pattern_value = Field(type=BasePattern)


@abstract
class NodePatternDetail(LKQLNode):
    """
    Access to a field, property or selector inside a node pattern.
    """
    pass


class NodePatternField(NodePatternDetail):
    """
    Access to a field in a node pattern.
    """
    identifier = Field(type=Identifier)
    expected_value = Field(type=DetailValue)


class NodePatternProperty(NodePatternDetail):
    """
    Access to a property in a node pattern.
    """
    call = Field(type=FunCall)
    expected_value = Field(type=DetailValue)


class NodePatternSelector(NodePatternDetail):
    """
    Use of a selector in a node pattern
    """
    call = Field(type=SelectorCall)
    pattern = Field(type=BasePattern)


class ExtendedNodePattern(NodePattern):
    """
    Node pattern of the form:

    KindName(field=val, prop() is val, any selector is Pattern)

    For instance::
        ObjectDecl(children: AspectAssoc)
    """
    node_pattern = Field(type=ValuePattern)
    details = Field(type=NodePatternDetail.list)


@abstract
class ChainedPatternLink(LKQLNode):
    """
    Element of a chained pattern of the form:
        (selector|field|property) pattern
    """
    pattern = AbstractField(type=BasePattern)


class SelectorLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
        quantifier selector_name pattern
    """
    selector = Field(type=SelectorCall)
    pattern = Field(type=BasePattern)


class FieldLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
       field pattern
    """
    field = Field(type=Identifier)
    pattern = Field(type=BasePattern)


class PropertyLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
       property(args) pattern
    """
    property = Field(type=FunCall)
    pattern = Field(type=BasePattern)


class ChainedNodePattern(ValuePattern):
    """
    Node pattern of the form:
        Kind1(details...) selector1 Kind2(details...) selector2 ... KindN
    """
    first_pattern = Field(type=BasePattern)
    chain = Field(type=ChainedPatternLink.list)


class MatchArm(LKQLNode):
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


class Import(LKQLNode):
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

    query=Query(
        Opt(
            "from",
            Or(G.expr, Unpack("*", G.expr))
        ),
        "select", c(), Or(
            QueryKind.alt_first(L.Identifier(match_text="first")),
            QueryKind.alt_all(),
        ), G.pattern
    ),

    pattern=Or(
        OrPattern(
            G.chained_node_pattern,
            "or",
            G.pattern
        ),
        G.chained_node_pattern
    ),

    chained_node_pattern=Or(
        ChainedNodePattern(
            G.filtered_pattern,
            List(Or(
                SelectorLink(G.selector_call, "is", G.filtered_pattern),
                FieldLink(".", G.id, "is", G.filtered_pattern),
                PropertyLink(".", G.fun_call, "is", G.filtered_pattern)
            ))
        ),
        G.filtered_pattern
    ),

    filtered_pattern=Or(
        FilteredPattern(G.binding_pattern, "when", G.expr),
        G.binding_pattern

    ),

    binding_pattern=Or(
        BindingPattern(G.id, "@", G.value_pattern),
        G.value_pattern
    ),

    value_pattern=Or(
        ExtendedNodePattern(

            Or(UniversalPattern("*"),
               NodeKindPattern(G.kind_name)),

            Pick("(", c(), List(G.pattern_arg, sep=","), ")")
        ),
        NodeKindPattern(G.kind_name),
        UniversalPattern("*"),
        NullPattern("null"),
        RegexPattern(Token.String),
        NotPattern("not", G.value_pattern),
        ParenPattern("(", G.pattern, ")")
    ),

    pattern_arg=Or(
        NodePatternSelector(
            SelectorCall(
                G.id,
                Opt(Pick(G.id, "@")),
                G.id,
                Opt("(", c(),
                    List(G.named_arg, sep=",", empty_valid=False), ")")
            ), "is", G.pattern
        ),
        NodePatternField(G.id, "is", c(), G.detail_value),
        NodePatternProperty(G.fun_call, "is", c(), G.detail_value)
    ),

    detail_value=Or(DetailPattern(G.pattern), DetailExpr(G.expr)),

    selector_call=SelectorCall(
        G.id,
        Opt(Pick(G.id, "@")),
        G.id,
        Opt("(", c(), List(G.named_arg, sep=",", empty_valid=False), ")")
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
            G.comp_expr
        ),
        G.comp_expr,
        G.block_expr,
        G.tuple_expr,
    ),

    tuple_expr=Tuple(
        "(", List(G.expr, sep=","), ")"
    ),

    comp_expr=Or(
        IsClause(G.comp_expr, "is", G.pattern),
        InClause(G.comp_expr, "in", G.expr),
        Not("not", G.expr),
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

        G.value_expr

    ),

    value_expr=Or(
        Unwrap(G.value_expr, "!!"),

        DotAccess(G.value_expr, ".", c(), G.id),
        SafeAccess(G.value_expr, "?.", c(), G.id),
        Indexing(G.value_expr, "[", c(), G.expr, "]"),
        FunCall(
            G.value_expr, Safe("?"),
            "(", c(), List(G.arg, empty_valid=True, sep=","), ")"
        ),
        G.query,
        G.listcomp,
        G.listlit,
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
        G.if_then_else
    ),

    anonymous_function=AnonymousFunction(
        "(", List(G.param, empty_valid=True, sep=","), ")",
        "=>", Null(G.doc_node), G.expr
    ),

    block_expr=BlockExpr(
        "{", c(), List(G.decl, sep=";", empty_valid=False), ";", G.expr,
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
        "=>",
        List(G.selector_expr,
             empty_valid=False, sep="<>")
    ),

    selector_expr=SelectorExpr(
        Or(SelectorExprMode.alt_rec("rec"),
           SelectorExprMode.alt_skip("skip"),
           SelectorExprMode.alt_default()),
        Or(G.expr, Unpack("*", G.expr))
    ),

    match=Match("match", G.expr, List(G.match_arm, empty_valid=False)),

    match_arm=MatchArm("|", G.pattern, "=>", G.expr),

    if_then_else=IfThenElse("if", G.expr, "then", G.expr, "else", G.expr),

    id=Identifier(Token.Identifier),
    kind_name=Identifier(Token.KindName),
    integer=IntegerLiteral(Token.Integer),

    bool_literal=Or(BoolLiteral.alt_true("true"),
                    BoolLiteral.alt_false("false")),

    string_literal=StringLiteral(Token.String),
    block_string_literal=BlockStringLiteral(List(
        SubBlockLiteral(Token.SubBlockLiteral), empty_valid=False)
    ),

    unit_literal=UnitLiteral("(", ")"),

    arg=Or(NamedArg(G.id, "=", G.expr), ExprArg(G.expr)),

    named_arg=NamedArg(G.id, "=", G.expr),

    param=ParameterDecl(
        Null(G.decl_annotation),
        G.id, Opt(":", G.id), Opt("=", G.expr)
    ),

    decl_annotation=DeclAnnotation("@", G.id, Opt("(", G.arg_list, ")")),

    doc_node=Or(G.string_literal, G.block_string_literal),

)
