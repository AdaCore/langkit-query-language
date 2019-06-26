from langkit.parsers import Grammar, Or, List, Pick, Opt
from langkit.dsl import (
    T, ASTNode, abstract, Field, AbstractField, has_abstract_list, synthetic,
    UserField, LogicVar, Equation, Struct
)
from langkit.expressions import (
    Self, String, No, langkit_property, AbstractKind, Let, If, Bind, LogicTrue,
    Property, And
)
import langkit.expressions as dsl_expr
from langkit.envs import add_to_env_kv, EnvSpec
from lexer import Token


class UndefinedType(Struct):
    """
    Represents the type of a node that couldn't be type_checked due to the
    incompleteness of the typechecker.
    """
    origin = UserField(type=T.LKQLNode)


class ValidType(Struct):
    """
    Represents the type of a well-typed node.
    """
    type_value = UserField(type=T.TypeExpr.entity)


class UnexpectedType(Struct):
    """
    Represents a typing error.
    """
    expected = UserField(type=ValidType)
    expected_origin = UserField(type=T.LKQLNode)
    actual = UserField(type=T.ValidType)
    actual_origin = UserField(type=T.LKQLNode)


class UnknownType(Struct):
    """
    represents a typing error involving a type name that doesn't match any
    known type.
    """
    name = UserField(type=T.String)


class TypingJudgment(Struct):
    """
    Pseudo-union of typing judgment values.
    """
    undefined = UserField(type=UndefinedType)
    valid = UserField(type=ValidType)
    unexpected = UserField(type=UnexpectedType)
    unknown = UserField(type=UnknownType)


def new_typing_judgment(value):
    field_name = value.struct_type.__name__.replace('Type', '').lower()

    args = {
        "undefined": No(T.UndefinedType),
        "valid": No(T.ValidType),
        "unexpected": No(T.UnexpectedType),
        "unknown": No(T.UnknownType),
        field_name: value
    }

    return TypingJudgment.new(**args)


@abstract
@has_abstract_list
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """

    @langkit_property(public=True, return_type=T.AnalysisUnit, external=True,
                      uses_envs=False, uses_entity_info=False)
    def prelude_unit():
        """
        Retrieve the Prelude unit.
        """
        pass

    @langkit_property(public=True, return_type=TypingJudgment)
    def type_check():
        """
        Typecheck the current node.
        """
        return new_typing_judgment(UndefinedType.new(origin=Self))

    @langkit_property(public=False, return_type=TypingJudgment)
    def compare_type(expected=T.TypingJudgment, expected_origin=T.LKQLNode,
                     value=T.LKQLNode):
        """
        Check the type of `value` against the expected type.
        """
        return If(
            expected.valid.is_null,
            expected,
            Let(
                lambda actual=value.type_check:
                If(actual.valid.is_null,
                   actual,
                   If(
                       actual == expected,
                       actual,
                       new_typing_judgment(UnexpectedType.new(
                           expected=expected.valid,
                           expected_origin=expected_origin,
                           actual=actual.valid,
                           actual_origin=value
                       ))
                   )
                )
            )
        )

    @langkit_property(public=False, return_type=TypingJudgment)
    def check_identical_types(left=T.LKQLNode, right=T.LKQLNode):
        return Self.compare_type(left.type_check, left, right)

    @langkit_property(public=True, return_type=TypingJudgment)
    def lookup_type(name=T.Symbol):
        """
        Return the TypeExpr node representing the type with the given name.
        If there is no type named `name`, return the error type.
        """
        return Let(lambda n=Self.node_env.get_first(name):
                   If(n.is_null,
                      new_typing_judgment(UnknownType.new(name=name.image)),
                      new_typing_judgment(
                          ValidType.new(type_value=n.cast(TypeExpr))
                      )))

    @langkit_property(public=True, return_type=T.String)
    def type_name():
        """
        Return the name of the node's type.
        """
        return If(Self.type_check.valid.is_null,
                  String("error"),
                  Self.type_check.valid.type_value.name)


@abstract
class Declaration(LKQLNode):
    """
    Root node class for LKQL declarations.
    """
    pass


@abstract
@has_abstract_list
class Expr(LKQLNode):
    """
    Root node class for LKQL expressions.
    """

    type_var = UserField(LogicVar, public=False)


class TopLevelList(LKQLNode.list):
    """
    Holder for the top-level members
    """
    pass


class BoolLiteral(Expr):
    """
    Boolean literal
    """
    enum_node = True
    alternatives = ['true', 'false']

    @langkit_property()
    def type_check():
        return Self.lookup_type("bool")


class Identifier(Expr):
    """
    Regular identifier.
    """
    token_node = True

    @langkit_property(return_type=LKQLNode.entity, public=True)
    def referenced_node():
        """
        Return the node referenced by this identifier, if any.
        """
        return Self.node_env.get_first(Self.symbol)

    @langkit_property()
    def type_check():
        return Self.referenced_node.type_check()


class IntegerLiteral(Expr):
    """
    Integer literal.
    """
    token_node = True

    @langkit_property()
    def type_check():
        return Self.lookup_type("int")


class StringLiteral(Expr):
    """
    String literal.
    """
    token_node = True

    @langkit_property()
    def type_check():
        return Self.lookup_type("string")


class UnitLiteral(Expr):
    """
    Literal representing the unit value.
    """
    @langkit_property()
    def type_check():
        return Self.lookup_type("unit")


class NullLiteral(Expr):
    """
    Literal representing a null node.
    """
    token_node = True


@abstract
class TypeExpr(Declaration):
    """
    Reference to a type.
    """
    @langkit_property(public=True, kind=AbstractKind.abstract,
                      return_type=T.String)
    def name():
        """
        Return the name of the type.
        """
        pass


class BuiltinTypeDecl(TypeExpr):
    """
    Declaration of a builtin type using the BUILTIN_DECL keyword.

    For instance::
       BUILTIN_DECL int;
    """
    identifier = Field(type=Identifier)

    env_spec = EnvSpec(add_to_env_kv(Self.identifier.symbol, Self))

    @langkit_property()
    def name():
        return Self.identifier.text


class TypeName(TypeExpr):
    """
    Named type annotation.
    """
    identifier = Field(type=Identifier)

    @langkit_property()
    def name():
        return Self.identifier.text


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

    @langkit_property(return_type=Expr, public=True)
    def default():
        """
        Return the default value of the parameter.
        """
        return No(Expr)


class DefaultParam(ParameterDecl):
    """
    Parameter with a default value.

    For instance::
       fun add(x, y=42) = ...
    """
    default_expr = Field(type=Expr)

    @langkit_property()
    def default():
        return Self.default_expr


class Not(Expr):
    """
    Negation of a boolean value.
    """
    value = Field(type=Expr)

    @langkit_property()
    def type_check():
        return Self.value.type_check


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


class BinOp(Expr):
    """
    Binary operation.
    """
    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)

    @langkit_property()
    def type_check():
        return Self.op.match(
            lambda _=Op.alt_concat: Self.compare_type(
                expected=Self.lookup_type("string"),
                expected_origin=Self.op,
                value=Self.left
            ),

            lambda _: new_typing_judgment(UndefinedType.new(origin=Self.op))
        )


class Unpack(Expr):
    """
    Unpacking operator, written '*'.

    For instance::
       \*listValue
    """
    collection_expr = Field(type=Expr)


class Assign(Declaration):
    """
    Assign expression.
    An assignment associates a name with a value, and returns Unit.

    For instance::
       let message: string = "Hello World"
    """
    identifier = Field(type=Identifier)
    type_annotation = Field(type=TypeName)
    value = Field(type=Expr)

    env_spec = EnvSpec(
        add_to_env_kv(Self.identifier.symbol, Self.value)
    )

    @langkit_property()
    def type_check():
        return Self.compare_type(
            expected=Self.lookup_type(Self.type_annotation.identifier.symbol),
            expected_origin=Self.type_annotation,
            value=Self.value
        )


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


class DotCall(Expr):
    """
    Call a property with arguments using dot notation
    """
    receiver = Field(type=Expr)
    member = Field(type=Identifier)
    arguments = Field(type=Arg.list)


class SafeCall(DotCall):
    """
    Call a property using the ?. operator
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

    @langkit_property(return_type=T.String, public=True,
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
        return Self.binding_name.length > 0

    @langkit_property(return_type=T.ValuePattern, public=True)
    def value_part():
        """
        Return the value pattern contained in the pattern , if any.
        """
        return No(ValuePattern)

    @langkit_property(return_type=T.Expr, public=True)
    def predicate_part():
        """
        Return the filtering predicate associated with this pattern, if any.
        """
        return No(Expr)

    @langkit_property(return_type=T.Bool, public=True)
    def contains_chained():
        """
        Return whether this pattern contains a value part
        that is a chained pattern.
        """
        return If(Self.value_part.is_null,
                  False,
                  Self.value_part.is_a(ChainedNodePattern))


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

    @langkit_property()
    def predicate_part():
        return Self.predicate


@abstract
class ValuePattern(UnfilteredPattern):
    """
    Root node class for patterns that filter values.
    (As opposed to patterns that only bind values to a given name without
    doing any kind of filtering)
    """

    @langkit_property()
    def binding_name():
        return String("")

    @langkit_property()
    def value_part():
        return Self


class BindingPattern(UnfilteredPattern):
    """
    Pattern comprising only an identifier.
    """

    binding = Field(type=Identifier)

    @langkit_property()
    def binding_name():
        return Self.binding.text


class FullPattern(BindingPattern):
    """
    Pattern comprising a binding name and a value pattern.

    For instance::
       o @ ObjectDecl
    """

    value_pattern = Field(type=ValuePattern)


class IsClause(Expr):
    """
    Check that a node matches a given pattern
    """
    node_expr = Field(type=Expr)
    pattern = Field(type=BasePattern)


class UniversalPattern(ValuePattern):
    """
    Universal pattern that matches any value.

    For instance::
       let declParent = query _ [children(depth==1)] BasicDecl
    """
    pass


class Query(Expr):
    """
    Query against a pattern.
    this kind of expression will return every AST node that matches the given
    pattern.

    For instance::
       let withAspects = query ObjectDecl [child] AspectAssoc
    """

    pattern = Field(type=BasePattern)


class ArrowAssoc(LKQLNode):
    """
    Arrow association of the form: id <- expr.
    This construction is meant to be used a part of a list comprehension
    """

    binding_name = Field(type=Identifier)
    coll_expr = Field(type=Expr)


class ListComprehension(Expr):
    """
    List comprehension of the form:
        [ expr | generator1, generator2, ...,  opt(guard)]
    """

    expr = Field(type=Expr)
    generators = Field(type=ArrowAssoc.list)
    guard = Field(type=Expr)


class ValExpr(Expr):
    """
    Expression of the form: val id = value; expr

    For instance::
       val x = 40;
       val y = 2;
       x + y
    """

    binding_name = Field(type=Identifier)
    binding_value = Field(type=Expr)
    expr = Field(type=Expr)


class FunDecl(Declaration):
    """
    Function definition

    For instance::
       fun add(x, y) = x + y
    """

    name = Field(type=Identifier)
    parameters = Field(type=ParameterDecl.list)
    body_expr = Field(type=Expr)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))

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
        return Self.parameters.find(lambda p: p.text == name)

    @langkit_property(return_type=T.Bool, public=True)
    def has_parameter(name=T.String):
        """
        Return whether the function has a parameter with the given name.
        """
        return dsl_expr.Not(Self.find_parameter(name).is_null)

    @langkit_property(return_type=DefaultParam.entity.array, public=True)
    def default_parameters():
        """
        Return the defaults parameters of the function, if any.
        """
        return Self.parameters.filtermap(
            lambda p: p.cast(DefaultParam).as_entity,
            lambda p: p.is_a(DefaultParam)
        )


class FunCall(Expr):
    """
    Function call.

    For instance::
       add(2, 40)
    """

    name = Field(type=Identifier)
    arguments = Field(type=Arg.list)

    @langkit_property(return_type=T.Int, public=True)
    def arity():
        """
        Return the number of arguments of the function call
        """
        return Self.arguments.length

    @langkit_property(return_type=FunDecl.entity, public=True)
    def called_function():
        """
        Return the function definition that corresponds to the called function.
        """
        return Self.node_env.get_first(Self.name.symbol).cast(FunDecl)

    @langkit_property(return_type=NamedArg.entity.array, memoized=True)
    def call_args():
        """
        Return the explicit arguments of this call as named arguments.
        """
        return Self.arguments.map(
            lambda pos, arg:
            arg.match(
                lambda e=ExprArg:
                SynthNamedArg.new(arg_name=Self.called_function()
                                  .parameters.at(pos).identifier,
                                  value_expr=e.value_expr).cast(
                    NamedArg).as_entity,
                lambda n=NamedArg: n.as_entity,
            )
        )

    @langkit_property(return_type=NamedArg.entity.array, public=True,
                      memoized=True)
    def resolved_arguments():
        """
        Return the arguments of this call (default arguments included)
        as named arguments.
        """
        return Let(
            lambda call_args=Self.as_entity.call_args:
            Let(lambda default_args=
                       Self.called_function()
                           .default_parameters()
                           .filter(lambda p:
                                   dsl_expr.Not(call_args.any(
                                       lambda e: e.name().text == p.name())))
                           .map(lambda param:
                                SynthNamedArg.new(
                                    arg_name=param.param_identifier.node,
                                    value_expr=param.default_expr.node)
                                .cast(NamedArg)
                                .as_entity)
                : call_args.concat(default_args)))

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
               (Self.name.text == String("debug"))


class SelectorExprMode(LKQLNode):
    """
    Modes for selector values:
        - default: add the value to the result set
        - rec: add the value to the result set and call the selector recursively
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
    arms = Field(type=SelectorArm.list)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))

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

    @langkit_property(return_type=T.String, public=True)
    def binding_name():
        """
        Return the binding name associated with this selector call, if any.
        """
        return If(Self.binding.is_null,
                  String(""),
                  Self.binding.text)

    @langkit_property(return_type=Expr)
    def expr_for_arg(name=T.String):
        return Let(lambda x=Self.args.find(lambda a: a.arg_name.text == name)
                   : If(x.is_null, No(Expr), x.expr))

    @langkit_property(return_type=SelectorDecl.entity, public=True,
                      memoized=True)
    def called_selector():
        """
        Return the function definition that corresponds to the called function.
        """
        return Self.node_env.get_first(Self.selector_identifier.symbol) \
            .cast(SelectorDecl)

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def depth_expr():
        """
        Return the expression associated to the 'expr' argument, if any.
        """
        return Self.expr_for_arg(String('depth'))

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def max_depth_expr():
        """
        If if the 'max_depth' arg is set and 'depth' is not set, return the
        expression for 'max_depth'. If 'depth' is set return it's expression.
        If neither 'depth' or 'max_depth' is set, return a null expression.
        """
        return If(Self.depth_expr.is_null,
                  Self.expr_for_arg(String('max_depth')),
                  Self.depth_expr)

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def min_depth_expr():
        """
        If if the 'min_depth' arg is set and 'depth" is not set, return the
        expression for 'min_depth'. If 'depth' is set return it's expression.
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
    Node pattern of the form: KindName(field: val, prop: val, selector: Pattern)

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
    pattern = AbstractField(type=UnfilteredPattern)


class SelectorLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
        quantifier selector_name pattern
    """
    selector = Field(type=SelectorCall)
    pattern = Field(type=UnfilteredPattern)


class FieldLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
       field pattern
    """
    field = Field(type=Identifier)
    pattern = Field(type=UnfilteredPattern)


class PropertyLink(ChainedPatternLink):
    """
    Element of a chained pattern of the form:
       property(args) pattern
    """
    property = Field(type=FunCall)
    pattern = Field(type=UnfilteredPattern)


class ChainedNodePattern(ValuePattern):
    """
    Node pattern of the form:
        Kind1(details...) selector1 Kind2(details...) selector2 ... KindN
    """
    first_pattern = Field(type=UnfilteredPattern)
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
           | _          => false
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


lkql_grammar = Grammar('main_rule')
G = lkql_grammar
# noinspection PyTypeChecker
lkql_grammar.add_rules(
    main_rule=List(Or(G.decl, G.expr),
                   list_cls=TopLevelList),

    query=Query(Token.QueryTok, G.pattern),

    pattern=Or(FilteredPattern(G.unfiltered_pattern_optional_chain,
                               Token.When,
                               G.expr),
               G.unfiltered_pattern_optional_chain),

    unfiltered_pattern_optional_chain=Or(
        ChainedNodePattern(
            G.unfiltered_pattern,
            List(Or(SelectorLink(G.selector_call, G.unfiltered_pattern),
                    FieldLink(G.identifier, G.unfiltered_pattern),
                    PropertyLink(G.fun_call, G.unfiltered_pattern)))
        ),
        G.unfiltered_pattern
    ),

    unfiltered_pattern=Or(FullPattern(G.identifier, Token.At, G.value_pattern),
                          BindingPattern(G.identifier),
                          G.value_pattern),

    value_pattern=Or(G.node_pattern,
                     G.universal_pattern),

    universal_pattern=UniversalPattern(Token.UnderScore),

    node_pattern=Or(G.extended_node_pattern, G.node_kind_pattern),

    node_kind_pattern=NodeKindPattern(G.kind_name),

    detail_value=Or(DetailPattern(G.pattern), DetailExpr(G.expr)),

    extended_node_pattern=ExtendedNodePattern(Or(G.universal_pattern,
                                                 G.node_kind_pattern),
                                              Pick(Token.LPar,
                                                   List(G.node_pattern_detail,
                                                        sep=Token.Coma),
                                                   Token.RPar)),

    node_pattern_detail=Or(NodePatternSelector(
        SelectorCall(G.identifier,
                     Opt(Pick(G.identifier, Token.At)),
                     G.identifier,
                     Opt(Token.LPar,
                         List(G.named_arg,
                              sep=Token.Coma,
                              empty_valid=False),
                         Token.RPar)),
        Token.Colon,
        G.pattern),
        NodePatternField(G.identifier,
                         Token.Colon,
                         G.detail_value),
        NodePatternProperty(G.fun_call,
                            Token.Colon,
                            G.detail_value)),

    selector_call=SelectorCall(G.identifier,
                               Opt(Pick(G.identifier, Token.At)),
                               G.identifier,
                               Opt(Token.LPar,
                                   List(G.named_arg,
                                        sep=Token.Coma,
                                        empty_valid=False),
                                   Token.RPar)),

    arrow_assoc=ArrowAssoc(G.identifier, Token.LArrow, G.expr),

    listcomp=ListComprehension(Token.LBrack,
                               G.expr,
                               Token.Pipe,
                               List(G.arrow_assoc,
                                    sep=Token.Coma, empty_valid=False),
                               Opt(Token.Coma, G.expr),
                               Token.RBrack),

    decl=Or(G.fun_decl,
            G.selector_decl,
            G.assign,
            BuiltinTypeDecl(Token.Builtin,
                            Identifier(Or(Token.Identifier, Token.KindName)),
                            Token.SemiCol)),

    expr=Or(BinOp(G.expr,
                  Or(Op.alt_and(Token.And),
                     Op.alt_or(Token.Or)),
                  G.comp_expr),
            G.comp_expr,
            G.val_expr),

    comp_expr=Or(IsClause(G.comp_expr, Token.Is, G.pattern),
                 InClause(G.comp_expr, Token.In, G.expr),
                 Not(Token.Not, G.expr),
                 BinOp(G.comp_expr,
                       Or(Op.alt_eq(Token.EqEq),
                          Op.alt_neq(Token.Neq),
                          Op.alt_concat(Token.Amp),
                          Op.alt_lt(Token.Lt),
                          Op.alt_leq(Token.LEq),
                          Op.alt_gt(Token.Gt),
                          Op.alt_geq(Token.GEq)),
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

    value_expr=Or(Unwrap(G.value_expr, Token.ExclExcl),
                  DotCall(G.value_expr,
                          Token.Dot,
                          G.identifier,
                          Token.LPar,
                          List(G.arg, sep=Token.Coma, empty_valid=True),
                          Token.RPar),
                  SafeCall(G.value_expr,
                           Token.QuestionDot,
                           G.identifier,
                           Token.LPar,
                           List(G.arg, sep=Token.Coma, empty_valid=True),
                           Token.RPar),
                  DotAccess(G.value_expr, Token.Dot, G.identifier),
                  SafeAccess(G.value_expr, Token.QuestionDot, G.identifier),
                  Indexing(G.value_expr, Token.LBrack, G.expr, Token.RBrack),
                  G.fun_call,
                  G.query,
                  G.listcomp,
                  G.match,
                  G.identifier,
                  G.string_literal,
                  G.bool_literal,
                  G.unit_literal,
                  NullLiteral(Token.Null),
                  G.integer,
                  Pick(Token.LPar, G.expr, Token.RPar),
                  G.if_then_else),

    val_expr=ValExpr(Token.Val, G.identifier, Token.Eq,
                     G.expr, Token.SemiCol, G.expr),

    assign=Assign(Token.Let,
                  G.identifier,
                  Token.Colon,
                  G.type_expr,
                  Token.Eq,
                  G.expr),

    fun_decl=FunDecl(Token.Fun,
                     G.identifier,
                     Token.LPar,
                     List(G.param, empty_valid=True, sep=Token.Coma),
                     Token.RPar,
                     Token.Eq,
                     G.expr),

    fun_call=FunCall(G.identifier,
                     Token.LPar,
                     List(G.arg, empty_valid=True, sep=Token.Coma),
                     Token.RPar),

    selector_decl=SelectorDecl(Token.Selector,
                               G.identifier,
                               List(G.selector_arm, empty_valid=False)),

    selector_arm=SelectorArm(Token.Pipe,
                             G.pattern,
                             Token.BigRArrow,
                             List(G.selector_expr,
                                  empty_valid=False, sep=Token.Box)),

    selector_expr=SelectorExpr(Or(SelectorExprMode.alt_rec(Token.Rec),
                                  SelectorExprMode.alt_skip(Token.Skip),
                                  SelectorExprMode.alt_default()),
                               G.unpackable_expr),

    unpackable_expr=Or(G.expr, Unpack(Token.Mul, G.expr)),

    match=Match(Token.Match, G.expr, List(G.match_arm, empty_valid=False)),

    match_arm=MatchArm(Token.Pipe,
                       G.pattern,
                       Token.BigRArrow,
                       G.expr),

    if_then_else=IfThenElse(
        Token.If, G.expr, Token.Then, G.expr, Token.Else, G.expr
    ),

    type_expr=TypeName(Identifier(Or(Token.Identifier, Token.KindName))),

    identifier=Identifier(Token.Identifier),

    kind_name=Identifier(Token.KindName),

    integer=IntegerLiteral(Token.Integer),

    bool_literal=Or(BoolLiteral.alt_true(Token.TrueLit),
                    BoolLiteral.alt_false(Token.FalseLit)),

    string_literal=StringLiteral(Token.String),

    unit_literal=UnitLiteral(Token.LPar, Token.RPar),

    arg=Or(NamedArg(G.identifier, Token.Eq, G.expr), ExprArg(G.expr)),

    named_arg=NamedArg(G.identifier, Token.Eq, G.expr),

    param=Or(DefaultParam(G.identifier, Token.Eq, G.expr),
             ParameterDecl(G.identifier))
)
