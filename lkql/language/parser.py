from langkit.parsers import Grammar, Or, List, Pick, Opt
from langkit.dsl import (
    T, ASTNode, abstract, Field, AbstractField, has_abstract_list, synthetic,
    UserField, LogicVar, Symbol
)
from langkit.expressions import (
    Self, String, No, langkit_property, AbstractKind, Let, If, Property,
    Bind, LogicFalse, Entity, Not, Var, ArrayLiteral
)
import langkit.expressions as dsl_expr
from langkit.envs import add_to_env_kv, EnvSpec
from lexer import Token


@abstract
@has_abstract_list
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """

    type_eq = Property(LogicFalse(), public=False)

    type_var = Property(Entity.create_logic_var, memoized=True)

    @langkit_property(public=False, return_type=LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def create_logic_var():
        """
        LogicVar constructor.
        """
        pass

    @langkit_property(public=True, return_type=T.AnalysisUnit, external=True,
                      uses_envs=False, uses_entity_info=False)
    def prelude_unit():
        """
        Retrieve the Prelude unit.
        """
        pass

    @langkit_property(public=True, return_type=T.TypeDecl.entity, memoized=True)
    def lookup_type(name=T.Symbol):
        """
        Return the TypeDecl node representing the type with the given name.
        If there is no type named `name`, return the error type.
        """
        return Self.node_env.get_first(name).cast_or_raise(TypeDecl)

    @langkit_property(public=True, return_type=T.TypeDecl.entity, external=True,
                      uses_entity_info=True, uses_envs=True)
    def lookup_type_name(name=T.String):
        pass

    @langkit_property(public=True, return_type=T.TypeDecl.entity)
    def get_type():
        """
        Return an node's type.
        A special "error" type will be returned if the node isn't well-typed.
        """
        return If(Entity.type_eq.solve,
                  Entity.type_var.get_value.cast_or_raise(TypeDecl),
                  Self.lookup_type("error"))

    @langkit_property(public=True, return_type=T.String)
    def type_name():
        """
        Return the name of the node's type.
        """
        return If(Entity.get_type.is_null,
                  String("error"),
                  Entity.get_type.name)


@abstract
class Declaration(LKQLNode):
    """
    Root node class for LKQL declarations.
    """

    type_eq = Property(Bind(Entity.type_var, Self.lookup_type("unit")))


@abstract
@has_abstract_list
class Expr(LKQLNode):
    """
    Root node class for LKQL expressions.
    """

    type_eq = Property(LogicFalse())

    @langkit_property()
    def get_type():
        return If(
            Entity.type_eq.solve,
            Entity.type_var.get_value.cast_or_raise(TypeDecl),
            Self.lookup_type("error")
        )


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

    type_eq = Property(
        Bind(Entity.type_var, Self.lookup_type("bool"))
    )


class Identifier(Expr):
    """
    Regular identifier.
    """
    token_node = True

    type_eq = Property(
        Let(lambda ref=Self.referenced_node:
            If(ref.is_null,
               LogicFalse(),
               ref.type_eq & Bind(Entity.type_var, ref.type_var))))

    @langkit_property(return_type=LKQLNode.entity, public=True)
    def referenced_node():
        """
        Return the node referenced by this identifier, if any.
        """
        return (Self.node_env.get_first(Self.symbol))


class IntegerLiteral(Expr):
    """
    Integer literal.
    """
    token_node = True

    type_eq = Property(
        Bind(Entity.type_var, Self.lookup_type("int"))
    )


class StringLiteral(Expr):
    """
    String literal.
    """
    token_node = True

    type_eq = Property(
        Bind(Entity.type_var, Self.lookup_type("string"))
    )


class UnitLiteral(Expr):
    """
    Literal representing the unit value.
    """
    type_eq = Property(
        Bind(Entity.type_var, Self.lookup_type("unit"))
    )


class NullLiteral(Expr):
    """
    Literal representing a null node.
    """
    token_node = True


@abstract
class TypeDecl(Declaration):
    """
    Declaration of a new type.
    """

    type_eq = Property(Bind(Entity.type_var, Self))

    @langkit_property(public=True, return_type=T.String,
                      kind=AbstractKind.abstract)
    def name():
        """
        Return the name of the type.
        """
        pass


class BuiltinTypeDecl(TypeDecl):
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


@abstract
class TypeRef(LKQLNode):
    """
    Referenced to an existing type.
    """

    @langkit_property(return_type=T.String, kind=AbstractKind.abstract,
                      public=True)
    def name():
        """
        Return the name of the type referenced by this node.
        """
        pass


@abstract
class TypeNameBase(TypeRef):
    """
    Base class for syntactic & synthetic TypeName nodes.
    """
    type_eq = Property(
        Let(lambda t=Entity.lookup_type_name(Entity.name):
            If(t.is_null,
               LogicFalse(),
               Bind(Entity.type_var, t))))


class TypeName(TypeNameBase):
    """
    Named type annotation.
    """
    identifier = Field(type=Identifier)

    @langkit_property()
    def name():
        return Self.identifier.text


@synthetic
class SynthTypeName(TypeNameBase):
    """
    Synthetic TypeName node.
    """
    name_val = UserField(type=T.String, public=False)

    @langkit_property()
    def name():
        return Self.name_val


class IfThenElse(Expr):
    """
    Expression of the form: if CONDITION then EXPR1 else EXPR2
    """
    condition = Field(type=Expr)
    then_expr = Field(type=Expr)
    else_expr = Field(type=Expr)

    type_eq = Property(
        Entity.condition.type_eq &
        Entity.then_expr.type_eq &
        Entity.else_expr.type_eq &
        Bind(Entity.type_var, Entity.then_expr.type_var) &
        Bind(Entity.condition.type_var, Self.lookup_type("bool")) &
        Bind(Entity.then_expr.type_var, Entity.else_expr.type_var)
    )


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
    type_annotation = Field(type=TypeRef)

    env_spec = EnvSpec(
        add_to_env_kv(Self.param_identifier.symbol, Self)
    )

    type_eq = Property(
        Entity.type_annotation.type_eq &
        Bind(Entity.type_var, Entity.type_annotation.type_var)
    )

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


@synthetic
class SynthParameterDecl(ParameterDecl):
    """
    Synthetic ParameterDecl node.
    """
    pass


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


@synthetic
class SynthDefaultParam(DefaultParam):
    """
    Synthetic DefaultParam node.
    """
    pass


class NotOp(Expr):
    """
    Negation of a boolean value.
    """
    value = Field(type=Expr)

    type_eq = Property(
        Entity.value.type_eq() &
        Bind(Entity.value.type_var, Self.lookup_type("bool")) &
        Bind(Entity.type_var, Entity.value.type_var)
    )


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

    type_eq = Property(
        Entity.left.type_eq &
        Entity.right.type_eq &

        Self.op.match(
            lambda _=Op.alt_concat:
            Bind(Entity.type_var, Self.lookup_type("string")) &
            Bind(Entity.left.type_var, Self.lookup_type("string")),

            lambda _=Op.alt_plus: Entity.operands_type_is("int") &
            Bind(Entity.type_var, Self.lookup_type("int")),

            lambda _=Op.alt_minus: Entity.operands_type_is("int") &
            Bind(Entity.type_var, Self.lookup_type("int")),

            lambda _=Op.alt_mul: Entity.operands_type_is("int") &
            Bind(Entity.type_var, Self.lookup_type("int")),

            lambda _=Op.alt_div: Entity.operands_type_is("int") &
            Bind(Entity.type_var, Self.lookup_type("int")),

            lambda _=Op.alt_and: Entity.operands_type_is("bool") &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_or: Entity.operands_type_is("bool") &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_eq:
            Bind(Entity.left.type_var, Entity.right.type_var) &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_neq:
            Bind(Entity.left.type_var, Entity.right.type_var) &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_gt:
            (Entity.operands_type_is("int") | Entity.operands_type_is("string")) &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_geq:
            (Entity.operands_type_is("int") | Entity.operands_type_is("string")) &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_lt:
            (Entity.operands_type_is("int") | Entity.operands_type_is("string")) &
            Bind(Entity.type_var, Self.lookup_type("bool")),

            lambda _=Op.alt_leq:
            (Entity.operands_type_is("int") | Entity.operands_type_is("string")) &
            Bind(Entity.type_var, Self.lookup_type("bool")),
        )
    )

    @langkit_property()
    def operands_type_is(name=T.Symbol):
        return Let(
            lambda t=Self.lookup_type(name):
            Bind(Entity.left.type_var, t) & Bind(Entity.right.type_var, t)
        )

    @langkit_property()
    def get_type():
        return If(Entity.type_eq.solve,
                  Entity.type_var.get_value.cast_or_raise(TypeDecl),
                  Self.lookup_type('error'))


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
    type_annotation = Field(type=TypeRef)
    value = Field(type=Expr)

    env_spec = EnvSpec(
        add_to_env_kv(Self.identifier.symbol, Self.value)
    )

    type_eq = Property(
        Entity.type_annotation.type_eq &
        Entity.value.type_eq &
        Bind(Entity.value.type_var, Entity.type_annotation.type_var) &
        Bind(Entity.type_var, Self.lookup_type("unit"))
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
    call = Field(type=T.FunCall)


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

    @langkit_property(return_type=T.Symbol, public=True)
    def value_type_name():
        """
        Return the kind name of the values that match this pattern.
        """
        return No(Symbol)


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

    @langkit_property()
    def type_eq():
        node_type = Var(Self.lookup_type(Self.pattern.value_type_name)
                        ._.cast(T.Prototype))

        return If(node_type.is_null | Not(node_type.is_node_prototype),
                  LogicFalse(),
                  Bind(Entity.type_var, Entity.get_result_type(node_type)))

    @langkit_property(return_type=T.SynthPrototype, public=False, memoized=True)
    def get_result_type(node_type=T.Prototype.entity):
        name_node = SynthTypeName.new(
            name_val=node_type.name
        )
        return (Self.lookup_type("List").cast(T.Prototype)
                .apply_type_args(ArrayLiteral([name_node.cast(TypeRef).as_entity])))


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


class ValDecl(Declaration):
    """
    Declarative part of a val expression.
    """
    binding_name = Field(type=Identifier)
    binding_value = Field(type=Expr)

    type_eq = Property(
        Entity.binding_value.type_eq &
        Bind(Entity.type_var, Entity.binding_value.type_var)
    )


class ValExpr(Expr):
    """
    Expression of the form: val id = value; expr

    For instance::
       val x = 40;
       val y = 2;
       x + y
    """

    decl = Field(type=ValDecl)
    expr = Field(type=Expr)

    env_spec = EnvSpec(
        add_to_env_kv(Self.decl.binding_name.symbol, Self.decl)
    )

    type_eq = Property(
        Entity.decl.binding_value.type_eq &
        Entity.expr.type_eq &
        Bind(Entity.type_var, Entity.expr.type_var)
    )


class FunKind(LKQLNode):
    """
    Denoted the "kind" of a function.
    """
    enum_node = True
    alternatives = ["function"]


@abstract
class FunSpecBase(Declaration):
    """
    Base node for syntactic & synthetic function declarations.
    """
    fun_kind = Field(type=FunKind)
    name = Field(type=Identifier)

    @langkit_property(return_type=ParameterDecl.entity.array, public=True,
                      kind=AbstractKind.abstract)
    def parameters():
        """
        Return the parameters of the function.
        """
        pass

    @langkit_property(return_type=TypeRef, public=True,
                      kind=AbstractKind.abstract)
    def return_type_annotation():
        """
        Return the type annotation for the return type of the function.
        """
        pass

    @langkit_property(return_type=T.Int, public=True)
    def arity():
        """
        Return the number of parameters of the function
        """
        return Entity.parameters.length

    @langkit_property(return_type=ParameterDecl, public=True)
    def find_parameter(name=T.String):
        """
        Return the parameter associated with the given name, if any.
        """
        return Entity.parameters.find(lambda p: p.name == name)._.node

    @langkit_property(return_type=T.Bool, public=True)
    def has_parameter(name=T.String):
        """
        Return whether the function has a parameter with the given name.
        """
        return Not(Entity.find_parameter(name).is_null)

    @langkit_property(return_type=DefaultParam.entity.array, public=True)
    def default_parameters():
        """
        Return the defaults parameters of the function, if any.
        """
        return Entity.parameters.filtermap(
            lambda p: p.cast(DefaultParam),
            lambda p: p.is_a(DefaultParam)
        )

    @langkit_property(return_type=T.SynthFunSpec, public=False, memoized=True)
    def monomorphize(type_args=T.TypeParameterValue.array):
        return SynthFunSpec.new(
            fun_kind=Self.fun_kind,
            name=Self.name,
            params=Entity.parameters.map(
                lambda p: Entity.monomorphize_param(p, type_args)
            ),
            ret_type=Let(
                lambda a=type_args.find(
                    lambda t: t.identifier ==
                    Self.return_type_annotation.as_entity.name
                )
                : If(a.is_null, Self.return_type_annotation, a.value)
            )
        )

    @langkit_property(return_type=T.ParameterDecl, memoized=True,
                      public=False)
    def monomorphize_param(param=ParameterDecl.entity,
                           type_args=T.TypeParameterValue.array):

        matching_arg = Var(type_args.find(
            lambda t: t.identifier == param.type_annotation.name
        ))

        return If(matching_arg.is_null,

                  param.node,

                  SynthParameterDecl.new(
                      param_identifier=param.param_identifier.node,
                      type_annotation=matching_arg.value
                  ).cast(ParameterDecl)
        )


@synthetic
class SynthFunSpec(FunSpecBase):
    """
    Synthetic function specification.
    """
    params = UserField(type=ParameterDecl.array, public=False)
    ret_type = UserField(type=TypeRef, public=False)

    @langkit_property()
    def parameters():
        return Self.params.map(lambda x: x.as_entity)

    @langkit_property()
    def return_type_annotation():
        return Self.ret_type


class FunSpec(FunSpecBase):
    """
    Function specification.

    For instance::
       fun incr(x: int) -> int
    """
    params = Field(type=ParameterDecl.list)
    ret_type = Field(type=TypeRef)

    @langkit_property()
    def parameters():
        return Self.params.map(lambda x: x.as_entity)

    @langkit_property()
    def return_type_annotation():
        return Self.ret_type


class FunDecl(Declaration):
    """
    Function definition.

    For instance::
       fun add(x: int, y: int) -> int = x + y
    """

    spec = Field(type=FunSpec)
    body_expr = Field(type=Expr)

    env_spec = EnvSpec(add_to_env_kv(Self.spec.name.symbol, Self))

    type_eq = Property(
        Entity.spec.parameters.logic_all(lambda p: p.type_eq) &
        Entity.spec.return_type_annotation.as_entity.type_eq &
        Entity.body_expr.type_eq &
        Bind(Entity.body_expr.type_var,
             Entity.spec.return_type_annotation.as_entity.type_var) &
        Bind(Entity.type_var, Self.lookup_type("unit"))
    )


class FunCall(Expr):
    """
    Function call.

    For instance::
       add(2, 40)
    """

    name = Field(type=Identifier)
    arguments = Field(type=Arg.list)

    type_eq = Property(
        Let(
            lambda f=Self.called_function:
            If(f.is_null | Not(f.is_a(FunDecl)) |
               (Entity.resolved_arguments.length != f.spec.arity),

               LogicFalse(),

               f.type_eq &
               Entity.resolved_arguments.logic_all(
                    lambda a: Let(
                        lambda p=f.spec.find_parameter(a.name.text)._.as_entity:
                        If(p.is_null,
                           LogicFalse(),
                           p.type_eq &
                           a.expr.as_entity.type_eq &
                           Bind(a.expr.as_entity.type_var, p.type_var))
                    )
               ) &
               f.spec.return_type_annotation.as_entity.type_eq &
               Bind(Entity.type_var,
                    f.spec.return_type_annotation.as_entity.type_var))
            )
    )

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
        return Self.node_env.get_first(Self.name.symbol) \
                    ._.cast_or_raise(FunDecl)

    @langkit_property(return_type=FunSpec.entity, public=True)
    def called_spec():
        """
        Return the specification of the function/method referenced by a FunCall
        node.
        """
        dot_call_parent = Var(Entity.parent.cast(DotCall))

        return If(dot_call_parent.is_null,

                  # If the call doesnt belong to a DotCall node, search the
                  # function in the environment.
                  (Self.node_env.get_first(Self.name.symbol)
                   ._.cast_or_raise(FunDecl).spec),

                  # Otherwise, search it in the receiver's prototype
                  (dot_call_parent.receiver.get_type._.cast_or_raise(Prototype)
                   .then(lambda p: p.find_method(Self.name.text)._.as_entity))
                  )


    @langkit_property(return_type=NamedArg.entity.array, memoized=True,
                      public=False)
    def call_args():
        """
        Return the explicit arguments of this call as named arguments.
        """
        return Let(
            lambda f=Self.called_function:
            If(f.is_null | (Self.arity > f.spec.arity),
               No(NamedArg.entity.array),
               Self.arguments.map(
                   lambda pos, arg:
                   arg.match(
                       lambda e=ExprArg:
                       SynthNamedArg.new(arg_name=Self.called_function()
                                         .spec.parameters.at(pos).identifier,
                                         value_expr=e.value_expr).cast(
                           NamedArg).as_entity,
                       lambda n=NamedArg: n.as_entity,
                   )
               ))
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
                           .spec
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
        syntactic construct that looks like a function call but isn't one).
        """
        return Not(Self.parent.is_a(NodePatternProperty) |
                   Self.parent.is_a(DotCall))

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

    @langkit_property()
    def value_type_name():
        return Self.kind_name.symbol


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

    @langkit_property()
    def value_type_name():
        return Self.node_pattern.value_type_name()


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


class TypeParameter(TypeDecl):
    """
    Type parameter in a prototype or function specification
    """
    identifier = Field(type=Identifier)

    env_spec = EnvSpec(
        add_to_env_kv(Self.identifier.symbol, Self)
    )

    @langkit_property()
    def name():
        return Self.identifier.text


@synthetic
@has_abstract_list
class TypeParameterValue(LKQLNode):
    """
    Key/value pair associating a type parameter's name to it's concrete type
    value.
    """
    identifier = UserField(type=T.String, public=False)
    value = UserField(type=TypeRef, public=False)


class PrototypeKind(LKQLNode):
    """
    Denotes the kind of a prototype.
    """
    enum_node = True
    alternatives = ["prototype", "astnode"]


@abstract
class PrototypeBase(TypeDecl):
    """
    Root node class for syntactic & synthetic prototype nodes.
    """
    kind = Field(type=PrototypeKind)
    identifier = Field(type=Identifier)

    @langkit_property()
    def name():
        return Entity.format_name(Entity.type_parameters.map(
            lambda t: t.name
        ))

    @langkit_property(return_type=TypeParameter.entity.array, public=True,
                      kind=AbstractKind.abstract)
    def type_parameters():
        """
        Return the type parameters of the prototype, if any.
        """
        pass

    @langkit_property(return_type=FunSpecBase.entity.array, public=True,
                      kind=AbstractKind.abstract)
    def fun_specs():
        """
        Return an array containing the function specifications of the
        prototype, if any.
        """
        pass

    @langkit_property(return_type=T.SynthPrototype, public=True, memoized=True)
    def apply_type_args(args=TypeRef.entity.array):
        resolved_args = Var(Entity.resolve_type_args(args))

        return If(
            resolved_args.length != Entity.type_parameters.length,
            No(T.SynthPrototype),
            SynthPrototype.new(
                kind=Self.kind,
                identifier=Self.identifier,
                full_name=Entity.format_name(args.map(lambda a: a.name)),
                type_params=(No(TypeParameter.entity.array)),
                specs=Entity.fun_specs.map(
                    lambda s: s.monomorphize(resolved_args)\
                        .cast(FunSpecBase).as_entity
                )
            )
        )

    @langkit_property(return_type=T.TypeParameterValue.array, public=False,
                      memoized=True)
    def resolve_type_args(args=TypeRef.entity.array):
        """
        Return a list of TypeParameterValue associating each concrete type
        argument to the matching formal parameter.
        """
        return If(args.length != Entity.type_parameters.length,
                  No(TypeParameterValue.array),
                  args.map(lambda i, a: TypeParameterValue.new(
                      identifier=Entity.type_parameters.at(i).identifier.text,
                      value=a.node
                  )))

    @langkit_property(return_type=T.String, public=False)
    def format_name(type_names=T.String.array):
        return Self.identifier.text.concat(
            If(type_names.length == 0,
               String(""),
               String("<").concat(Entity.format_args(type_names))\
               .concat(String(">")))
        )

    @langkit_property(return_type=T.String, public=False)
    def format_args(type_names=T.String.array, pos=(T.Int, 0)):
        return If(pos >= type_names.length,
                  String(""),
                  Let(lambda name=type_names.at(pos):
                      If(pos == 0,
                         name,
                         String(", ").concat(name))
                      ).concat(Entity.format_args(type_names, pos + 1))
                  )


@synthetic
class SynthPrototype(PrototypeBase):
    """
    Synthetic Prototype node.
    """
    full_name = UserField(type=T.String, public=False)
    type_params = UserField(type=TypeParameter.entity.array, public=False)
    specs = UserField(type=FunSpecBase.entity.array, public=False)

    type_parameters = Property(Self.type_params)

    fun_specs = Property(Self.specs)

    @langkit_property()
    def name():
        return Entity.full_name


class Prototype(PrototypeBase):
    """
    Represents a class prototype.
    """
    type_params = Field(type=TypeParameter.list)
    specs = Field(type=FunSpec.list)

    env_spec = EnvSpec(
        add_to_env_kv(Self.identifier.symbol, Self)
    )

    @langkit_property()
    def type_parameters():
        return Self.type_params.map(lambda p: p.as_entity)

    @langkit_property()
    def fun_specs():
        return Self.specs.map(lambda s: s.cast(FunSpecBase).as_entity)

    @langkit_property(return_type=T.Bool, public=True)
    def is_node_prototype():
        """
        Return whether this is the prototype of an AST node type.
        """
        return Self.kind.is_a(T.PrototypeKindAstnode)
    #
    @langkit_property(return_type=FunSpec, public=True)
    def find_method(name=T.String):
        """
        Return the method named "name", if any.
        """
        return Self.specs.find(lambda x: x.name.text == name)


@abstract
class ParametrizedGenericBase(TypeRef):
    """
    Root node class for syntactic & synthetic parametrized generics.
    """
    prototype_name = Field(type=TypeName)

    @langkit_property(public=True, memoized=True, kind=AbstractKind.abstract,
                      return_type=T.TypeRef.entity.array)
    def type_parameters():
        """
        Return an array containing the type parameters of a parametrized
        generic.
        """
        pass

    @langkit_property()
    def name():
        return Entity.prototype_name.name.concat(
            If(Entity.type_parameters.length == 0,
               String(""),
               String("<").concat(Entity.format_args).concat(String(">")))
        )

    @langkit_property(return_type=T.String, public=False)
    def format_args(pos=(T.Int, 0)):
        return If(pos >= Entity.type_parameters.length,
                  String(""),
                  Let(lambda name=Entity.type_parameters.at(pos).name:
                      If(pos == 0,
                         name,
                         String(", ").concat(name))
                  ).concat(Entity.format_args(pos + 1))
               )


class ParametrizedGeneric(ParametrizedGenericBase):
    """
    Actual type made from a parametrized generic prototype.

    For instance::
        List<List<int>>
    """
    parameters = Field(type=TypeRef.list)

    @langkit_property()
    def type_parameters():
        return Self.parameters.map(lambda p: p.as_entity)


@synthetic
class SynthParametrizedGeneric(ParametrizedGenericBase):
    """
    Synthetic ParametrizedGeneric node.
    """
    parameters = UserField(type=TypeRef.array, public=False)

    @langkit_property()
    def type_parameters():
        return Self.parameters.map(lambda p: p.as_entity)


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
            G.prototype,
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
                 NotOp(Token.Not, G.expr),
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
                  DotCall(G.value_expr, Token.Dot, G.fun_call),
                  SafeCall(G.value_expr, Token.QuestionDot, G.fun_call),
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

    val_expr=ValExpr(ValDecl(Token.Val, G.identifier, Token.Eq, G.expr),
                     Token.SemiCol,
                     G.expr),

    assign=Assign(Token.Let,
                  G.identifier,
                  Token.Colon,
                  G.type_expr,
                  Token.Eq,
                  G.expr),

    fun_kind=Or(FunKind.alt_function(Token.Fun)),

    fun_spec=FunSpec(G.fun_kind,
                     G.identifier,
                     Token.LPar,
                     List(G.param, empty_valid=True, sep=Token.Coma),
                     Token.RPar,
                     Token.RArrow,
                     G.type_expr),

    fun_decl=FunDecl(G.fun_spec,
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

    type_name=TypeName(Identifier(Or(Token.Identifier, Token.KindName))),

    type_expr=Or(G.type_name,
                 ParametrizedGeneric(G.type_name,
                                     Token.Lt,
                                     List(G.type_expr, sep=Token.Coma),
                                     Token.Gt)),

    prototype_kind=Or(PrototypeKind.alt_prototype(Token.Prototype),
                      PrototypeKind.alt_astnode(Token.AstNode)),

    prototype=Prototype(G.prototype_kind,
                        G.kind_name,
                        Opt(Token.Lt,
                            List(G.type_parameter, sep=Token.Coma,
                                 empty_valid=False),
                            Token.Gt),
                        Token.LCurl,
                        List(G.fun_spec, empty_valid=True),
                        Token.RCurl),

    type_parameter=TypeParameter(Identifier(Or(Token.Identifier,
                                               Token.KindName))),

    identifier=Identifier(Token.Identifier),

    kind_name=Identifier(Token.KindName),

    integer=IntegerLiteral(Token.Integer),

    bool_literal=Or(BoolLiteral.alt_true(Token.TrueLit),
                    BoolLiteral.alt_false(Token.FalseLit)),

    string_literal=StringLiteral(Token.String),

    unit_literal=UnitLiteral(Token.LPar, Token.RPar),

    arg=Or(NamedArg(G.identifier, Token.Eq, G.expr), ExprArg(G.expr)),

    named_arg=NamedArg(G.identifier, Token.Eq, G.expr),

    param=Or(DefaultParam(G.identifier,
                          Token.Colon,
                          G.type_expr,
                          Token.Eq,
                          G.expr),
             ParameterDecl(G.identifier, Token.Colon, G.type_expr))
)
