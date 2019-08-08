from langkit.parsers import Grammar, Or, List, Pick, Opt
from langkit.dsl import (
    T, ASTNode, abstract, Field, AbstractField, has_abstract_list, synthetic,
    UserField
)
from langkit.expressions import (
    Self, String, No, langkit_property, AbstractKind, Let, If, Property,
    Bind, LogicFalse, Entity, Not, Var, ArrayLiteral, Predicate
)
import langkit.expressions as dsl_expr
from langkit.envs import add_to_env_kv, EnvSpec, add_env
from lexer import Token


@abstract
@has_abstract_list
class LKQLNode(ASTNode):
    """
    Root node class for LKQL AST nodes.
    """

    type_eq = Property(LogicFalse(), public=False)

    polymorphic_type_eq = Property(Entity.get_type.type_eq, public=False)

    polymorphic_type_var = Property(Entity.get_type.type_var, public=False)

    type_var_dbg_name = Property(String("default"), public=False)

    type_var = Property(Entity.create_logic_var(Entity.type_var_dbg_name),
                        memoized=True)

    @langkit_property(public=True, return_type=T.Int, external=True,
                      uses_entity_info=False, uses_envs=False)
    def enable_solver_traces():
        """
        Activate logic solver traces.
        """
        pass

    @langkit_property(public=True, return_type=T.Int, external=True,
                      uses_entity_info=False, uses_envs=False)
    def disable_solver_traces():
        """
        Deactivate logic solver traces.
        """
        pass

    @langkit_property(public=False, return_type=T.LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def create_logic_var(name=T.String):
        """
        LogicVar constructor.
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
        Return this node's type.
        A special "error" type will be returned if the node isn't well-typed.
        """
        return If(Entity.type_eq.solve & Not(Entity.type_var.get_value.is_null),
                  Entity.type_var.get_value.cast_or_raise(TypeDecl),
                  Self.lookup_type("error"))

    @langkit_property(public=True, return_type=T.String)
    def type_name():
        """
        Return the name of the node's type.
        """
        return Entity.get_type.then(lambda t: t.name, String("error"))


@abstract
class Declaration(LKQLNode):
    """
    Root node class for LKQL declarations.
    """

    type_eq = Property(Bind(Entity.type_var, Self.lookup_type("unit")))

    type_var_dbg_name = Property(String("Declaration"))


@abstract
@has_abstract_list
class Expr(LKQLNode):
    """
    Root node class for LKQL expressions.
    """

    type_eq = Property(LogicFalse())

    type_var_dbg_name = Property(
        String("Expr(").concat(Self.text).concat(String(")"))
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

    type_var_dbg_name = Property(
        String("Bool(").concat(Self.text).concat(String(")"))
    )


class Identifier(Expr):
    """
    Regular identifier.
    """
    token_node = True

    type_eq = Property(
        Self.referenced_node.then(
            lambda ref: ref.type_eq & Bind(Entity.type_var, ref.type_var),
            LogicFalse()
        )
    )

    type_var_dbg_name = Property(
        String("Identifier(").concat(Self.text).concat(String(")"))
    )

    @langkit_property(return_type=LKQLNode.entity, public=True)
    def referenced_node():
        """
        Return the node referenced by this identifier, if any.
        """
        return Self.node_env.get_first(Self.symbol)


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

    @langkit_property(public=True, return_type=T.SynthTypeName, memoized=True)
    def make_synthetic_type_name():
        """
        Return a synthetic TypeName node referencing this type.
        """
        return SynthTypeName.new(
            name_val=Entity.name
        )

    @langkit_property(public=False, return_type=T.TypeDecl, memoized=True)
    def list_type():
        return Self.lookup_type("List").cast(Prototype).apply_type_args(
            ArrayLiteral([Entity])
        )

    @langkit_property(public=True, return_type=T.Bool)
    def is_indexable():
        """
        Return whether the current type is a subtype of Indexable.
        """
        return Entity.cast(PrototypeBase).then(lambda p:
            (p.identifier.text == String("Indexable")) |
            p.parent_prototype.then(lambda parent: parent.is_indexable, False),
            False
        )


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

    type_eq = Property(
        Entity.referenced_type.then(
            lambda t: t.type_eq & Bind(Entity.type_var, t),
            LogicFalse()
        )
    )

    type_var_dbg_name = Property(
        String("TypeRef(").concat(Entity.name).concat(String(")"))
    )

    @langkit_property(return_type=T.String, kind=AbstractKind.abstract,
                      public=True)
    def name():
        """
        Return the name of the type referenced by this node.
        """
        pass

    @langkit_property(return_type=T.TypeDecl.entity, public=False,
                      memoized=True)
    def referenced_type():
        return Entity.lookup_type_name(Entity.name)

    @langkit_property(return_type=T.TypeRef, public=False,
                      kind=AbstractKind.abstract)
    def monomorphize(formals=T.TypeParameter.entity.array,
                     actuals=T.TypeDecl.entity.array):
        """
        Given two lists representing formal type parameters and the associated
        actual types, monomorphize the current type reference if it is (or if
        it contains) a formal type parameter that belongs to the list.
        """
        pass


@abstract
class TypeNameBase(TypeRef):
    """
    Base class for syntactic & synthetic TypeName nodes.
    """
    @langkit_property()
    def monomorphize(formals=T.TypeParameter.entity.array,
                     actuals=T.TypeDecl.entity.array):
        idx = Var(formals.filtermap(
            lambda i, e: i,
            lambda e: e.name == Entity.name
        ))

        return If(idx.length == 0,
                  Self,
                  actuals.at(idx.at(0)).make_synthetic_type_name)


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

    expr = AbstractField(type=Expr)

    type_eq = Property(
        Entity.expr.type_eq &
        Bind(Entity.type_var, Entity.expr.type_var)
    )

    type_var_dbg_name = Property(
        String("Arg(").concat(Self.text).concat(String(")"))
    )

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


class NamedArg(Arg):
    """
    Named argument of the form: name=expression

    For instance::
       add(x=20, y=22)
    """
    arg_name = Field(type=Identifier)
    expr = Field(type=Expr)

    type_var_dbg_name = Property(
        String("NamedArg(").concat(Self.text).concat(String(")"))
    )

    @langkit_property()
    def name():
        return Self.arg_name


@synthetic
class SynthNamedArg(NamedArg):
    """
    Synthetic NamedArg node
    """
    type_var_dbg_name = Property(
        String("SynthNamedArg(").concat(Self.arg_name.text)
                                .concat(String(","))
                                .concat(Self.expr.text)
                                .concat(String(")"))
    )


class ExprArg(Arg):
    """
    Argument that consists of an expression
    """
    expr = Field(type=Expr)

    type_var_dbg_name = Property(
        String("ExprArg(").concat(Self.text).concat(String(")"))
    )


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

    type_var_dbg_name = Property(
        String("ParameterDecl(").concat(Self.text).concat(String(")"))
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
    type_var_dbg_name = Property(
        String("SynthParameterDecl(").concat(Self.text).concat(String(")"))
    )


class DefaultParam(ParameterDecl):
    """
    Parameter with a default value.

    For instance::
       fun add(x, y=42) = ...
    """
    default_expr = Field(type=Expr)

    type_var_dbg_name = Property(
        String("DefaultParam(").concat(Self.text).concat(String(")"))
    )

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

    type_eq = Property(
        Entity.receiver.type_eq &
        Entity.call.type_eq &
        Bind(Entity.type_var, Entity.call.type_var)
    )


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

    type_eq = Property(
        Entity.collection_expr.type_eq &
        Entity.index_expr.type_eq &
        Entity.collection_expr.get_type.cast(T.PrototypeBase).then(lambda t:
            Predicate(T.TypeDecl.is_indexable, Entity.collection_expr.type_var)
            & Bind(Entity.type_var,
                   t.find_method(String("at")).return_type.as_entity.get_type),
            LogicFalse()
        )
    )


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
        return Self.value_part.then(lambda v: v.is_a(ChainedNodePattern), False)


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

    type_eq = Property(
        Entity.pattern.type_eq &
        Entity.predicate.type_eq &
        Bind(Entity.predicate.type_var, Entity.lookup_type("bool")) &
        Bind(Entity.type_var, Entity.pattern.type_var)
    )

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

    type_eq = Property(
        Entity.value_pattern.type_eq &
        Bind(Entity.type_var, Entity.value_pattern.type_var)
    )

    env_spec = EnvSpec(
        add_to_env_kv(Self.binding.symbol, Self.value_pattern)
    )


class IsClause(Expr):
    """
    Check that a node matches a given pattern
    """
    node_expr = Field(type=Expr)
    pattern = Field(type=BasePattern)

    type_eq = Property(
        Entity.pattern.type_eq &
        Entity.node_expr.polymorphic_type_eq &
        Bind(Entity.node_expr.polymorphic_type_var, Entity.pattern.type_var) &
        Bind(Entity.type_var, Entity.lookup_type("bool"))
    )


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

    env_spec = EnvSpec(add_env())

    @langkit_property()
    def type_eq():
        t = Var(Entity.pattern.get_type.cast(T.Prototype))

        return If(t.is_null | Not(t.is_node_prototype),
                  LogicFalse(),
                  Bind(Entity.type_var, t.list_type))


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
    Denotes the "kind" of a function.
    """
    enum_node = True
    alternatives = ["function", "property", "field"]


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
    def return_type():
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
    def monomorphize(formals=T.TypeParameter.entity.array,
                     actuals=T.TypeDecl.entity.array):
        return SynthFunSpec.new(
            fun_kind=Self.fun_kind,
            name=Self.name,
            params=Entity.parameters.map(
                lambda p: Entity.monomorphize_param(formals, actuals, p)
            ),
            ret_type=Entity.return_type.as_entity.monomorphize(formals, actuals)
        )

    @langkit_property(return_type=T.ParameterDecl, memoized=True,
                      public=False)
    def monomorphize_param(formals=T.TypeParameter.entity.array,
                           actuals=T.TypeDecl.entity.array,
                           param=ParameterDecl.entity):
        matching_arg = Entity.actual_for_name(
            formals, actuals, param.type_annotation.name
        )

        return If(matching_arg.is_null,
                  param.node,
                  SynthParameterDecl.new(
                      param_identifier=param.param_identifier.node,
                      type_annotation=matching_arg.make_synthetic_type_name
                  ).cast(ParameterDecl)
        )

    @langkit_property(return_type=T.TypeDecl.entity, public=False)
    def actual_for_name(formals=T.TypeParameter.entity.array,
                        actuals=T.TypeDecl.entity.array,
                        name=T.String):
        """
        Return the type argument matching the formal type parameter named
        'name'.
        """
        idx = Var(formals.filtermap(
            lambda i, e: i,
            lambda e: e.name == name
        ))

        return If(idx.length == 0,
                  No(TypeDecl).as_entity,
                  actuals.at(0))


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
    def return_type():
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
    def return_type():
        return Self.ret_type


class FunDecl(Declaration):
    """
    Function definition.

    For instance::
       fun add(x: int, y: int) -> int = x + y
    """

    spec = Field(type=FunSpec)
    body_expr = Field(type=Expr)

    env_spec = EnvSpec(
        add_to_env_kv(Self.spec.name.symbol, Self),
        add_env()
    )

    type_eq = Property(
        Entity.spec.parameters.logic_all(lambda p: p.type_eq) &
        Entity.spec.return_type.as_entity.type_eq &
        Entity.body_expr.type_eq &
        Bind(Entity.body_expr.type_var,
             Entity.spec.return_type.as_entity.type_var) &
        Bind(Entity.type_var, Self.lookup_type("unit"))
    )

    type_var_dbg_name = Property(
        String("FunDecl(").concat(Self.spec.name.text).concat(String(")"))
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
        Entity.called_spec.then(lambda spec:
            # type check the arguments & verrify that the number of arguments
            # matches the arity of the function
            Let(lambda args=Entity.resolved_arguments:
                If(args.length != spec.arity,
                   LogicFalse(),
                   args.logic_all(lambda a:
                       spec.find_parameter(a.name.text).then(
                           lambda p: Bind(a.polymorphic_type_var, p.as_entity.type_var) &
                                     p.as_entity.type_eq &
                                     a.polymorphic_type_eq,
                           LogicFalse()
                       )
                   ))) &

            # type_check the return type annotation and bind the call's type
            # variable to this type
            spec.return_type.as_entity.type_eq &
            Bind(Entity.type_var, spec.return_type.as_entity.type_var),

            # If the spec is null, don't resolve
            LogicFalse()
        )
    )

    type_var_dbg_name = Property(
        String("FunCall(").concat(Self.name.text).concat(String(")"))
    )

    @langkit_property(return_type=T.Int, public=True)
    def arity():
        """
        Return the number of arguments of the function call
        """
        return Self.arguments.length

    @langkit_property(return_type=FunSpecBase.entity, public=True)
    def called_spec():
        """
        Return the specification of the function/method referenced by a FunCall
        node.
        """

        return Entity.parent.match(
            # If the call belong to a DotCall node, search the corresponding
            # method.
            lambda p=DotCall: p.receiver.get_type._.cast(PrototypeBase)
                     ._.find_method(Self.name.text),

            # Otherwise search the function in the environment.
            lambda _: Self.node_env.get_first(Self.name.symbol)
                      ._.cast(FunDecl).spec
        )

    @langkit_property(return_type=T.FunDecl.entity, public=True)
    def called_function():
        """
        Return the called function, if any.
        """
        return Entity.called_spec.parent.cast(FunDecl)

    @langkit_property(return_type=NamedArg.entity.array, public=True)
    def resolved_arguments():
        """
        Return the arguments of this call (default arguments included)
        as named arguments.
        """
        call_args = Var(Let(lambda spec=Entity.called_spec:
            If(spec.is_null | (spec.arity < Self.arity),

               No(NamedArg.entity.array),

               spec.parameters.map(lambda i, param:
                   If(Entity.find_named_arg(param.name).is_null,
                      # If there is no named argument with the current parameter's
                      # name, look if there is a positional argument at the
                      # parameter's position.
                      Let(lambda arg=Entity.arguments.at(i):
                          If(arg.is_null | arg.is_a(NamedArg),
                             No(NamedArg).as_entity,
                             Entity.make_named_arg(
                                 param.param_identifier, arg.expr
                             ))),

                      # Otherwies, use the named argument.
                      Entity.find_named_arg(param.name))
               ))
        ))

        return If(call_args.filter(lambda a: a.is_null).length > 0,
                  No(NamedArg.entity.array),
                  call_args)

    @langkit_property(return_type=NamedArg.entity, public=False, memoized=True)
    def find_named_arg(name=T.String):
        """
        Return the named argument named 'name', if any.
        """
        return Entity.arguments.find(lambda a:
            a.is_a(NamedArg) & (a.name.text == name)
        ).cast(NamedArg)

    @langkit_property(return_type=NamedArg.entity, public=False, memoized=True)
    def make_named_arg(name=Identifier.entity, value=Expr.entity):
        """
        Create a SynthNamedArg node with the given name and value.
        """
        return SynthNamedArg.new(
            arg_name=name.node,
            expr=value.node
        ).cast(NamedArg).as_entity

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
        return Self.quantifier.then(lambda q: q.text, String("all"))

    @langkit_property(return_type=T.String, public=True)
    def binding_name():
        """
        Return the binding name associated with this selector call, if any.
        """
        return Self.binding.then(lambda b: b.text, String(""))

    @langkit_property(return_type=Expr)
    def expr_for_arg(name=T.String):
        """
        Return the expression associated to the selector argument named 'name'.
        """
        return Self.args.find(lambda a: a.name.text == name)\
            .then(lambda a: a.expr, No(Expr))

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
        return Self.depth_expr.then(lambda e: e,
                                    Self.expr_for_arg(String('max_depth')))

    @langkit_property(return_type=Expr, public=True, memoized=True)
    def min_depth_expr():
        """
        If if the 'min_depth' arg is set and 'depth" is not set, return the
        expression for 'min_depth'. If 'depth' is set return it's expression.
        If neither 'depth' or 'min_depth' is set, return a null expression.
        """
        return Self.depth_expr.then(lambda e: e,
                                    Self.expr_for_arg(String('e')))


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

    type_eq = Property(
        Self.kind_name.referenced_node.cast(T.PrototypeBase).then(
            lambda p: p.type_eq & Bind(Entity.type_var, p.type_var),
            LogicFalse()
        )
    )

    type_var_dbg_name = Property(
        String("NodeKindPattern(").concat(Self.kind_name.text).concat(String(")"))
    )


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

    type_eq = Property(
        Entity.expr_value.type_eq &
        Bind(Entity.type_var, Entity.expr_value.type_var)
    )


class DetailPattern(DetailValue):
    """
    Pattern pattern data value
    """
    pattern_value = Field(type=BasePattern)

    type_eq = Property(
        Entity.pattern_value.type_eq &
        Bind(Entity.type_var, Entity.pattern_value.type_var)
    )


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

    type_eq = Property(
        Let(lambda t=Entity.lookup_type(Self.identifier.symbol)
                        .cast(T.PrototypeBase)
            : Entity.type_var.domain(
                ArrayLiteral([t]).concat(Entity.supertypes)
        ))
    )

    type_var_dbg_name = Property(
        String("PrototypeBase(").concat(Entity.name).concat(String(")"))
    )

    @langkit_property()
    def name():
        return Entity.format_name(Entity.type_parameters.map(
            lambda t: t.name
        ))

    @langkit_property(return_type=T.PrototypeBase.entity.array, public=True)
    def supertypes():
        """
        Return the prototypes of the supertypes, if any.
        """
        return Entity.parent_prototype.then(
            lambda p: ArrayLiteral([p]).concat(p.supertypes),
            ArrayLiteral([], element_type=T.PrototypeBase.entity)
        )

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

    @langkit_property(return_type=TypeRef.entity, public=True,
                      kind=AbstractKind.abstract)
    def parent_reference():
        """
        Return a type annotation referencing the parent prototype, if any.
        """
        pass

    @langkit_property(return_type=T.PrototypeBase.entity, public=True)
    def parent_prototype():
        """
        Return the parent prototype, if any.
        """
        return Entity.parent_reference._.get_type.cast(PrototypeBase)

    @langkit_property(return_type=T.SynthPrototype, public=False, memoized=True)
    def apply_type_args(args=TypeDecl.entity.array):
        return If(
            args.length != Entity.type_parameters.length,
            No(T.SynthPrototype),
            SynthPrototype.new(
                kind=Self.kind,
                identifier=Self.identifier,
                full_name=Entity.format_name(args.map(lambda a: a.name)),
                type_params=(No(TypeParameter.entity.array)),
                parent_ref=Entity.parent_reference._
                    .monomorphize(Entity.type_parameters, args),
                specs=Entity.fun_specs.map(
                    lambda s: s.monomorphize(Entity.type_parameters, args)\
                        .cast(FunSpecBase).as_entity
                )
            )
        )

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

    @langkit_property(return_type=FunSpecBase.entity, public=True)
    def find_method(name=T.String):
        """
        Return the method named "name", if any.
        """
        return Self.find_member(name, Entity.methods)

    @langkit_property(return_type=FunSpecBase.entity, public=True)
    def find_property(name=T.String):
        """
        Return the property named "name", if any.
        """
        return Self.find_member(name, Entity.properties)

    @langkit_property(return_type=FunSpecBase.entity, public=True)
    def find_field(name=T.String):
        """
        Return the field named "name", if any.
        """
        return Self.find_member(name, Entity.fields)

    @langkit_property(return_type=FunSpecBase.entity.array, public=True)
    def methods(inherited=(T.Bool, True)):
        m = Var(Entity.fun_specs
                .filter(lambda f: f.fun_kind.is_a(FunKind.alt_function)))

        return If(inherited,
                  m.concat(Entity.parent_prototype._.methods),
                  m)

    @langkit_property(return_type=FunSpecBase.entity.array, public=True)
    def properties(inherited=(T.Bool, True)):
        p = Var(Entity.fun_specs
                .filter(lambda f: f.fun_kind.is_a(FunKind.alt_property)))

        return If(inherited,
                  p.concat(Entity.parent_prototype._.properties),
                  p)

    @langkit_property(return_type=FunSpecBase.entity.array, public=True)
    def fields(inherited=(T.Bool, True)):
        f = Var(Entity.fun_specs
                .filter(lambda f: f.fun_kind.is_a(FunKind.alt_field)))

        return If(inherited,
                  f.concat(Entity.parent_prototype._.methods),
                  f)

    @langkit_property(return_type=FunSpecBase.entity, public=False)
    def find_member(name=T.String, members=T.FunSpecBase.entity.array):
        return members.find(lambda m: m.name.text == name)


    @langkit_property(return_type=T.Bool, public=True)
    def is_node_prototype():
        """
        Return whether this is the prototype of an AST node type.
        """
        return Self.kind.is_a(T.PrototypeKindAstnode)


@synthetic
class SynthPrototype(PrototypeBase):
    """
    Synthetic Prototype node.
    """
    full_name = UserField(type=T.String, public=False)
    type_params = UserField(type=TypeParameter.entity.array, public=False)
    specs = UserField(type=FunSpecBase.entity.array, public=False)
    parent_ref = UserField(type=TypeRef, public=False)

    type_parameters = Property(Self.type_params)

    fun_specs = Property(Self.specs)

    parent_reference = Property(Self.parent_ref.as_entity)

    @langkit_property()
    def name():
        return Entity.full_name


class Prototype(PrototypeBase):
    """
    Represents a class prototype.
    """
    type_params = Field(type=TypeParameter.list)
    parent_ref = Field(type=TypeRef)
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

    @langkit_property()
    def parent_reference():
        return Entity.parent_ref


@abstract
class ParametrizedGenericBase(TypeRef):
    """
    Root node class for syntactic & synthetic parametrized generics.
    """
    prototype_name = Field(type=TypeName)

    @langkit_property()
    def type_eq():
        return Entity.type_parameters.logic_all(lambda p: p.type_eq) & \
               Entity.prototype_name.type_eq & \
               Entity.prototype_name.get_type.cast(Prototype) \
                   ._.apply_type_args(
                          Entity.type_parameters.map(lambda t: t.get_type)
                   ).then(lambda p: Bind(Entity.type_var, p),
                          LogicFalse())

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

    @langkit_property(memoized=True)
    def monomorphize(formals=T.TypeParameter.entity.array,
                     actuals=T.TypeDecl.entity.array):
        return SynthParametrizedGeneric.new(
            prototype_name=Self.prototype_name,
            parameters=Entity.type_parameters.map(lambda p:
                p.monomorphize(formals, actuals)
            )
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
                  G.type_ref,
                  Token.Eq,
                  G.expr),

    fun_kind=Or(FunKind.alt_function(Token.Fun),
                FunKind.alt_property(Token.Property),
                FunKind.alt_field(Token.Field)),

    fun_spec=FunSpec(G.fun_kind,
                     G.identifier,
                     Token.LPar,
                     List(G.param, empty_valid=True, sep=Token.Coma),
                     Token.RPar,
                     Token.RArrow,
                     G.type_ref),

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

    type_ref=Or(ParametrizedGeneric(G.type_name,
                                     Token.Lt,
                                     List(G.type_ref, sep=Token.Coma),
                                     Token.Gt),
                 G.type_name),

    prototype_kind=Or(PrototypeKind.alt_prototype(Token.Prototype),
                      PrototypeKind.alt_astnode(Token.AstNode)),

    prototype=Prototype(G.prototype_kind,
                        G.kind_name,
                        Opt(Token.Lt,
                            List(G.type_parameter, sep=Token.Coma,
                                 empty_valid=False),
                            Token.Gt),
                        Opt(Token.Colon, G.type_ref),
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
                          G.type_ref,
                          Token.Eq,
                          G.expr),
             ParameterDecl(G.identifier, Token.Colon, G.type_ref))
)
