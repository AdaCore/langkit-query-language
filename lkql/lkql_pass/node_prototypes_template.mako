% for n in ctx.astnode_types:
${render_prototype(n)}
% endfor

<%def name="render_prototype(node)">\
astnode ${node.dsl_name}: ${base_type(node)} {
%if node.base is None:
    field text() -> ${types_map['Character.array']}
    field image() -> ${types_map['Character.array']}
%endif
${render_fields(node)}
${render_properties(node)}\
}
</%def>

<%def name="base_type(node)">${"ASTNode" if node.base is None else node.base.dsl_name}</%def>

<%def name="render_fields(node)">\
<% fields = (f for f in public_nonoverriding_fields(node)\
             if not f.is_property) %>\
% for f in fields:
    field ${f._name.lower}() -> ${lkql_type_name(f.type)}
% endfor
</%def>

<%def name="render_properties(node)">\
<% fields = (f for f in public_nonoverriding_fields(node) if f.is_property) %>\
% for f in fields:
<% args = ", ".join(format_arg(a) for a in f.arguments)%>\
    property ${f._name.lower}(${args}) -> ${lkql_type_name(f.type)}
% endfor
</%def>

<%def name="public_nonoverriding_fields(node)">\
<% return (f for f in node._fields.values() if f.is_public and not f.is_overriding) %>
</%def>

<%def name="format_arg(arg)">\
<% return "{}: {}".format(arg.dsl_name, lkql_type_name(arg.type)) %>
</%def>