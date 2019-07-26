from os import listdir, makedirs
from os.path import join, isdir, dirname
from langkit.passes import GlobalPass
from mako.template import Template

"""
This langkit plugin pass will recursively walk folders listed in
'PROJECTS' and run mako on every visited file.
Processed files will be stored in the the given output directory.

Variables available in the mako templates are:
  - lib_name: name of the Langkit library beeing generated
  - lang_name: name of the language
  - root_type: name of the root AST node class
  - node_array_types: array types that can be stored in Value_Type values
"""


PROJECTS = [('lkql_repl_template', 'lkql_repl')]
# List of template projects to be processed, of the form:
# (PROJECT_TEMPLATE_PATH, OUTPUT_PATH)


def run(ctx):
    """
    Execute the plugin pass
    """
    array_types = get_array_types(ctx)
    node_array_types = [t for t in array_types if t.element_type.is_entity_type]
    node_prototypes = generate_node_prototypes(ctx)

    values = {
        'lib_name': ctx.lib_name,
        'lang_name': ctx.lang_name,
        'root_type': ctx.root_grammar_class.raw_name.base_name,
        'node_array_types': node_array_types,
        'node_lkql_prototypes': node_prototypes,
        'use_unbounded_text_array':
            any(x.api_name == "Unbounded_Text_Array" for x in array_types)
    }

    generate_projects(values)


def generate_node_prototypes(ctx):
    """
    Return a string containing the LKQL prototype of all node types.
    """
    types_map = {"Character.array": "string", "Int": "int", "Bool": "bool"}

    def lkql_type_name(t):
        if t.is_entity_type:
            return lkql_type_name(t.element_type)
        elif t.is_array_type:
            return 'List<{}>'.format(lkql_type_name(t.element_type))
        else:
            return types_map[t.dsl_name] if t.dsl_name in types_map else t.dsl_name

    return Template(nodes_prototypes_template)\
        .render(ctx=ctx, lkql_type_name=lkql_type_name, types_map=types_map)\
        .strip()


def get_array_types(ctx):
    """
    Return the list of array types that can be returned by the introspection
    API.
    :param ctx:
    :return:
    """
    fields_types = set()

    for node in ctx.astnode_types:
        current_types = (f.public_type for f in node._fields.values()
                         if f.is_public and f.public_type.is_array and
                         f.public_type.api_name != 'Text_Type')
        fields_types = fields_types.union(set(current_types))

    return fields_types


def generate_projects(template_args):
    """
    Given a dictionnary of values, execute mako on every project listed in
    'PROJECTS' with this values as arguments.
    """
    for (project_dir, output_dir) in PROJECTS:
        create_dir(output_dir)
        process_templates(project_dir, output_dir, template_args)


def process_templates(prj_path, output, template_args):
    """
    For every file in 'prj_path', run mako on the file with the given arguments
    and store the result in 'output'.
    """
    for src in listdir(prj_path):
        current_path = join(prj_path, src)
        if isdir(current_path):
            process_templates(current_path, join(output, src), template_args)
        else:
            template = Template(filename=current_path, strict_undefined=True)
            output_path = join(output, src)
            create_dir(dirname(output_path))
            with open(output_path, 'w+') as f:
                f.write(template.render(**template_args))
    create_dir(output)


def create_dir(dir):
    """
    Create the directory 'dir' (including it's intermediate directories,
    if any). No exception will be raised if 'dir' already exists.
    """
    try:
        makedirs(dir)
    except OSError:
        pass


def get_pass():
    return GlobalPass("generating the interpreter", run)


def adaize_string(text):
    """
    Return an Ada String literal representing the input text.
    """
    return " & LF &\n".join(['"{}"'.format(line) for line in text.split('\n')])


nodes_prototypes_template = r"""
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
"""
"""