from os import listdir, makedirs, getcwd
from os.path import join, isdir, dirname, abspath
from langkit.passes import GlobalPass
from mako.template import Template

LKQL_GEN_ROOT = dirname(abspath(__file__))
# Root of the `lkql_pass` directory.

OUTPUT_DIRECTORY = join(getcwd(), 'lkql_generated')
# Output directory for the generated artifacts


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
        'node_prototypes_string': adaize_string(node_prototypes),
        'use_unbounded_text_array':
            any(x.api_name == "Unbounded_Text_Array" for x in array_types)
    }

    create_dir(OUTPUT_DIRECTORY)
    generate_ast_impl(values, OUTPUT_DIRECTORY)
    generate_projects(LKQL_GEN_ROOT, OUTPUT_DIRECTORY, values)


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


def generate_node_prototypes(ctx):
    """
    Return a string containing the LKQL prototype of all node types.
    """
    template_file = join(LKQL_GEN_ROOT, 'node_prototypes_template.mako')
    types_map = {"Character.array": "string", "Int": "int", "Bool": "bool"}

    def lkql_type_name(t):
        """
        Return the name of the lkql type matching the given Langkit `Type`
        value.
        """
        if t.is_entity_type:
            return lkql_type_name(t.element_type)
        elif t.is_array_type:
            return 'List<{}>'.format(lkql_type_name(t.element_type))
        else:
            return types_map[t.dsl_name] if t.dsl_name in types_map else t.dsl_name

    return Template(filename=template_file)\
        .render(ctx=ctx, lkql_type_name=lkql_type_name, types_map=types_map)\
        .strip()


def generate_projects(root_dir, output_dir, template_args):
    """
    Given a dictionnary of values, execute mako on every project listed in
    'PROJECTS' with this values as arguments.
    """

    for entry in listdir(root_dir):
        project_dir = join(root_dir, entry)
        if isdir(project_dir):
            output_path = join(output_dir, entry)
            process_template(project_dir, output_path, template_args)
            generate_ast_impl(template_args, output_dir=join(output_path, 'src'))


def process_template(prj_path, output, template_args):
    """
    For every file in 'prj_path', run mako on the file with the given arguments
    and store the result in 'output'.
    """
    for src in listdir(prj_path):
        current_path = join(prj_path, src)
        if isdir(current_path):
            create_dir(output)
            process_template(current_path, join(output, src), template_args)
        else:
            render_template(current_path, join(output, src), template_args)
    create_dir(output)


def generate_ast_impl(values, output_dir=''):
    """
    Create a file containing an implementation of the AST_Node interface for the
    current language.
    """
    render_template('ast_spec.mako',
                    join(output_dir, "%s_ast_nodes.ads" % values['lang_name'].lower),
                    values)
    render_template('ast_impl.mako',
                    join(output_dir, "%s_ast_nodes.adb" % values['lang_name'].lower),
                    values)


def render_template(template_file, output_file, values):
    result = Template(filename=join(LKQL_GEN_ROOT, template_file))\
        .render(**values)

    create_dir(dirname(output_file))

    with open(output_file, 'w') as f:
        f.write(result)


def create_dir(dir):
    """
    Create the directory 'dir' (including it's intermediate directories,
    if any). No exception will be raised if 'dir' already exists.
    """
    try:
        makedirs(dir)
    except OSError:
        pass


def subdirs(dir_path):
    """
    Return the absolute paths of the subdirectories contained in `dir_path`.
    """
    entries = (abspath(join(dir_path, entry)) for entry in listdir(dir_path))
    return [p for p in entries if isdir(p)]


def adaize_string(text):
    """
    Return an Ada String literal representing the input text.
    """
    return " & ASCII.LF &\n      ".join(['"{}"'.format(line) for line in text.split('\n')])


def get_pass():
    return GlobalPass("generating the interpreter", run)
