from os import listdir, makedirs
from os.path import join, isdir, dirname, relpath
from langkit.passes import GlobalPass
from mako.template import Template

TEMPLATES_DIR = 'src_templates'
OUTPUT_DIR = 'generated_sources'


def run(ctx):
    display_introspection_kinds(ctx)
    values = {
        'lib_name': ctx.lib_name,
        'root_name': ctx.root_grammar_class.raw_name.base_name
    }
    create_dir(OUTPUT_DIR)
    process_templated(TEMPLATES_DIR, values)


def display_introspection_kinds(ctx):
    fields_types = set()
    for node in ctx.astnode_types:
        fields = [f for f in node._fields.values()
                  if f.is_property and f.is_public]
        for field in fields:
            fields_types.add(field.public_type.introspection_kind)
    print(fields_types)


def create_dir(dir):
    try:
        makedirs(dir)
    except OSError:
        pass


def process_templated(dir, template_args):
    for src in listdir(dir):
        current_path = join(dir, src)
        if isdir(current_path):
            process_templated(current_path, template_args)
        else:
            template = Template(filename=current_path)
            output_path = join(OUTPUT_DIR, relpath(dir, TEMPLATES_DIR), src)
            create_dir(dirname(output_path))
            with open(output_path, 'w+') as f:
                f.write(template.render(**template_args))


def get_pass():
    return GlobalPass("generating the interpreter", run)
