from os import listdir, mkdir
from os.path import join, isdir
from shutil import rmtree
from langkit.passes import GlobalPass
from mako.template import Template

TEMPLATES_DIR = 'lkql_src_templates'
OUTPUT_DIR = 'lkql_generated_sources'


def run(ctx):
    values = {
        'lib_name': ctx.ada_api_settings.lib_name,
        'root_name': ctx.astnode_types[0].entity.api_name
    }

    # if isdir(OUTPUT_DIR):
    #     rmtree(OUTPUT_DIR)

    if not isdir(OUTPUT_DIR):
        mkdir(OUTPUT_DIR)

    for src in listdir(TEMPLATES_DIR):
        template = Template(filename=join(TEMPLATES_DIR, src))
        with open(join(OUTPUT_DIR, src), 'w+') as f:
            f.write(template.render(**values))


def get_pass():
    return GlobalPass("generating the interpreter", run)
