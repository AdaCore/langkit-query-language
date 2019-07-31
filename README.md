# langkit-query-language
query language for Libadalang and Langkit

## Using the Langkit plugin
The Langkit plugin pass and all the necessary mako templates
are located in the `lkql/lkql_pass` directory.

Generating the AST_Node implementation & the demo project:
1. Copy the `lkql/lkql_pass` directory to the same directory as the `manage.py` file of the target language.
2. Execute `python manage.py --library-types static,relocatable --plugin-pass=lkql_pass.ql_pass.get_pass make`
3. The generated sources will be stored in the `lkql_generated` directory.

