from langkit.passes import GlobalPass

def run(ctx):
    print(ctx)

def get_pass():
    return GlobalPass("generating the interpreter", run)
