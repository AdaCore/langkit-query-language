import gdb.printing


class PrinterBase(object):

    @classmethod
    def matches(cls, val):
        try:
            return val.type.target().name == cls.typeName
        except RuntimeError:
            return False


class GlobalDataPrinter(PrinterBase):
    typeName = 'interpreter__eval_contexts__global_data'

    def __init__(self, val):
        self.val = val

    def children(self):
        return [("Ast_Root", self.val["ast_root"]),
                ("Last_Error", self.val["last_error"])]

    def to_string(self):
        return "Kernel"


class EnvironmentPrinter(PrinterBase):
    typeName = 'interpreter__eval_contexts__environment'

    def __init__(self, val):
        self.val = val

    def children(self):
        return [("Bindings", self.val["local_bindings"]),
                ("Parent", self.val["parent"].dereference())]

    def to_string(self):
        return "Environment"


class EvalContextPrinter(PrinterBase):
    typeName = 'interpreter__eval_contexts__eval_context'

    def __init__(self, val):
        self.val = val

    def children(self):
        return [("kernel", self.val["kernel"].dereference()),
                ("frames", self.val["frames"].dereference())]

    def to_string(self):
        return "Evaluation context"


printer_types = [EvalContextPrinter, GlobalDataPrinter, EnvironmentPrinter]


def eval_context_lookup(val):
    for printer_type in printer_types:
        if printer_type.matches(val):
            return printer_type(val)


gdb.current_objfile().pretty_printers.append(eval_context_lookup)

