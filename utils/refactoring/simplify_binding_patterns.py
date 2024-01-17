import refactor
import liblkqllang as lkql
from refactor import Action, ActionKind

class App(refactor.Refactor):

    def preprocess(self, unit):
        for binding_pat in unit.root.findall(lkql.BindingPattern):
            # Remove every token from the @ token to the last whitespace after the '*' token
            
            if binding_pat.f_value_pattern and binding_pat.f_value_pattern.is_a(lkql.UniversalPattern):
                cur_tok = refactor.first_with_pred(binding_pat.token_start, lambda t: t.kind == 'At')
                if cur_tok.previous.kind == 'Whitespace':
                    cur_tok = cur_tok.previous
                end_tok = binding_pat.token_end.next

                while cur_tok != end_tok:
                    self.add_action(cur_tok, Action(ActionKind.replace, ""))
                    cur_tok = cur_tok.next


if __name__ == '__main__':
    App.run()
