import refactor
import liblkqllang as lkql
from refactor import Action, ActionKind

class App(refactor.Refactor):

    def preprocess(self, unit):
        for or_pat in unit.root.findall(lkql.OrPattern):
            # Replace 'or' by '|'
            or_tok = refactor.first_with_pred(or_pat.f_left.token_end, lambda t: t.kind == 'Or')
            # Put parens around the OrPattern if needed
            if not or_pat.parent.is_a(lkql.OrPattern, lkql.ParenPattern,
                                      lkql.NodePatternSelector,
                                      lkql.NodePatternField,
                                      lkql.NodePatternProperty):
                self.add_action(or_pat.token_start, Action(ActionKind.prepend, "("))
                self.add_action(or_pat.token_end, Action(ActionKind.append, ")"))
            self.add_action(or_tok, Action(ActionKind.replace, "|"))


if __name__ == '__main__':
    App.run()
