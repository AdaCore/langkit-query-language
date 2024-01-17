import refactor
import liblkqllang as lkql
from refactor import Action, ActionKind

class App(refactor.Refactor):

    def preprocess(self, unit):
        for det in unit.root.findall(lkql.NodePatternDetail):
            tok_is = refactor.first_with_pred(det.token_start, lambda t: t.kind == 'Is')
            self.add_action(tok_is, Action(ActionKind.replace, ":"))
            self.add_action(tok_is.previous, Action(ActionKind.replace, ""))


if __name__ == '__main__':
    App.run()
