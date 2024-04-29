from pygments.lexer import RegexLexer, words
from pygments import token

class LKQLPygmentsLexer(RegexLexer):
    """
    Pygments lexer for LKQL
    """
    name = 'LKQL'
    filenames = ['*.lkql']

    tokens = {
        'root': [
            (words(('select', 'let', 'when', 'val', 'fun', 'selector',
                    'match', 'rec', 'skip', 'is', 'in', 'true', 'false',
                    'if', 'else', 'then', 'not', 'null', 'from'),
                   prefix=r'\b', suffix=r'\b'),
             token.Keyword),
            (r"\|\".+", token.String),
            (r"#(.?)+", token.Comment),
            (r"(\-\>|=|\=\>|\<\=|\>\=|\=|\!\=|\+|\-|\*|\/|\&|"
             r"\@|\||\>|\<|:)", token.Operator),
            (r"\b(and|or|not)\b", token.Operator),
            (r"\{|\}|\(|\)|\[|\]|;|\.|,", token.Punctuation),
            (r'"(\\.|[^"])*"', token.String),
            (r'[0-9]+', token.Number),
            (r'_?[a-zA-Z][\w\']*', token.Name),
            (r'_', token.Name),
            (r'\n', token.Text),
            (r'[^\S\n]+', token.Text),
        ]
    }

