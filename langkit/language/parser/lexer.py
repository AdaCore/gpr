from langkit.lexer import (
    Lexer,
    LexerToken,
    Literal,
    NoCaseLit,
    Pattern,
    WithSymbol,
    WithText,
    WithTrivia,
    TokenFamily,
)


class Token(LexerToken):
    Identifier = WithSymbol()
    All = WithSymbol()

    # Ada Keywords
    Abstract = WithSymbol()
    At = WithSymbol()
    Case = WithSymbol()
    End = WithSymbol()
    For = WithSymbol()
    Is = WithSymbol()
    Limited = WithSymbol()
    Private = WithSymbol()
    Null = WithSymbol()
    Others = WithSymbol()
    Package = WithSymbol()
    Renames = WithSymbol()
    Type = WithSymbol()
    Use = WithSymbol()
    Pragma = WithSymbol()
    When = WithSymbol()
    With = WithSymbol()

    # Gpr Keywords
    Extends = WithSymbol()

    # Punctuation
    ParOpen = WithSymbol()
    ParClose = WithSymbol()
    Semicolon = WithSymbol()
    Colon = WithSymbol()
    Comma = WithSymbol()
    Dot = WithSymbol()

    Amp = WithSymbol()
    Tick = WithSymbol()
    Pipe = WithSymbol()
    Assign = WithSymbol()
    Arrow = WithSymbol()

    # Literals
    String = WithText()
    Number = WithText()

    # Hidden framework dependencies???
    Label = WithText()
    Char = WithSymbol()

    Comment = WithTrivia()
    Whitespace = WithTrivia()

    Alphanumericals = TokenFamily(
        Identifier,
        All,
        Abstract,
        At,
        Case,
        End,
        For,
        Is,
        Limited,
        Null,
        Others,
        Package,
        Renames,
        Type,
        Use,
        Pragma,
        When,
        With,
        Extends,
        String,
        Number,
        Label,
        Char,
    )


gpr_lexer = Lexer(Token)

gpr_lexer.add_patterns(
    ("p_string", r"\"(\"\"|[^\n\"])*\""),
    ("digit", r"[0-9]"),
    ("integer", r"({digit}(_?{digit})*)"),
)

gpr_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Token.Whitespace),
    (Pattern(r"--(.?)+"), Token.Comment),
    (NoCaseLit("all"), Token.All),
    (NoCaseLit("abstract"), Token.Abstract),
    (NoCaseLit("at"), Token.At),
    (NoCaseLit("case"), Token.Case),
    (NoCaseLit("end"), Token.End),
    (NoCaseLit("for"), Token.For),
    (NoCaseLit("is"), Token.Is),
    (NoCaseLit("limited"), Token.Limited),
    (NoCaseLit("private"), Token.Private),
    (NoCaseLit("null"), Token.Null),
    (NoCaseLit("others"), Token.Others),
    (NoCaseLit("package"), Token.Package),
    (NoCaseLit("renames"), Token.Renames),
    (NoCaseLit("type"), Token.Type),
    (NoCaseLit("use"), Token.Use),
    (NoCaseLit("pragma"), Token.Pragma),
    (NoCaseLit("when"), Token.When),
    (NoCaseLit("with"), Token.With),
    (NoCaseLit("extends"), Token.Extends),
    (Literal("("), Token.ParOpen),
    (Literal(")"), Token.ParClose),
    (Literal(";"), Token.Semicolon),
    (Literal(":"), Token.Colon),
    (Literal(","), Token.Comma),
    (Literal("."), Token.Dot),
    (Literal("&"), Token.Amp),
    (Literal("'"), Token.Tick),
    (Literal("|"), Token.Pipe),
    (Literal(":="), Token.Assign),
    (Literal("=>"), Token.Arrow),
    (Pattern("{integer}"), Token.Number),
    (Pattern(r"[_a-zA-Z][_a-zA-Z0-9]*"), Token.Identifier),
    (Pattern("{p_string}"), Token.String),
)

gpr_lexer.add_spacing((Token.Alphanumericals, Token.Alphanumericals))
gpr_lexer.add_newline_after(Token.Comment)
