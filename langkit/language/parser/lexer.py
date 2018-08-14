from langkit.lexer import (
    Eof, Lexer, LexerToken, Literal, NoCaseLit, Pattern, WithSymbol, WithText,
    WithTrivia, TokenFamily
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
    Null = WithSymbol()
    Others = WithSymbol()
    Package = WithSymbol()
    Renames = WithSymbol()
    Type = WithSymbol()
    Use = WithSymbol()
    When = WithSymbol()
    With = WithSymbol()

    # GPR Keywords
    Project = WithSymbol()
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
        When,
        With,
        Project,
        Extends,
        String,
        Number,
        Label,
        Char
    )


gpr_lexer = Lexer(Token)

gpr_lexer.add_patterns(
    ('p_string', r"\"(\"\"|[^\n\"])*\""),
    ('digit', r"[0-9]"),
    ('integer', r"({digit}(_?{digit})*)"),
)

gpr_lexer.add_rules(
    (Eof(),                                     Token.Termination),
    (Pattern(r"[ \t\r\n]+"),                    Token.Whitespace),
    (Pattern(r"--(.?)+"),                       Token.Comment),
    (NoCaseLit("all"),                             Token.All),
    (NoCaseLit("abstract"),                        Token.Abstract),
    (NoCaseLit("at"),                              Token.At),
    (NoCaseLit("case"),                            Token.Case),
    (NoCaseLit("end"),                             Token.End),
    (NoCaseLit("for"),                             Token.For),
    (NoCaseLit("is"),                              Token.Is),
    (NoCaseLit("limited"),                         Token.Limited),
    (NoCaseLit("null"),                            Token.Null),
    (NoCaseLit("others"),                          Token.Others),
    (NoCaseLit("package"),                         Token.Package),
    (NoCaseLit("renames"),                         Token.Renames),
    (NoCaseLit("type"),                            Token.Type),
    (NoCaseLit("use"),                             Token.Use),
    (NoCaseLit("when"),                            Token.When),
    (NoCaseLit("with"),                            Token.With),

    (NoCaseLit("project"),                         Token.Project),
    (NoCaseLit("extends"),                         Token.Extends),
    (Literal("("),                              Token.ParOpen),
    (Literal(")"),                              Token.ParClose),
    (Literal(";"),                              Token.Semicolon),
    (Literal(":"),                              Token.Colon),
    (Literal(","),                              Token.Comma),
    (Literal("."),                              Token.Dot),

    (Literal("&"),                              Token.Amp),
    (Literal("'"),                              Token.Tick),
    (Literal("|"),                              Token.Pipe),

    (Literal(":="),                             Token.Assign),
    (Literal("=>"),                             Token.Arrow),

    (gpr_lexer.patterns.integer,                Token.Number),

    (Pattern(r"[_a-zA-Z][_a-zA-Z0-9]*"),        Token.Identifier),

    (gpr_lexer.patterns.p_string,               Token.String),
)

gpr_lexer.add_spacing((Token.Alphanumericals, Token.Alphanumericals))
gpr_lexer.add_newline_after(Token.Comment)
