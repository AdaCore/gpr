from langkit.lexer import (
    Lexer, LexerToken, NoCase, Literal, Pattern, Eof, NoText, Ignore, WithText,
    WithSymbol, Case, Alt, WithSymbol, Failure
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

    # Fail token
    LexFail = NoText()

gpr_lexer = Lexer(Token)

gpr_lexer.add_patterns(
    ('p_string', r"\"(\"\"|[^\n\"])*\""),
    ('digit', r"[0-9]"),
    ('integer', r"({digit}(_?{digit})*)"),
)

gpr_lexer.add_rules(
    (Eof(),                                     Token.Termination),
    (Pattern(r"[ \t\r\n]+"),                    Ignore()),
    (Pattern(r"--(.?)+"),                       Ignore()),
    (NoCase("all"),                             Token.All),
    (NoCase("abstract"),                        Token.Abstract),
    (NoCase("at"),                              Token.At),
    (NoCase("case"),                            Token.Case),
    (NoCase("end"),                             Token.End),
    (NoCase("for"),                             Token.For),
    (NoCase("is"),                              Token.Is),
    (NoCase("limited"),                         Token.Limited),
    (NoCase("null"),                            Token.Null),
    (NoCase("others"),                          Token.Others),
    (NoCase("package"),                         Token.Package),
    (NoCase("renames"),                         Token.Renames),
    (NoCase("type"),                            Token.Type),
    (NoCase("use"),                             Token.Use),
    (NoCase("when"),                            Token.When),
    (NoCase("with"),                            Token.With),

    (NoCase("project"),                         Token.Project),
    (NoCase("extends"),                         Token.Extends),
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

    (Failure(),                                 Token.LexFail),
)
