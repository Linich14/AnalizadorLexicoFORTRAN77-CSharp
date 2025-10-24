namespace AnalizadorLexico.Lexico;

public enum TipoToken
{
    Identificador,
    NumeroEntero,
    NumeroReal,
    StringLiteral,        // 'texto' o "texto"
    OperadorAritmetico,   // + - * /
    OperadorRelacional,   // .GT. .LT. .EQ. .NE. .GE. .LE.
    Asignacion,           // =
    ParentesisApertura,   // (
    ParentesisCierre,     // )
    PalabraReservada,     // IF, THEN, END, DO, CONTINUE, etc.
    Coma,                 // ,
    EOL,                  // fin de línea
    EOF,                  // fin de archivo
    Desconocido
}

public sealed class Token
{
    public TipoToken Tipo { get; }
    public string Lexema { get; }
    public int Linea { get; }
    public int Columna { get; }

    public Token(TipoToken tipo, string lexema, int linea, int columna)
    {
        Tipo = tipo;
        Lexema = lexema;
        Linea = linea;
        Columna = columna;
    }

    public override string ToString() => $"{Tipo}('{Lexema}') @ {Linea}:{Columna}";
}
