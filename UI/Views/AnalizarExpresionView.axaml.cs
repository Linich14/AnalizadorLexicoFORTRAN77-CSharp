using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using Avalonia.Interactivity;
using AnalizadorLexico.Lexico;
using AnalizadorLexico.Sintaxis;
using System;
using System.Text;
using System.Linq;

namespace AnalizadorLexico.UI.Views;

public partial class AnalizarExpresionView : UserControl
{
    // Propiedades usando FindControl para evitar conflictos con generación automática
    private TextBox? _txtExpression;
    private Button? _btnAnalyze;
    private TextBox? _txtTokens;
    private TextBox? _txtSyntaxTree;
    private TextBox? _txtErrors;

    private TextBox? TxtExpression => _txtExpression ??= this.FindControl<TextBox>("InputExpression");
    private Button? BtnAnalyze => _btnAnalyze ??= this.FindControl<Button>("BtnAnalyzeExpression");
    private TextBox? TxtTokens => _txtTokens ??= this.FindControl<TextBox>("OutputTokens");
    private TextBox? TxtSyntaxTree => _txtSyntaxTree ??= this.FindControl<TextBox>("OutputSyntaxTree");
    private TextBox? TxtErrors => _txtErrors ??= this.FindControl<TextBox>("OutputErrors");

    public AnalizarExpresionView()
    {
        InitializeComponent();
        
        this.AttachedToVisualTree += (s, e) =>
        {
            InicializarControles();
        };
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }

    private void InicializarControles()
    {
        if (BtnAnalyze != null)
        {
            BtnAnalyze.Click += CuandoClickAnalizar;
        }
    }

    private void CuandoClickAnalizar(object? sender, RoutedEventArgs e)
    {
        try
        {
            // Limpiar resultados anteriores
            if (TxtTokens != null) TxtTokens.Text = string.Empty;
            if (TxtSyntaxTree != null) TxtSyntaxTree.Text = string.Empty;
            if (TxtErrors != null) TxtErrors.Text = string.Empty;

            // Obtener el texto de entrada
            var codigoFuente = TxtExpression?.Text ?? string.Empty;

            if (string.IsNullOrWhiteSpace(codigoFuente))
            {
                MostrarError("Por favor ingresa una expresión o código FORTRAN77 para analizar.");
                return;
            }

            // FASE 1: Análisis Léxico
            var lexer = new Lexer(codigoFuente);
            var tokens = lexer.Tokenizar().ToList();

            // Mostrar tokens
            MostrarTokens(tokens);

            // FASE 2: Análisis Sintáctico
            var parser = new Parser(tokens);
            var arbolSintactico = parser.ParsePrograma();

            // Mostrar árbol sintáctico
            MostrarArbolSintactico(arbolSintactico);

            // Mensaje de éxito
            if (TxtErrors != null)
            {
                TxtErrors.Text = "✓ Análisis completado exitosamente.\n\n" +
                                "El código es sintácticamente correcto según la gramática FORTRAN77 implementada.";
            }
        }
        catch (Exception ex)
        {
            MostrarError($"Error durante el análisis:\n\n{ex.Message}");
        }
    }

    private void MostrarTokens(System.Collections.Generic.List<Token> tokens)
    {
        if (TxtTokens == null) return;

        var resultado = new StringBuilder();
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine("  TOKENS GENERADOS POR EL ANALIZADOR LÉXICO");
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine();
        resultado.AppendLine($"Total de tokens: {tokens.Count}");
        resultado.AppendLine();
        resultado.AppendLine("┌─────┬──────────────────────┬────────────────┬──────────┐");
        resultado.AppendLine("│ #   │ Tipo                 │ Lexema         │ Posición │");
        resultado.AppendLine("├─────┼──────────────────────┼────────────────┼──────────┤");

        int contador = 1;
        foreach (var token in tokens)
        {
            if (token.Tipo == TipoToken.EOF) continue; // No mostrar EOF
            if (token.Tipo == TipoToken.EOL) continue; // No mostrar EOL

            var tipo = token.Tipo.ToString().PadRight(20);
            var lexema = token.Lexema.Length > 14 ? token.Lexema.Substring(0, 11) + "..." : token.Lexema.PadRight(14);
            var posicion = $"{token.Linea}:{token.Columna}".PadRight(8);

            resultado.AppendLine($"│ {contador.ToString().PadLeft(3)} │ {tipo} │ {lexema} │ {posicion} │");
            contador++;
        }

        resultado.AppendLine("└─────┴──────────────────────┴────────────────┴──────────┘");
        resultado.AppendLine();
        resultado.AppendLine("Leyenda de Tipos:");
        resultado.AppendLine("  • Identificador: Variables y nombres");
        resultado.AppendLine("  • NumeroEntero / NumeroReal: Constantes numéricas");
        resultado.AppendLine("  • OperadorAritmetico: +, -, *, /");
        resultado.AppendLine("  • OperadorRelacional: .GT., .LT., .EQ., .NE., .GE., .LE.");
        resultado.AppendLine("  • PalabraReservada: IF, THEN, DO, INTEGER, REAL, etc.");

        TxtTokens.Text = resultado.ToString();
    }

    private void MostrarArbolSintactico(Programa programa)
    {
        if (TxtSyntaxTree == null) return;

        var resultado = new StringBuilder();
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine("  ÁRBOL SINTÁCTICO ABSTRACTO (AST)");
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine();
        resultado.AppendLine("Programa");
        resultado.AppendLine("│");

        for (int i = 0; i < programa.Sentencias.Count; i++)
        {
            var esUltimo = i == programa.Sentencias.Count - 1;
            var prefijo = esUltimo ? "└── " : "├── ";
            var prefijoHijo = esUltimo ? "    " : "│   ";

            ImprimirNodo(resultado, programa.Sentencias[i], prefijo, prefijoHijo);
        }

        TxtSyntaxTree.Text = resultado.ToString();
    }

    private void ImprimirNodo(StringBuilder sb, NodoSintactico nodo, string prefijo, string prefijoHijo)
    {
        switch (nodo)
        {
            case Asignacion asig:
                sb.AppendLine($"{prefijo}Asignación: {asig.Identificador}");
                ImprimirNodo(sb, asig.Valor, prefijoHijo + "└── ", prefijoHijo + "    ");
                break;

            case Binaria bin:
                sb.AppendLine($"{prefijo}Operación: {bin.Operador}");
                ImprimirNodo(sb, bin.Izquierda, prefijoHijo + "├── ", prefijoHijo + "│   ");
                ImprimirNodo(sb, bin.Derecha, prefijoHijo + "└── ", prefijoHijo + "    ");
                break;

            case Numero num:
                sb.AppendLine($"{prefijo}Número: {num.Lexema}");
                break;

            case Variable var:
                sb.AppendLine($"{prefijo}Variable: {var.Nombre}");
                break;

            case If ifNode:
                sb.AppendLine($"{prefijo}IF");
                sb.AppendLine($"{prefijoHijo}├── Condición:");
                ImprimirNodo(sb, ifNode.Condicion, prefijoHijo + "│   └── ", prefijoHijo + "│       ");
                sb.AppendLine($"{prefijoHijo}├── THEN ({ifNode.SentenciasThen.Count} sentencias)");
                for (int i = 0; i < ifNode.SentenciasThen.Count; i++)
                {
                    var esUltimo = i == ifNode.SentenciasThen.Count - 1;
                    var pre = esUltimo ? "│   └── " : "│   ├── ";
                    var preHijo = esUltimo ? "│       " : "│   │   ";
                    ImprimirNodo(sb, ifNode.SentenciasThen[i], prefijoHijo + pre, prefijoHijo + preHijo);
                }
                if (ifNode.SentenciasElse.Count > 0)
                {
                    sb.AppendLine($"{prefijoHijo}└── ELSE ({ifNode.SentenciasElse.Count} sentencias)");
                    for (int i = 0; i < ifNode.SentenciasElse.Count; i++)
                    {
                        var esUltimo = i == ifNode.SentenciasElse.Count - 1;
                        var pre = esUltimo ? "    └── " : "    ├── ";
                        var preHijo = esUltimo ? "        " : "    │   ";
                        ImprimirNodo(sb, ifNode.SentenciasElse[i], prefijoHijo + pre, prefijoHijo + preHijo);
                    }
                }
                break;

            case DoLoop doLoop:
                sb.AppendLine($"{prefijo}DO Loop (etiqueta: {doLoop.Label})");
                sb.AppendLine($"{prefijoHijo}├── Variable: {doLoop.Identificador}");
                sb.AppendLine($"{prefijoHijo}├── Inicio:");
                ImprimirNodo(sb, doLoop.Inicio, prefijoHijo + "│   └── ", prefijoHijo + "│       ");
                sb.AppendLine($"{prefijoHijo}├── Fin:");
                ImprimirNodo(sb, doLoop.Fin, prefijoHijo + "│   └── ", prefijoHijo + "│       ");
                if (doLoop.Incremento != null)
                {
                    sb.AppendLine($"{prefijoHijo}├── Incremento:");
                    ImprimirNodo(sb, doLoop.Incremento, prefijoHijo + "│   └── ", prefijoHijo + "│       ");
                }
                sb.AppendLine($"{prefijoHijo}└── Cuerpo ({doLoop.Sentencias.Count} sentencias)");
                for (int i = 0; i < doLoop.Sentencias.Count; i++)
                {
                    var esUltimo = i == doLoop.Sentencias.Count - 1;
                    var pre = esUltimo ? "    └── " : "    ├── ";
                    var preHijo = esUltimo ? "        " : "    │   ";
                    ImprimirNodo(sb, doLoop.Sentencias[i], prefijoHijo + pre, prefijoHijo + preHijo);
                }
                break;

            case Declaracion decl:
                sb.AppendLine($"{prefijo}Declaración: {decl.TipoDato} {string.Join(", ", decl.Identificadores)}");
                break;

            case DirectivaProgram prog:
                sb.AppendLine($"{prefijo}PROGRAM: {prog.NombrePrograma}");
                break;

            case SentenciaPrint print:
                sb.AppendLine($"{prefijo}PRINT: {string.Join(" ", print.Argumentos)}");
                break;

            case DirectivaSimple dir:
                sb.AppendLine($"{prefijo}{dir.Tipo}");
                break;

            default:
                sb.AppendLine($"{prefijo}{nodo.Tipo}");
                break;
        }
    }

    private void MostrarError(string mensaje)
    {
        if (TxtErrors != null)
        {
            TxtErrors.Text = "✗ ERROR\n\n" + mensaje;
        }

        // Cambiar a la pestaña de errores
        var tabControl = this.FindControl<TabControl>("ResultsTabControl");
        if (tabControl != null)
        {
            tabControl.SelectedIndex = 2; // Pestaña de errores
        }
    }
}