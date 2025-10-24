using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using Avalonia.Interactivity;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System;
using System.Text;
using AnalizadorLexico.Lexico;
using AnalizadorLexico.Sintaxis;

namespace AnalizadorLexico.UI.Views;

public class InformacionArchivo
{
    public string Nombre { get; set; } = string.Empty;
    public long Tamanio { get; set; }
    public string RutaCompleta { get; set; } = string.Empty;
}

public partial class CargarArchivoView : UserControl
{
    public ObservableCollection<InformacionArchivo> ArchivosDisponibles { get; } = new ObservableCollection<InformacionArchivo>();
    
    // Propiedades usando FindControl para evitar conflictos con generación automática
    private ComboBox? _cmbFiles;
    private Button? _btnRefreshFiles;
    private Button? _btnAnalyzeFile;
    private TextBlock? _txtFileInfo;
    private TextBox? _outputSourceCode;
    private TextBox? _outputTokensResult;
    private TextBox? _outputSyntaxTreeResult;
    private TextBox? _outputMessages;
    
    private ComboBox? CmbFiles => _cmbFiles ??= this.FindControl<ComboBox>("FilesCombo");
    private Button? BtnRefreshFiles => _btnRefreshFiles ??= this.FindControl<Button>("RefreshBtn");
    private Button? BtnAnalyzeFile => _btnAnalyzeFile ??= this.FindControl<Button>("AnalyzeBtn");
    private TextBlock? TxtFileInfo => _txtFileInfo ??= this.FindControl<TextBlock>("FileInfoLabel");
    private TextBox? OutputSourceCode => _outputSourceCode ??= this.FindControl<TextBox>("SourceCodeArea");
    private TextBox? OutputTokensResult => _outputTokensResult ??= this.FindControl<TextBox>("TokensArea");
    private TextBox? OutputSyntaxTreeResult => _outputSyntaxTreeResult ??= this.FindControl<TextBox>("TreeArea");
    private TextBox? OutputMessages => _outputMessages ??= this.FindControl<TextBox>("MessagesArea");

    public CargarArchivoView()
    {
        InitializeComponent();
        
        this.AttachedToVisualTree += (s, e) =>
        {
            InicializarControles();
            CargarArchivosDisponibles();
        };
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
    
    private void InicializarControles()
    {
        if (CmbFiles != null)
        {
            CmbFiles.ItemsSource = ArchivosDisponibles;
            CmbFiles.SelectionChanged += CuandoCambiaSeleccionArchivo;
        }
        
        if (BtnRefreshFiles != null)
            BtnRefreshFiles.Click += CuandoClickRefrescar;
            
        if (BtnAnalyzeFile != null)
            BtnAnalyzeFile.Click += CuandoClickAnalizar;
    }

    private void CargarArchivosDisponibles()
    {
        ArchivosDisponibles.Clear();

        string rutaInput = Path.Combine(Directory.GetCurrentDirectory(), "input");

        if (Directory.Exists(rutaInput))
        {
            var archivos = Directory.GetFiles(rutaInput, "*.*")
                .Where(archivo => archivo.EndsWith(".txt", StringComparison.OrdinalIgnoreCase) ||
                                 archivo.EndsWith(".f77", StringComparison.OrdinalIgnoreCase) ||
                                 archivo.EndsWith(".for", StringComparison.OrdinalIgnoreCase))
                .Select(archivo => new InformacionArchivo
                {
                    Nombre = Path.GetFileName(archivo),
                    Tamanio = new FileInfo(archivo).Length,
                    RutaCompleta = archivo
                })
                .OrderBy(archivo => archivo.Nombre);

            foreach (var archivo in archivos)
            {
                ArchivosDisponibles.Add(archivo);
            }
        }
    }

    private void CuandoCambiaSeleccionArchivo(object? sender, SelectionChangedEventArgs e)
    {
        var archivoSeleccionado = CmbFiles?.SelectedItem as InformacionArchivo;
        
        if (archivoSeleccionado != null)
        {
            if (TxtFileInfo != null)
                TxtFileInfo.Text = $"{archivoSeleccionado.Nombre} ({archivoSeleccionado.Tamanio} bytes) - {archivoSeleccionado.RutaCompleta}";
            
            if (BtnAnalyzeFile != null)
                BtnAnalyzeFile.IsEnabled = true;
        }
        else
        {
            if (TxtFileInfo != null)
                TxtFileInfo.Text = "Selecciona un archivo de la carpeta 'input/'";
            
            if (BtnAnalyzeFile != null)
                BtnAnalyzeFile.IsEnabled = false;
        }
        
        // Limpiar resultados anteriores
        LimpiarResultados();
    }

    private void CuandoClickRefrescar(object? sender, RoutedEventArgs e)
    {
        CargarArchivosDisponibles();
        if (CmbFiles != null)
            CmbFiles.SelectedIndex = -1;
        LimpiarResultados();
    }

    private void CuandoClickAnalizar(object? sender, RoutedEventArgs e)
    {
        var archivoSeleccionado = CmbFiles?.SelectedItem as InformacionArchivo;
        if (archivoSeleccionado == null) return;

        try
        {
            // Leer el contenido del archivo
            string codigoFuente = File.ReadAllText(archivoSeleccionado.RutaCompleta);

            // Mostrar código fuente
            if (OutputSourceCode != null)
            {
                var sb = new StringBuilder();
                sb.AppendLine("══════════════════════════════════════════════════════════");
                sb.AppendLine($"  ARCHIVO: {archivoSeleccionado.Nombre}");
                sb.AppendLine("══════════════════════════════════════════════════════════");
                sb.AppendLine();
                sb.AppendLine(codigoFuente);
                OutputSourceCode.Text = sb.ToString();
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

            // Actualizar MainWindow con el archivo cargado
            var ventanaPrincipal = this.VisualRoot as MainWindow;
            if (ventanaPrincipal != null)
            {
                ventanaPrincipal.CargarArchivo(archivoSeleccionado.RutaCompleta);
            }

            // Mensaje de éxito
            if (OutputMessages != null)
            {
                var msg = new StringBuilder();
                msg.AppendLine("✓ ANÁLISIS COMPLETADO EXITOSAMENTE");
                msg.AppendLine();
                msg.AppendLine($"Archivo: {archivoSeleccionado.Nombre}");
                msg.AppendLine($"Tamaño: {archivoSeleccionado.Tamanio} bytes");
                msg.AppendLine($"Tokens generados: {tokens.Count(t => t.Tipo != TipoToken.EOF && t.Tipo != TipoToken.EOL)}");
                msg.AppendLine($"Sentencias: {arbolSintactico.Sentencias.Count}");
                msg.AppendLine();
                msg.AppendLine("El código es sintácticamente correcto según la gramática FORTRAN77 implementada.");
                OutputMessages.Text = msg.ToString();
            }

            // Cambiar a la pestaña de tokens automáticamente
            var tabControl = this.FindControl<TabControl>("TabResults");
            if (tabControl != null)
                tabControl.SelectedIndex = 1; // Pestaña de tokens
        }
        catch (Exception ex)
        {
            MostrarError($"Error durante el análisis:\n\n{ex.Message}");
        }
    }

    private void MostrarTokens(System.Collections.Generic.List<Token> tokens)
    {
        if (OutputTokensResult == null) return;

        var resultado = new StringBuilder();
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine("  TOKENS GENERADOS POR EL ANALIZADOR LÉXICO");
        resultado.AppendLine("════════════════════════════════════════════════════════");
        resultado.AppendLine();
        resultado.AppendLine($"Total de tokens: {tokens.Count(t => t.Tipo != TipoToken.EOF && t.Tipo != TipoToken.EOL)}");
        resultado.AppendLine();
        resultado.AppendLine("┌─────┬──────────────────────┬────────────────┬──────────┐");
        resultado.AppendLine("│ #   │ Tipo                 │ Lexema         │ Posición │");
        resultado.AppendLine("├─────┼──────────────────────┼────────────────┼──────────┤");

        int contador = 1;
        foreach (var token in tokens)
        {
            if (token.Tipo == TipoToken.EOF) continue;
            if (token.Tipo == TipoToken.EOL) continue;

            var tipo = token.Tipo.ToString().PadRight(20);
            var lexema = token.Lexema.Length > 14 ? token.Lexema.Substring(0, 11) + "..." : token.Lexema.PadRight(14);
            var posicion = $"{token.Linea}:{token.Columna}".PadRight(8);

            resultado.AppendLine($"│ {contador.ToString().PadLeft(3)} │ {tipo} │ {lexema} │ {posicion} │");
            contador++;
        }

        resultado.AppendLine("└─────┴──────────────────────┴────────────────┴──────────┘");

        OutputTokensResult.Text = resultado.ToString();
    }

    private void MostrarArbolSintactico(Programa programa)
    {
        if (OutputSyntaxTreeResult == null) return;

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

        OutputSyntaxTreeResult.Text = resultado.ToString();
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
        if (OutputMessages != null)
        {
            OutputMessages.Text = "✗ ERROR\n\n" + mensaje;
        }

        // Cambiar a la pestaña de mensajes
        var tabControl = this.FindControl<TabControl>("TabResults");
        if (tabControl != null)
        {
            tabControl.SelectedIndex = 3; // Pestaña de mensajes/errores
        }
    }

    private void LimpiarResultados()
    {
        if (OutputSourceCode != null) OutputSourceCode.Text = string.Empty;
        if (OutputTokensResult != null) OutputTokensResult.Text = string.Empty;
        if (OutputSyntaxTreeResult != null) OutputSyntaxTreeResult.Text = string.Empty;
        if (OutputMessages != null) OutputMessages.Text = "Selecciona un archivo y presiona 'Analizar Archivo' para ver los resultados.";
    }
}