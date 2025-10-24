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
    private TextBox? _outputMessages;
    
    private ComboBox? CmbFiles => _cmbFiles ??= this.FindControl<ComboBox>("FilesCombo");
    private Button? BtnRefreshFiles => _btnRefreshFiles ??= this.FindControl<Button>("RefreshBtn");
    private Button? BtnAnalyzeFile => _btnAnalyzeFile ??= this.FindControl<Button>("AnalyzeBtn");
    private TextBlock? TxtFileInfo => _txtFileInfo ??= this.FindControl<TextBlock>("FileInfoLabel");
    private TextBox? OutputSourceCode => _outputSourceCode ??= this.FindControl<TextBox>("SourceCodeArea");
    private TextBox? OutputTokensResult => _outputTokensResult ??= this.FindControl<TextBox>("TokensArea");
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
        if (archivoSeleccionado == null)
            return;

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
            var stopwatch = System.Diagnostics.Stopwatch.StartNew();
            var parser = new Parser(tokens);
            var arbolSintactico = parser.ParsePrograma();
            stopwatch.Stop();

            // Actualizar MainWindow con el archivo y árbol cargado
            var ventanaPrincipal = this.VisualRoot as MainWindow;
            if (ventanaPrincipal == null)
                ventanaPrincipal = MainWindow.Instance;
            
            if (ventanaPrincipal != null)
            {
                ventanaPrincipal.CargarArchivo(archivoSeleccionado.RutaCompleta);
                ventanaPrincipal.ActualizarArbolSintactico(arbolSintactico, stopwatch.ElapsedMilliseconds);
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
                msg.AppendLine($"Tiempo de análisis: {stopwatch.ElapsedMilliseconds} ms");
                msg.AppendLine();
                msg.AppendLine("El código es sintácticamente correcto según la gramática FORTRAN77 implementada.");
                msg.AppendLine();
                msg.AppendLine("💡 Navega a 'Árbol Sintáctico' en el menú lateral para ver la visualización completa.");
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
            tabControl.SelectedIndex = 2; // Pestaña de mensajes (ahora es la tercera)
        }
    }

    private void LimpiarResultados()
    {
        if (OutputSourceCode != null) OutputSourceCode.Text = string.Empty;
        if (OutputTokensResult != null) OutputTokensResult.Text = string.Empty;
        if (OutputMessages != null) OutputMessages.Text = "Selecciona un archivo y presiona 'Analizar Archivo' para ver los resultados.";
    }
}