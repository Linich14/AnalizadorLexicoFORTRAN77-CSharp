using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Shapes;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
using AnalizadorLexico.Sintaxis;
using System;
using System.Collections.Generic;
using System.Text;

namespace AnalizadorLexico.UI.Views;

public partial class ArbolSintacticoView : UserControl
{
    private Canvas? _treeCanvas;
    private TextBox? _treeTextView;
    private TextBlock? _totalNodesText;
    private TextBlock? _maxDepthText;
    private TextBlock? _longestBranchText;
    private TextBlock? _leafNodesText;
    private TextBlock? _analysisTimeText;
    private Button? _zoomInButton;
    private Button? _zoomOutButton;
    private Button? _fitToScreenButton;
    
    private Programa? _arbolActual;
    private double _escalaActual = 1.0;
    private const double EscalaIncremento = 0.1;
    
    public ArbolSintacticoView()
    {
        InitializeComponent();
        
        // Configurar controles cuando la vista esté completamente cargada
        this.AttachedToVisualTree += (s, e) =>
        {
            ConfigurarControles();
            
            // Intentar obtener MainWindow de múltiples formas
            var mainWindow = this.VisualRoot as MainWindow;
            if (mainWindow == null)
                mainWindow = MainWindow.Instance;
            
            if (mainWindow?.ArbolSintacticoActual != null)
            {
                MostrarArbol(mainWindow.ArbolSintacticoActual, mainWindow.TiempoAnalisisMs);
            }
            else
            {
                MostrarMensajeInicial();
            }
        };
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
    
    private void ConfigurarControles()
    {
        _treeCanvas = this.FindControl<Canvas>("TreeCanvas");
        _treeTextView = this.FindControl<TextBox>("TreeTextView");
        _totalNodesText = this.FindControl<TextBlock>("TotalNodesText");
        _maxDepthText = this.FindControl<TextBlock>("MaxDepthText");
        _longestBranchText = this.FindControl<TextBlock>("LongestBranchText");
        _leafNodesText = this.FindControl<TextBlock>("LeafNodesText");
        _analysisTimeText = this.FindControl<TextBlock>("AnalysisTimeText");
        
        _zoomInButton = this.FindControl<Button>("ZoomInButton");
        _zoomOutButton = this.FindControl<Button>("ZoomOutButton");
        _fitToScreenButton = this.FindControl<Button>("FitToScreenButton");
        
        if (_zoomInButton != null)
            _zoomInButton.Click += (s, e) => AplicarZoom(1 + EscalaIncremento);
        
        if (_zoomOutButton != null)
            _zoomOutButton.Click += (s, e) => AplicarZoom(1 - EscalaIncremento);
        
        if (_fitToScreenButton != null)
            _fitToScreenButton.Click += (s, e) => AjustarAPantalla();
    }
    
    public void MostrarArbol(Programa arbol, long tiempoMs = 0)
    {
        _arbolActual = arbol;
        
        // Actualizar vista de texto
        ActualizarVistaTexto(arbol);
        
        // Actualizar vista gráfica
        ActualizarVistaGrafica(arbol);
        
        // Actualizar estadísticas
        ActualizarEstadisticas(arbol, tiempoMs);
    }
    
    private void MostrarMensajeInicial()
    {
        if (_treeTextView != null)
        {
            _treeTextView.Text = 
                "════════════════════════════════════════════════════════\n" +
                "  VISUALIZACIÓN DEL ÁRBOL SINTÁCTICO\n" +
                "════════════════════════════════════════════════════════\n\n" +
                "No hay árbol sintáctico disponible.\n\n" +
                "Para visualizar el árbol:\n" +
                "1. Ve a 'Cargar Archivo' y analiza un archivo FORTRAN77\n" +
                "   o\n" +
                "2. Ve a 'Analizar Expresión' e ingresa código FORTRAN77\n\n" +
                "Luego regresa a esta vista para ver el árbol generado.";
        }
        
        if (_totalNodesText != null) _totalNodesText.Text = "0";
        if (_maxDepthText != null) _maxDepthText.Text = "0";
        if (_longestBranchText != null) _longestBranchText.Text = "0";
        if (_leafNodesText != null) _leafNodesText.Text = "0";
        if (_analysisTimeText != null) _analysisTimeText.Text = "N/A";
    }
    
    private void ActualizarVistaTexto(Programa arbol)
    {
        if (_treeTextView == null) return;
        
        var sb = new StringBuilder();
        sb.AppendLine("════════════════════════════════════════════════════════");
        sb.AppendLine("  ÁRBOL SINTÁCTICO - REPRESENTACIÓN TEXTUAL");
        sb.AppendLine("════════════════════════════════════════════════════════");
        sb.AppendLine();
        sb.AppendLine("Programa");
        
        for (int i = 0; i < arbol.Sentencias.Count; i++)
        {
            var esUltimo = i == arbol.Sentencias.Count - 1;
            var prefijo = esUltimo ? "└── " : "├── ";
            var prefijoHijo = esUltimo ? "    " : "│   ";
            ImprimirNodo(sb, arbol.Sentencias[i], prefijo, prefijoHijo);
        }
        
        _treeTextView.Text = sb.ToString();
    }
    
    private void ImprimirNodo(StringBuilder sb, NodoSintactico nodo, string prefijo, string prefijoHijo)
    {
        switch (nodo)
        {
            case Asignacion asig:
                sb.AppendLine($"{prefijo}Asignación: {asig.Identificador} =");
                ImprimirNodo(sb, asig.Valor, prefijoHijo + "└── ", prefijoHijo + "    ");
                break;

            case Binaria bin:
                sb.AppendLine($"{prefijo}Operación: {bin.Operador}");
                sb.AppendLine($"{prefijoHijo}├── Izquierda:");
                ImprimirNodo(sb, bin.Izquierda, prefijoHijo + "│   └── ", prefijoHijo + "│       ");
                sb.AppendLine($"{prefijoHijo}└── Derecha:");
                ImprimirNodo(sb, bin.Derecha, prefijoHijo + "    └── ", prefijoHijo + "        ");
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
    
    private void ActualizarVistaGrafica(Programa arbol)
    {
        if (_treeCanvas == null) return;
        
        _treeCanvas.Children.Clear();
        
        // Dibujar el árbol desde la raíz
        double inicioX = 400;
        double inicioY = 30;
        
        DibujarNodo(arbol, inicioX, inicioY, 300, 0);
    }
    
    private double DibujarNodo(NodoSintactico nodo, double x, double y, double espacioHorizontal, int nivel)
    {
        if (_treeCanvas == null) return y;
        
        // Obtener información del nodo
        string texto = ObtenerTextoNodo(nodo);
        var color = ObtenerColorNodo(nodo);
        
        // Dibujar el nodo como un rectángulo con texto
        var rect = new Rectangle
        {
            Width = 120,
            Height = 40,
            Fill = new SolidColorBrush(color),
            Stroke = new SolidColorBrush(Colors.Black),
            StrokeThickness = 2,
            RadiusX = 5,
            RadiusY = 5
        };
        Canvas.SetLeft(rect, x - 60);
        Canvas.SetTop(rect, y);
        _treeCanvas.Children.Add(rect);
        
        var textBlock = new TextBlock
        {
            Text = texto,
            Foreground = new SolidColorBrush(Colors.White),
            FontSize = 11,
            FontWeight = FontWeight.Bold,
            TextWrapping = TextWrapping.Wrap,
            Width = 110,
            TextAlignment = TextAlignment.Center
        };
        Canvas.SetLeft(textBlock, x - 55);
        Canvas.SetTop(textBlock, y + 10);
        _treeCanvas.Children.Add(textBlock);
        
        // Dibujar hijos
        var hijos = ObtenerHijos(nodo);
        if (hijos.Count > 0)
        {
            double yHijo = y + 80;
            double espacioTotal = espacioHorizontal * hijos.Count;
            double xInicio = x - espacioTotal / 2 + espacioHorizontal / 2;
            
            for (int i = 0; i < hijos.Count; i++)
            {
                double xHijo = xInicio + i * espacioHorizontal;
                
                // Dibujar línea al hijo
                var line = new Line
                {
                    StartPoint = new Point(x, y + 40),
                    EndPoint = new Point(xHijo, yHijo),
                    Stroke = new SolidColorBrush(Colors.Gray),
                    StrokeThickness = 2
                };
                _treeCanvas.Children.Add(line);
                
                // Dibujar el hijo recursivamente
                DibujarNodo(hijos[i], xHijo, yHijo, espacioHorizontal / 2, nivel + 1);
            }
            
            return yHijo;
        }
        
        return y;
    }
    
    private string ObtenerTextoNodo(NodoSintactico nodo)
    {
        return nodo switch
        {
            Programa => "PROGRAMA",
            Asignacion asig => $"{asig.Identificador} =",
            Binaria bin => bin.Operador,
            Numero num => num.Lexema,
            Variable var => var.Nombre,
            If => "IF",
            DoLoop loop => $"DO {loop.Label}",
            Declaracion decl => $"{decl.TipoDato}",
            DirectivaProgram prog => $"PROGRAM\n{prog.NombrePrograma}",
            SentenciaPrint => "PRINT",
            DirectivaSimple dir => dir.Tipo,
            _ => nodo.Tipo
        };
    }
    
    private Color ObtenerColorNodo(NodoSintactico nodo)
    {
        return nodo switch
        {
            Programa => Color.Parse("#3182CE"),          // Azul
            Asignacion => Color.Parse("#38A169"),        // Verde
            Binaria or Numero or Variable => Color.Parse("#D69E2E"), // Amarillo
            If or DoLoop => Color.Parse("#E53E3E"),      // Rojo
            Declaracion => Color.Parse("#805AD5"),       // Púrpura
            DirectivaProgram => Color.Parse("#2C5282"),  // Azul oscuro
            SentenciaPrint => Color.Parse("#DD6B20"),    // Naranja
            DirectivaSimple => Color.Parse("#718096"),   // Gris
            _ => Color.Parse("#4A5568")                  // Gris oscuro
        };
    }
    
    private List<NodoSintactico> ObtenerHijos(NodoSintactico nodo)
    {
        var hijos = new List<NodoSintactico>();
        
        switch (nodo)
        {
            case Programa prog:
                hijos.AddRange(prog.Sentencias);
                break;
            case Asignacion asig:
                hijos.Add(asig.Valor);
                break;
            case Binaria bin:
                hijos.Add(bin.Izquierda);
                hijos.Add(bin.Derecha);
                break;
            case If ifNode:
                hijos.Add(ifNode.Condicion);
                hijos.AddRange(ifNode.SentenciasThen);
                hijos.AddRange(ifNode.SentenciasElse);
                break;
            case DoLoop loop:
                hijos.Add(loop.Inicio);
                hijos.Add(loop.Fin);
                if (loop.Incremento != null)
                    hijos.Add(loop.Incremento);
                hijos.AddRange(loop.Sentencias);
                break;
        }
        
        return hijos;
    }
    
    private void ActualizarEstadisticas(Programa arbol, long tiempoMs)
    {
        int totalNodos = ContarNodos(arbol);
        int profundidadMax = CalcularProfundidad(arbol, 0);
        int nodosHoja = ContarNodosHoja(arbol);
        
        if (_totalNodesText != null)
            _totalNodesText.Text = totalNodos.ToString();
        
        if (_maxDepthText != null)
            _maxDepthText.Text = profundidadMax.ToString();
        
        if (_longestBranchText != null)
            _longestBranchText.Text = profundidadMax.ToString();
        
        if (_leafNodesText != null)
            _leafNodesText.Text = nodosHoja.ToString();
        
        if (_analysisTimeText != null)
            _analysisTimeText.Text = tiempoMs > 0 ? $"{tiempoMs} ms" : "N/A";
    }
    
    private int ContarNodos(NodoSintactico nodo)
    {
        int count = 1;
        foreach (var hijo in ObtenerHijos(nodo))
        {
            count += ContarNodos(hijo);
        }
        return count;
    }
    
    private int CalcularProfundidad(NodoSintactico nodo, int nivelActual)
    {
        var hijos = ObtenerHijos(nodo);
        if (hijos.Count == 0)
            return nivelActual;
        
        int maxProfundidad = nivelActual;
        foreach (var hijo in hijos)
        {
            int profundidad = CalcularProfundidad(hijo, nivelActual + 1);
            if (profundidad > maxProfundidad)
                maxProfundidad = profundidad;
        }
        return maxProfundidad;
    }
    
    private int ContarNodosHoja(NodoSintactico nodo)
    {
        var hijos = ObtenerHijos(nodo);
        if (hijos.Count == 0)
            return 1;
        
        int count = 0;
        foreach (var hijo in hijos)
        {
            count += ContarNodosHoja(hijo);
        }
        return count;
    }
    
    private void AplicarZoom(double factor)
    {
        _escalaActual *= factor;
        _escalaActual = Math.Max(0.5, Math.Min(2.0, _escalaActual));
        
        if (_treeCanvas != null && _arbolActual != null)
        {
            _treeCanvas.Width = 800 * _escalaActual;
            _treeCanvas.Height = 600 * _escalaActual;
            ActualizarVistaGrafica(_arbolActual);
        }
    }
    
    private void AjustarAPantalla()
    {
        _escalaActual = 1.0;
        if (_treeCanvas != null)
        {
            _treeCanvas.Width = 800;
            _treeCanvas.Height = 600;
        }
        if (_arbolActual != null)
        {
            ActualizarVistaGrafica(_arbolActual);
        }
    }
}