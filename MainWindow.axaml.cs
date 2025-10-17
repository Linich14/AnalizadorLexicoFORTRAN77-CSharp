using Avalonia.Controls;
using AnalizadorLexico.Lexico;
using AnalizadorLexico.Sintaxis;
using System.Collections.Generic;
using System.Linq;

namespace AnalizadorLexico;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
    }

    private void AnalyzeButton_Click(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        // TODO: Implementar lógica de análisis léxico y sintáctico
        // Obtener texto de InputTextBox
        string input = this.FindControl<TextBox>("InputTextBox")?.Text ?? "";
        // Tokenizar
        var lexer = new Lexer(input);
        var tokens = lexer.Tokenizar().ToList();
        // Parsear
        ParserLL1 parser = new ParserLL1();
        ArbolSintactico arbol = parser.Parsear(tokens);
        // Mostrar resultados en OutputTextBlock
        var output = this.FindControl<TextBlock>("OutputTextBlock");
        if (output != null)
        {
            output.Text = "Análisis completado (implementación pendiente).";
        }
    }
}