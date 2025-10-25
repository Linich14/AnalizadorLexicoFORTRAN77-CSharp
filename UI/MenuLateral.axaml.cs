using System;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace AnalizadorLexico.UI;

public partial class MenuLateral : UserControl
{
    public event System.Action? OnLoadFileClicked;
    public event System.Action? OnAnalyzeExpressionClicked;
    public event System.Action? OnViewTreeClicked;
    public event System.Action? OnExitClicked;

    public MenuLateral()
    {
        InitializeComponent();
        ConfigurarEventosInternos();
    }

    private void InitializeComponent()
    {   Console.WriteLine("MenuLateral InitializeComponent llamado");
        AvaloniaXamlLoader.Load(this);
    }

    private void ConfigurarEventosInternos()
    {
        this.AttachedToVisualTree += (s, e) =>
        {
            Console.WriteLine("MenuLateral.AttachedToVisualTree disparado");
            
            // Configurar eventos de los botones internos cuando el control esté listo
            var loadFileButton = this.FindControl<Button>("LoadFileButton");
            Console.WriteLine($"LoadFileButton encontrado: {loadFileButton != null}");
            if (loadFileButton != null)
            {
                loadFileButton.Click += (sender, args) => {
                    Console.WriteLine("LoadFileButton.Click disparado");
                    OnLoadFileClicked?.Invoke();
                };
            }

            var analyzeExpressionButton = this.FindControl<Button>("AnalyzeExpressionButton");
            if (analyzeExpressionButton != null)
            {
                analyzeExpressionButton.Click += (sender, args) => OnAnalyzeExpressionClicked?.Invoke();
            }

            var viewTreeButton = this.FindControl<Button>("ViewTreeButton");
            if (viewTreeButton != null)
            {
                viewTreeButton.Click += (sender, args) => OnViewTreeClicked?.Invoke();
            }

            var exitButton = this.FindControl<Button>("ExitButton");
            if (exitButton != null)
            {
                exitButton.Click += (sender, args) => OnExitClicked?.Invoke();
            }
        };
    }
}