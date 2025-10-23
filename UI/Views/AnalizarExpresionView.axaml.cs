using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace AnalizadorLexico.UI.Views;

public partial class AnalizarExpresionView : UserControl
{
    public AnalizarExpresionView()
    {
        InitializeComponent();
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
}