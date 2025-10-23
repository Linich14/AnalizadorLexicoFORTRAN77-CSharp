using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace AnalizadorLexico.UI.Views;

public partial class ArbolSintacticoView : UserControl
{
    public ArbolSintacticoView()
    {
        InitializeComponent();
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
}