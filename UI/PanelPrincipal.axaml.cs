using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace AnalizadorLexico.UI;

/// <summary>
/// Panel principal de la aplicación que contiene el área donde se muestran las diferentes vistas.
/// La propiedad AreaContenido es generada automáticamente por Avalonia desde el XAML.
/// </summary>
public partial class PanelPrincipal : UserControl
{
    // Propiedad que busca el control - nombre diferente en XAML para evitar conflicto
    public ContentControl? AreaContenido => this.FindControl<ContentControl>("ContentArea");

    public PanelPrincipal()
    {
        InitializeComponent();
    }

    private void InitializeComponent()
    {   
        AvaloniaXamlLoader.Load(this);
    }
}