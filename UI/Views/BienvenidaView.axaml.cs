using System;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;

namespace AnalizadorLexico.UI.Views;

public partial class BienvenidaView : UserControl
{
    public BienvenidaView()
    {
        Console.WriteLine("BienvenidaView constructor llamado");
        InitializeComponent();
        Console.WriteLine("BienvenidaView InitializeComponent completado");
    }

    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    }
}