using Avalonia.Controls;
using AnalizadorLexico.UI.Views;
using System.IO;
using System;

namespace AnalizadorLexico;

public partial class MainWindow : Window
{
    public string? ArchivoCargado { get; private set; }
    public string? ContenidoArchivoCargado { get; private set; }
    public MainWindow()
    {
        InitializeComponent();
        
        // Usar Opened en lugar de AttachedToVisualTree
        this.Opened += (s, e) =>
        {
            ConfigurarEventosMenu();
            MostrarVistaBienvenida();
        };
    }

    private void ConfigurarEventosMenu()
    {
        if (MenuLateral == null || PanelPrincipal == null)
        {
            return;
        }

        // Configurar eventos usando los eventos del MenuLateral
        MenuLateral.OnLoadFileClicked += () => {
            MostrarVistaCargarArchivo();
        };
        MenuLateral.OnAnalyzeExpressionClicked += () => {
            MostrarVistaAnalizarExpresion();
        };
        MenuLateral.OnViewTreeClicked += () => {
            MostrarVistaArbolSintactico();
        };
        MenuLateral.OnThemeToggleClicked += () => {
            AlternarTema();
        };
        MenuLateral.OnExitClicked += () => Close();
    }

    private void MostrarVistaBienvenida()
    {
        if (PanelPrincipal?.AreaContenido != null)
        {
            var vista = new BienvenidaView();
            PanelPrincipal.AreaContenido.Content = vista;
        }
        else
        {
            Console.WriteLine("No se pudo asignar vista Bienvenida - PanelPrincipal o AreaContenido es null");
        }
    }

    private void MostrarVistaCargarArchivo()
    {
        if (PanelPrincipal?.AreaContenido != null)
        {
            var vista = new CargarArchivoView();
            PanelPrincipal.AreaContenido.Content = vista;
        }
    }

    private void MostrarVistaAnalizarExpresion()
    {
        if (PanelPrincipal?.AreaContenido != null)
            PanelPrincipal.AreaContenido.Content = new AnalizarExpresionView();
    }

    private void MostrarVistaArbolSintactico()
    {
        if (PanelPrincipal?.AreaContenido != null)
            PanelPrincipal.AreaContenido.Content = new ArbolSintacticoView();
    }

    private void AlternarTema()
    {
        var botonTema = MenuLateral?.FindControl<Button>("ThemeToggleButton");
        if (botonTema != null && botonTema.Content != null)
        {
            if (botonTema.Content.ToString()?.Contains("Oscuro") == true)
            {
                botonTema.Content = "Tema Claro";
            }
            else
            {
                botonTema.Content = "Tema Oscuro";
            }
        }
    }

    public void CargarArchivo(string rutaArchivo)
    {
        try
        {
            ArchivoCargado = Path.GetFileName(rutaArchivo);
            ContenidoArchivoCargado = File.ReadAllText(rutaArchivo);
        }
        catch (Exception)
        {
            // Manejar error de carga
            ArchivoCargado = null;
            ContenidoArchivoCargado = null;
        }
    }
}
