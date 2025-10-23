using Avalonia.Controls;
using AnalizadorLexico.UI.Views;

namespace AnalizadorLexico;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        MostrarVistaBienvenida();
        ConfigurarEventosMenu();
    }

    private void ConfigurarEventosMenu()
    {
        if (MenuLateral == null || PanelPrincipal == null)
            return;

        // Evento para cargar archivo
        var botonCargarArchivo = MenuLateral.FindControl<Button>("LoadFileButton");
        if (botonCargarArchivo != null)
            botonCargarArchivo.Click += (sender, e) =>
            {
                MostrarVistaCargarArchivo();
            };

        // Evento para analizar expresión
        var botonAnalizarExpresion = MenuLateral.FindControl<Button>("AnalyzeExpressionButton");
        if (botonAnalizarExpresion != null)
            botonAnalizarExpresion.Click += (sender, e) =>
            {
                MostrarVistaAnalizarExpresion();
            };

        // Evento para visualizar árbol
        var botonArbolSintactico = MenuLateral.FindControl<Button>("ViewTreeButton");
        if (botonArbolSintactico != null)
            botonArbolSintactico.Click += (sender, e) =>
            {
                MostrarVistaArbolSintactico();
            };

        // Evento para alternar tema
        var botonAlternarTema = MenuLateral.FindControl<Button>("ThemeToggleButton");
        if (botonAlternarTema != null)
            botonAlternarTema.Click += (sender, e) =>
            {
                AlternarTema();
            };

        // Evento para cerrar aplicación
        var botonCerrar = MenuLateral.FindControl<Button>("ExitButton");
        if (botonCerrar != null)
            botonCerrar.Click += (sender, e) =>
            {
                Close();
            };
    }

    private void MostrarVistaBienvenida()
    {
        if (PanelPrincipal?.AreaContenido != null)
            PanelPrincipal.AreaContenido.Content = new BienvenidaView();
    }

    private void MostrarVistaCargarArchivo()
    {
        if (PanelPrincipal?.AreaContenido != null)
            PanelPrincipal.AreaContenido.Content = new CargarArchivoView();
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
        var botonTema = MenuLateral.FindControl<Button>("ThemeToggleButton");
        if (botonTema != null && botonTema.Content != null)
        {
            if (botonTema.Content.ToString().Contains("Oscuro"))
            {
                botonTema.Content = "Tema Claro";
            }
            else
            {
                botonTema.Content = "Tema Oscuro";
            }
        }
    }
}
