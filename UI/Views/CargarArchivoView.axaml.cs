using Avalonia;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System;

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
    
    // Propiedades que usan FindControl para evitar problemas con generación automática
    private ListBox? _filesListBox;
    private TextBlock? _selectedFileText;
    private TextBox? _fileContentTextBox;
    private Button? _refreshButton;
    private Button? _loadFileButton;
    
    private ListBox? FilesListBox => _filesListBox ??= this.FindControl<ListBox>("LstFiles");
    private TextBlock? SelectedFileText => _selectedFileText ??= this.FindControl<TextBlock>("TxtSelectedFile");
    private TextBox? FileContentTextBox => _fileContentTextBox ??= this.FindControl<TextBox>("TxtFileContent");
    private Button? RefreshButton => _refreshButton ??= this.FindControl<Button>("BtnRefresh");
    private Button? LoadFileButton => _loadFileButton ??= this.FindControl<Button>("BtnLoadFile");

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
        if (FilesListBox != null)
            FilesListBox.SelectionChanged += CuandoCambiaSeleccionArchivos;
        if (RefreshButton != null)
            RefreshButton.Click += CuandoClickRefrescar;
        if (LoadFileButton != null)
            LoadFileButton.Click += CuandoClickCargarArchivo;
    }

    private void CargarArchivosDisponibles()
    {
        ArchivosDisponibles.Clear();

        string rutaInput = Path.Combine(Directory.GetCurrentDirectory(), "input");

        if (Directory.Exists(rutaInput))
        {
            var archivos = Directory.GetFiles(rutaInput, "*.*")
                .Where(archivo => archivo.EndsWith(".txt", System.StringComparison.OrdinalIgnoreCase) ||
                                 archivo.EndsWith(".f77", System.StringComparison.OrdinalIgnoreCase) ||
                                 archivo.EndsWith(".for", System.StringComparison.OrdinalIgnoreCase))
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

        if (FilesListBox != null)
        {
            FilesListBox.ItemsSource = ArchivosDisponibles;
        }
    }

    private void CuandoCambiaSeleccionArchivos(object? sender, SelectionChangedEventArgs e)
    {
        if (FilesListBox?.SelectedItem is InformacionArchivo archivoSeleccionado)
        {
            if (SelectedFileText != null)
                SelectedFileText.Text = $"Archivo seleccionado: {archivoSeleccionado.Nombre}";
            if (LoadFileButton != null)
                LoadFileButton.IsEnabled = true;

            try
            {
                string contenido = File.ReadAllText(archivoSeleccionado.RutaCompleta);
                if (FileContentTextBox != null)
                    FileContentTextBox.Text = contenido;
            }
            catch (Exception ex)
            {
                if (FileContentTextBox != null)
                    FileContentTextBox.Text = $"Error al leer el archivo: {ex.Message}";
            }
        }
        else
        {
            if (SelectedFileText != null)
                SelectedFileText.Text = "Ningún archivo seleccionado";
            if (LoadFileButton != null)
                LoadFileButton.IsEnabled = false;
            if (FileContentTextBox != null)
                FileContentTextBox.Text = string.Empty;
        }
    }

    private void CuandoClickRefrescar(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        CargarArchivosDisponibles();
        if (SelectedFileText != null)
            SelectedFileText.Text = "Ningún archivo seleccionado";
        if (LoadFileButton != null)
            LoadFileButton.IsEnabled = false;
        if (FileContentTextBox != null)
            FileContentTextBox.Text = string.Empty;
    }

    private void CuandoClickCargarArchivo(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        if (FilesListBox?.SelectedItem is InformacionArchivo archivoSeleccionado)
        {
            // Obtener referencia al MainWindow
            var ventanaPrincipal = this.VisualRoot as MainWindow;
            if (ventanaPrincipal != null)
            {
                ventanaPrincipal.CargarArchivo(archivoSeleccionado.RutaCompleta);

                // Mostrar mensaje de confirmación
                var ventanaMensaje = new Window
                {
                    Title = "Archivo Cargado",
                    Content = new TextBlock
                    {
                        Text = $"Archivo '{archivoSeleccionado.Nombre}' cargado exitosamente.\n\nYa puede proceder con el análisis léxico y sintáctico.",
                        Margin = new Thickness(20)
                    },
                    SizeToContent = SizeToContent.WidthAndHeight,
                    WindowStartupLocation = WindowStartupLocation.CenterOwner
                };
                ventanaMensaje.ShowDialog(ventanaPrincipal);
            }
        }
    }
}