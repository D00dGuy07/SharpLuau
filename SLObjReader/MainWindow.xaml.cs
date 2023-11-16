using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Runtime.InteropServices;
using SharpLuau;
using System.IO;
using SharpLuau.Compilation;

namespace SLObjReader
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		public MainWindow()
		{
			InitializeComponent();

			// Bind events
			App.Current.ProjectLoaded += OnProjectLoaded;
			App.Current.ProjectUnloading += OnProjectUnloaded;

			App.Current.SLObjFileSelected += (sender, args) =>
			{
				if (FindName("FileSelection") is not TextBlock fileSelection) return;

				fileSelection.Text = args.File?.Name ?? string.Empty;
			};
		}

		private void OnProjectLoaded(object? sender, ProjectLoadedEventArgs args)
		{
			// Verify the existence of dependencies
			if (args.ProjectFile.IntermediatePath == null) return;
			if (FindName("FileSelectorPanel") is not StackPanel filesPanel) return;

			// Update the list of files
			filesPanel.Children.Clear();
			foreach (string filePath in args.ProjectFile.Files)
			{
				FileInfo fileInfo = Transpiler.GetIntermediatePath(filePath, new(args.ProjectFile.IntermediatePath));
				if (fileInfo.Exists)
					filesPanel.Children.Add(new SLObjFileButton(fileInfo) { Margin = new Thickness(0.0, 0.0, 0.0, 5.0) });
			}
		}

		private void OnProjectUnloaded(object? sender, ProjectUnloadingEventArgs args)
		{
			if (FindName("FileSelectorPanel") is not StackPanel filesPanel) return;
			filesPanel.Children.Clear();
		}

		private void OnOpenCommand(object sender, RoutedEventArgs e)
		{
			App.Current.PromptLoadProject();
		}

		private void OnCloseCommand(object sender, RoutedEventArgs e)
		{
			App.Current.UnloadProjectFile();
		}
	}
}
