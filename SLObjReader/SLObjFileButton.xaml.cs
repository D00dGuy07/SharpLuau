using System;
using System.Collections.Generic;
using System.IO;
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

namespace SLObjReader
{
	/// <summary>
	/// Interaction logic for SLObjFileButton.xaml
	/// </summary>
	public partial class SLObjFileButton : UserControl
	{
		private FileInfo m_FileInfo;

		public SLObjFileButton(FileInfo fileInfo)
		{
			InitializeComponent();

			m_FileInfo = fileInfo;

			if (FindName("FileName") is not TextBlock fileName) return;
			if (FindName("RelativePath") is not TextBlock relativePath) return;

			fileName.Text = m_FileInfo.Name;
			relativePath.Text = System.IO.Path.GetRelativePath(
				App.Current.ProjectFileInfo?.Directory?.FullName ?? string.Empty, // This value should never be null
				m_FileInfo.FullName
			);
		}

		private void OnButtonClick(object sender, RoutedEventArgs e)
		{
			App.Current.SelectSLObjFile(m_FileInfo);
		}
	}
}
