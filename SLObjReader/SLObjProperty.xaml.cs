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

namespace SLObjReader
{
	/// <summary>
	/// Interaction logic for SLObjProperty.xaml
	/// </summary>
	public partial class SLObjProperty : UserControl
	{
		public SLObjProperty(string title, string value)
		{
			InitializeComponent();

			if (FindName("Title") is TextBlock titleBlock) titleBlock.Text = title;
			if (FindName("Value") is TextBlock valueBlock) valueBlock.Text = value;
		}
	}
}
