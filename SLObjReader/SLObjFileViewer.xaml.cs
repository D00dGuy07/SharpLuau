using SharpLuau.Compilation.Syntax;
using System;
using System.Collections.Generic;
using System.Collections;
using System.IO;
using System.Linq;
using System.Reflection;
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
	/// Interaction logic for SLObjFileViewer.xaml
	/// </summary>
	public partial class SLObjFileViewer : UserControl
	{
		private LuauContext? m_Context;
		private LuauBlock? m_Block;

		public SLObjFileViewer()
		{
			InitializeComponent();

			// Bind events
			App.Current.SLObjFileSelected += OnFileSelected;
			App.Current.ProjectUnloading += OnProjectUnloading;

			if (FindName("FileTree") is TreeView treeView)
				treeView.SelectedItemChanged += OnSelectedItemChanged;
		}

		private void OnProjectUnloading(object? sender, ProjectUnloadingEventArgs args)
		{
			// Clear the state
			m_Context = null;
			m_Block = null;

			// Clear the panels
			if (FindName("FileTree") is TreeView treeView) treeView.Items.Clear();
			if (FindName("PropertiesPanel") is StackPanel propertiesPanel) propertiesPanel.Children.Clear();
			if (FindName("EmitPanel") is TextBlock emitPanel) emitPanel.Text = string.Empty;
		}

		private void OnFileSelected(object? sender, SLObjFileSelectedEventArgs args)
		{
			if (args.File != null)
				LoadFile(args.File);
		}

		private void OnDisplayNodeUnselected(object? sender, RoutedEventArgs args)
		{
			if (FindName("PropertiesPanel") is not StackPanel propertiesPanel) return;
			if (FindName("EmitPanel") is not TextBlock emitPanel) return;
			if (sender is not TreeViewItem displayNode) return;

			// Clear the info panels
			propertiesPanel.Children.Clear();
			emitPanel.Text = string.Empty;

			// Unbind the event so that only active node unselections will trigger this event
			displayNode.Unselected -= OnDisplayNodeUnselected;
		}

		private void OnSelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> args)
		{
			if (args.NewValue is not TreeViewItem displayNode) return;
			if (displayNode.Tag == null) return;

			// If the tag is just a string then just display that
			if (displayNode.Tag is string displayTag)
			{
				if (FindName("PropertiesPanel") is not StackPanel propertiesPanel) return;
				propertiesPanel.Children.Add(
					new SLObjProperty("Value", displayTag) { Margin = new Thickness(0.0, 0.0, 0.0, 5.0) }
				);
			}
			else // Otherwise fill the panel with the object's properties
			{
				ShowLuauProperties(displayNode.Tag);
				ShowEmittedLuau(displayNode.Tag);
			}

			// Bind the event so that the active node will triger unselections
			displayNode.Unselected += OnDisplayNodeUnselected;
		}

		private void ShowLuauProperties(object luauObject)
		{
			Type objectType = luauObject.GetType();
			BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.FlattenHierarchy;

			// Very straightforward
			foreach (PropertyInfo property in objectType.GetProperties(bindingFlags).Where(prop => prop.IsDefined(typeof(LuauTreePropertyAttribute), true)))
				AddLuauObjectProperty(property.GetValue(luauObject), property.Name);

			foreach (FieldInfo field in objectType.GetFields(bindingFlags).Where(field => field.IsDefined(typeof(LuauTreePropertyAttribute), true)))
				AddLuauObjectProperty(field.GetValue(luauObject), field.Name);
		}

		private void AddLuauObjectProperty(object? luauProperty, string name)
		{
			if (FindName("PropertiesPanel") is not StackPanel propertiesPanel) return;

			// I thought this would be more difficult but it wasn't
			propertiesPanel.Children.Add(
				new SLObjProperty(name, luauProperty?.ToString() ?? "null") { Margin = new Thickness(0.0, 0.0, 0.0, 5.0) }
			);
		}

		private void ShowEmittedLuau(object luauObject)
		{
			if (FindName("EmitPanel") is not TextBlock emitPanel) return;
			if (m_Context == null) return;

			// Do type safe stuff
			if (luauObject is LuauStatement statement)
				emitPanel.Text = statement.WriteText(m_Context);
			if (luauObject is LuauExpression expression)
				emitPanel.Text = expression.WriteText(m_Context);
		}

		private void LoadFile(FileInfo fileInfo)
		{
			{ // Load the file data in a dedicated scope so that everything gets cleaned up right after
				using FileStream fileStream = File.OpenRead(fileInfo.FullName);
				using BinaryReader reader = new(fileStream, Encoding.UTF8);

				m_Context = SLOBJParserV1.ParseContext(reader);
				m_Block = SLOBJParserV1.ParseCode(reader);
			}

			// Build the visual UI
			BuildTree();
		}

		private void BuildTree()
		{
			if (FindName("FileTree") is not TreeView treeView) return;
			treeView.Items.Clear();

			// Build the context branch
			BuildContextTree(treeView);

			// Build the code branch
			TreeViewItem codeNodes = new()
			{
				Header = "Code"
			};

			if (m_Block == null) return;
			foreach (LuauStatement statement in m_Block.Statements)
				AddCodeNode(statement, codeNodes);

			treeView.Items.Add(codeNodes);
		}

		private void BuildContextTree(TreeView treeView)
		{
			if (m_Context == null) return;

			TreeViewItem contextNode = new()
			{
				Header = "Context"
			};

			// Build the dynamic identifiers branch
			TreeViewItem dynamicIdentifiersNode = new()
			{
				Header = "Dynamic Identifiers"
			};
			contextNode.Items.Add(dynamicIdentifiersNode);

			foreach (var pair in m_Context.GetDynamicIdentifiersDictionary())
			{
				TreeViewItem identifierNode = new()
				{
					Header = pair.Value.Text,
					Tag = pair.Key,
				};

				dynamicIdentifiersNode.Items.Add(identifierNode);
			}

			treeView.Items.Add(contextNode);
		}

		private void AddCodeNode(object luauObject, TreeViewItem parent, string? name = null)
		{
			Type objectType = luauObject.GetType();

			TreeViewItem codeNode = new()
			{
				Header = name != null ? $"{name} ({objectType.Name})" : objectType.Name,
				Tag = luauObject
			};

			// Add all single child properties and fields

			foreach (FieldInfo field in objectType.GetFields().Where(field => field.IsDefined(typeof(LuauTreeChildAttribute), true)))
			{
				if (field.GetValue(luauObject) is LuauExpression expression)
					AddCodeNode(expression, codeNode, field.Name);
			}

			foreach (PropertyInfo property in objectType.GetProperties().Where(prop => prop.IsDefined(typeof(LuauTreeChildAttribute), true)))
			{
				if (property.GetValue(luauObject) is LuauExpression expression)
					AddCodeNode(expression, codeNode, property.Name);
			}

			// Add all child containing properties and fields

			foreach (FieldInfo field in objectType.GetFields().Where(field => field.IsDefined(typeof(LuauTreeChildrenAttribute), true)))
			{
				// Get objects and the attribute
				if (field.GetValue(luauObject) is not object obj) continue;
				if (field.GetCustomAttribute<LuauTreeChildrenAttribute>() is not LuauTreeChildrenAttribute attribute)
					throw new Exception("Field doesn't have attribute but is in the loop????");
				
				// Pass it off to the other function
				AddChildrenCodeNode(obj, codeNode, attribute.IncludeName ? field.Name : null);
			}

			foreach (PropertyInfo property in objectType.GetProperties().Where(prop => prop.IsDefined(typeof(LuauTreeChildrenAttribute), true)))
			{
				// Get objects and the attribute
				if (property.GetValue(luauObject) is not object obj) continue;
				if (property.GetCustomAttribute<LuauTreeChildrenAttribute>() is not LuauTreeChildrenAttribute attribute)
					throw new Exception("Property doesn't have attribute but is in the loop????");

				// Pass it off the the other function
				AddChildrenCodeNode(obj, codeNode, attribute.IncludeName ? property.Name : null);
			}

			parent.Items.Add(codeNode);
		}

		private void AddChildrenCodeNode(object childrenContainer, TreeViewItem parent, string? name = null)
		{
			// Make a new node if there's a name or create a new node if there shouldn't be a name
			TreeViewItem parentNode = parent;
			if (name != null)
			{
				parentNode = new()
				{
					Header = name
				};
				parent.Items.Add(parentNode);
			}
			
			// Add the children
			if (childrenContainer is LuauBlock block)
			{
				foreach (LuauStatement statement in block.Statements)
					AddCodeNode(statement, parentNode);
			}
			else if (childrenContainer is IList && childrenContainer.GetType().IsGenericType)
			{
				IList expressions = (IList)childrenContainer;
				foreach (object expression in expressions)
					AddCodeNode(expression, parentNode);
			}
		}
	}
}
