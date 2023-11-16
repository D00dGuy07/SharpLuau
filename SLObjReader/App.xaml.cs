using SharpLuau;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Windows;

namespace SLObjReader
{
	public class ProjectLoadedEventArgs
	{
		public ProjectFile ProjectFile;

		public ProjectLoadedEventArgs(ProjectFile projectFile)
		{
			ProjectFile = projectFile;
		}
	}

	public class ProjectUnloadingEventArgs
	{
		public ProjectFile ProjectFile;

		public ProjectUnloadingEventArgs(ProjectFile projectFile)
		{
			ProjectFile = projectFile;
		}
	}

	public class SLObjFileSelectedEventArgs
	{
		public FileInfo? File;

		public SLObjFileSelectedEventArgs(FileInfo? file)
		{
			File = file;
		}
	}

	/// <summary>
	/// Interaction logic for App.xaml
	/// </summary>
	public partial class App : Application
	{
		public static new App Current { get { return (App)Application.Current; } }

		public ProjectFile? ProjectFile { get; private set; }
		public FileInfo? ProjectFileInfo { get; private set; }
		public FileInfo? SelectedSLObjInfo { get; private set; }

		public event EventHandler<ProjectLoadedEventArgs>? ProjectLoaded;
		public event EventHandler<ProjectUnloadingEventArgs>? ProjectUnloading;

		public event EventHandler<SLObjFileSelectedEventArgs>? SLObjFileSelected;

		public void PromptLoadProject()
		{
			// Prompt the user to select a project to load
			OpenFileName ofn = new()
			{
				filter = "All Files\0*.*\0SharpLuau Projects\0*.toml\0",
				file = new string(new char[1024]),
				fileTitle = new string(new char[256]),
				initialDir = "C:\\",
				title = "Select a SharpLuau project file",
				defExt = "toml"
			};
			ofn.structSize = Marshal.SizeOf(ofn);
			ofn.maxFile = ofn.file.Length;
			ofn.maxFileTitle = ofn.fileTitle.Length;

			if (!NativeFunctions.GetOpenFileName(ofn)) return;

			// Load the project file
			LoadProjectFile(new(ofn.file));
		}

		public void LoadProjectFile(FileInfo fileInfo)
		{
			if (ProjectFile != null) UnloadProjectFile();

			// Parse the project file
			ProjectFile = ProjectFile.Parse(fileInfo);

			if (ProjectFile != null) 
			{
				// Update state and invoke event if the file was valid
				ProjectFileInfo = fileInfo;
				ProjectLoaded?.Invoke(this, new(ProjectFile));
			}
		}

		public void UnloadProjectFile()
		{
			if (ProjectFile == null) return;

			// Invoke the event
			ProjectUnloading?.Invoke(this, new(ProjectFile));

			// Update state
			ProjectFile = null;
			ProjectFileInfo = null;
			SelectedSLObjInfo = null;
		}

		public void SelectSLObjFile(FileInfo? fileInfo)
		{
			// If a file was passed in then verify that it exists and is a .slobj file
			if (fileInfo != null && (!fileInfo.Exists || fileInfo.Extension != ".slobj")) return;

			// Update the state and invoke the event
			SelectedSLObjInfo = fileInfo;
			SLObjFileSelected?.Invoke(this, new(fileInfo));
		}
	}
}
