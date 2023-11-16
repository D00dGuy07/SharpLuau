using SharpLuau.Compilation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau.Commands
{
    internal class BuildCommand : CommandImplementation
	{
		[Option("rootFolder", "The folder where the project file is.")]
		[Alias("f")]
		public DirectoryInfo RootFolder = new(Directory.GetCurrentDirectory());

		public void OnRun(object globalOptions)
		{
			// Get the project file
			FileInfo projectFile = new(Path.Join(RootFolder.FullName, "sharpluau.toml"));
			if (!projectFile.Exists)
				Console.Error.WriteLine("Error: Couldn't find 'sharpluau.toml'.\nRun the 'generate' command to generate a project file.");

			ProjectFile? settings = ProjectFile.Parse(projectFile);
			if (settings == null || settings.Files.Count < 1) { return; }

			foreach (string path in settings.Files)
				Console.WriteLine(path);
			Console.WriteLine();

			TranspilerOptions options = new()
			{
				IncludeNewlines = settings.IncludeNewlines ?? true,
				OutputDirectory = new DirectoryInfo(settings.OutputPath ?? string.Empty),
				IntermediateDirectory = new DirectoryInfo(settings.IntermediatePath ?? string.Empty)
			};

			Transpiler transpiler = new Transpiler(options);
			transpiler.Transpile(settings.Files);

			/*
			Assembly? file = Assembly.GetAssembly(typeof(int));
			Console.WriteLine(file != null ? file.FullName : "Failed");
			*/
		}
	}
}
