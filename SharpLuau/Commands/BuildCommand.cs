using System;
using System.Collections.Generic;
using System.Linq;
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
		}
	}
}
