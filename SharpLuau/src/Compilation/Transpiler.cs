using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using SharpLuau.Compilation.Syntax;
using System.Collections.Immutable;
using System.Reflection;
using System.Text;

namespace SharpLuau.Compilation
{
	public struct TranspilationStatus
    {
        public string? Message;
        public bool Success;
    }

    public struct TranspilerOptions
    {
        public bool IncludeNewlines;
        public DirectoryInfo OutputDirectory;
        public DirectoryInfo IntermediateDirectory;
	}

	public struct SLOBJHeader
	{
		// Version 1

		public uint Version;

		public void WriteBinary(BinaryWriter writer)
		{
			writer.Write(Version);

			// Reserving 64 bytes total for the header allows for future compatibility
			byte[] reserved = new byte[60];
			writer.Write(reserved);
		}

		public static SLOBJHeader ReadBinary(BinaryReader reader)
		{
			var header = new SLOBJHeader()
			{
				Version = reader.ReadUInt32()
			};

			// Advance past the padding
			reader.ReadBytes(60);

			return header;
		}
	}

	public class Transpiler
    {
        private TranspilerOptions m_Options;
		private LuauSyntaxConverter m_Converter;

        public Transpiler(TranspilerOptions options) 
        { 
            m_Options = options;

			m_Converter = new();
			m_Converter.IncludeNewlines = m_Options.IncludeNewlines;
		}

		public static FileInfo GetObjectPath(string relativePath, DirectoryInfo intDirectory)
		{
			return new(Path.Join(
				Path.Join(intDirectory.FullName, Path.GetDirectoryName(relativePath)),
				$"{Path.GetFileNameWithoutExtension(relativePath)}.slobj"
			));
		}

		public static FileInfo GetOutputPath(string relativePath, DirectoryInfo outDirectory)
		{
			return new(Path.Join(
				Path.Join(outDirectory.FullName, Path.GetDirectoryName(relativePath)),
				$"{Path.GetFileNameWithoutExtension(relativePath)}.lua"
			));
		}

		public TranspilationStatus Transpile(List<string> files)
        {
			// Splitting up the Transpile function is difficult because a lot of information has to be shared between steps.
			// Many of the steps also create multiple variables that need to be used by other steps. The more I write this
			// comment the more I'm realizing that I'm describing spaghetti code. This function is messy, and I'm not sure
			// that I can make it as clean as I would like it to be.

			// Preprocess all of the files first
			List<SyntaxTree> trees = new();
            List<PreprocessorDirective[]> directives = new();
            foreach (string filePath in files) 
            {
                directives.Add(CustomPreprocessor.PreprocessText(File.ReadAllText(filePath), out string preprocessedText));
                trees.Add(CSharpSyntaxTree.ParseText(preprocessedText));
            }

            // I wish I could just access the corelib that is already embedded
            Stream? coreLibStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("SharpLuau.res.System.Private.CoreLib.dll");
            if (coreLibStream == null) return new TranspilationStatus { Success = false, Message="Couldn't access the embedded CoreLib" };

			CSharpCompilationOptions cSharpOptions = new(
				OutputKind.DynamicallyLinkedLibrary, 
				nullableContextOptions: NullableContextOptions.Enable
			);

            // This is all it takes to compile the c# code as far as I need
			var compilation = CSharpCompilation.Create("Compilation", options: cSharpOptions)
				.AddReferences(MetadataReference.CreateFromStream(coreLibStream))
				.AddSyntaxTrees(trees);

			RojoSettings rojoSettings = RojoProjectReader.FindRojoSettings();
			DateTime rojoSettingsLastWrite = File.GetLastWriteTime(rojoSettings.FilePath);
			Dictionary<string, RobloxObjectPath> robloxAssociations = RojoProjectReader.GetPathAssociations(rojoSettings);
			foreach (var pair in robloxAssociations)
				Console.WriteLine($"{pair.Key}: game.{pair.Value.Service}.{string.Join('.', pair.Value.Path)}");
			Console.WriteLine();

			// For each file, get the file conversion data
			bool shouldNukeBuild = false;
			FileConversionData[] conversionData = new FileConversionData[files.Count];
			for (int i = 0; i < files.Count; i++)
            {
				SyntaxTree tree = trees[i];
				SemanticModel model = compilation.GetSemanticModel(tree);

				// Console diagnostics are good
				Console.WriteLine(files[i]);
				shouldNukeBuild |= HandleCSharpDiagnostics(model.GetDiagnostics());
				Console.WriteLine();

				// Convert the file
				conversionData[i] = ProcessFile(files[i], directives[i], tree, model, 
					robloxAssociations, rojoSettingsLastWrite);
			}

			// If the build generated any errors then don't continue
			if (shouldNukeBuild)
				return new TranspilationStatus { Success = false };

			// Combine all of the local file contexts
			LuauContext[] contexts = new LuauContext[files.Count];
			for (int i = 0; i < files.Count; i++) 
				contexts[i] = conversionData[i].CompilationContext; 
			LuauContext globalContext = LuauContext.Combine(contexts);

            // Output the result for each file
            for (int i = 0; i < files.Count; i++)
            {
				// Get the paths
				FileInfo outputFile = GetOutputPath(files[i], m_Options.OutputDirectory);
				if (outputFile.Directory == null)
					throw new Exception($"Couldn't get the output directories for {files[i]}");

				if (!outputFile.Directory.Exists)
					outputFile.Directory.Create();

				// Write the resulting syntax out to a file
				File.WriteAllText(outputFile.FullName, conversionData[i].ConvertedCode.WriteText(new(globalContext)));
			}

			return new TranspilationStatus { Success = true };
        }

		private static bool HandleCSharpDiagnostics(ImmutableArray<Diagnostic> diagnostics)
		{
			bool shouldNukeBuild = false;
			foreach (var diagnostic in diagnostics)
			{
				// Cancel the build if there was an error
				shouldNukeBuild |= diagnostic.Severity == DiagnosticSeverity.Error;

				// Print the diagnostic to the console
				Console.WriteLine(diagnostic.ToString());

				TextSpan span = diagnostic.Location.SourceSpan;
				Console.WriteLine($"-> {diagnostic.Location.SourceTree?.GetText().GetSubText(span)}");
			}

			return shouldNukeBuild;
		}

		private static RobloxObjectPath MatchRobloxAssociation(Dictionary<string, RobloxObjectPath> associations, FileInfo outputFile)
		{
			string outputPath = outputFile.FullName;

			string matchedKey = string.Empty;
			string matchedRelativePath = string.Empty;
			foreach (var pair in associations)
			{
				string relativePath = Path.GetRelativePath(pair.Key, outputPath);
				
				// The first two conditions ensure that the file is a descendant of the rojo capture path
				// The second one checks to see if this path matches a different association better
				// I don't actually know if the second case is even possible, but I'm including it to be safe
				if (relativePath != outputPath && !relativePath.Contains("..") && pair.Key.Length > matchedKey.Length)
				{
					matchedKey = pair.Key;
					matchedRelativePath = relativePath;
				}
			}

			if (matchedKey == string.Empty)
				throw new Exception("Failed to match a roblox path for a file. The rojo project file might not be configured correctly.");

			// Separate the individual names in order from the relative path
			List<string> objectNames = matchedRelativePath.Split(Path.DirectorySeparatorChar).SkipLast(1).ToList();
			string fileName = Path.GetFileNameWithoutExtension(matchedRelativePath);
			objectNames.Add(fileName);

			RobloxObjectPath rootObjectPath = associations.GetValueOrDefault(matchedKey) ?? new();
			return new(
				rootObjectPath.Service,
				rootObjectPath.Path.Concat(objectNames).ToArray()
			);
		}

		// I don't like that this function takes 6 parameters, but I think that it makes sense to do it like this
		private FileConversionData ProcessFile(
			string relativePath, PreprocessorDirective[] directives, 
			SyntaxTree tree, SemanticModel model,
			Dictionary<string, RobloxObjectPath> robloxAssociations,
			DateTime rojoSettingsLastWrite
		)
		{
			// Try to load an object file if a current one exists
			FileInfo objFile = GetObjectPath(relativePath, m_Options.IntermediateDirectory);
			FileInfo outputFile = GetOutputPath(relativePath, m_Options.OutputDirectory);
			FileInfo sourceFile = new(relativePath);

			// The source file and the rojo settings file need to be older than the binary file
			// in order for it to be valid
			if (objFile.LastWriteTime.CompareTo(sourceFile.LastWriteTime) > 0 && 
				objFile.LastWriteTime.CompareTo(rojoSettingsLastWrite) > 0)
			{
				if (TryReadObjectFile(objFile) is FileConversionData objData)
					return objData;
			}

			// Process the file otherwise
			FileConversionData data = m_Converter.ConvertFile(tree, directives, model, outputFile,
				MatchRobloxAssociation(robloxAssociations, outputFile));

			// Write the conversion data to an object file
			if (objFile.Directory != null && !objFile.Directory.Exists)
				objFile.Directory.Create();
			WriteObjectFile(data, objFile);

			return data;
		}

		private static void WriteObjectFile(FileConversionData data, FileInfo fileInfo)
		{
			// Prepare the header data
			SLOBJHeader header = new()
			{
				Version = 1
			};

			// Open the file for binary writing
			using FileStream fileStream = File.OpenWrite(fileInfo.FullName);
			using BinaryWriter writer = new(fileStream, Encoding.UTF8);

			// Write the file data
			header.WriteBinary(writer);
			data.CompilationContext.WriteBinary(writer);
			data.ConvertedCode.WriteBinary(writer);

			// Close the file
			fileStream.Close();
		}

		private static FileConversionData? TryReadObjectFile(FileInfo fileInfo)
		{
#if DEBUG
			// For development builds, I want to rebuild the file every time
			return null;
#else
			// If the object file doesn't exist then it needs to be recompiled
			if (!fileInfo.Exists)
				return null;

			// Open the file for binary reading
			using FileStream fileStream = File.OpenRead(fileInfo.FullName);
			using BinaryReader reader = new(fileStream, Encoding.UTF8);

			// If the object file isn't the latest version then just recompile it
			SLOBJHeader header = SLOBJHeader.ReadBinary(reader);
			if (header.Version != 1)
				return null;

			// Otherwise load it to speed up compile time (hopefully)
			return new()
			{
				CompilationContext = SLOBJParserV1.ParseContext(reader),
				ConvertedCode = SLOBJParserV1.ParseCode(reader)
			};
#endif
		}
    }
}