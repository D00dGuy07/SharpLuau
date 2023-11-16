using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SharpLuau.Compilation.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

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

    public class Transpiler
    {
        private TranspilerOptions m_Options;

        public Transpiler(TranspilerOptions options) 
        { 
            m_Options = options;
        }

        public TranspilationStatus Transpile(List<string> files)
        {
            // Preprocess all of the files first
            List<SyntaxTree> trees = new();
            List<PreprocessorDirective[]> directives = new();
            foreach (string filePath in files) 
            {
                directives.Add(CustomPreprocessor.PreprocessText(File.ReadAllText(filePath), out string preprocessedText));
                trees.Add(CSharpSyntaxTree.ParseText(preprocessedText));
            }

            // I wish I could just access the corelib that is embedded already
            Stream? coreLibStream = Assembly.GetExecutingAssembly().GetManifestResourceStream("SharpLuau.res.System.Private.CoreLib.dll");
            if (coreLibStream == null) return new TranspilationStatus { Success = false, Message="Couldn't access the embedded CoreLib" };

            // This is all it takes to compile the c# code as far as I need
			var compilation = CSharpCompilation.Create("Compilation")
				.AddReferences(MetadataReference.CreateFromStream(coreLibStream))
				.AddSyntaxTrees(trees);

			LuauSyntaxConverter converter = new();
			converter.IncludeNewlines = m_Options.IncludeNewlines;

			// For each file, run the syntax converter
			FileConversionData[] conversionData = new FileConversionData[files.Count];
			for (int i = 0; i < files.Count; i++)
            {
				SyntaxTree tree = trees[i];

				SemanticModel model = compilation.GetSemanticModel(tree);
				CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

                // Console diagnostics are good
				var diagnostics = model.GetDiagnostics();
				foreach (var diagnostic in diagnostics)
					Console.WriteLine(diagnostic.ToString());
				if (diagnostics.Length > 0) continue;

				// Convert the file
				conversionData[i] = converter.ConvertFile(tree, directives[i], model);
			}

			// Combine all of the local file contexts
			LuauContext[] contexts = new LuauContext[files.Count];
			for (int i = 0; i < files.Count; i++) contexts[i] = conversionData[i].CompilationContext; 
			LuauContext globalContext = LuauContext.Combine(contexts);

            // Output the result for each file
            for (int i = 0; i < files.Count; i++)
            {
				// Get the paths
				FileInfo outputFile = GetOutputPath(files[i], m_Options.OutputDirectory);
				FileInfo intFile = GetIntermediatePath(files[i], m_Options.IntermediateDirectory);

				if (outputFile.Directory == null || intFile.Directory == null)
					throw new Exception($"Couldn't get the output directories for {files[i]}");

				// Stuff gets mad when the output directory doesn't already exist
				if (!outputFile.Directory.Exists)
					outputFile.Directory.Create();

				if (!intFile.Directory.Exists)
					intFile.Directory.Create();

				// Write the resulting syntax out to a file
				string? fileName = Path.GetFileNameWithoutExtension(files[i]);
				File.WriteAllText(outputFile.FullName, conversionData[i].ConvertedCode.WriteText(globalContext));

				// Write the obj
				using (FileStream fileStream = File.OpenWrite($"{intFile.DirectoryName}\\{fileName}.slobj"))
				{
					using BinaryWriter writer = new(fileStream, Encoding.UTF8);

					conversionData[i].CompilationContext.WriteBinary(writer);
					conversionData[i].ConvertedCode.WriteBinary(writer);
					fileStream.Close();
				}

				using (FileStream fileStream = File.OpenRead($"{intFile.DirectoryName}\\{fileName}.slobj"))
				{
					using BinaryReader reader = new(fileStream, Encoding.UTF8);

					LuauContext parsedContext = SLOBJParserV1.ParseContext(reader);
					LuauBlock statement = SLOBJParserV1.ParseCode(reader);
					Console.WriteLine(statement.WriteText(parsedContext));
					Console.WriteLine();
				}
			}

			return new TranspilationStatus { Success = true };
        }

		public static FileInfo GetIntermediatePath(string relativePath, DirectoryInfo intDirectory)
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
    }
}
