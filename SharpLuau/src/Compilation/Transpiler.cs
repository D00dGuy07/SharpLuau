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
    internal struct TranspilationStatus
    {
        public string? Message;
        public bool Success;
    }

    internal struct TranspilerOptions
    {
        public bool IncludeNewlines;
        public string OutputDirectory;
    }

    internal class Transpiler
    {
        private TranspilerOptions m_Options;

        public Transpiler(TranspilerOptions options) 
        { 
            m_Options = options;
        }

        public TranspilationStatus Transpile(List<string> files)
        {
            List<SyntaxTree> trees = new();
            List<PreprocessorDirective[]> directives = new();
            foreach (string filePath in files) 
            {
                directives.Add(CustomPreprocessor.PreprocessText(File.ReadAllText(filePath), out string programText));
                trees.Add(CSharpSyntaxTree.ParseText(programText));
            }

			var compilation = CSharpCompilation.Create("Compilation")
				.AddReferences(MetadataReference.CreateFromFile(Assembly.GetAssembly(typeof(int))?.Location ?? ""))
				.AddSyntaxTrees(trees);

            DirectoryInfo outputDirectory = new DirectoryInfo(m_Options.OutputDirectory);
            if (!outputDirectory.Exists)
                outputDirectory.Create();

			LuauSyntaxConverter converter = new();
			converter.IncludeNewlines = m_Options.IncludeNewlines;

			for (int i = 0; i < files.Count; i++)
            {
				SyntaxTree tree = trees[i];

				SemanticModel model = compilation.GetSemanticModel(tree);
				CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

				var diagnostics = model.GetDiagnostics();
				foreach (var diagnostic in diagnostics)
					Console.WriteLine(diagnostic.ToString());
				if (diagnostics.Length > 0) continue;

                converter.SetDirectives(directives[i], root);
				converter.ConvertTree(tree, model);

				LuauBlock? block = converter.GetConvertedCode().FirstOrDefault();
                if (block == null) continue;
                
                File.WriteAllText($"{m_Options.OutputDirectory}\\{Path.GetFileNameWithoutExtension(files[i])}.lua", block.ToString());
			}

			return new TranspilationStatus { Success = true };
        }
    }
}
