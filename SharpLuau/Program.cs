using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using SharpLuau.Commands;
using SharpLuau.Syntax;
using System.Collections.Immutable;

namespace SharpLuau
{
    internal class Program
	{
		static void Main(string[] args)
		{
			// Option parsing
			CommandLineParser argsParser = new("SharpLuau");
			argsParser.RegisterCommand<BuildCommand>("build", "Compile a project");

            argsParser.Parse(args);
			return;

			var watch = new System.Diagnostics.Stopwatch();
			watch.Start();

			// Preprocess the file

			string fileText = File.ReadAllText("D:\\dev\\CSharpResearch\\SharpLuau\\SharpLuau\\HelloWorld.cs");
			var directives = CustomPreprocessor.PreprocessText(fileText, out string programText);

			// Compile the file and get the information about it

			SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);

			var compilation = CSharpCompilation.Create("Compilation")
				.AddReferences(MetadataReference.CreateFromFile("C:\\Program Files\\dotnet\\packs\\Microsoft.NETCore.App.Ref\\6.0.12\\ref\\net6.0\\System.Runtime.dll"))
				.AddSyntaxTrees(tree);

			SemanticModel model = compilation.GetSemanticModel(tree);
			CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

			// Print errors and stop here or continue if there were none

			var diagnostics = model.GetDiagnostics();
			foreach(var diagnostic in diagnostics )
				Console.WriteLine(diagnostic.ToString());
			if (diagnostics.Length > 0) return;

			// Convert the tree

			LuauSyntaxConverter converter = new(model);
			converter.IncludeNewlines = true;
			converter.AddDirectives(directives, root);
			converter.ConvertTree(tree);

			LuauBlock block = converter.GetConvertedCode().First();

			watch.Stop();
			Console.WriteLine($"Execution Time (ms): {watch.ElapsedMilliseconds}\n");

			// Replace the last line with a call to Program::Main()

			block.RemoveLast();

			LuauExpressionStatement mainCall = new();
			block.AddLast(mainCall);
			mainCall.Expression = new LuauInvocation(new LuauIdentifier("print"),
				new List<LuauExpression> { new LuauInvocation(new LuauQualifiedIdentifier("Program._Main0"), null) });

			// Print the transpiled Luau code

			string src = block.ToString() ?? "";
			Console.WriteLine(src);

			// Run the Luau code

			//LuauInterop.RunCode(src);
		}
	}
}