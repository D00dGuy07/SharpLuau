using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Text;

namespace SharpLuau
{
    internal class Program
    {
        const string programText =
        @"namespace HelloWorld
        {
            class Program
            {
                static void Main(int d, string x)
                {
                    int a = 4;
                    int b = 5;
                    int c = a + b;
                }

                public void Test(int a, int b)
                {
                    int c = a << b;
                    bool d = a >= b || false;
                }
            }
        }";

        static void Main(string[] args)
        {
            SyntaxTree tree = CSharpSyntaxTree.ParseText(programText);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();

            var compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(MetadataReference.CreateFromFile(
                    typeof(string).Assembly.Location))
                .AddSyntaxTrees(tree);

            SemanticModel model = compilation.GetSemanticModel(tree);

            var diagnostics = model.GetDiagnostics();
            foreach(var diagnostic in diagnostics )
            {
                Console.WriteLine(diagnostic.ToString());
            }

            var namespaceDecl = root.DescendantNodes().OfType<NamespaceDeclarationSyntax>().Single();

            var classDecl = namespaceDecl.Members.OfType<ClassDeclarationSyntax>().FirstOrDefault();

            if (classDecl == null)
                return;

            Console.WriteLine(LuauSyntaxConverter.ConvertClass(classDecl, model));

        }
    }
}