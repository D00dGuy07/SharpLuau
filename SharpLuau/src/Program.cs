using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using SharpLuau.Compilation;
using SharpLuau.Commands;
using SharpLuau.Compilation.Syntax;
using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.Loader;

namespace SharpLuau
{
    internal class Program
	{
		static void Main(string[] args)
		{
			// Option parsing
			//CommandLineParser argsParser = new("SharpLuau");
			//argsParser.RegisterCommand<BuildCommand>("build", "Compile a project");

			new BuildCommand().OnRun(1);

            //argsParser.Parse(args);
		}
	}
}