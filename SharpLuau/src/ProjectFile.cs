using Microsoft.Extensions.FileSystemGlobbing;
using Microsoft.Extensions.FileSystemGlobbing.Abstractions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.Text;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Tomlyn;
using Tomlyn.Syntax;

namespace SharpLuau
{
    internal class ProjectFile
    {
        private class TomlCamelCaseConverter
        {
            [ThreadStatic]
            private static StringBuilder? _Builder;
            private static StringBuilder m_Builder => _Builder ??= new StringBuilder();

            public static string ToCamelCase(string name)
            {
                StringBuilder builder = m_Builder;
                builder.Length = 0;

                // Handle the first part of pascal case
                builder.Append(char.ToLowerInvariant(name.First()));

                char last = name.First();
                for (int i = 1; i < name.Length; i++)
                {
                    // Exclude '_' characters and make the following characters uppercase
                    if (char.IsLower(name[i]) && last == '_')
                        builder.Append(char.ToUpperInvariant(name[i]));
                    else if (name[i] != '_')
                        builder.Append(name[i]);

                    last = name[i];
                }

                return builder.ToString();
            }
        }

        public List<string> Files { get; private set; }

        [DataMember(Name = "outputPath")]
        public string? OutputPath { get; set; }

        public bool? IncludeNewlines { get; set; }

        public ProjectFile()
        {
            Files = new List<string>();
            OutputPath = "build";
            IncludeNewlines = true;
        }

        public static ProjectFile? Parse(FileInfo file)
        {
            // This should be checked before but im including it here anyway
            if (file.Exists == false)
                throw new ArgumentException("Project file doesn't exist");

            // Parse the file
            TomlModelOptions options = new();
            options.ConvertPropertyName = TomlCamelCaseConverter.ToCamelCase;

            string fileContents = File.ReadAllText(file.FullName);
            if (!Toml.TryToModel(fileContents, out ProjectFile? model, out DiagnosticsBag? diagnostics, file.FullName, options) && diagnostics != null)
            {
                foreach (DiagnosticMessage diagnostic in diagnostics)
                    Console.Error.WriteLine(diagnostic);
            }

            // Get the actual list of files and not just the patterns
            if (model != null)
                model.Files = ExpandFileGlobs(model.Files);

            return model;
        }

        private static List<string> ExpandFileGlobs(List<string> files)
        {
            // Run the pattern matcher
            Matcher matcher = new();
            matcher.AddIncludePatterns(files);
            PatternMatchingResult result = matcher.Execute(new DirectoryInfoWrapper(new(Directory.GetCurrentDirectory())));

            // Get the paths from the pattern match structure
            List<string> matchedPaths = new List<string>(result.Files.Count());
            foreach (FilePatternMatch match in result.Files)
                matchedPaths.Add(match.Path);

            return matchedPaths;
        }
    }
}
