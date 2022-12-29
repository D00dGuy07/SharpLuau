using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau
{
    internal static class StringBuilderExtensions
    {
        public static StringBuilder AppendIndented(this StringBuilder stringBuilder, string text, string indentation)
        {
            // For each line, append the indentation and the line to the StringBuilder
            using (StringReader reader = new StringReader(text))
            {
                string? line;
                while ((line = reader.ReadLine()) != null)
                {
                    stringBuilder.Append(indentation);
                    stringBuilder.AppendLine(line);
                }
            }

            return stringBuilder;
        }

        public static StringBuilder AppendIndented(this StringBuilder stringBuilder, string text, char indentation)
        {
            // For each line, append the indentation and the line to the StringBuilder
            using (StringReader reader = new StringReader(text))
            {
                string? line;
                while ((line = reader.ReadLine()) != null)
                {
                    stringBuilder.Append(indentation);
                    stringBuilder.AppendLine(line);
                }
            }

            return stringBuilder;
        }
    }
}
