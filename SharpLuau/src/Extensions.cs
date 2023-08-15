using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SharpLuau.Compilation.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau
{
    internal static class Extensions
    {
        // StringBuilder

        public static StringBuilder AppendIndented(this StringBuilder stringBuilder, string? text, string indentation)
        {
            // Make sure text isn't null
            text ??= string.Empty;

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

        public static StringBuilder AppendIndented(this StringBuilder stringBuilder, string? text, char indentation)
        {
            // Make sure text isn't null
            text ??= string.Empty;

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

        // SyntaxNode
        
        public static T? FirstAncestorOfType<T>(this SyntaxNode node) where T : SyntaxNode
        {
            SyntaxNode? currentNode = node.Parent;
            while (currentNode != null)
            {
                if (currentNode is T)
                    break;
                currentNode = currentNode.Parent;
            }
            
            return (T?)currentNode;
        }
    }
}
