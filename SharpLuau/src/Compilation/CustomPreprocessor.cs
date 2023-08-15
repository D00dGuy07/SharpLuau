using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau.Compilation
{
    internal enum DirectiveKind
    {
        LuauSplice,
    }

    internal class PreprocessorDirective
    {
        public int Position { get; protected set; }
        public DirectiveKind Kind { get; protected set; }
    }

    internal sealed class LuauSpliceDirective : PreprocessorDirective
    {
        public string LuauCode { get; private set; }

        public LuauSpliceDirective(int position, StringReader reader)
        {
            Kind = DirectiveKind.LuauSplice;
            Position = position;

            StringBuilder luauCodeBuilder = new();

            // Loop through all the lines
            string? line;
            bool isFirstLine = true;
            while ((line = reader.ReadLine()) != null)
            {
                // Add line to the luau code unless it is another directive
                if (!line.StartsWith('#'))
                {
                    // Add a newline character unless it's the first line
                    if (!isFirstLine)
                        luauCodeBuilder.Append('\n');
                    else
                        isFirstLine = false;

                    luauCodeBuilder.Append(line);
                    continue;
                }
                string[] arguments = line[1..].Split(' ', StringSplitOptions.RemoveEmptyEntries);

                // break out of the loop when the endluau directive is found
                if (arguments[0] == "endluau" && arguments.Length == 1)
                    break;
            }

            LuauCode = luauCodeBuilder.ToString();
        }
    }

    internal class CustomPreprocessor
    {
        public static PreprocessorDirective[] PreprocessText(string text, out string strippedText)
        {
            StringBuilder strippedCode = new();
            List<PreprocessorDirective> directives = new();

            // Loop through the file line by line
            using (StringReader reader = new StringReader(text))
            {
                string? line;
                while ((line = reader.ReadLine()) != null)
                {
                    if (!line.StartsWith('#'))
                    {
                        strippedCode.AppendLine(line);
                        continue;
                    }

                    // Check the directive arguments for custom directives

                    string[] arguments = line[1..].Split(' ', StringSplitOptions.RemoveEmptyEntries);

                    if (arguments[0] == "luau" && arguments.Length == 1)
                    {
                        directives.Add(new LuauSpliceDirective(strippedCode.Length, reader));
                        continue;
                    }

                    // Add the line to the code because it isn't a custom one
                    strippedCode.AppendLine(line);
                }
            }

            // Return vales
            strippedText = strippedCode.ToString();
            return directives.ToArray();
        }
    }
}
