using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau
{
	[AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
	internal class OptionAttribute : Attribute
	{
		public string Name { get; private set; }
		public string Description { get; private set; }

		public OptionAttribute(string name, string description)
		{
			Name = name;
			Description = description;
		}

		public object? GetValue(string argument, Type fieldType)
		{
			if (fieldType == typeof(string))
				return argument;
			else if (fieldType == typeof(bool))
			{
				if (!bool.TryParse(argument, out bool value))
					Console.WriteLine("Option {0} must precede a boolean value, not '{1}'", OptionFlag(), argument);
				return value;
			}
			else if (fieldType == typeof(int))
			{
				if (!int.TryParse(argument, out int value))
					Console.WriteLine("Option {0} must precede an integer value, not '{1}'", OptionFlag(), argument);
				return value;
			}
			else if (fieldType == typeof(long))
			{
				if (!long.TryParse(argument, out long value))
					Console.WriteLine("Option {0} must precede an integer value, not '{1}'", OptionFlag(), argument);
				return value;
			}
			else if (fieldType == typeof(double))
			{
				if (!double.TryParse(argument, out double value))
					Console.WriteLine("Option {0} must precede a double value, not '{1}'", OptionFlag(), argument);
				return value;
			}
			else if (fieldType == typeof(float))
			{
				if (!float.TryParse(argument, out float value))
					Console.WriteLine("Option {0} must precede a float value, not '{1}'", OptionFlag(), argument);
				return value;
			}

			return null;
		}

		public string OptionFlag() => "--" + Name;
	}

	[AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
	internal class AliasAttribute : Attribute
	{
		public List<string> FullAliases { get; private set; }
		public List<char> PrefixAliases { get; private set; }

		public AliasAttribute(params string[] aliases)
		{
			FullAliases = new();
			PrefixAliases = new();

			// Separate the full aliases from the single letter aliases
			foreach(var alias in aliases)
			{
				if (alias.Length == 1)
					PrefixAliases.Add(alias.First());
				else
					FullAliases.Add(alias);
			}
		}
	}

	internal class CommandLineParser<T> where T : struct
	{
		protected string m_AppName;

		public CommandLineParser(string appName)
		{
			m_AppName = appName;
		}

		public T Parse(string[] args)
		{
			object parsedStructure = new T();

			if (args.Contains("-h") || args.Contains("--help"))
			{
				PrintHelp();
				return (T)parsedStructure;
			}

			foreach (FieldInfo field in typeof(T).GetFields())
			{
				OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
				if (optionAttr == null) continue;

				// Get an index for the flag
				int index = Array.IndexOf(args, optionAttr.OptionFlag());

				// Check for aliases
				if (index == -1)
				{
					AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));
					if (aliasAttr == null) continue; // There is nothing to add

					// Check the prefix alias list
					if (aliasAttr.PrefixAliases.Count > 0)
						for (int i = 0; i < aliasAttr.PrefixAliases.Count && index == -1; i++)
							index = Array.IndexOf(args, "-" + aliasAttr.PrefixAliases[i]);

					// Check the full alias list
					if (aliasAttr.PrefixAliases.Count > 0)
						for (int i = 0; i < aliasAttr.FullAliases.Count && index == -1; i++)
							index = Array.IndexOf(args, "--" + aliasAttr.FullAliases[i]);
				}

				// Skip this option if there's no value present
				if (index == -1)
					continue;

				// Parse the argument and set the value in the struct
				if (args.Length > index + 1 && !args[index + 1].StartsWith('-'))
					field.SetValue(parsedStructure, optionAttr.GetValue(args[index + 1], field.FieldType));
				else if (field.FieldType == typeof(bool))
					field.SetValue(parsedStructure, true);
			}

			return (T)parsedStructure;
		}

		private void PrintHelp()
		{
			// Print the top of the help section
			Console.Write("\nUsage: {0} [options]\n\nOptions:\n\t--help, -h : Display Help\n", m_AppName);

			// Print each option's information
			foreach (FieldInfo field in typeof(T).GetFields())
			{
				// Get the option information
				OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
				if (optionAttr == null) continue;
				AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));

				// Print the default name
				Console.Write("\t" + optionAttr.OptionFlag());

				// Print the aliases
				if (aliasAttr != null)
				{
					foreach (string fullAlias in aliasAttr.FullAliases)
						Console.Write(", --" + fullAlias);
					foreach (char prefixAlias in aliasAttr.PrefixAliases)
						Console.Write(", -" + prefixAlias);
				}

				// Print the description
				Console.Write(" : {0}\n", optionAttr.Description);
			}

			Console.Write('\n');
		}
	}

	internal class TrailingFileParser<T>  where T : struct
	{
		protected string m_AppName;

		public TrailingFileParser(string appName)
		{
			m_AppName = appName;
		}

		public T Parse(string[] args, out FileInfo[] trailingFiles)
		{
			// Check for the help option
            if (args.Contains("-h") || args.Contains("--help"))
            {
                PrintHelp();

				// Return nothing
				trailingFiles = Array.Empty<FileInfo>();
                return new T();
            }

			// Get the list of trailing file infos
            List<FileInfo> fileInfos = new();

			// Get the index of the last option
			int startIndex = args.Length - 1;
			while (startIndex >= 0 && !args[startIndex].StartsWith('-'))
				startIndex--;

			// Find the starting index of the file list
			if (startIndex != args.Length - 1 && startIndex != -1)
			{
				// Get the type of field associated with this option
				FieldInfo? fieldInfo = MatchOptionToField(args[startIndex]);

				if (fieldInfo != null && fieldInfo.FieldType == typeof(bool))
				{
					// Check for the special boolean case of no option following the flag
					if (!bool.TryParse(args[startIndex + 1], out bool _))
						startIndex++;
					else
						startIndex += 2;
				}
				else
					startIndex += 2;
			}

            // Get the fileinfo for each file
            for (int i = startIndex == -1 ? 0 : startIndex; i < args.Length; i++)
                if (!args[i].EndsWith('\\'))
                    fileInfos.Add(new FileInfo(args[i]));
            trailingFiles = fileInfos.ToArray();

			// Parse the other options
            object parsedStructure = new T();

            foreach (FieldInfo field in typeof(T).GetFields())
            {
                OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
                if (optionAttr == null) continue;

                // Get an index for the flag
                int index = Array.IndexOf(args, optionAttr.OptionFlag());

                // Check for aliases
                if (index == -1)
                {
                    AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));
                    if (aliasAttr == null) continue; // There is nothing to add

                    // Check the prefix alias list
                    if (aliasAttr.PrefixAliases.Count > 0)
                        for (int i = 0; i < aliasAttr.PrefixAliases.Count && index == -1; i++)
                            index = Array.IndexOf(args, "-" + aliasAttr.PrefixAliases[i]);

                    // Check the full alias list
                    if (aliasAttr.PrefixAliases.Count > 0)
                        for (int i = 0; i < aliasAttr.FullAliases.Count && index == -1; i++)
                            index = Array.IndexOf(args, "--" + aliasAttr.FullAliases[i]);
                }

                // Skip this option if there's no value present
                if (index == -1)
                    continue;

                // Parse the argument and set the value in the struct
                if (args.Length > index + 1 && !args[index + 1].StartsWith('-'))
                    field.SetValue(parsedStructure, optionAttr.GetValue(args[index + 1], field.FieldType));
                else if (field.FieldType == typeof(bool))
                    field.SetValue(parsedStructure, true);
            }

            return (T)parsedStructure;
        }

        private void PrintHelp()
        {
            // Print the top of the help section
            Console.Write("\nUsage: {0} [options] files\n\nOptions:\n\t--help, -h : Display Help\n", m_AppName);

            // Print each option's information
            foreach (FieldInfo field in typeof(T).GetFields())
            {
                // Get the option information
                OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
                if (optionAttr == null) continue;
                AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));

                // Print the default name
                Console.Write("\t" + optionAttr.OptionFlag());

                // Print the aliases
                if (aliasAttr != null)
                {
                    foreach (string fullAlias in aliasAttr.FullAliases)
                        Console.Write(", --" + fullAlias);
                    foreach (char prefixAlias in aliasAttr.PrefixAliases)
                        Console.Write(", -" + prefixAlias);
                }

                // Print the description
                Console.Write(" : {0}\n", optionAttr.Description);
            }

            Console.Write('\n');
        }

        private FieldInfo? MatchOptionToField(string option)
		{
            foreach (FieldInfo field in typeof(T).GetFields())
            {
                // Get the option attibutes
                OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
                if (optionAttr == null) continue;
                AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));

				// Check if this option uses the main name
				if (option == optionAttr.OptionFlag())
					return null;

                // Check if it uses an alias
                if (aliasAttr != null)
                {
                    foreach (string fullAlias in aliasAttr.FullAliases)
						if (option.Equals("--" + fullAlias))
							return field;

                    foreach (char prefixAlias in aliasAttr.PrefixAliases)
                        if (option.Equals('-' + prefixAlias))
                            return field;
                }
            }

			return null;
        }
	}
}
