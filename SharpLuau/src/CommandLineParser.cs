using Microsoft.VisualBasic.FileIO;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.Serialization;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau
{


	[AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
	class OptionAttribute : Attribute
	{
		public string Name { get; private set; }
		public string Description { get; private set; }

		public OptionAttribute(string name, string description)
		{
			Name = name;
			Description = description;
		}

		public string OptionFlag() => "--" + Name;
	}

	[AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
	class ArgumentAttribute : Attribute
	{
		public string Name { get; private set; }
		public string Description { get; private set; }

		public ArgumentAttribute(string name, string description)
		{
			Name = name;
			Description = description;
		}
	}

	[AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
	class AliasAttribute : Attribute
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

	interface CommandImplementation
	{
		void OnRun(object globalOptions);
	}

	class CommandLineParser
	{
		private string m_AppName;

		private Type? m_GlobalOptions;
		private Dictionary<string, Type> m_CommandTable;
		private Dictionary<string, string> m_CommandDescriptions;

		public CommandLineParser(string appName)
		{
			m_AppName = appName;

			m_GlobalOptions = null;
			m_CommandTable = new();
			m_CommandDescriptions = new();
		}

		public void RegisterGlobalOptions<T>() where T : struct
		{
			m_GlobalOptions = typeof(T);
		}

		public void RegisterCommand<T>(string identifier, string description) where T : CommandImplementation
		{
			m_CommandTable.Add(identifier, typeof(T));
			m_CommandDescriptions.Add(identifier, description);
		}

		public void Parse(string[] args)
		{
			// Print help and return if the help flag is present
			if (args.Length == 0 || args[0] == "-h" || args[0] == "--help")
			{
				PrintGeneralHelp();
				return;
			}

			// Find the start of the command
			int commandStart = FindCommandIndex(args);
			if (commandStart == -1)
			{
				Console.Error.WriteLine("Error: You must specify a command");
				PrintGeneralHelp();
				return;
			}

			// Parse global args
			object? globalOptions = null;
			if (m_GlobalOptions != null && commandStart > 0)
			{
				string[] globalArgs = new string[commandStart];
				Array.Copy(args, 0, globalArgs, 0, commandStart);
				globalOptions = ParseStructure(globalArgs, m_GlobalOptions);
			}
			else if (m_GlobalOptions != null)
				globalOptions = Activator.CreateInstance(m_GlobalOptions);

            // Parse the command
            string[] commandArgs = new string[args.Length - commandStart];
			Array.Copy(args, commandStart, commandArgs, 0, args.Length - commandStart);

			// Print help and return if the help flag is present
			if (commandArgs.Contains("-h") || commandArgs.Contains("--help"))
			{
				PrintCommandHelp(commandArgs[0]);
				return;
			}

			Type commandType = m_CommandTable[commandArgs[0]];
			object? commandStructure = ParseCommandStructure(commandArgs, commandType);
			if (commandStructure != null) 
			{
				MethodInfo? method = commandType.GetMethod("OnRun", new Type[] { typeof(object) });

				try
				{
					method?.Invoke(commandStructure, new object?[] { globalOptions });
				} catch (TargetInvocationException e)
				{
					Console.WriteLine(e.InnerException);
				}
				
			}
		}

		private int FindCommandIndex(string[] args)
		{
			for (int i = 0; i < args.Length; i++)
			{
				if (m_CommandTable.ContainsKey(args[i]))
					return i;
			}

			return -1;
		}

		private object? ParseCommandStructure(string[] args, Type structureType)
		{
			// Parse the parts of the structure that are the same
			object? commandStructure = ParseStructure(args, structureType);
			if (commandStructure == null)
				return null;

			// Parse all of the arguments
			int argumentIndex = 1;
			foreach (FieldInfo field in structureType.GetFields())
			{
                ArgumentAttribute? argumentAttr = (ArgumentAttribute?)Attribute.GetCustomAttribute(field, typeof(ArgumentAttribute));
				if (argumentAttr == null) continue;

				// Check to see if there's even a value for this argument
				if (argumentIndex > args.Length - 1)
				{
					Console.Error.WriteLine("Error: Missing value for argument '{0}'", argumentAttr.Name);
					return null;
				}

				// Parse the argument
                if (!ParseArgument(args[argumentIndex], field.FieldType, out object value))
                    Console.Error.WriteLine("Error: Argument number {0}, '{1}', could not be parsed", argumentIndex, argumentAttr.Name);
                field.SetValue(commandStructure, value);

				argumentIndex++;
            }

			return commandStructure;
		}

		private object? ParseStructure(string[] args, Type structureType)
		{
			// Create an instance of the specified type
			object? structure = Activator.CreateInstance(structureType);
			if (structure == null)
			{
				Console.Error.WriteLine("Error: Failed to instantiate an instance of " + structureType.FullName + " while parsing commmand line arguments.");
				return null;
			}

			foreach (FieldInfo field in structureType.GetFields())
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
				{
					if (!ParseArgument(args[index + 1], field.FieldType, out object value))
						Console.Error.WriteLine("Error: Option '{0}' could not be parsed", optionAttr.OptionFlag());
					field.SetValue(structure, value);
				}
				else if (field.FieldType == typeof(bool))
					field.SetValue(structureType, true);
			}

			return structure;
		}
		private static bool ParseArgument(string argument, Type expectedType, out object value)
		{
			value = new();

			if (expectedType == typeof(string))
			{
				value = argument;
				return true;
			}
			else if (expectedType == typeof(bool))
			{
				if (!bool.TryParse(argument, out bool result))
					return false;
				value = result;
				return true;
			}
			else if (expectedType == typeof(int))
			{
				if (!int.TryParse(argument, out int result))
					return false;
				value = result;
				return true;
			}
			else if (expectedType == typeof(long))
			{
				if (!long.TryParse(argument, out long result))
					return false;
				value = result;
				return true;
			}
			else if (expectedType == typeof(double))
			{
				if (!double.TryParse(argument, out double result))
					return false;
				value = result;
				return true;
			}
			else if (expectedType == typeof(float))
			{
				if (float.TryParse(argument, out float result))
					return false;
				value = result;
				return true;
			}
			else if (expectedType == typeof(FileInfo))
			{
				try
				{
					value = new FileInfo(argument);
					return true;
				}
				catch (Exception) { }
			}
			else if (expectedType == typeof(DirectoryInfo))
			{
				try
				{
					value = new DirectoryInfo(argument);
					return true;
				}
				catch (Exception) { }
			}

			return false;
		}

		private void PrintGeneralHelp()
		{
			// Usage section
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("Usage:");

			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine($"    {m_AppName} [global options] <command> [command arguments] [command options]");

			// Global options section
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("\nGlobal Options:");

			List<string> identifiers = new();
			List<string> descriptions = new();

			identifiers.Add("    -h, --help");
			descriptions.Add("Display this help menu");

			int longestIdentifier = identifiers.First().Length;
			if (m_GlobalOptions != null)
			{
				foreach (FieldInfo field in m_GlobalOptions.GetFields())
				{
					// Get the option information
					OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
					if (optionAttr == null) continue;
					AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));

					// Build the option names string and update the longestIdentifer variable
					StringBuilder builder = new();
					builder.Append("    ");
					builder.Append(optionAttr.OptionFlag());

					if (aliasAttr != null)
					{
						foreach (string fullAlias in aliasAttr.FullAliases)
						{
							builder.Append(", --");
							builder.Append(fullAlias);
						}
						foreach (char prefixAlias in aliasAttr.PrefixAliases)
						{
							builder.Append(", -");
							builder.Append(prefixAlias);
						}
					}

					string nameString = builder.ToString();
					if (nameString.Length > longestIdentifier)
						longestIdentifier = nameString.Length;
					identifiers.Add(nameString);

					// Add the description string
					descriptions.Add(optionAttr.Description);
				}
			}

			// Print the options
			longestIdentifier += 4;
			for (int i = 0; i < identifiers.Count; i++)
			{
				string identifier = identifiers[i];
				string description = descriptions[i];

				// Print all of the commas in the identfier as white but everything else yellow
				Console.ForegroundColor = ConsoleColor.Yellow;
				foreach (char character in identifier)
				{
					if (character == ',')
					{
						Console.ForegroundColor = ConsoleColor.White;
						Console.Write(character);
						Console.ForegroundColor = ConsoleColor.Yellow;
						continue;
					}

					Console.Write(character);
				}

				// Print the description;
				Console.ForegroundColor = ConsoleColor.White;
				Console.CursorLeft = longestIdentifier;
				Console.WriteLine(description);
			}

			// Commands section
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("\nCommands:");

			identifiers.Clear();
			descriptions.Clear();

			longestIdentifier = 0;
			foreach(string commandName in m_CommandTable.Keys)
			{
				// Add the identifier
				string identifier = "    " + commandName;
				if (identifier.Length > longestIdentifier)
					longestIdentifier = identifier.Length;
				identifiers.Add(identifier);

				// Add the description
				descriptions.Add(m_CommandDescriptions[commandName]);
			}

			// Print the commands
			longestIdentifier += 4;
			for (int i = 0; i < identifiers.Count; i++)
			{
				string identifier = identifiers[i];
				string description = descriptions[i];

				// Print all of the commas in the identfier as white but everything else yellow
				Console.ForegroundColor = ConsoleColor.Yellow;
				Console.Write(identifier);

				// Print the description;
				Console.ForegroundColor = ConsoleColor.White;
				Console.CursorLeft = longestIdentifier;
				Console.Write(description);
				Console.Write('\n');
			}

			Console.Write('\n');
		}

		private void PrintCommandHelp(string commandName)
		{
			Type structureType = m_CommandTable[commandName];

            List<string> argumentNames = new();
            List<string> argumentDescriptions = new();
            int longestArgument = 0;

            List<string> optionNames = new();
            List<string> optionDescriptions = new();
            optionNames.Add("    -h, --help");
            optionDescriptions.Add("Display this help menu");
            int longestOption = optionNames.First().Length;

            foreach (FieldInfo field in structureType.GetFields())
			{
                // Get the argument information
                ArgumentAttribute? argumentAttr = (ArgumentAttribute?)Attribute.GetCustomAttribute(field, typeof(ArgumentAttribute));
                if (argumentAttr != null)
				{
					// Add the name
                    if (argumentAttr.Name.Length > longestArgument)
                        longestArgument = argumentAttr.Name.Length;
                    argumentNames.Add(argumentAttr.Name);

                    // Add the description
                    argumentDescriptions.Add(argumentAttr.Description);
                }

                // Get the option information
                OptionAttribute? optionAttr = (OptionAttribute?)Attribute.GetCustomAttribute(field, typeof(OptionAttribute));
                if (optionAttr != null)
                {
                    AliasAttribute? aliasAttr = (AliasAttribute?)Attribute.GetCustomAttribute(field, typeof(AliasAttribute));

                    // Build the option names string and update the longestIdentifer variable
                    StringBuilder builder = new();
                    builder.Append("    ");
                    builder.Append(optionAttr.OptionFlag());

                    if (aliasAttr != null)
                    {
                        foreach (string fullAlias in aliasAttr.FullAliases)
                        {
                            builder.Append(", --");
                            builder.Append(fullAlias);
                        }
                        foreach (char prefixAlias in aliasAttr.PrefixAliases)
                        {
                            builder.Append(", -");
                            builder.Append(prefixAlias);
                        }
                    }

                    string nameString = builder.ToString();
                    if (nameString.Length > longestOption)
                        longestOption = nameString.Length;
                    optionNames.Add(nameString);

                    // Add the description string
                    optionDescriptions.Add(optionAttr.Description);
                }
            }

            // Description
            Console.WriteLine(m_CommandDescriptions[commandName]);

			// Usage
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("\nUsage:");

			Console.ForegroundColor = ConsoleColor.White;
			Console.Write("    ");
			Console.Write(m_AppName);
			Console.Write(" [Global Options] ");
			Console.Write(commandName);

            foreach (string name in argumentNames)
			{
				Console.Write(" [");
				Console.Write(name);
				Console.Write(']');
			}

			Console.Write(" [Command Options]\n");

			// Args
			if (argumentNames.Count > 0)
			{
                Console.ForegroundColor = ConsoleColor.Green;
                Console.WriteLine("\nArgs:");

                longestArgument += 8;
                for (int i = 0; i < argumentNames.Count; i++)
                {
                    string identifier = "    " + argumentNames[i];
                    string description = argumentDescriptions[i];

                    // Print all of the commas in the identfier as white but everything else yellow
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.Write(identifier);

                    // Print the description;
                    Console.ForegroundColor = ConsoleColor.White;
                    Console.CursorLeft = longestArgument;
                    Console.WriteLine(description);
                }
            }

            // Options
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine("\nOptions:");

            longestOption += 4;
            for (int i = 0; i < optionNames.Count; i++)
            {
                string identifier = optionNames[i];
                string description = optionDescriptions[i];

                // Print all of the commas in the identfier as white but everything else yellow
                Console.ForegroundColor = ConsoleColor.Yellow;
                foreach (char character in identifier)
                {
                    if (character == ',')
                    {
                        Console.ForegroundColor = ConsoleColor.White;
                        Console.Write(character);
                        Console.ForegroundColor = ConsoleColor.Yellow;
                        continue;
                    }

                    Console.Write(character);
                }

                // Print the description;
                Console.ForegroundColor = ConsoleColor.White;
                Console.CursorLeft = longestOption;
                Console.WriteLine(description);
            }
        }
    }
}