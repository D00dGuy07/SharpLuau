using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using System.Xml.Linq;

namespace SharpLuau.Compilation
{
	internal class RojoSettings
	{
		public string name { get; set; } = string.Empty;
		public RojoInstanceDescription tree { get; set; } = new RojoInstanceDescription();
		public uint? servePort { get; set; }
		public string[]? servePlaceIds { get; set; }
		public string? placeId { get; set; }
		public string? gameId { get; set; }
		public string? serveAddress { get; set; }
		public string[]? globIgnorePaths { get; set; }

		[JsonIgnore]
		public string FilePath = string.Empty;
	}

	internal class RojoInstanceDescription
	{
		[JsonPropertyName("$className")]
		public string? className { get; set; }

		[JsonPropertyName("$path")]
		public string? path { get; set; }

		[JsonPropertyName("$properties")]
		public JsonElement? properties { get; set; }

		[JsonPropertyName("$ignoreUnknownInstances")]
		public bool? ignoreUnknownInstances { get; set; }

		[JsonExtensionData]
		public Dictionary<string, JsonElement>? children { get; set; }
	}

	public class RobloxObjectPath
	{
		public readonly string Service = string.Empty;
		public readonly string[] Path = Array.Empty<string>();

		public RobloxObjectPath() {}

		public RobloxObjectPath(string service, string[] path)
		{
			Service = service;
			Path = path;
		}

		public string GetRequireLine()
		{
			StringBuilder output = new();

			if (Service == "StarterPlayer" && Path.First() == "StarterPlayerScripts")
			{
				output.Append($"require(game:GetService(\"Players\").LocalPlayer:FindFirstChild(\"PlayerScripts\")");
				for (int i = 1; i < Path.Length; i++)
					output.Append($":FindFirstChild(\"{Path[i]}\")");
				output.Append(")");

				return output.ToString();
			}

			output.Append($"require(game:GetService(\"{Service}\")");
			foreach (string objectName in Path)
				output.Append($":FindFirstChild(\"{objectName}\")");
			output.Append(")");

			return output.ToString();
		}
	}

	internal class RojoProjectReader
	{
		public static RojoSettings FindRojoSettings()
		{
			// Find the settings file
			string? settingsFile = null;
			foreach(string filePath in Directory.EnumerateFiles(Directory.GetCurrentDirectory()))
			{
				string name = Path.GetFileName(filePath) ?? string.Empty;
				if (name.EndsWith(".project.json")) // This name requirement is taken from the rojo wiki
					settingsFile = filePath;
			}

			if (settingsFile == null)
			{
				Console.WriteLine("Couldn't find a rojo settings file");
				throw new Exception();
			}

			// Non-challantly load and parse the file in one line kinda, but I promise that it started as one line
			RojoSettings? rojoSettings = JsonSerializer.Deserialize<RojoSettings>(File.ReadAllText(settingsFile)) ?? new();
			rojoSettings.FilePath = settingsFile;

			return rojoSettings;
		}

		public static Dictionary<string, RobloxObjectPath> GetPathAssociations(RojoSettings rojoSettings)
		{
			// I don't know exactly how it works when the root isn't 'DataModel', so I'm just requiring this
			if (rojoSettings.tree.className != "DataModel")
			{
				Console.WriteLine("Rojo tree root should be 'DataModel'");
				return new();
			}

			// Build the dictionary recursively
			Dictionary<string, RobloxObjectPath> associations = new();

			if (rojoSettings.tree.children == null) return associations;
			foreach (var pair in rojoSettings.tree.children)
			{
				// I would really prefer not to have to incrementally deserialize the nodes like this, but
				// I couldn't find a better way to do it with the built in JSON library
				RojoInstanceDescription? childNode = JsonSerializer.Deserialize<RojoInstanceDescription>(pair.Value);
				if (childNode == null) continue;

				GetPathAssociationsRecursive(associations, childNode, pair.Key, new List<string>());
			}

			return associations;
		}

		private static void GetPathAssociationsRecursive(Dictionary<string, RobloxObjectPath> associations,
			RojoInstanceDescription node, string service, List<string> path)
		{
			// Record associations
			if (node.path != null)
			{
				associations.Add(
					node.path, 
					new(service, path.ToArray())
				);
			}

			// Walk each child branch
			if (node.children == null) return;
			foreach (var pair in node.children)
			{
				// I would really prefer not to have to incrementally deserialize the nodes like this, but
				// I couldn't find a better way to do it with the built in JSON library
				RojoInstanceDescription? childNode = JsonSerializer.Deserialize<RojoInstanceDescription>(pair.Value);
				if (childNode == null) continue;

				List<string> localPath = new(path);
				localPath.Add(pair.Key);

				GetPathAssociationsRecursive(associations, childNode, service, localPath);
			}
		}
	}
}
