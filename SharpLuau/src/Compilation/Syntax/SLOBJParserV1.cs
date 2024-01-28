using SharpLuau.Compilation.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;

namespace SharpLuau.Compilation.Syntax
{
	/// <summary>
	/// This class is responsible for parsing the first version of the .slobj luau syntax format.
	/// If I decide to make a change to the format that isn't backwards compatible then I'll create a
	/// new class that can parse it.
	/// 
	/// My intent is that each version of this program can only output one version of this format but
	/// should be able to parse all previous versions.
	/// </summary>
	public static class SLOBJParserV1
	{
		public static LuauContext ParseContext(BinaryReader reader)
		{
			// Read all of the renamed symbols
			Dictionary<string, LuauIdentifier> renamedSymbols = new();
			int renamedSymbolCount = reader.ReadInt32();
			for (int i = 0; i < renamedSymbolCount; i++)
			{
				string symbolName = reader.ReadString();

				reader.ReadInt32(); // This one will actually always be a regular identifier
				renamedSymbols.Add(symbolName, ParseIdentifier(reader));
			}

			// Read all of the output file relations
			Dictionary<string, string> typeToOutputFile = new();
			int typeToOutputFileCount = reader.ReadInt32();
			for (int i = 0; i < typeToOutputFileCount; i++)
				typeToOutputFile.Add(reader.ReadString(), reader.ReadString());

			// Read all of the roblox path relations
			Dictionary<string, RobloxObjectPath> outputFileToRobloxPath = new();
			int outputFileToRobloxPathCount = reader.ReadInt32();
			for (int i = 0; i < outputFileToRobloxPathCount; i++)
			{
				string outputFilePath = reader.ReadString();

				string service = reader.ReadString();

				int pathLength = reader.ReadInt32();
				string[] path = new string[pathLength];
				for (int j = 0; j < pathLength; j++)
					path[j] = reader.ReadString();

				outputFileToRobloxPath.Add(outputFilePath, new(service, path));
			}

			return new(renamedSymbols, typeToOutputFile, outputFileToRobloxPath);
		}

		public static LuauBlock ParseCode(BinaryReader reader)
		{
			reader.ReadInt32(); // Skip the header
			return ParseBlock(reader);
		}

		internal enum StatementKindV1
		{
			BadStatement = 0,

			Block = 1,
			NewLine = 2,
			Comment = 3,
			SourceSplice = 4,
			VariableDeclaration = 5,
			GlobalFunctionDeclaration = 6,
			LocalFunctionDeclaration = 7,
			Return = 8,
			ExpressionStatement = 9,
			If = 10,
			Import = 11,

			// Assignment Statements
			SimpleAssignment = 101,
			AddAssignment = 102,
			SubtractAssignment = 103,
			MultiplyAssignment = 104,
			DivideAssignment = 105,
			ModuloAssignment = 106,
			ExponentAssignment = 107,
			ConcatenateAssignment = 108
		}

		internal enum ExpressionKindV1
		{
			BadExpression = 0,

			Ternary = 1,
			MemberAccess = 2,
			Invocation = 3,
			Identifier = 4,
			QualifiedIdentifier = 5,
			Lambda = 6,
			ParenthesizedExpression = 7,
			DynamicIdentifier = 8,
			TableItem = 9,

			// Literal Expressions
			NumericLiteral = 101,
			StringLiteral = 102,
			CharacterLiteral = 103,
			TrueLiteral = 104,
			FalseLiteral = 105,
			NilLiteral = 106,
			Table = 107,

			// Unary Expressions
			UnaryPlus = 201,
			UnaryMinus = 202,
			UnaryLogicalNot = 203,

			// Binary Expressions
			BinaryAdd = 301,
			BinarySubtract = 302,
			BinaryMultiply = 303,
			BinaryDivide = 304,
			BinaryModulo = 305,
			BinaryConcatenate = 306,
			BinaryLogicalOr = 307,
			BinaryLogicalAnd = 308,
			BinaryEquals = 309,
			BinaryNotEquals = 310,
			BinaryLessThan = 311,
			BinaryLessThanOrEqual = 312,
			BinaryGreaterThan = 313,
			BinaryGreaterThanOrEqual = 314,
		}

		private static LuauStatement ParseStatement(BinaryReader reader)
		{
			// Parse header
			StatementKindV1 kind = (StatementKindV1)reader.ReadInt32();

			switch (kind)
			{
				case StatementKindV1.BadStatement:
					return new LuauBadStatement();
				case StatementKindV1.Block:
					return ParseBlock(reader);
				case StatementKindV1.NewLine:
					return new LuauNewline();
				case StatementKindV1.Comment:
					return ParseComment(reader);
				case StatementKindV1.SourceSplice:
					return ParseSourceSplice(reader);
				case StatementKindV1.VariableDeclaration:
					return ParseVariableDeclaration(reader);
				case StatementKindV1.GlobalFunctionDeclaration:
					return ParseGlobalFunctionDeclaration(reader);
				case StatementKindV1.LocalFunctionDeclaration:
					return ParseLocalFunctionDeclaration(reader);
				case StatementKindV1.Return:
					return ParseReturnStatement(reader);
				case StatementKindV1.ExpressionStatement:
					return ParseExpressionStatement(reader);
				case StatementKindV1.If:
					return ParseIfStatement(reader);
				case StatementKindV1.Import:
					return ParseImportStatement(reader);
				case StatementKindV1.SimpleAssignment:
				case StatementKindV1.AddAssignment:
				case StatementKindV1.SubtractAssignment:
				case StatementKindV1.MultiplyAssignment:
				case StatementKindV1.DivideAssignment:
				case StatementKindV1.ModuloAssignment:
				case StatementKindV1.ExponentAssignment:
				case StatementKindV1.ConcatenateAssignment:
					return ParseAssignment(reader, kind);
				default:
					return new LuauBadStatement();
			}
		}

		private static LuauBlock ParseBlock(BinaryReader reader)
		{
			LuauBlock block = new();

			// I'm 99% sure that they are serialized in order
			int statementCount = reader.ReadInt32();
			for (int i = 0; i < statementCount; i++)
				block.AddLast(ParseStatement(reader));

			return block;
		}

		private static LuauComment ParseComment(BinaryReader reader)
		{
			return new LuauComment(reader.ReadString());
		}

		private static LuauSourceSplice ParseSourceSplice(BinaryReader reader)
		{
			return new LuauSourceSplice(reader.ReadString());
		}

		private static LuauVariableDeclaration ParseVariableDeclaration(BinaryReader reader)
		{
			LuauVariableDeclaration declaration = new();

			declaration.Identifier = ParseAnyIdentifier(reader);

			// This seems like a decent way to store and load optional values
			if (reader.ReadBoolean())
				declaration.Expression = ParseExpression(reader);

			return declaration;
		}

		private static LuauGlobalFunctionDeclaration ParseGlobalFunctionDeclaration(BinaryReader reader)
		{
			LuauGlobalFunctionDeclaration declaration = new();

			reader.ReadInt32(); // Skip Header
			declaration.Identifier = ParseQualifiedIdentifier(reader);

			int parameterCount = reader.ReadInt32();
			for (int i = 0; i < parameterCount; i++)
				declaration.ParameterList.Add(ParseAnyIdentifier(reader));

			reader.ReadInt32(); // Skip Header
			declaration.Body = ParseBlock(reader);

			return declaration;
		}

		private static LuauLocalFunctionDeclaration ParseLocalFunctionDeclaration(BinaryReader reader)
		{
			LuauLocalFunctionDeclaration declaration = new();

			reader.ReadInt32(); // Skip Header
			declaration.Identifier = ParseQualifiedIdentifier(reader);

			int parameterCount = reader.ReadInt32();
			for (int i = 0; i < parameterCount; i++)
				declaration.ParameterList.Add(ParseAnyIdentifier(reader));

			reader.ReadInt32(); // Skip Header
			declaration.Body = ParseBlock(reader);

			return declaration;
		}

		private static LuauReturnStatement ParseReturnStatement(BinaryReader reader)
		{
			LuauReturnStatement statement = new();

			if (reader.ReadBoolean())
				statement.Expression = ParseExpression(reader);

			return statement;
		}

		private static LuauExpressionStatement ParseExpressionStatement(BinaryReader reader)
		{
			return new LuauExpressionStatement() { Expression = ParseExpression(reader) };
		}

		private static LuauIfStatement ParseIfStatement(BinaryReader reader)
		{
			LuauIfStatement ifStatement = new();

			ifStatement.Condition = ParseExpression(reader);

			reader.ReadInt32(); // Skip Header
			ifStatement.Body = ParseBlock(reader);

			// The SetElse function verifies the type of statement
			// This ParseStatement call is recursive because it could be another if statement
			if (reader.ReadBoolean())
				ifStatement.SetElse(ParseStatement(reader));

			return ifStatement;
		}

		private static LuauImportStatement ParseImportStatement(BinaryReader reader)
		{
			List<string> symbols = new();

			int symbolCount = reader.ReadInt32();
			for (int i = 0; i < symbolCount; i++)
				symbols.Add(reader.ReadString());

			return new(symbols);
		}

		private static LuauAssignmentStatement ParseAssignment(BinaryReader reader, StatementKindV1 kind)
		{
			// This function won't check that the kind is a valid assignment kind
			// That check happens when creating the statement anyway

			return new LuauAssignmentStatement((StatementKind)kind)
			{
				Left = ParseExpression(reader),
				Right = ParseExpression(reader)
			};
		}

		private static LuauExpression ParseExpression(BinaryReader reader)
		{
			// Parse header
			ExpressionKindV1 kind = (ExpressionKindV1)reader.ReadInt32();

			switch (kind)
			{
				case ExpressionKindV1.BadExpression:
					return new LuauBadExpression();
				case ExpressionKindV1.Ternary:
					return ParseTernary(reader);
				case ExpressionKindV1.MemberAccess:
					return ParseMemberAccess(reader);
				case ExpressionKindV1.Invocation:
					return ParseInvocation(reader);
				case ExpressionKindV1.Identifier:
					return ParseIdentifier(reader);
				case ExpressionKindV1.DynamicIdentifier:
					return ParseDynamicIdentifier(reader);
				case ExpressionKindV1.TableItem:
					return ParseTableItem(reader);
				case ExpressionKindV1.QualifiedIdentifier:
					return ParseQualifiedIdentifier(reader);
				case ExpressionKindV1.Lambda:
					return ParseLambda(reader);
				case ExpressionKindV1.ParenthesizedExpression:
					return ParseParenthesizedExpression(reader);
				case ExpressionKindV1.NumericLiteral:
				case ExpressionKindV1.StringLiteral:
				case ExpressionKindV1.CharacterLiteral:
				case ExpressionKindV1.TrueLiteral:
				case ExpressionKindV1.FalseLiteral:
				case ExpressionKindV1.NilLiteral:
					return ParseLiteralExpression(reader, kind);
				case ExpressionKindV1.Table:
					return ParseTable(reader);
				case ExpressionKindV1.UnaryPlus:
				case ExpressionKindV1.UnaryMinus:
				case ExpressionKindV1.UnaryLogicalNot:
					return ParseUnaryExpression(reader, kind);
				case ExpressionKindV1.BinaryAdd:
				case ExpressionKindV1.BinarySubtract:
				case ExpressionKindV1.BinaryMultiply:
				case ExpressionKindV1.BinaryDivide:
				case ExpressionKindV1.BinaryModulo:
				case ExpressionKindV1.BinaryConcatenate:
				case ExpressionKindV1.BinaryLogicalOr:
				case ExpressionKindV1.BinaryLogicalAnd:
				case ExpressionKindV1.BinaryEquals:
				case ExpressionKindV1.BinaryNotEquals:
				case ExpressionKindV1.BinaryLessThan:
				case ExpressionKindV1.BinaryLessThanOrEqual:
				case ExpressionKindV1.BinaryGreaterThan:
				case ExpressionKindV1.BinaryGreaterThanOrEqual:
					return ParseBinaryExpression(reader, kind);
				default:
					return new LuauBadExpression();
			}
		}

		private static LuauIdentifier ParseAnyIdentifier(BinaryReader reader)
		{
			// Parse header
			ExpressionKindV1 kind = (ExpressionKindV1)reader.ReadInt32();

			// Parse the correct kind of identifer defaulting to the regular one
			switch (kind)
			{
				case ExpressionKindV1.DynamicIdentifier:
					return ParseDynamicIdentifier(reader);
				default:
					return ParseIdentifier(reader);
			}
		}

		private static LuauTernary ParseTernary(BinaryReader reader)
		{
			return new LuauTernary(
				ParseExpression(reader),
				ParseExpression(reader),
				ParseExpression(reader)
			);
		}

		private static LuauMemberAccess ParseMemberAccess(BinaryReader reader)
		{
			LuauExpression expression = ParseExpression(reader);
			LuauIdentifier member = ParseAnyIdentifier(reader);

			return new LuauMemberAccess(
				expression,
				member,
				reader.ReadBoolean()
			);
		}

		private static LuauInvocation ParseInvocation(BinaryReader reader)
		{
			LuauExpression identifier = ParseExpression(reader);
			List<LuauExpression> arguments = new();

			int argCount = reader.ReadInt32();
			for (int i = 0; i < argCount; i++)
				arguments.Add(ParseExpression(reader));

			return new LuauInvocation(identifier, arguments);
		}

		private static LuauIdentifier ParseIdentifier(BinaryReader reader)
		{
			return new LuauIdentifier(reader.ReadString());
		}

		private static LuauIdentifier ParseDynamicIdentifier(BinaryReader reader)
		{
			return new LuauDynamicIdentifier(reader.ReadString());
		}

		private static LuauQualifiedIdentifier ParseQualifiedIdentifier(BinaryReader reader)
		{
			List<LuauIdentifier> identifiers = new();

			int identifierCount = reader.ReadInt32();
			for (int i = 0; i < identifierCount; i++)
			{
				identifiers.Add(ParseExpression(reader) as LuauIdentifier ?? "<Bad Identifier>");
			}

			return new LuauQualifiedIdentifier(identifiers, reader.ReadBoolean());
		}

		private static LuauLambda ParseLambda(BinaryReader reader)
		{
			// The order of this function is a little wonky because of the logistics of parsing the body first
			// but needing the list first to create the lambda

			reader.ReadInt32(); // Skip header
			LuauBlock body = ParseBlock(reader);

			int parameterCount = reader.ReadInt32();
			List<LuauIdentifier> parameters = new();
			for (int i = 0; i < parameterCount; i++)
				parameters.Add(ParseAnyIdentifier(reader));

			return new LuauLambda(parameters)
			{
				Body = body
			};
		}

		private static LuauParenthesizedExpression ParseParenthesizedExpression(BinaryReader reader)
		{
			return new LuauParenthesizedExpression(ParseExpression(reader));
		}

		private static LuauLiteralExpression ParseLiteralExpression(BinaryReader reader, ExpressionKindV1 kind)
		{
			// This should catch all of the literals that are saved with text
			if (kind >= ExpressionKindV1.NumericLiteral && kind <= ExpressionKindV1.CharacterLiteral)
				return new LuauLiteralExpression((ExpressionKind)kind, reader.ReadString());
			return new LuauLiteralExpression((ExpressionKind)kind);
		}

		private static LuauTableItemExpression ParseTableItem(BinaryReader reader)
		{
			LuauExpression value = ParseExpression(reader);
			LuauExpression? key = null;

			if (reader.ReadBoolean())
				key = ParseExpression(reader);

			return new LuauTableItemExpression(value, key);
		}

		private static LuauTableExpression ParseTable(BinaryReader reader)
		{
			List<LuauTableItemExpression> contents = new();

			bool ShouldSpread = reader.ReadBoolean();

			int length = reader.ReadInt32();
			for (int i = 0; i < length; i++)
			{
				reader.ReadInt32(); // Skip Header
				contents.Add(ParseTableItem(reader));
			}

			return new LuauTableExpression(contents);
		}

		private static LuauUnaryExpression ParseUnaryExpression(BinaryReader reader, ExpressionKindV1 kind)
		{
			return new LuauUnaryExpression(
				(ExpressionKind)kind, 
				ParseExpression(reader)
			);
		}

		private static LuauBinaryExpression ParseBinaryExpression(BinaryReader reader, ExpressionKindV1 kind)
		{
			return new LuauBinaryExpression(
				(ExpressionKind)kind, 
				ParseExpression(reader), 
				ParseExpression(reader)
			);
		}
	}
}
