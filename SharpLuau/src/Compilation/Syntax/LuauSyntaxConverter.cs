using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;
using System.Numerics;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using SharpLuau.Compilation;

namespace SharpLuau.Compilation.Syntax
{
	/// <summary>
	/// This class stores the output of transpilation for each individual file
	/// </summary>
	internal struct FileConversionData
	{
		/// <summary>
		/// The tree of resulting Luau code
		/// </summary>
		public LuauBlock ConvertedCode;

		/// <summary>
		/// The compilation context for this file
		/// </summary>
		public LuauContext CompilationContext;

		public FileConversionData()
		{
			ConvertedCode = new();
			CompilationContext = new();
		}
	}

	internal class LuauSyntaxConverter
	{
		private SemanticModel? m_SemanticModel;


		private FileConversionData m_FileConversionData;
		private LuauContext m_Context { get { return m_FileConversionData.CompilationContext; } }

		private struct FileConversionState
		{
			// Global information lookup tables
			public Dictionary<INamedTypeSymbol, Dictionary<string, int>> OverloadCounters;
			public Dictionary<LuauBlock, int> AssignedTempValues;

			// Directive tables
			public Dictionary<SyntaxNode, LuauSpliceDirective> SpliceLocations;

			public FileConversionState()
			{
				OverloadCounters = new(SymbolEqualityComparer.Default);
				AssignedTempValues = new();
				SpliceLocations = new();
			}
		}
		private FileConversionState m_State;
		

		// Options
		public bool IncludeNewlines = true;

		public LuauSyntaxConverter()
		{
			m_FileConversionData = new();
			m_State = new();
		}

		// Preprocessor directive handling

		private void ProcessDirectives(PreprocessorDirective[] directives, CompilationUnitSyntax root)
		{
			// Clear necessary state
			m_State.SpliceLocations.Clear();

			// Setup processing for each directive
			foreach (PreprocessorDirective directive in directives)
			{
				switch (directive.Kind)
				{
					case DirectiveKind.LuauSplice:
						{
							// Find the node add it to the list of locations along the directive
							var node = root.FindNode(new(directive.Position, 0));
							if (node != null) m_State.SpliceLocations.Add(node, (LuauSpliceDirective)directive);
							break;
						}
					default:
						continue;
				}
			}
		}

		private void HandleLuauSplice(LuauSpliceDirective directive, LuauBlock block)
		{
			LuauSourceSplice sourceSplice = new(directive.LuauCode);
			block.AddLast(sourceSplice);
		}

		// Error handling helpers
		private IMethodSymbol GetMethodSymbol(BaseMethodDeclarationSyntax methodDecl)
		{
			// Get the Symbol
			IMethodSymbol? symbol = m_SemanticModel.GetDeclaredSymbol(methodDecl);

			// Report any errors and return
			if (symbol == null)
				throw new ArgumentOutOfRangeException("Couldn't get the symbol for the method"
					+ " (LineSpan " + methodDecl.GetLocation().GetLineSpan() + ")");
			return symbol;
		}


		// Statement Syntax Conversions

		public FileConversionData ConvertFile(SyntaxTree tree, PreprocessorDirective[] directives, SemanticModel model)
		{
			// Reset state
			m_FileConversionData = new();
			m_State = new();
			m_SemanticModel = model;

			// Gather additional information about the file
			var root = tree.GetCompilationUnitRoot();
			ProcessDirectives(directives, root);

			// Convert each namespace
			foreach (var namespaceDecl in root.Members.OfType<NamespaceDeclarationSyntax>())
			{
				// Convert all of the members
				foreach (var member in namespaceDecl.Members)
				{
					switch (member.Kind())
					{
						case SyntaxKind.ClassDeclaration:
							ConvertClass((ClassDeclarationSyntax)member, m_FileConversionData.ConvertedCode);
							break;
						default:
							throw new ArgumentException("Undefined Declaration Conversion: {0}", member.Kind().ToString()
								+ " (LineSpan " + member.GetLocation().GetLineSpan() + ")");
					}
				}
			}

			return m_FileConversionData;
		}

		private void ConvertClass(ClassDeclarationSyntax classDecl, LuauBlock block)
		{
			// Get the class symbol and register its name
			INamedTypeSymbol? classSymbol = m_SemanticModel.GetDeclaredSymbol(classDecl);
			if (classSymbol == null)
				return;

			m_Context.AssignSymbolName(classSymbol, classSymbol.Name);
			LuauDynamicIdentifier className = new(classSymbol);

			// Add class header
			{
				// local className = {}
				LuauVariableDeclaration tableAssignment = new();
				block.AddLast(tableAssignment);

				tableAssignment.Identifier = className;
				tableAssignment.Expression = new LuauTableExpression();

				// className.__index = className
				LuauAssignmentStatement indexAssignment = new(StatementKind.SimpleAssignment);
				block.AddLast(indexAssignment);

				indexAssignment.Left = new LuauQualifiedIdentifier(
					new List<LuauIdentifier> { className, "__index" }, false);
				indexAssignment.Right = className;

				// Newline
				if (IncludeNewlines)
					block.AddLast(new LuauNewline());
			}

			// Initializing constructor
			AddInitializingConstructor(classDecl, block);

			// Add all of the defined constructors
			int constructorCount = 0;
			foreach (ConstructorDeclarationSyntax ctorDecl in classDecl.Members.OfType<ConstructorDeclarationSyntax>())
			{
				AddConstructor(ctorDecl, constructorCount, block);
				constructorCount++;
			}

			// Initialize method overload counters for this type
			InitMethodOverloadCounters(classSymbol);

			// Add the methods
			foreach (var methodDecl in classDecl.Members.OfType<MethodDeclarationSyntax>())
				ConvertMethod(methodDecl, block);

			// Release method overload counters for this type
			ReleaseMethodOverloadCounters(classSymbol);
		}

		private void AddConstructor(ConstructorDeclarationSyntax ctorDecl, int ctorNumber, LuauBlock block)
		{
			// Filter out pesky expression bodied constructors
			if (ctorDecl.ExpressionBody != null) 
				throw new ArgumentException("Expression bodied constructors are not supported"
					+ " (LineSpan " + ctorDecl.GetLocation().GetLineSpan() + ")");

			// Get the symbol for the constructor
			IMethodSymbol symbol = GetMethodSymbol(ctorDecl);

			// Register its new name
			string ctorName = "ctor" + ctorNumber;
			if (symbol.DeclaredAccessibility != Accessibility.Public)
				ctorName = '_' + ctorName;
			m_Context.AssignSymbolName(symbol, ctorName);

			// Get dynamic identifiers for the constructor and containing type
			LuauDynamicIdentifier ctorDynamicName = new(symbol);
			LuauDynamicIdentifier typeDynamicName = new(symbol.ContainingType);

			// Create the luau function
			LuauGlobalFunctionDeclaration luauFunction = new();
			block.AddLast(luauFunction);

			LuauBlock bodyBlock = new();
			luauFunction.Body = bodyBlock;

			luauFunction.Identifier =
				new LuauQualifiedIdentifier(new List<LuauIdentifier> { typeDynamicName, ctorDynamicName }, false);

			luauFunction.ParameterList = GetParameterList(ctorDecl.ParameterList, bodyBlock);

			// Call the initializing constructor
			LuauVariableDeclaration selfDeclaration = new();
			bodyBlock.AddLast(selfDeclaration);

			selfDeclaration.Identifier = "self";
			selfDeclaration.Expression = new LuauInvocation(
				new LuauQualifiedIdentifier(new List<LuauIdentifier> { typeDynamicName, "_initCtor" }, false),
				null
			);

			if (IncludeNewlines)
				bodyBlock.AddLast(new LuauNewline());

			// Fill the body
			FillFunctionBody(ctorDecl, ref bodyBlock);

			if (IncludeNewlines)
				bodyBlock.AddLast(new LuauNewline());

			// return self
			LuauReturnStatement returnStatement = new();
			bodyBlock.AddLast(returnStatement);

			returnStatement.Expression = new LuauIdentifier("self");

			// Newline
			if (IncludeNewlines)
				block.AddLast(new LuauNewline());
		}

		private void AddInitializingConstructor(ClassDeclarationSyntax classDecl, LuauBlock block)
		{
			LuauGlobalFunctionDeclaration luauFunction = new();
			block.AddLast(luauFunction);

			// Get a dynamic identifier for this class
			INamedTypeSymbol? classSymbol = m_SemanticModel.GetDeclaredSymbol(classDecl);
			if (classSymbol == null) return;

			LuauDynamicIdentifier className = new(classSymbol);

			luauFunction.Identifier = new LuauQualifiedIdentifier(new List<LuauIdentifier> { className, "_initCtor" }, false);
			luauFunction.Body = new();

			// Constructor header
			{
				// local self = {}
				LuauVariableDeclaration selfDeclaration = new();
				luauFunction.Body.AddLast(selfDeclaration);
				selfDeclaration.Identifier = "self";
				selfDeclaration.Expression = new LuauTableExpression();

				// setmetatable(self, className)
				LuauExpressionStatement setMetatable = new();
				luauFunction.Body.AddLast(setMetatable);
				setMetatable.Expression = new LuauInvocation(new LuauIdentifier("setmetatable"),
					new List<LuauExpression> { new LuauIdentifier("self"), className });

				// Newline
				if (IncludeNewlines)
					luauFunction.Body.AddLast(new LuauNewline());
			}

			// Field declarations
			foreach (var fieldDecl in classDecl.Members.OfType<FieldDeclarationSyntax>())
			{
				foreach (VariableDeclaratorSyntax fieldDeclarator in fieldDecl.Declaration.Variables)
				{
					// Register the variable symbol
					IFieldSymbol? symbol = (IFieldSymbol?)m_SemanticModel.GetDeclaredSymbol(fieldDeclarator);
					if (symbol == null) continue;
					m_Context.AssignSymbolName(symbol, symbol.Name);

					LuauAssignmentStatement luauAssignment = new(StatementKind.SimpleAssignment);
					luauFunction.Body.AddLast(luauAssignment);

					luauAssignment.Left = new LuauQualifiedIdentifier(new List<LuauIdentifier> { "self", new LuauDynamicIdentifier(symbol) }, false);

					// Give it a default value or its declared values
					if (fieldDeclarator.Initializer == null)
						luauAssignment.Right = GetTypeUninitializedValue(symbol.Type);
					else
						luauAssignment.Right = ConvertExpression(fieldDeclarator.Initializer.Value, luauAssignment);
				}
			}

			// Newline
			if (IncludeNewlines)
				luauFunction.Body.AddLast(new LuauNewline());

			// Constructor footer
			{
				// return self
				LuauReturnStatement returnStatement = new();
				luauFunction.Body.AddLast(returnStatement);
				returnStatement.Expression = new LuauIdentifier("self");
			}

			// Newline
			if (IncludeNewlines)
				block.AddLast(new LuauNewline());
		}

		private static LuauExpression GetTypeUninitializedValue(ITypeSymbol type)
		{
			switch (type.SpecialType)
			{
				case SpecialType.System_Boolean:
					return new LuauLiteralExpression(ExpressionKind.FalseLiteral);
				case SpecialType.System_String:
					return new LuauLiteralExpression(ExpressionKind.StringLiteral, "");
				case SpecialType.System_Char:
					return new LuauLiteralExpression(ExpressionKind.CharacterLiteral, "");
				case SpecialType.System_Decimal:
				case SpecialType.System_Single:
				case SpecialType.System_Double:
					return new LuauLiteralExpression(ExpressionKind.NumericLiteral, "0.0");
				case SpecialType.System_SByte:
				case SpecialType.System_Byte:
				case SpecialType.System_Int16:
				case SpecialType.System_Int32:
				case SpecialType.System_Int64:
				case SpecialType.System_UInt16:
				case SpecialType.System_UInt32:
				case SpecialType.System_UInt64:
					return new LuauLiteralExpression(ExpressionKind.NumericLiteral, "0");
				default:
					return new LuauLiteralExpression(ExpressionKind.NilLiteral);
			}
		}

		private void ConvertMethod(MethodDeclarationSyntax methodDecl, LuauBlock block)
		{
			LuauGlobalFunctionDeclaration luauFunction = new();
			block.AddLast(luauFunction);

			// Get the method symbol
			IMethodSymbol methodSymbol = GetMethodSymbol(methodDecl);

			// Get the overload count
			int methodOverloadCount = IncrementOverloadCount(methodSymbol.ContainingType, methodSymbol.Name);

			// Add an underscore to the beginning of the name if it's not a public function
			string methodName = methodSymbol.Name + methodOverloadCount;
			if (methodSymbol.DeclaredAccessibility != Accessibility.Public)
				methodName = '_' + methodName;
			m_Context.AssignSymbolName(methodSymbol, methodName);

			// Create the qualified identifier
			LuauQualifiedIdentifier identifier = new(
				new List<LuauIdentifier> { 
					new LuauDynamicIdentifier(methodSymbol.ContainingType), 
					new LuauDynamicIdentifier(methodSymbol) 
				},
				!methodSymbol.IsStatic
			);

			// Set the function values
			LuauBlock bodyBlock = new();
			luauFunction.Body = bodyBlock;

			luauFunction.Identifier = identifier;
			luauFunction.ParameterList = GetParameterList(methodDecl.ParameterList, bodyBlock);

			// Fill the function's body
			FillFunctionBody(methodDecl, ref bodyBlock);

			// Newline
			if (IncludeNewlines)
				block.AddLast(new LuauNewline());
		}

		private void FillFunctionBody(BaseMethodDeclarationSyntax methodDecl, ref LuauBlock bodyBlock)
		{
			// Fill the body with its statements
			if (methodDecl.Body != null)
				ConvertBlockSyntax(methodDecl.Body, ref bodyBlock);
			else if (methodDecl.ExpressionBody != null)
			{
				// The arrow expression clause thing is just a return statement
				LuauReturnStatement returnStatement = new();
				bodyBlock.AddLast(returnStatement);

				returnStatement.Expression = ConvertExpression(methodDecl.ExpressionBody.Expression, returnStatement);
			}

			// Release any temporary identifiers
			ReleaseTempIdentifers(bodyBlock);
		}

		private void ConvertVariableDeclarator(VariableDeclaratorSyntax variableDeclarator, LuauBlock block)
		{
			LuauVariableDeclaration luauDeclaration = new();
			block.AddLast(luauDeclaration);

			// Get the identifier symbol and register its name
			ISymbol? identifierSymbol = m_SemanticModel.GetDeclaredSymbol(variableDeclarator);
			if (identifierSymbol == null) return;
			m_Context.AssignSymbolName(identifierSymbol, identifierSymbol.Name);

			// Add the identifier and the initializer if there is one
			luauDeclaration.Identifier = new LuauDynamicIdentifier(identifierSymbol);
			if (variableDeclarator.Initializer != null)
				luauDeclaration.Expression = ConvertExpression(variableDeclarator.Initializer.Value, luauDeclaration);
		}

		private void ConvertVariableDeclaration(VariableDeclarationSyntax variableDecl, LuauBlock block)
		{
			// I've heard that in Luau it is more efficient to do the declarations separately
			// I don't know how accurate that is but it's why I'm doing it that way
			foreach (var declaration in variableDecl.Variables)
			{
				if (declaration != null)
					ConvertVariableDeclarator(declaration, block);
			}
		}

		private void ConvertBlockSyntax(BlockSyntax block, ref LuauBlock luauBlock)
		{
			// Add all of the statements
			foreach (var statement in block.Statements)
				ConvertAndAddStatement(statement, luauBlock);

			// Check if there's a luau splice right before the end of this block
			if (m_State.SpliceLocations.ContainsKey(block))
				HandleLuauSplice(m_State.SpliceLocations[block], luauBlock);
		}

		private void ConvertLocalDeclarationStatement(LocalDeclarationStatementSyntax statement, LuauBlock block)
			=> ConvertVariableDeclaration(statement.Declaration, block);

		private void ConvertReturnStatementSyntax(ReturnStatementSyntax statement, LuauBlock block)
		{
			LuauReturnStatement luauStatement = new();
			block.AddLast(luauStatement);

			// If the statement has a return value then add it
			if (statement.Expression != null)
				luauStatement.Expression = ConvertExpression(statement.Expression, luauStatement);
		}

		private void ConvertIfStatementSyntax(IfStatementSyntax statement, LuauBlock block)
		{
			LuauIfStatement luauStatement = new();
			block.AddLast(luauStatement);

			// Add the condition and main body of the if statement
			luauStatement.Condition = ConvertExpression(statement.Condition, luauStatement);

			LuauBlock bodyBlock = new();
			luauStatement.Body = bodyBlock;
			WrapBlock(statement.Statement, ref bodyBlock);
			ReleaseTempIdentifers(bodyBlock);

			if (statement.Else != null)
			{
				// Add all of the elseif and or else clauses
				// This algorithm is so much simpler when it uses recursion but that doesn't work
				// with how statements are separated from expressions in my logic
				StatementSyntax? cSharpClause = statement.Else.Statement;
				LuauIfStatement luauIfClause = luauStatement;
				while (cSharpClause != null)
				{
					if (cSharpClause is IfStatementSyntax)
					{
						// Get a casted version of the clause for readability
						IfStatementSyntax cSharpIfClause = (IfStatementSyntax)cSharpClause;

						// Create the else if clause statement
						LuauIfStatement newLuauIfClause = new();
						newLuauIfClause.Condition = ConvertExpression(cSharpIfClause.Condition, luauStatement);

						LuauBlock elseIfBlock = new();
						newLuauIfClause.Body = elseIfBlock;
						WrapBlock(cSharpIfClause.Statement, ref elseIfBlock);
						ReleaseTempIdentifers(elseIfBlock);


						// Set the last if clause statement's else property to the new clause
						luauIfClause.SetElse(newLuauIfClause);

						// Go deeper into the if nest
						luauIfClause = newLuauIfClause;
						cSharpClause = cSharpIfClause.Else != null ? cSharpIfClause.Else.Statement : null;
					}
					else
					{
						LuauBlock elseBlock = new();
						luauIfClause.SetElse(elseBlock);
						WrapBlock(cSharpClause, ref elseBlock);
						ReleaseTempIdentifers(block);

						cSharpClause = null;
					}
				}
			}
		}

		private void ConvertExpressionStatement(ExpressionStatementSyntax statement, LuauBlock block)
		{
			LuauExpressionStatement luauStatement = new();
			block.AddLast(luauStatement);

			luauStatement.Expression = ConvertExpression(statement.Expression, luauStatement, true);

			// Remove any unnecessary statements that do nothing
			if (!luauStatement.Expression.DoesSomething)
			{
				var statementNode = block.Statements.Find(luauStatement);
				if (statementNode != null) block.Remove(statementNode);
			}
		}

		// Expression Syntax Conversions

		private LuauQualifiedIdentifier ConvertIdentifierNameExpression(IdentifierNameSyntax identifierName,
			LuauStatement _, bool isTopLevelStatement = false)
		{
			// Get the symbol for the type that this code is a part of
			ClassDeclarationSyntax? parentClassDecl = identifierName.FirstAncestorOfType<ClassDeclarationSyntax>();
			if (parentClassDecl == null) throw new Exception("This code is not inside of a class declaration");
			ITypeSymbol? containingTypeSymbol = m_SemanticModel?.GetDeclaredSymbol(parentClassDecl);
			if (containingTypeSymbol == null) throw new Exception("There is no symbol for the containing type");

			// Get the identifier's dynamic identifier
			SymbolInfo? info = m_SemanticModel?.GetSymbolInfo(identifierName);
			if (info == null || info.Value.Symbol == null) throw new Exception("There is no symbol corresponding to the IdentifierNameSyntax");
			ISymbol identifierSymbol = info.Value.Symbol;
			LuauDynamicIdentifier convertedName = new(identifierSymbol);

			// If the symbol refers to a member of the containing type then add 'self.'
			if (containingTypeSymbol.GetMembers().Contains(identifierSymbol))
			{
				return new LuauQualifiedIdentifier(
					new List<LuauIdentifier>() {
						identifierSymbol.IsStatic ? new LuauDynamicIdentifier(containingTypeSymbol) : new LuauIdentifier("self"),
						convertedName
					},
					!identifierSymbol.IsStatic && identifierSymbol.Kind == SymbolKind.Method
				);
			}

			return convertedName;
		}

		// This method only handles the member variables because member methods
		// have to be matched by signature in case of overloads
		private LuauMemberAccess ConvertMemberAccessExpression(MemberAccessExpressionSyntax memberAccess,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			// Make sure it's not a PointerMemberAccessExpression
			if (memberAccess.Kind() == SyntaxKind.PointerMemberAccessExpression)
			{
				throw new NotImplementedException("PointerMemberAccessExpression not supported "
					+ " (LineSpan " + memberAccess.GetLocation().GetLineSpan() + ")");
			}

			// Get the symbol of the member
			TypeInfo info = m_SemanticModel.GetTypeInfo(memberAccess.Expression);
			if (info.Type == null) throw new Exception("Object has no type!");

			// Get the symbol for the member
			ISymbol memberSymbol = info.Type.GetMembers(memberAccess.Name.Identifier.Text).First();
			if (memberSymbol.Kind != SymbolKind.Property) throw new ArgumentException("Member is not a property!");
			
			// Return the converted expression
			return new LuauMemberAccess(ConvertExpression(memberAccess.Expression, statement), new LuauDynamicIdentifier(memberSymbol));
		}

		private LuauInvocation ConvertInvocationExpression(InvocationExpressionSyntax invocation,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			// Everything but a simple member access expression is easy
			if (invocation.Expression.Kind() != SyntaxKind.SimpleMemberAccessExpression)
			{
				return new LuauInvocation(
					ConvertExpression(invocation.Expression, statement),
					GetArgumentList(invocation.ArgumentList, statement)
				);
			}

			// Simple member access expressions have to be parameter matched for overloads
			MemberAccessExpressionSyntax memberAccess = (MemberAccessExpressionSyntax)invocation.Expression;
			var methods = m_SemanticModel.GetMemberGroup(memberAccess).OfType<IMethodSymbol>();
			if (methods == null) throw new Exception("No methods with the same name found!");

			// Match a method to the argument list
			IMethodSymbol? methodMatch = MatchMethodToArguments(methods.ToImmutableArray(),
				invocation.ArgumentList.Arguments);
			if (methodMatch == null) throw new Exception("No method was matched to this call");

			// Return the invocation
			return new LuauInvocation(
				new LuauMemberAccess(
					ConvertExpression(memberAccess.Expression, statement),
					new LuauDynamicIdentifier(methodMatch),
					true
				),
				GetArgumentList(invocation.ArgumentList, statement)
			);
		}

		private LuauInvocation ConvertObjectCreationExpression(ObjectCreationExpressionSyntax objectCreation,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			// Get the symbol info for the object type
			var info = m_SemanticModel.GetSymbolInfo(objectCreation.Type);
			if (info.Symbol == null) throw new Exception("No symbol info for object creation! (LineSpan " +
				objectCreation.GetLocation().GetLineSpan() + ")");

			INamedTypeSymbol typeSymbol = (INamedTypeSymbol)info.Symbol;
			LuauDynamicIdentifier typeIdentifier = new(typeSymbol);

			// Match a constructor to the argument list

			IMethodSymbol? matchedConstructor = null;

			// Match a parameterless constructor
			if (objectCreation.ArgumentList == null)
			{
				// Get an explicitly defined default constructor
				foreach (IMethodSymbol constructor in typeSymbol.Constructors)
				{
					if (constructor.Parameters.Length == 0)
					{
						matchedConstructor = constructor;
						break;
					}
				}

				// Use the implicitly defined constructor that got generated for this object
				if (matchedConstructor == null)
				{
					return new LuauInvocation(
						new LuauQualifiedIdentifier(
							new List<LuauIdentifier> { typeIdentifier, "_initCtor" },
							false
						),
						null
					);
				}

				// Return an invocation for the parameterless constructor
				return new LuauInvocation(
					new LuauQualifiedIdentifier(
						new List<LuauIdentifier> { typeIdentifier, new LuauDynamicIdentifier(matchedConstructor) },
						false
					),
					null
				);
			}

			// Match a constructor with parameters
			matchedConstructor = MatchMethodToArguments(typeSymbol.Constructors, objectCreation.ArgumentList.Arguments);
			if (matchedConstructor == null) { throw new Exception("No constructor match found!"); }

			// Return an invocation for the constructor
			return new LuauInvocation(
				new LuauQualifiedIdentifier(
					new List<LuauIdentifier> { typeIdentifier, new LuauDynamicIdentifier(matchedConstructor) },
					false
				),
				GetArgumentList(objectCreation.ArgumentList, statement)
			);
		}

		private LuauParenthesizedExpression ConvertParenthesizedExpression(ParenthesizedExpressionSyntax expression,
			LuauStatement statement, bool isTopLevelStatement = false)
			=> new(ConvertExpression(expression.Expression, statement, false));

		private LuauLiteralExpression ConvertLiteralExpression(LiteralExpressionSyntax expression,
			LuauStatement _, bool isTopLevelStatement = false)
		{
			switch (expression.Kind())
			{
				case SyntaxKind.NumericLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.NumericLiteral, expression.Token.ValueText);
				case SyntaxKind.StringLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.StringLiteral, expression.Token.ValueText);
				case SyntaxKind.CharacterLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.CharacterLiteral, expression.Token.ValueText);
				case SyntaxKind.TrueLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.TrueLiteral);
				case SyntaxKind.FalseLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.FalseLiteral);
				case SyntaxKind.NullLiteralExpression:
					return new LuauLiteralExpression(ExpressionKind.NilLiteral);

				default:
					throw new NotImplementedException("Undefined literal expression conversion found: " + expression.Kind().ToString()
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
			}
		}

		private LuauExpression ConvertPrefixUnaryExpression(PrefixUnaryExpressionSyntax expression,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			switch (expression.Kind())
			{
				case SyntaxKind.UnaryPlusExpression:
					return new LuauUnaryExpression(ExpressionKind.UnaryPlus, ConvertExpression(expression.Operand, statement, false));
				case SyntaxKind.UnaryMinusExpression:
					return new LuauUnaryExpression(ExpressionKind.UnaryMinus, ConvertExpression(expression.Operand, statement, false));
				case SyntaxKind.BitwiseNotExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.bnot"), new List<LuauExpression> { ConvertExpression(expression.Operand, statement, false) });
				case SyntaxKind.LogicalNotExpression:
					return new LuauUnaryExpression(ExpressionKind.UnaryLogicalNot, ConvertExpression(expression.Operand, statement, false));
				case SyntaxKind.PreIncrementExpression:
					{
						if (expression.Operand.Kind() != SyntaxKind.IdentifierName)
							throw new NotImplementedException("Unsure how to handle increment on non IdentifierName tokens. "
								+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");

						// These values could be null but should never be
						if (statement.Parent == null)
							throw new ArgumentNullException(nameof(statement));
						LuauBlock parent = (LuauBlock)statement.Parent;

						var statementNode = parent.Statements.Find(statement);
						if (statementNode == null)
							throw new ArgumentNullException(nameof(statement));

						// Create a line before that adds 1 to the variable
						LuauAssignmentStatement assignStatement = new(StatementKind.AddAssignment);
						parent.AddBefore(statementNode, assignStatement);

						LuauQualifiedIdentifier variableIdentifier = ConvertIdentifierNameExpression(
							(IdentifierNameSyntax)expression.Operand, statement, false);
						assignStatement.Left = variableIdentifier;
						assignStatement.Right = new LuauLiteralExpression(ExpressionKind.NumericLiteral, "1");

						return variableIdentifier;
					}
				case SyntaxKind.PreDecrementExpression:
					{
						if (expression.Operand.Kind() != SyntaxKind.IdentifierName)
							throw new NotImplementedException("Unsure how to handle decrement on non IdentifierName tokens. "
								+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");

						// These values could be null but should never be
						if (statement.Parent == null)
							throw new ArgumentNullException(nameof(statement));
						LuauBlock parent = (LuauBlock)statement.Parent;

						var statementNode = parent.Statements.Find(statement);
						if (statementNode == null)
							throw new ArgumentNullException(nameof(statement));

						// Create a line before that adds 1 to the variable
						LuauAssignmentStatement assignStatement = new(StatementKind.SubtractAssignment);
						parent.AddBefore(statementNode, assignStatement);

						LuauQualifiedIdentifier variableIdentifier = ConvertIdentifierNameExpression(
							(IdentifierNameSyntax)expression.Operand, statement, false);
						assignStatement.Left = variableIdentifier;
						assignStatement.Right = new LuauLiteralExpression(ExpressionKind.NumericLiteral, "1");

						return variableIdentifier;
					}
				/*
				case SyntaxKind.AddressOfExpression:
				case SyntaxKind.PointerIndirectionExpression:
				case SyntaxKind.IndexExpression:
				*/

				default:
					throw new ArgumentException("Undefined prefix unary expression found: " + expression.Kind().ToString()
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
			}
		}

		private LuauExpression ConvertPostfixUnaryExpression(PostfixUnaryExpressionSyntax expression,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			switch (expression.Kind())
			{
				case SyntaxKind.PostIncrementExpression:
					{
						if (expression.Operand.Kind() != SyntaxKind.IdentifierName)
							throw new NotImplementedException("Unsure how to handle increment on non IdentifierName tokens. "
								+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");

						// These values could be null but should never be
						if (statement.Parent == null)
							throw new ArgumentNullException(nameof(statement));
						LuauBlock parent = (LuauBlock)statement.Parent;

						var statementNode = parent.Statements.Find(statement);
						if (statementNode == null)
							throw new ArgumentNullException(nameof(statement));

						LuauQualifiedIdentifier variableIdentifier = ConvertIdentifierNameExpression(
							(IdentifierNameSyntax)expression.Operand, statement, false);

						// If the depth is zero then we don't need the value of it so we don't need a temp value
						LuauIdentifier retValue = "no_value";
						if (!isTopLevelStatement)
						{
							// Create a line before that stores the value in a temporary variable
							LuauVariableDeclaration tempAssignStatement = new();
							parent.AddBefore(statementNode, tempAssignStatement);

							retValue = ReserveTempIdentifier(parent);
							tempAssignStatement.Identifier = retValue;
							tempAssignStatement.Expression = variableIdentifier;
						}


						// Create a line after that one which increments the variable
						LuauAssignmentStatement assignStatement = new(StatementKind.AddAssignment);
						parent.AddBefore(statementNode, assignStatement);

						assignStatement.Left = variableIdentifier;
						assignStatement.Right = new LuauLiteralExpression(ExpressionKind.NumericLiteral, "1");

						return retValue;
					}
				case SyntaxKind.PostDecrementExpression:
					{
						if (expression.Operand.Kind() != SyntaxKind.IdentifierName)
							throw new NotImplementedException("Unsure how to handle decrement on non IdentifierName tokens. "
								+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");

						// These values could be null but should never be
						if (statement.Parent == null)
							throw new ArgumentNullException(nameof(statement));
						LuauBlock parent = (LuauBlock)statement.Parent;

						var statementNode = parent.Statements.Find(statement);
						if (statementNode == null)
							throw new ArgumentNullException(nameof(statement));

						LuauQualifiedIdentifier variableIdentifier = ConvertIdentifierNameExpression(
							(IdentifierNameSyntax)expression.Operand, statement, false);

						// If the depth is zero then we don't need the value of it so we don't need a temp value
						LuauIdentifier retValue = "no_value";
						if (!isTopLevelStatement)
						{
							// Create a line before that stores the value in a temporary variable
							LuauVariableDeclaration tempAssignStatement = new();
							parent.AddBefore(statementNode, tempAssignStatement);

							retValue = ReserveTempIdentifier(parent);
							tempAssignStatement.Identifier = retValue;
							tempAssignStatement.Expression = variableIdentifier;
						}

						// Create a line after that one which decrements the variable
						LuauAssignmentStatement assignStatement = new(StatementKind.SubtractAssignment);
						parent.AddBefore(statementNode, assignStatement);

						assignStatement.Left = variableIdentifier;
						assignStatement.Right = new LuauLiteralExpression(ExpressionKind.NumericLiteral, "1");

						return retValue;
					}
				//case SyntaxKind.SuppressNullableWarningExpression:

				default:
					throw new ArgumentException("Undefined postifix unary expression found: " + expression.Kind().ToString()
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
			}
		}

		private LuauExpression ConvertBinaryExpression(BinaryExpressionSyntax expression,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			LuauExpression left = ConvertExpression(expression.Left, statement, false);
			LuauExpression right = ConvertExpression(expression.Right, statement, false);

			switch (expression.Kind())
			{
				case SyntaxKind.AddExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryAdd, left, right);
				case SyntaxKind.SubtractExpression:
					return new LuauBinaryExpression(ExpressionKind.BinarySubtract, left, right);
				case SyntaxKind.MultiplyExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryMultiply, left, right);
				case SyntaxKind.DivideExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryDivide, left, right);
				case SyntaxKind.ModuloExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryModulo, left, right);
				case SyntaxKind.LeftShiftExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.lshift"), new List<LuauExpression> { left, right });
				case SyntaxKind.RightShiftExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.rshift"), new List<LuauExpression> { left, right });
				case SyntaxKind.UnsignedRightShiftExpression:
					Console.WriteLine("Unsigned right shift used. Converting to regular right shift."
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
					return new LuauInvocation(new LuauIdentifier("bit32.rshift"), new List<LuauExpression> { left, right });
				case SyntaxKind.LogicalOrExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryLogicalOr, left, right);
				case SyntaxKind.LogicalAndExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryLogicalAnd, left, right);
				case SyntaxKind.BitwiseOrExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.bor"), new List<LuauExpression> { left, right });
				case SyntaxKind.BitwiseAndExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.band"), new List<LuauExpression> { left, right });
				case SyntaxKind.ExclusiveOrExpression:
					return new LuauInvocation(new LuauIdentifier("bit32.bxor"), new List<LuauExpression> { left, right });
				case SyntaxKind.EqualsExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryEquals, left, right);
				case SyntaxKind.NotEqualsExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryNotEquals, left, right);
				case SyntaxKind.LessThanExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryLessThan, left, right);
				case SyntaxKind.LessThanOrEqualExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryLessThanOrEqual, left, right);
				case SyntaxKind.GreaterThanExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryGreaterThan, left, right);
				case SyntaxKind.GreaterThanOrEqualExpression:
					return new LuauBinaryExpression(ExpressionKind.BinaryGreaterThanOrEqual, left, right);
				default:
					throw new ArgumentException("Unsupported binary expression: {0}", expression.Kind().ToString()
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
					/*
				 case SyntaxKind.IsExpression:
					 break;
				 case SyntaxKind.AsExpression:
					 break;
				 case SyntaxKind.CoalesceExpression: CAN BE IMPLEMENTED WITH TERNARY OPERATOR
					 break;
					*/
			}
		}

		private LuauExpression ConvertAssignmentExpression(AssignmentExpressionSyntax expression,
			LuauStatement statement, bool isTopLevelStatement = false)
		{
			// Convert the expressions on either side
			LuauExpression left = ConvertExpression(expression.Left, statement, false);
			LuauExpression right = ConvertExpression(expression.Right, statement, false);

			// Get the values needed to add statements before this one
			LuauBlock? parent = (LuauBlock?)statement.Parent;
			if (parent == null) throw new Exception("Statement had no parent");

			var statementNode = parent.Statements.Find(statement);
			if (statementNode == null) throw new Exception("Statement wasn't a child of its parent");

			// Figure out the type specific assignment details
			StatementKind assignmentKind = StatementKind.SimpleAssignment;
			switch (expression.Kind())
			{
				// Directly supported assignment types
				case SyntaxKind.SimpleAssignmentExpression:
					break;
				case SyntaxKind.AddAssignmentExpression:
					assignmentKind = StatementKind.AddAssignment;
					break;
				case SyntaxKind.SubtractAssignmentExpression:
					assignmentKind = StatementKind.SubtractAssignment;
					break;
				case SyntaxKind.MultiplyAssignmentExpression:
					assignmentKind = StatementKind.MultiplyAssignment;
					break;
				case SyntaxKind.DivideAssignmentExpression:
					assignmentKind = StatementKind.DivideAssignment;
					break;
				case SyntaxKind.ModuloAssignmentExpression:
					assignmentKind = StatementKind.ModuloAssignment;
					break;

				// Supported with extra steps
				case SyntaxKind.AndAssignmentExpression:
					right = new LuauInvocation(new LuauIdentifier("bit32.band"), new List<LuauExpression> { left, right });
					break;
				case SyntaxKind.ExclusiveOrAssignmentExpression:
					right = new LuauInvocation(new LuauIdentifier("bit32.bxor"), new List<LuauExpression> { left, right });
					break;
				case SyntaxKind.OrAssignmentExpression:
					right = new LuauInvocation(new LuauIdentifier("bit32.bor"), new List<LuauExpression> { left, right });
					break;
				case SyntaxKind.LeftShiftAssignmentExpression:
					right = new LuauInvocation(new LuauIdentifier("bit32.lshift"), new List<LuauExpression> { left, right });
					break;
				case SyntaxKind.RightShiftAssignmentExpression:
					right = new LuauInvocation(new LuauIdentifier("bit32.rshift"), new List<LuauExpression> { left, right });
					break;
				case SyntaxKind.UnsignedRightShiftAssignmentExpression:
					Console.WriteLine("Unsigned right shift used. Converting to regular right shift."
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");
					right = new LuauInvocation(new LuauIdentifier("bit32.rshift"), new List<LuauExpression> { left, right });
					break;

				default:
					throw new ArgumentException("Unsupported assignment expression: {0}", expression.Kind().ToString()
						+ " (LineSpan " + expression.GetLocation().GetLineSpan() + ")");

					//case SyntaxKind.CoalesceAssignmentExpression:
			}

			// Add the assignment statement before the current line
			LuauAssignmentStatement luauAssignment = new(assignmentKind);
			parent.AddBefore(statementNode, luauAssignment);

			luauAssignment.Left = left;
			luauAssignment.Right = right;

			// Fill in the current line
			if (isTopLevelStatement)
				// Throw away this expression statement
				return new LuauIdentifier("no_value");
			else
				return left;
		}

		private void WrapBlock(CSharpSyntaxNode node, ref LuauBlock block)
		{
			if (node.Kind() == SyntaxKind.Block)
				// If it's already a block then just convert it to a luau block
				ConvertBlockSyntax((BlockSyntax)node, ref block);
			else
				// Otherwise create a new one and add the statement to it
				ConvertAndAddStatement(node, block);
		}

		private List<LuauIdentifier> GetParameterList(ParameterListSyntax parameterList, LuauBlock block)
		{
			List<LuauIdentifier> luauParameterList = new();
			foreach (var parameter in parameterList.Parameters)
			{
				if (parameter == null) continue;

				// Get and register the symbol for this parameter
				ISymbol? symbol = m_SemanticModel.GetDeclaredSymbol(parameter);
				if (symbol == null) throw new Exception("Couldn't match a symbol to a parameter");
				m_Context.AssignSymbolName(symbol, symbol.Name);

				LuauDynamicIdentifier paramIdentifier = new(symbol);
				luauParameterList.Add(paramIdentifier);

				// Add the default clause as a ternary operator
				if (parameter.Default == null) continue;

				LuauAssignmentStatement assignment = new(StatementKind.SimpleAssignment);
				block.AddLast(assignment);

				assignment.Left = paramIdentifier;

				// parameter = if parameter == nil then [default value] else parameter
				assignment.Right = new LuauTernary(
					// Condition
					new LuauBinaryExpression(
						ExpressionKind.BinaryEquals,
						paramIdentifier,
						new LuauLiteralExpression(ExpressionKind.NilLiteral)
					),
					// Value
					ConvertExpression(parameter.Default.Value, assignment),
					// Alternative
					paramIdentifier
				);
			}

			return luauParameterList;
		}

		private List<LuauExpression> GetArgumentList(ArgumentListSyntax argumentList, LuauStatement statement)
		{
			List<LuauExpression> luauArgumentList = new();
			foreach (var argument in argumentList.Arguments)
			{
				if (argument != null)
					luauArgumentList.Add(ConvertExpression(argument.Expression, statement));
			}

			return luauArgumentList;
		}

		private IMethodSymbol? MatchMethodToArguments(ImmutableArray<IMethodSymbol> methods,
			SeparatedSyntaxList<ArgumentSyntax> arguments)
		{
			// Check each method in the array
			foreach (IMethodSymbol method in methods)
			{
				// If the number of arguments and parameters isn't the same then it's not a match
				if (arguments.Count > method.Parameters.Length) continue;

				// Check each argument to match the type
				bool isMatch = true;
				for (int i = 0; i < method.Parameters.Length; i++)
				{
					if (i + 1 <= arguments.Count)
					{ // There's an argument, so the types have to match
						// Get the type info of the argument in the i-th position
						var argument = arguments[i];
						var typeInfo = m_SemanticModel.GetTypeInfo(argument.Expression);

						// If the type info doesn't match the i-th parameter of the constructor then skip to the next constructor
						if (typeInfo.Type == null ||
							!SymbolEqualityComparer.Default.Equals(typeInfo.Type, method.Parameters[i].Type))
						{
							isMatch = false;
							break;
						}
					}
					else if (!method.Parameters[i].HasExplicitDefaultValue)
					{ // There's no argument, so the parameter has to have a default value for this to be valid
						isMatch = false;
						break;
					}
				}

				// Return the first match
				if (isMatch)
					return method;
			}

			// There was no match
			return null;
		}

		private void ConvertAndAddStatement(CSharpSyntaxNode node, LuauBlock block)
		{
			// This way newlines can be transferred so that it's readable
			if (IncludeNewlines)
			{
				foreach (var trivia in node.GetLeadingTrivia())
				{
					if (trivia.IsKind(SyntaxKind.EndOfLineTrivia))
						block.AddLast(new LuauNewline());
				}
			}

			// Check if there's a luau splice before this node
			if (m_State.SpliceLocations.TryGetValue(node, out LuauSpliceDirective? value))
				HandleLuauSplice(value, block);

			switch (node.Kind())
			{
				case SyntaxKind.MethodDeclaration:
					ConvertMethod((MethodDeclarationSyntax)node, block);
					break;
				case SyntaxKind.VariableDeclarator:
					ConvertVariableDeclarator((VariableDeclaratorSyntax)node, block);
					break;
				case SyntaxKind.VariableDeclaration:
					ConvertVariableDeclaration((VariableDeclarationSyntax)node, block);
					break;
				case SyntaxKind.LocalDeclarationStatement:
					ConvertLocalDeclarationStatement((LocalDeclarationStatementSyntax)node, block);
					break;
				case SyntaxKind.ReturnStatement:
					ConvertReturnStatementSyntax((ReturnStatementSyntax)node, block);
					break;
				case SyntaxKind.IfStatement:
					ConvertIfStatementSyntax((IfStatementSyntax)node, block);
					break;
				case SyntaxKind.ExpressionStatement:
					ConvertExpressionStatement((ExpressionStatementSyntax)node, block);
					break;

				default:
					throw new ArgumentException("Undefined Expression Conversion: {0}", node.Kind().ToString()
						+ " (LineSpan " + node.GetLocation().GetLineSpan() + ")");
			}
		}

		private LuauExpression ConvertExpression(CSharpSyntaxNode node, LuauStatement statement, bool isTopLevelStatement = false)
		{
			switch (node.Kind())
			{
				// Expressions
				case SyntaxKind.IdentifierName:
					return ConvertIdentifierNameExpression((IdentifierNameSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.SimpleMemberAccessExpression:
				case SyntaxKind.PointerMemberAccessExpression:
					return ConvertMemberAccessExpression((MemberAccessExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.ObjectCreationExpression:
					return ConvertObjectCreationExpression((ObjectCreationExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.ParenthesizedExpression:
					return ConvertParenthesizedExpression((ParenthesizedExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.InvocationExpression:
					return ConvertInvocationExpression((InvocationExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.ArgListExpression:
				case SyntaxKind.NumericLiteralExpression:
				case SyntaxKind.StringLiteralExpression:
				case SyntaxKind.Utf8StringLiteralExpression:
				case SyntaxKind.CharacterLiteralExpression:
				case SyntaxKind.TrueLiteralExpression:
				case SyntaxKind.FalseLiteralExpression:
				case SyntaxKind.NullLiteralExpression:
				case SyntaxKind.DefaultLiteralExpression:
					return ConvertLiteralExpression((LiteralExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.UnaryPlusExpression:
				case SyntaxKind.UnaryMinusExpression:
				case SyntaxKind.BitwiseNotExpression:
				case SyntaxKind.LogicalNotExpression:
				case SyntaxKind.PreIncrementExpression:
				case SyntaxKind.PreDecrementExpression:
				case SyntaxKind.AddressOfExpression:
				case SyntaxKind.PointerIndirectionExpression:
				case SyntaxKind.IndexExpression:
					return ConvertPrefixUnaryExpression((PrefixUnaryExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.PostIncrementExpression:
				case SyntaxKind.PostDecrementExpression:
				case SyntaxKind.SuppressNullableWarningExpression:
					return ConvertPostfixUnaryExpression((PostfixUnaryExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.AddExpression:
				case SyntaxKind.SubtractExpression:
				case SyntaxKind.MultiplyExpression:
				case SyntaxKind.DivideExpression:
				case SyntaxKind.ModuloExpression:
				case SyntaxKind.LeftShiftExpression:
				case SyntaxKind.RightShiftExpression:
				case SyntaxKind.UnsignedRightShiftExpression:
				case SyntaxKind.LogicalOrExpression:
				case SyntaxKind.LogicalAndExpression:
				case SyntaxKind.BitwiseOrExpression:
				case SyntaxKind.BitwiseAndExpression:
				case SyntaxKind.ExclusiveOrExpression:
				case SyntaxKind.EqualsExpression:
				case SyntaxKind.NotEqualsExpression:
				case SyntaxKind.LessThanExpression:
				case SyntaxKind.LessThanOrEqualExpression:
				case SyntaxKind.GreaterThanExpression:
				case SyntaxKind.GreaterThanOrEqualExpression:
				case SyntaxKind.IsExpression:
				case SyntaxKind.AsExpression:
				case SyntaxKind.CoalesceExpression:
					return ConvertBinaryExpression((BinaryExpressionSyntax)node, statement, isTopLevelStatement);

				case SyntaxKind.SimpleAssignmentExpression:
				case SyntaxKind.AddAssignmentExpression:
				case SyntaxKind.SubtractAssignmentExpression:
				case SyntaxKind.MultiplyAssignmentExpression:
				case SyntaxKind.DivideAssignmentExpression:
				case SyntaxKind.ModuloAssignmentExpression:
				case SyntaxKind.AndAssignmentExpression:
				case SyntaxKind.ExclusiveOrAssignmentExpression:
				case SyntaxKind.OrAssignmentExpression:
				case SyntaxKind.LeftShiftAssignmentExpression:
				case SyntaxKind.RightShiftAssignmentExpression:
				case SyntaxKind.UnsignedRightShiftAssignmentExpression:
				case SyntaxKind.CoalesceAssignmentExpression:
					return ConvertAssignmentExpression((AssignmentExpressionSyntax)node, statement, isTopLevelStatement);

				default:
					throw new ArgumentException("Undefined Expression Conversion: {0}", node.Kind().ToString()
						+ " (LineSpan " + node.GetLocation().GetLineSpan() + ")");
			}
		}

		private LuauIdentifier ReserveTempIdentifier(LuauBlock block)
		{
			// Add the block to the registry if it isn't already
			if (!m_State.AssignedTempValues.ContainsKey(block))
			{
				// Go up the tree and look for free temp values
				int? ancestorTemp = null;
				LuauStatement? parent = block.Parent;
				while (ancestorTemp == null && parent != null)
				{
					if (parent is LuauBlock parentBlock)
					{
						// If theres a free temp value then assign it to ancestorTemp
						if (m_State.AssignedTempValues.TryGetValue(parentBlock, out int value))
							ancestorTemp = value;
					}
					parent = parent.Parent;
				}

				// Add the ancestor's free temp value or 0 because none are reserved
				m_State.AssignedTempValues.Add(block, ancestorTemp ?? 0);
			}

			int tempNumber = m_State.AssignedTempValues[block]++;
			return "temp" + tempNumber.ToString();
		}

		private void ReleaseTempIdentifers(LuauBlock block)
		{
			// Remove this block from the registry
			m_State.AssignedTempValues.Remove(block);
		}

		private void InitMethodOverloadCounters(INamedTypeSymbol typeSymbol) =>
			m_State.OverloadCounters[typeSymbol] = new Dictionary<string, int>();

		private int IncrementOverloadCount(INamedTypeSymbol typeSymbol, string sourceMethodName)
		{
			if (!m_State.OverloadCounters.ContainsKey(typeSymbol)) return -1;

			// Initialize the count if it's not in there and increment if it is
			if (!m_State.OverloadCounters[typeSymbol].ContainsKey(sourceMethodName))
				return m_State.OverloadCounters[typeSymbol][sourceMethodName] = 0;
			else
				return ++m_State.OverloadCounters[typeSymbol][sourceMethodName];
		}

		private void ReleaseMethodOverloadCounters(INamedTypeSymbol typeSymbol) =>
			m_State.OverloadCounters.Remove(typeSymbol);
	}
}