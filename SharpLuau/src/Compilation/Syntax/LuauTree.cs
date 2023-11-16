using Microsoft.CodeAnalysis;
using System.Diagnostics.CodeAnalysis;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using static System.Net.Mime.MediaTypeNames;

namespace SharpLuau.Compilation.Syntax
{
    /// <summary>
    /// The transpilation context stores information from each file that must be shared with other files.
    /// Each file gets its own context which will be stored in its object file, but they are combined at the end
    /// so that it can be accessed globally. This is class can be thought of as the glue that holds everything together.
    /// When class A needs to know how to access a function from class B, that information is stored in this 
    /// transpilation context.
    /// </summary>
    public class LuauContext
    {
		private Dictionary<string, LuauIdentifier> m_DynamicSymbols;

		public LuauContext(Dictionary<string, LuauIdentifier> dynamicSymbols)
		{
			m_DynamicSymbols = dynamicSymbols;
		}

		public LuauContext()
			: this(new()) {}

        public static LuauContext Combine(LuauContext[] contexts)
        {
            // Combine dynamic symbols
            Dictionary<string, LuauIdentifier> dynamicSymbols = new();
            foreach(LuauContext context in contexts)
				context.m_DynamicSymbols.ToList().ForEach(x => dynamicSymbols.Add(x.Key, x.Value));

            return new LuauContext(dynamicSymbols);
		}

        public static string SymbolHash(ISymbol symbol)
		{
			// Collect all of the unique information as an array of bytes
			List<byte[]> hashInfo = new();

			// Name of the symbol
			hashInfo.Add(Encoding.UTF8.GetBytes(symbol.Name));

			// Containing type of the symbol
			INamedTypeSymbol? containingType = symbol.ContainingType;
			if (containingType != null) hashInfo.Add(Encoding.UTF8.GetBytes(containingType.Name));

			// Containing namespace of the symbol
			INamespaceSymbol? containingNamespace = symbol.ContainingNamespace;
			if (containingNamespace != null) hashInfo.Add(Encoding.UTF8.GetBytes(containingNamespace.Name));

			// Location where the symbol is defined
			foreach (Location location in symbol.Locations)
			{
				hashInfo.Add(BitConverter.GetBytes(location.SourceSpan.Start));
				hashInfo.Add(BitConverter.GetBytes(location.SourceSpan.End));
			}

			// Combine all of the byte arrays into one long array
			int totalLength = 0;
			foreach (byte[] bytes in hashInfo) totalLength += bytes.Length;
			byte[] dataBytes = new byte[totalLength];

			int copyIndex = 0;
			foreach (byte[] bytes in hashInfo)
			{
				Array.Copy(bytes, 0, dataBytes, copyIndex, bytes.Length);
				copyIndex += bytes.Length;
			}

			// Generate the hash string
			byte[] hashBytes = SHA256.HashData(dataBytes);

			// Convert the hash bytes into a hexidecimal string and remove the '-' character added byte the converter
			return BitConverter.ToString(hashBytes).Replace("-", String.Empty);
		}

        public Dictionary<string, LuauIdentifier> GetDynamicIdentifiersDictionary() => new(m_DynamicSymbols);

		public void AssignSymbolName(ISymbol symbol, LuauIdentifier newIdentifier) => m_DynamicSymbols.Add(SymbolHash(symbol), newIdentifier);
        public LuauIdentifier GetSymbolName(ISymbol symbol) => m_DynamicSymbols[SymbolHash(symbol)];
        public LuauIdentifier? GetSymbolNameOrDefault(ISymbol symbol) => m_DynamicSymbols.GetValueOrDefault(SymbolHash(symbol));
		public LuauIdentifier? GetSymbolNameOrDefault(string symbolHash) => m_DynamicSymbols.GetValueOrDefault(symbolHash);

		public void WriteBinary(BinaryWriter writer)
        {
            writer.Write(m_DynamicSymbols.Count);
            foreach (var pair in m_DynamicSymbols)
            {
                writer.Write(pair.Key);
                pair.Value.WriteBinary(writer);
            }
        }
	}

    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Field, Inherited = true)]
    public class LuauTreePropertyAttribute : Attribute
    { }

    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Field)]
    public class LuauTreeChildrenAttribute : Attribute
    {
        public readonly bool IncludeName;

        public LuauTreeChildrenAttribute(bool includeName = true) => IncludeName = includeName;
    }

    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Field)]
    public class LuauTreeChildAttribute : Attribute 
    { }

    public enum StatementKind
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

    /// <summary>
    /// A standard interface for all Luau statement nodes. Mainly for type assignment at runtime.
    /// </summary>
    public abstract class LuauStatement
    {
        /// <summary>
        /// The kind of statement for runtime type assignment
        /// </summary>
        [LuauTreeProperty]
        public abstract StatementKind Kind { get; }

        /// <summary>
        /// A unique id used for statement comparisons
        /// </summary>
        public Guid Guid { get; } = Guid.NewGuid();

		/// <summary>
		/// The statement's parent node in the syntax tree
		/// </summary>
		public LuauStatement? Parent { get; private set; }
        protected void SetParent(LuauStatement child, LuauStatement? parent) => child.Parent = parent;

        /// <summary>
        /// Serializes the statement to a binary representation because binary serialization is better.
        /// 
        /// The original purpose of this function is to be able to store custom object files for incremental compilation
        /// </summary>
        /// <param name="writer">The writer used to access a stream</param>
        public abstract void WriteBinary(BinaryWriter writer);

        /// <summary>
        /// Serializes the statement to a text representation. Basically the final pass of converting it to Luau code
        /// </summary>
        /// <param name="context">The context generated by the transpiler</param>
        public abstract string WriteText(LuauContext context);

		protected void WriteHeader(BinaryWriter writer)
        {
			// Write the statement header
			writer.Write((int)Kind);
		}
    }

	public class LuauStatementComparer : IEqualityComparer<LuauStatement>
    {
        public static LuauStatementComparer Default = new LuauStatementComparer();

        public bool Equals(LuauStatement? x, LuauStatement? y)
        {
            return x != null && y != null ? x.Guid == y.Guid : false;
        }

        public int GetHashCode([DisallowNull] LuauStatement obj)
        {
            return obj.Guid.GetHashCode();
        }
    }

	public sealed class LuauBadStatement : LuauStatement
	{
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.BadStatement;

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);
		}

		public override string WriteText(LuauContext context)
		{
            return "<Bad Statement>";
		}
	}

	public sealed class LuauBlock : LuauStatement
	{
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.Block;
		public LinkedList<LuauStatement> Statements = new();

        // All of the linked list methods that add or remove items need to modify the parent of that item

        public void AddAfter(LinkedListNode<LuauStatement> node, LuauStatement newNode)
        {
            SetParent(newNode, this);
            Statements.AddAfter(node, newNode);
        }
        public void AddAfter(LinkedListNode<LuauStatement> node, LinkedListNode<LuauStatement> newNode)
        {
			SetParent(newNode.ValueRef, this);
            Statements.AddAfter(node, newNode);
        }
        public void AddBefore(LinkedListNode<LuauStatement> node, LuauStatement newNode)
        {
			SetParent(newNode, this);
            Statements.AddBefore(node, newNode);
        }
        public void AddBefore(LinkedListNode<LuauStatement> node, LinkedListNode<LuauStatement> newNode)
        {
			SetParent(newNode.ValueRef, this);
            Statements.AddBefore(node, newNode);
        }
        public void AddFirst(LuauStatement newNode)
        {
			SetParent(newNode, this);
            Statements.AddFirst(newNode);
        }
        public void AddFirst(LinkedListNode<LuauStatement> newNode)
        {
			SetParent(newNode.ValueRef, this);
            Statements.AddFirst(newNode);
        }
        public void AddLast(LuauStatement newNode)
        {
			SetParent(newNode, this);
            Statements.AddLast(newNode);
        }
        public void AddLast(LinkedListNode<LuauStatement> newNode)
        {
			SetParent(newNode.ValueRef, this);
            Statements.AddLast(newNode);
        }
        public void Remove(LinkedListNode<LuauStatement> node)
        {
			SetParent(node.ValueRef, null);
            Statements.Remove(node);
        }
        public void RemoveFirst()
        {
			SetParent(Statements.First(), null);
            Statements.RemoveFirst();
        }
        public void RemoveLast()
        {
			SetParent(Statements.Last(), null);
            Statements.RemoveLast();
        }

        // Move all of the items out of the statements out of the other block and into this block.
        // The other block will be empty after this operation.
        public void MoveAppend(LuauBlock block)
        {
            for (int i = 0; i < block.Statements.Count; i++)
            {
                // Get the first node
                var node = block.Statements.First; // If this is null then it will never enter the loop
                if (node == null) { throw new Exception("Linked list is in an illegal state"); }

                // Move the node to this block
                block.Statements.RemoveFirst();
                AddLast(node);
            }
        }

        public override string WriteText(LuauContext context)
        {
            // Expand the children statements into strings
            List<string> statementStrings = new();
            foreach (LuauStatement statement in Statements)
                statementStrings.Add(statement.WriteText(context));

            // Join them with newline characters
            return string.Join('\n', statementStrings);
		}

		public override void WriteBinary(BinaryWriter writer)
		{
			WriteHeader(writer);

			// Write the statement body
			writer.Write(Statements.Count);
            foreach (LuauStatement child in Statements)
                child.WriteBinary(writer);
		}
	}

    public sealed class LuauNewline : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.NewLine;

		public override string WriteText(LuauContext context) => string.Empty;

		public override void WriteBinary(BinaryWriter writer)
		{
			WriteHeader(writer);
		}
	}

    public sealed class LuauComment : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.Comment;

        [LuauTreeProperty]
		public string Text;

        public LuauComment(string text)
        {
            Text = text;
        }

        public override string WriteText(LuauContext context) => "-- " + Text;

		public override void WriteBinary(BinaryWriter writer)
		{
			WriteHeader(writer);

            // Write the body
            writer.Write(Text);
		}
	}

    public sealed class LuauSourceSplice : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.SourceSplice;

		[LuauTreeProperty]
		public string Text;

        public LuauSourceSplice(string text)
        {
            Text = text;
        }

        public override string WriteText(LuauContext context) => Text;

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

			// Write the source
			writer.Write(Text);
		}
	}

    public sealed class LuauVariableDeclaration : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.VariableDeclaration;

        [LuauTreeChild]
		public LuauIdentifier Identifier;
        [LuauTreeChild]
        public LuauExpression? Expression;

        public LuauVariableDeclaration()
        {
            Identifier = LuauIdentifier.Empty;
        }

        public override string WriteText(LuauContext context)
        {
            if (Expression != null)
                return string.Format("local {0} = {1}", Identifier.WriteText(context), Expression.WriteText(context));
            else
                return string.Format("local {0}", Identifier.WriteText(context));
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Identifier.WriteBinary(writer);

            writer.Write(Expression != null); // 1 means there's an expression
            Expression?.WriteBinary(writer);
		}
	}

    public sealed class LuauGlobalFunctionDeclaration : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.GlobalFunctionDeclaration;

        [LuauTreeChild]
		public LuauQualifiedIdentifier Identifier;

        [LuauTreeChildren]
        public List<LuauIdentifier> ParameterList;

        private LuauBlock m_Body;
        [LuauTreeChildren]
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                SetParent(m_Body, null);
                m_Body = value;
                SetParent(m_Body, this);
            }
        }

        public LuauGlobalFunctionDeclaration()
        {
            Identifier = LuauQualifiedIdentifier.Empty;
            ParameterList = new();
            m_Body = new();
            SetParent(m_Body, this);
        }

		public override string WriteText(LuauContext context)
		{
			// Convert the list of parameters to text
			List<string> paramsAsText = new();
			foreach (LuauIdentifier parameter in ParameterList)
				paramsAsText.Add(parameter.WriteText(context));

            // Join everthing into the final statement
			StringBuilder builder = new();

            builder.AppendFormat("function {0}({1}) \n", Identifier.WriteText(context), string.Join(", ", paramsAsText));
            builder.AppendIndented(Body.WriteText(context), '\t');
            builder.Append("end");

            return builder.ToString();
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            // Write the body
            Identifier.WriteBinary(writer);

            writer.Write(ParameterList.Count);
            foreach (LuauIdentifier parameter in ParameterList)
                parameter.WriteBinary(writer);

            m_Body.WriteBinary(writer);
		}
	}

    public sealed class LuauLocalFunctionDeclaration : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.LocalFunctionDeclaration;

        [LuauTreeChild]
		public LuauQualifiedIdentifier Identifier;
        [LuauTreeChildren]
        public List<LuauIdentifier> ParameterList;

        private LuauBlock m_Body;
        [LuauTreeChildren]
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                SetParent(m_Body, null);
                m_Body = value;
                SetParent(m_Body, this);
            }
        }

        public LuauLocalFunctionDeclaration()
        {
            Identifier = LuauQualifiedIdentifier.Empty;
            ParameterList = new();
            m_Body = new();
            SetParent(m_Body, this);
        }

		public override string WriteText(LuauContext context)
		{
			// Convert the list of parameters to text
			List<string> paramsAsText = new();
			foreach (LuauIdentifier parameter in ParameterList)
				paramsAsText.Add(parameter.WriteText(context));

            // Join everthing into the final statement
			StringBuilder builder = new();

            builder.AppendFormat("local function {0}({1}) \n", Identifier.WriteText(context), string.Join(", ", paramsAsText));
            builder.AppendIndented(Body.WriteText(context), '\t');
            builder.Append("end");

            return builder.ToString();
        }

		public override void WriteBinary(BinaryWriter writer)
		{
			WriteHeader(writer);

			// Write the body
			Identifier.WriteBinary(writer);

			writer.Write(ParameterList.Count);
			foreach (LuauIdentifier parameter in ParameterList)
				parameter.WriteBinary(writer);

			m_Body.WriteBinary(writer);
		}
	}

    public sealed class LuauReturnStatement : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.Return;
        [LuauTreeChild]
		public LuauExpression? Expression;

		public override string WriteText(LuauContext context) => string.Format("return {0}", Expression?.WriteText(context));

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            // I don't know why im even commenting this
            writer.Write(Expression != null);
            Expression?.WriteBinary(writer);
		}
	}

    public sealed class LuauExpressionStatement : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.ExpressionStatement;
        [LuauTreeChild]
		public LuauExpression Expression;

        public LuauExpressionStatement()
        {
            Expression = new LuauBadExpression();
        }

		public override string WriteText(LuauContext context) => Expression.WriteText(context);

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Expression.WriteBinary(writer);
		}
	}

    public sealed class LuauIfStatement : LuauStatement
    {
		[LuauTreeProperty]
		public override StatementKind Kind => StatementKind.If;
        [LuauTreeChild]
		public LuauExpression Condition;

        private LuauBlock m_Body;
        [LuauTreeChildren]
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                SetParent(m_Body, null);
                m_Body = value;
                SetParent(m_Body, this);
            }
        }

        [LuauTreeChild]
        public LuauStatement? Else { get; private set; }

        public LuauIfStatement()
        {
            Condition = new LuauBadExpression();
            m_Body = new();
            SetParent(m_Body, this);
        }

        public void SetElse(LuauStatement statement)
        {
            if (statement.Kind != StatementKind.If && statement.Kind != StatementKind.Block)
            {
                throw new ArgumentException("LuauIfStatement else clause requires either another IfStatement or a Block. Got: " +
               statement.Kind.ToString());
            }

            Else = statement;
            SetParent(Else, this);
        }

		public override string WriteText(LuauContext context)
		{
            StringBuilder builder = new();

            // Add the main statement body
            builder.AppendFormat("if {0} then\n", Condition.WriteText(context));
            builder.AppendIndented(Body.WriteText(context), '\t');

            // Add any else clauses
            if (Else != null)
            {
                if (Else.Kind == StatementKind.If)
                {
                    // Add an ifelse clause
                    builder.Append("else")
                        .Append(Else.WriteText(context));
                }
                else
                {
                    // Add an else clause
                    builder.Append("else\n")
                        .AppendIndented(Else.WriteText(context), "\t");
                    builder.Append("end");
                }
            }
            else
                builder.Append("end");

            return builder.ToString();
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            // Write the body
            Condition.WriteBinary(writer);

            m_Body.WriteBinary(writer);

            // This could be another if statement which makes serialization and parsing very simple with recursion
            writer.Write(Else != null);
            Else?.WriteBinary(writer);
		}
	}

    public sealed class LuauAssignmentStatement : LuauStatement
    {
        private StatementKind m_Kind;
		[LuauTreeProperty]
		public override StatementKind Kind => m_Kind;

        [LuauTreeChild]
		public LuauExpression Left;
        [LuauTreeChild]
        public LuauExpression Right;

        public LuauAssignmentStatement(StatementKind kind)
        {
            if (kind < StatementKind.SimpleAssignment || kind > StatementKind.ConcatenateAssignment)
            {
                throw new ArgumentException("LuauAssignmentStatement requires an assignment StatementKind. Got: " +
                kind.ToString());
            }
            m_Kind = kind;

            Left = LuauQualifiedIdentifier.Empty;
            Right = new LuauBadExpression();
        }

		public override string WriteText(LuauContext context)
		{
            string left = Left.WriteText(context), right = Right.WriteText(context);

			return Kind switch
			{
				StatementKind.SimpleAssignment => string.Format("{0} = {1}", left, right),
				StatementKind.AddAssignment => string.Format("{0} += {1}", left, right),
				StatementKind.SubtractAssignment => string.Format("{0} -= {1}", left, right),
				StatementKind.MultiplyAssignment => string.Format("{0} *= {1}", left, right),
				StatementKind.DivideAssignment => string.Format("{0} /= {1}", left, right),
				StatementKind.ModuloAssignment => string.Format("{0} %= {1}", left, right),
				StatementKind.ExponentAssignment => string.Format("{0} ^= {1}", left, right),
				StatementKind.ConcatenateAssignment => string.Format("{0} ..= {1}", left, right),
				_ => "<Unknown Assignment Statement>",
			};
		}

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Left.WriteBinary(writer);
            Right.WriteBinary(writer);
		}
	}

	public enum ExpressionKind
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

	/// <summary>
	/// A standard interface for all Luau statement nodes. Mainly for type assignment at runtime.
	/// </summary>
	public abstract class LuauExpression
    {
        /// <summary>
        /// The kind of statement for runtime type assignment
        /// </summary>
        [LuauTreeProperty]
        public abstract ExpressionKind Kind { get; }

        /// <summary>
        /// Returns false if this expression could be ommitted without changing basic
        /// program behavior
        /// </summary>
        [LuauTreeProperty]
		public virtual bool DoesSomething => false;

        public override string? ToString()
        {
            Console.WriteLine("Base luau expression cannot be converted to a string!");
            return base.ToString();
        }

		/// <summary>
		/// Serializes the expression to a binary representation because binary serialization is better.
		/// 
		/// The original purpose of this function is to be able to store custom object files for incremental compilation
		/// </summary>
		/// <param name="writer">The writer used to access a stream</param>
		public abstract void WriteBinary(BinaryWriter writer);

		/// <summary>
		/// Serializes the expression to a text representation. Basically the final pass of converting it to Luau code
		/// </summary>
		/// <param name="context">The context generated by the transpiler</param>
		public abstract string WriteText(LuauContext context);

		protected void WriteHeader(BinaryWriter writer)
		{
			// Write the statement header
			writer.Write((int)Kind);
		}
	}

	public sealed class LuauBadExpression : LuauExpression
	{
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.BadExpression;

		public override string WriteText(LuauContext context) => "<Bad Expression>";

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);
		}
	}

	public sealed class LuauTernary : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.Ternary;

        [LuauTreeChild]
		public LuauExpression Condition;
        [LuauTreeChild]
        public LuauExpression Value;
        [LuauTreeChild]
        public LuauExpression Alternative;

        public LuauTernary(LuauExpression condition, LuauExpression value, LuauExpression alternative)
        {
            Condition = condition;
            Value = value;
            Alternative = alternative;
        }

		[LuauTreeProperty]
		public override bool DoesSomething => Condition.DoesSomething || Value.DoesSomething || Alternative.DoesSomething;

		public override string WriteText(LuauContext context) => 
            string.Format("if {0} then {1} else {2}", Condition.WriteText(context), Value.WriteText(context), Alternative.WriteText(context));

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            // It's funny how these functions seem to write themselves
            Condition.WriteBinary(writer);
            Value.WriteBinary(writer);
            Alternative.WriteBinary(writer);
		}
	}

    public sealed class LuauMemberAccess : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.MemberAccess;

        [LuauTreeChild]
		public LuauExpression Expression;
        [LuauTreeChild]
        public LuauIdentifier Member;
        [LuauTreeProperty]
        public bool IsMethod;

        public LuauMemberAccess(LuauExpression expression, LuauIdentifier member, bool isMethod = false)
        {
            Expression = expression;
            Member = member;
            IsMethod = isMethod;
        }

		public override string WriteText(LuauContext context) => 
            string.Format("{0}{1}{2}", Expression.WriteText(context), IsMethod ? ':' : ".", Member.WriteText(context));

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Expression.WriteBinary(writer);
            Member.WriteBinary(writer);
            writer.Write(IsMethod);
		}
	}

    public sealed class LuauInvocation : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.Invocation;

        [LuauTreeChild]
		public LuauExpression Identifier;
        [LuauTreeChildren]
        public List<LuauExpression> Arguments;

        public LuauInvocation(LuauExpression identifier, List<LuauExpression>? arguments)
        {
            Identifier = identifier;
            Arguments = arguments ?? new List<LuauExpression>();
        }

		[LuauTreeProperty]
		public override bool DoesSomething => true;

		public override string WriteText(LuauContext context)
        {
            // Convert the list of arguments to text
            List<string> argsAsText = new();
            foreach (LuauExpression argument in Arguments)
                argsAsText.Add(argument.WriteText(context));

            // Join everything into the expression
            return string.Format("{0}({1})", Identifier.WriteText(context), string.Join(", ", argsAsText));
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Identifier.WriteBinary(writer);

            writer.Write(Arguments.Count);
            foreach (LuauExpression argument in Arguments)
                argument.WriteBinary(writer);
		}
	}

    public class LuauIdentifier : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.Identifier;

        [LuauTreeProperty]
		public string Text;

        public LuauIdentifier(string text)
        {
            Text = text;
        }

        public static implicit operator LuauIdentifier(string text) => new(text);
        public static implicit operator string(LuauIdentifier identifier) => identifier.Text;
        public static implicit operator LuauQualifiedIdentifier(LuauIdentifier identifier) =>
            new(new List<LuauIdentifier> { identifier }, false);

		public override string WriteText(LuauContext context) => Text;

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

			writer.Write(Text);
		}

		public static LuauIdentifier Empty = new("");
    }

    public sealed class LuauDynamicIdentifier : LuauIdentifier
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.DynamicIdentifier;

		public LuauDynamicIdentifier(string symbolIdentifier)
            : base(symbolIdentifier) { }

		public LuauDynamicIdentifier(ISymbol symbol)
			: base(LuauContext.SymbolHash(symbol)) { }

		public override string WriteText(LuauContext context) => context.GetSymbolNameOrDefault(Text) ?? "<Unassigned Dynamic Symbol>";
	}

    public sealed class LuauQualifiedIdentifier : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.QualifiedIdentifier;

        [LuauTreeChildren(false)]
		public List<LuauIdentifier> Identifiers;
        [LuauTreeProperty]
        public bool IsMemberMethod;

        public LuauQualifiedIdentifier(List<LuauIdentifier> identifiers, bool isMemberMethod)
        {
            Identifiers = identifiers;
            IsMemberMethod = isMemberMethod;
        }

        public LuauQualifiedIdentifier(string qualifiedIdentifier)
        {
            Identifiers = new();
            IsMemberMethod = qualifiedIdentifier.Contains(':');

            // Split the string and add the individual identifiers
            var identifiers = qualifiedIdentifier.Split('.', ':');
            foreach (string identifier in identifiers)
                Identifiers.Add(identifier);
        }

        public LuauQualifiedIdentifier()
        {
            Identifiers = new();
            IsMemberMethod = false;
        }

		public override string WriteText(LuauContext context)
		{
			// Convert the list of identifiers to text
			List<string> idsAsText = new();
			foreach (LuauIdentifier identifier in Identifiers)
				idsAsText.Add(identifier.WriteText(context));

			// Join everything into the expression
			if (IsMemberMethod)
            {
                StringBuilder builder = new();

                // Join them all together with a : in between the last two
                builder.AppendJoin('.', idsAsText.SkipLast(1)).
                    Append(':').Append(idsAsText.Last());

                return builder.ToString();
            }
            else
                return string.Join('.', idsAsText);

        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            writer.Write(Identifiers.Count);
            foreach (LuauIdentifier identifier in Identifiers)
                identifier.WriteBinary(writer);

            writer.Write(IsMemberMethod);
		}

		public static implicit operator LuauQualifiedIdentifier(string text) => new(new List<LuauIdentifier> { text }, false);
        public static implicit operator LuauQualifiedIdentifier(LuauIdentifier identifier) => new(new List<LuauIdentifier> { identifier }, false);
		public static implicit operator LuauQualifiedIdentifier(LuauDynamicIdentifier identifier) => new(new List<LuauIdentifier> { identifier }, false);
		public static implicit operator LuauQualifiedIdentifier(List<LuauIdentifier> identifiers) => new(identifiers, false);

        public static LuauQualifiedIdentifier Empty = new(new List<LuauIdentifier>(), false);
    }

    public sealed class LuauLambda : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.Lambda;

        [LuauTreeChildren]
        public List<LuauIdentifier> ParameterList;
        [LuauTreeChildren]
		public LuauBlock Body;

        public LuauLambda(List<LuauIdentifier> parameterList)
        {
            ParameterList = parameterList;
            Body = new();
        }

		public override string WriteText(LuauContext context)
		{
			// Convert the list of parameters to text
			List<string> paramsAsText = new();
			foreach (LuauIdentifier parameter in ParameterList)
				paramsAsText.Add(parameter.WriteText(context));

            // Join everything together into the final expression
			StringBuilder builder = new();

            builder.AppendFormat("function({0}) \n", string.Join(", ", paramsAsText));
            builder.AppendIndented(Body.WriteText(context), '\t');
            builder.Append("\nend");

            return builder.ToString();
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Body.WriteBinary(writer);

            writer.Write(ParameterList.Count);
            foreach (LuauIdentifier parameter in ParameterList)
                parameter.WriteBinary(writer);
		}
	}

    public sealed class LuauParenthesizedExpression : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.ParenthesizedExpression;

        [LuauTreeChild]
		public LuauExpression Expression;

        public LuauParenthesizedExpression(LuauExpression expression)
        {
            Expression = expression;
        }

		public override string WriteText(LuauContext context)
		{
            return string.Format("({0})", Expression.WriteText(context));
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Expression.WriteBinary(writer);
		}
	}

    public sealed class LuauLiteralExpression : LuauExpression
    {
        private ExpressionKind m_Kind;

		[LuauTreeProperty]
		public override ExpressionKind Kind => m_Kind;

        [LuauTreeProperty]
		public string? Text;

        public LuauLiteralExpression(ExpressionKind kind)
        {
            if (kind < ExpressionKind.TrueLiteral || kind > ExpressionKind.NilLiteral)
            {
                throw new ArgumentException("LuauLiteralExpression with no data has to be a keyword literal expression. Got: " +
                kind.ToString());
            }
            m_Kind = kind;
        }

        public LuauLiteralExpression(ExpressionKind kind, string text)
        {
            if (kind < ExpressionKind.NumericLiteral || kind > ExpressionKind.CharacterLiteral)
            {
                throw new ArgumentException("LuauLiteralExpression with text data has to be either a string, " +
                "a number or a character. Got" + kind.ToString());
            }
            m_Kind = kind;
            Text = text;
        }

		public override string WriteText(LuauContext context)
		{
			return Kind switch
			{
				ExpressionKind.NumericLiteral => Text ?? "0",
				ExpressionKind.StringLiteral => '"' + Text + '"',
				ExpressionKind.CharacterLiteral => "'" + Text + "'",
				ExpressionKind.TrueLiteral => "true",
				ExpressionKind.FalseLiteral => "false",
				ExpressionKind.NilLiteral => "nil",
				_ => "<Unknown Literal Expression>",
			};
		}

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            // Whether or not there is text can be determined while parsing by which kind it is
            if (Text != null)
                writer.Write(Text);
		}
	}

    public sealed class LuauTableExpression : LuauExpression
    {
		[LuauTreeProperty]
		public override ExpressionKind Kind => ExpressionKind.Table;

        [LuauTreeChildren(false)]
		public List<LuauExpression> Contents;

        public LuauTableExpression()
        {
            Contents = new();
        }

        public LuauTableExpression(List<LuauExpression> contents)
        {
            Contents = contents;
        }

		public override string WriteText(LuauContext context)
		{
            StringBuilder stringBuilder = new StringBuilder();

            // Add the opening bracket
            stringBuilder.Append('{');

            // Fill all of the contents
            int index = 0;
            int lastIndex = Contents.Count() - 1;
            foreach (LuauExpression expression in Contents)
            {
                stringBuilder.Append(expression.WriteText(context));
                if (index < lastIndex)
                    stringBuilder.Append(", ");
            }

            // Add the closing bracket
            stringBuilder.Append("}");

            return stringBuilder.ToString();
        }

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            writer.Write(Contents.Count);
            foreach (LuauExpression expression in Contents)
                expression.WriteBinary(writer);
		}
	}

    public sealed class LuauUnaryExpression : LuauExpression
    {
        private ExpressionKind m_Kind;

		[LuauTreeProperty]
		public override ExpressionKind Kind => m_Kind;

        [LuauTreeChild]
		public LuauExpression Expression;

        public LuauUnaryExpression(ExpressionKind kind, LuauExpression expression)
        {
            if (kind < ExpressionKind.UnaryPlus || kind > ExpressionKind.UnaryLogicalNot)
            {
                throw new ArgumentException("LuauUnaryExpression kind has to be a unary expression. Got " +
                kind.ToString());
            }
            m_Kind = kind;

            Expression = expression;
        }

		[LuauTreeProperty]
		public override bool DoesSomething => Expression.DoesSomething;

		public override string WriteText(LuauContext context)
		{
			return Kind switch
			{
				ExpressionKind.UnaryPlus => "+" + Expression.WriteText(context),
				ExpressionKind.UnaryMinus => "-" + Expression.WriteText(context),
				ExpressionKind.UnaryLogicalNot => "not " + Expression.WriteText(context),
				_ => "<Unknown Unary Expression>",
			};
		}

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Expression.WriteBinary(writer);
		}
	}

    public sealed class LuauBinaryExpression : LuauExpression
    {
        private ExpressionKind m_Kind;

		[LuauTreeProperty]
		public override ExpressionKind Kind => m_Kind;

        [LuauTreeChild]
		public LuauExpression Left;
        [LuauTreeChild]
        public LuauExpression Right;

        public LuauBinaryExpression(ExpressionKind kind, LuauExpression left, LuauExpression right)
        {
            if (kind < ExpressionKind.NumericLiteral || kind > ExpressionKind.BinaryGreaterThanOrEqual)
            {
                throw new ArgumentException("LuauBinaryExpression kind has to be a binary expression. Got: " +
                kind.ToString());
            }
            m_Kind = kind;

            Left = left;
            Right = right;
        }

        public override bool DoesSomething => Left.DoesSomething || Right.DoesSomething;

		public override string WriteText(LuauContext context)
		{
            string left = Left.WriteText(context), right = Right.WriteText(context);

			return Kind switch
			{
				ExpressionKind.BinaryAdd => string.Format("{0} + {1}", left, right),
				ExpressionKind.BinarySubtract => string.Format("{0} - {1}", left, right),
				ExpressionKind.BinaryMultiply => string.Format("{0} * {1}", left, right),
				ExpressionKind.BinaryDivide => string.Format("{0} / {1}", left, right),
				ExpressionKind.BinaryModulo => string.Format("{0} % {1}", left, right),
				ExpressionKind.BinaryConcatenate => string.Format("{0} .. {1}", left, right),
				ExpressionKind.BinaryLogicalOr => string.Format("{0} or {1}", left, right),
				ExpressionKind.BinaryLogicalAnd => string.Format("{0} and {1}", left, right),
				ExpressionKind.BinaryEquals => string.Format("{0} == {1}", left, right),
				ExpressionKind.BinaryNotEquals => string.Format("{0} ~= {1}", left, right),
				ExpressionKind.BinaryLessThan => string.Format("{0} < {1}", left, right),
				ExpressionKind.BinaryLessThanOrEqual => string.Format("{0} <= {1}", left, right),
				ExpressionKind.BinaryGreaterThan => string.Format("{0} > {1}", left, right),
				ExpressionKind.BinaryGreaterThanOrEqual => string.Format("{0} >= {1}", left, right),
				_ => "<Unknown Binary Expression>",
			};
		}

		public override void WriteBinary(BinaryWriter writer)
		{
            WriteHeader(writer);

            Left.WriteBinary(writer);
            Right.WriteBinary(writer);
		}
	}
}