using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau.Compilation.Syntax
{
    internal enum StatementKind
    {
        Block,
        NewLine,
        Comment,
        SourceSplice,
        VariableDeclaration,
        FunctionDeclaration,
        LocalFunctionDeclaration,
        Return,
        ExpressionStatement,
        If,

        // Assignment Statements
        SimpleAssignment,
        AddAssignment,
        SubtractAssignment,
        MultiplyAssignment,
        DivideAssignment,
        ModuloAssignment,
        ExponentAssignment,
        ConcatenateAssignment
    }

    internal class LuauStatement
    {
        public StatementKind Kind { get; protected set; }
        public Guid Guid { get; protected set; }
        public LuauStatement? Parent;


        public LuauStatement() { Guid = Guid.NewGuid(); }
    }

    internal class LuauStatementComparer : IEqualityComparer<LuauStatement>
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

    internal sealed class LuauBlock : LuauStatement
    {
        public LinkedList<LuauStatement> Statements;

        public LuauBlock()
        {
            Kind = StatementKind.Block;
            Statements = new();
        }

        // All of the linked list methods that add or remove items need to modify the parent of that item

        public void AddAfter(LinkedListNode<LuauStatement> node, LuauStatement newNode)
        {
            newNode.Parent = this;
            Statements.AddAfter(node, newNode);
        }
        public void AddAfter(LinkedListNode<LuauStatement> node, LinkedListNode<LuauStatement> newNode)
        {
            newNode.ValueRef.Parent = this;
            Statements.AddAfter(node, newNode);
        }
        public void AddBefore(LinkedListNode<LuauStatement> node, LuauStatement newNode)
        {
            newNode.Parent = this;
            Statements.AddBefore(node, newNode);
        }
        public void AddBefore(LinkedListNode<LuauStatement> node, LinkedListNode<LuauStatement> newNode)
        {
            newNode.ValueRef.Parent = this;
            Statements.AddBefore(node, newNode);
        }
        public void AddFirst(LuauStatement newNode)
        {
            newNode.Parent = this;
            Statements.AddFirst(newNode);
        }
        public void AddFirst(LinkedListNode<LuauStatement> newNode)
        {
            newNode.ValueRef.Parent = this;
            Statements.AddFirst(newNode);
        }
        public void AddLast(LuauStatement newNode)
        {
            newNode.Parent = this;
            Statements.AddLast(newNode);
        }
        public void AddLast(LinkedListNode<LuauStatement> newNode)
        {
            newNode.ValueRef.Parent = this;
            Statements.AddLast(newNode);
        }
        public void Remove(LinkedListNode<LuauStatement> node)
        {
            node.ValueRef.Parent = null;
            Statements.Remove(node);
        }
        public void RemoveFirst()
        {
            Statements.First().Parent = null;
            Statements.RemoveFirst();
        }
        public void RemoveLast()
        {
            Statements.Last().Parent = null;
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

        public override string? ToString() => string.Join('\n', Statements);
    }

    internal sealed class LuauNewline : LuauStatement
    {
        public LuauNewline()
        {
            Kind = StatementKind.NewLine;
        }

        public override string? ToString() => string.Empty;
    }

    internal sealed class LuauComment : LuauStatement
    {
        string Text;

        public LuauComment(string text)
        {
            Kind = StatementKind.Comment;
            Text = text;
        }

        public override string? ToString() => "-- " + Text;
    }

    internal sealed class LuauSourceSplice : LuauStatement
    {
        string Text;

        public LuauSourceSplice(string text)
        {
            Kind = StatementKind.SourceSplice;
            Text = text;
        }

        public override string? ToString() => Text;
    }

    internal sealed class LuauVariableDeclaration : LuauStatement
    {
        public LuauIdentifier Identifier;
        public LuauExpression? Expression;

        public LuauVariableDeclaration()
        {
            Identifier = LuauIdentifier.Empty;
        }

        public override string? ToString()
        {
            if (Expression != null)
                return string.Format("local {0} = {1}", Identifier, Expression);
            else
                return string.Format("local {0}", Identifier);
        }
    }

    internal sealed class LuauGlobalFunctionDeclaration : LuauStatement
    {
        public LuauQualifiedIdentifier Identifier;
        public List<LuauIdentifier> ParameterList;

        private LuauBlock m_Body;
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                m_Body.Parent = null;
                m_Body = value;
                m_Body.Parent = this;
            }
        }

        public LuauGlobalFunctionDeclaration()
        {
            Kind = StatementKind.FunctionDeclaration;
            Identifier = LuauQualifiedIdentifier.Empty;
            ParameterList = new();
            m_Body = new();
            m_Body.Parent = this;
        }

        public override string? ToString()
        {
            StringBuilder builder = new();

            builder.AppendFormat("function {0}({1}) \n", Identifier, string.Join(", ", ParameterList));
            builder.AppendIndented(Body.ToString(), '\t');
            builder.Append("end");

            return builder.ToString();
        }
    }

    internal sealed class LuauLocalFunctionDeclaration : LuauStatement
    {
        public LuauQualifiedIdentifier Identifier;
        public List<LuauIdentifier> ParameterList;

        private LuauBlock m_Body;
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                m_Body.Parent = null;
                m_Body = value;
                m_Body.Parent = this;
            }
        }

        public LuauLocalFunctionDeclaration()
        {
            Kind = StatementKind.LocalFunctionDeclaration;
            Identifier = LuauQualifiedIdentifier.Empty;
            ParameterList = new();
            m_Body = new();
            m_Body.Parent = this;
        }

        public override string? ToString()
        {
            StringBuilder builder = new();

            builder.AppendFormat("local function {0}({1}) \n", Identifier, string.Join(", ", ParameterList));
            builder.AppendIndented(Body.ToString(), '\t');
            builder.Append("end");

            int i = 0;
            i += 4;

            return builder.ToString();
        }
    }

    internal sealed class LuauReturnStatement : LuauStatement
    {
        public LuauExpression? Expression;

        public LuauReturnStatement()
        {
            Kind = StatementKind.ExpressionStatement;
        }

        public override string ToString() => string.Format("return {0}", Expression);
    }

    internal sealed class LuauExpressionStatement : LuauStatement
    {
        public LuauExpression Expression;
        public LuauExpressionStatement()
        {
            Kind = StatementKind.ExpressionStatement;
            Expression = new LuauExpression();
        }

        public override string ToString() => Expression.ToString() ?? string.Empty;
    }

    internal sealed class LuauIfStatement : LuauStatement
    {
        public LuauExpression Condition;

        private LuauBlock m_Body;
        public LuauBlock Body
        {
            get { return m_Body; }
            set
            {
                m_Body.Parent = null;
                m_Body = value;
                m_Body.Parent = this;
            }
        }

        public LuauStatement? Else { get; private set; }

        public LuauIfStatement()
        {
            Kind = StatementKind.If;
            Condition = new();
            m_Body = new();
            m_Body.Parent = this;
        }

        public void SetElse(LuauStatement statement)
        {
            if (statement.Kind != StatementKind.If && statement.Kind != StatementKind.Block)
            {
                throw new ArgumentException("LuauIfStatement else clause requires either another IfStatement or a Block. Got: " +
               statement.Kind.ToString());
            }

            Else = statement;
            Else.Parent = this;
        }

        public override string ToString()
        {
            StringBuilder builder = new();

            // Add the main statement body
            builder.AppendFormat("if {0} then\n", Condition);
            builder.AppendIndented(Body.ToString(), '\t');

            // Add any else clauses
            if (Else != null)
            {
                if (Else.Kind == StatementKind.If)
                {
                    // Add an ifelse clause
                    builder.Append("else")
                        .Append(Else.ToString());
                }
                else
                {
                    // Add an else clause
                    builder.Append("else\n")
                        .AppendIndented(Else.ToString(), "\t");
                    builder.Append("end");
                }
            }
            else
                builder.Append("end");

            return builder.ToString();
        }
    }

    internal sealed class LuauAssignmentStatement : LuauStatement
    {
        public LuauExpression Left;
        public LuauExpression Right;

        public LuauAssignmentStatement(StatementKind kind)
        {
            if (kind < StatementKind.SimpleAssignment || kind > StatementKind.ConcatenateAssignment)
            {
                throw new ArgumentException("LuauAssignmentStatement requires an assignment StatementKind. Got: " +
                kind.ToString());
            }
            Kind = kind;

            Left = LuauQualifiedIdentifier.Empty;
            Right = new LuauExpression();
        }

        public override string? ToString()
        {
            switch (Kind)
            {
                case StatementKind.SimpleAssignment:
                    return string.Format("{0} = {1}", Left, Right);
                case StatementKind.AddAssignment:
                    return string.Format("{0} += {1}", Left, Right);
                case StatementKind.SubtractAssignment:
                    return string.Format("{0} -= {1}", Left, Right);
                case StatementKind.MultiplyAssignment:
                    return string.Format("{0} *= {1}", Left, Right);
                case StatementKind.DivideAssignment:
                    return string.Format("{0} /= {1}", Left, Right);
                case StatementKind.ModuloAssignment:
                    return string.Format("{0} %= {1}", Left, Right);
                case StatementKind.ExponentAssignment:
                    return string.Format("{0} ^= {1}", Left, Right);
                case StatementKind.ConcatenateAssignment:
                    return string.Format("{0} ..= {1}", Left, Right);
                default:
                    return null;
            }
        }
    }

    internal enum ExpressionKind
    {
        Ternary,
        MemberAccess,
        Invocation,
        Identifier,
        QualifiedIdentifier,
        Lambda,
        ParenthesizedExpression,

        // Literal Expressions
        NumericLiteral,
        StringLiteral,
        CharacterLiteral,
        TrueLiteral,
        FalseLiteral,
        NilLiteral,

        Table,

        // Unary Expressions
        UnaryPlus,
        UnaryMinus,
        UnaryLogicalNot,

        // Binary Expressions
        BinaryAdd,
        BinarySubtract,
        BinaryMultiply,
        BinaryDivide,
        BinaryModulo,
        BinaryConcatenate,
        BinaryLogicalOr,
        BinaryLogicalAnd,
        BinaryEquals,
        BinaryNotEquals,
        BinaryLessThan,
        BinaryLessThanOrEqual,
        BinaryGreaterThan,
        BinaryGreaterThanOrEqual,
    }

    internal class LuauExpression
    {
        public ExpressionKind Kind { get; protected set; }

        // Returns false if the expression could be ommited in an ExpressionStatement
        // without altering the behavior of the program
        public virtual bool DoesSomething() => false;

        public override string? ToString()
        {
            Console.WriteLine("Base luau expression cannot be converted to a string!");
            return base.ToString();
        }
    }

    internal sealed class LuauTernary : LuauExpression
    {
        public LuauExpression Condition;
        public LuauExpression Value;
        public LuauExpression Alternative;

        public LuauTernary(LuauExpression condition, LuauExpression value, LuauExpression alternative)
        {
            Kind = ExpressionKind.Ternary;

            Condition = condition;
            Value = value;
            Alternative = alternative;
        }

        public override bool DoesSomething() => Condition.DoesSomething() || Value.DoesSomething() || Alternative.DoesSomething();

        public override string ToString() => string.Format("if {0} then {1} else {2}", Condition, Value, Alternative);
    }

    internal sealed class LuauMemberAccess : LuauExpression
    {
        public LuauExpression Expression;
        public LuauIdentifier Member;
        public bool IsMethod;

        public LuauMemberAccess(LuauExpression expression, LuauIdentifier member, bool isMethod = false)
        {
            Kind = ExpressionKind.MemberAccess;
            Expression = expression;
            Member = member;
            IsMethod = isMethod;
        }

        public override string ToString() => string.Format("{0}{1}{2}", Expression, IsMethod ? ':' : ".", Member);
    }

    internal sealed class LuauInvocation : LuauExpression
    {
        public LuauExpression Identifier;
        public List<LuauExpression> Arguments;

        public LuauInvocation(LuauExpression identifier, List<LuauExpression>? arguments)
        {
            Kind = ExpressionKind.Invocation;
            Identifier = identifier;
            Arguments = arguments ?? new List<LuauExpression>();
        }

        public override bool DoesSomething() => true;

        public override string ToString() => string.Format("{0}({1})", Identifier, string.Join(", ", Arguments));
    }

    internal sealed class LuauIdentifier : LuauExpression
    {
        public string Text;

        public LuauIdentifier(string text)
        {
            Kind = ExpressionKind.Identifier;
            Text = text;
        }

        public static implicit operator LuauIdentifier(string text) => new(text);
        public static implicit operator string(LuauIdentifier identifier) => identifier.Text;
        public static implicit operator LuauQualifiedIdentifier(LuauIdentifier identifier) =>
            new LuauQualifiedIdentifier(new List<LuauIdentifier> { identifier }, false);

        public override string ToString() => Text;

        public static LuauIdentifier Empty = new LuauIdentifier("");
    }

    internal sealed class LuauQualifiedIdentifier : LuauExpression
    {
        public List<LuauIdentifier> Identifiers;
        public bool IsMemberMethod;

        public LuauQualifiedIdentifier(List<LuauIdentifier> identifiers, bool isMemberMethod)
        {
            Kind = ExpressionKind.QualifiedIdentifier;

            Identifiers = identifiers;
            IsMemberMethod = isMemberMethod;
        }

        public LuauQualifiedIdentifier(string qualifiedIdentifier)
        {
            Kind = ExpressionKind.QualifiedIdentifier;

            Identifiers = new();
            IsMemberMethod = qualifiedIdentifier.Contains(':');

            // Split the string and add the individual identifiers
            var identifiers = qualifiedIdentifier.Split('.', ':');
            foreach (string identifier in identifiers)
                Identifiers.Add(identifier);
        }

        public LuauQualifiedIdentifier()
        {
            Kind = ExpressionKind.QualifiedIdentifier;

            Identifiers = new();
            IsMemberMethod = false;
        }

        public override string? ToString()
        {
            if (IsMemberMethod)
            {
                StringBuilder builder = new();

                // Join them all together with a : in between the last two
                builder.AppendJoin('.', Identifiers.SkipLast(1)).
                    Append(':').Append(Identifiers.Last());

                return builder.ToString();
            }
            else
                return string.Join('.', Identifiers);

        }

        public static implicit operator LuauQualifiedIdentifier(string text) => new(new List<LuauIdentifier> { text }, false);
        public static implicit operator LuauQualifiedIdentifier(List<LuauIdentifier> identifiers) => new(identifiers, false);

        public static LuauQualifiedIdentifier Empty = new LuauQualifiedIdentifier(new(), false);
    }

    internal sealed class LuauLambda : LuauExpression
    {
        public LuauBlock Body;
        public List<LuauIdentifier> ParameterList;

        public LuauLambda(List<LuauIdentifier> parameterList)
        {
            Kind = ExpressionKind.Lambda;
            ParameterList = parameterList;
            Body = new();
        }

        public override string ToString()
        {
            StringBuilder builder = new();

            builder.AppendFormat("function({0}) \n", string.Join(", ", ParameterList));
            builder.AppendIndented(Body.ToString(), '\t');
            builder.Append("\nend");

            return builder.ToString();
        }
    }

    internal sealed class LuauParenthesizedExpression : LuauExpression
    {
        public LuauExpression Expression;

        public LuauParenthesizedExpression(LuauExpression expression)
        {
            Expression = expression;
        }

        public override bool DoesSomething() => false;

        public override string ToString()
        {
            return string.Format("({0})", Expression);
        }
    }

    internal sealed class LuauLiteralExpression : LuauExpression
    {
        public string? Text;

        public LuauLiteralExpression(ExpressionKind kind)
        {
            if (kind < ExpressionKind.TrueLiteral || kind > ExpressionKind.NilLiteral)
            {
                throw new ArgumentException("LuauLiteralExpression with no data has to be a keyword literal expression. Got: " +
                kind.ToString());
            }
            Kind = kind;
        }

        public LuauLiteralExpression(ExpressionKind kind, string text)
        {
            if (kind < ExpressionKind.NumericLiteral || kind > ExpressionKind.CharacterLiteral)
            {
                throw new ArgumentException("LuauLiteralExpression with text data has to be either a string, " +
                "a number or a character. Got" + kind.ToString());
            }
            Kind = kind;
            Text = text;
        }

        public override string? ToString()
        {
            switch (Kind)
            {
                case ExpressionKind.NumericLiteral:
                    return Text;
                case ExpressionKind.StringLiteral:
                    return '"' + Text + '"';
                case ExpressionKind.CharacterLiteral:
                    return "'" + Text + "'";
                case ExpressionKind.TrueLiteral:
                    return "true";
                case ExpressionKind.FalseLiteral:
                    return "false";
                case ExpressionKind.NilLiteral:
                    return "nil";
                default:
                    return null;
            }
        }
    }

    internal sealed class LuauTableExpression : LuauExpression
    {
        public List<LuauExpression> Contents;

        public LuauTableExpression()
        {
            Kind = ExpressionKind.Table;
            Contents = new();
        }

        public LuauTableExpression(List<LuauExpression> contents)
        {
            Kind = ExpressionKind.Table;
            Contents = contents;
        }

        public override string ToString()
        {
            StringBuilder stringBuilder = new StringBuilder();

            // Add the opening bracket
            stringBuilder.Append('{');

            // Fill all of the contents
            int index = 0;
            int lastIndex = Contents.Count() - 1;
            foreach (LuauExpression expression in Contents)
            {
                stringBuilder.Append(expression.ToString());
                if (index < lastIndex)
                    stringBuilder.Append(", ");
            }

            // Add the closing bracket
            stringBuilder.Append("}");

            return stringBuilder.ToString();
        }
    }

    internal sealed class LuauUnaryExpression : LuauExpression
    {
        public LuauExpression Expression;

        public LuauUnaryExpression(ExpressionKind kind, LuauExpression expression)
        {
            if (kind < ExpressionKind.UnaryPlus || kind > ExpressionKind.UnaryLogicalNot)
            {
                throw new ArgumentException("LuauUnaryExpression kind has to be a unary expression. Got " +
                kind.ToString());
            }
            Kind = kind;

            Expression = expression;
        }

        public override bool DoesSomething() => Expression.DoesSomething();

        public override string? ToString()
        {
            switch (Kind)
            {
                case ExpressionKind.UnaryPlus:
                    return "+" + Expression;
                case ExpressionKind.UnaryMinus:
                    return "-" + Expression;
                case ExpressionKind.UnaryLogicalNot:
                    return "not " + Expression;
                default:
                    return null;
            }
        }
    }

    internal sealed class LuauBinaryExpression : LuauExpression
    {
        public LuauExpression Left;
        public LuauExpression Right;

        public LuauBinaryExpression(ExpressionKind kind, LuauExpression left, LuauExpression right)
        {
            if (kind < ExpressionKind.NumericLiteral || kind > ExpressionKind.BinaryGreaterThanOrEqual)
            {
                throw new ArgumentException("LuauBinaryExpression kind has to be a binary expression. Got: " +
                kind.ToString());
            }
            Kind = kind;

            Left = left;
            Right = right;
        }

        public override bool DoesSomething() => Left.DoesSomething() || Right.DoesSomething();

        public override string? ToString()
        {
            switch (Kind)
            {
                case ExpressionKind.BinaryAdd:
                    return string.Format("{0} + {1}", Left, Right);
                case ExpressionKind.BinarySubtract:
                    return string.Format("{0} - {1}", Left, Right);
                case ExpressionKind.BinaryMultiply:
                    return string.Format("{0} * {1}", Left, Right);
                case ExpressionKind.BinaryDivide:
                    return string.Format("{0} / {1}", Left, Right);
                case ExpressionKind.BinaryModulo:
                    return string.Format("{0} % {1}", Left, Right);
                case ExpressionKind.BinaryConcatenate:
                    return string.Format("{0} .. {1}", Left, Right);
                case ExpressionKind.BinaryLogicalOr:
                    return string.Format("{0} or {1}", Left, Right);
                case ExpressionKind.BinaryLogicalAnd:
                    return string.Format("{0} and {1}", Left, Right);
                case ExpressionKind.BinaryEquals:
                    return string.Format("{0} == {1}", Left, Right);
                case ExpressionKind.BinaryNotEquals:
                    return string.Format("{0} ~= {1}", Left, Right);
                case ExpressionKind.BinaryLessThan:
                    return string.Format("{0} < {1}", Left, Right);
                case ExpressionKind.BinaryLessThanOrEqual:
                    return string.Format("{0} <= {1}", Left, Right);
                case ExpressionKind.BinaryGreaterThan:
                    return string.Format("{0} > {1}", Left, Right);
                case ExpressionKind.BinaryGreaterThanOrEqual:
                    return string.Format("{0} >= {1}", Left, Right);
                default:
                    return null;
            }
        }
    }
}