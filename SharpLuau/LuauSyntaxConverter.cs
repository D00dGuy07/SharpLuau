using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace SharpLuau
{
    internal static class LuauSyntaxConverter
    {
        public static string ConvertClass(ClassDeclarationSyntax classDecl, SemanticModel model)
        {
            StringBuilder classString = new();

            // Get class specific info
            string className = classDecl.Identifier.Text;

            // Create the luau class mimic header
            classString.AppendFormat("local {0} = {{}}\n", className);
            classString.AppendFormat("{0}.__index = {0}\n", className);
            classString.Append("\n");

            // Convert the body and append it
            foreach (var methodDecl in classDecl.Members.OfType<MethodDeclarationSyntax>())
                classString.AppendLine(ConvertMethod(methodDecl, model));

            // Create the module footer
            classString.AppendFormat("return {0}", className);

            return classString.ToString();
        }

        public static string ConvertMethod(MethodDeclarationSyntax methodDecl, SemanticModel model)
        {
            StringBuilder methodString = new();

            // Get the method symbol
            var methodSymbol = model.GetDeclaredSymbol(methodDecl);
            if (methodSymbol == null || methodDecl.Body == null)
                return "";

            // Figure out what style to use for this method
            string methodAccessor = methodSymbol.IsStatic ? "." : ":";
            string methodName = (methodSymbol.DeclaredAccessibility == Accessibility.Public ? "" : "_") + methodSymbol.Name;

            // Create the function signature
            methodString.AppendFormat("function {0}{1}{2}({3})\n", 
                methodSymbol.ContainingType.Name, methodAccessor, methodName,
                ConvertParameterListSyntax(methodDecl.ParameterList, model));

            // Build the body and add it indented
            methodString.AppendIndented(ConvertBlockSyntax(methodDecl.Body, model), '\t');

            // Create the end of the function
            methodString.Append("end\n");

            return methodString.ToString();
        }

        public static string ConvertParameterSyntax(ParameterSyntax parameter, SemanticModel model)
        {
            return parameter.Identifier.Text;
        }

        public static string ConvertParameterListSyntax(ParameterListSyntax parameterList, SemanticModel model)
        {
            StringBuilder parametersString = new();

            // Add all the parameter strings to a list
            List<string> parameterStrings = new();
            foreach (var parameter in parameterList.Parameters)
            {
                if (parameter != null)
                    parameterStrings.Add(ConvertParameterSyntax(parameter, model));
            }

            // Join the parameter strings with a comma and return them
            parametersString.AppendJoin(", ", parameterStrings);
            return parametersString.ToString();
        }

        public static string ConvertVariableDeclarator(VariableDeclaratorSyntax variableDeclarator, SemanticModel model)
        {
            if (variableDeclarator.Initializer != null)
            {
                // Convert the full expression and return the variable equal to it
                return String.Format("{0} = {1}", variableDeclarator.Identifier.Text,
                    ConvertSyntaxNode(variableDeclarator.Initializer.Value, model));
            }
            else
                // Declare but not define the variable
                return variableDeclarator.Identifier.Text;
        }

        public static string ConvertVariableDeclaration(VariableDeclarationSyntax variableDecl, SemanticModel model)
        {
            // I've heard that in Luau it is more efficient to do the declarations separately
            // I don't know how accurate that is but it's why I'm doing it that way

            StringBuilder declarationString = new();

            // Add each variable declaration
            foreach (var declaration in variableDecl.Variables)
                declarationString.AppendFormat("local {0}", ConvertVariableDeclarator(declaration, model));

            return declarationString.ToString();
        }

        public static string ConvertIdentifierNameSyntax(IdentifierNameSyntax identifierName, SemanticModel model)
        {
            return identifierName.Identifier.Text;
        }

        // Statement Syntax Conversions

        public static string ConvertBlockSyntax(BlockSyntax block, SemanticModel model)
        {
            StringBuilder blockString = new();

            // Convert each statement and append it to the string
            foreach (var statement in block.Statements)
            {
                if (statement != null)
                    blockString.AppendLine(ConvertSyntaxNode(statement, model));
            }

            return blockString.ToString();
        }

        public static string ConvertLocalDeclarationStatement(LocalDeclarationStatementSyntax statement, SemanticModel model)
        {
            return ConvertVariableDeclaration(statement.Declaration, model);
        }

        // Expression Syntax Conversions

        public static string ConvertLiteralExpression(LiteralExpressionSyntax expression, SemanticModel model)
        {
            switch (expression.Kind())
            {
                case SyntaxKind.NullLiteralExpression:
                    return "nil";
                case SyntaxKind.ArgListExpression: // I don't know if this one has a luau equivalent
                    Console.WriteLine("Unkown ArgListExpression detected");
                    return "(ARGLIST EXPRESSION WAS HERE)";
                default:
                    return expression.Token.ToString();
            }
            
        }

        public static string ConvertBinaryExpression(BinaryExpressionSyntax expression, SemanticModel model)
        {
            string left = ConvertSyntaxNode(expression.Left, model);
            string right = ConvertSyntaxNode(expression.Right, model);

            string op = "";
            switch (expression.Kind())
            {
                case SyntaxKind.AddExpression:
                    op = "+";
                    break;
                case SyntaxKind.SubtractExpression:
                    op = "=";
                    break;
                case SyntaxKind.MultiplyExpression:
                    op = "*";
                    break;
                case SyntaxKind.DivideExpression:
                    op = "/";
                    break;
                case SyntaxKind.ModuloExpression:
                    op = "%";
                    break;
                case SyntaxKind.LeftShiftExpression:
                    return String.Format("bit32.lshift({0}, {1})", left, right);
                case SyntaxKind.RightShiftExpression:
                    return String.Format("bit32.rshift({0}, {1})", left, right);
                case SyntaxKind.UnsignedRightShiftExpression:
                    Console.WriteLine("Unsigned right shift used. Converting to regular right shift.");
                    return String.Format("bit32.rshift({0}, {1})", left, right);
                case SyntaxKind.LogicalOrExpression:
                    op = "or";
                    break;
                case SyntaxKind.LogicalAndExpression:
                    op = "and";
                    break;
                case SyntaxKind.BitwiseOrExpression:
                    return String.Format("bit32.bor({0}, {1})", left, right);
                case SyntaxKind.BitwiseAndExpression:
                    return String.Format("bit32.band({0}, {1})", left, right);
                case SyntaxKind.ExclusiveOrExpression:
                    return String.Format("bit32.bxor({0}, {1})", left, right);
                case SyntaxKind.EqualsExpression:
                    op = "==";
                    break;
                case SyntaxKind.NotEqualsExpression:
                    op = "!=";
                    break;
                case SyntaxKind.LessThanExpression:
                    op = "<";
                    break;
                case SyntaxKind.LessThanOrEqualExpression:
                    op = "<=";
                    break;
                case SyntaxKind.GreaterThanExpression:
                    op = ">";
                    break;
                case SyntaxKind.GreaterThanOrEqualExpression:
                    op = ">=";
                    break;
                default:
                    Console.WriteLine("Unsupported expression used!");
                    break;
                   /*
                case SyntaxKind.IsExpression:
                    break;
                case SyntaxKind.AsExpression:
                    break;
                case SyntaxKind.CoalesceExpression:
                    break;
                   */
            }

            return String.Format("{0} {1} {2}", 
                left, op, right);
        }

        public static string ConvertSyntaxNode(CSharpSyntaxNode syntaxNode, SemanticModel model) 
        {
            switch (syntaxNode.Kind())
            {
                case SyntaxKind.ClassDeclaration:
                    return ConvertClass((ClassDeclarationSyntax)syntaxNode, model);
                case SyntaxKind.MethodDeclaration:
                    return ConvertMethod((MethodDeclarationSyntax)syntaxNode, model);
                case SyntaxKind.Parameter:
                    return ConvertParameterSyntax((ParameterSyntax)syntaxNode, model);
                case SyntaxKind.ParameterList:
                    return ConvertParameterListSyntax((ParameterListSyntax)syntaxNode, model);
                case SyntaxKind.VariableDeclarator:
                    return ConvertVariableDeclarator((VariableDeclaratorSyntax)syntaxNode, model);
                case SyntaxKind.VariableDeclaration:
                    return ConvertVariableDeclaration((VariableDeclarationSyntax)syntaxNode, model);
                case SyntaxKind.IdentifierName:
                    return ConvertIdentifierNameSyntax((IdentifierNameSyntax)syntaxNode, model);

                // Statements
                case SyntaxKind.Block:
                    return ConvertBlockSyntax((BlockSyntax)syntaxNode, model);
                case SyntaxKind.LocalDeclarationStatement:
                    return ConvertLocalDeclarationStatement((LocalDeclarationStatementSyntax)syntaxNode, model);

                // Expressions
                case SyntaxKind.ArgListExpression:
                case SyntaxKind.NumericLiteralExpression:
                case SyntaxKind.StringLiteralExpression:
                case SyntaxKind.Utf8StringLiteralExpression:
                case SyntaxKind.CharacterLiteralExpression:
                case SyntaxKind.TrueLiteralExpression:
                case SyntaxKind.FalseLiteralExpression:
                case SyntaxKind.NullLiteralExpression:
                case SyntaxKind.DefaultLiteralExpression:
                    return ConvertLiteralExpression((LiteralExpressionSyntax)syntaxNode, model);

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
                    return ConvertBinaryExpression((BinaryExpressionSyntax)syntaxNode, model);

                default:
                    Console.WriteLine("Undefined Syntax Conversion: {0}", syntaxNode.Kind().ToString());
                    return "";
            }
        }
    }
}
