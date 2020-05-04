/**************************************************************************************************
 * @(#)Common.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import java.util.*;

/**
 * Common expression nodes
 *
 * @author		Wesley Faler
 * @version		2011-07-06
**/
public class Common {
	private static TreeSet<String> seenNames = new TreeSet<String>();

	/** Node for Literal values **/
	public static class LiteralNode implements IExpressionNode {
		public Value value = null;

		public LiteralNode(Value valueToUse) {
			value = valueToUse;
		}

		public LiteralNode(int valueToUse) {
			value = new Value(valueToUse);
		}

		public LiteralNode(double valueToUse) throws Exception {
			value = new Value(valueToUse);
		}

		public LiteralNode(String valueToUse) throws Exception {
			if(valueToUse == null) {
				valueToUse = "";
			}
			if(valueToUse.length() > 0) {
				try {
					double t = Double.parseDouble(valueToUse);
					value = new Value(t);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(value == null) {
				value = new Value(valueToUse);
			}
		}

		public LiteralNode(LiteralNode other) {
			value = other.value;
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			String t = value.toString();
			if(t.equalsIgnoreCase("Infinity") || t.equalsIgnoreCase("-Infinity")) {
				throw new Exception("Infinite LiteralNode value");
			}
			return t;
		}

		public IExpressionNode deepCopy() throws Exception {
			return new LiteralNode(this);
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			return this;
		}
	}

	/** Node referencing a variable **/
	public static class VariableNode implements IExpressionNode {
		//static boolean didOne = false;
		public String variableName = "";

		public VariableNode(String variableToUse) {
			variableName = variableToUse;
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			Value result = null;
			if(variables != null) {
				result = variables.getValue(variableName);
			}
			return result;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return variables.getExpressionText(variableName);
			//return variableName;
		}

		public IExpressionNode deepCopy() throws Exception {
			return new VariableNode(variableName);
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			/*
			if(!Common.seenNames.contains(variableName)) {
				Common.seenNames.add(variableName);
				System.out.println("VariableName optimize for " + variableName);
			}
			boolean shouldLog = false;
			if(!didOne || variableName.equalsIgnoreCase("fp_T50.center")) {
				didOne = true;
				shouldLog = true;
			}
			*/
			if(optimizer.shouldRemainSymbolic(variableName)) {
				return this;
			}
			IExpressionNode namedExpression = optimizer.getExpression(variableName);
			if(namedExpression != null) {
				//if(shouldLog) {
				//	System.out.println("VariableName.optimize found named expression for " + variableName);
				//}
				optimizer.changed();
				return optimizer.optimize(namedExpression);
			}
			Value v = optimizer.getVariableValue(variableName);
			if(v != null) {
				//if(shouldLog) {
				//	System.out.println("VariableName.optimize found value for " + variableName);
				//}
				optimizer.changed();
				return new Common.LiteralNode(v);
			}
			//if(shouldLog) {
			//	System.out.println("VariableName.optimize found nothing for " + variableName);
			//}
			return this;
		}
	}

	/** Node with a name, like a variable, but referencing an expression **/
	public static class NamedExpressionNode implements IExpressionNode {
		//static boolean didOne = false;
		public String name = "";
		public IExpressionNode expression = null;

		public NamedExpressionNode(String nameToUse, IExpressionNode expressionToUse)
				throws Exception {
			name = nameToUse;
			expression = expressionToUse;
			if(expression == null) {
				throw new Exception("Syntax error: missing expression in named expression " + name);
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			if(expression != null) {
				return expression.evaluate(variables);
			} else {
				return null;
			}
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return expression.getExpressionText(variables);
		}

		public IExpressionNode deepCopy() throws Exception {
			return new NamedExpressionNode(name,expression.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			/*
			if(!Common.seenNames.contains(name)) {
				Common.seenNames.add(name);
				System.out.println("NamedExpressionNode optimize for " + name);
			}
			boolean shouldLog = false;
			if(!didOne || name.equalsIgnoreCase("fm_1.cmp_34.coeff1")) {
				didOne = true;
				shouldLog = true;
			}
			*/
			if(optimizer.shouldRemainSymbolic(name)) {
				optimizer.changed();
				return new VariableNode(name);
			}
			//if(shouldLog) {
			//	System.out.println("NamedExpressionNode optimizing " + name);
			//}
			optimizer.changed();
			return optimizer.optimize(expression);
		}
	}

	/** If node **/
	public static class IfNode implements IExpressionNode {
		public IExpressionNode condition = null;
		public IExpressionNode trueValue = null;
		public IExpressionNode falseValue = null;

		public IfNode(IExpressionNode conditionToUse,IExpressionNode trueValueToUse,
				IExpressionNode falseValueToUse) throws Exception {
			condition = conditionToUse;
			trueValue = trueValueToUse;
			falseValue = falseValueToUse;
			if(condition == null) {
				throw new Exception("Syntax error: missing condition in IF statement");
			}
			if(trueValue == null) {
				throw new Exception("Syntax error: missing true branch in IF statement");
			}
			if(falseValue == null) {
				throw new Exception("Syntax error: missing false branch in IF statement");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			Value ifValue = condition.evaluate(variables);
			if(ifValue.getBoolean()) {
				return trueValue.evaluate(variables);
			} else {
				return falseValue.evaluate(variables);
			}
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "if(" + condition.getExpressionText(variables)
					+ "," + trueValue.getExpressionText(variables)
					+ "," + falseValue.getExpressionText(variables)
					+ ")";
			/*
			return "(case when (" + condition.getExpressionText(variables)
					+ ") then (" + trueValue.getExpressionText(variables)
					+ ") else (" + falseValue.getExpressionText(variables)
					+ ") end)";
			*/
		}

		public IExpressionNode deepCopy() throws Exception {
			return new IfNode(condition.deepCopy(),trueValue.deepCopy(),falseValue.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			condition = optimizer.optimize(condition);
			trueValue = optimizer.optimize(trueValue);
			falseValue = optimizer.optimize(falseValue);

			// If the condition reduces to a literal value, then the only one branch
			// can be in effect.
			if(Optimizer.isLiteral(condition)) {
				optimizer.changed();
				boolean isTrue = ((LiteralNode)condition).value.getBoolean();
				if(isTrue) {
					return trueValue;
				} else {
					return falseValue;
				}
			}
			return this;
		}
	}

	/** Addition node **/
	public static class AddNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();

		public AddNode(IExpressionNode leftToUse,IExpressionNode rightToUse) throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in addition");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in addition");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			Value leftValue = left.evaluate(variables);
			Value rightValue = right.evaluate(variables);
			if(leftValue.isString() || rightValue.isString()) {
				//return new Value(leftValue.toString() + rightValue.toString());
				value.set(leftValue.toString() + rightValue.toString());
			} else {
				//return new Value(leftValue.getNumber() + rightValue.getNumber());
				value.set(leftValue.getNumber() + rightValue.getNumber());
			}
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ "+" + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new AddNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			boolean leftIsZero = Optimizer.isLiteral(left,0);
			boolean rightIsZero = Optimizer.isLiteral(right,0);
			if(leftIsZero && rightIsZero) {
				optimizer.changed();
				return Optimizer.zero;
			}
			if(leftIsZero) {
				optimizer.changed();
				return right;
			}
			if(rightIsZero) {
				optimizer.changed();
				return left;
			}
			if(Optimizer.isLiteral(right)) {
				// convert: "left + -number" into "left - number"
				double v = ((LiteralNode)right).value.getNumber();
				if(v < 0) {
					optimizer.changed();
					return new SubtractNode(left,new LiteralNode(-v));
				}
			}
			return this;
		}
	}

	/** Subtraction node **/
	public static class SubtractNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();

		public SubtractNode(IExpressionNode leftToUse,IExpressionNode rightToUse)
				throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in subtraction");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in subtraction");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(left.evaluate(variables).getNumber()
					- right.evaluate(variables).getNumber());
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ "-" + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new SubtractNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			boolean leftIsZero = Optimizer.isLiteral(left,0);
			boolean rightIsZero = Optimizer.isLiteral(right,0);
			if(leftIsZero && rightIsZero) {
				optimizer.changed();
				return Optimizer.zero;
			}
			if(leftIsZero) {
				optimizer.changed();
				return new Common.NegativeNode(right);
			}
			if(rightIsZero) {
				optimizer.changed();
				return left;
			}
			if(Optimizer.isLiteral(right)) {
				// convert: "left - -number" into "left + number"
				double v = ((LiteralNode)right).value.getNumber();
				if(v < 0) {
					optimizer.changed();
					return new AddNode(left,new LiteralNode(-v));
				}
			}
			return this;
		}
	}

	/** Multiply node **/
	public static class MultiplyNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();
		boolean leftMightBeZero = true;

		public MultiplyNode(IExpressionNode leftToUse,IExpressionNode rightToUse)
				throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in multiplication");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in multiplication");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			if(leftMightBeZero) {
				double leftNumber = left.evaluate(variables).getNumber();
				if(leftNumber == 0.0) {
					value.set(0.0);
				} else {
					double rightNumber = right.evaluate(variables).getNumber();
					if(rightNumber == 0.0) {
						leftMightBeZero = false;
						value.set(0.0);
					} else {
						value.set(leftNumber * rightNumber);
					}
				}
			} else {
				double rightNumber = right.evaluate(variables).getNumber();
				if(rightNumber == 0.0) {
					value.set(0.0);
				} else {
					double leftNumber = left.evaluate(variables).getNumber();
					if(leftNumber == 0.0) {
						leftMightBeZero = true;
						value.set(0.0);
					} else {
						value.set(leftNumber * rightNumber);
					}
				}
			}
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ "*" + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new MultiplyNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			boolean leftIsZero = Optimizer.isLiteral(left,0);
			boolean rightIsZero = Optimizer.isLiteral(right,0);
			if(leftIsZero || rightIsZero) {
				optimizer.changed();
				return Optimizer.zero;
			}
			boolean leftIsOne = Optimizer.isLiteral(left,1);
			boolean rightIsOne = Optimizer.isLiteral(right,1);
			if(leftIsZero && rightIsZero) {
				optimizer.changed();
				return Optimizer.one;
			}
			if(leftIsOne) {
				optimizer.changed();
				return right;
			}
			if(rightIsOne) {
				optimizer.changed();
				return left;
			}
			return this;
		}
	}

	/** Divide node **/
	public static class DivideNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();

		public DivideNode(IExpressionNode leftToUse,IExpressionNode rightToUse)
				throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in division");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in division");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			double leftNumber = left.evaluate(variables).getNumber();
			double rightNumber = right.evaluate(variables).getNumber();
			if(rightNumber == 0.0) {
				if(leftNumber == 0.0) {
					value.set(0.0); // 0/anything ==> 0
				} else {
					throw new Exception("Divide by 0 (" + leftNumber + "/" + rightNumber + "): "
							+ right.getExpressionText(variables));
				}
			}
			value.set(leftNumber / rightNumber);
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ "/" + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new DivideNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			IExpressionNode newRight = optimizer.optimize(right);
			boolean rightIsZero = Optimizer.isLiteral(newRight,0);

			if(rightIsZero) {
				System.out.println("Divide by 0: 0 from " + optimizer.getExpressionText(right));
			}

			right = newRight;
			left = optimizer.optimize(left);
			boolean leftIsZero = Optimizer.isLiteral(left,0);
			if(leftIsZero || rightIsZero) {
				optimizer.changed();
				return Optimizer.zero;
			}
			if(rightIsZero) {
				optimizer.changed();
				return Optimizer.one;
			}
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			if(Optimizer.isLiteral(right,1)) {
				optimizer.changed();
				return left;
			}
			if(Optimizer.isLiteral(right,-1)) {
				optimizer.changed();
				return new NegativeNode(left);
			}
			return this;
		}
	}

	/** Modulus node **/
	public static class ModulusNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();

		public ModulusNode(IExpressionNode leftToUse,IExpressionNode rightToUse)
				throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in modulus");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in modulus");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			int rightNumber = (int)right.evaluate(variables).getNumber();
			if(rightNumber == 0) {
				throw new Exception("Modulus by 0");
			}
			value.set((int)left.evaluate(variables).getNumber() % rightNumber);
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ "%" + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new ModulusNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Negative node **/
	public static class NegativeNode implements IExpressionNode {
		public IExpressionNode right = null;
		Value value = new Value();

		public NegativeNode(IExpressionNode rightToUse) throws Exception {
			right = rightToUse;
			if(right == null) {
				throw new Exception("Syntax error: missing right side in negation");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(-right.evaluate(variables).getNumber());
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "-" + right.getExpressionText(variables);
		}

		public IExpressionNode deepCopy() throws Exception {
			return new NegativeNode(right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			right = optimizer.optimize(right);
			if(right instanceof NegativeNode) {
				optimizer.changed();
				return ((NegativeNode)right).right;
			}
			if(Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Exp node **/
	public static class ExpNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public ExpNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in exp");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			double innerValue = 0;
			try {
				innerValue = inner.evaluate(variables).getNumber();
				double exp = Math.exp(innerValue);
				if(Double.isInfinite(exp)) {
					exp = 1e+12;
				}
				value.set(exp);
				return value;
			} catch(Exception e) {
				String message = e.getMessage()
						+ " in ExpNode, innerValue=" + innerValue + ", text="
						+ inner.getExpressionText(variables);
				throw new Exception(message);
			}
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "exp(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new ExpNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner,0)) {
				optimizer.changed();
				return Optimizer.one;
			}
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Ln node **/
	public static class LnNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public LnNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in ln");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			double t = inner.evaluate(variables).getNumber();
			if(t <= 0) {
				throw new Exception("Value out of range in ln(" + t + ")");
			}
			value.set(Math.log(t));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "ln(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new LnNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Sine node **/
	public static class SineNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public SineNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in sine");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(Math.sin(inner.evaluate(variables).getNumber()));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "sin(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new SineNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Cosine node **/
	public static class CosineNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public CosineNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in cosine");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(Math.cos(inner.evaluate(variables).getNumber()));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "cos(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new CosineNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Tangent node **/
	public static class TanNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public TanNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in tangent");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(Math.tan(inner.evaluate(variables).getNumber()));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "tan(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new TanNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Sqrt node **/
	public static class SqrtNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public SqrtNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in sqrt");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			double t = inner.evaluate(variables).getNumber();
			if( t < 0) {
				throw new Exception("Sqrt of negative number (" + t + ")");
			}
			value.set(Math.sqrt(t));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "sqrt(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new SqrtNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Power node **/
	public static class PowNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		Value value = new Value();

		public PowNode(IExpressionNode leftToUse,IExpressionNode rightToUse) throws Exception {
			left = leftToUse;
			right = rightToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in pow");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in pow");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			value.set(Math.pow(left.evaluate(variables).getNumber(),
					right.evaluate(variables).getNumber()));
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "pow(" + left.getExpressionText(variables)
					+ "," + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new PowNode(left.deepCopy(),right.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** Not node **/
	public static class NotNode implements IExpressionNode {
		public IExpressionNode inner = null;
		Value value = new Value();

		public NotNode(IExpressionNode innerToUse) throws Exception {
			inner = innerToUse;
			if(inner == null) {
				throw new Exception("Syntax error: missing value in NOT");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			boolean t = !inner.evaluate(variables).getBoolean();
			value.set(t?1:0);
			return value;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "not(" + inner.getExpressionText(variables) + ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new NotNode(inner.deepCopy());
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			inner = optimizer.optimize(inner);
			if(Optimizer.isLiteral(inner)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}

	/** And node **/
	public static class AndNode implements IExpressionNode {
		public ArrayList<IExpressionNode> nodes = new ArrayList<IExpressionNode>();

		public void add(IExpressionNode n) {
			nodes.add(n);
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			boolean result = true;
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				result = result && i.next().evaluate(variables).getBoolean();
				if(!result) {
					return Value.falseValue;
				}
			}
			return Value.trueValue;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			/*
			String result = "and(";
			for(int i=0;i<nodes.size();i++) {
				if(i > 0) {
					result += ",";
				}
				result += nodes.get(i).getExpressionText(variables);
			}
			result += ")";
			*/
			String result = "(";
			for(int i=0;i<nodes.size();i++) {
				if(i > 0) {
					result += " and ";
				}
				result += "(" + nodes.get(i).getExpressionText(variables) + ")";
			}
			result += ")";

			return result;
		}

		public IExpressionNode deepCopy() throws Exception {
			AndNode result = new AndNode();
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				result.add(i.next().deepCopy());
			}
			return result;
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			ArrayList<IExpressionNode> results = new ArrayList<IExpressionNode>();
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				IExpressionNode n = i.next();
				IExpressionNode r = optimizer.optimize(n);

/*
if(Optimizer.isLiteral(r,0)) {
	System.out.println("AND replacing node:" + n.toString() + " with " + r.toString());

	if(n instanceof Common.LiteralNode) {
		Common.LiteralNode l = (Common.LiteralNode)n;
		System.out.println("l.value.isString=" + l.value.isString() + ", isNumber=" + l.value.isNumber());
	}

	r = Optimizer.zero;
}
*/


				if(Optimizer.isLiteral(r,0)) {
					optimizer.changed();
					return Optimizer.zero;
				}

				results.add(r);
			}
			nodes = results;
			return this;
		}
	}

	/** Or node **/
	public static class OrNode implements IExpressionNode {
		public ArrayList<IExpressionNode> nodes = new ArrayList<IExpressionNode>();

		public void add(IExpressionNode n) {
			nodes.add(n);
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			boolean result = false;
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				result = result || i.next().evaluate(variables).getBoolean();
				if(result) {
					return Value.trueValue;
				}
			}
			return Value.falseValue;
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			/*
			String result = "or(";
			for(int i=0;i<nodes.size();i++) {
				if(i > 0) {
					result += ",";
				}
				result += nodes.get(i).getExpressionText(variables);
			}
			result += ")";
			*/

			String result = "(";
			for(int i=0;i<nodes.size();i++) {
				if(i > 0) {
					result += " or ";
				}
				result += "(" + nodes.get(i).getExpressionText(variables) + ")";
			}
			result += ")";
			return result;
		}

		public IExpressionNode deepCopy() throws Exception {
			OrNode result = new OrNode();
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				result.add(i.next().deepCopy());
			}
			return result;
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			ArrayList<IExpressionNode> results = new ArrayList<IExpressionNode>();
			for(Iterator<IExpressionNode> i=nodes.iterator();i.hasNext();) {
				IExpressionNode r = optimizer.optimize(i.next());
				if(Optimizer.isLiteral(r,1)) {
					optimizer.changed();
					return Optimizer.one;
				}
				results.add(r);
			}
			nodes = results;
			return this;
		}
	}

	/** Comparison node **/
	public static class ComparisonNode implements IExpressionNode {
		public IExpressionNode left = null;
		public IExpressionNode right = null;
		public String comparison = "";

		public ComparisonNode(IExpressionNode leftToUse,IExpressionNode rightToUse,
				String comparisonToUse) throws Exception {
			left = leftToUse;
			right = rightToUse;
			comparison = comparisonToUse;
			if(left == null) {
				throw new Exception("Syntax error: missing left side in comparison");
			}
			if(right == null) {
				throw new Exception("Syntax error: missing right side in comparison");
			}
		}

		public Value evaluate(IVariableSource variables) throws Exception {
			Value leftValue = left.evaluate(variables);
			Value rightValue = right.evaluate(variables);

			if(comparison.equals("=") || comparison.equals("==")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(leftValue.toString().equalsIgnoreCase(rightValue.toString())) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() == rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else if(comparison.equals("<>") || comparison.equals("!=")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(!leftValue.toString().equalsIgnoreCase(rightValue.toString())) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() != rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else if(comparison.equals("<")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(leftValue.toString().compareToIgnoreCase(rightValue.toString())<0) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() < rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else if(comparison.equals("<=")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(leftValue.toString().compareToIgnoreCase(rightValue.toString())<=0) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() <= rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else if(comparison.equals(">")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(leftValue.toString().compareToIgnoreCase(rightValue.toString())>0) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() > rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else if(comparison.equals(">=")) {
				if(leftValue.isString() && rightValue.isString()) {
					if(leftValue.toString().compareToIgnoreCase(rightValue.toString())>=0) {
						return Value.trueValue;
					}
				} else if(leftValue.getNumber() >= rightValue.getNumber()) {
					return Value.trueValue;
				}
				return Value.falseValue;
			} else {
				throw new Exception("Syntax error: unknown comparison operator " + comparison);
			}
		}

		public String getExpressionText(IVariableSource variables) throws Exception {
			return "(" + left.getExpressionText(variables)
					+ comparison + right.getExpressionText(variables)
					+ ")";
		}

		public IExpressionNode deepCopy() throws Exception {
			return new ComparisonNode(left.deepCopy(),right.deepCopy(),comparison);
		}

		public IExpressionNode optimize(Optimizer optimizer) throws Exception {
			left = optimizer.optimize(left);
			right = optimizer.optimize(right);
			if(Optimizer.isLiteral(left) && Optimizer.isLiteral(right)) {
				optimizer.changed();
				return new LiteralNode(evaluate(null));
			}
			return this;
		}
	}
}

