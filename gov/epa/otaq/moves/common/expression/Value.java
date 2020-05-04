/**************************************************************************************************
 * @(#)Value.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

/**
 * A polymorphic value type.
 *
 * @author		Wesley Faler
 * @version		2011-07-06
**/
public class Value {
	/** Code used when doubleValue is to be used **/
	private static int DOUBLE_TYPE = 1;
	/** Code used when textValue is to be used **/
	private static int STRING_TYPE = 2;

	/** Convenient object holding a value that translates a True boolean **/
	public static Value trueValue = new Value((int)1);
	/** Convenient object holding a value that translates a False boolean **/
	public static Value falseValue = new Value((int)0);

	/** Type of the variable **/
	private int type = DOUBLE_TYPE;
	/** Numeric value of the variable **/
	private double doubleValue = 0;
	/** Textual value of the variable **/
	private String textValue = "";

	/**
	 * Constructor
	**/
	public Value() {
	}

	/**
	 * Constructor
	 * @param value value to be set
	**/
	public Value(int value) {
		try {
			set(value);
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/**
	 * Constructor
	 * @param value value to be set
	 * @throws Exception if an infinite or NaN value is used
	**/
	public Value(double value) throws Exception {
		set(value);
	}

	/**
	 * Constructor
	 * @param value value to be set
	**/
	public Value(String value) {
		set(value);
	}

	/**
	 * Copy constructor
	 * @param other another Value to be copied from
	**/
	public Value(Value other) {
		copyFrom(other);
	}

	/**
	 * Copy from another copy
	 * @param other another Value to be copied from
	**/
	public void copyFrom(Value other) {
		type = other.type;
		doubleValue = other.doubleValue;
		textValue = other.textValue;
	}

	/**
	 * Check the type of the data
	 * @return true if the value is a String
	**/
	public boolean isString() {
		return type == STRING_TYPE;
	}

	/**
	 * Check the type of the data
	 * @return true if the value is a number
	**/
	public boolean isNumber() {
		return type == DOUBLE_TYPE;
	}

	/**
	 * Set the value
	 * @param value value to be set
	 * @return true if the new value or type is different
	 * @throws Exception if an infinite or NaN value is used
	**/
	public boolean set(double value) throws Exception {
		if(Double.isInfinite(value)) {
			throw new Exception("Infinite value set");
		}
		if(Double.isNaN(value)) {
			throw new Exception("NaN value set");
		}
		if(type != DOUBLE_TYPE) {
			type = DOUBLE_TYPE;
			doubleValue = value;
			return true;
		} else if(doubleValue != value) {
			doubleValue = value;
			return true;
		}
		return false;
	}

	/**
	 * Set the value
	 * @param value value to be set
	 * @return true if the new value or type is different
	**/
	public boolean set(String value) {
		if(value == null) {
			value = "";
		}
		if(type != STRING_TYPE) {
			type = STRING_TYPE;
			textValue = value;
			return true;
		} else if(textValue.length() != value.length()) {
			textValue = value;
			return true;
		} else if(!textValue.equals(value)) {
			textValue = value;
			return true;
		}
		return false;
	}

	/**
	 * Get the textual value of the variable
	 * @return textual value of the variable
	**/
	public String toString() {
		if(type == STRING_TYPE) {
			return textValue;
		}
		return "" + doubleValue;
	}

	/**
	 * Get the numeric value of the variable
	 * @return numeric value of the variable
	**/
	public double getNumber() {
		if(type == DOUBLE_TYPE) {
			return doubleValue;
		}
		try {
			return Double.parseDouble(textValue);
		} catch(Exception e) {
			return 0;
		}
	}

	/**
	 * Get the boolean value of the variable
	 * @return boolean value of the variable
	**/
	public boolean getBoolean() {
		if(type == DOUBLE_TYPE) {
			return doubleValue != 0.0;
		}
		if(textValue == null || textValue.length() <= 0) {
			return false;
		}
		char t = textValue.charAt(0);
		switch(t) {
			case 'Y':
			case 'y':
			case 'T':
			case 't':
			case '1':
			case '-': 	// "-1" is also used for "true" since it is an integer with all
						// bits set to 1.
				return true;
		}
		return false;
	}

	/** Display text for debugging purposes **/
	public void debugPrint() {
		String t = "type: " + type + ", doubleValue=" + doubleValue + ", textValue=\"" + textValue + "\"";
		if(this == trueValue) {
			t += ", is trueValue object";
		} else if(this == falseValue) {
			t += ", is falseValue object";
		} else {
			t += ", is other value object";
		}
		System.out.println(t);
	}
}
