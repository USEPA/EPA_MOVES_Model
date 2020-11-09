package gov.epa.otaq.moves.utils;

public class FormatUtil {
	/**
	 * Formats an int into a string with the specified number of digits, padded
	 * with leading zeros if the number has less digits than specified. If the
	 * input number already has more digits than specified, the output string
	 * will not be truncated to the specified length.
	 * 
	 * @param numberToFormat
	 *            The number that is to be formatted
	 * @param numberOfOutputDigits
	 *            The number of digits in the resultant format string
	 **/
	public static String formatWithLeadingZeros(int numberToFormat,
			int numberOfOutputDigits) {
		if (numberOfOutputDigits <= 0) {
			return "";
		}
		Integer i = Integer.valueOf(numberToFormat);
		String s = i.toString();
		String result = "";
		for (int j = 0, count = numberOfOutputDigits - s.length(); j < count; j++) {
			result += "0";
		}
		result += s;
		return result;
	}

	/**
	 * Ensures that a string is exactly the specified number of characters long,
	 * padded with trailing characters if the string has less characters than
	 * specified. If the input string already has more digits than specified,
	 * the output string will not be truncated to the specified length.
	 * 
	 * @param stringToSize
	 *            The number that is to be formatted
	 * @param numberOfOutputCharacters
	 *            The number of digits in the resultant format string
	 * @param padCharacter
	 *            The character to pad the string with.
	 * @return The padded number string.
	 **/
	public static String ensureStringLength(String stringToSize,
			int numberOfOutputCharacters, char padCharacter) {
		if (numberOfOutputCharacters <= 0) {
			return "";
		}
		String result = stringToSize;
		if (result.length() > numberOfOutputCharacters) {
			result = result.substring(0, numberOfOutputCharacters);
		} else if (result.length() < numberOfOutputCharacters) {
			for (int i = result.length(); i < numberOfOutputCharacters; i++) {
				result += padCharacter;
			}
		}
		return result;
	}

	/**
	 * @param array
	 * @param delimiter
	 * @return
	 */
	public static String intArrayToString(int[] array, String delimiter) {
		String retStr = "";

		for (int a : array) {
			retStr += a + delimiter;
		}

		if (retStr.endsWith(delimiter)) {
			retStr = retStr.substring(0, retStr.length() - 1);
		}

		return retStr;
	}

	/**
	 * @param num
	 * @param maxWidth
	 *            : including floating point
	 * @param maxPrecision
	 * @return
	 */
	public static String formatFloatToString(double num1, int maxWidth,
			int maxPrecision) {
		String retStr = "";

		double num = num1;
		String numStr = Double.toString(num);
		int dotPos = numStr.indexOf(".");
		if (dotPos < 0) { // nothing after floating point
			retStr = numStr.substring(numStr.length() - maxWidth,
					numStr.length());
		} else {

			String floatStr = numStr.substring(dotPos + 1);
			if (floatStr.equals("0")) { // nothing after floating point
				String intStr = numStr.substring(0, dotPos);
				retStr = numStr.substring(
						intStr.length() - maxWidth > 0 ? intStr.length()
								- maxWidth : 0, intStr.length());
			} else {
				String intStr = numStr.substring(0, dotPos);
				if (intStr.length() >= maxWidth) {
					retStr = intStr.substring(intStr.length() - maxWidth,
							intStr.length());
				} else if (intStr.length() >= maxWidth - 1) {
					retStr = intStr;
				} else if (intStr.length() >= maxWidth - 1 - maxPrecision) {
					int maxPrecisionActual = maxWidth - 1 - intStr.length();
					if (floatStr.length() > maxPrecisionActual) {
						floatStr = floatStr.substring(0, maxPrecision);
					}
					retStr = intStr + "." + floatStr;
				} else {
					if (floatStr.length() > maxPrecision) {
						floatStr = floatStr.substring(0, maxPrecision);
					}
					retStr = intStr + "." + floatStr;
				}
			}
		}

		return retStr;
	}

	public static void main(String[] args) {
		// int[] array = { 1, 2, 3, 4 };
		// System.out.println(FormatUtil.intArrayToString(array, ","));
		System.out.println("23456 : "
				+ FormatUtil.formatFloatToString(123456.000, 5, 2));
		System.out.println("123.2 : "
				+ FormatUtil.formatFloatToString(123.2, 5, 2));
		System.out.println("99999 : "
				+ FormatUtil.formatFloatToString(99999.234, 5, 2));
		System.out.println("2345 : "
				+ FormatUtil.formatFloatToString(2345, 5, 2));
		System.out.println("99.99 : "
				+ FormatUtil.formatFloatToString(99.99, 5, 2));
		System.out.println("99.99 : "
				+ FormatUtil.formatFloatToString(99.99123, 5, 2));
		System.out.println("9.99 : "
				+ FormatUtil.formatFloatToString(9.99123, 5, 2));
	}
}
