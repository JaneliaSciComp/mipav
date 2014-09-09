package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * Copyright © 2000, 2001, 2002, 2003 by Mikkel B.
 * Stegmann IMM, Informatics & Mathmatical Modelling Technical University of
 * Denmark Richard Petersens Plads Building 321 DK-2800 Lyngby, Denmark
 * http://www.imm.dtu.dk/
 * 
 * Author: Mikkel B. Stegmann - http://www.imm.dtu.dk/~aam/ - aam@imm.dtu.dk
 * 
 * $Id: AAMdef.h,v 1.2 2003/01/20 10:29:15 aam Exp $
 * 
 * AAMDef defines a few global central math functions.
 * 
 * @author Ruida Cheng
 * 
 */
public class AAMdef {

	/**
	 * define the number of bands used in the AAM model. 1 for single intensity
	 * channel of MRI prostate grayscale image. 3 for the RGB channels.
	 * */
	public static int BANDS = 1;

	/** define the max function with two given double values. */

	/**
	 * define the max function with two given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @return the max of the two given numbers
	 */
	public static double AAM_MAX2(double a, double b) {
		return a > b ? a : b;
	}

	/**
	 * define the max function for three given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @param c
	 *            third number
	 * @return max value from the three
	 */
	public static double AAM_MAX3(double a, double b, double c) {
		return AAM_MAX2(AAM_MAX2(a, b), c);
	}

	/**
	 * define the min function for two given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @return min value from the two
	 */
	public static double AAM_MIN2(double a, double b) {
		return a < b ? a : b;
	}

	/**
	 * define the min function for three given double values.
	 * 
	 * @param a
	 *            first number
	 * @param b
	 *            second number
	 * @param c
	 *            third number
	 * @return min value from the three
	 */
	public static double AAM_MIN3(double a, double b, double c) {
		return AAM_MIN2(AAM_MIN2(a, b), c);
	}

	/**
	 * Round up function
	 * 
	 * @param x
	 *            double precision value
	 * @return round up integer value
	 */
	public static int AAMRound(double x) {
		return (int) (.5 + x);
	}

	/**
	 * Set the bands for the AAM model.
	 * 
	 * @param AAM_3BAND
	 *            true == color RGB channel, false == intensity channel
	 */
	public void setBands(boolean AAM_3BAND) {
		if (AAM_3BAND == true) {
			BANDS = 3;
		} else {
			BANDS = 1;
		}
	}
}