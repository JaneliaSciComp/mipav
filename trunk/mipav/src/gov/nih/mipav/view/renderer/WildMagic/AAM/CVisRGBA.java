package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * 
 * INTERNAL EXTERNAL VISRGBAPIXELS
 *
 * Copyright © 1996-2000 Microsoft Corporation, All Rights Reserved
 *
 * class CVisRGBA 
 * 
 * Object containing red, green, blue, and alpha values. Used as pixels in color images. 
 * (This class also includes standard arithmetic operators that are not listed 
 * in this documentation.)    Java modification from the CVisRGBA.cpp file. 
 * 
 * The type of numbers used to store the red, green, blue, and alpha values.
 * 
 * @author Ruida Cheng
 */
public class CVisRGBA {

	// The blue component.
	protected byte m_numB;

	// The green component.
	protected byte m_numG;

	// The red component.
	protected byte m_numR;

	// The alpha component.
	protected byte m_numA;

	// Default constructor.
	public CVisRGBA() {

	}

	// Constructor (from a single number).
	public CVisRGBA(byte num) {
		m_numR = num;
		m_numG = num;
		m_numB = num;
	}


	// Constructor from an other <c CVisRGBA> object.
	public CVisRGBA(final CVisRGBA refrgba) {
		m_numR = refrgba.R();
		m_numG = refrgba.G();
		m_numB = refrgba.B();
		m_numA = refrgba.A();

	}

	// Constructor (from red, green, and blue values).  Alpha will not be
	// initialized.
	public CVisRGBA(byte numR, byte numG, byte numB) {
		m_numR = numR;
		m_numG = numG;
		m_numB = numB;
	}

	// Constructor (from red, green, blue, and alpha values).
	public CVisRGBA(byte numR, byte numG, byte numB, byte numA) {
		m_numR = numR;
		m_numG = numG;
		m_numB = numB;
		m_numA = numA;
	}

	// Assignment operator.
	public CVisRGBA assign(final CVisRGBA refrgba) {
		SetR(refrgba.R());
		SetG(refrgba.G());
		SetB(refrgba.B());
		SetA(refrgba.A());

		return this;
	}

	// Assignment operator.
	public CVisRGBA assign(byte num) {
		SetR(num);
		SetG(num);
		SetB(num);

		return this;
	}

	public boolean equals(final CVisRGBA refrgba) {
		return ((R() == refrgba.R()) && (G() == refrgba.G())
				&& (B() == refrgba.B()) && (A() == refrgba.A()));
	}

	public boolean notequals(final CVisRGBA refrgba) {
		return !this.equals(refrgba);
	}

	
	// Set the red component value.
	public void SetR(byte num) {
		m_numR = num;
	}

	// Set the green component value.
	public void SetG(byte num) {
		m_numG = num;
	}

	// Set the blue component value.
	public void SetB(byte num) {
		m_numB = num;
	}

	// Set the alpha component value.
	public void SetA(byte num) {
		m_numA = num;
	}

	// Set the red, green, and blue values.
	public void SetRGB(byte numR, byte numG, byte numB) {
		m_numR = numR;
		m_numG = numG;
		m_numB = numB;
	}

	// Set the red, green, blue, and alpha values.
	public void SetRGBA(byte numR, byte numG, byte numB, byte numA) {
		m_numR = numR;
		m_numG = numG;
		m_numB = numB;
		m_numA = numA;
	}

	public CVisRGBA add_into(byte num) {
		m_numR += num;
		m_numG += num;
		m_numB += num;

		return this;
	}

	public CVisRGBA add_into(final CVisRGBA refrgba) {
		m_numR += refrgba.R();
		m_numG += refrgba.G();
		m_numB += refrgba.B();

		return this;
	}

	public CVisRGBA sub_into(byte num) {
		m_numR -= num;
		m_numG -= num;
		m_numB -= num;

		return this;
	}

	public CVisRGBA sub_into(final CVisRGBA refrgba) {
		m_numR -= refrgba.R();
		m_numG -= refrgba.G();
		m_numB -= refrgba.B();

		return this;
	}

	public CVisRGBA mult_into(int i) {
		m_numR = (byte) (m_numR * i);
		m_numG = (byte) (m_numG * i);
		m_numB = (byte) (m_numB * i);

		return this;
	}

	/*
	 * public CVisRGBA mult_into(int ui) { m_numR = (byte) (m_numR * ui); m_numG
	 * = (byte) (m_numG * ui); m_numB = (byte) (m_numB * ui);
	 * 
	 * return this; }
	 */

	public CVisRGBA mult_into(long l) {
		m_numR = (byte) (m_numR * l);
		m_numG = (byte) (m_numG * l);
		m_numB = (byte) (m_numB * l);

		return this;
	}

	public CVisRGBA mult_into(float flt) {
		m_numR = (byte) (m_numR * flt);
		m_numG = (byte) (m_numG * flt);
		m_numB = (byte) (m_numB * flt);

		return this;
	}

	public CVisRGBA mult_into(double dbl) {
		m_numR = (byte) (m_numR * dbl);
		m_numG = (byte) (m_numG * dbl);
		m_numB = (byte) (m_numB * dbl);

		return this;
	}

	public CVisRGBA div_into(int i) {
		m_numR = (byte) (m_numR / i);
		m_numG = (byte) (m_numG / i);
		m_numB = (byte) (m_numB / i);

		return this;
	}

	public CVisRGBA div_into(long l) {
		m_numR = (byte) (m_numR / l);
		m_numG = (byte) (m_numG / l);
		m_numB = (byte) (m_numB / l);

		return this;
	}

	public CVisRGBA div_into(float flt) {
		m_numR = (byte) (m_numR / flt);
		m_numG = (byte) (m_numG / flt);
		m_numB = (byte) (m_numB / flt);

		return this;
	}

	public CVisRGBA div_into(double dbl) {
		m_numR = (byte) (m_numR / dbl);
		m_numG = (byte) (m_numG / dbl);
		m_numB = (byte) (m_numB / dbl);

		return this;
	}

	public CVisRGBA add(final CVisRGBA refrgba1, final CVisRGBA refrgba2) {
		return new CVisRGBA((byte) (refrgba1.R() + refrgba2.R()),
				(byte) (refrgba1.G() + refrgba2.G()),
				(byte) (refrgba1.B() + refrgba2.B()));
	}

	public CVisRGBA sub(final CVisRGBA refrgba1, final CVisRGBA refrgba2) {
		return new CVisRGBA((byte) (refrgba1.R() - refrgba2.R()),
				(byte) (refrgba1.G() - refrgba2.G()),
				(byte) (refrgba1.B() - refrgba2.B()));
	}

	public CVisRGBA mult(final CVisRGBA refrgba, int i) {
		return new CVisRGBA((byte) (refrgba.R() * i), (byte) (refrgba.G() * i),
				(byte) (refrgba.B() * i));
	}

	public CVisRGBA mult(final CVisRGBA refrgba, long l) {
		return new CVisRGBA((byte) (refrgba.R() * l), (byte) (refrgba.G() * l),
				(byte) (refrgba.B() * l));
	}

	public CVisRGBA mult(final CVisRGBA refrgba, float flt) {
		return new CVisRGBA((byte) (refrgba.R() * flt),
				(byte) (refrgba.G() * flt), (byte) (refrgba.B() * flt));
	}

	public CVisRGBA mult(final CVisRGBA refrgba, double dbl) {
		return new CVisRGBA((byte) (refrgba.R() * dbl),
				(byte) (refrgba.G() * dbl), (byte) (refrgba.B() * dbl));
	}

	public CVisRGBA mult(int i, final CVisRGBA refrgba) {
		return new CVisRGBA((byte) (refrgba.R() * i), (byte) (refrgba.G() * i),
				(byte) (refrgba.B() * i));
	}

	public CVisRGBA mult(long l, final CVisRGBA refrgba) {
		return new CVisRGBA((byte) (refrgba.R() * l), (byte) (refrgba.G() * l),
				(byte) (refrgba.B() * l));
	}

	public CVisRGBA mult(float flt, final CVisRGBA refrgba) {
		return new CVisRGBA((byte) (refrgba.R() * flt),
				(byte) (refrgba.G() * flt), (byte) (refrgba.B() * flt));
	}

	public CVisRGBA mult(double dbl, final CVisRGBA refrgba) {
		return new CVisRGBA((byte) (refrgba.R() * dbl),
				(byte) (refrgba.G() * dbl), (byte) (refrgba.B() * dbl));
	}

	public CVisRGBA div(final CVisRGBA refrgba, int i) {
		return new CVisRGBA((byte) (refrgba.R() / i), (byte) (refrgba.G() / i),
				(byte) (refrgba.B() / i));
	}

	public CVisRGBA div(final CVisRGBA refrgba, long l) {
		return new CVisRGBA((byte) (refrgba.R() / l), (byte) (refrgba.G() / l),
				(byte) (refrgba.B() / l));
	}

	public CVisRGBA div(final CVisRGBA refrgba, float flt) {
		return new CVisRGBA((byte) (refrgba.R() / flt),
				(byte) (refrgba.G() / flt), (byte) (refrgba.B() / flt));
	}

	public CVisRGBA div(final CVisRGBA refrgba, double dbl) {
		return new CVisRGBA((byte) (refrgba.R() / dbl),
				(byte) (refrgba.G() / dbl), (byte) (refrgba.B() / dbl));
	}

	public byte R() {
		return m_numR;
	}

	public byte G() {
		return m_numG;
	}

	public byte B() {
		return m_numB;
	}

	public byte A() {
		return m_numA;
	}

}