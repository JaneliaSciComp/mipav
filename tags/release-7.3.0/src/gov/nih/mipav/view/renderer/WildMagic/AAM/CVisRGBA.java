package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 *  * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
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