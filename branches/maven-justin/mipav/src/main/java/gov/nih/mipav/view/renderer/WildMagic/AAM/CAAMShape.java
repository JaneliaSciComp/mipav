package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.*;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * AAM-API LICENSE  -  file: license.txt
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
 * 
 * Shape container. This class act as a container for a shape. Essentially it's
 * just a set of 2D points stored in a vector in the format xxxyyy.
 * 
 * @author Ruida Cheng
 */
public class CAAMShape extends CDVector {

	/****************************************
	 * Remember to modify the CopyData() method, each time a new data field is
	 * added.
	 ****************************************/

	/** Auxillary point data. */
	private Vector<CAAMPointInfo> m_vPointAux = new Vector<CAAMPointInfo>();

	/** User-defined field 1. */
	private Vector<Float> m_vUser1 = new Vector<Float>();

	/** User-defined field 2. */
	private Vector<Float> m_vUser2 = new Vector<Float>();

	/** User-defined field 3. */
	private Vector<Float> m_vUser3 = new Vector<Float>();

	/** Current ASF version number. */
	private String m_szASFVer = new String();

	/** The number of points. */
	private int m_iNbPoints;

	/** Indicates if the point coordinates is in relative or absolute format. */
	private boolean m_bAbsPointCoordinates;

	/** Optional 'host image' filename including full path. */
	private String m_szHostImage = new String();

	/** host image. */
	private ModelImage myHostImage;

	/** un-safe pointer to the first point data element. */
	private double[] m_pData;

	/**
	 * Default constructor. Defaults to absolute shape coordinates and sets the
	 * number of points to zero.
	 * 
	 * Notice that the user defined fields is *not* allocated by default.
	 * 
	 * In order to preserved memory the user must to this explicitly by using
	 * AllocateUserFields();
	 */
	public CAAMShape() {

		m_iNbPoints = 0;
		m_szHostImage = new String("");
		m_szASFVer = new String("1.0");
		m_bAbsPointCoordinates = true;

		m_pData = this.m_data;
	}

	/**
	 * Constructs a shape with 'nbPoints' points. Defaults to absolute point
	 * coordinates and single closed path connectivity.
	 * 
	 * @see SetClosedPathConnectivity
	 * @param nbPoints
	 *            The number of points the shape should contain.
	 */
	public CAAMShape(int nbPoints) {

		m_szHostImage = new String("");
		m_bAbsPointCoordinates = true;
		m_iNbPoints = nbPoints;
		this.Resize(2 * m_iNbPoints);

		SetClosedPathConnectivity();
	}

	/**
	 * Constructs a shape from a vector. Defaults to absolute point coordinates
	 * and single closed path connectivity.
	 * 
	 * @see SetClosedPathConnectivity
	 * @param v
	 *            The input vector.
	 */
	public CAAMShape(final CDVector v) {

		m_szHostImage = "";

		m_bAbsPointCoordinates = true;
		assert (v.Length() % 2 == 0);

		Resize(v.Length());
		this.assign(v);

		m_pData = this.m_data;

		SetClosedPathConnectivity();
	}

	/**
	 * Copy contructor.
	 * 
	 * @param s
	 *            The input shape.
	 */
	public CAAMShape(final CAAMShape s) {
		this.CopyData(s);
	}

	/**
	 * Write the shape to a binary file.
	 * 
	 * @param sFilename
	 *            Destination filename.
	 */
	public void ToFile(DataOutputStream fh) {

		try {
			// write point data
			super.ToFile(fh);

			assert (m_iNbPoints == m_vPointAux.size());

			// write aux point data
			for (int i = 0; i < m_iNbPoints; i++) {
				m_vPointAux.get(i).ToFile(fh);
			}

			byte[] temp = m_szHostImage.getBytes();
			byte[] buf = new byte[256];
			int temp_size = temp.length;
			for (int i = 0; i < temp_size; i++) {
				buf[i] = temp[i];
			}
			fh.write(buf, 0, 256);

			// abs/rel status
			int isAbs;
			isAbs = m_bAbsPointCoordinates ? 1 : 0;
			fh.writeInt(isAbs);
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	/**
	 * Reads a shape from a file.
	 * 
	 * @param fh
	 *            An open file handle.
	 */
	public void FromFile(DataInputStream fh) {

		assert (fh != null);
		try {
			// read data
			super.FromFile(fh);

			// update 'm_iNbPoints'
			assert (Length() % 2 == 0);
			m_iNbPoints = Length() / 2;

			m_vPointAux.clear();

			// read aux point data
			for (int i = 0; i < m_iNbPoints; i++) {

				CAAMPointInfo pi = new CAAMPointInfo();
				// fread( &pi, sizeof(CAAMPointInfo), 1, fh );
				pi.FromFile(fh);
				m_vPointAux.add(pi);
			}

			// char buf[256];
			// for(int p=0;p<256;p++) buf[p] = fgetc( fh );
			// m_szHostImage = fh.read
			byte[] buf = new byte[256];
			fh.read(buf, 0, 256);
			char[] temp = new char[256];
			for (int k = 0; k < 256; k++) {
				temp[k] = (char) buf[k];
			}
			m_szHostImage = m_szHostImage.valueOf(temp).trim();

			// read abs/rel status
			int isAbs;
			// fread( &isAbs, sizeof(int), 1, fh );
			isAbs = fh.readInt();
			m_bAbsPointCoordinates = isAbs == 1;

			m_bAbsPointCoordinates = true;

			// restore un-safe pointer to the first point data element
			// m_pData = &((*this)[0]);
			m_pData = this.m_data;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Sets all point to be connected in one closed path. Manipulates the aux
	 * point info such that the shape points is interpreted as one closed outer
	 * path defined clock-wise with repect to it's normals.
	 */
	public void SetClosedPathConnectivity() {

		// set closed path connectivity
		m_vPointAux.setSize(m_iNbPoints);

		for (int i = 0; i < m_iNbPoints; i++) {

			m_vPointAux.add(i, new CAAMPointInfo());

			// the first and only path
			m_vPointAux.get(i).m_iPathID = 0;

			// closed path, outer point etc.
			m_vPointAux.get(i).m_iTypeFlags = 0;

			// make closed path
			m_vPointAux.get(i).m_iConnectFrom = i == 0 ? m_iNbPoints - 1
					: i - 1;

			// make closed path
			m_vPointAux.get(i).m_iConnectTo = i == m_iNbPoints - 1 ? 0 : i + 1;
		}

	}

	/**
	 * Assignment operator (double). Sets all x and y compoments of a shape
	 * equal to a double. [Actually only used when calculating a mean shape --
	 * since we want to accumulate in an empty shape].
	 * 
	 * @param value
	 *            The value to set all shape points to.
	 */
	public CDVector assign(double value) {

		int len = this.m_data.length;
		for (int i = 0; i < len; i++) {
			this.m_data[i] = value;
		}
		return this;
	}

	/**
	 * Assignment operator (CAAMShape). Set one shape equal another.
	 * 
	 * @param s
	 *            The shape to copy.
	 */
	public CAAMShape assign(final CAAMShape s) {

		// copy all data from s
		CopyData(s);

		return this;
	}

	/**
	 * Copies all data from a shape to this. Called from the assignment
	 * operator.
	 * 
	 * @param s
	 *            Shape to copy data from.
	 */
	private void CopyData(final CAAMShape s) {

		// copy vector data (i.e. the point coordinates)
		this.Resize(s.Length());
		for (int i = 0; i < s.m_data.length; i++) {
			this.m_data[i] = s.m_data[i];
		}

		m_pData = this.m_data;

		// copy class data members
		this.m_iNbPoints = s.m_iNbPoints;
		this.m_bAbsPointCoordinates = s.m_bAbsPointCoordinates;

		int len = s.m_vPointAux.size();
		this.m_vPointAux.setSize(len);
		this.m_vPointAux.clear();
		for (int i = 0; i < len; i++) {
			CAAMPointInfo temp = new CAAMPointInfo();
			temp.assign(s.m_vPointAux.get(i));
			this.m_vPointAux.add(temp);
		}

		this.m_szHostImage = s.m_szHostImage;
		this.m_szASFVer = s.m_szASFVer;
		this.m_vUser1 = (Vector<Float>) (s.m_vUser1.clone());
		this.m_vUser2 = (Vector<Float>) (s.m_vUser2.clone());
		this.m_vUser3 = (Vector<Float>) (s.m_vUser3.clone());
	}

	/**
	 * Assignment operator (CAAMShape). Set the shape to be equal an xxx-yyy
	 * formatted vector. NOTE: this method does not manipulate any connectivity
	 * (i.e. point aux) info. So, if the shape beforehand was empty one should
	 * call SetClosedPathConnectivity() afterwards this call to obtain sensible
	 * point aux info.
	 * 
	 * @param vIn
	 *            xxx-yyy formatted vector.
	 */
	public CDVector assign(final CVisDVector vIn) {

		// CVisDVector vTmp = this;
		this.assign(vIn);
		int len = vIn.m_data.length;

		for (int i = 0; i < len; i++) {
			this.m_data[i] = vIn.m_data[i];
		}

		m_pData = this.m_data;

		return this;
	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * rotate shape with given theta angle
	 * 
	 * @param theta
	 */
	public void Rotate(final double theta) {
		Rotate(theta, false);
	}

	/**
	 * Rotates the shape. Rotates the shape 'theta' radians.
	 * 
	 * @param theta
	 *            Rotation angle in radians.
	 * @param aroundCOG
	 *            If true the rotation is being done around the cog of the shape
	 *            instead of around the global center.
	 */
	public void Rotate(final double theta, final boolean aroundCOG) {

		double x, y;
		double[] cx = new double[1];
		double[] cy = new double[1];

		// set up rotation matrix
		double c00 = Math.cos(theta);
		double c01 = -Math.sin(theta);
		double c10 = Math.sin(theta);
		double c11 = Math.cos(theta);

		if (aroundCOG) {

			COG(cx, cy);
			Translate(-cx[0], -cy[0]);
		}

		for (int i = 0; i < m_iNbPoints; i++) {

			x = this.m_pData[i];
			y = this.m_pData[i + m_iNbPoints];
			this.m_pData[i] = c00 * x + c01 * y;
			this.m_pData[i + m_iNbPoints] = c10 * x + c11 * y;
		}

		if (aroundCOG) {
			Translate(cx[0], cy[0]);
		}
	}

	/**
	 * Translates the shape.
	 * 
	 * @param p
	 *            The offset to translate.
	 */
	public void Translate(final CAAMPoint p) {
		this.Translate(p.x, p.y);
	}

	/**
	 * Translates the shape.
	 * 
	 * @param x
	 *            X-translation.
	 * @param y
	 *            Y-translation.
	 */
	public void Translate(final double x, final double y) {
		for (int i = 0; i < m_iNbPoints; i++) {
			this.m_pData[i] += x;
			this.m_pData[i + m_iNbPoints] += y;
		}
	}

	/**
	 * Returns the 2-norm of this shape centralized.
	 * 
	 * @return The 2-norm.
	 */
	public double ShapeSize() {

		CAAMShape tmp = new CAAMShape(this);

		double[] x = new double[1];
		double[] y = new double[1];
		tmp.COG(x, y);
		tmp.Translate(-x[0], -y[0]);

		return tmp.Norm2();
	}

	/**
	 * Find the maximum x component of the shape.
	 * 
	 * @return The x-maximum.
	 */
	public double MinX() {

		double val, min = 1.7E+308;

		for (int i = 0; i < m_iNbPoints; i++) {

			val = this.m_pData[i];
			min = val < min ? val : min;
		}
		return min;
	}

	/**
	 * Find the maximum x component of the shape.
	 * 
	 * @return The x-maximum.
	 */
	public double MaxX() {

		double val, max = -1.7E+308;

		for (int i = 0; i < m_iNbPoints; i++) {

			val = this.m_pData[i];
			max = val > max ? val : max;
		}
		return max;
	}

	/**
	 * Find the minimum y component of the shape.
	 * 
	 * @return The y-minimum.
	 */
	public double MinY() {

		double val, min = 1.7E+308;

		for (int i = 0; i < m_iNbPoints; i++) {

			val = this.m_pData[i + m_iNbPoints];
			min = val < min ? val : min;
		}
		return min;
	}

	/**
	 * Find the maximum y component of the shape.
	 * 
	 * @return The y-maximum.
	 */
	public double MaxY() {

		double val, max = -1.7E+308;

		for (int i = 0; i < m_iNbPoints; i++) {

			val = this.m_pData[i + m_iNbPoints];
			max = val > max ? val : max;
		}
		return max;
	}

	/**
	 * Scale the shape
	 * 
	 * @param s
	 */
	public void Scale(final double s) {
		Scale(s, false);
	}

	/**
	 * Scales the shape.
	 * 
	 * @param s
	 *            Scale factor.
	 * @param aroundCOG
	 *            If true the scale is being done around the cog of the shape
	 *            instead of around the global center.
	 */
	public void Scale(final double s, final boolean aroundCOG) {

		double[] cx = new double[1];
		double[] cy = new double[1];

		if (aroundCOG) {

			COG(cx, cy);
			Translate(-cx[0], -cy[0]);
		}

		for (int i = 0; i < 2 * m_iNbPoints; i++) {

			this.m_pData[i] *= s;
		}

		if (aroundCOG) {

			Translate(cx[0], cy[0]);
		}
	}

	/**
	 * Calculates the center of gravity of the shape (actually it's the center
	 * of the centroid).
	 * 
	 * @param p
	 *            cog output.
	 */
	public CAAMPoint COG() {

		CAAMPoint p = new CAAMPoint();
		double x[] = new double[1];
		double y[] = new double[1];
		// COG( p.x, p.y );
		COG(x, y);
		p.x = x[0];
		p.y = y[0];
		return p;
	}

	/**
	 * Calculates the center of gravity of the shape (actually it's the center
	 * of the centroid).
	 * 
	 * @param x
	 *            X cog output.
	 * @param y
	 *            X cog output.
	 */
	public void COG(double[] x, double[] y) {

		assert (m_iNbPoints > 0);

		double xSum, ySum;

		xSum = 0.0;
		ySum = 0.0;
		for (int i = 0; i < m_iNbPoints; i++) {

			xSum += this.m_pData[i];
			ySum += this.m_pData[i + m_iNbPoints];
		}

		x[0] = xSum / m_iNbPoints;
		y[0] = ySum / m_iNbPoints;
	}

	/**
	 * Normalizes the shape by translating it's center of gravity to origo and
	 * scale by the reciprocal of the 2-norm.
	 * 
	 * @return The 2-norm of the shape seen as a 2*nbPoint vector after the
	 *         translation to orig.
	 */
	public double Normalize() {

		double[] x = new double[1];
		double[] y = new double[1];

		COG(x, y);
		Translate(-x[0], -y[0]);

		// Normalize the vector to unit length, using the 2-norm.
		double norm = Norm2();
		Scale(1. / norm);

		return norm;
	}

	/**
	 * Overwrites the i'th point.
	 * 
	 * @return Zero.
	 */
	public int SetPoint(int i, final double x, final double y) {

		assert (i < m_iNbPoints);

		this.m_pData[i] = x;
		this.m_pData[i + m_iNbPoints] = y;

		return 0;
	}

	/**
	 * Overwrites the i'th point. If the point doesn't exists, non-zero is
	 * returned.
	 * 
	 * @return Zero on success, non-zero if the point doesn't exists.
	 */
	public int SetPoint(final int i, final CAAMPoint p) {

		return SetPoint(i, p.x, p.y);
	}

	/**
	 * Returns the i'th point.
	 * 
	 * @return Zero.
	 */
	public int GetPoint(int i, double[] x, double[] y) {

		assert (i < m_iNbPoints);

		x[0] = this.m_pData[i];
		y[0] = this.m_pData[i + m_iNbPoints];

		return 0;
	}

	/**
	 * Returns the i'th point.
	 * 
	 * @return The i'th point.
	 */
	public CAAMPoint GetPoint(int i) {

		CAAMPoint out = new CAAMPoint();
		double[] x = new double[1];
		double[] y = new double[1];
		// GetPoint( i, out.x, out.y );
		GetPoint(i, x, y);
		out.x = x[0];
		out.y = y[0];
		return out;
	}

	/**
	 * Change number of shape points in the shape. Note: Destroys *all* current
	 * point data and point info data.
	 * 
	 * @param length
	 * @param storage
	 */
	public void Resize(int length, double[] storage) {

		assert (length % 2 == 0);
		super.Resize(length, storage);
		m_iNbPoints = length / 2;

		m_pData = this.m_data;

		SetClosedPathConnectivity();
	}

	/**
	 * Returns the transformation that aligns this to 'ref' with respect to
	 * pose.
	 * 
	 * @param ref
	 *            The reference shape.
	 */

	public void AlignTransformation(final CAAMShape ref, double[] scale,
			double[] theta, CAAMPoint t) {

		CAAMShape refCpy = new CAAMShape(ref);
		CAAMShape thisCpy = new CAAMShape(this);
		double[] x = new double[1];
		double[] y = new double[1];

		// move thisCpy and refCpy to origo
		double[] t_x = new double[1];
		double[] t_y = new double[1];
		thisCpy.COG(t_x, t_y);
		t.x = t_x[0];
		t.y = t_y[0];
		thisCpy.Translate(-t.x, -t.y);
		refCpy.COG(x, y);
		refCpy.Translate(-x[0], -y[0]);
		t.x = x[0] - t.x;
		t.y = y[0] - t.y;

		// normalize scale, using the 2-norm
		double this_size = thisCpy.Norm2();
		double ref_size = refCpy.Norm2();
		scale[0] = refCpy.Norm2() / thisCpy.Norm2();
		thisCpy.Scale(scale[0]);

		// align rotation between thisCpy and refCpy
		theta[0] = -thisCpy.GetRotation(refCpy);
	}

	/**
	 * Init ref to this.
	 * 
	 * @param ref
	 * @return 2-norm of the shape
	 */
	public double initAlignTo(final CAAMShape ref) {
		return initAlignTo(ref, null);
	}

	/**
	 * Aligns this to 'ref' with respect to pose.
	 * 
	 * @param ref
	 *            The reference shape.
	 * @param pTheta
	 *            Optional pointer to return the rotation carried out on this.
	 * @return The 2-norm of the this shape seen as a 2*nbPoint vector after the
	 *         translation to orig but before the scale done to fit 'ref'.
	 */
	public double initAlignTo(final CAAMShape ref, double[] pTheta) {

		// make a copy of 'ref'
		CAAMShape refCpy = new CAAMShape(ref);
		double[] x = new double[1];
		double[] y = new double[1];

		// move this and refCpy to origo
		this.COG(x, y);
		// System.err.println("this.x = " + x[0] + "this.y = " + y[0]);
		this.Translate(-x[0], -y[0]);
		refCpy.COG(x, y);
		// x[0] = 256; y[0] = 256;
		// System.err.println("ref.x = " + x[0] + "ref.y = " + y[0]);
		refCpy.Translate(-x[0], -y[0]);

		// normalize scale, using the 2-norm
		double this_size = this.Norm2();
		double ref_size = refCpy.Norm2();
		this.Scale(ref_size / this_size);
		// this.Scale(0.9);
		// System.err.println("ref_size = " + ref_size + " this_size = " +
		// this_size);

		// align rotation between this and refCpy
		double theta;
		theta = this.GetRotation(refCpy);
		// this.Rotate( -theta );

		if (pTheta != null) {

			pTheta[0] = -theta;
		}

		// translate this to ref origo
		this.Translate(x[0], y[0]);

		return this_size;
	}

	/**
	 * Wrapper to align this to ref.
	 * 
	 * @param ref
	 * @return
	 */
	public double AlignTo(final CAAMShape ref) {
		return AlignTo(ref, null);
	}

	/**
	 * Aligns this to 'ref' with respect to pose.
	 * 
	 * @param ref
	 *            The reference shape.
	 * @param pTheta
	 *            Optional pointer to return the rotation carried out on this.
	 * @return The 2-norm of the this shape seen as a 2*nbPoint vector after the
	 *         translation to origo but before the scale done to fit 'ref'.
	 */
	public double AlignTo(final CAAMShape ref, double[] pTheta) {

		// make a copy of 'ref'
		CAAMShape refCpy = new CAAMShape(ref);
		double[] x = new double[1];
		double[] y = new double[1];

		// move this and refCpy to origo
		this.COG(x, y);
		// System.err.println("this.x = " + x[0] + "this.y = " + y[0]);
		this.Translate(-x[0], -y[0]);
		refCpy.COG(x, y);
		// x[0] = 256; y[0] = 256;
		// System.err.println("ref.x = " + x[0] + "ref.y = " + y[0]);
		refCpy.Translate(-x[0], -y[0]);

		// normalize scale, using the 2-norm
		double this_size = this.Norm2();
		double ref_size = refCpy.Norm2();
		this.Scale(ref_size / this_size);
		// System.err.println("ref_size = " + ref_size + " this_size = " +
		// this_size);

		// align rotation between this and refCpy
		double theta;
		theta = this.GetRotation(refCpy);
		this.Rotate(-theta);

		if (pTheta != null) {

			pTheta[0] = -theta;
		}

		// translate this to ref origo
		this.Translate(x[0], y[0]);

		return this_size;
	}

	/**
	 * Returns the rotation between ref and this (in radians). Get the rotation
	 * between two shapes by minimizing the sum of squared point distances, as
	 * described by Goodall (and Bookstein) using Singular Value Decomposition
	 * (SVD).
	 * 
	 * Note that both shapes must be normalized with respect to scale and
	 * position beforehand. This could be done by using CAAMSAhape::Normalize().
	 * 
	 * @return The estimated angle, theta, between the two shapes.
	 */
	public double GetRotation(final CAAMShape ref) {

		assert (ref.NPoints() == this.NPoints());

		int nbPoints = ref.NPoints();

		CVisDMatrix mRef = new CVisDMatrix(nbPoints, 2);
		CVisDMatrix mS = new CVisDMatrix(nbPoints, 2);
		CVisDMatrix res = new CVisDMatrix(2, 2);
		int i;
		final int X = 0;
		final int Y = 1;

		// get data as matrices ( nbPoints x 2 columns )
		for (i = 0; i < nbPoints; i++) {

			mRef.m_data[i][X] = ref.m_pData[i];
			mRef.m_data[i][Y] = ref.m_pData[i + nbPoints];

			mS.m_data[i][X] = this.m_pData[i];
			mS.m_data[i][Y] = this.m_pData[i + nbPoints];
		}

		// calculate the rotation by minimizing the sum of squared
		// point distances

		res = mRef.Transposed().mult(mS);

		int m = 2;
		int n = 2;

		CVisDVector S = new CVisDVector(2);
		CVisDMatrix U = new CVisDMatrix(2, 2);
		CVisDMatrix V = new CVisDMatrix(2, 2);

		// CDMatrix tempMatrix = new CDMatrix(m, n);
		// tempMatrix.VisDMatrixSVD(res, S, U, V); // ??????????????????

		// VisDMatrixSVD( res, S, U, V );
		SingularValueDecomposition svd = new SingularValueDecomposition(res);
		// S = svd.getS();
		U = svd.getU();
		V = svd.getV();

		res = V.mult(U.Transposed());

		/*
		 * char jobu = 'S'; // the first min(m,n) columns of U (the left
		 * singular // vectors) are returned in the array U; char jobvt = 'A';
		 * // all N rows of V**T are returned in the array VT; int ldx =
		 * Math.max(1, m); // The leading dimension of array X double sv[] = new
		 * double[Math.min(m, n)]; // The singular values of X, sorted so //
		 * that s[i] >= s[i+1]. double UT[][] = new double[m][n]; // if jobu =
		 * 'S', U contains the first min(m,n) columns of U // (the left singular
		 * vectors, stored columnwise); int ldu = m; // The leading dimension of
		 * the array U. ldu >= 1; if // jobu = 'S' or 'A', ldu >= m. double
		 * VT[][] = new double[n][n]; // If jobvt = 'A', VT contains the n-by-n
		 * orthogonal matrix V**T; int ldvt = n; // The leading dimension of the
		 * array VT. ldvt >= 1; if // jobvt = 'A', ldvt >= n; if jobvt = 'S',
		 * ldvt >= min(m,n). int nmax = 132; int lwork = (nmax * ((5 * nmax) +
		 * 5)) + 1; double[] work = new double[lwork]; int[] info = new int[1];
		 * // = 0: successful exit. // < 0: if info[0] = -i, the i-th argument
		 * had an illegal value. // > 0: if dbdsqr did not converge, info[0]
		 * specifies how many // superdiagonals of an intermediate bidiagonal
		 * form B // did not converge to zero.
		 * 
		 * // if info[0] > 0, work[1:min(n,m)-1] contains the unconverged //
		 * superdiagonal elements of an upper bidiagonal matrix B // whose
		 * diagonal is in s (not necessarily sorted). B // satisfies A = U * B *
		 * VT, so it has the same singular values // as A, and singular vectors
		 * related by U and VT. SVD svd = new SVD(); svd.dgesvd(jobu, jobvt, m,
		 * n, res.m_data, ldx, sv, UT, ldu, VT, ldvt, work, lwork, info); if
		 * (info[0] < 0) { System.err.println("In svd.dgesvd argument " +
		 * (-info[0]) + " had an illegal value"); return 0; } if (info[0] > 0) {
		 * System.err.println("in svd.dgesvd dbdsqr did not converge.");
		 * System.err.println(info[0] +
		 * " superdiagonals of an intermediate form B did not converge to zero."
		 * ); return 0; }
		 * 
		 * Matrix vTMat = new Matrix(VT); Matrix vMat = vTMat.transpose();
		 * double VV[][] = vMat.getArray(); V = new CDMatrix(m,n, VV);
		 * 
		 * Matrix uTMat = new Matrix(UT); Matrix uMat = uTMat.transpose();
		 * double UU[][] = uMat.getArray(); U = new CDMatrix(m,n, UU);
		 * 
		 * 
		 * res = V.mult(U.Transposed());
		 */

		// res holds now a normal 2x2 rotation matrix
		double angle;
		double cos_theta = res.m_data[0][0];
		double sin_theta = res.m_data[1][0];
		final double epsilon = 1e-12;

		if (1.0 - cos_theta < epsilon) {

			// cos_theta=1 => shapes are already aligned
			angle = 0;
			return angle;
		}

		if (Math.abs(cos_theta) < epsilon) {

			// cos_theta=0 => 90 degrees rotation
			return Math.PI / 2;
		}

		if (1.0 + cos_theta < epsilon) {

			// cos_theta=-1 => 180 degrees rotation
			angle = Math.PI;
		} else {

			// get the rotation in radians
			double a_cos = Math.acos(cos_theta);
			double a_sin = Math.asin(sin_theta);

			if (a_sin < 0) {

				// lower half of the unit circle
				angle = -a_cos;
			} else {

				// upper half of the unit circle
				angle = a_cos;
			}
		}

		return angle;
	}

	/**
	 * Converts pose parameters: scale, theta, tx, ty to a pose vector. The pose
	 * vector will be in the format:
	 * 
	 * [ s, theta, tx, ty ]
	 * 
	 * where s = scale-1
	 * 
	 * @param scale
	 *            Scale input.
	 * @param theta
	 *            Rotational input.
	 * @param tx
	 *            X translation input.
	 * @param ty
	 *            Y translation input.
	 * @param poseVec
	 *            The output pose vector.
	 */
	public static void Param2PoseVec(final double scale, final double theta,
			final double tx, final double ty, CDVector poseVec) {

		poseVec.Resize(4);

		if (true) {

			// MBS pose representation
			poseVec.m_data[0] = scale - 1.;
			poseVec.m_data[1] = theta;
			poseVec.m_data[2] = tx;
			poseVec.m_data[3] = ty;

		} else {

			// traditional AAM pose representation
			// (don't work with the gradient learning method)
			/*
			 * 
			 * The pose vector will be in the format:
			 * 
			 * [ s_x, s_y, tx, ty ]
			 * 
			 * where
			 * 
			 * s_x = scale*cos( theta )-1 s_y = scale*sin( theta )
			 */
			poseVec.m_data[0] = scale * Math.cos(theta) - 1.;
			poseVec.m_data[1] = scale * Math.sin(theta);
			poseVec.m_data[2] = tx;
			poseVec.m_data[3] = ty;

		}
	}

	/**
	 * Converts a pose vector to pose parameters: scale, theta, tx, ty. The pose
	 * vector are expected to be in the format:
	 * 
	 * [ s, theta, tx, ty ]
	 * 
	 * where s = scale-1
	 * 
	 * @see Param2PoseVec, Displace
	 * @param poseVec
	 *            The input pose vector.
	 * @param scale
	 *            Scale output.
	 * @param theta
	 *            Rotational output.
	 * @param tx
	 *            X translation output.
	 * @param ty
	 *            Y translation output.
	 */
	public static void PoseVec2Param(final CDVector poseVec, double[] scale,
			double[] theta, double[] tx, double[] ty) {

		assert (poseVec.Length() == 4);

		if (true) {

			// MBS pose representation
			scale[0] = poseVec.m_data[0] + 1.;
			theta[0] = poseVec.m_data[1];
			tx[0] = poseVec.m_data[2];
			ty[0] = poseVec.m_data[3];

		} else {

			// traditional AAM pose representation
			// (don't work with the gradient learning method)
			/*
			 * 
			 * The pose vector are expected to be in the format:
			 * 
			 * [ sx, sy, tx, ty ]
			 * 
			 * where
			 * 
			 * sx = scale*cos( theta )-1 sy = scale*sin( theta )
			 */

			// find scale
			scale[0] = Math.sqrt((1. + poseVec.m_data[0])
					* (1. + poseVec.m_data[0]) + poseVec.m_data[1]
					* poseVec.m_data[1]);

			// find rotation
			theta[0] = Math.atan2(poseVec.m_data[1], (1. + poseVec.m_data[0]));

			tx[0] = poseVec.m_data[2];
			ty[0] = poseVec.m_data[3];
		}
	}

	/**
	 * Displaces the shape around it's center of gravity using a displacement
	 * vector.
	 * 
	 * @param poseVec
	 *            The input pose vector.
	 * @return Nothing.
	 */
	public void Displace(final CDVector poseVec) {

		double[] scale = new double[1];
		double[] theta = new double[1];
		double[] tx = new double[1];
		double[] ty = new double[1];
		double[] cx = new double[1];
		double[] cy = new double[1];

		// convert pose vector
		this.PoseVec2Param(poseVec, scale, theta, tx, ty);

		// translate to origin
		this.COG(cx, cy);
		this.Translate(-cx[0], -cy[0]);

		// scale, rotate and translate
		this.Scale(scale[0]);
		this.Rotate(theta[0]);

		// translate the specified amount plus back to c.o.g.
		// this.Translate( cx[0]+tx[0], cy[0]+ty[0] );
		this.Translate(tx[0], ty[0]);
		this.Translate(cx[0], cy[0]);

	}

	/**
	 * Tests if the point 'p' is inside the path starting at position
	 * 'path_start'. Rarely used. Primary a helper function to IsInside.
	 * 
	 * @param p
	 *            The point to test for.
	 * @param path_start
	 *            The point index where a path starts
	 * @return True if 'p' is inside
	 */
	public boolean IsInsidePath(final CAAMPoint p, final int path_start) {

		double[] x1 = new double[1];
		double[] x2 = new double[1];
		double[] y1 = new double[1];
		double[] y2 = new double[1];
		double dX;
		double dY;
		double slope;
		double b;

		int nEdgeHit, path_id;

		// save the path id
		path_id = m_vPointAux.get(path_start).m_iPathID;

		// test if the path should be an open path
		if (m_vPointAux.get(path_start).IsClosed() == false) {

			// it's open -> we can't be inside
			return false;
		}

		if (m_vPointAux.get(path_start).m_iConnectTo == m_vPointAux
				.get(path_start).m_iConnectFrom) {

			// the path consist of single points -> we 'inside'
			return true;
		}

		// do the test
		nEdgeHit = 0;
		int pathlen = PathLen(path_start);
		for (int i = path_start; i < path_start + pathlen; i++) {

			// test the path id
			if (path_id != m_vPointAux.get(i).m_iPathID) {

				// we've reached the end of the path
				// -> stop
				break;
			}

			if (m_vPointAux.get(i).m_iConnectTo == i) {

				// there is not connection -> continue
				continue;
			}

			// get points
			GetPoint(i, x1, y1);
			GetPoint(m_vPointAux.get(i).m_iConnectTo, x2, y2);

			if (AAMdef.AAM_MIN2(x1[0], x2[0]) >= p.x
					|| AAMdef.AAM_MAX2(x1[0], x2[0]) < p.x) {

				// no intersection
				continue;
			}

			dX = x2[0] - x1[0];
			dY = y2[0] - y1[0];

			if (dX == 0.0) {

				// edge is horisontal -> no intersection
				// since we test for intersection on the line
				// from 'p' to (p.x, infinity), that is upwards
				// to infinity
				continue;
			}

			if (dY == 0.0) {

				// edge is vertical
				if (p.y <= y1[0]) {

					nEdgeHit++;
				}
				continue;
			}

			slope = dX / dY;
			b = x1[0] - slope * y1[0];

			if (slope > .0) {

				// increasing line
				if (p.x >= slope * p.y + b) {

					nEdgeHit++;
					continue;
				}
			} else {

				// decreasing line
				if (p.x <= slope * p.y + b) {

					nEdgeHit++;
					continue;
				}
			}
		}

		// heureka(!) is edgehit is an odd number?
		// if the line from 'p' to (p.y,infinity)
		// intersects an odd number of edges, 'p'
		// is inside in n-polygon
		return (nEdgeHit % 2) != 0;
	}

	/**
	 * Wrapper to check is point p is inside the shape
	 * 
	 * @param p
	 *            point
	 * @return inside or not
	 */
	public boolean IsInside(final CAAMPoint p) {
		return IsInside(p, false);
	}

	/**
	 * Tests if the point 'p' belongs to the shape. In the current version all
	 * shape points are assumes to be ordered points in a n-point polygon with
	 * no holes. For example used to clean up the meshes (removing unwanted
	 * triangles from the triangulisation).
	 * 
	 * @param 'p' The test point.
	 * @param bBoundTest
	 *            Flags wheather a bounding box test should be performed prior
	 *            to the exact test.
	 * @return True if 'p' is inside.
	 */
	public boolean IsInside(final CAAMPoint p, boolean bBoundTest) {

		boolean isInside;

		if (bBoundTest) {

			// do bounding box test
			if (p.x > MaxX() || p.x < MinX() || p.y > MaxY() || p.y < MinY()) {

				return false;
			}
		}

		// get paths
		Vector<Integer> paths = this.GetPaths();

		// test each path
		isInside = false;
		for (int i = 0; i < paths.size(); i++) {

			// for each path
			if (this.IsInsidePath(p, paths.get(i))) {

				// we were inside a path
				isInside = true;

				// investigate further
				boolean isHole = m_vPointAux.get(paths.get(i)).IsHole();

				if (isHole) {

					// oops we were inside a hole and thus
					// can never be inside the shape
					return false;
				}
			}
		}

		return isInside;
	}

	/**
	 * Expands the shape by moving each model point 'nPixels' perperdicular to
	 * the shape contour (that is: along the model point normal).
	 * 
	 * This function will expand each outer (closed) path of the shape.
	 * 
	 * No tests for crossing contours are being made as of now.
	 * 
	 * @param nPixels
	 *            The number of pixel to expand the shape with.
	 * @return Nothing.
	 */
	public void Expand(int nPixels) {

		Vector<Integer> paths = new Vector<Integer>();

		paths = this.GetPaths();

		for (int i = 0; i < paths.size(); i++) {

			// check type
			final CAAMPointInfo pi = this.PointAux().get(paths.get(i));

			if (pi.IsOuterEdge() && pi.IsClosed()) {

				for (int j = 0; j < m_iNbPoints; j++) {
					CAAMPoint p_out = new CAAMPoint();
					CAAMPoint p_in = new CAAMPoint();

					this.Normal(j, p_out, p_in, Math.abs(nPixels));
					this.SetPoint(j, nPixels > 0 ? p_out : p_in);
				}
			}
		}
	}

	/**
	 * Finds the normal to the i'th point on the shape. If the point should be a
	 * single point the normal points is defined to be equal to the point it
	 * self.
	 * 
	 * @param i
	 *            Index of point.
	 * @param p1
	 *            Reference to the outside normal point.
	 * @param p2
	 *            Reference to the inside normal point.
	 * @param dist
	 *            The desired distance from p[1|2] to the i-th point.
	 */
	public void Normal(final int i, CAAMPoint p1, CAAMPoint p2,
			final double dist) {

		double theta, theta0, theta1;
		CAAMPoint p = new CAAMPoint();
		int from, to;

		// get the point
		p = this.GetPoint(i);

		// if the point is an endpoint the point itself are used as neighbor
		from = m_vPointAux.get(i).m_iConnectFrom;
		to = m_vPointAux.get(i).m_iConnectTo;

		// check for connection
		assert (!(m_vPointAux.get(i).m_iConnectFrom == i && m_vPointAux.get(i).m_iConnectTo == i));

		// get connected points
		p1 = this.GetPoint(from);
		p2 = this.GetPoint(to);

		// find angles (why not use the dot product..should be faster...???)
		theta0 = Math.atan2(p1.y - p.y, p1.x - p.x);
		theta1 = Math.atan2(p2.y - p.y, p2.x - p.x);
		theta = (theta0 + theta1) / 2.;

		// ensure that p1 is always the outside point
		if (theta0 > theta1)
			theta += Math.PI;

		// calc output normal points
		p1.x = p.x + dist * Math.cos(theta);
		p1.y = p.y + dist * Math.sin(theta);
		p2.x = p.x - dist * Math.cos(theta);
		p2.y = p.y - dist * Math.sin(theta);
	}

	/**
	 * Displaces the i-th point along the normal.
	 * 
	 * @param i
	 *            Index of point.
	 * @param dist
	 *            The distance to move the point (>0 move point outwards, <0
	 *            inwards).
	 */
	public void NormalDisplacement(final int i, final double dist) {

		CAAMPoint p1 = new CAAMPoint();
		CAAMPoint p2 = new CAAMPoint();

		// get point on normal
		this.Normal(i, p1, p2, dist);

		// replace the i-th point
		this.SetPoint(i, p1);
	}

	/**
	 * Writes the shape to a ASF file. Remember asf's are always in relative
	 * coordinates. Se format description else where.
	 * 
	 * @param filename
	 *            Output filename.
	 * @param image_width
	 *            The image the coord. is relative to.
	 * @param image_height
	 *            The image the coord. is relative to.
	 * @return true on success, false on errors
	 */
	public boolean WriteASF(final String filename, final int image_width,
			final int image_height) {

		return WriteASF0_90(filename, image_width, image_height);
	}

	/**
	 * ASF writer version 0.90. Writes the shape to a ver. 0.90 ASF file.
	 * Remember asf's are always in relative coordinates. Se format description
	 * else where.
	 * 
	 * @param filename
	 *            Output filename.
	 * @param image_width
	 *            The image the coord. is relative to.
	 * @param image_height
	 *            The image the coord. is relative to.
	 * @return true on success, false on errors
	 */
	public boolean WriteASF0_90(final String filename, final int image_width,
			final int image_height) {
		try {
			// FILE *fh;
			PrintWriter fh = new PrintWriter(filename);

			// fh = fopen( filename, "wb" );

			if (fh == null)
				return false;

			// convert to relative coordinates
			if (IsAbs()) {

				Abs2Rel(image_width, image_height);
			}

			// write header
			long t = System.currentTimeMillis(); // CTime::GetCurrentTime();
			// s = t.Format( "%A %B %d - %Y [%H:%M]" );
			String pattern = "yyyy.MMMMM.dd GGG hh:mm aaa";
			Locale currentLocale = new Locale("en", "US");
			Date today;
			SimpleDateFormat formatter;
			String output;

			formatter = new SimpleDateFormat(pattern, currentLocale);
			today = new Date();
			output = formatter.format(today);

			fh.println("######################################################################");
			fh.println("##    AAM Shape File  -  written: " + output + "#");
			fh.println("######################################################################");

			// write nb points
			fh.println("#");
			fh.println("# number of model points\n#\n" + m_iNbPoints + "\n");

			// write points
			fh.println("#");
			fh.println("# model points");
			fh.println("#");
			fh.println("# format: <path#> <type> <x rel.> <y rel.> <point#> <connects from> <connects to> <user1> <user2> <user3>\n");
			fh.println("#");

			// check for user fields
			boolean u1 = (m_vUser1.size() == NPoints());
			boolean u2 = (m_vUser2.size() == NPoints());
			boolean u3 = (m_vUser3.size() == NPoints());

			DecimalFormat floatFormat = new DecimalFormat("#####.########");

			for (int i = 0; i < m_iNbPoints; i++) {

				CAAMPoint p = this.GetPoint(i);
				CAAMPointInfo pInfo = m_vPointAux.get(i);
				fh.print(pInfo.m_iPathID + "\t" + pInfo.m_iTypeFlags + " \t"
						+ floatFormat.format(p.x) + "\t "
						+ floatFormat.format(p.y) + "\t\t" + i + " \t"
						+ pInfo.m_iConnectFrom + "\t" + pInfo.m_iConnectTo);

				// print user fields if present
				if (u1)
					fh.print("\t" + m_vUser1.get(i));
				else
					fh.print("\t0.00");
				if (u2)
					fh.print("\t" + m_vUser2.get(i));
				else
					fh.print("\t0.00");
				if (u3)
					fh.print("\t" + m_vUser3.get(i));
				else
					fh.print("\t0.00");
				fh.print("\n");
			}

			// write host image
			fh.println("\n#");
			fh.println("# host image");
			fh.println("#");
			fh.println(m_szHostImage);

			// close file
			fh.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (!IsAbs()) {

			// convert back
			Rel2Abs(image_width, image_height);
		}

		// succes
		return true;
	}

	/**
	 * ASF writer version 0.90. Writes the shape to a ver. 0.90 ASF file.
	 * Remember asf's are always in relative coordinates. Se format description
	 * else where.
	 * 
	 * @param filename
	 *            Output filename.
	 * @param image_width
	 *            The image the coord. is relative to.
	 * @param image_height
	 *            The image the coord. is relative to.
	 * @return true on success, false on errors
	 */
	public boolean generateVOIs(ModelImage targetImageSlice) {

		int index = 0;
		VOIBase vTemp = new VOIContour(true);

		int nPts = m_iNbPoints / 2;
		
		// nPts = m_iNbPoints;

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		float[] zPtsZero = new float[nPts];

		for (int j = 0; j < nPts; j++) {
			CAAMPoint p = this.GetPoint(index);
			// CAAMPointInfo pInfo = m_vPointAux.get(index);
			xPts[j] = (float) p.x;
			yPts[j] = (float) p.y;
			index++;
		}
		// / vTemp.exportArrays(xPts, yPts, zPts);
		// rotateToStartingPoint(xPts, yPts, extents[1]);
		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

		VOI voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);

		targetImageSlice.getVOIs().removeAllElements();
		VOIVector voiVectorNew = new VOIVector();
		voiVectorNew.add(voiNew);
		targetImageSlice.addVOIs(voiVectorNew);

		// succes
		return true;
	}
	
	/**
	 * From the current shape, generate the corresponding triagnle meshes view.
	 * VOIs is used to display the mesh.   
	 * @param targetImageSlice
	 * @return  true by default
	 */
	public boolean generateVOImesh(ModelImage targetImageSlice) {
		int index = 0;
		
		CAAMMesh mesh = new CAAMMesh();
		// Delaunay
		// remove triangles composites the concave shape in the mesh. 
		CAAMDelaunay.MakeMesh(this, mesh, true);
		
		Vector<CAAMTriangle> triangles = mesh.Triangles();
		int size = triangles.size();
		int numPoints = size * 3; 
		
		VOIVector voiVectorNew = new VOIVector();
		
		for (int i = 0; i < size; i++) {
			// update pointer member
			index = 0;
			VOIBase vTemp = new VOIContour(true);
			CAAMTriangle triangle = triangles.get(i);
			Vector<CAAMPoint> points = triangle.m_pPoints;
			double x1, x2, x3, y1, y2, y3;

			x1 = points.get(triangle.V1()).x;
			x2 = points.get(triangle.V2()).x;
			x3 = points.get(triangle.V3()).x;
			y1 = points.get(triangle.V1()).y;
			y2 = points.get(triangle.V2()).y;
			y3 = points.get(triangle.V3()).y;

			float[] xPts = new float[3];
			float[] yPts = new float[3];
			float[] zPts = new float[3];
			float[] zPtsZero = new float[3];
			
			xPts[index] = (float) x1;
			yPts[index] = (float) y1;
			index++;
			xPts[index] = (float) x2;
			yPts[index] = (float) y2;
			index++;
			xPts[index] = (float) x3;
			yPts[index] = (float) y3;
			index++;		
			
			vTemp.importArrays(xPts, yPts, zPtsZero, index);
			VOI voiNew = new VOI((short) 0, "blank");
			voiNew.importCurve(vTemp);
			voiVectorNew.add(voiNew);
		}

		targetImageSlice.getVOIs().removeAllElements();
		targetImageSlice.addVOIs(voiVectorNew);
		
		return true; 
	}
	
	
	/**
	 * Reads an ver. 0.90 .asf into relative coordinates. Se format description
	 * else where.
	 * 
	 * @param filename
	 *            Input filename.
	 * @return true on success, false on errors
	 */
	public boolean ReadASF(final String filename) {

		return ReadASF0_90(filename);
	}

	/**
	 * Reads an ver. 0.90 .asf into relative coordinates. Se format description
	 * else where.
	 * 
	 * @param filename
	 *            Input filename.
	 * @return true on success, false on errors
	 */
	public boolean ReadASF0_90(final String filename) {

		CAAMPropsReader r = new CAAMPropsReader(filename);

		if (!r.IsValid()) {

			return false;
		}

		int npoints;

		// read nb points
		npoints = r.Sync();
		// fscanf( r.FH(), "%i", &npoints );

		// resize this shape
		this.Resize(npoints * 2);
		m_vPointAux.setSize(npoints);

		// read all point data
		// r.Sync();
		r.SkipLines();
		int nelem = 0;
		float user1, user2, user3;
		m_vUser1.clear();
		m_vUser2.clear();
		m_vUser3.clear();

		int[] path_id = new int[1];
		int[] type = new int[1];
		int[] point_nb = new int[1];
		int[] from = new int[1];
		int[] to = new int[1];
		float[] x_rel = new float[1];
		float[] y_rel = new float[1];

		for (int i = 0; i < npoints; i++) {

			// read point data
			// format: <path#> <type> <x rel.> <y rel.> <point#> <connects from>
			// <connects to>

			r.processCoordinatePoints(path_id, type, x_rel, y_rel, point_nb,
					from, to);

			// save point data
			SetPoint(i, x_rel[0], y_rel[0]);

			// save point aux data
			m_vPointAux.get(i).m_iPathID = path_id[0];
			m_vPointAux.get(i).m_iTypeFlags = type[0];
			m_vPointAux.get(i).m_iConnectFrom = from[0];
			m_vPointAux.get(i).m_iConnectTo = to[0];
		}

		// read host image
		r.SkipLines();
		String[] fileName = new String[1];
		r.processImageName(fileName);
		m_szHostImage = fileName[0];

		// we're now hold relative coordinates
		m_bAbsPointCoordinates = false;

		return true;
	}

	/**
	 * Generate ASF shape from given VOI
	 * 
	 * @param image
	 *            image with VOI.
	 * @return success or not
	 */
	public boolean ReadASFfromVOI(ModelImage image) {

		VOIVector VOIs = image.getVOIs();
		int nVOI = VOIs.size();
		int npoints;
		int totalShapePoints;

		npoints = 120;

		// resize this shape
		this.Resize(npoints * 2);
		m_vPointAux.setSize(npoints);

		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, 1);

		int type = 0;
		int idx;
		int from;
		int to;

		idx = 0;
		for (int i = 0; i < vArray[0].size(); i++) {
			// Vector<VOIBase>[] vArray =
			// VOIs.VOIAt(i).getSortedCurves(VOIBase.ZPLANE, 1);
			VOIBase v = vArray[0].get(i);
			VOIBase vTemp = (VOIBase) v.clone();

			// zero out the z dimension VOI
			float[] xPts = new float[60];
			float[] yPts = new float[60];
			float[] zPts = new float[60];

			vTemp.exportArrays(xPts, yPts, zPts);

			m_vUser1.clear();
			m_vUser2.clear();
			m_vUser3.clear();

			// save point data
			// idx = j;

			for (int k = 0; k < 60; k++) {

				SetPoint(idx, xPts[k], yPts[k]);

				from = (k == 0 ? 60 - 1 : k - 1);
				to = (k == 60 - 1 ? 0 : k + 1);

				from += i * 60;
				to += i * 60;

				// save point aux data
				m_vPointAux.get(idx).m_iPathID = i;
				m_vPointAux.get(idx).m_iTypeFlags = type;
				m_vPointAux.get(idx).m_iConnectFrom = from;
				m_vPointAux.get(idx).m_iConnectTo = to;
				idx++;
			}
		}

		/*
		 * double[] x = new double[1]; double[] y = new double[1]; for (int i =
		 * 0; i < m_vPointAux.size(); i++ ) { GetPoint(i, x, y); CAAMPointInfo p
		 * = m_vPointAux.get(i); System.err.println(p.m_iPathID + "\t" +
		 * p.m_iTypeFlags + "\t" + x[0] + "\t" + y[0] + "\t" + i + "\t" +
		 * p.m_iConnectFrom + "\t" + p.m_iConnectTo); }
		 */
		myHostImage = image;

		// we're now hold relative coordinates
		m_bAbsPointCoordinates = true;

		return true;
	}

	/**
	 * Convert ASF shape from VOI
	 * 
	 * @param v
	 *            VOI
	 * @return success or not.
	 */
	public boolean ConvertASFfromVOI(VOIBase v) {

		int type = 0;
		int idx;
		int from;
		int to;

		idx = 0;

		VOIBase vTemp = (VOIBase) v.clone();

		int nPts = vTemp.size();
		// resize this shape
		this.Resize(nPts * 2);
		m_vPointAux.setSize(nPts);

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];

		vTemp.exportArrays(xPts, yPts, zPts);

		for (int k = 0; k < nPts; k++) {

			SetPoint(idx, xPts[k], yPts[k]);

			from = (k == 0 ? nPts - 1 : k - 1);
			to = (k == nPts - 1 ? 0 : k + 1);

			from += nPts;
			to += nPts;

			// save point aux data
			m_vPointAux.get(idx).m_iPathID = 1;
			m_vPointAux.get(idx).m_iTypeFlags = type;
			m_vPointAux.get(idx).m_iConnectFrom = from;
			m_vPointAux.get(idx).m_iConnectTo = to;
			idx++;
		}

		return true;
	}

	public boolean ReadASFfromVOI_init(ModelImage image) {
		/*
		 * VOIVector VOIs = image.getVOIs(); int nVOI = VOIs.size(); int
		 * npoints; int totalShapePoints;
		 * 
		 * npoints = 120;
		 * 
		 * // resize this shape this.Resize(npoints * 2);
		 * m_vPointAux.setSize(npoints);
		 * 
		 * Vector<VOIBase>[] vArray =
		 * VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);
		 * 
		 * int type = 0; int idx; int from; int to;
		 * 
		 * idx = 0; for (int i = 0; i < vArray[0].size(); i++) { //
		 * Vector<VOIBase>[] vArray =
		 * VOIs.VOIAt(i).getSortedCurves(VOIBase.ZPLANE, 1); VOIBase v =
		 * vArray[0].get(i); VOIBase vTemp = (VOIBase) v.clone();
		 * 
		 * // zero out the z dimension VOI float[] xPts = new float[60]; float[]
		 * yPts = new float[60]; float[] zPts = new float[60];
		 * 
		 * vTemp.exportArrays(xPts, yPts, zPts);
		 * 
		 * m_vUser1.clear(); m_vUser2.clear(); m_vUser3.clear();
		 * 
		 * // save point data // idx = j;
		 * 
		 * for ( int k = 0; k < 60; k++ ) {
		 * 
		 * SetPoint(idx, xPts[k], yPts[k]);
		 * 
		 * from = (k == 0 ? 60 - 1 : k - 1); to = (k == 60 - 1 ? 0 : k + 1);
		 * 
		 * from += i * 60; to += i * 60;
		 * 
		 * // save point aux data m_vPointAux.get(idx).m_iPathID = i;
		 * m_vPointAux.get(idx).m_iTypeFlags = type;
		 * m_vPointAux.get(idx).m_iConnectFrom = from;
		 * m_vPointAux.get(idx).m_iConnectTo = to; idx++; } }
		 */

		/*
		 * double[] x = new double[1]; double[] y = new double[1]; for (int i =
		 * 0; i < m_vPointAux.size(); i++ ) { GetPoint(i, x, y); CAAMPointInfo p
		 * = m_vPointAux.get(i); System.err.println(p.m_iPathID + "\t" +
		 * p.m_iTypeFlags + "\t" + x[0] + "\t" + y[0] + "\t" + i + "\t" +
		 * p.m_iConnectFrom + "\t" + p.m_iConnectTo); }
		 */
		myHostImage = image;

		// we're now hold relative coordinates
		m_bAbsPointCoordinates = true;

		return true;
	}

	/**
	 * Converts shape coordinates from absolute to relative by using the
	 * hostimage.
	 * 
	 * @doc Converts shape coordinates from absolute to relative by using the
	 *      hostimage.
	 */
	public void Abs2Rel(final String hostImagePath) {

		if (IsAbs() == true) {

			// CVisImage img = new CVisImage();
			FileIO io = new FileIO();

			ModelImage img = io.readImage(CAAMUtil.AddBackSlash(hostImagePath)
					+ m_szHostImage);
			// img.ReadFile( CAAMUtil.AddBackSlash( hostImagePath ) +
			// m_szHostImage );
			int[] extents = new int[2];
			extents = img.getExtents();

			// convert to absolute coordinates
			Abs2Rel(extents[0], extents[1]);
		}
	}

	/**
	 * Returns true if the shape is in absolute coordinates.
	 * 
	 * @return is abs coordinate or not
	 */
	public boolean IsAbs() {
		return m_bAbsPointCoordinates;
	}

	/*
	 * Converts shape coordinates from relative to absolute by using the
	 * hostimage.
	 */
	public void Rel2Abs(final String hostImagePath) {

		if (IsAbs() == false) {

			// CVisImage img = new CVisImage();
			FileIO io = new FileIO();
			ModelImage img = io.readImage(hostImagePath + m_szHostImage);
			// img.ReadFile( CAAMUtil.AddBackSlash( hostImagePath ) +
			// m_szHostImage );
			int[] extents = new int[2];
			extents = img.getExtents();

			// convert to absolute coordinates
			Rel2Abs(extents[0], extents[1]);
		}
	}

	/**
	 * Converts shape coordinates from absolute to relative. Relative
	 * coordinates are specified as:
	 * 
	 * x_relative = x_abs/image_width
	 * 
	 * y_relative = y_abs/image_height
	 * 
	 * @see Rel2Abs
	 * @param image_width
	 *            The image the coord. should be relative to.
	 * @param image_height
	 *            The image the coord. should be relative to.
	 * @return Nothing.
	 */
	public void Abs2Rel(final int image_width, final int image_height) {

		if (!IsAbs()) {

			// coordinates are already in relative format
			return;
		}

		// scale x
		for (int i = 0; i < m_iNbPoints; i++) {

			this.m_pData[i] /= image_width;
		}

		// scale y
		for (int i = m_iNbPoints; i < 2 * m_iNbPoints; i++) {

			this.m_pData[i] /= image_height;
		}

		m_bAbsPointCoordinates = false;
	}

	/**
	 * Converts shape coordinates from relative to absolute. Relative
	 * coordinates are specified as:
	 * 
	 * x_relative = x_abs/image_width
	 * 
	 * y_relative = y_abs/image_height
	 * 
	 * @see Abs2Rel
	 * @param image_width
	 *            The image the coord. is relative to.
	 * @param image_height
	 *            The image the coord. is relative to.
	 * @return Nothing.
	 */
	public void Rel2Abs(final int image_width, final int image_height) {

		if (IsAbs()) {

			// coordinates are already in absolute format
			return;
		}

		// scale x
		for (int i = 0; i < m_iNbPoints; i++) {

			this.m_pData[i] *= image_width;
		}

		// scale y
		for (int i = m_iNbPoints; i < 2 * m_iNbPoints; i++) {

			this.m_pData[i] *= image_height;
		}

		m_bAbsPointCoordinates = true;
	}

	/**
	 * Adds a path to the shape. All added point inherits the given pointtype.
	 * 
	 * @param shape
	 *            A shape containing one path.
	 * @param pointType
	 *            The pointtype of the added points.
	 * @return Nothing.
	 */
	public void AddPath(final CAAMShape shape, final CAAMPointInfo pointType) {

		int max_path_id = -1;
		int first_point; // the first point the new path

		// backup current shape
		CAAMShape tmpOrg = this;

		int newNPoints = tmpOrg.NPoints() + shape.NPoints();

		// resize this
		this.Resize(2 * newNPoints);

		first_point = tmpOrg.NPoints();

		// copy all points
		for (int i = 0; i < newNPoints; i++) {

			if (i < first_point) {

				// copy old point
				this.SetPoint(i, tmpOrg.GetPoint(i));

				// copy old point info
				m_vPointAux.set(i, tmpOrg.PointAux().get(i));

				// find max path_id
				if (m_vPointAux.get(i).m_iPathID > max_path_id) {

					max_path_id = m_vPointAux.get(i).m_iPathID;
				}

			} else {

				// copy new point
				this.SetPoint(i, shape.GetPoint(i - first_point));

				// make new point info

				// copy flags
				m_vPointAux.set(i, pointType);

				// set the remaining fields
				m_vPointAux.get(i).m_iPathID = max_path_id + 1;

				if (pointType.IsClosed()) {

					// make closed path
					m_vPointAux.get(i).m_iConnectFrom = i == first_point ? newNPoints - 1
							: i - 1;
					m_vPointAux.get(i).m_iConnectTo = i == newNPoints - 1 ? first_point
							: i + 1;

				} else {

					// make open path
					m_vPointAux.get(i).m_iConnectFrom = i == first_point ? first_point
							: i - 1;
					m_vPointAux.get(i).m_iConnectTo = i == newNPoints - 1 ? newNPoints - 1
							: i + 1;
				}
			}
		}
	}

	/**
	 * Reverses the point point ordering. Used when a clock-wise outer path
	 * shall converted to a counter-clock wise hole.
	 * 
	 * @return Nothing.
	 */
	public void ReversePointOrder() {

		CDVector xx = new CDVector(m_iNbPoints);
		CDVector yy = new CDVector(m_iNbPoints);

		// extract x and y part
		for (int i = 0; i < m_iNbPoints; i++) {

			xx.m_data[i] = this.m_pData[i];
			yy.m_data[i] = this.m_pData[i + m_iNbPoints];
		}

		// reverse
		xx.Reverse();
		yy.Reverse();

		// write back
		for (int i = 0; i < m_iNbPoints; i++) {

			this.m_pData[i] = xx.m_data[i];
			this.m_pData[i + m_iNbPoints] = yy.m_data[i];
		}
	}

	/**
	 * Extracts the starting positions of each path in the shape by a simple
	 * linear search. The starting position is identified by a change in path id
	 * (saved in the m_vPointAux member).
	 * 
	 * @return A vector of path starting positions.
	 */
	public Vector<Integer> GetPaths() {

		Vector<Integer> paths = new Vector<Integer>();
		int currentPathID = -1;

		for (int i = 0; i < m_iNbPoints; i++) {

			// System.err.println("i = " + i + " currentPathID = " +
			// currentPathID + " m_vPointAux.get(i).m_iPathID = " +
			// m_vPointAux.get(i).m_iPathID);
			if (currentPathID != m_vPointAux.get(i).m_iPathID) {

				currentPathID = m_vPointAux.get(i).m_iPathID;
				paths.add(i);
			}
		}

		return paths;
	}

	/**
	 * Returns the length of a path.
	 * 
	 * @see GetPaths
	 * @param startPosition
	 *            The starting position of the path.
	 * @return The path length.
	 */
	public int PathLen(final int startPosition) {

		int i;
		assert (startPosition < m_iNbPoints && startPosition >= 0);

		int path_id = m_vPointAux.get(startPosition).m_iPathID;

		for (i = startPosition; i < m_iNbPoints; i++) {

			if (path_id != m_vPointAux.get(i).m_iPathID) {

				break;
			}

		}

		return i - startPosition;
	}

	/**
	 * Extracts one path from a shape into a new shape.
	 * 
	 * @param startPosition
	 *            The starting position of the path.
	 * @return The path as a new shape.
	 */
	public CAAMShape ExtractPath(final int startPosition) {

		int len = PathLen(startPosition);

		CAAMShape outShape = new CAAMShape(len);

		outShape.SetHostImage(this.HostImage());

		for (int i = 0; i < len; i++) {

			CAAMPoint p = new CAAMPoint();
			p = this.GetPoint(i + startPosition);
			outShape.SetPoint(i, p);
		}

		return outShape;
	}

	/**
	 * Adds an extra outer path on each outer path in the distance of 'nPixels'
	 * along the point normal.
	 * 
	 * This method is primary used in conjunction with the
	 * "do not use the convex hull" feature. In such a case one often still
	 * wants a certain neighborhood of the shape to be included in the model.
	 * 
	 * @param nPixels
	 *            The size of the extents.
	 */
	public void AddShapeExtends(int nPixels) {

		Vector<Integer> paths = new Vector();

		CAAMShape tmpShape = new CAAMShape(this.NPoints());

		// make a copy of this shape
		tmpShape = this;

		paths = tmpShape.GetPaths();

		// for each path in the tmpShape
		for (int i = 0; i < paths.size(); i++) {

			// check type
			final CAAMPointInfo pi = tmpShape.PointAux().get(paths.get(i));

			if (pi.IsOuterEdge() && pi.IsClosed()) {

				// allright the path is an outer egde and it's closed
				// we can proceed

				// make a copy of the path
				CAAMShape newOuterPath = tmpShape.ExtractPath(paths.get(i));

				// expand the new outer path
				newOuterPath.Expand(nPixels);

				// setup new path properties
				CAAMPointInfo newOuterPathPI = new CAAMPointInfo();

				newOuterPathPI.SetClosed(true);
				newOuterPathPI.SetOriginal(false);
				newOuterPathPI.SetOuterEdge(true);
				newOuterPathPI.SetHole(false);

				// add to this shape
				this.AddPath(newOuterPath, newOuterPathPI);

				// remove the outer path property from the original path
				CAAMPointInfo oldPI = new CAAMPointInfo();
				oldPI = m_vPointAux.get(paths.get(i));
				oldPI.SetOuterEdge(false);
				SetPointInfoFlagsInPath(paths.get(i), oldPI.m_iTypeFlags);
			}
		}
	}

	/**
	 * Sets all flags in one path to the same value.
	 * 
	 * @param startPosition
	 *            Starting position of the path.
	 * @param flags
	 *            The flags.
	 */
	public void SetPointInfoFlagsInPath(final int startPosition, final int flags) {

		int len = PathLen(startPosition);

		for (int i = 0; i < len; i++)
			m_vPointAux.get(i + startPosition).m_iTypeFlags = flags;
	}

	/**
	 * Add articifial interior points to the shape by making a Delaunay
	 * triangulation and adding the centroid of each triangle. This is done
	 * iteratively. Default is one iteration
	 * 
	 * @param interations
	 *            Controls the number of artificial points. One iteration equals
	 *            one triangulation.
	 */
	public void AddInterior(final int interations) {

		for (int iter = 0; iter < interations; iter++) {

			CAAMMesh mesh = new CAAMMesh();

			// do the Delaunay triangulation of this shape
			CAAMDelaunay.MakeMesh(this, mesh, true);

			int n = mesh.NTriangles();

			// make new temporary shape
			CAAMShape tmpShape = new CAAMShape(this.NPoints() + n);

			// copy points and point aux
			for (int i = 0; i < NPoints(); i++) {

				tmpShape.SetPoint(i, this.GetPoint(i));
			}
			tmpShape.setPointAux(this.PointAux());

			// setup the point aux of the new artificial points
			CAAMPointInfo pi = new CAAMPointInfo();
			pi.SetClosed(false);
			pi.SetHole(false);
			pi.SetOriginal(false);
			pi.SetOuterEdge(false);

			Vector<Integer> paths = this.GetPaths();
			int npaths = paths.size();

			// add the centroids to the new shape
			for (int i = 0; i < n; i++) {

				CAAMPoint centriod = new CAAMPoint();

				centriod = mesh.Triangles().get(i).CenterPoint();

				int nbp = NPoints() + i;

				// set the point
				tmpShape.SetPoint(nbp, centriod);

				// set the point aux
				pi.m_iConnectFrom = nbp;
				pi.m_iConnectTo = nbp;
				pi.m_iPathID = npaths;
				tmpShape.setPointAux(nbp, pi);
			}

			// copy the temporary shape into this
			this.assign(tmpShape);
		}
	}

	/**
	 * Tests if any interior points has gone outside outer path of the shape.
	 * 
	 * @return True if the shape looks ok, false if not.
	 */
	public boolean ConsistencyCheck() {

		int n = NPoints();

		for (int i = 0; i < n; i++) {

			// test if any interior points has gone outside
			// the outerpath of the shape
			if (PointAux().get(i).IsOuterEdge() == false) {

				// the point should belong to the interior of the shape
				CAAMPoint p = this.GetPoint(i);
				if (this.IsInside(p) == false) {

					// wooops, it didn't
					return false;
				}
			}
		}

		// no point failed the test -> we're happy :-)
		return true;
	}

	/**
	 * Converts a one path shape into a border shape by adding two symmetric
	 * borders: an inside (a hole) and an outside border.
	 * 
	 * No checks for folding paths are done as of now.
	 * 
	 * @param size
	 *            The size of the border in pixels.
	 */
	public void MakeBorderShape(int size) {

		CAAMShape hole = new CAAMShape();
		hole.assign(this);
		hole.Expand(-size);
		// hole.ReversePointOrder();

		// setup new path properties
		CAAMPointInfo newHolePathPI = new CAAMPointInfo();

		newHolePathPI.SetClosed(true);
		newHolePathPI.SetOriginal(false);
		newHolePathPI.SetOuterEdge(false);
		newHolePathPI.SetHole(true);

		// update point aux of the hole
		hole.SetPointInfoFlagsInPath(0, newHolePathPI.m_iTypeFlags);

		// add the hole
		this.AddPath(hole, newHolePathPI);

		// add the outside path
		this.AddShapeExtends(size);
	}

	/**
	 * Plus operator.
	 * 
	 * @param v
	 *            Vector to add.
	 * @return The addition of this and 'v'.
	 */
	public CAAMShape add(final CVisDVector v) {
		CAAMShape ret = new CAAMShape(this);

		ret.add_into(v);

		return ret;
	}

	/**
	 * Minus operator.
	 * 
	 * @param v
	 *            Vector to add.
	 * @return The subtraction of this and 'v'.
	 */
	public CAAMShape sub(final CVisDVector v) {
		CAAMShape ret = new CAAMShape(this);

		ret.sub_into(v);

		return ret;
	}

	/**
	 * Retrives the image connected to the shape. As of now it's loaded from
	 * disk using the HostImage() member in the shape.
	 * 
	 * @param dest
	 *            The destination image.
	 * @param path
	 *            The path to the .asf file.
	 * @param rfactor
	 *            Optional reduction factor. Performs a scaling of the the shape
	 *            by 1/rfactor. Default 1 i.e. no scaling.
	 * @return refactored image.
	 */
	public ModelSimpleImage GetHostImage(ModelSimpleImage dest,
			final String path, final int rfactor) {

		assert (m_szHostImage != "");

		dest = dest.ReadBandedFile(CAAMUtil.AddBackSlash(path) + m_szHostImage);
		dest.SetName(m_szHostImage);

		if (rfactor > 1) {

			// dest.ReducePyr( rfactor );
			dest.subSample2dBy2();
		}
		return dest;

	}

	/**
	 * Retrives the image connected to the shape. As of now it's loaded from
	 * disk using the HostImage() member in the shape.
	 * 
	 * @param src
	 *            source image
	 * @param rfactor
	 *            reduction factor
	 * @return refactored iamge.
	 */
	public ModelSimpleImage GetHostImage(ModelImage src, final int rfactor) {

		if (m_szHostImage.equals("")) {
			m_szHostImage = src.getImageFileName();
		}
		ModelSimpleImage temp = new ModelSimpleImage(src.getExtents(), src
				.getFileInfo(0).getResolutions(), src);
		// dest = dest.ReadBandedFile(CAAMUtil.AddBackSlash(path) +
		// m_szHostImage);
		// dest.SetName(m_szHostImage);

		if (rfactor > 1) {
			temp.subSample2dBy2();
		}
		return temp;

	}

	public double Area() {
		return Area(false);
	}

	/**
	 * Returns the total area of the shape (with holes excluded).
	 * 
	 * @param use_covex_hull
	 *            Use the convex hull of the shape for area calculation (default
	 *            false).
	 * @return The area.
	 */
	public double Area(boolean use_covex_hull) {

		CAAMMesh mesh = new CAAMMesh();

		// lazy and a bit costly way to calculate the area
		CAAMDelaunay.MakeMesh(this, mesh, !use_covex_hull);

		return mesh.Area();
	}

	/**
	 * Calculates the convex hull of each path in the shape. The calulation is
	 * built upon the geometrical fact that the homogenous point matrix: [ p1x
	 * p2x p3x ; p1y p2y p3y ; 1 1 1 ] is positive if p1, p2, p3 is a convex
	 * segment and negative if concave. Note: this holds for a clock-wise
	 * ordering of p1,p2,p3.
	 * 
	 * Any open paths are considered cyclic in the concavity calulation.
	 * 
	 * Remember that paths should be defined clock-wise in the asf format.
	 * 
	 * BUG COMMENT: This does not seem to work with multiple paths.
	 * 
	 * @return A convex version of this shape w.r.t. each path.
	 */
	public CAAMShape CalcConvexHull() {

		CAAMPoint point = new CAAMPoint();
		CDMatrix m = new CDMatrix(3, 3);
		Vector<Integer> paths = new Vector<Integer>();

		paths = GetPaths();

		for (int p = 0; p < paths.size(); p++) {

			CAAMShape path_shape = this.ExtractPath(paths.get(p));

			int path_len = PathLen(paths.get(p));
			for (int i = 0; i < path_len && path_len > 2; i++) {

				point = this.GetPoint(i - 1 > 0 ? paths.get(p) + i - 1 : paths
						.get(p) + path_len - 1);
				m.m_data[0][0] = point.x;
				m.m_data[1][0] = point.y;
				m.m_data[2][0] = 1.0;

				point = this.GetPoint(paths.get(p) + i);
				m.m_data[0][1] = point.x;
				m.m_data[1][1] = point.y;
				m.m_data[2][1] = 1.0;

				point = this.GetPoint(i + 1 < path_len ? paths.get(p) + i + 1
						: paths.get(p));
				m.m_data[0][2] = point.x;
				m.m_data[1][2] = point.y;
				m.m_data[2][2] = 1.0;

				if (m.Det() < 0) {

					// concave segment detected

					// Remove the i-th point and continue on the new shape
					CAAMShape out = new CAAMShape(this);

					out.RemovePoint(i);

					return out.CalcConvexHull();
				}
			}
		}

		//
		// shape was convex from the start
		//
		return this;
	}

	/**
	 * Tests if the shap is convex w.r.t. each path. This call is rather
	 * expensive, since it spaws a call to CalcConvexHull().
	 * 
	 * @return True on convex, false on concave.
	 */
	public boolean IsConvex() {

		CAAMShape convex = CalcConvexHull();

		return convex.NPoints() == this.NPoints();
	}

	/**
	 * Removes the i-th point from the shape. Since this require massive
	 * reordering of the preceeding point connectivity this is actually a very
	 * expensive call [a fairly ugly in it's implementation] :-(
	 * 
	 * @param i
	 *            The index of the point to remove.
	 * @return Nothing.
	 */
	public void RemovePoint(final int i) {

		// get info about the path the i'th point is
		// belonging to
		Vector<Integer> paths = this.GetPaths();
		int path_id = this.m_vPointAux.get(i).m_iPathID;
		int path_start = paths.get(path_id);
		int path_end = (path_id == paths.size() - 1 ? this.NPoints() : paths
				.get(path_id + 1)) - 1;
		int path_len = path_end - path_start + 1;
		boolean closed_path = this.m_vPointAux.get(i).IsClosed();

		// create new shape
		CAAMShape new_shape = new CAAMShape(this.NPoints() - 1);

		// copy all point + aux data
		// except the i-th point
		int np = new_shape.NPoints();
		for (int j = 0; j < np; j++) {

			new_shape.SetPoint(j, this.GetPoint(j >= i ? j + 1 : j));
			new_shape.m_vPointAux.set(j,
					this.m_vPointAux.get(j >= i ? j + 1 : j));
		}

		// ///////////////////////////////////////////////////////
		// update to, from and path id in the aux data part
		// ///////////////////////////////////////////////////////

		// if path_len==1 then we're removing a path totally
		// and hence we need to decrement the preceeding
		// path id's
		for (int j = i; j < np && path_len == 1; j++) {

			--new_shape.m_vPointAux.get(j).m_iPathID;
		}

		// decrement all to/from
		int to, from;
		for (int j = i; j < np; j++) {

			from = new_shape.m_vPointAux.get(j).m_iConnectFrom;
			to = new_shape.m_vPointAux.get(j).m_iConnectTo;
			new_shape.m_vPointAux.get(j).m_iConnectFrom = DEC_TO_ZERO(from);
			new_shape.m_vPointAux.get(j).m_iConnectTo = DEC_TO_ZERO(to);

		}

		// In the to/from updates we have the following cases:
		//
		// 1) First point on a closed path
		// 2) Last point on a closed path
		// 3) First point on an open path
		// 4) Last point on an open path
		// 5) Intermediate point on a open/closed path
		//
		// In case 1-4 we need to fix the open/close path property
		//

		// case 1
		if (i == path_start && closed_path == true) {

			// 'close' the new first point on this path
			new_shape.m_vPointAux.get(i).m_iConnectFrom = DEC_TO_ZERO(path_end);
		}

		// case 2
		if (i == path_end && closed_path == true) {

			// 'open' the new last point on this path
			new_shape.m_vPointAux.get(DEC_TO_ZERO(i)).m_iConnectTo = path_start;
		}

		// case 3
		if (i == path_start && closed_path == false) {

			// 'open' the new first point on this path
			new_shape.m_vPointAux.get(i).m_iConnectFrom = i;
		}

		// case 4
		if (i == path_end && closed_path == false) {

			// 'open' the new last point on this path
			new_shape.m_vPointAux.get(DEC_TO_ZERO(i)).m_iConnectTo = DEC_TO_ZERO(i);
		}

		this.assign(new_shape);
	}

	/**
	 * Convert deciam number to 0
	 * 
	 * @param a
	 *            int number
	 * @return
	 */
	public int DEC_TO_ZERO(int a) {
		return (a == 0 ? 0 : a - 1);
	}

	/**
	 * Returns the reference of the user defined field vector number 'field_nb'.
	 * 
	 * @param field_nb
	 *            Field number [1-3].
	 * @return A reference to the specified field vector.
	 */
	public Vector<Float> UserField(final int field_nb) {

		assert (field_nb >= 0 && field_nb <= 3);

		switch (field_nb) {

		case 1:
			return m_vUser1;
		case 2:
			return m_vUser2;
		case 3:
			return m_vUser3;
		default:
			System.err
					.println("Error: Wrong field number given. Junk data returned.");
		}

		// return junk data
		return m_vUser1;
	}

	/**
	 * Allocates room for the three user defined fields.
	 * 
	 * Notice that the user defined fields is *not* allocated by default.
	 * 
	 * In order to preserved memory the user must to this explicitly by using
	 * AllocateUserFields();
	 */
	public void AllocateUserFields() {

		m_vUser1.setSize(NPoints());
		m_vUser2.setSize(NPoints());
		m_vUser3.setSize(NPoints());
	}

	/**
	 * Host image (if any).
	 * 
	 * @return host image
	 */
	public final String HostImage() {
		return m_szHostImage;
	}

	/**
	 * Returns the host image (if any).
	 * 
	 * @param hostImageFilename
	 */
	public void SetHostImage(final String hostImageFilename) {
		m_szHostImage = hostImageFilename;
	}

	/**
	 * Shape width.
	 * 
	 * @return shape width
	 */
	public double Width() {
		return MaxX() - MinX();
	}

	/**
	 * Shape height
	 * 
	 * @return shape height
	 */
	public double Height() {
		return MaxY() - MinY();
	}

	/**
	 * The number of shape points.
	 * 
	 * @return number of points on shape
	 */
	public final int NPoints() {
		return m_iNbPoints;
	}

	/**
	 * un-safe (and dirty) since it depends on assumptions regarding the
	 * 
	 * @param i   index
	 * @param x   x coordinate pointer
	 * @param y	  y conrdinate pointer
	 */
	public void GetPointUS(int i, double[] x, double[] y) {
		x[0] = m_pData[i];
		y[0] = m_pData[i + m_iNbPoints];
	}

	/**
	 * Returns the complete point aux vector of the shape.
	 * 
	 * @return point info vector
	 */
	public Vector<CAAMPointInfo> PointAux() {
		return m_vPointAux;
	}

	/**
	 * set point info vector
	 * 
	 * @param value
	 */
	public void setPointAux(Vector<CAAMPointInfo> value) {
		m_vPointAux = value;
	}

	/**
	 * Set specific point info
	 * 
	 * @param index
	 * @param value
	 */
	public void setPointAux(int index, CAAMPointInfo value) {
		m_vPointAux.set(index, value);
	}

	/**
	 * Get host image.
	 * 
	 * @return image
	 */
	public ModelImage getHostImage() {
		return myHostImage;
	}

}