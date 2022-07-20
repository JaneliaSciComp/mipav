package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.io.IOException;
import java.util.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.POINT;

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
 * Concrete class implementation of a simple initialization method.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMInitializeStegmann extends CAAMInitialize {

	/** Centered reference shape. */
	public CAAMShape m_sCentredRefshape = new CAAMShape();

	/** model scale steps. */
	private CDVector m_vScaleSteps = new CDVector();

	/** model rotation steps. */
	private CDVector m_vRotationSteps = new CDVector();

	/** Model parameters changes steps. */
	private Vector<CDVector> m_vModelParameterSteps = new Vector<CDVector>();

	/** stepping length, currently not used. */
	private double m_dXspacing, m_dYspacing;

	/** search range, currently not used. */
	private double m_dXmin, m_dXmax, m_dYmin, m_dYmax;

	/** number of iterations. */
	private int m_nIterations1stPass, m_nIterations2ndPass;

	/** number of candidates. */
	private int m_nCandidates;

	/** mean shape polygon. */
	public poly meanShapePolygon;

	/**
	 * Constructor.
	 * 
	 * @param aammodel
	 *            The model to initialize.
	 */
	public CAAMInitializeStegmann(final CAAMModel aammodel) {

		// setup model
		m_pModel = aammodel;
		m_sCentredRefshape.assign(m_pModel.ReferenceShape());

		// center the reference shape
		double[] cogx = new double[1];
		double[] cogy = new double[1];
		m_sCentredRefshape.COG(cogx, cogy);
		m_sCentredRefshape.Translate(-cogx[0], -cogy[0]);

		// setup number of iterations
		m_nIterations1stPass = 3;
		m_nIterations2ndPass = 10;

		// setup number of refinement candidates
		m_nCandidates = 10;

		// ///////////////////////////////
		// setup default search ranges
		// ///////////////////////////////

		// scale
		m_vScaleSteps.Resize(3);
		m_vScaleSteps.m_data[0] = 1.0;
		m_vScaleSteps.m_data[1] = 1.1;
		m_vScaleSteps.m_data[2] = 0.9;

		// rotation
		m_vRotationSteps.Resize(1);
		m_vRotationSteps.m_data[0] = .0; // radians
		// m_vRotationSteps[1] = CAAMUtil::Deg2Rad(20);
		// m_vRotationSteps[2] = CAAMUtil::Deg2Rad(-20);

		// model parameters (mean)
		CDVector cparam = new CDVector(1);
		cparam.m_data[0] = .0; // in standard deviations of that parameter
		m_vModelParameterSteps.add(cparam);

		// default grid size
		m_dXspacing = m_sCentredRefshape.Width() / 5.;
		m_dYspacing = m_sCentredRefshape.Height() / 5.;

		// calc border size
		m_dXmin = m_sCentredRefshape.MinX();
		m_dXmax = m_sCentredRefshape.MaxX();
		m_dYmin = m_sCentredRefshape.MinY();
		m_dYmax = m_sCentredRefshape.MaxY();
	}

	/**
	 * Pause function for testing and debugging purpose.
	 */
	public static void pause() {
		int count = 0;
		System.err.println("enter to continue: ");
		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
		}

	}

	/**
	 * AAM initialization from given aam model, target imge, and sample image
	 * and VOI
	 * 
	 * @param aammodel
	 *            aam model
	 * @param targetImageSlice
	 *            target image
	 * @param sampleImage
	 *            sample image
	 * @param sampleShape
	 *            sample VOI
	 */
	public CAAMInitializeStegmann(final CAAMModel aammodel,
			ModelImage targetImageSlice, ModelImage sampleImage,
			CAAMShape sampleShape) {

		// setup model
		m_pModel = aammodel;
		m_sCentredRefshape.assign(m_pModel.ReferenceShape());

		// center the reference shape
		/*
		 * double[] cogx = new double[1]; double[] cogy = new double[1];
		 * m_sCentredRefshape.COG( cogx, cogy ); m_sCentredRefshape.Translate(
		 * -cogx[0], -cogy[0] ); m_sCentredRefshape.Translate( 250, 250 );
		 */
		m_sCentredRefshape.initAlignTo(sampleShape);

		m_sCentredRefshape.generateVOIs(targetImageSlice);

		VOIVector voiVector = targetImageSlice.getVOIs();
		VOI voi = voiVector.VOIAt(0);

		VOIBaseVector va = voi.getCurves();
		VOIBase v = va.get(0);

		// Vector<VOIBase>[] vArray = voi.getSortedCurves(VOIBase.ZPLANE, zDim);
		// VOIBase v = vArray[6].get(0);

		VOIBase vTemp = (VOIBase) v.clone();

		CAAMShape polygon1Shape = new CAAMShape();
		polygon1Shape.ConvertASFfromVOI(v);
		float polygon1Area = (float) polygon1Shape.ShapeSize();

		int nPts = vTemp.size();

		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		vTemp.exportArrays(xPts, yPts, zPts);
		counterClockwise(xPts, yPts, zPts, nPts);

		meanShapePolygon = new poly(nPts);
		meanShapePolygon.n = nPts;

		for (int z = 0; z < nPts; z++) {
			meanShapePolygon.pt[z] = new POINT();
			meanShapePolygon.pt[z].x = xPts[z];
			meanShapePolygon.pt[z].y = yPts[z];
		}

		// setup number of iterations
		m_nIterations1stPass = 1;
		m_nIterations2ndPass = 3;

		// setup number of refiment candidates
		m_nCandidates = 6200;
		// m_nCandidates = 20;

		// ///////////////////////////////
		// setup default search ranges
		// ///////////////////////////////

		// scale
		/*
		 * m_vScaleSteps.Resize(14); m_vScaleSteps.m_data[0] = 0.7;
		 * m_vScaleSteps.m_data[1] = 0.8; m_vScaleSteps.m_data[2] = 0.9;
		 * m_vScaleSteps.m_data[3] = 1.0; m_vScaleSteps.m_data[4] = 1.1;
		 * m_vScaleSteps.m_data[5] = 1.2; m_vScaleSteps.m_data[6] = 1.3;
		 * m_vScaleSteps.m_data[7] = 1.4; m_vScaleSteps.m_data[8] = 1.5;
		 * m_vScaleSteps.m_data[9] = 1.6; m_vScaleSteps.m_data[10] = 1.7;
		 * m_vScaleSteps.m_data[11] = 1.8; m_vScaleSteps.m_data[12] = 1.9;
		 * m_vScaleSteps.m_data[13] = 2.0;
		 */

		/*
		 * m_vScaleSteps.Resize(5); m_vScaleSteps.m_data[0] = 0.9;
		 * m_vScaleSteps.m_data[1] = 1.0; m_vScaleSteps.m_data[2] = 1.1;
		 * m_vScaleSteps.m_data[3] = 1.05; m_vScaleSteps.m_data[4] = 0.95;
		 */
        /*
		m_vScaleSteps.Resize(3);
		m_vScaleSteps.m_data[0] = 1.0;
		m_vScaleSteps.m_data[1] = 0.9;
		m_vScaleSteps.m_data[2] = 1.1;
		*/
	
		m_vScaleSteps.Resize(7);
		m_vScaleSteps.m_data[0] = 1.0;
		m_vScaleSteps.m_data[1] = 0.9;
		m_vScaleSteps.m_data[2] = 1.1;
		m_vScaleSteps.m_data[3] = 0.8;
		m_vScaleSteps.m_data[4] = 1.2;
		m_vScaleSteps.m_data[5] = 0.7;
		m_vScaleSteps.m_data[6] = 1.3;
	
		// m_vScaleSteps.m_data[6] = .80;
		// m_vScaleSteps.m_data[7] = 1.20;

		// rotation
		m_vRotationSteps.Resize(3);
		m_vRotationSteps.m_data[0] = 0; // radians
		m_vRotationSteps.m_data[1] = CAAMUtil.Deg2Rad(-5);
		m_vRotationSteps.m_data[2] = CAAMUtil.Deg2Rad(5);
		/*
		 * m_vRotationSteps.m_data[0] = -2.; // degrees
		 * m_vRotationSteps.m_data[1] = 2.; m_vRotationSteps.m_data[2] = -4.;
		 * m_vRotationSteps.m_data[3] = 4.; m_vRotationSteps.m_data[4] = -6.;
		 * m_vRotationSteps.m_data[5] = 6.;
		 */
		// model parameters (mean)

	 
		CDVector cparam = new CDVector(4);
		cparam.m_data[0] = .25;
		cparam.m_data[1] = -.25;
		cparam.m_data[2] = .5;
		cparam.m_data[3] = -.5;
        
		// CDVector cparam = new CDVector(1);
		// cparam.m_data[0] = .0; // in standard deviations of that parameter
		// cparam.m_data[1] = .01;
		// cparam.m_data[2] = -.01;
		// cparam.m_data[3] = .05;
		// cparam.m_data[4] = -.05;

		m_vModelParameterSteps.add(cparam);

		// default grid size
		m_dXspacing = m_sCentredRefshape.Width() / 5.;
		m_dYspacing = m_sCentredRefshape.Height() / 5.;

		// calc border size
		m_dXmin = m_sCentredRefshape.MinX();
		m_dXmax = m_sCentredRefshape.MaxX();
		m_dYmin = m_sCentredRefshape.MinY();
		m_dYmax = m_sCentredRefshape.MaxY();
	}

	/**
	 * Change the given VOI to counter clock wise order.
	 * 
	 * @param x
	 *            VOI x coordinate array
	 * @param y
	 *            VOI y coordinate array
	 * @param z
	 *            VOI z coordinate array
	 * @param nPts
	 *            number of VOI points
	 */
	private void counterClockwise(float[] x, float[] y, float[] z, int nPts) {
		float[] x1 = new float[nPts];
		float[] y1 = new float[nPts];
		float[] z1 = new float[nPts];
		int index = 0;
		for (int i = nPts - 1; i >= 0; i--) {
			x1[index] = x[i];
			y1[index] = y[i];
			z1[index] = z[i];
			index++;
		}

		for (int i = 0; i < nPts; i++) {
			x[i] = x1[i];
			y[i] = y1[i];
			z[i] = z1[i];
		}

	}

	/**
	 * Performs a somewhat brute force initialization of an AAM.
	 * 
	 * @param image
	 *            The image beeing searched in.
	 * @param s
	 *            The output shape after initialization.
	 * @param c
	 *            The output model parameters after initialization.
	 * @return 0 on succes, non-zero on errors.
	 */
	public int Initialize(final ModelSimpleImage image, CAAMShape s, CDVector c) {

		// seach rectangle
		double xst, xend, yst, yend;

		// normal search over the complete image
		// (excluding some border due to possible scaling)
		// using the reference shape
		double sb = 1.05; // some border

		int iw = image.Width();
		int ih = image.Height();

		xst = -sb * m_dXmin;
		xend = iw - sb * m_dXmax;
		yst = -sb * m_dYmin;
		yend = ih - sb * m_dYmax;

		// first pass: find candidates
		int cfgs = 0, np = m_pModel.CombinedPCA().NParameters();
		c.Resize(np);
		CAAMOptRes res = new CAAMOptRes();
		CAAMInitCandidates candidates = new CAAMInitCandidates(m_nCandidates);

		// for each model parameter
		for (int mode = 0; mode < m_vModelParameterSteps.size(); mode++) {

			// for each model parameter displacement
			for (int modedisp = 0; modedisp < m_vModelParameterSteps.get(mode)
					.Length(); modedisp++) {

				c.assign(.0);
				// get mode standard deviation
				double stddev = Math
						.sqrt(m_pModel.CombinedPCA().EigenValues().m_data[np
								- mode - 1]);

				// displace
				c.m_data[np - mode - 1] = m_vModelParameterSteps.get(mode).get(
						modedisp)
						* stddev;

				// for each scale
				for (int scale = 0; scale < m_vScaleSteps.Length(); scale++) {

					// for each rotation
					for (int rot = 0; rot < m_vRotationSteps.Length(); rot++) {

						// for each y position
						for (double y = yst; y < yend; y += m_dYspacing) {

							// for each x position
							for (double x = xst; x < xend; x += m_dXspacing) {

								// apply pose
								s.assign(m_sCentredRefshape);
								s.Scale(m_vScaleSteps.m_data[scale], false); // false
																				// since
																				// COG==(0,0)
																				// for
								s.Rotate(m_vRotationSteps.m_data[rot], false); // centred
																				// reference
																				// shape

								// apply translation to (x,y)
								s.Translate(x, y);

								// optimize
								res = m_pModel.OptimizeModel(image, s, c,
										m_nIterations1stPass, null, true);
								++cfgs;

								// record experiment
								CAAMInitEntry e = new CAAMInitEntry(
										res.SimilarityMeasure(), s, c);
								candidates.ApplyForAcceptance(e);
							}
						}
					}
				}
			}
		}

		// pause();

		// 2nd pass: refine candidates
		int nc = candidates.NCandidates();
		System.err.println("nc = " + nc);
		assert (nc > 0);
		double min_e_fit = 1e306;
		int minPos = 0;
		CDVector minC = new CDVector(c.Length());
		CAAMShape minShape = new CAAMShape();
		for (int cand = 0; cand < nc; cand++) {
			// ???????????????????????????????????????? = or assign
			// get shape and model parameters for this candidate
			CDVector c_tmp = new CDVector();
			c_tmp.assign(candidates.Candidate(cand).C());
			CAAMShape s_tmp = new CAAMShape();
			s_tmp.assign(candidates.Candidate(cand).Shape());

			// optimize to convergence
			res = m_pModel.OptimizeModel(image, s_tmp, c_tmp,
					m_nIterations2ndPass, null, false);

			// test results
			if (min_e_fit > res.SimilarityMeasure()) {

				// best configuration obtained -> save
				min_e_fit = res.SimilarityMeasure();
				minShape.assign(s_tmp);
				minC.assign(c_tmp);
			}
		}

		// set output shape to the best configuration
		s.assign(minShape);

		// get the corresponding model parameters
		c.assign(minC);

		// debug
		boolean debug = true;
		if (debug) {
			double[] cx = new double[1];
			double[] cy = new double[1];
			s.COG(cx, cy);
			System.err.println("Final COG=( " + cx[0] + ", " + cy[0]
					+ " ), E_min= " + min_e_fit + ", " + cfgs
					+ " configurations searched ");
		}

		boolean dumpCandidates = false;
		for (int cand = 0; cand < nc && dumpCandidates; cand++) {

			String fn = new String();
			// fn.format( "init_cand%02i.bmp", cand );
			fn = "init_cand" + cand + ".xml";
			CAAMVisualizer.ShapeStill(image,
					candidates.Candidate(cand).Shape(), fn);
		}

		return 0;
	}

	/**
	 * Performs a somewhat brute force initialization of an AAM.
	 * 
	 * @param image
	 *            The image being searched in.
	 * @param s
	 *            The output shape after initialization.
	 * @param c
	 *            The output model parameters after initialization.
	 * @return 0 on succes, non-zero on errors.
	 */
	public int Initialize(final ModelSimpleImage image, CAAMShape s,
			CDVector c, ModelImage targetImageSlice) {

		// seach rectangle
		double xst, xend, yst, yend;

		// normal search over the complete image
		// (excluding some border due to possible scaling)
		// using the reference shape
		double sb = 1.05; // some border

		int iw = image.Width();
		int ih = image.Height();

		/*
		 * xst = -sb*m_dXmin; xend = iw-sb*m_dXmax; yst = -sb*m_dYmin; yend =
		 * ih-sb*m_dYmax;
		 */
		xst = m_dXmin;
		xend = m_dXmax;
		yst = m_dYmin;
		yend = m_dYmax;

		// first pass: find candidates
		int cfgs = 0, np = m_pModel.CombinedPCA().NParameters();
		c.Resize(np);
		CAAMOptRes res = new CAAMOptRes();
		CAAMInitCandidates candidates = new CAAMInitCandidates(m_nCandidates);

		double[] cogx = new double[1];
		double[] cogy = new double[1];
		m_sCentredRefshape.COG(cogx, cogy);

		double meanShapeSize = m_sCentredRefshape.ShapeSize();
		double optShapeSize;
		double scaleSize;
		// for each model parameter
		for (int mode = 0; mode < m_vModelParameterSteps.size(); mode++) {

			// for each model parameter displacement
			for (int modedisp = 0; modedisp < m_vModelParameterSteps.get(mode)
					.Length(); modedisp++) {

				c.assign(.0);
				// get mode standard deviation
				double stddev = Math
						.sqrt(m_pModel.CombinedPCA().EigenValues().m_data[np
								- mode - 1]);

				// displace
				c.m_data[np - mode - 1] = m_vModelParameterSteps.get(mode).get(
						modedisp)
						* stddev;

				// for each scale
				for (int scale = 0; scale < m_vScaleSteps.Length(); scale++) {

					// for each rotation
					for (int rot = 0; rot < m_vRotationSteps.Length(); rot++) {

						// for each y position
						// for(double y=yst;y<yend;y+=m_dYspacing) {
						for (int y = -15; y <= 15; y += 15) {
							// for each x position
							// for(double x=xst;x<xend;x+=m_dXspacing) {
							for (int x = -15; x <= 15; x += 15) {
								// apply pose
								// s.Translate( -250, -250 );

								// m_sCentredRefshape.Translate( -cogx[0],
								// -cogy[0] );
								// s = new CAAMShape();
								s.assign(m_sCentredRefshape);

								/*
								 * s.generateVOIs(targetImageSlice); new
								 * ViewJFrameImage
								 * ((ModelImage)(targetImageSlice.clone()));
								 * pause();
								 */
								s.Translate(-cogx[0], -cogy[0]);

								s.Scale(m_vScaleSteps.m_data[scale], true); // false
																			// since
																			// COG==(0,0)
																			// for
								// s.Rotate( m_vRotationSteps.m_data[rot], false
								// ); // centred reference shape

								// apply translation to (x,y)
								s.Translate(x, y);

								s.Translate(cogx[0], cogy[0]);

								// s.generateVOIs(targetImageSlice);
								// new
								// ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
								// s.generateVOImesh(targetImageSlice);
								// new ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
								// pause();

								res = m_pModel.OptimizeModel(image, s, c,
										m_nIterations1stPass, null, false);

								if (res == null)
									continue;
								// s.generateVOIs(targetImageSlice);
								// new
								// ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
								optShapeSize = s.ShapeSize();

								++cfgs;

								CAAMInitEntry e = new CAAMInitEntry(
										res.SimilarityMeasure(), s, c);
								candidates.ApplyForAcceptance(e);

							}
						}
					} // end rot
				} // end scale
			}
		}

		// 2nd pass: refine candidates
		int nc = candidates.NCandidates();
		assert (nc > 0);
		double min_e_fit = 1e306;
		int minPos = 0;
		CDVector minC = new CDVector(c.Length());
		CAAMShape minShape = new CAAMShape();
		System.err.println("nc = " + nc);
		for (int cand = 0; cand < nc; cand++) {
			
			// get shape and model parameters for this candidate
			CDVector c_tmp = new CDVector();
			c_tmp.assign(candidates.Candidate(cand).C());
			CAAMShape s_tmp = new CAAMShape();
			s_tmp.assign(candidates.Candidate(cand).Shape());

			// s_tmp.generateVOIs(targetImageSlice);
			// new ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
			// pause();
			// meanShapeSize = s_tmp.ShapeSize();

			// optimize to convergence
			res = m_pModel.OptimizeModel(image, s_tmp, c_tmp,
					m_nIterations2ndPass, null, false);
			// optShapeSize = s_tmp.ShapeSize();
			// scaleSize = optShapeSize / meanShapeSize;
			if (res == null)
				continue;
			
			
			if (min_e_fit > res.SimilarityMeasure() && s_tmp.m_data != null) {

				// best configuration obtained -> save
				min_e_fit = res.SimilarityMeasure();
				minShape.assign(s_tmp);
				minC.assign(c_tmp);
			}
			
		}

		// set output shape to the best configuration
		/*
		 * if ( minShape.m_data == null ) { s.assign(m_sCentredRefshape); } else
		 * { s.assign(minShape); }
		 */
		if (minShape.m_data != null && s.m_data != null) {
			s.assign(minShape);
		}

		// s.generateVOImesh(targetImageSlice);
		// new ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
		
		// get the corresponding model parameters
		c.assign(minC);

		// s.generateVOIs(targetImageSlice);
		// new ViewJFrameImage((ModelImage)(targetImageSlice.clone()));
		// pause();

		// debug
		boolean debug = true;
		if (debug) {
			double[] cx = new double[1];
			double[] cy = new double[1];
			s.COG(cx, cy);
			System.err.println("Final COG=( " + cx[0] + ", " + cy[0]
					+ " ), E_min= " + min_e_fit + ", " + cfgs
					+ " configurations searched ");
		}

		boolean dumpCandidates = false;

		for (int cand = 0; cand < nc && dumpCandidates; cand++) {

			String fn = new String();
			// fn.format( "init_cand%02i.bmp", cand );
			fn = "init_cand" + cand + ".xml";
			CAAMVisualizer.ShapeStill(image,
					candidates.Candidate(cand).Shape(), fn);
		}

		// s.assign(m_sCentredRefshape);

		return 0;
	}

	/**
	 * Set the number of candidates
	 * 
	 * @param i
	 *            candidiate index
	 */
	public void SetNCandidates(final int i) {
		m_nCandidates = i;
	}

	/**
	 * Sets the number of iterations in the coarse search.
	 * 
	 * @param i
	 *            iteration number
	 */
	public void SetIterations1stPass(final int i) {
		m_nIterations1stPass = i;
	}

	/**
	 * Sets the number of iterations in the refinement search.
	 * 
	 * @param i
	 *            number of iterations
	 */
	public void SetIterations2ndPass(final int i) {
		m_nIterations2ndPass = i;
	}

	/**
	 * Sets the spacing of the search grid in x and y.
	 * 
	 * @param xs
	 *            x coordinate step
	 * @param ys
	 *            y coordinate step
	 */
	public void SetXYSpacing(final double xs, final double ys) {
		m_dXspacing = xs;
		m_dYspacing = ys;
	}

	/**
	 * Sets the number of scale steps.
	 * 
	 * @param v
	 *            scale vector
	 */
	public void SetScaleSteps(final CDVector v) {
		m_vScaleSteps.Resize(v.Length());
		m_vScaleSteps = v;
	}

	/**
	 * Sets the number of rotation steps.
	 * 
	 * @param v
	 *            rotation vector
	 */
	public void SetRotationSteps(final CDVector v) {
		m_vRotationSteps.Resize(v.Length());
		m_vRotationSteps = v;
	}

	/**
	 * Sets the number of model parameter steps.
	 * 
	 * @param av
	 *            parameters change vector
	 */
	public void SetModelParameterSteps(final Vector<CDVector> av) {
		m_vModelParameterSteps = av;
	}

	/**
	 * Container for initialization results.
	 */
	class CAAMInitEntry {

		/** fit measure */
		private double m_dE_fit;

		/** shape */
		private CAAMShape m_Shape = new CAAMShape();

		/** model parameters vector. */
		private CDVector m_vC = new CDVector();

		/**
		 * Constructor
		 * 
		 * @param e_fit
		 *            fit measure
		 * @param s
		 *            shape
		 * @param c
		 *            parameter changes
		 */
		public CAAMInitEntry(final double e_fit, final CAAMShape s,
				final CDVector c) {
			m_dE_fit = e_fit;
			m_Shape.assign(s);
			m_vC.assign(c);
		}

		/**
		 * Returns the fit measure.
		 * 
		 * @return fit measure
		 */
		public double E_fit() {
			return m_dE_fit;
		}

		/**
		 * Returns the corresponding shape.
		 * 
		 * @return shape
		 */
		public CAAMShape Shape() {
			return m_Shape;
		}

		/**
		 * Returns the corresponding model parameters.
		 * 
		 * @return model parameters
		 */
		public CDVector C() {
			return m_vC;
		}
	};

	/**
	 * Initialization candidate containiner. Holds the 'n' best candidates that
	 * has applieed for acceptance.
	 */
	class CAAMInitCandidates {

		/** max candidate number. */
		private int m_iNMaxCand;

		/** initial entry vector. */
		private Vector<CAAMInitEntry> m_vInitEntries = new Vector<CAAMInitEntry>();

		/**
		 * Allocates a pool with room for 'n' candidates.
		 * 
		 * @param n
		 *            The number of candidates.
		 */
		public CAAMInitCandidates(int n) {

			m_iNMaxCand = n;

			// m_vInitEntries.setSize( m_iNMaxCand );

		}

		/**
		 * Apply for acceptance of a new initialization hypothesis. The entry
		 * are accepted if it is better than the worst candiate, or if there is
		 * less than 'n' candidates in the set.
		 * 
		 * @param e
		 *            The seach result, which is applieing for acceptance.
		 * @return True if it is accepted, false if not.
		 */
		public boolean ApplyForAcceptance(final CAAMInitEntry e) {

			// find max
			int len = m_vInitEntries.size();
			double max = -1e306;
			int maxPos = -1;

			if (e.E_fit() < .0) {

				// something's wrong
				// the model is probably outside the image
				return false;
			}

			if (len < m_iNMaxCand) {

				m_vInitEntries.add(e);
				return true;
			}

			for (int i = 0; i < len; i++) {

				if (m_vInitEntries.get(i).E_fit() > max) {

					max = m_vInitEntries.get(i).E_fit();
					maxPos = i;
				}
			}

			if (e.E_fit() < max) {

				if (maxPos != -1) {

					m_vInitEntries.remove((m_vInitEntries.get(maxPos)));
				}

				m_vInitEntries.add(e);

				return true;
			}

			return false;
		}

		// / Returns the number of candidates.
		public int NCandidates() {
			return m_vInitEntries.size();
		}

		// / Returns the i-th candidate.
		public CAAMInitEntry Candidate(int i) {
			return m_vInitEntries.get(i);
		}
	};

}