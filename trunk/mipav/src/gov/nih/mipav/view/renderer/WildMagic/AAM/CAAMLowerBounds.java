package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.text.DecimalFormat;
import java.util.*;
import java.io.*;
import gov.nih.mipav.model.structures.*;

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
 * AAM lower bounds class.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMLowerBounds {

	/** AAM model */
	private CAAMModel m_pModel;

	/** vector to hold the average point to point landmark error. */
	Vector<Double> m_vPtPt = new Vector<Double>();

	/** vector to average point to curve landmark error. */
	Vector<Double> m_vPtCrv = new Vector<Double>();

	/** vector to hold the overlap region. */
	Vector<Double> m_vOverlap = new Vector<Double>();

	/** vector to hold the texture error separable. */
	Vector<Double> m_vTextureErrorSeparate = new Vector<Double>();

	/**
	 * Constructor.
	 */
	public CAAMLowerBounds() {
		m_pModel = null;
	}

	/**
	 * Constructor
	 * 
	 * @param model
	 *            model
	 */
	public CAAMLowerBounds(final CAAMModel model) {
		m_pModel = model;
	}

	/**
	 * Assign operator. Deep copying
	 * 
	 * @param lb
	 *            lower bound reference
	 */
	public CAAMLowerBounds assign(final CAAMLowerBounds lb) {

		this.m_pModel = lb.m_pModel;
		this.m_vPtPt = (Vector<Double>) lb.m_vPtPt.clone();
		this.m_vPtCrv = (Vector<Double>) lb.m_vPtCrv.clone();
		this.m_vOverlap = (Vector<Double>) lb.m_vOverlap.clone();
		this.m_vTextureErrorSeparate = (Vector<Double>) lb.m_vTextureErrorSeparate
				.clone();

		return this;
	}

	/**
	 * Add ground truth
	 * 
	 * @param gt
	 *            lower bound reference.
	 */
	public void AddGroundtruths(final CAAMLowerBounds gt) {

		for (int i = 0; i < gt.m_vPtPt.size(); i++) {

			this.m_vPtPt.add(gt.m_vPtPt.get(i));
			this.m_vPtCrv.add(gt.m_vPtCrv.get(i));
			this.m_vOverlap.add(gt.m_vOverlap.get(i));
			this.m_vTextureErrorSeparate.add(gt.m_vTextureErrorSeparate.get(i));
		}
	}

	/**
	 * Add ground truth from give image and voi
	 * 
	 * @param gt
	 *            ground truth shape
	 * @param image
	 *            given image
	 */
	public void AddGroundtruth(final CAAMShape gt, final ModelSimpleImage image) {

		CAAMDeformPCA shapePCA = m_pModel.ShapePCA();

		// calc model approximation of the ground truth shape
		CAAMShape gt_copy = new CAAMShape(gt);
		CAAMShape meanShape = new CAAMShape(shapePCA.MeanDataItem());
		gt_copy.AlignTo(meanShape);

		CDVector b_s = new CDVector();
		b_s.assign(shapePCA.EigenVectors().Transposed()
				.mult((gt_copy.sub(meanShape))));

		CAAMShape reconstructed_shape = new CAAMShape(meanShape.add(shapePCA
				.EigenVectors().mult(b_s)));
		reconstructed_shape.AlignTo(gt);

		// calc shape distances to the model approximation
		double[] ptpt = new double[1];
		double[] ptcrv = new double[1];
		CAAMUtil.CalcShapeDistances(reconstructed_shape, gt, ptpt, ptcrv, null);
		m_vPtPt.add(ptpt[0]);
		m_vPtCrv.add(ptcrv[0]);

		// calc shape overlap to the mode approximation
		double overlap;
		overlap = CAAMUtil.ShapeOverlap(reconstructed_shape, gt);
		m_vOverlap.add(overlap);

		// calc texture error assuming the shape and texture are not coupled
		CDVector texture = new CDVector();
		m_pModel.SampleShape(image, reconstructed_shape, texture, true, true,
				true);

		CDVector modelTexture = new CDVector(texture);
		m_pModel.TexturePCA().Filter(modelTexture); // make model equivalent

		texture.sub_into(modelTexture); // form difference
		double e = texture.Norm2(); // calc L_2 norm of the difference
		m_vTextureErrorSeparate.add(e);
	}

	/**
	 * print statistics
	 * 
	 * @param filename
	 *            output file stream name
	 */
	public void PrintStatistics(final String filename) {
		try {
			PrintWriter fh = new PrintWriter(filename); // file name path
			this.PrintStatistics(fh);
			fh.close();
		} catch (IOException error) {
			error.printStackTrace();
		}
	}

	/**
	 * print statistics
	 */
	public void PrintStatistics() {
		PrintWriter fh = null;
		PrintStatistics(fh);
	}

	/**
	 * print statistics to the output file
	 * 
	 * @param fh
	 *            output file handler
	 */
	public void PrintStatistics(PrintWriter fh) {

		if (fh == null) {
			fh = new PrintWriter(System.out);
		}
		String s = new String();

		Calendar c = Calendar.getInstance();
		System.out.println("current: " + c.getTime());

		TimeZone z = c.getTimeZone();

		s = c.getTime().toString();

		fh.println("######################################################################");
		fh.println("##    Lower bounds for an AAM  -  written: " + s + "#");
		fh.println("######################################################################");

		// lazy, ugly way to do statistics... I know...
		int n = m_vPtPt.size();
		CDVector ptpt = new CDVector(n);
		CDVector ptcrv = new CDVector(n);
		CDVector overlap = new CDVector(n);
		CDVector textureErrorSeparate = new CDVector(n);

		DecimalFormat intFormat = new DecimalFormat("########");
		DecimalFormat floatFormat = new DecimalFormat("########.####");

		// print header + entries
		fh.println("\tExp\tPt.pt.\tPt.crv.\tOverlap ErrorSep");
		for (int i = 0; i < n; i++) {

			// print entry
			fh.println("\t" + intFormat.format(i) + "\t"
					+ floatFormat.format(m_vPtPt.get(i)) + "\t"
					+ floatFormat.format(m_vPtCrv.get(i)) + "\t"
					+ floatFormat.format(m_vOverlap.get(i)) + "\t"
					+ floatFormat.format(m_vTextureErrorSeparate.get(i)));

			// collect data
			ptpt.m_data[i] = m_vPtPt.get(i);
			ptcrv.m_data[i] = m_vPtCrv.get(i);
			overlap.m_data[i] = m_vOverlap.get(i);
			textureErrorSeparate.m_data[i] = m_vTextureErrorSeparate.get(i);
		}
		fh.println();
		fh.println();

		DecimalFormat statsFormat = new DecimalFormat("########.##");
		// print statistics
		// print statistics
		fh.println("SUMMARY");
		fh.println("\t\tMean\tStd.err\tMedian\tMin\tMax");
		fh.println("Pt.pt.     " + "\t" + statsFormat.format(ptpt.Mean())
				+ "\t" + statsFormat.format((ptpt.Std() / Math.sqrt(n))) + "\t"
				+ statsFormat.format(ptpt.Median()) + "\t"
				+ statsFormat.format(ptpt.Min()) + "\t"
				+ statsFormat.format(ptpt.Max()));
		fh.println("Pt.crv.    " + "\t" + statsFormat.format(ptcrv.Mean())
				+ "\t" + statsFormat.format((ptcrv.Std() / Math.sqrt(n)))
				+ "\t" + statsFormat.format(ptcrv.Median()) + "\t"
				+ statsFormat.format(ptcrv.Min()) + "\t"
				+ statsFormat.format(ptcrv.Max()));
		fh.println("Overlap    " + "\t" + statsFormat.format(overlap.Mean())
				+ "\t" + statsFormat.format((overlap.Std() / Math.sqrt(n)))
				+ "\t" + statsFormat.format(overlap.Median()) + "\t"
				+ statsFormat.format(overlap.Min()) + "\t"
				+ statsFormat.format(overlap.Max()));
		fh.println("ErrorSep   "
				+ "\t"
				+ statsFormat.format(textureErrorSeparate.Mean())
				+ "\t"
				+ statsFormat.format((textureErrorSeparate.Std() / Math.sqrt(n)))
				+ "\t" + statsFormat.format(textureErrorSeparate.Median())
				+ "\t" + statsFormat.format(textureErrorSeparate.Min()) + "\t"
				+ statsFormat.format(textureErrorSeparate.Max()));
		fh.println();
	}

}