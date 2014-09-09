package gov.nih.mipav.view.renderer.WildMagic.AAM;

import java.util.*;
import java.io.*;
import java.text.*;

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
 * Container to store evaluation results in. Further, it can print out the
 * results along with some simple statistics.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMEvaluationResults {

	/** vector of segmentation results */
	public Vector<CAAMEvalRes> m_vSegResults = new Vector<CAAMEvalRes>();

	/**
	 * return the segmentation results
	 * 
	 * @return segmentation results
	 */
	public final Vector<CAAMEvalRes> Data() {
		return m_vSegResults;
	}

	/**
	 * Adds a new result to the back.
	 * 
	 * @param model_shape
	 *            model shape
	 * @param ground_truth
	 *            ground truth shape
	 * @param time
	 *            time
	 * @param optRes
	 *            optimization results
	 */
	public void AddResult(final CAAMShape model_shape,
			final CAAMShape ground_truth, final double time,
			final CAAMOptRes optRes) {
		m_vSegResults.add(new CAAMEvalRes(model_shape, ground_truth, time,
				optRes));
	}

	/**
	 * Adds a new set of results to the back.
	 * 
	 * @param results
	 *            a set of results
	 */
	public void AddResults(final CAAMEvaluationResults results) {

		for (int i = 0; i < results.Data().size(); i++) {

			m_vSegResults.add(results.Data().get(i));
		}
	}

	/**
	 * Print statistics
	 */
	public void PrintStatistics() {
		PrintWriter fh = null;
		PrintStatistics(fh);
	}

	/**
	 * Prints all results and some statistics to either file or screen
	 * 
	 * @param fh
	 *            file writer
	 */
	public void PrintStatistics(PrintWriter fh) {

		if (fh == null) {
			fh = new PrintWriter(System.out);
		}
		String s = new String();

		Calendar c = Calendar.getInstance();
		System.out.println("current: " + c.getTime());

		s = c.getTime().toString();

		fh.println("######################################################################");
		fh.println("##    AAM Evaluation  -  written: " + s + "#");
		fh.println("######################################################################");

		int n = m_vSegResults.size();
		CDVector ptpt = new CDVector(n);
		CDVector ptcrv = new CDVector(n);
		CDVector overlap = new CDVector(n);
		CDVector iter = new CDVector(n);
		CDVector maha = new CDVector(n);
		CDVector error = new CDVector(n);
		CDVector time = new CDVector(n);

		DecimalFormat intFormat = new DecimalFormat("########");
		DecimalFormat floatFormat = new DecimalFormat("########.####");

		// print header + entries
		fh.println("\tExp\tPt.pt.\tPt.crv.\tOverlap\tIter\tMaha\tError\tTime");
		for (int i = 0; i < n; i++) {

			// print entry
			fh.println("\t"
					+ intFormat.format(i)
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_dPtPt[0])
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_dPtCrv[0])
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_dOverlap)
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_OptRes.NIter())
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_OptRes
							.Mahalanobis())
					+ "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_OptRes
							.SimilarityMeasure()) + "\t"
					+ floatFormat.format(m_vSegResults.get(i).m_dTime));

			// collect data
			ptpt.m_data[i] = m_vSegResults.get(i).m_dPtPt[0];
			ptcrv.m_data[i] = m_vSegResults.get(i).m_dPtCrv[0];
			overlap.m_data[i] = m_vSegResults.get(i).m_dOverlap;
			iter.m_data[i] = m_vSegResults.get(i).m_OptRes.NIter();
			maha.m_data[i] = m_vSegResults.get(i).m_OptRes.Mahalanobis();
			error.m_data[i] = m_vSegResults.get(i).m_OptRes.SimilarityMeasure();
			time.m_data[i] = m_vSegResults.get(i).m_dTime;
		}
		fh.println();
		fh.println();

		DecimalFormat statsFormat = new DecimalFormat("########.##");
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
		fh.println("Iterations " + "\t" + statsFormat.format(iter.Mean())
				+ "\t" + statsFormat.format((iter.Std() / Math.sqrt(n))) + "\t"
				+ statsFormat.format(iter.Median()) + "\t"
				+ statsFormat.format(iter.Min()) + "\t"
				+ statsFormat.format(iter.Max()));
		fh.println("Mahlanobis " + "\t" + statsFormat.format(maha.Mean())
				+ "\t" + statsFormat.format((maha.Std() / Math.sqrt(n))) + "\t"
				+ statsFormat.format(maha.Median()) + "\t"
				+ statsFormat.format(maha.Min()) + "\t"
				+ statsFormat.format(maha.Max()));
		fh.println("Error      " + "\t" + statsFormat.format(error.Mean())
				+ "\t" + statsFormat.format((error.Std() / Math.sqrt(n)))
				+ "\t" + statsFormat.format(error.Median()) + "\t"
				+ statsFormat.format(error.Min()) + "\t"
				+ statsFormat.format(error.Max()));
		fh.println("Time       " + "\t" + statsFormat.format(time.Mean())
				+ "\t" + statsFormat.format((time.Std() / Math.sqrt(n))) + "\t"
				+ statsFormat.format(time.Median()) + "\t"
				+ statsFormat.format(time.Min()) + "\t"
				+ statsFormat.format(time.Max()));
		fh.println();
	}

	/**
	 * Prints all results to a file.
	 * 
	 * @param filename
	 *            file name
	 */
	public void PrintStatistics(final String filename) {
		try {

			PrintWriter fh = new PrintWriter(filename);
			/*
			 * if (fh == null) {
			 * 
			 * System.err.println("Result file " + filename +
			 * " could not be opened.\n"); return; }
			 */
			PrintStatistics(fh);

			fh.close();
		} catch (IOException error) {
			error.printStackTrace();
		}
	}

}