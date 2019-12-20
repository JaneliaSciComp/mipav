package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.*;

import java.awt.Point;
import java.io.*;
import java.util.*;
import java.time.format.DateTimeFormatter;
import java.time.LocalDateTime;
import javax.vecmath.*;

import Jama.Matrix;
import Jama.SingularValueDecomposition;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
 * libdt - OpenCV library for Dynamic Textures - version 1.0
 * 
 * Copyright (c) 2011-2014 Antoni B. Chan, Adeel Mumtaz, City University of Hong
 * Kong All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * 3. Neither name of copyright holders nor the names of its contributors may be
 * used to endorse or promote products derived from this software without
 * specific prior written permission.
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

public class libdt extends AlgorithmBase {
	private RandomAccessFile raFile;
	private boolean endian = false;
	/** byte array for int * */
	private final byte[] byteIntBuffer = new byte[4];
	/** byte array for double * */
	private final byte[] byteDoubleBuffer = new byte[8];
	/** byte array for long * */
    private final byte[] byteLongBuffer = new byte[8];
	private final int CV_8U = 0;
	private final int CV_8UC1 = 0;
	private final int CV_8UC = 1;
	private final int CV_64F = 6;
	private final int CV_64FC1 = 6;
	private final int CV_64FC = 7;
	private final int CV_64FC3 = 22;
	private final int CV_REDUCE_SUM = 0;
	private final int CV_REDUCE_AVG = 1;
	private final int CV_REDUCE_MAX = 2;
	private final int CV_REDUCE_MIN = 3;
	private final int MAXCOL = 12;
	private Mat g_mask_lut[][] = new Mat[MAXCOL][3];
	private double g_rgbtable[][] = new double[][]{ {1.0, 1.0, 1.0},  // 0: clear
					 {1.0, 0.7, 0.7},  // 1: pink
					 {0.7, 1.0, 0.7},  // 2: light green
					 {0.7, 0.7, 1.0},  // 3: indigo
					 {1.0, 1.0, 0.0},  // 4: yellow
					 {1.0, 0.0, 1.0},  // 5: magenta
					 {0.0, 1.0, 1.0},  // 6: cyan
					 {1.0, 0.0, 0.0},  // 7: red
					 {0.0, 1.0, 0.0},  // 8: green
					 {0.0, 0.0, 1.0},  // 9: blue
					 {0.5, 1.0, 1.0},  // 10: ?
					 {1.0, 1.0, 1.0},  // 11: white
	};
	private boolean debug = true;

	public libdt() {

	}
	/*
	 * ! <Full 20 trial BoS classification on UCLA9 Eight Class Data Set!!>
	 * 
	 * Copyright (c) 2014 by <Adeel Mumtaz/ VISAL@City University of Hong Kong>
	 * libdt - OpenCV library for Dynamic Textures - version 1.0
	 */

	/**
	 * #include <iostream> #include <iomanip> #include <string> #include
	 * <iterator> #include<fstream> #include<math.h> #include <ctime>
	 * 
	 * #include "opencv/cv.h" #include "opencv/cxcore.h" #include
	 * "opencv/highgui.h"
	 * 
	 * #include "options.h" #include "utils/libutils.h" #include
	 * "stats/libstats.h" #include "video/libvideo.h" #include
	 * "dytex/libdytex.h" #include "platInd/platInd.hpp" #include
	 * "bufferer/Bufferer.hpp"
	 * 
	 * using namespace cv; using namespace std;
	 */

	public void runAlgorithm() {

	}

	private String getTime() {
		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
		LocalDateTime now = LocalDateTime.now();
		return dtf.format(now);
	}

	public void test_HEM() {
		File file;
		System.out.println("Experiment started: " + getTime());
		file = new File("C:/temporal texture/libdt-v1.0/libdt-v1.0/testdata/HEM/47fa110.dtm");
		try {
			raFile = new RandomAccessFile(file, "r");
		} catch (FileNotFoundException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		// load existing dtm
		DytexMix dtm = new DytexMix();
		read(dtm);
		try {
			raFile.close();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

		// setting up HEM to reduce mixture to only 4 components
		DytexRegOptions ropt = new DytexRegOptions(cov_reg_type.COV_REG_MIN, 0.01, cov_reg_type.COV_REG_MIN, 0.01,
				cov_reg_type.COV_REG_MIN, 0.01, cov_reg_type.COV_REG_ADD, 0.999);
		HEMOptions hopt = new HEMOptions(4, ropt, 0.0001, Ymean_type.NONZERO_YMEAN, Verbose_mode.COMPACT);

		// split schedule of 1,2,4
		for (int i = 1; i <= 4; i = i * 2)
			hopt.splitOpt.sched.add(i);

		// run The HEM
		DytexMix emout = reduceWithSplitting(dtm, hopt);

		for (int i = 0; i < emout.alpha.size(); i++)
			System.out.println("Alpha " + (i + 1) + " is " + emout.alpha.get(i));

		System.out.println("Experiment Finish: " + getTime());

	}

	private DytexMix reduceWithSplitting(DytexMix dtm, HEMOptions hopt) {
		int j, k;
		// reduced mixture
		DytexMix hembest = new DytexMix(dtm.opt);
		// OPTIONS
		// double pert=hopt.splitOpt.pert;
		int Ks = hopt.K;
		// initialize splitting sequence
		if (hopt.splitOpt.sched.isEmpty()) {
			for (int i = 1; i <= hopt.K; i++)
				hopt.splitOpt.sched.add(i);
		}

		// %%% preprocess %%%
		System.out.println("preprocessing DT...");
		for (int i = 0; i < dtm.dt.size(); i++) {
			if ((dtm.dt.get(i).C.dims == 2) && (dtm.dt.get(i).C.type == CV_64F)) {
				Matrix cMat = new Matrix(dtm.dt.get(i).C.double2D);
				SingularValueDecomposition svd = new SingularValueDecomposition(cMat);
				Matrix matV = svd.getV();
				double singularValues[] = svd.getSingularValues();
				double arrSingular[][] = new double[singularValues.length][singularValues.length];
				for (j = 0; j < singularValues.length; j++) {
					arrSingular[j][j] = singularValues[j];
				}
				Matrix matS = new Matrix(arrSingular);
				Matrix matVS = matV.times(matS);
				double arrVS[][] = matVS.getArray();
				dtm.dt.get(i).Cvs.create(arrVS.length, arrVS[0].length, CV_64F);
				for (j = 0; j < arrVS.length; j++) {
					for (k = 0; k < arrVS[0].length; k++) {
						dtm.dt.get(i).Cvs.double2D[j][k] = arrVS[j][k];
					}
				}
				dtm.dt.get(i).isCvs = true;
			} // if ((dtm.dt.get(i).C.dims == 2) && (dtm.dt.get(i).C.type ==
				// CV_64F))
			else {
				MipavUtil.displayError("For SVD dims = " + dtm.dt.get(i).C.dims + " type = " + dtm.dt.get(i).C.type);
				System.exit(-1);
			}
		} // for(int i=0;i<dtm.dt.size();i++)

		// check for valid splitting sched
		if (hopt.splitOpt.sched.get(0) != 1) {
			MipavUtil.displayError("schedule must start with 1!");
			System.exit(-1);
		}
		Vector<Integer> tmp = new Vector<Integer>();
		for (int i = 1; i < hopt.splitOpt.sched.size(); i++)
			tmp.add(hopt.splitOpt.sched.get(i) / hopt.splitOpt.sched.get(i - 1));

		for (j = 0; j < tmp.size(); j++) {
			if (tmp.get(j) > 2) {
				MipavUtil.displayError("Cannot grow K more than 2 times previous");
				System.exit(-1);
			}
		}

		System.out.print("Growing schedule: ");
		for (j = 0; j < hopt.splitOpt.sched.size(); j++) {
			System.out.print(hopt.splitOpt.sched.get(j) + " ");
		}
		System.out.print("\n");
		System.out.println("Ks: " + Ks);
		// HEM splitting loop
		int Kiter = 1;
		while (hembest.dt.size() < hopt.K) {
			if (Kiter == 1) {
				System.out.println("*** EM: K= " + (hembest.dt.size() + 1) + " ***********************");
			} else {
				Vector<Integer> mysplits = new Vector<Integer>();
				// split here
				while (hembest.dt.size() < hopt.splitOpt.sched.get(Kiter - 1)) {
					DytexSplitParams splitopt = new DytexSplitParams();
					splitopt.crit = hopt.splitOpt.crit;
					splitopt.ignore = mysplits;
					splitopt.target = -1;
					splitopt.pert = hopt.splitOpt.pert;
					splitopt.mode = hopt.splitOpt.mode;
					splitopt.vars = hopt.splitOpt.vars;
					int c1[] = new int[1];
					int c2[] = new int[1];
					dytex_mix_split(hembest, splitopt, c2, c1);
					mysplits.add(c1[0]);
					mysplits.add(c2[0]);
				}
				// remove pre-cache (since it is invalid after splitting)
				for (int ii = 0; ii < hembest.dt.size(); ii++) {
					hembest.dt.get(ii).isCvs = false;
				}
				System.out.println("*** EM: K= " + hembest.dt.size() + " ******************");
			}
			Vector<Integer> classes = new Vector<Integer>();
			// runs HEM algorithm for current mixture
			runHEM(dtm, hembest, hopt, classes);
			Kiter++;
		}

		// RUN HEM again on once on final solution
		hopt.termvalue = hopt.termvalBest;
		hopt.maxiter = 50; // Can be adjusted to run more iterations
		runHEM(dtm, hembest, hopt, hembest.classes);
		return hembest;
	}

	/*
	 * ! \brief run iterations of HEM for a mixture of DT
	 * 
	 * \param hembest input DT mixture.
	 * 
	 * \param hopt learning option for HEM.
	 * 
	 * \param classes Class of each input DT that is ID of the new DT
	 * 
	 * \remarks in general, this should not be called.use reduceWithSplitting
	 * instead
	 * 
	 * \see reduceWithSplitting | HEMOptions
	 */
	private void runHEM(DytexMix dtm, DytexMix hembest, HEMOptions hopt, Vector<Integer> classes) {
		int i, j, r, c;
		// used to display info in change in classes during EM loop
		long elapsedtime;
		int numlastclasses = 5;
		boolean FlagYmean;
		if (hopt.Ymean == Ymean_type.ZERO_YMEAN) {
			FlagYmean = false;
		} else {
			FlagYmean = true;
		}
		Verbose_mode FlagVerbose = hopt.verbose;

		int Kb = dtm.dt.size();
		if (FlagVerbose != Verbose_mode.QUIET)
			System.out.println("Preprocessing " + Kb + " base components...");

		for (i = 0; i < Kb; i++) {
			if (dtm.dt.get(i).dtopt.Yopt != hopt.Ymean) {
				System.out.println("** Warning: hemopt.Ymean does not match " + dtm.dt.get(i).dtopt.Yopt);
			}
			// Preprocessing already done
		}

		// HEM parameters
		int n = dtm.dt.get(0).dtopt.n;
		int m = dtm.dt.get(0).dtopt.m;
		if (FlagVerbose != Verbose_mode.QUIET)
			System.out.println("n = " + n);
		System.out.println("m = " + m);
		System.out.println("Ymean = " + dtm.dt.get(0).dtopt.Yopt);

		int Nvs = hopt.N;
		int tau = hopt.tau;
		// min total probability for blank cluster
		double MINPROB = (((double) 1.0) / (((double) 2.0) * (double) Kb));

		// initializations
		if (hembest.dt.size() == 0) {
			if (FlagVerbose != Verbose_mode.QUIET)
				System.out.println("Initializing First DT with Sub-optimal: ");

			// average of all DTs
			Dytex tmpC = init_multiple_dt(dtm);
			hembest.dt.add(tmpC);
			hembest.alpha.add(1.0);
		}

		// current mixture size
		int Kr = hembest.dt.size();

		// Regularize the initializations
		for (i = 0; i < Kr; i++) {
			setRegularizer(hembest.dt.get(i), hopt.regopt);
			regularize(hembest.dt.get(i), true);

			if (hembest.dt.get(i).isCvs == false) {
				Matrix cMat = new Matrix(hembest.dt.get(i).C.double2D);
				SingularValueDecomposition svd = new SingularValueDecomposition(cMat);
				double singularValues[] = svd.getSingularValues();
				Mat Cv = new Mat(svd.getV().getArray());
				double arr[][] = new double[singularValues.length][singularValues.length];
				for (j = 0; j < singularValues.length; j++) {
					arr[j][j] = singularValues[j];
				}
				Mat Cs = new Mat(arr);
				Mat tmpM = times(Cv, Cs);
				copyTo(tmpM, hembest.dt.get(i).Cvs);
				hembest.dt.get(i).isCvs = true;
			}
		}

		// %%% RUN HEM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		// initialize convergence measures
		Vector<Double> datalikelihood = new Vector<Double>(hopt.maxiter + 1);
		for (i = 0; i < hopt.maxiter + 1; i++) {
			datalikelihood.add(0.0);
		}
		Vector<Double> ddtall = new Vector<Double>(hopt.maxiter);
		Vector<Double> pdtall = new Vector<Double>(hopt.maxiter);
		for (i = 0; i < hopt.maxiter; i++) {
			ddtall.add(0.0);
			pdtall.add(0.0);
		}
		Vector<Vector<Integer>> lastclasses = new Vector<Vector<Integer>>(numlastclasses);
		int lastclassesind = 0;

		for (i = 0; i < numlastclasses; i++) {
			lastclasses.add(new Vector<Integer>(Kb));
			for (j = 0; j < Kb; j++) {
				lastclasses.get(i).add(0);
			}
		}

		// initialize blanks
		Vector<Double> blank = new Vector<Double>(Kr);
		for (i = 0; i < Kr; i++) {
			blank.add(0.0);
		}
		for (j = 0; j < Kr; j++) {
			if (hembest.dt.get(j).isempty)
				blank.set(j, 1.0);
		}

		// initialize loop
		long starttime = System.currentTimeMillis();
		int iter = 0;
		// hem loop
		while (true) {
			// compute statistics between 2 DT for HEM E-step
			Estats Estat = new Estats(dtm.dt, hembest.dt, tau, FlagYmean);
			computeEll(Estat);
			Mat ell = clone(Estat.Ell);
			Mat tmpM = new Mat(dtm.alpha.size(), 1, CV_64F);
			for (r = 0; r < dtm.alpha.size(); r++) {
				tmpM.double2D[r][0] = dtm.alpha.get(r);
			}
			tmpM = times(tmpM, Nvs);
			Mat tmpM2 = new Mat();
			repeat(tmpM, 1, Kr, tmpM2);

			Mat tmpM4 = new Mat(1, hembest.alpha.size(), CV_64F);
			for (c = 0; c < hembest.alpha.size(); c++) {
				tmpM4.double2D[0][c] = Math.log(hembest.alpha.get(c));
			}
			Mat tmpM5 = new Mat();
			repeat(tmpM4, Kb, 1, tmpM5);
			tmpM = elementTimes(ell, tmpM2);

			// aggregated statistics for dti and dtj
			ell = plus(tmpM, tmpM5);

			// soft assignment and data likelihood
			Mat logZ = new Mat(Kb, Kr, CV_64F);
			Mat tmp = transpose(logtrick(transpose(ell)));
			for (j = 0; j < Kr; j++) {
				for (r = 0; r < Kb; r++) {
					logZ.double2D[r][j] = ell.double2D[r][j] - tmp.double2D[r][0];
				}
			}

			double sumtmp = 0.0;
			for (r = 0; r < Kb; r++) {
				sumtmp += tmp.double2D[r][0];
			}
			datalikelihood.set(iter, sumtmp);
			Mat Z = new Mat(Kb, Kr, CV_64F);
			for (r = 0; r < Kb; r++) {
				for (c = 0; c < Kr; c++) {
					Z.double2D[r][c] = Math.exp(logZ.double2D[r][c]);
				}
			}

			if (FlagVerbose == Verbose_mode.VERBOSE)
				System.out.print("\n");

			// hard assignment
			classes.clear();
			for (i = 0; i < Z.rows; i++) {
				int maxL = -1;
				double max = -Double.MAX_VALUE;
				for (c = 0; c < Z.cols; c++) {
					if (Z.double2D[i][c] > max) {
						max = Z.double2D[i][c];
						maxL = c;
					}
				}
				classes.add(maxL + 1);
			}

			// Check Convergence
			double ddLL = 0;
			double dpLL = 0;

			if (iter > 0) {
				// compute change in log-likelihood
				ddLL = datalikelihood.get(iter) - datalikelihood.get(iter - 1);
				dpLL = Math.abs(ddLL / datalikelihood.get(iter - 1));
			} else {
				ddLL = Double.MAX_VALUE;
				dpLL = Double.MAX_VALUE;
			}
			// class assignment info
			lastclasses.set(lastclassesind, classes);

			// count the number of class changes
			Vector<Integer> dclass = new Vector<Integer>();
			for (int ii = 0; ii < numlastclasses; ii++) {
				int sum = 0;
				for (i = 0; i < lastclasses.get(0).size(); i++) {
					if (lastclasses.get(ii).get(i) != lastclasses.get(lastclassesind).get(i))
						sum++;
				}
				dclass.add(sum);
			}

			String dclassstr = "";
			for (i = lastclassesind + 1; i < numlastclasses; i++) {
				dclassstr = dclassstr + String.valueOf(dclass.get(i)) + " ";
			}
			for (i = 0; i < lastclassesind; i++) {
				dclassstr = dclassstr + String.valueOf(dclass.get(i)) + " ";
			}

			// % lastclassind points to the oldest classes
			lastclassesind = lastclassesind + 1;
			if (lastclassesind >= numlastclasses)
				lastclassesind = 0;

			// output strings
			String outstr2 = "dclass = " + dclassstr;
			String outstr1s;

			outstr1s = "L= " + datalikelihood.get(iter) + " (pL= " + dpLL + ")";

			if (FlagVerbose != Verbose_mode.QUIET) {
				System.out.println("iter= " + (iter + 1));
				System.out.println(outstr1s);
				System.out.println(outstr2);
			}

			// check if negative change in log-likelihood!
			if (ddLL < 0) {
				System.out.println("WARNING -- change in log likelihood is negative???");
			}
			// %%% check convergence conditions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			int breakout = 0;
			String termstr = null;
			if (iter >= hopt.maxiter) {
				termstr = "***** done -- max iter reached\n";
				breakout = 1;
			}
			// only this convergence condition
			if ((ddLL >= 0) && (dpLL < hopt.termvalue)) {
				termstr = "***** done -- small percent change in data likelihood\n";
				breakout = 1;
			}

			// %%% convergence condition was reached...
			// %%%%%%%%%%%%%%%%%%%%%%%%%%%%
			if (breakout == 1) {
				if (FlagVerbose != Verbose_mode.QUIET) {
					System.out.println(termstr);
				}
				break;
			}

			// %%% M-Step %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

			// compute Nhat - total soft assignments per cluster
			// 1) update prior probabilities
			Mat Nhat = new Mat();
			reduce(Z, Nhat, 0, CV_REDUCE_SUM);
			tmp = divide(Nhat, (double) Kb);
			for (i = 0; i < hembest.alpha.size(); i++)
				hembest.alpha.set(i, tmp.double2D[0][i]);

			// compute weights: Zij * alpha(i)
			Mat alphaMat = new Mat(dtm.alpha.size(), 1, CV_64F);
			for (i = 0; i < dtm.alpha.size(); i++) {
				alphaMat.double2D[i][0] = dtm.alpha.get(i);
			}
			tmpM = new Mat();
			repeat(alphaMat, 1, Kr, tmpM);
			Mat W = elementTimes(Z, tmpM);
			// normalize weights
			tmpM = new Mat();
			reduce(W, tmpM, 0, CV_REDUCE_SUM);
			tmpM2 = new Mat();
			repeat(tmpM, Kb, 1, tmpM2);
			Mat tmpM3 = divide(W, tmpM2);
			W = clone(tmpM3);
			computeAggregateStats(Estat, W);

			// %%% loop through each cluster %%%
			for (j = 0; j < Kr; j++) {
				// check if this is cluster is blank
				if (hembest.alpha.get(j) <= MINPROB) {
					blank.set(j, 1.0);
					hembest.dt.get(j).isempty = true;
					if (FlagVerbose != Verbose_mode.QUIET)
						System.out.print("blank");
				} else // % --- standard M-step: learn the parameters
						// -------------------------
				{

					Mat xij = Estat.xij.get(j);
					Mat etaj = Estat.etaj.get(j);
					Mat gammaj = Estat.gammaj.get(j);
					Mat Phij = Estat.Phij.get(j);
					Mat varphij = Estat.varphij.get(j);
					Mat phij = Estat.phij.get(j);
					Mat betaj = Estat.betaj.get(j);
					Mat Psij = Estat.Psij.get(j);
					double Lambdaj = Estat.Lambdaj.get(j);
					Mat Gammaj = Estat.Gammaj.get(j);

					// %%% compute new parameters %%%

					// C parameter
					Mat iPhij = new Mat((new Matrix(Phij.double2D)).inverse().getArray());
					Mat newC = times(Gammaj, iPhij);
					hembest.dt.get(j).C = newC;
					// update preprocessing step
					SingularValueDecomposition svd = new SingularValueDecomposition(new Matrix(newC.double2D));
					Mat matV = new Mat(svd.getV().getArray());
					double singularValues[] = svd.getSingularValues();
					Mat test = new Mat(singularValues.length, singularValues.length, CV_64F);
					for (r = 0; r < singularValues.length; r++) {
						test.double2D[r][r] = singularValues[r];
					}
					hembest.dt.get(j).Cvs = times(matV, test);
					hembest.dt.get(j).isCvs = true;

					// R parameter
					for (r = 0; r < hembest.dt.get(j).R.mtx.rows; r++) {
						for (c = 0; c < hembest.dt.get(j).R.mtx.cols; c++) {
							hembest.dt.get(j).R.mtx.double2D[r][c] = (Lambdaj
									- trace(times(iPhij, times(transpose(Gammaj), Gammaj)))) / (m);
						}
					}

					// A parameter
					Mat newA = times(Psij, new Mat((new Matrix(phij.double2D)).inverse().getArray()));
					hembest.dt.get(j).A = newA;

					// Q parameter
					hembest.dt.get(j).Q.mtx = minus(varphij, times(newA, transpose(Psij)));

					// mu parameter
					Mat newmu = clone(xij);
					hembest.dt.get(j).mu0 = newmu;

					// S parameter
					Mat newS = minus(etaj, times(newmu, transpose(newmu)));
					switch (dtm.opt.Sopt) {
					case COV_DIAG:
						hembest.dt.get(j).S0.mtx = new Mat(newS.rows, 1, CV_64F);
						for (r = 0; r < newS.rows; r++) {
							hembest.dt.get(j).S0.mtx.double2D[r][0] = newS.double2D[r][r];
						}
						hembest.dt.get(j).S0.covopt = cov_type.COV_DIAG;
						break;
					default:
						MipavUtil.displayError("TO DO");
						System.exit(-1);
					}

					// Ymean parameter
					Mat newYmean;
					if (FlagYmean) {
						newYmean = minus(gammaj, times(newC, betaj));
					} else {
						newYmean = new Mat(m, 1, CV_64F);
					}
					hembest.dt.get(j).Ymean = newYmean;

					// regularize the new parameters
					setRegularizer(hembest.dt.get(j), hopt.regopt);
					regularize(hembest.dt.get(j), true);
				}
			}

			// %%% handle empty clusters
			// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			//// find the largest cluster and split it to fill the blank cluster
			for (j = 0; j < Kr; j++) {
				if ((blank.get(j) != null) && (blank.get(j) != 0.0)) {
					if (FlagVerbose != Verbose_mode.QUIET)
						System.out.println("Cluster " + j + " Is blank");

					DytexSplitParams splitopt = new DytexSplitParams();
					splitopt.crit = hopt.emptySplitOpt.crit;
					splitopt.pert = hopt.emptySplitOpt.pert;
					splitopt.mode = hopt.emptySplitOpt.mode;
					splitopt.vars = hopt.emptySplitOpt.vars;
					// splitopt.len=tau;
					splitopt.ignore.clear();
					splitopt.target = j + 1;
					int c1[] = new int[1];
					int c2[] = new int[1];
					dytex_mix_split(hembest, splitopt, c1, c2);

					blank.set(j, 0.0);
				}
			}

			elapsedtime = System.currentTimeMillis() - starttime;
			if (FlagVerbose == Verbose_mode.VERBOSE) {
				System.out.println("Elapsed Time: " + elapsedtime + " msec");
			}

			iter = iter + 1; // End of HEM iteration

		}
	}

	/*
	 * ! \brief Save aggregate statistics for reduced DTs only.
	 * 
	 * \param W normalize weight matrix
	 * 
	 * \see computeEll.
	 */
	private void computeAggregateStats(Estats Estat, Mat W) {
		int i, j, m, n, r, c, t;
		int Kb = Estat.dti.size();
		int Kr = Estat.dtj.size();
		int len = Estat.tau;
		int dx = Estat.dtj.get(0).C.cols;
		int dy = Estat.dtj.get(0).C.rows;

		// 2. Loop THROUGH EACH DTI
		resize(Estat.xij, Kr);
		resize(Estat.etaj, Kr);
		;
		resize(Estat.gammaj, Kr);
		resize(Estat.Phij, Kr);
		resize(Estat.varphij, Kr);
		resize(Estat.phij, Kr);
		resize(Estat.betaj, Kr);
		resize(Estat.Psij, Kr);
		resizeDouble(Estat.Lambdaj, Kr);
		resize(Estat.Gammaj, Kr);

		Mat Ell = new Mat(Kb, Kr, CV_64F);

		// constants
		double ell_const = dy * Math.log(2 * Math.PI);

		for (i = 0; i < Kb; i++) {
			Dytex dt1 = Estat.dti.get(i);

			Mat S1 = new Mat();
			Mat tmpM;
			switch (dt1.dtopt.Sopt) {
			case COV_DIAG:
				S1.create(dt1.S0.mtx.rows, dt1.S0.mtx.rows, CV_64F);
				for (r = 0; r < dt1.S0.mtx.rows; r++) {
					S1.double2D[r][r] = dt1.S0.mtx.double2D[r][0];
				}
				break;

			default:
				MipavUtil.displayError("Cov type Not supported");
				System.exit(-1);
			}

			boolean S1zero = true;
			for (m = 0; m < S1.rows; m++)
				for (n = 0; n < S1.cols; n++) {
					if (S1.double2D[m][n] != 0.0)
						S1zero = false;
				}

			// special n=0 case
			if (dx == 0) {
				MipavUtil.displayError("n=0 case not supported");
				System.exit(-1);
			}

			Mat A1 = dt1.A;
			Mat Q1 = dt1.Q.mtx;
			Mat C1 = dt1.C;
			Mat mu01 = dt1.mu0;
			double r1 = dt1.R.mtx.double2D[0][0];
			Mat Ymean1 = dt1.Ymean;

			// some constants
			Mat Ydiff = new Mat(dy, Kr, CV_64F);
			Mat C1C1 = times(transpose(C1), C1);
			Mat C1R1C1 = divide(C1C1, r1);
			Mat C1R2C1[] = new Mat[Kr];
			Mat C2R2C1[] = new Mat[Kr];
			Mat C1R2C2[] = new Mat[Kr];
			Mat C2R2R1R2C2[] = new Mat[Kr];
			Mat C2R2C2[] = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				C1R2C1[r] = new Mat(dx, dx, CV_64F);
				C2R2C1[r] = new Mat(dx, dx, CV_64F);
				C1R2C2[r] = new Mat(dx, dx, CV_64F);
				C2R2R1R2C2[r] = new Mat(dx, dx, CV_64F);
				C2R2C2[r] = new Mat(dx, dx, CV_64F);
			}
			Mat C1R2Ydiff = new Mat(dx, Kr, CV_64F);
			Mat C2R2Ydiff = new Mat(dx, Kr, CV_64F);
			double Szero = 0.0;
			for (j = 0; j < Kr; j++) {
				if (!Estat.dtjblank.get(j)) {
					if (Estat.useYmean) {
						for (r = 0; r < Ydiff.rows; r++) {
							Ydiff.double2D[r][j] = dt1.Ymean.double2D[r][0] - Estat.dtj.get(j).Ymean.double2D[r][0];
						}
					}
					if (S1zero && (Estat.dtjsa_Szero.double2D[0][j] != 0.0)) {
						Szero = 1;
					} else {
						if (S1zero || (Estat.dtjsa_Szero.double2D[0][j] != 0.0)) {
							MipavUtil.displayError("both must have S=0");
							System.exit(-1);
						}
						Szero = 0;
					}

					Mat C2 = Estat.dtj.get(j).C;
					double r2 = Estat.dtj.get(j).R.mtx.double2D[0][0];
					C1R2C1[j] = divide(times(transpose(C1), C1), r2);
					C2R2C2[j] = divide(times(transpose(C2), C2), r2);
					C2R2C1[j] = divide(times(transpose(C2), C1), r2);
					C1R2C2[j] = transpose(C2R2C1[j]);
					C2R2R1R2C2[j] = divide(times(times(transpose(C2), C2), r1), r2 * r2);
					if (Estat.useYmean) {
						for (c = 0; c < C1.cols; c++) {
							C1R2Ydiff.double2D[c][j] = 0.0;
							for (r = 0; r < C1.rows; r++) {
								C1R2Ydiff.double2D[c][j] += C1.double2D[r][c] * Ydiff.double2D[r][j] / r2;
							}
						}
						for (c = 0; c < C2.cols; c++) {
							C2R2Ydiff.double2D[c][j] = 0.0;
							for (r = 0; r < C2.rows; r++) {
								C2R2Ydiff.double2D[c][j] += C2.double2D[r][c] * Ydiff.double2D[r][j] / r2;
							}
						}
					}
				}
			} // end cache constants 287

			// initialize KALMAN (t=1)
			Mat P_Vtt1 = clone(S1);

			// storage for Kalman smoother
			Vector<Mat[]> Q_GtC1 = new Vector<Mat[]>();
			for (m = 0; m < Kr; m++) {
				Mat tm[] = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				Q_GtC1.add(tm);
			}

			// storage for ELL
			Mat ell = new Mat(1, Kr, CV_64F);

			// initialize sensitivity analysis
			Mat bxt1 = new Mat(dx, len + 1, CV_64F);
			for (r = 0; r < bxt1.rows; r++) {
				bxt1.double2D[r][0] = mu01.double2D[r][0];
			}

			Mat bxt2 = new Mat(dx, len + 1, CV_64F);
			for (r = 0; r < bxt2.rows; r++) {
				bxt2.double2D[r][0] = mu01.double2D[r][0];
			}

			Mat bxt3[] = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				bxt3[r] = new Mat(dx, len + 1, CV_64F);
			}

			for (j = 0; j < Kr; j++) {
				if (!Estat.dtjblank.get(j)) {
					for (r = 0; r < bxt3[j].rows; r++) {
						bxt3[j].double2D[r][0] = Estat.dtj.get(j).mu0.double2D[r][0];
					}
				}
			}

			Mat bVt11[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt11[r] = new Mat(dx, dx, CV_64F);
			}

			copyTo(S1, bVt11[0]);

			Mat bVt12[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt12[r] = new Mat(dx, dx, CV_64F);
			}

			Vector<Mat[]> bVt13 = new Vector<Mat[]>();
			for (j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt13.add(tm);
			}

			Mat bVt22[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt22[r] = new Mat(dx, dx, CV_64F);
			}

			Vector<Mat[]> bVt23 = new Vector<Mat[]>();
			for (j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt23.add(tm);
			}

			Vector<Mat[]> bVt33 = new Vector<Mat[]>();
			for (j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt33.add(tm);
			}

			// iterate from t=1 to len
			Mat P_Vtt = null;
			Mat P_foo, P_Wt, P_KtC1, P_GtC1, P_GtR1Gt, P_Ft, tmp_GbFb;
			for (t = 0; t < len; t++) {
				// KALMAN filter on P at time t
				if (t > 0) {
					P_Vtt1 = plus(times(times(A1, P_Vtt), transpose(A1)), Q1);
				}
				if ((t == 0) && (Szero != 0.0)) {
					P_foo = new Mat(dx, dx, CV_64F);
				} else {
					P_Wt = new Mat(new Matrix(
							plus(new Mat((new Matrix(P_Vtt1.double2D)).inverse().getArray()), C1R1C1).double2D)
									.inverse().getArray());
					Mat eyeMat = new Mat(dx, dx, CV_64F);
					for (r = 0; r < dx; r++) {
						eyeMat.double2D[r][r] = 1.0;
					}
					P_foo = times(P_Vtt1, minus(eyeMat, times(C1R1C1, P_Wt)));
				}

				P_KtC1 = times(P_foo, C1R1C1);
				P_GtC1 = times(A1, P_KtC1);
				P_Vtt = minus(P_Vtt1, times(P_KtC1, P_Vtt1));
				P_GtR1Gt = times(times(P_GtC1, transpose(P_foo)), transpose(A1));
				P_Ft = minus(A1, P_GtC1);

				// update sensitivity analysis for P
				tmp_GbFb = new Mat();
				tmp_GbFb.create(P_GtC1.rows, P_GtC1.cols + P_Ft.cols, CV_64F);
				for (r = 0; r < P_GtC1.rows; r++) {
					for (c = 0; c < P_GtC1.cols; c++) {
						tmp_GbFb.double2D[r][c] = P_GtC1.double2D[r][c];
					}
				}
				for (r = 0; r < P_GtC1.rows; r++) {
					for (c = P_GtC1.cols; c < P_GtC1.cols + P_Ft.cols; c++) {
						tmp_GbFb.double2D[r][c] = P_Ft.double2D[r][c - P_GtC1.cols];
					}
				}

				for (r = 0; r < P_GtC1.rows; r++) {
					bxt2.double2D[r][t + 1] = 0;
					for (c = 0; c < P_GtC1.cols; c++) {
						bxt2.double2D[r][t + 1] += P_GtC1.double2D[r][c] * bxt1.double2D[c][t];
					}
					for (c = 0; c < P_Ft.cols; c++) {
						bxt2.double2D[r][t + 1] += P_Ft.double2D[r][c] * bxt2.double2D[c][t];
					}
				}
				for (r = 0; r < A1.rows; r++) {
					bxt1.double2D[r][t + 1] = 0.0;
					for (c = 0; c < A1.cols; c++) {
						bxt1.double2D[r][t + 1] += A1.double2D[r][c] * bxt1.double2D[c][t];
					}
				}

				Mat tmp1 = bVt11[t];
				Mat tmp2 = bVt12[t];
				Mat tmp3 = transpose(bVt12[t]);
				Mat tmp4 = bVt22[t];
				tmpM = new Mat();
				tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[r][c] = tmp1.double2D[r][c];
					}
				}
				for (r = 0; r < tmp1.rows; r++) {
					for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
						tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
					}
				}
				for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
					}
				}
				for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
					for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
						tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
					}
				}

				bVt22[t + 1] = plus(times(times(tmp_GbFb, tmpM), transpose(tmp_GbFb)), P_GtR1Gt);

				tmp1 = bVt11[t];
				tmp2 = bVt12[t];
				tmpM = new Mat();
				tmpM.create(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[r][c] = tmp1.double2D[r][c];
					}
				}
				for (r = 0; r < tmp1.rows; r++) {
					for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
						tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
					}
				}

				bVt12[t + 1] = times(A1, times(tmpM, transpose(tmp_GbFb)));
				bVt11[t + 1] = plus(times(times(A1, bVt11[t]), transpose(A1)), Q1);

				// compute cross-covariance
				for (j = 0; j < Kr; j++) {
					if (Estat.dtjblank.get(j)) {
						ell.double2D[0][j] = -1e300;
					} else {
						Mat tmp = times(Estat.dtj.get(j).A, Estat.dtjsa_Q_foo.get(j)[t]);
						Q_GtC1.get(j)[t] = times(tmp, C2R2C1[j]);
						Mat Q_GtR1Gt = times(times(tmp, C2R2R1R2C2[j]), transpose(tmp));
						Mat PQ_GtR1Gt = times(times(times(A1, P_foo), C1R2C2[j]), transpose(tmp));

						double ell_mahal = 0.0;
						if ((t == 0) && (Szero != 0.0)) {
							MipavUtil.displayError("not supported yet");
							System.exit(-1);
						} else {
							Mat tmp_QWtC2R2C1 = times(Estat.dtjsa_Q_Wt.get(j)[t], C2R2C1[j]);
							Mat bxt1_bxt1 = new Mat(bxt1.rows, bxt1.rows, CV_64F);
							for (r = 0; r < bxt1.rows; r++) {
								for (c = 0; c < bxt1.rows; c++) {
									bxt1_bxt1.double2D[r][c] = bxt1.double2D[r][t] * bxt1.double2D[c][t];
								}
							}
							Mat part1a = plus(bVt11[t], bxt1_bxt1);
							Mat part1b = minus(C1R2C1[j], times(C1R2C2[j], tmp_QWtC2R2C1));
							double trace1 = trace(times(part1a, part1b));
							double num2 = dy * r1 / Estat.dtj.get(j).R.mtx.double2D[0][0];
							double trace3 = trace(times(Estat.dtjsa_Q_Wt.get(j)[t], C2R2R1R2C2[j]));
							double ell_mahal1 = trace1 + num2 - trace3;
							Mat bxt2_bxt3_j = new Mat(bxt2.rows, bxt3[j].rows, CV_64F);
							for (r = 0; r < bxt2.rows; r++) {
								for (c = 0; c < bxt3[j].rows; c++) {
									bxt2_bxt3_j.double2D[r][c] = bxt2.double2D[r][t] * bxt3[j].double2D[c][t];
								}
							}
							Mat part2a = plus(bVt23.get(j)[t], bxt2_bxt3_j);
							Mat part2b = minus(C2R2C1[j], times(C2R2C2[j], tmp_QWtC2R2C1));
							double ell_mahal2 = trace(times(part2a, part2b));
							Mat bxt3_j_bxt3_j = new Mat(bxt3[j].rows, bxt3[j].rows, CV_64F);
							for (r = 0; r < bxt3[j].rows; r++) {
								for (c = 0; c < bxt3[j].rows; c++) {
									bxt3_j_bxt3_j.double2D[r][c] = bxt3[j].double2D[r][t] * bxt3[j].double2D[c][t];
								}
							}
							Mat part3a = plus(bVt33.get(j)[t], bxt3_j_bxt3_j);
							Mat part3b = minus(C2R2C2[j],
									times(times(C2R2C2[j], Estat.dtjsa_Q_Wt.get(j)[t]), C2R2C2[j]));
							double ell_mahal3 = trace(times(part3a, part3b));

							if (Estat.useYmean) {
								Mat tmp_QWtC2R2Ydiff = new Mat(Estat.dtjsa_Q_Wt.get(j)[t].rows, 1, CV_64F);
								for (r = 0; r < Estat.dtjsa_Q_Wt.get(j)[t].rows; r++) {
									tmp_QWtC2R2Ydiff.double2D[r][0] = 0.0;
									for (c = 0; c < Estat.dtjsa_Q_Wt.get(j)[t].cols; c++) {
										tmp_QWtC2R2Ydiff.double2D[r][0] += Estat.dtjsa_Q_Wt.get(j)[t].double2D[r][c]
												* C2R2Ydiff.double2D[c][j];
									}
								}
								Mat part1c = times(C1R2C2[j], tmp_QWtC2R2Ydiff);
								part1b = new Mat(C1R2Ydiff.rows, 1, CV_64F);
								for (r = 0; r < C1R2Ydiff.rows; r++) {
									part1b.double2D[r][0] = C1R2Ydiff.double2D[r][j] - part1c.double2D[r][0];
								}
								double var1 = 0.0;
								for (r = 0; r < bxt1.rows; r++) {
									var1 += 2 * bxt1.double2D[r][t] * part1b.double2D[r][0];
								}
								double var2 = 0.0;
								for (r = 0; r < Ydiff.rows; r++) {
									var2 += Ydiff.double2D[r][j] * Ydiff.double2D[r][j]
											/ Estat.dtj.get(j).R.mtx.double2D[0][0];
								}
								double var3 = 0.0;
								for (r = 0; r < C2R2Ydiff.rows; r++) {
									var3 += (C2R2Ydiff.double2D[r][j] * tmp_QWtC2R2Ydiff.double2D[r][0]);
								}
								double varTotal = var1 + var2 - var3;
								ell_mahal1 = ell_mahal1 + varTotal;

								Mat part2 = times(C2R2C2[j], tmp_QWtC2R2Ydiff);
								var1 = 0.0;
								for (r = 0; r < bxt3[j].rows; r++) {
									var1 += bxt3[j].double2D[r][t] * (C2R2Ydiff.double2D[r][j] - part2.double2D[r][0]);
								}
								ell_mahal2 = ell_mahal2 + var1;
							}
							ell_mahal = ell_mahal1 - 2 * ell_mahal2 + ell_mahal3;
						} // 376

						ell.double2D[0][j] = ell.double2D[0][j]
								- 0.5 * (ell_mahal + Estat.dtjsa_Q_logdet.double2D[t][j] + ell_const);

						// sensitivity analysis (for t+1)
						tmp1 = Q_GtC1.get(j)[t];
						tmp2 = Estat.dtjsa_Q_Ft.get(j)[t];
						Mat tmp_GrFr = new Mat(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmp_GrFr.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmp_GrFr.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}

						tmp1 = new Mat(bxt1.rows, 1, CV_64F);
						for (r = 0; r < bxt1.rows; r++) {
							tmp1.double2D[r][0] = bxt1.double2D[r][t];
						}
						tmp2 = new Mat(bxt3[j].rows, 1, CV_64F);
						for (r = 0; r < bxt3[j].rows; r++) {
							tmp2.double2D[r][0] = bxt3[j].double2D[r][t];
						}

						Mat tmpM2 = new Mat(tmp1.rows + tmp2.rows, tmp1.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM2.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp2.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM2.double2D[r][c] = tmp2.double2D[r - tmp1.rows][c];
							}
						}
						Mat prod = times(tmp_GrFr, tmpM2);
						for (r = 0; r < bxt3[j].rows; r++) {
							bxt3[j].double2D[r][t + 1] = prod.double2D[r][0];
						}

						if (Estat.useYmean) {
							double result;
							for (r = 0; r < tmp.rows; r++) {
								result = 0.0;
								for (c = 0; c < tmp.cols; c++) {
									result += tmp.double2D[r][c] * C2R2Ydiff.double2D[c][j];
								}
								bxt3[j].double2D[r][t + 1] = bxt3[j].double2D[r][t + 1] + result;
							}
						}

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmp3 = transpose(bVt13.get(j)[t]);
						tmp4 = bVt33.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
							}
						}
						bVt33.get(j)[t + 1] = plus(times(times(tmp_GrFr, tmpM), transpose(tmp_GrFr)), Q_GtR1Gt);

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmp3 = transpose(bVt12[t]);
						tmp4 = bVt23.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
							}
						}
						bVt23.get(j)[t + 1] = plus(times(times(tmp_GbFb, tmpM), transpose(tmp_GrFr)), PQ_GtR1Gt);

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}

						bVt13.get(j)[t + 1] = times(A1, times(tmpM, transpose(tmp_GrFr)));
					} // 388

				} // 389

			} // 390

			// store expected log-likelihood
			for (c = 0; c < Ell.cols; c++) {
				Ell.double2D[i][c] = ell.double2D[0][c];
			}

			// constants
			Mat ut_init = new Mat(C1.rows, Estat.tau, CV_64F);
			for (r = 0; r < C1.rows; r++) {
				for (c = 0; c < Estat.tau; c++) {
					ut_init.double2D[r][c] = 0.0;
					for (m = 0; m < C1.cols; m++) {
						ut_init.double2D[r][c] += C1.double2D[r][m] * bxt1.double2D[m][c];
					}
				}
			}
			Mat ut_inits = new Mat();
			reduce(ut_init, ut_inits, 1, CV_REDUCE_SUM);

			double Ut_init = 0;
			for (t = 0; t < len; t++) {
				Ut_init = Ut_init + trace(times(bVt11[t], C1C1)) + dy * r1;
			}

			// Sensitivity Analysis for Kalman Smoothing Filter %%%

			for (j = 0; j < Kr; j++) {
				if (!Estat.dtjblank.get(j)) {
					// initialize aggregate statistics
					Mat xij = new Mat(dx, 1, CV_64F);
					Mat etaj = clone(Estat.dtjsa_etaj_init[j]);
					Mat Phij = clone(Estat.dtjsa_Phij_init[j]);
					Mat varphij = clone(Estat.dtjsa_varphij_init[j]);
					Mat phij = clone(Estat.dtjsa_phij_init[j]);
					Mat betaj = new Mat(dx, 1, CV_64F);
					Mat Psij = clone(Estat.dtjsa_Psij_init[j]);
					Mat Gammaj = new Mat(dy, dx, CV_64F);

					Mat d_ut, udiff;
					Mat gammaj;
					if (Estat.useYmean) {
						d_ut = Ymean1;
						tmpM = new Mat(Ydiff.rows, len, CV_64F);
						for (r = 0; r < Ydiff.rows; r++) {
							for (c = 0; c < len; c++) {
								tmpM.double2D[r][c] = Ydiff.double2D[r][j];
							}
						}
						udiff = plus(ut_init, tmpM);
						gammaj = plus(ut_inits, times(d_ut, len));
					} else {
						d_ut = new Mat(1, 1, CV_64F);
						udiff = ut_init; // reference NO change
						gammaj = ut_inits;
					}

					double Lambdaj = Ut_init + trace(times(transpose(udiff), udiff));

					// initialize
					Mat bxt3_j_len = new Mat(bxt3[j].rows, 1, CV_64F);
					for (r = 0; r < bxt3[j].rows; r++) {
						bxt3_j_len.double2D[r][0] = bxt3[j].double2D[r][len];
					}
					Mat xtb = times(Estat.dtjsa_iA2[j], bxt3_j_len);
					Mat Xit = times(times(Estat.dtjsa_iA2[j], bVt33.get(j)[len]), transpose(Estat.dtjsa_iA2[j]));
					Mat Mt = new Mat(dx, dx, CV_64F);
					Mat omt = new Mat(dx, dx, CV_64F);

					Mat Xit_t1 = null;
					Mat xtb_t1 = null;
					for (t = len - 1; t >= 0; t--) {
						Mat LGCM = plus(times(Estat.dtjsa_Lt.get(j)[t], Q_GtC1.get(j)[t]), Mt);

						Mat omt_t1 = clone(omt);
						Mat Omt = plus(times(LGCM, bVt11[t]),
								times(Estat.dtjsa_LF.get(j)[t], transpose(bVt23.get(j)[t])));
						omt = plus(times(LGCM, bVt23.get(j)[t]), times(Estat.dtjsa_LF.get(j)[t], bVt33.get(j)[t]));

						Mat kappat = plus(times(C1, transpose(Omt)), transpose(times(Estat.dtjsa_LtGt.get(j)[t], r1)));

						// %%% compute statistics %%%
						Mat d_Pt = plus(Xit, times(xtb, transpose(xtb)));
						Mat d_Ptt1 = null;
						if (t < (len - 1)) {
							Mat Xitt1 = plus(times(omt_t1, transpose(Estat.dtjsa_Q_Ht.get(j)[t])),
									times(Xit_t1, transpose(Estat.dtjsa_Q_Jt.get(j)[t])));
							d_Ptt1 = plus(Xitt1, times(xtb_t1, transpose(xtb)));
						}
						Mat xtb_t = transpose(xtb);
						Mat udiff_xtb = new Mat(udiff.rows, xtb_t.cols, CV_64F);
						for (r = 0; r < udiff.rows; r++) {
							for (c = 0; c < xtb_t.cols; c++) {
								udiff_xtb.double2D[r][c] = udiff.double2D[r][t] * xtb_t.double2D[0][c];
							}
						}
						Mat Wt = plus(kappat, udiff_xtb);

						// %%% aggregate statistics
						if (t == 0) {
							xij = clone(xtb);
							etaj = plus(etaj, d_Pt);
						} else {
							varphij = plus(varphij, d_Pt);
						}
						Phij = plus(Phij, d_Pt);
						betaj = plus(betaj, xtb);
						Gammaj = plus(Gammaj, Wt);
						if (t < (Estat.tau - 1)) {
							phij = plus(phij, d_Pt);
							Psij = plus(Psij, d_Ptt1);
						}

						// %%% update sensitivity analysis
						if (t > 0) {
							xtb_t1 = clone(xtb);
							Xit_t1 = clone(Xit);

							Mat tmp1 = Estat.dtjsa_Q_Ht.get(j)[t - 1];
							Mat tmp2 = Estat.dtjsa_Q_Jt.get(j)[t - 1];
							Mat QHJ = new Mat(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
							for (r = 0; r < tmp1.rows; r++) {
								for (c = 0; c < tmp1.cols; c++) {
									QHJ.double2D[r][c] = tmp1.double2D[r][c];
								}
							}
							for (r = 0; r < tmp1.rows; r++) {
								for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
									QHJ.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
								}
							}

							Mat tmp3 = new Mat(bxt3[j].rows, 1, CV_64F);
							for (r = 0; r < bxt3[j].rows; r++) {
								tmp3.double2D[r][0] = bxt3[j].double2D[r][t];
							}
							Mat tmp4 = xtb;
							tmpM = new Mat(tmp3.rows + tmp4.rows, tmp3.cols, CV_64F);
							for (r = 0; r < tmp3.rows; r++) {
								for (c = 0; c < tmp3.cols; c++) {
									tmpM.double2D[r][c] = tmp3.double2D[r][c];
								}
							}

							for (r = tmp3.rows; r < tmp3.rows + tmp4.rows; r++) {
								for (c = 0; c < tmp3.cols; c++) {
									tmpM.double2D[r][c] = tmp4.double2D[r - tmp3.rows][c];
								}
							}
							xtb = times(QHJ, tmpM);

							tmp1 = bVt33.get(j)[t];
							Mat tmp5 = transpose(omt);
							tmp2 = tmp5;
							tmp3 = omt;
							tmp4 = Xit;
							tmpM = new Mat();
							tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
							for (r = 0; r < tmp1.rows; r++) {
								for (c = 0; c < tmp1.cols; c++) {
									tmpM.double2D[r][c] = tmp1.double2D[r][c];
								}
							}
							for (r = 0; r < tmp1.rows; r++) {
								for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
									tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
								}
							}
							for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
								for (c = 0; c < tmp1.cols; c++) {
									tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
								}
							}
							for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
								for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
									tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
								}
							}

							Xit = times(times(QHJ, tmpM), transpose(QHJ));

							Mt = times(times(Estat.dtjsa_Q_Jt.get(j)[t - 1], LGCM), A1);
						}

					} // 501

					// save and aggregate statistics
					if (Estat.xij.get(j).rows != 0) {
						Estat.xij.set(j, plus(Estat.xij.get(j), times(xij, W.double2D[i][j])));
					} else {
						Estat.xij.set(j, times(xij, W.double2D[i][j]));
					}
					if (Estat.etaj.get(j).rows != 0) {
						Estat.etaj.set(j, plus(Estat.etaj.get(j), times(etaj, W.double2D[i][j])));
					} else {
						Estat.etaj.set(j, times(etaj, W.double2D[i][j]));
					}
					if (Estat.gammaj.get(j).rows != 0) {
						Estat.gammaj.set(j,
								plus(Estat.gammaj.get(j), times(divide(gammaj, Estat.tau), W.double2D[i][j])));
					} else {
						Estat.gammaj.set(j, times(divide(gammaj, Estat.tau), W.double2D[i][j]));
					}
					if (Estat.Phij.get(j).rows != 0) {
						Estat.Phij.set(j, plus(Estat.Phij.get(j), times(divide(Phij, Estat.tau), W.double2D[i][j])));
					} else {
						Estat.Phij.set(j, times(divide(Phij, Estat.tau), W.double2D[i][j]));
					}
					if (Estat.varphij.get(j).rows != 0) {
						Estat.varphij.set(j,
								plus(Estat.varphij.get(j), times(divide(varphij, (Estat.tau - 1)), W.double2D[i][j])));
					} else {
						Estat.varphij.set(j, times(divide(varphij, (Estat.tau - 1)), W.double2D[i][j]));
					}
					if (Estat.phij.get(j).rows != 0) {
						Estat.phij.set(j,
								plus(Estat.phij.get(j), times(divide(phij, (Estat.tau - 1)), W.double2D[i][j])));
					} else {
						Estat.phij.set(j, times(divide(phij, (Estat.tau - 1)), W.double2D[i][j]));
					}
					if (Estat.betaj.get(j).rows != 0) {
						Estat.betaj.set(j, plus(Estat.betaj.get(j), times(divide(betaj, Estat.tau), W.double2D[i][j])));
					} else {
						Estat.betaj.set(j, times(divide(betaj, Estat.tau), W.double2D[i][j]));
					}
					if (Estat.Psij.get(j).rows != 0) {
						Estat.Psij.set(j,
								plus(Estat.Psij.get(j), times(divide(Psij, (Estat.tau - 1)), W.double2D[i][j])));
					} else {
						Estat.Psij.set(j, times(divide(Psij, (Estat.tau - 1)), W.double2D[i][j]));
					}
					Estat.Lambdaj.set(j, Estat.Lambdaj.get(j) + W.double2D[i][j] * (Lambdaj / Estat.tau));
					if (Estat.Gammaj.get(j).rows != 0) {
						Estat.Gammaj.set(j,
								plus(Estat.Gammaj.get(j), times(divide(Gammaj, Estat.tau), W.double2D[i][j])));
					} else {
						Estat.Gammaj.set(j, times(divide(Gammaj, Estat.tau), W.double2D[i][j]));
					}

				} // 522

			} // 523

		} // 525

	}

	/*
	 * ! \brief calculate log(sum(A)) using only log(A)
	 * 
	 * \param lA column vector of log values
	 * 
	 * \returns log(sum(A))
	 *
	 * \see runEM
	 */
	Mat logtrick(Mat lA) {
		int r, c;
		Mat s;
		Mat mv = new Mat();
		reduce(lA, mv, 0, CV_REDUCE_MAX);
		Mat tmpM = new Mat();
		repeat(mv, lA.rows, 1, tmpM);
		Mat temp = minus(lA, tmpM);

		for (r = 0; r < temp.rows; r++) {
			for (c = 0; c < temp.cols; c++) {
				tmpM.double2D[r][c] = Math.exp(temp.double2D[r][c]);
			}
		}
		Mat cterm = new Mat();
		reduce(tmpM, cterm, 0, CV_REDUCE_SUM);
		for (r = 0; r < cterm.rows; r++) {
			for (c = 0; c < cterm.cols; c++) {
				cterm.double2D[r][c] = Math.log(cterm.double2D[r][c]);
			}
		}
		s = plus(mv, cterm);

		return s;
	}

	/*
	 * ! \brief Compute Expected log-likelihood between base and reduced DTs
	 */
	private void computeEll(Estats Estat) {
		int r, c;
		int Kb = Estat.dti.size();
		int Kr = Estat.dtj.size();
		int len = Estat.tau;
		int dx = Estat.dtj.get(0).C.cols;
		int dy = Estat.dtj.get(0).C.rows;

		// 2. Loop THROUGH EACH DTI

		Estat.Ell = new Mat(Kb, Kr, CV_64F);

		// constants
		double ell_const = dy * Math.log(2 * Math.PI);

		for (int i = 0; i < Kb; i++) {
			Dytex dt1 = Estat.dti.get(i);

			Mat S1 = new Mat();
			Mat tmpM;
			switch (dt1.dtopt.Sopt) {
			case COV_DIAG:
				S1.create(dt1.S0.mtx.rows, dt1.S0.mtx.rows, CV_64F);
				for (r = 0; r < dt1.S0.mtx.rows; r++) {
					S1.double2D[r][r] = dt1.S0.mtx.double2D[r][0];
				}
				break;

			default:
				MipavUtil.displayError("Cov type Not supported");
				System.exit(-1);
			}

			boolean S1zero = true;
			for (int m = 0; m < S1.rows; m++)
				for (int n = 0; n < S1.cols; n++) {
					if (S1.double2D[m][n] != 0)
						S1zero = false;
				}

			// special n=0 case
			if (dx == 0) {
				MipavUtil.displayError("n=0 case not supported");
				System.exit(-1);
			}

			Mat A1 = dt1.A;
			Mat Q1 = dt1.Q.mtx;
			Mat C1 = dt1.C;
			Mat mu01 = dt1.mu0;
			double r1 = dt1.R.mtx.double2D[0][0];
			// Mat Ymean1 = dt1.Ymean;

			// some constants
			Mat Ydiff = new Mat(dy, Kr, CV_64F);
			Mat C1C1 = times(transpose(C1), C1);
			Mat C1R1C1 = divide(C1C1, r1);
			Mat C1R2C1[] = new Mat[Kr];
			Mat C2R2C1[] = new Mat[Kr];
			Mat C1R2C2[] = new Mat[Kr];
			Mat C2R2R1R2C2[] = new Mat[Kr];
			Mat C2R2C2[] = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				C1R2C1[r] = new Mat(dx, dx, CV_64F);
				C2R2C1[r] = new Mat(dx, dx, CV_64F);
				C1R2C2[r] = new Mat(dx, dx, CV_64F);
				C2R2R1R2C2[r] = new Mat(dx, dx, CV_64F);
				C2R2C2[r] = new Mat(dx, dx, CV_64F);
			}
			Mat C1R2Ydiff = new Mat(dx, Kr, CV_64F);
			Mat C2R2Ydiff = new Mat(dx, Kr, CV_64F);
			double Szero = 0.0;
			// cache constants
			for (int j = 0; j < Kr; j++) {
				if (!Estat.dtjblank.get(j)) {
					if (Estat.useYmean) {
						for (r = 0; r < dy; r++) {
							Ydiff.double2D[r][j] = dt1.Ymean.double2D[r][0] - Estat.dtj.get(j).Ymean.double2D[r][0];
						}
					}
					if (S1zero && (Estat.dtjsa_Szero.double2D[0][j] != 0)) {
						Szero = 1;
					} else {
						if (S1zero || (Estat.dtjsa_Szero.double2D[0][j] != 0)) {
							MipavUtil.displayError("both must have S=0");
							System.exit(-1);
						}
						Szero = 0;
					}

					Mat C2 = Estat.dtj.get(j).C;
					double r2 = Estat.dtj.get(j).R.mtx.double2D[0][0];
					C1R2C1[j] = divide(times(transpose(C1), C1), r2);
					C2R2C2[j] = divide(times(transpose(C2), C2), r2);
					C2R2C1[j] = divide(times(transpose(C2), C1), r2);
					C1R2C2[j] = transpose(C2R2C1[j]);
					C2R2R1R2C2[j] = divide(times(times(transpose(C2), C2), r1), (r2 * r2));
					if (Estat.useYmean) {

						for (c = 0; c < C1.cols; c++) {
							C1R2Ydiff.double2D[c][j] = 0.0;
							for (r = 0; r < dy; r++) {
								C1R2Ydiff.double2D[c][j] += (C1.double2D[r][c] * Ydiff.double2D[r][j]) / r2;
							}
						}
						for (c = 0; c < C2.cols; c++) {
							C2R2Ydiff.double2D[c][j] = 0.0;
							for (r = 0; r < dy; r++) {
								C2R2Ydiff.double2D[c][j] += (C2.double2D[r][c] * Ydiff.double2D[r][j]) / r2;
							}
						}
					}
				}
			} // end cache constants 287

			// initialize KALMAN (t=1)
			Mat P_Vtt1 = clone(S1);

			// storage for Kalman smoother
			Vector<Mat[]> Q_GtC1 = new Vector<Mat[]>();
			for (int m = 0; m < Kr; m++) {
				Mat tm[] = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				Q_GtC1.add(tm);
			}

			// storage for ELL
			Mat ell = new Mat(1, Kr, CV_64F);

			// initialize sensitivity analysis
			Mat bxt1 = new Mat(dx, len + 1, CV_64F);
			for (r = 0; r < dx; r++) {
				bxt1.double2D[r][0] = mu01.double2D[r][0];
			}

			Mat bxt2 = new Mat(dx, len + 1, CV_64F);
			for (r = 0; r < dx; r++) {
				bxt2.double2D[r][0] = mu01.double2D[r][0];
			}

			Mat bxt3[] = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				bxt3[r] = new Mat(dx, len + 1, CV_64F);
			}

			for (int j = 0; j < Kr; j++) {
				if (!Estat.dtjblank.get(j)) {
					for (r = 0; r < dx; r++) {
						bxt3[j].double2D[r][0] = Estat.dtj.get(j).mu0.double2D[r][0];
					}
				}
			}

			Mat bVt11[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt11[r] = new Mat(dx, dx, CV_64F);
			}

			copyTo(S1, bVt11[0]);

			Mat bVt12[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt12[r] = new Mat(dx, dx, CV_64F);
			}

			Vector<Mat[]> bVt13 = new Vector<Mat[]>();
			for (int j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt13.add(tm);
			}

			Mat bVt22[] = new Mat[len + 1];
			for (r = 0; r < len + 1; r++) {
				bVt22[r] = new Mat(dx, dx, CV_64F);
			}

			Vector<Mat[]> bVt23 = new Vector<Mat[]>();
			for (int j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt23.add(tm);
			}

			Vector<Mat[]> bVt33 = new Vector<Mat[]>();
			for (int j = 0; j < Kr; j++) {
				Mat tm[] = new Mat[len + 1];
				for (r = 0; r < len + 1; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				bVt33.add(tm);
			}

			// iterate from t=1 to len
			Mat P_Vtt = null;
			Mat P_foo, P_Wt, P_KtC1, P_GtC1, P_GtR1Gt, P_Ft;
			Mat tmp_GbFb = new Mat();
			for (int t = 0; t < len; t++) {
				// KALMAN filter on P at time t
				if (t > 0) {
					P_Vtt1 = plus(times(times(A1, P_Vtt), transpose(A1)), Q1);
				}
				if ((t == 0) && (Szero != 0)) {
					P_foo = new Mat(dx, dx, CV_64F);
				} else {
					Mat P_Vtt1_inverse = new Mat((new Matrix(P_Vtt1.double2D)).inverse().getArray());
					P_Wt = new Mat((new Matrix((plus(P_Vtt1_inverse, C1R1C1)).double2D)).inverse().getArray());
					Mat eyeMat = new Mat(dx, dx, CV_64F);
					for (r = 0; r < dx; r++) {
						eyeMat.double2D[r][r] = 1.0;
					}
					P_foo = times(P_Vtt1, minus(eyeMat, times(C1R1C1, P_Wt)));
				}

				P_KtC1 = times(P_foo, C1R1C1);
				P_GtC1 = times(A1, P_KtC1);
				P_Vtt = minus(P_Vtt1, times(P_KtC1, P_Vtt1));
				P_GtR1Gt = times(times(P_GtC1, transpose(P_foo)), transpose(A1));
				P_Ft = minus(A1, P_GtC1);

				// update sensitivity analysis for P
				tmp_GbFb.create(P_GtC1.rows, P_GtC1.cols + P_Ft.cols, CV_64F);
				for (r = 0; r < P_GtC1.rows; r++) {
					for (c = 0; c < P_GtC1.cols; c++) {
						tmp_GbFb.double2D[r][c] = P_GtC1.double2D[r][c];
					}
				}

				for (r = 0; r < P_GtC1.rows; r++) {
					for (c = 0; c < P_Ft.cols; c++) {
						tmp_GbFb.double2D[r][P_GtC1.cols + c] = P_Ft.double2D[r][c];
					}
				}

				for (r = 0; r < dx; r++) {
					bxt2.double2D[r][t + 1] = 0.0;
					bxt1.double2D[r][t + 1] = 0.0;
					for (c = 0; c < dx; c++) {
						bxt2.double2D[r][t + 1] += (P_GtC1.double2D[r][c] * bxt1.double2D[c][t]
								+ P_Ft.double2D[r][c] * bxt2.double2D[c][t]);
						bxt1.double2D[r][t + 1] += (A1.double2D[r][c] * bxt1.double2D[c][t]);
					}
				}

				Mat tmp1 = bVt11[t];
				Mat tmp2 = bVt12[t];
				Mat tmp3 = transpose(bVt12[t]);
				Mat tmp4 = bVt22[t];
				tmpM = new Mat();
				tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[r][c] = tmp1.double2D[r][c];
					}
				}
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp2.cols; c++) {
						tmpM.double2D[r][tmp1.cols + c] = tmp2.double2D[r][c];
					}
				}
				for (r = 0; r < tmp3.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[tmp1.rows + r][c] = tmp3.double2D[r][c];
					}
				}
				for (r = 0; r < tmp3.rows; r++) {
					for (c = 0; c < tmp2.cols; c++) {
						tmpM.double2D[tmp1.rows + r][tmp1.cols + c] = tmp4.double2D[r][c];
					}
				}

				bVt22[t + 1] = plus(times(times(tmp_GbFb, tmpM), transpose(tmp_GbFb)), P_GtR1Gt);

				tmp1 = bVt11[t];
				tmp2 = bVt12[t];
				tmpM = new Mat();
				tmpM.create(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp1.cols; c++) {
						tmpM.double2D[r][c] = tmp1.double2D[r][c];
					}
				}
				for (r = 0; r < tmp1.rows; r++) {
					for (c = 0; c < tmp2.cols; c++) {
						tmpM.double2D[r][tmp1.cols + c] = tmp2.double2D[r][c];
					}
				}

				bVt12[t + 1] = times(A1, times(tmpM, transpose(tmp_GbFb)));
				bVt11[t + 1] = plus(times(times(A1, bVt11[t]), transpose(A1)), Q1);

				// compute cross-covariance
				for (int j = 0; j < Kr; j++) {
					if (Estat.dtjblank.get(j)) {
						ell.double2D[0][j] = -1e300;
					} else {
						Mat tmp = times(Estat.dtj.get(j).A, Estat.dtjsa_Q_foo.get(j)[t]);
						Q_GtC1.get(j)[t] = times(tmp, C2R2C1[j]);
						Mat Q_GtR1Gt = times(times(tmp, C2R2R1R2C2[j]), transpose(tmp));
						Mat PQ_GtR1Gt = times(times(times(A1, P_foo), C1R2C2[j]), transpose(tmp));

						double ell_mahal = 0.0;
						// compute expected log-likelihood
						if ((t == 0) && (Szero != 0.0)) {
							MipavUtil.displayError("not supported yet");
							System.exit(-1);
						} else {
							Mat tmp_QWtC2R2C1 = times(Estat.dtjsa_Q_Wt.get(j)[t], C2R2C1[j]);
							Mat bxt1_colt = new Mat(bxt1.rows, 1, CV_64F);
							for (r = 0; r < bxt1.rows; r++) {
								bxt1_colt.double2D[r][0] = bxt1.double2D[r][t];
							}
							Mat firstMat = plus(bVt11[t], times(bxt1_colt, transpose(bxt1_colt)));
							Mat secondMat = minus(C1R2C1[j], times(C1R2C2[j], tmp_QWtC2R2C1));
							Mat firstProd = times(firstMat, secondMat);
							double firstTrace = trace(firstProd);
							double middleNum = dy * r1 / Estat.dtj.get(j).R.mtx.double2D[0][0];
							Mat lastMat = times(Estat.dtjsa_Q_Wt.get(j)[t], C2R2R1R2C2[j]);
							double lastTrace = trace(lastMat);
							double ell_mahal1 = firstTrace + middleNum - lastTrace;
							Mat bxt2_colt = new Mat(bxt2.rows, 1, CV_64F);
							for (r = 0; r < bxt2.rows; r++) {
								bxt2_colt.double2D[r][0] = bxt2.double2D[r][t];
							}
							Mat bxt3_j_colt = new Mat(bxt3[j].rows, 1, CV_64F);
							for (r = 0; r < bxt3[j].rows; r++) {
								bxt3_j_colt.double2D[r][0] = bxt3[j].double2D[r][t];
							}
							firstMat = plus(bVt23.get(j)[t], times(bxt2_colt, transpose(bxt3_j_colt)));
							secondMat = minus(C2R2C1[j], times(C2R2C2[j], tmp_QWtC2R2C1));
							double ell_mahal2 = trace(times(firstMat, secondMat));
							firstMat = plus(bVt33.get(j)[t], times(bxt3_j_colt, transpose(bxt3_j_colt)));
							secondMat = minus(C2R2C2[j],
									times(times(C2R2C2[j], Estat.dtjsa_Q_Wt.get(j)[t]), C2R2C2[j]));
							double ell_mahal3 = trace(times(firstMat, secondMat));
							if (Estat.useYmean) {
								Mat C2R2Ydiff_colj = new Mat(C2R2Ydiff.rows, 1, CV_64F);
								for (r = 0; r < C2R2Ydiff.rows; r++) {
									C2R2Ydiff_colj.double2D[r][0] = C2R2Ydiff.double2D[r][j];
								}
								Mat tmp_QWtC2R2Ydiff = times(Estat.dtjsa_Q_Wt.get(j)[t], C2R2Ydiff_colj);
								Mat C1R2Ydiff_colj = new Mat(C1R2Ydiff.rows, 1, CV_64F);
								for (r = 0; r < C1R2Ydiff.rows; r++) {
									C1R2Ydiff_colj.double2D[r][0] = C1R2Ydiff.double2D[r][j];
								}
								Mat Ydiff_colj = new Mat(Ydiff.rows, 1, CV_64F);
								for (r = 0; r < Ydiff.rows; r++) {
									Ydiff_colj.double2D[r][0] = Ydiff.double2D[r][j];
								}
								firstMat = times(transpose(bxt1_colt), 2.0);
								secondMat = minus(C1R2Ydiff_colj, times(C1R2C2[j], tmp_QWtC2R2Ydiff));
								Mat thirdMat = divide(times(transpose(Ydiff_colj), Ydiff_colj),
										Estat.dtj.get(j).R.mtx.double2D[0][0]);
								Mat fourthMat = times(transpose(C2R2Ydiff_colj), tmp_QWtC2R2Ydiff);
								tmpM = minus(plus(times(firstMat, secondMat), thirdMat), fourthMat);
								ell_mahal1 = ell_mahal1 + tmpM.double2D[0][0];

								tmpM = times(transpose(bxt3_j_colt),
										minus(C2R2Ydiff_colj, times(C2R2C2[j], tmp_QWtC2R2Ydiff)));
								ell_mahal2 = ell_mahal2 + tmpM.double2D[0][0];
							}
							ell_mahal = ell_mahal1 - 2 * ell_mahal2 + ell_mahal3;
						} // 376

						ell.double2D[0][j] = ell.double2D[0][j]
								- 0.5 * (ell_mahal + Estat.dtjsa_Q_logdet.double2D[t][j] + ell_const);

						// sensitivity analysis (for t+1)
						tmp1 = Q_GtC1.get(j)[t];
						tmp2 = Estat.dtjsa_Q_Ft.get(j)[t];
						Mat tmp_GrFr = new Mat(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmp_GrFr.double2D[r][c] = tmp1.double2D[r][c];
							}
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmp_GrFr.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}

						tmp1 = new Mat(bxt1.rows, 1, CV_64F);
						for (r = 0; r < bxt1.rows; r++) {
							tmp1.double2D[r][0] = bxt1.double2D[r][t];
						}
						tmp2 = new Mat(bxt3[j].rows, 1, CV_64F);
						for (r = 0; r < bxt3[j].rows; r++) {
							tmp2.double2D[r][0] = bxt3[j].double2D[r][t];
						}

						Mat tmpM2 = new Mat(tmp1.rows + tmp2.rows, tmp1.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM2.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp2.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM2.double2D[r][c] = tmp2.double2D[r - tmp1.rows][c];
							}
						}
						Mat bxt3_j_tp1 = new Mat(bxt3[j].rows, 1, CV_64F);
						bxt3_j_tp1 = times(tmp_GrFr, tmpM2);
						for (r = 0; r < bxt3[j].rows; r++) {
							bxt3[j].double2D[r][t + 1] = bxt3_j_tp1.double2D[r][0];
						}

						if (Estat.useYmean) {
							Mat C2R2Ydiff_colj = new Mat(C2R2Ydiff.rows, 1, CV_64F);
							for (r = 0; r < C2R2Ydiff.rows; r++) {
								C2R2Ydiff_colj.double2D[r][0] = C2R2Ydiff.double2D[r][j];
							}
							bxt3_j_tp1 = plus(bxt3_j_tp1, times(tmp, C2R2Ydiff_colj));
							for (r = 0; r < bxt3[j].rows; r++) {
								bxt3[j].double2D[r][t + 1] = bxt3_j_tp1.double2D[r][0];
							}
						}

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmp3 = transpose(bVt13.get(j)[t]);
						tmp4 = bVt33.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
							}
						}
						bVt33.get(j)[t + 1] = plus(times(times(tmp_GrFr, tmpM), transpose(tmp_GrFr)), Q_GtR1Gt);

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmp3 = transpose(bVt12[t]);
						tmp4 = bVt23.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows + tmp3.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
						}
						for (r = 0; r < tmp1.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp3.double2D[r - tmp1.rows][c];
							}
						}
						for (r = tmp1.rows; r < tmp1.rows + tmp3.rows; r++) {
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp4.double2D[r - tmp1.rows][c - tmp1.cols];
							}
						}
						bVt23.get(j)[t + 1] = plus(times(times(tmp_GbFb, tmpM), transpose(tmp_GrFr)), PQ_GtR1Gt);

						tmp1 = bVt11[t];
						tmp2 = bVt13.get(j)[t];
						tmpM = new Mat();
						tmpM.create(tmp1.rows, tmp1.cols + tmp2.cols, CV_64F);
						for (r = 0; r < tmp1.rows; r++) {
							for (c = 0; c < tmp1.cols; c++) {
								tmpM.double2D[r][c] = tmp1.double2D[r][c];
							}
							for (c = tmp1.cols; c < tmp1.cols + tmp2.cols; c++) {
								tmpM.double2D[r][c] = tmp2.double2D[r][c - tmp1.cols];
							}
						}

						bVt13.get(j)[t + 1] = times(A1, times(tmpM, transpose(tmp_GrFr)));
					} // 388

				} // 389

			} // 390
				// end Kalman forward

			// store expected log-likelihood
			for (c = 0; c < Estat.Ell.cols; c++) {
				Estat.Ell.double2D[i][c] = ell.double2D[0][c];
			}
		}
	}

	/*
	 * ! \brief Computes statistics between 2 DT for HEM E-step.
	 * 
	 * Write detailed description for Estats here.
	 * 
	 * \remarks Fatser version and takes less memory because only aggregate
	 * statistics are precomputed and stored
	 * 
	 * \see DytexMix::runHEM
	 */
	class Estats {

		/*
		 * ! \brief cell array of DT components of base mixture
		 */
		public Vector<Dytex> dti = new Vector<Dytex>();
		/*
		 * ! \brief cell array of DT component of reduced mixture
		 */
		public Vector<Dytex> dtj = new Vector<Dytex>();

		/*
		 * ! \brief sequence length
		 */
		public int tau;

		/*
		 * ! \brief 1 = use Ymean, 0 = don't use Ymean
		 */
		public boolean useYmean;

		/*
		 * ! \brief Expected log-likelihood between Dts
		 */
		public Mat Ell;

		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> xij = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> etaj = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> Phij = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> varphij = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> phij = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> betaj = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> Psij = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> Gammaj = new Vector<Mat>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Double> Lambdaj = new Vector<Double>();
		/*
		 * ! \brief aggregate statistic.
		 */
		public Vector<Mat> gammaj = new Vector<Mat>();

		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_Ft = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_GtC2 = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_Wt = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_foo = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_Jt = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Q_Ht = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_Lt = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_LF = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Vector<Mat[]> dtjsa_LtGt = new Vector<Mat[]>();
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_iA2[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_Szero;
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_Psij_init[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_etaj_init[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_varphij_init[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_Phij_init[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_phij_init[];
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */
		public Mat dtjsa_Q_logdet;
		/*
		 * ! \brief SENS ANALYSIS Cache variable
		 */

		/*
		 * ! \brief check if reduced DT is blank or not
		 */
		Vector<Boolean> dtjblank = new Vector<Boolean>();

		/*
		 * ! \brief initialize Estats object.
		 * 
		 * \param dti cell array of DT components of base mixture.
		 * 
		 * \param dtj cell array of DT component of reduced mixture.
		 * 
		 * \param tau sequence length.
		 * 
		 * \param useYmean 1 = use Ymean, 0 = don't use Ymean.
		 * 
		 * initialize SENS ANALYSIS Cache for base and reduced DTs
		 * 
		 */
		public Estats(Vector<Dytex> dti, Vector<Dytex> dtj, int tau, boolean useYmean) {
			this.dti = dti;
			this.dtj = dtj;
			this.tau = tau;
			this.useYmean = useYmean;
			// build SENS ANALYSIS Cache
			saveCache();
		}

		/*
		 * ! \brief RUN AND CACHE THE SENS ANALYSIS FOR DTJ
		 */
		public void saveCache() {
			int i, j, r, c;
			Mat tm[];
			// int Kb=dti.size();
			int Kr = dtj.size();
			int len = tau;
			int dx = dtj.get(0).C.cols;
			int dy = dtj.get(0).C.rows;

			// initialize cache
			for (i = 0; i < Kr; i++) {
				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_Ft.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_GtC2.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_Wt.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_foo.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_Jt.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Q_Ht.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_Lt.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dx, CV_64F);
				}
				dtjsa_LF.add(tm);

				tm = new Mat[len];
				for (r = 0; r < len; r++) {
					tm[r] = new Mat(dx, dy, CV_64F);
				}
				dtjsa_LtGt.add(tm);
			}
			dtjsa_iA2 = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_iA2[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_Szero = new Mat();
			dtjsa_Szero.create(1, Kr, CV_64F);
			dtjsa_Psij_init = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_Psij_init[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_etaj_init = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_etaj_init[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_varphij_init = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_varphij_init[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_Phij_init = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_Phij_init[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_phij_init = new Mat[Kr];
			for (r = 0; r < Kr; r++) {
				dtjsa_phij_init[r] = new Mat(dx, dx, CV_64F);
			}
			dtjsa_Q_logdet = new Mat();
			dtjsa_Q_logdet.create(len, Kr, CV_64F);

			// RUN AND CACHE THE SENS ANALYSIS FOR DTJ
			if (dtjblank.size() > Kr) {
				while (dtjblank.size() > Kr) {
					dtjblank.remove(dtjblank.size() - 1);
				}
			} else if (dtjblank.size() < Kr) {
				while (dtjblank.size() < Kr) {
					dtjblank.add(false);
				}
			}
			for (j = 0; j < Kr; j++) {
				Dytex dt2 = dtj.get(j);

				if (dt2.isempty) {
					dtjblank.set(j, true);
					continue;
				}

				Mat S2 = new Mat();
				switch (dt2.dtopt.Sopt) {
				case COV_DIAG:
					S2.create(dt2.S0.mtx.rows, dt2.S0.mtx.rows, CV_64F);
					for (i = 0; i < dt2.S0.mtx.rows; i++) {
						S2.double2D[i][i] = dt2.S0.mtx.double2D[i][0];
					}
					break;

				default:
					MipavUtil.displayError("Cov type Not supported");
					System.exit(-1);
				}

				boolean Szero = true;
				for (int m = 0; m < S2.rows; m++)
					for (int n = 0; n < S2.cols; n++) {
						if (S2.double2D[m][n] != 0)
							Szero = false;
					}

				// special n=0 case
				if (dx == 0) {
					MipavUtil.displayError("n=0 case not supported");
					System.exit(-1);
				}

				Mat A2 = dt2.A;
				Matrix A2M = new Matrix(dt2.A.double2D);
				Mat iA2 = new Mat(A2M.inverse().getArray());
				Mat Q2 = dt2.Q.mtx;
				Mat C2 = dt2.C;
				Mat C2vs = dt2.Cvs;
				double r2 = dt2.R.mtx.double2D[0][0];
				Mat C2R2C2 = times(transpose(C2), C2);
				C2R2C2.divide(r2);

				// initialize KALMAN (t=1)
				Mat Q_curVtt1 = S2;

				// Kalman cache
				Mat Q_Vtt1[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Vtt1[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_Vtt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Vtt[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_Ft[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Ft[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_GtC2[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_GtC2[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_Gt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Gt[r] = new Mat(dx, dy, CV_64F);
				}
				Mat Q_Wt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Wt[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_foo[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_foo[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_logdet = new Mat(len, 1, CV_64F);

				// KALMAN filter on Q
				Mat Q_curVtt = null;
				Mat Q_curfoo;
				Mat Q_curWt = null;
				Mat Q_KtC2;
				for (int t = 0; t < len; t++) {
					if (t > 0) {
						Q_curVtt1 = plus(times(times(A2, Q_curVtt), transpose(A2)), Q2);
					}
					if ((t == 0) && (Szero == true)) {
						Q_curfoo = new Mat(dx, dx, CV_64F);
					} else {
						Matrix Qinv = (new Matrix(Q_curVtt1.double2D)).inverse();
						Matrix Qc = Qinv.plus(new Matrix(C2R2C2.double2D));
						Q_curWt = new Mat(Qc.inverse().getArray());
						Mat eyeMat = new Mat(dx, dx, CV_64F);
						for (i = 0; i < dx; i++) {
							eyeMat.double2D[i][i] = 1.0;
						}
						Q_curfoo = times(Q_curVtt1, minus(eyeMat, times(C2R2C2, Q_curWt)));
					}
					Q_KtC2 = times(Q_curfoo, C2R2C2);
					copyTo(times(A2, Q_KtC2), Q_GtC2[t]);
					copyTo(minus(A2, Q_GtC2[t]), Q_Ft[t]);
					copyTo(divide(times(times(A2, Q_curfoo), transpose(C2)), r2), Q_Gt[t]);
					Q_curVtt = minus(Q_curVtt1, times(Q_KtC2, Q_curVtt1));

					copyTo(Q_curVtt1, Q_Vtt1[t]);
					copyTo(Q_curVtt, Q_Vtt[t]);
					copyTo(Q_curWt, Q_Wt[t]);
					copyTo(Q_curfoo, Q_foo[t]);
					Q_logdet.double2D[t][0] = logdetiid(times(times(transpose(C2vs), Q_curVtt1), C2vs), r2, dy);
				} // 142

				// KALMAN smoothing filter on Q, and sensitivity analysis
				Mat Q_Jt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Jt[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_Ht[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Ht[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Q_Vtt1tau[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Vtt1tau[r] = new Mat(dx, dx, CV_64F);
				}
				for (r = 0; r < Q_Vtt1tau[0].double2D.length; r++) {
					for (c = 0; c < Q_Vtt1tau[0].double2D[0].length; c++) {
						Q_Vtt1tau[0].double2D[r][c] = Double.MAX_VALUE;
					}
				}
				Mat Q_Vttau[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Q_Vttau[r] = new Mat(dx, dx, CV_64F);
				}
				Mat Lt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					Lt[r] = new Mat(dx, dx, CV_64F);
				}
				Mat LF[] = new Mat[len];
				for (r = 0; r < len; r++) {
					LF[r] = new Mat(dx, dx, CV_64F);
				}
				Mat LtGt[] = new Mat[len];
				for (r = 0; r < len; r++) {
					LtGt[r] = new Mat(dx, dy, CV_64F);
				}

				Mat Q_curVttau = null;
				Mat curLt = new Mat();
				Mat Q_curVtt1tau = null;
				for (int t = len - 1; t >= 0; t--) {
					if (t == (len - 1)) {
						Q_curVttau = Q_Vtt[len - 1];
						copyTo(iA2, curLt);
					} else {
						Mat A2t = transpose(A2);
						Mat Q_Vtt1_tp1_inv = new Mat((new Matrix(Q_Vtt1[t + 1].double2D)).inverse().getArray());
						copyTo(times(times(Q_Vtt[t], A2t), Q_Vtt1_tp1_inv), Q_Jt[t]);
						copyTo(minus(iA2, Q_Jt[t]), Q_Ht[t]);
						Mat Q_Jt_t_trans = transpose(Q_Jt[t]);
						Mat diff = minus(Q_curVttau, Q_Vtt1[t + 1]);
						Mat prod = times(times(Q_Jt[t], diff), Q_Jt_t_trans);
						Q_curVttau = plus(Q_Vtt[t], prod);
						curLt = plus(Q_Ht[t], times(times(Q_Jt[t], curLt), Q_Ft[t + 1]));
					}
					copyTo(Q_curVttau, Q_Vttau[t]);

					if (t < (len - 1)) {
						if (t == len - 2) {
							Mat dxdx = new Mat(dx, dx, CV_64F);
							for (r = 0; r < dx; r++) {
								dxdx.double2D[r][r] = 1.0;
							}
							Mat prod = times(times(times(iA2, Q_GtC2[len - 1]), A2), Q_Vtt[len - 2]);
							Q_curVtt1tau = minus(dxdx, prod);
						} else {
							Mat Q_Jt_t_transpose = transpose(Q_Jt[t]);
							Mat diff = minus(Q_curVtt1tau, times(A2, Q_Vtt[t + 1]));
							Mat prod = times(times(Q_Jt[t + 1], diff), Q_Jt_t_transpose);
							Mat firstProd = times(Q_Vtt[t + 1], Q_Jt_t_transpose);
							Q_curVtt1tau = plus(firstProd, prod);
						}
						copyTo(Q_curVtt1tau, Q_Vtt1tau[t + 1]);
					}

					// sensitivity analysis cache
					LF[t] = times(curLt, Q_Ft[t]);
					LtGt[t] = times(curLt, Q_Gt[t]);
					copyTo(curLt, Lt[t]);
				} // 182

				// save things
				dtjsa_Q_Ft.set(j, Q_Ft);
				dtjsa_Q_GtC2.set(j, Q_GtC2);
				dtjsa_Q_Wt.set(j, Q_Wt);
				dtjsa_Q_foo.set(j, Q_foo);
				dtjsa_Q_Jt.set(j, Q_Jt);
				dtjsa_Q_Ht.set(j, Q_Ht);
				dtjsa_Lt.set(j, Lt);
				dtjsa_LF.set(j, LF);
				dtjsa_LtGt.set(j, LtGt);
				copyTo(iA2, dtjsa_iA2[j]);
				if (Szero) {
					dtjsa_Szero.double2D[0][j] = 1.0;
				} else {
					dtjsa_Szero.double2D[0][j] = 0.0;
				}

				Mat tmpMS[] = subvid(Q_Vtt1tau, 1, len);
				Mat tmpM2 = new Mat();
				reduce(tmpMS, tmpM2, CV_REDUCE_SUM);
				// = sum(Q_Vtt1tau(:,:,2:len),3);
				copyTo(tmpM2, dtjsa_Psij_init[j]);

				copyTo(Q_Vttau[0], dtjsa_etaj_init[j]);

				tmpMS = subvid(Q_Vttau, 1, len);
				Mat tmpN1 = new Mat();
				reduce(tmpMS, tmpN1, CV_REDUCE_SUM);
				// = sum(Q_Vtt1tau(:,:,2:len),3);
				copyTo(tmpN1, dtjsa_varphij_init[j]);

				Mat tmpN2 = new Mat();
				reduce(Q_Vttau, tmpN2, CV_REDUCE_SUM);
				// = sum(Q_Vtt1tau(:,:,2:len),3);
				copyTo(tmpN2, dtjsa_Phij_init[j]);

				tmpMS = subvid(Q_Vttau, 0, len - 1);
				Mat tmpN3 = new Mat();
				reduce(tmpMS, tmpN3, CV_REDUCE_SUM);
				// = sum(Q_Vtt1tau(:,:,2:len),3);
				copyTo(tmpN3, dtjsa_phij_init[j]);

				for (r = 0; r < len; r++) {
					dtjsa_Q_logdet.double2D[r][j] = Q_logdet.double2D[r][0];
				}

			} // 202
		}
	}

	Mat[] subvid(Mat vid[], int zstart, int zend, int ystart, int yend, int xstart, int xend) {
		int i, d, r, c, ch;
		int dstart;
		int rstart;
		int cstart;
		Mat sub[];
		int depth;
		int rows;
		int cols;

		depth = zend - zstart;
		dstart = zstart;
		sub = new Mat[depth];
		rows = yend - ystart;
		rstart = ystart;
		cols = xend - xstart;
		cstart = xstart;
		int channels = vid[0].channels;
		int type = vid[0].type;
		if (type == CV_64F) {
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(rows, cols, CV_64F);
			}
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						sub[d].double2D[r][c] = vid[d + dstart].double2D[r + rstart][c + cstart];
					}
				}
			}
		} // if (type == CV_64F)
		else if (type == CV_8U) {
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(rows, cols, CV_8U);
			}
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						sub[d].byte2D[r][c] = vid[d + dstart].byte2D[r + rstart][c + cstart];
					}
				}
			}
		} // else if (type == CV_8U)
		else if (type == CV_64FC) {
			int sz[] = new int[] { rows, cols };
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(2, sz, CV_64FC, channels);
			}
			for (d = 0; d < depth; d++) {
				for (ch = 0; ch < channels; ch++) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							sub[d].double2DC[r][c][ch] = vid[d + dstart].double2DC[r + rstart][c + cstart][ch];
						}
					}
				}
			}
		} // else if (type == CV_64FC)
		else if (type == CV_8UC) {
			int sz[] = new int[] { rows, cols };
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(2, sz, CV_8UC, channels);
			}
			for (d = 0; d < depth; d++) {
				for (ch = 0; ch < channels; ch++) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							sub[d].byte2DC[r][c][ch] = vid[d + dstart].byte2DC[r + rstart][c + cstart][ch];
						}
					}
				}
			}
		} // else if (type == CV_8UC)
		return sub;
	}

	Mat[] subvid(Mat vid[], Range box_z, Range box_y, Range box_x) {
		int i, d, r, c, ch;
		int dstart;
		int rstart;
		int cstart;
		Mat sub[];
		int depth;
		int rows;
		int cols;
		if (box_z.all) {
			depth = vid.length;
			dstart = 0;
		} else {
			depth = box_z.end - box_z.start;
			dstart = box_z.start;
		}
		sub = new Mat[depth];
		if (box_y.all) {
			rows = vid[0].rows;
			rstart = 0;
		} else {
			rows = box_y.end - box_y.start;
			rstart = box_y.start;
		}
		if (box_x.all) {
			cols = vid[0].cols;
			cstart = 0;
		} else {
			cols = box_x.end - box_x.start;
			cstart = box_x.start;
		}
		int channels = vid[0].channels;
		int type = vid[0].type;
		if (type == CV_64F) {
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(rows, cols, CV_64F);
			}
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						sub[d].double2D[r][c] = vid[d + dstart].double2D[r + rstart][c + cstart];
					}
				}
			}
		} // if (type == CV_64F)
		else if (type == CV_8U) {
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(rows, cols, CV_8U);
			}
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						sub[d].byte2D[r][c] = vid[d + dstart].byte2D[r + rstart][c + cstart];
					}
				}
			}
		} // else if (type == CV_8U)
		else if (type == CV_64FC) {
			int sz[] = new int[] { rows, cols };
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(2, sz, CV_64FC, channels);
			}
			for (d = 0; d < depth; d++) {
				for (ch = 0; ch < channels; ch++) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							sub[d].double2DC[r][c][ch] = vid[d + dstart].double2DC[r + rstart][c + cstart][ch];
						}
					}
				}
			}
		} // else if (type == CV_64FC)
		else if (type == CV_8UC) {
			int sz[] = new int[] { rows, cols };
			for (i = 0; i < depth; i++) {
				sub[i] = new Mat(2, sz, CV_8UC, channels);
			}
			for (d = 0; d < depth; d++) {
				for (ch = 0; ch < channels; ch++) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							sub[d].byte2DC[r][c][ch] = vid[d + dstart].byte2DC[r + rstart][c + cstart][ch];
						}
					}
				}
			}
		} // else if (type == CV_8UC)
		return sub;
	}

	Mat[] subvid(Mat vid[], int frange_start_inclusive, int frange_end_exclusive) {
		int i, r, c, ch;
		if ((frange_start_inclusive < 0) || frange_start_inclusive >= vid.length) {
			MipavUtil.displayError("frange_start_incluive is an impossible " + frange_start_inclusive);
			System.exit(-1);
		}
		if ((frange_end_exclusive <= 0) || (frange_end_exclusive > vid.length)) {
			MipavUtil.displayError("frange_end_exclusive is an impossible " + frange_end_exclusive);
			System.exit(-1);
		}
		Mat sub[] = new Mat[frange_end_exclusive - frange_start_inclusive];
		if (vid[0].type == CV_64F) {
			for (i = 0; i < sub.length; i++) {
				sub[i] = new Mat(vid[0].rows, vid[0].cols, CV_64F);
				for (r = 0; r < vid[0].rows; r++) {
					for (c = 0; c < vid[0].cols; c++) {
						sub[i].double2D[r][c] = vid[i + frange_start_inclusive].double2D[r][c];
					}
				}
			}
		} else if (vid[0].type == CV_8U) {
			for (i = 0; i < sub.length; i++) {
				sub[i] = new Mat(vid[0].rows, vid[0].cols, CV_8U);
				for (r = 0; r < vid[0].rows; r++) {
					for (c = 0; c < vid[0].cols; c++) {
						sub[i].byte2D[r][c] = vid[i + frange_start_inclusive].byte2D[r][c];
					}
				}
			}
		} else if (vid[0].type == CV_64FC) {
			int sz[] = new int[] { vid[0].rows, vid[0].cols };
			for (i = 0; i < sub.length; i++) {
				sub[i] = new Mat(2, sz, CV_64FC, vid[0].channels);
				for (ch = 0; ch < vid[0].channels; ch++) {
					for (r = 0; r < vid[0].rows; r++) {
						for (c = 0; c < vid[0].cols; c++) {
							sub[i].double2DC[r][c][ch] = vid[i + frange_start_inclusive].double2DC[r][c][ch];
						}
					}
				}
			}
		} else if (vid[0].type == CV_8UC) {
			int sz[] = new int[] { vid[0].rows, vid[0].cols };
			for (i = 0; i < sub.length; i++) {
				sub[i] = new Mat(2, sz, CV_8UC, vid[0].channels);
				for (ch = 0; ch < vid[0].channels; ch++) {
					for (r = 0; r < vid[0].rows; r++) {
						for (c = 0; c < vid[0].cols; c++) {
							sub[i].byte2DC[r][c][ch] = vid[i + frange_start_inclusive].byte2DC[r][c][ch];
						}
					}
				}
			}
		}
		return sub;
	}

	private void reduce(Mat src, Mat dst, int dim, int rtype) {
		int r, c;
		// dim = 0 means the matrix is reduced to a single row
		// dim = 1 means the matrix is reduced to a single column
		if (dim == 0) {
			dst.create(1, src.cols, src.type);
			if ((rtype == CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG)) {
				for (c = 0; c < src.cols; c++) {
					dst.double2D[0][c] = 0.0;
					for (r = 0; r < src.rows; r++) {
						dst.double2D[0][c] += src.double2D[r][c];
					}
					if (rtype == CV_REDUCE_AVG) {
						dst.double2D[0][c] /= src.rows;
					}
				}
			} // if ((rtype == CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG))
			else if (rtype == CV_REDUCE_MAX) {
				for (c = 0; c < src.cols; c++) {
					dst.double2D[0][c] = -Double.MAX_VALUE;
					for (r = 0; r < src.rows; r++) {
						if (src.double2D[r][c] > dst.double2D[0][c]) {
							dst.double2D[0][c] = src.double2D[r][c];
						}
					}
				}
			} // else if (rtype == CV_REDUCE_MAX)
			else if (rtype == CV_REDUCE_MIN) {
				for (c = 0; c < src.cols; c++) {
					dst.double2D[0][c] = Double.MAX_VALUE;
					for (r = 0; r < src.rows; r++) {
						if (src.double2D[r][c] < dst.double2D[0][c]) {
							dst.double2D[0][c] = src.double2D[r][c];
						}
					}
				}
			} // else if (type == CV_REDUCE_MIN)
			else {
				MipavUtil.displayError("rytpe is an illegal " + rtype + " in reduce");
				System.exit(-1);
			}
		} // if (dim == 0)
		else if (dim == 1) {
			dst.create(src.rows, 1, src.type);
			if ((rtype == CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG)) {
				for (r = 0; r < src.rows; r++) {
					dst.double2D[r][0] = 0.0;
					for (c = 0; c < src.cols; c++) {
						dst.double2D[r][0] += src.double2D[r][c];
					}
					if (rtype == CV_REDUCE_AVG) {
						dst.double2D[r][0] /= src.cols;
					}
				}
			} // if ((rtype == CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG))
			else if (rtype == CV_REDUCE_MAX) {
				for (r = 0; r < src.rows; r++) {
					dst.double2D[r][0] = -Double.MAX_VALUE;
					for (c = 0; c < src.cols; c++) {
						if (src.double2D[r][c] > dst.double2D[r][0]) {
							dst.double2D[r][0] = src.double2D[r][c];
						}
					}
				}
			} // else if (rtype == CV_REDUCE_MAX)
			else if (rtype == CV_REDUCE_MIN) {
				for (r = 0; r < src.rows; r++) {
					dst.double2D[r][0] = Double.MAX_VALUE;
					for (c = 0; c < src.cols; c++) {
						if (src.double2D[r][c] < dst.double2D[r][0]) {
							dst.double2D[r][0] = src.double2D[r][c];
						}
					}
				}
			} // else if (rtype == CV_REDUCE_MIN)
			else {
				MipavUtil.displayError("rytpe is an illegal " + rtype + " in reduce");
				System.exit(-1);
			}
		} // else if (dim == 1)
		else {
			MipavUtil.displayError("dim = an illegal " + dim + " in reduce");
			System.exit(-1);
		}
	}

	/*
	 * private void reduce(Mat src[], Mat dst, int dim, int rtype) { int d,r, c;
	 * // dim = 0 means the matrix is reduced to a single row // dim = 1 means
	 * the matrix is reduced to a single column if (dim == 0) { dst.create(1,
	 * src[0].cols, src[0].type); if ((rtype == CV_REDUCE_SUM) || (rtype ==
	 * CV_REDUCE_AVG)) { for (c = 0; c < src[0].cols; c++) { dst.double2D[0][c]
	 * = 0.0; for (d = 0; d < src.length; d++) { for (r = 0; r < src[0].rows;
	 * r++) { dst.double2D[0][c] += src[d].double2D[r][c]; } } if (rtype ==
	 * CV_REDUCE_AVG) { dst.double2D[0][c] /= (src.length*src[0].rows); } } } //
	 * if ((rtype == CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG)) else if (rtype
	 * == CV_REDUCE_MAX) { for (c = 0; c < src[0].cols; c++) {
	 * dst.double2D[0][c] = -Double.MAX_VALUE; for (d = 0; d < src.length; d++)
	 * { for (r = 0; r < src[0].rows; r++) { if (src[d].double2D[r][c] >
	 * dst.double2D[0][c]) { dst.double2D[0][c] = src[d].double2D[r][c]; } } } }
	 * } // else if (rtype == CV_REDUCE_MAX) else if (rtype == CV_REDUCE_MIN) {
	 * for (c = 0; c < src[0].cols; c++) { dst.double2D[0][c] =
	 * Double.MAX_VALUE; for (d = 0; d < src.length; d++) { for (r = 0; r <
	 * src[0].rows; r++) { if (src[d].double2D[r][c] < dst.double2D[0][c]) {
	 * dst.double2D[0][c] = src[d].double2D[r][c]; } } } } } // else if (type ==
	 * CV_REDUCE_MIN) else { MipavUtil.displayError("rytpe is an illegal " +
	 * rtype + " in reduce"); System.exit(-1); } } // if (dim == 0) else if (dim
	 * == 1) { dst.create(src[0].rows, 1, src[0].type); if ((rtype ==
	 * CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG)) { for (r = 0; r <
	 * src[0].rows; r++) { dst.double2D[r][0] = 0.0; for (d = 0; d < src.length;
	 * d++) { for (c = 0; c < src[0].cols; c++) { dst.double2D[r][0] +=
	 * src[d].double2D[r][c]; } } if (rtype == CV_REDUCE_AVG) {
	 * dst.double2D[r][0] /= (src.length*src[0].cols); } } } // if ((rtype ==
	 * CV_REDUCE_SUM) || (rtype == CV_REDUCE_AVG)) else if (rtype ==
	 * CV_REDUCE_MAX) { for (r = 0; r < src[0].rows; r++) { dst.double2D[r][0] =
	 * -Double.MAX_VALUE; for (d = 0; d < src.length; d++) { for (c = 0; c <
	 * src[0].cols; c++) { if (src[d].double2D[r][c] > dst.double2D[r][0]) {
	 * dst.double2D[r][0] = src[d].double2D[r][c]; } } } } } // else if (rtype
	 * == CV_REDUCE_MAX) else if (rtype == CV_REDUCE_MIN) { for (r = 0; r <
	 * src[0].rows; r++) { dst.double2D[r][0] = Double.MAX_VALUE; for (d = 0; d
	 * < src.length; d++) { for (c = 0; c < src[0].cols; c++) { if
	 * (src[d].double2D[r][c] < dst.double2D[r][0]) { dst.double2D[r][0] =
	 * src[d].double2D[r][c]; } } } } } // else if (rtype == CV_REDUCE_MIN) else
	 * { MipavUtil.displayError("rytpe is an illegal " + rtype + " in reduce");
	 * System.exit(-1); } } // else if (dim == 1) else {
	 * MipavUtil.displayError("dim = an illegal " + dim + " in reduce");
	 * System.exit(-1); } }
	 */

	// reduce video to a single image (similar to OpenCV reduce)
	/*
	 * private void reduce(Mat vid, Mat out, int reduceOp) { int r,c; if
	 * ((reduceOp == CV_REDUCE_SUM) || (reduceOp == CV_REDUCE_AVG)) {
	 * out.create(vid.size[1], vid.size[2], CV_64F); } else {
	 * out.create(vid.size[1], vid.size[2], vid.type); }
	 * 
	 * //Mat vtmp;
	 * 
	 * for (int z=0; z<vid.size[0]; z++) { Mat vbz = frame(vid, z); switch
	 * (reduceOp) { case CV_REDUCE_SUM: case CV_REDUCE_AVG: if (vbz.type ==
	 * out.type) { for (r = 0; r < vid.size[1]; r++) { for (c = 0; c <
	 * vid.size[2]; c++) { out.double2D[r][c] += vbz.double2D[r][c]; } } } else
	 * { //vbz.convertTo(vtmp, out.type); //out += vtmp; } break; default:
	 * MipavUtil.displayError("bad option, or unimplemented!"); System.exit(-1);
	 * } }
	 * 
	 * if (reduceOp == CV_REDUCE_AVG) { for (r = 0; r < vid.size[1]; r++) { for
	 * (c = 0; c < vid.size[2]; c++) { out.double2D[r][c] /= vid.size[0]; } } }
	 * 
	 * }
	 */

	// reduce video to a single image (similar to OpenCV reduce)
	private void reduce(Mat vid[], Mat out, int reduceOp) {
		int r, c;
		if ((reduceOp == CV_REDUCE_SUM) || (reduceOp == CV_REDUCE_AVG)) {
			out.create(vid[0].rows, vid[0].cols, CV_64F);
		} else {
			out.create(vid[0].rows, vid[0].cols, vid[0].type);
		}

		// Mat vtmp;

		for (int z = 0; z < vid.length; z++) {
			switch (reduceOp) {
			case CV_REDUCE_SUM:
			case CV_REDUCE_AVG:
				if (vid[0].type == out.type) {
					for (r = 0; r < vid[0].rows; r++) {
						for (c = 0; c < vid[0].cols; c++) {
							out.double2D[r][c] += vid[z].double2D[r][c];
						}
					}
				} else {
					// vbz.convertTo(vtmp, out.type);
					// out += vtmp;
				}
				break;
			default:
				MipavUtil.displayError("bad option, or unimplemented!");
				System.exit(-1);
			}
		}

		if (reduceOp == CV_REDUCE_AVG) {
			for (r = 0; r < vid[0].rows; r++) {
				for (c = 0; c < vid[0].cols; c++) {
					out.double2D[r][c] /= vid.length;
				}
			}
		}

	}

	/*
	 * ! \brief compute logdet of transformed covariance w/ diagonal or iid
	 * noise.
	 * 
	 * \param Q parameter Q.
	 * 
	 * \param r parameter r.
	 * 
	 * \param C parameter C.
	 * 
	 * \returns logdet value.
	 * 
	 * \see saveCache.
	 */
	double logdetiid(Mat Q, double r, int C) {
		int i;
		double ld;
		double[] eigenvalue = new double[Q.cols];
		double[][] eigenvector = new double[Q.rows][Q.cols];
		Eigenvalue.decompose(Q.double2D, eigenvector, eigenvalue);
		double ts = 0.0;
		for (i = 0; i < eigenvalue.length; i++) {
			ts += Math.log(eigenvalue[i] / r + 1.0);
		}
		ld = ts + C * Math.log(r);
		return ld;
	}

	private Mat frame(Mat vid, int f) {

		// cout<<"Test "<<f<<" "<<vid.size[0]<<endl;
		int r, c;
		if (vid.dims != 3) {
			MipavUtil.displayError("vid.dims = " + vid.dims + " instead of the required 3 in Mat frame");
			System.exit(-1);
		}
		if ((f < 0) || (f >= vid.size[0])) {
			MipavUtil.displayError("f is an impossible " + f + " in Mat frame");
			System.exit(-1);
		}
		Mat myf = new Mat(vid.size[1], vid.size[2], vid.type);
		for (r = 0; r < vid.size[1]; r++) {
			for (c = 0; c < vid.size[2]; c++) {
				myf.double2D[r][c] = vid.double3D[f][r][c];
			}
		}
		myf.bytesPerRow = vid.step[1];

		return myf;

		// return MatVid::subvid(vid, f, f, 0, vid.size[1]-1, 0, vid.size[2]-1);

		// Mat myf = MatVid::subvid(vid, Range(f, f+1), Range::all(),
		// Range::all() );

		// dumpMatSize(myf);
	}

	/*
	 * private Mat create(int frames, int rows, int cols, int type) { int sz[] =
	 * {frames, rows, cols}; return new Mat(3, sz, type); }
	 */

	private Mat[] create(int frames, int rows, int cols, int type) {
		int r;
		Mat tm[] = new Mat[frames];
		for (r = 0; r < frames; r++) {
			tm[r] = new Mat(rows, cols, type);
		}
		return tm;
	}

	private void setRegularizer(Dytex dy, DytexRegOptions dtreg) {
		setRegularizer(dy.R, dtreg.Ropt, dtreg.Rval);
		setRegularizer(dy.Q, dtreg.Qopt, dtreg.Qval);
		setRegularizer(dy.S0, dtreg.Sopt, dtreg.Sval);
	}

	// set regularization mode
	private void setRegularizer(CovMatrix cov, cov_reg_type regopt, double regval) {
		switch (regopt) {
		case COV_REG_NONE:
		case COV_REG_MIN:
		case COV_REG_ADD:
			break;
		default:
			MipavUtil.displayError("ERROR: invalid cov_reg_type!");
			System.exit(-1);
		}

		if (regval < 0) {
			MipavUtil.displayError("ERROR: invalid regval");
			System.exit(-1);
		}

		cov.regopt = regopt;
		cov.regval = regval;
	}

	private void regularize(Dytex dy, boolean regA) {
		int i;
		regularize(dy.R);
		regularize(dy.Q);
		regularize(dy.S0);

		if (regA) // For HEM
		{
			// Regularization of A
			double target = 0.999;
			double[] eigenvalueR = new double[dy.A.cols];
			double[] eigenvalueI = new double[dy.A.cols];
			double[] eigenabs = new double[dy.A.cols];
			double[][] eigenvector = new double[dy.A.rows][dy.A.cols];
			Eigenvalue.decompose(dy.A.double2D, eigenvector, eigenvalueR, eigenvalueI);
			for (i = 0; i < eigenvalueR.length; i++) {
				eigenabs[i] = zabs(eigenvalueR[i], eigenvalueI[i]);
			}
			double maxVal = -Double.MAX_VALUE;
			for (i = 0; i < eigenabs.length; i++) {
				if (eigenabs[i] > maxVal) {
					maxVal = eigenabs[i];
				}
			}
			if (maxVal > target) {
				dy.A.multiply(target / maxVal);
			}
		}
	}

	/**
	 * zabs computes the absolute value or magnitude of a double precision
	 * complex variable zr + j*zi.
	 * 
	 * @param zr
	 *            double
	 * @param zi
	 *            double
	 * 
	 * @return double
	 */
	private double zabs(final double zr, final double zi) {
		double u, v, q, s;
		u = Math.abs(zr);
		v = Math.abs(zi);
		s = u + v;

		// s * 1.0 makes an unnormalized underflow on CDC machines into a true
		// floating zero
		s = s * 1.0;

		if (s == 0.0) {
			return 0.0;
		} else if (u > v) {
			q = v / u;

			return (u * Math.sqrt(1.0 + (q * q)));
		} else {
			q = u / v;

			return (v * Math.sqrt(1.0 + (q * q)));
		}
	}

	// regularize
	private void regularize(CovMatrix cov) {
		switch (cov.regopt) {
		case COV_REG_NONE:
			// do nothing
			break;

		case COV_REG_MIN:
			switch (cov.covopt) {
			case COV_FULL:
			// min bound on eigenvalues
			{
				// A = V * (diagonal values) * V' giving a rows * rows product
				// Column of V represent the eigenvectors
				double[] eigenvalue = new double[cov.mtx.cols];
				double[][] eigenvector = new double[cov.mtx.rows][cov.mtx.cols];
				Eigenvalue.decompose(cov.mtx.double2D, eigenvector, eigenvalue);
				// In openCV eigV rows are the eigenvectors
				// In openCV mtx = eigv" * diag(S) *eigV
				// In OpenvCV a cols * cols product
				// Size sz = mtx.size();
				// Mat eigS(sz.height, 1, OPT_MAT_TYPE),
				// eigV(sz.height, sz.width, OPT_MAT_TYPE);
				// eigen(mtx, eigS, eigV);
				// min bound on eigenvalues
				boolean doRecon = inp_minbnd(eigenvalue, cov.regval);
				// reconstruct matrix (if something changed)
				if (doRecon) {
					Mat matV = new Mat(eigenvector);
					double diag[][] = new double[eigenvalue.length][eigenvalue.length];
					for (int i = 0; i < eigenvalue.length; i++) {
						diag[i][i] = eigenvalue[i];
					}
					Mat matDiag = new Mat(diag);
					Mat matVT = transpose(matV);
					cov.mtx = times(times(matV, matDiag), matVT);
				}

				// cout << "eigS = \n" << eigS << "\n";
				// cout << "eigV = \n" << eigV << "\n";

				// reconstruct matrix (if something changed)
				// if (doRecon) {
				// note: eigV rows are the eigenvalues
				// Mat S = repeat(eigS, 1, sz.width); // S = [eigS, eigS ...]
				// multiply(S, eigV, S); // S = S .* eigV
				// mtx = eigV.t() * S; // mtx = eigV'*diag(S)*eigV
				// }
			}
				break;
			case COV_DIAG:
			case COV_IID:
				// min of elements
				inp_minbnd(cov.mtx.double2D, cov.regval);
				break;
			case COV_ILLEGAL:
				MipavUtil.displayError("cov.covopt is COV_ILLEGAL in regularize");
				System.exit(-1);
				break;
			}
			break;

		case COV_REG_ADD:
			// regularize by adding to diagonal
			switch (cov.covopt) {
			case COV_FULL: {
				// add to just diagonal
				for (int i = 0; i < cov.mtx.double2D.length; i++) {
					cov.mtx.double2D[i][i] += cov.regval;
				}
			}
				break;

			case COV_DIAG:
			case COV_IID:
				// add to diagonal (all elements)
				for (int i = 0; i < cov.mtx.double2D.length; i++) {
					for (int j = 0; j < cov.mtx.double2D[0].length; j++) {
						cov.mtx.double2D[i][j] += cov.regval;
					}
				}
				break;
			case COV_ILLEGAL:
				MipavUtil.displayError("cov.covopt is COV_ILLEGAL in regularize");
				System.exit(-1);
				break;
			}
			break;

		default:
			MipavUtil.displayError("ERROR: invalid cov_reg_type!");
			System.exit(-1);
		}
	}

	// bound the minimum entry of a matrix, in-place
	// returns true if an entry changed
	private boolean inp_minbnd(double arr[], double min_bnd) {
		int i;
		boolean retval = false;
		for (i = 0; i < arr.length; i++) {
			if (arr[i] < min_bnd) {
				arr[i] = min_bnd;
				retval = true;
			}
		}
		return retval;
	}

	// bound the minimum entry of a matrix, in-place
	// returns true if an entry changed
	private boolean inp_minbnd(double arr[][], double min_bnd) {
		int i, j;
		boolean retval = false;
		for (i = 0; i < arr.length; i++) {
			for (j = 0; j < arr[0].length; j++) {
				if (arr[i][j] < min_bnd) {
					arr[i][j] = min_bnd;
					retval = true;
				}
			}
		}
		return retval;
	}

	/*
	 * ! \brief initialize a single DT from current mixture
	 * 
	 * \returns new DT
	 * 
	 * \see initcluster_doretto
	 */
	private Dytex init_multiple_dt(DytexMix dtm) {
		int i, j;
		// copy template
		Dytex odt = new Dytex(dtm.dt.get(0).dtopt);

		if (odt.dtopt.Yopt == Ymean_type.ZERO_YMEAN) {
			odt.Ymean = new Mat(odt.dtopt.m, 1, CV_64F);
		}
		copyTo(dtm.dt.get(0).Cvs, odt.Cvs);
		odt.isCvs = false;
		// extract Cs

		Mat cMat = new Mat(odt.dtopt.m, odt.dtopt.n * dtm.dt.size(), CV_64F);
		for (i = 0; i < dtm.dt.size(); i++) {
			int cstart = i * odt.dtopt.n;
			copyToDstColRange(dtm.dt.get(i).C, cMat, cstart, cstart + odt.dtopt.n);
		}
		if (cMat.rows > cMat.cols) {
			Matrix cMatrix = new Matrix(cMat.double2D);
			SingularValueDecomposition svd = new SingularValueDecomposition(cMatrix);
			Mat matU = new Mat(svd.getU().getArray());
			copyFromSrcColRange(matU, odt.C, 0, odt.dtopt.n);
		} else {
			Matrix cMatrix = new Matrix(cMat.double2D);
			Matrix ccMatrix = cMatrix.times(cMatrix.transpose());
			SingularValueDecomposition svd = new SingularValueDecomposition(ccMatrix);
			Mat matU = new Mat(svd.getU().getArray());
			copyFromSrcColRange(matU, odt.C, 0, odt.dtopt.n);
		}
		// initialize accumulators
		odt.mu0.init(0);
		odt.S0.mtx.init(0);
		odt.Ymean.init(0);
		odt.A.init(0);
		odt.Q.mtx.init(0);
		odt.R.mtx.init(0);

		// compute other parameters by averaging
		for (i = 0; i < dtm.dt.size(); i++) {
			// compute transformation
			Mat F = times(transpose(odt.C), dtm.dt.get(i).C);

			// accumulate
			odt.mu0 = plus(odt.mu0, times(F, dtm.dt.get(i).mu0));

			Mat tmpM = new Mat(dtm.dt.get(i).S0.mtx.rows, dtm.dt.get(i).S0.mtx.rows, CV_64F);
			for (j = 0; j < dtm.dt.get(i).S0.mtx.rows; j++) {
				tmpM.double2D[j][j] = dtm.dt.get(i).S0.mtx.double2D[j][0];
			}

			Mat FTF = times(times(F, tmpM), transpose(F));
			for (j = 0; j < odt.S0.mtx.rows; j++) {
				odt.S0.mtx.double2D[j][0] = odt.S0.mtx.double2D[j][0] + FTF.double2D[j][j];
			}
			odt.Ymean = plus(odt.Ymean, dtm.dt.get(i).Ymean);
			Mat Finv = new Mat((new Matrix(F.double2D)).inverse().getArray());
			odt.A = plus(odt.A, times(times(F, dtm.dt.get(i).A), Finv));
			odt.Q.mtx = plus(odt.Q.mtx, times(times(F, dtm.dt.get(i).Q.mtx), transpose(F)));
			odt.R.mtx = plus(odt.R.mtx, dtm.dt.get(i).R.mtx);
		}

		odt.mu0.divide(dtm.dt.size());
		odt.S0.mtx.divide(dtm.dt.size());
		odt.Ymean.divide(dtm.dt.size());
		odt.A.divide(dtm.dt.size());
		odt.Q.mtx.divide(dtm.dt.size());
		odt.R.mtx.divide(dtm.dt.size());

		return odt;
	}

	/*
	 * ! \brief splits a component in current DT mixture
	 * 
	 * \param splitopt options for which component to split and how to split
	 * 
	 * \param ctg the index of the new component.
	 * 
	 * \param csp the component that was split
	 * 
	 * \see ppertC
	 */
	private void dytex_mix_split(DytexMix dtm, DytexSplitParams splitopt, int ctg[], int csp[]) {
		int i;
		int K = dtm.alpha.size();
		int c1 = 1;
		int c2;
		// int newK;
		if (K == 1) {
			c1 = 1;
		} else {
			Vector<Dytex> olddt = dtm.dt;
			Vector<Double> olda1 = dtm.alpha;
			Vector<Double> tmpal = new Vector<Double>();
			System.out.println("*** split criteria ***");

			Vector<Matrix> myQ = new Vector<Matrix>(K);
			Vector<Double> myQe = new Vector<Double>(K);
			double tmp[][] = new double[1][1];
			for (i = 0; i < K; i++) {
				myQ.add(new Matrix(tmp));
				myQe.add(0.0);
			}
			Matrix F;
			boolean flag = true;
			// vector<OPT_F_TYPE>::iterator result;
			switch (splitopt.crit) {
			// %%% split component with largest Q variance %%%
			case SPLITQ:
				for (int jjj = 0; jjj < K; jjj++) {
					boolean proceed = false;
					for (i = 0; i < splitopt.ignore.size() && (!proceed); i++) {
						if (splitopt.ignore.get(i) == jjj) {
							proceed = true;
						}
					}
					if (proceed) {
						myQe.set(jjj, -1.0);
					} else {
						// check empty
						if (olddt.get(jjj).isempty) {
							myQe.set(jjj, -1.0);
						} else {
							// normalize Q by forcing C to be orthonormal
							Matrix cMat = new Matrix(olddt.get(jjj).C.double2D);
							SingularValueDecomposition svd = new SingularValueDecomposition(cMat);
							Matrix matVt = svd.getV().transpose();
							double singularValues[] = svd.getSingularValues();
							double testArray[][] = new double[singularValues.length][singularValues.length];
							for (i = 0; i < singularValues.length; i++) {
								testArray[i][i] = singularValues[i];
							}
							Matrix test = new Matrix(testArray);
							F = test.times(matVt);
							myQ.set(jjj, (F.times(new Matrix(olddt.get(jjj).Q.mtx.double2D))).times(F.transpose()));
							double[] eigenvalue = new double[myQ.get(jjj).getColumnDimension()];
							double[][] eigenvector = new double[myQ.get(jjj).getRowDimension()][myQ.get(jjj)
									.getColumnDimension()];
							// In EigenvalueDecomposition the columns represent
							// the
							// eigenvectors
							Eigenvalue.decompose(myQ.get(jjj).getArray(), eigenvector, eigenvalue);
							double maxVal = -Double.MAX_VALUE;
							for (i = 0; i < eigenvalue.length; i++) {
								if (eigenvalue[i] > maxVal) {
									maxVal = eigenvalue[i];
								}
							}
							myQe.set(jjj, maxVal);
						}
					}
				}

				for (i = 0; i < myQe.size(); i++) {
					if (myQe.get(i) != -1) {
						flag = false;
						break;
					}
				}
				if (flag) {
					c1 = 0;
					System.out.println("nothing to split");
				} else {
					double result = -Double.MAX_VALUE;
					int index = -1;
					for (i = 0; i < myQe.size(); i++) {
						if (myQe.get(i) > result) {
							result = myQe.get(i);
							index = i;
						}
					}
					c1 = index + 1;
				}
				break;

			// split component with largest prior
			case SPLITP:
				tmpal = olda1;
				for (i = 0; i < splitopt.ignore.size(); i++) {
					tmpal.set(splitopt.ignore.get(i), -1.0);
				}
				double result = -Double.MAX_VALUE;
				int index = -1;
				for (i = 0; i < tmpal.size(); i++) {
					if (tmpal.get(i) > result) {
						result = tmpal.get(i);
						index = i;
					}
				}
				c1 = index + 1;

				break;

			default:
				MipavUtil.displayError("TO DO");
				System.exit(-1);
			}
		}

		// initialize with previous
		// adding a new one
		if (splitopt.target == -1) {
			c2 = K + 1;
			// newK=K+1;
			dtm.dt.add(new Dytex()); // add one more blank DT in the list
			dtm.alpha.add(0.0);

		}
		// updating existing
		else {
			c2 = splitopt.target;
			// newK=K;
		}

		System.out.println("Spliting Cluster " + c1 + " : new cluster " + c2);

		// check if there is anything to split
		if (c1 == 0) {
			dtm.dt.get(c2 - 1).isempty = true;
			dtm.alpha.set(c2 - 1, 0.0);
		} else {
			// duplicate cluster %%% all parameters c
			copyTo(dtm.dt.get(c1 - 1).A, dtm.dt.get(c2 - 1).A);
			copyTo(dtm.dt.get(c1 - 1).C, dtm.dt.get(c2 - 1).C);
			copyTo(dtm.dt.get(c1 - 1).Ymean, dtm.dt.get(c2 - 1).Ymean);
			copyTo(dtm.dt.get(c1 - 1).mu0, dtm.dt.get(c2 - 1).mu0);
			copyTo(dtm.dt.get(c1 - 1).Q.mtx, dtm.dt.get(c2 - 1).Q.mtx);
			dtm.dt.get(c2 - 1).Q.covopt = dtm.dt.get(c1 - 1).Q.covopt;
			dtm.dt.get(c2 - 1).Q.n = dtm.dt.get(c1 - 1).Q.n;
			dtm.dt.get(c2 - 1).Q.regopt = dtm.dt.get(c1 - 1).Q.regopt;
			dtm.dt.get(c2 - 1).Q.regval = dtm.dt.get(c1 - 1).Q.regval;
			copyTo(dtm.dt.get(c1 - 1).R.mtx, dtm.dt.get(c2 - 1).R.mtx);
			dtm.dt.get(c2 - 1).R.covopt = dtm.dt.get(c1 - 1).R.covopt;
			dtm.dt.get(c2 - 1).R.n = dtm.dt.get(c1 - 1).R.n;
			dtm.dt.get(c2 - 1).R.regopt = dtm.dt.get(c1 - 1).R.regopt;
			dtm.dt.get(c2 - 1).R.regval = dtm.dt.get(c1 - 1).R.regval;
			copyTo(dtm.dt.get(c1 - 1).S0.mtx, dtm.dt.get(c2 - 1).S0.mtx);
			dtm.dt.get(c2 - 1).S0.covopt = dtm.dt.get(c1 - 1).S0.covopt;
			dtm.dt.get(c2 - 1).S0.n = dtm.dt.get(c1 - 1).S0.n;
			dtm.dt.get(c2 - 1).S0.regopt = dtm.dt.get(c1 - 1).S0.regopt;
			dtm.dt.get(c2 - 1).S0.regval = dtm.dt.get(c1 - 1).S0.regval;
			dtm.dt.get(c2 - 1).isempty = dtm.dt.get(c1 - 1).isempty;
			dtm.dt.get(c2 - 1).vrows = dtm.dt.get(c1 - 1).vrows;
			dtm.dt.get(c2 - 1).vcols = dtm.dt.get(c1 - 1).vcols;
			dtm.dt.get(c2 - 1).dtopt = dtm.dt.get(c1 - 1).dtopt;
			copyTo(dtm.dt.get(c1 - 1).Cvs, dtm.dt.get(c2 - 1).Cvs);
			dtm.dt.get(c2 - 1).isCvs = dtm.dt.get(c1 - 1).isCvs;
			double tmp = dtm.alpha.get(c1 - 1) / ((double) 2.0);
			dtm.alpha.set(c1 - 1, tmp);
			dtm.alpha.set(c2 - 1, tmp);

			// perturb new cluster
			dytex_perturb(dtm.dt.get(c2 - 1), splitopt.pert, splitopt.mode, splitopt.vars);

			// also perturb old cluster (if principal axis split on C, A, x)
			if (splitopt.mode == Split_mode.MODEP) {
				if (splitopt.vars == Split_vars.VARC) {
					dytex_perturb(dtm.dt.get(c1 - 1), -splitopt.pert, splitopt.mode, splitopt.vars);
				}
			}

		}
		ctg[0] = c2 - 1;
		csp[0] = c1 - 1;
	}

	/*
	 * ! \brief custom perturbation of C based on max variance of S0
	 * 
	 * \param dtp Dt to perturb
	 * 
	 * \param pert pert value
	 * 
	 * \param mode perturbation mode;
	 * 
	 * \param vars variables to perturb
	 * 
	 * \remark only perturbation based on scale up principal axis and varialble
	 * C is implemented
	 *
	 * \see dytex_mix_split | ppertC
	 */
	private void dytex_perturb(Dytex dtp, double pert, Split_mode mode, Split_vars vars) {
		System.out.println("perturbing C by " + pert);

		switch (mode) {
		// scale up principal axis
		case MODEP:
			switch (vars) {
			// perturb C
			case VARC:
				ppertC(dtp.C, dtp.S0.mtx, pert);
				break;
			default:
				MipavUtil.displayError("TO DO");
				System.exit(-1);
			}
			break;
		default:
			MipavUtil.displayError("TO DO");
			System.exit(-1);
		}
	}

	/*
	 * ! \brief custom perturbation of C; normalize S0 by the lengths of C
	 * 
	 * \param C current C.
	 * 
	 * \param S0 parameter S0 of DT.
	 * 
	 * \param pert perturbation amount.
	 * 
	 * \see dytex_perturb
	 */
	private void ppertC(Mat C, Mat S0, double pert) {
		int i, x, y;
		// int m=C.rows;
		// int n=C.cols;

		Mat matCTC = times(transpose(C), C);
		double diag2D[][] = new double[matCTC.rows][1];
		for (i = 0; i < matCTC.rows; i++) {
			diag2D[i][0] = Math.sqrt(matCTC.double2D[i][i]);
		}
		Mat cc = new Mat(diag2D);
		Mat tmpM = elementTimes(cc, cc);
		Mat tmpM2 = elementTimes(S0, tmpM);
		// int maxLocx = -1;
		int maxLocy = -1;
		double maxVal = -Double.MAX_VALUE;
		for (y = 0; y < tmpM2.rows; y++) {
			for (x = 0; x < tmpM2.cols; x++) {
				if (tmpM2.double2D[y][x] > maxVal) {
					maxVal = tmpM2.double2D[y][x];
					// maxLocx = x;
					maxLocy = y;
				}
			}
		}
		for (y = 0; y < C.double2D.length; y++) {
			C.double2D[y][maxLocy] = (((double) 1.0) + pert) * C.double2D[y][maxLocy];
		}
	}

	private void copyTo(Mat A[], Mat B[]) {
		int i, j, n, d;
		int num = A.length;
		for (n = 0; n < num; n++) {
			B[n].flags = A[n].flags;
			B[n].dims = A[n].dims;
			B[n].depth = A[n].depth;
			B[n].rows = A[n].rows;
			B[n].cols = A[n].cols;
			if (A[n].size != null) {
				B[n].size = new int[A[n].size.length];
				for (i = 0; i < A[n].size.length; i++) {
					B[n].size[i] = A[n].size[i];
				}
			}
			B[n].type = A[n].type;
			if ((A[n].double2D != null) && (A[n].double2D[0] != null)) {
				if ((B[n].double2D == null) || (B[n].double2D[0] == null)
						|| (A[n].double2D.length != B[n].double2D.length)
						|| (A[n].double2D[0].length != B[n].double2D[0].length)) {
					B[n].double2D = new double[A[n].double2D.length][A[n].double2D[0].length];
				}
				for (i = 0; i < A[n].double2D.length; i++) {
					for (j = 0; j < A[n].double2D[0].length; j++) {
						B[n].double2D[i][j] = A[n].double2D[i][j];
					}
				}
			}
		}

	}

	private void copyTo(Mat A, Mat B) {
		int i, j;
		B.flags = A.flags;
		B.dims = A.dims;
		B.depth = A.depth;
		B.rows = A.rows;
		B.cols = A.cols;
		if (A.size != null) {
			B.size = new int[A.size.length];
			for (i = 0; i < A.size.length; i++) {
				B.size[i] = A.size[i];
			}
		}
		B.type = A.type;
		if ((A.double2D != null) && (A.double2D[0] != null)) {
			if ((B.double2D == null) || (B.double2D[0] == null) || (A.double2D.length != B.double2D.length)
					|| (A.double2D[0].length != B.double2D[0].length)) {
				B.double2D = new double[A.double2D.length][A.double2D[0].length];
			}
			for (i = 0; i < A.double2D.length; i++) {
				for (j = 0; j < A.double2D[0].length; j++) {
					B.double2D[i][j] = A.double2D[i][j];
				}
			}
		}
	}

	private void copyToDstColRange(Mat A, Mat B, int inclusiveStart, int exclusiveEnd) {
		int i, j;
		for (i = 0; i < A.double2D.length; i++) {
			for (j = inclusiveStart; j < exclusiveEnd; j++) {
				B.double2D[i][j] = A.double2D[i][j - inclusiveStart];
			}
		}
	}

	private void copyFromSrcColRange(Mat A, Mat B, int inclusiveStart, int exclusiveEnd) {
		int i, j;
		for (i = 0; i < A.double2D.length; i++) {
			for (j = inclusiveStart; j < exclusiveEnd; j++) {
				B.double2D[i][j - inclusiveStart] = A.double2D[i][j];
			}
		}
	}

	/*
	 * ! \brief Verbose modes.
	 */
	public enum Verbose_mode {
		QUIET, COMPACT, VERBOSE
	};

	/*
	 * ! \brief HEM learninig options.
	 *
	 * \remarks In HEM implementation few options are not implemented and their
	 * default values are used instead \see DytexMix | EMOptions
	 */
	public class HEMOptions {
		/*
		 * ! \brief number of clusters.
		 */
		public int K;
		/*
		 * ! \brief number of virtual samples.
		 */
		public int N;
		/*
		 * ! \brief temporal length of virtual samples.
		 */
		public int tau;
		/*
		 * ! \brief regularization options. \see DytexRegOptions
		 */
		public DytexRegOptions regopt;
		/*
		 * ! \brief termination parameter.
		 */
		double termvalue;
		/*
		 * ! \brief termination value for the HEMBEST.
		 */
		double termvalBest; // termination value for the EMBEST
		/*
		 * ! \brief maximum number of iterations.
		 */
		int maxiter;
		/*
		 * ! \brief Verbose mode.
		 */
		Verbose_mode verbose;
		/*
		 * ! \brief empty cluster splitting options. \see DytexSplitParams
		 */
		DytexSplitParams emptySplitOpt = new DytexSplitParams();
		/*
		 * ! \brief cluster splitting options. \see DytexSplitParams
		 */
		DytexSplitParams splitOpt = new DytexSplitParams();
		/*
		 * ! \brief assume DT are zero-mean or not.
		 */
		public Ymean_type Ymean;

		/*
		 * ! \brief initialize HEMOptions object.
		 * 
		 * \param K number of clusters.
		 * 
		 * \param regopt regularization options.
		 * 
		 * \param termvalue termination parameter.
		 * 
		 * \param ymean assume DT are zero-mean or not.
		 * 
		 * \param verbose verbose value.
		 * 
		 * \see DytexOptions | DytexMix | EMOptions
		 */
		public HEMOptions(int K, DytexRegOptions reg, double termvalue, Ymean_type ym, Verbose_mode verbose) {
			// setting parameters
			this.K = K;
			this.regopt = reg;
			this.verbose = verbose;
			this.termvalue = termvalue;
			this.termvalBest = 1e-5; // default
			maxiter = 500;

			// setting empty cluster splitting options
			emptySplitOpt.crit = Split_crit.SPLITP;
			emptySplitOpt.pert = 0.01;
			emptySplitOpt.mode = Split_mode.MODEP;
			emptySplitOpt.vars = Split_vars.VARC;
			Ymean = ym;
			N = 1000;
			tau = 20;

		}
	};

	/*
	 * ! \brief splitting criteria options. 'SPLITQ' -- split component with
	 * largest Q variance 'SPLITP' -- split component with largest prior
	 */
	public enum Split_crit {
		SPLITQ, SPLITP
	};

	/*
	 * ! \brief perturbation mode options. 'MODEP' -- = scale up principal axis
	 */
	public enum Split_mode {
		MODEP
	};

	/*
	 * ! \brief perturbation variable option, 'VARC' - observation matrix.
	 */
	public enum Split_vars {
		VARC
	};

	/*
	 * ! \brief Component splitting options in EM and HEM.
	 *
	 * \see DytexMix
	 */
	public class DytexSplitParams {
		/*
		 * ! \brief the growing schedule.
		 */
		public Vector<Integer> sched = new Vector<Integer>();

		/*
		 * ! \brief splitting criteria.
		 */
		public Split_crit crit;

		/*
		 * ! \brief perturbation mode.
		 */
		public Split_mode mode;
		/*
		 * ! \brief perturbation amount.
		 */
		public double pert;

		/*
		 * ! \brief variables to perturb.
		 */
		public Split_vars vars;
		/*
		 * ! \brief indices of components to ignore for splitting.
		 */
		Vector<Integer> ignore = new Vector<Integer>();
		/*
		 * ! \brief the index of the new component.
		 */
		public int target;

		// initialize DytexSplitParams
		public DytexSplitParams() {
			crit = Split_crit.SPLITQ;
			mode = Split_mode.MODEP;
			pert = 0.01;
			vars = Split_vars.VARC;
			target = -1;
		};
	};

	/** Options for modeling the observation mean, Ymean. */
	public enum Ymean_type {
		ZERO_YMEAN(0),
		/** < assume observations are already zero-mean, i.e. Ymean=0. */
		NONZERO_YMEAN(1),
		/** < model non-zero observation mean. */
		ILLEGAL_YMEAN(2);
		public final int Ymean_code;

		Ymean_type(int Ymean_code) {
			this.Ymean_code = Ymean_code;
		}
	}

	public Ymean_type getYmean_type(int Ymean_code) {
		if (Ymean_code == 0) {
			return Ymean_type.ZERO_YMEAN;
		} else if (Ymean_code == 1) {
			return Ymean_type.NONZERO_YMEAN;
		}

		else {
			MipavUtil.displayError("Illegal number = " + Ymean_code + " for Ymean_type");
			return Ymean_type.ILLEGAL_YMEAN;
		}
	}

	public class DytexOptions {
		// DT options
		public int n;
		/** < dimension of the state space, x_t. */
		public int m;
		/** < dimension of the observation space, y_t. */
		public cov_type Ropt;
		/** < covariance type for R (usually COV_IID). */
		public cov_type Sopt;
		/** < covariance type for S (usually COV_DIAG). */
		Ymean_type Yopt;

		/** < option to model observation mean. */
		public DytexOptions() {

		}

		// constructor
		DytexOptions(int n, int m, cov_type Ropt, cov_type Sopt, Ymean_type Yopt) {
			this.n = n;
			this.m = m;
			this.Ropt = Ropt;
			this.Sopt = Sopt;
			this.Yopt = Yopt;
		}
	}

	/**
	 * Class for specifying the regularization methods for a Dytex. \sa
	 * CovMatrix, Dytex, DytexOptions
	 */
	public class DytexRegOptions {
		// options
		public cov_reg_type Ropt, /** < Regularization method for R. */
				Qopt, /** < Regularization method for Q. */
				Sopt, /** < Regularization method for S. */
				Aopt;
		/** < Regularization method for S. */
		public double Rval, /** < Regularization value for R. */
				Qval, /** < Regularization value for Q. */
				Sval, /** < Regularization value for S. */
				Aval;

		/** < Regularization value for S. */

		public DytexRegOptions(cov_reg_type Ropt, double Rval, cov_reg_type Qopt, double Qval, cov_reg_type Sopt,
				double Sval, cov_reg_type Aopt, double Aval) {
			this.Ropt = Ropt;
			this.Rval = Rval;
			this.Qopt = Qopt;
			this.Qval = Qval;
			this.Sopt = Sopt;
			this.Sval = Sval;
			this.Aopt = Aopt;
			this.Aval = Aval;
		}

		public DytexRegOptions(cov_reg_type Ropt, double Rval, cov_reg_type Qopt, double Qval, cov_reg_type Sopt,
				double Sval) {
			this.Ropt = Ropt;
			this.Rval = Rval;
			this.Qopt = Qopt;
			this.Qval = Qval;
			this.Sopt = Sopt;
			this.Sval = Sval;
		}

	}

	/** type of covariance matrix. */
	public enum cov_type {
		COV_FULL(0),
		/** < Full covariance matrix. */
		COV_DIAG(1),
		/** < diagonal covariance matrix. */
		COV_IID(2), COV_ILLEGAL(3);
		/** < iid covariance matrix. */

		public final int cov_code;

		cov_type(int cov_code) {
			this.cov_code = cov_code;
		}
	}

	public cov_type getCov_type(int cov_code) {
		if (cov_code == 0) {
			return cov_type.COV_FULL;
		} else if (cov_code == 1) {
			return cov_type.COV_DIAG;
		} else if (cov_code == 2) {
			return cov_type.COV_IID;
		} else {
			MipavUtil.displayError("Illegal number = " + cov_code + " for cov_type");
			return cov_type.COV_ILLEGAL;
		}
	}

	/** regularization method. */
	public enum cov_reg_type {
		COV_REG_NONE(0),
		/** < no regularization */
		COV_REG_MIN(1),
		/** < enforce a minimum eigenvalue of regval */
		COV_REG_ADD(2),
		/** < add a constant regval to the diagonal */
		COV_REG_ILLEGAL(3);
		public final int cov_reg_code;

		cov_reg_type(int cov_reg_code) {
			this.cov_reg_code = cov_reg_code;
		}
	}

	public cov_reg_type getCov_reg_type(int cov_reg_code) {
		if (cov_reg_code == 0) {
			return cov_reg_type.COV_REG_NONE;
		} else if (cov_reg_code == 1) {
			return cov_reg_type.COV_REG_MIN;
		} else if (cov_reg_code == 2) {
			return cov_reg_type.COV_REG_ADD;
		} else {
			MipavUtil.displayError("Illegal number = " + cov_reg_code + " for cov_reg_type");
			return cov_reg_type.COV_REG_ILLEGAL;
		}
	}

	public class CovMatrix {
		public int n;
		/** < dimension of the (square) covariance matrix. */
		public Mat mtx = new Mat();
		/**
		 * < storage for the covariance matrix. full matrix is [n x n]; diagonal
		 * matrix is [n x 1]; iid matrix is [1 x 1].
		 */
		public cov_type covopt;
		/** < type of covariance matrix */
		public cov_reg_type regopt;
		/** < type of regularization to be used */
		public double regval;

		/** < regularization value */
		/** cache the matrix square-root of the covariance matrix. */
		// private Mat sqrtmtx;
		public CovMatrix() {
			// sqrtmtx = null;
		}

		public CovMatrix(int n, cov_type covopt) {
			mtx = new Mat((covopt == cov_type.COV_IID ? 1 : n), (covopt == cov_type.COV_FULL ? n : 1), CV_64F); // initialize
																												// mtx
			regopt = cov_reg_type.COV_REG_NONE;
			regval = 0.0; // initialize regs

			this.covopt = covopt;
			this.n = n;
			switch (covopt) {
			case COV_FULL:
			case COV_DIAG:
			case COV_IID:
				break;
			default:
				MipavUtil.displayError("Error: invalid cov_type");
				System.exit(-1);
			}
			if (n < 1) {
				MipavUtil.displayError("Error: invalid n");
				System.exit(-1);
			}

			// sqrtmtx = null;
		}
		
	}

	// convert to full matrix, return it
	Mat toFullMatrix(CovMatrix cov) {
	  int r;
	  Mat fmtx = new Mat();
	  fmtx.create(cov.n, cov.n, CV_64F);
	  switch(cov.covopt) {
	  case COV_FULL:
	    copyTo(cov.mtx,fmtx);
	    break;
	  case COV_DIAG:
		  for (r = 0; r < cov.n; r++) {
			  fmtx.double2D[r][r] = cov.mtx.double2D[r][0];
		  }
	    break;
	  case COV_IID:
		for (r = 0; r < cov.n; r++) {
			fmtx.double2D[r][r] = cov.mtx.double2D[0][0];
		}   
	    break;
	  default:
	    MipavUtil.displayError("ERROR: invalid cov_type!");
	    System.exit(-1);
	  }
	  return fmtx;
	}
	
	// return the inverse of a covariance matrix
	CovMatrix inv(CovMatrix cov) {
      int r;
	  CovMatrix out = new CovMatrix(cov.n, cov.covopt);

	  switch(cov.covopt) {
	  case COV_FULL:
	    out.mtx = new Mat(new Matrix(cov.mtx.double2D).inverse().getArray());
	    break;
	  case COV_DIAG:
		  for (r = 0; r < cov.n; r++) {
			  out.mtx.double2D[r][0] = 1.0/cov.mtx.double2D[r][0];
		  }
	  case COV_IID:
	    out.mtx.double2D[0][0] = 1.0/cov.mtx.double2D[0][0];
	    break;
	  default:
	    MipavUtil.displayError("bad covopt");
	    System.exit(-1);
	  }

	  return out;
	}
	
	// dst = cov*src  (n x m) = (n x n) * (n x m)
	void postmultiply(CovMatrix cov, Mat src, Mat dst) {
	  int c;
	  // allocate the output matrix
	  if (!(src.rows == cov.n)) {
		  MipavUtil.displayError("!(src.rows == cov.n) in postMultiply");
		  System.exit(-1);
	  }
	  dst.create(cov.n, src.cols, CV_64F);
	  switch(cov.covopt) {
	  case COV_FULL:
	    dst = times(cov.mtx,src);
	    break;
	  case COV_DIAG:
	    copyTo(src,dst);
	    scaleRows(dst, cov.mtx);
	    break;
	  case COV_IID:
	    copyTo(src,dst);
	    for (c = 0; c < src.cols; c++) {
	        dst.double2D[0][c] *= cov.mtx.double2D[0][0];
	    }
	    break;
	  default:
	    MipavUtil.displayError("bad covopt");
	    System.exit(-1);
	  }
	}

	/**
	 * Dynamic Texture class. This is the class for a standard dynamic texture.
	 * It serves as the base class for the online version of the DT. It includes
	 * functions for: 1) estimating parameters; 2) regularizing; 3) synthesizing
	 * video; 4) pre-processing video for usage with DT (for other classes).
	 */
	public class Dytex {
		// DT options
		public DytexOptions dtopt = new DytexOptions();
		/** < options for the Dytex */
		// DT parameters
		public Mat Ymean = new Mat();
		/** < observation mean */
		public Mat A = new Mat();
		/** < transition matrix */
		public Mat C = new Mat();
		/** < observation matrix */
		public Mat mu0 = new Mat();
		/** < initial state mean */
		public CovMatrix R = new CovMatrix();
		/** < observation noise covariance */
		public CovMatrix Q = new CovMatrix();
		/** < state noise covariance */
		public CovMatrix S0 = new CovMatrix();
		/** < initial state covariance */

		// video parameters (for synthesizing)
		// set to 0,0 if unknown
		public int vrows, /**
							 * < for synthesis, number of rows in a frame (0 if
							 * unknown).
							 */
				vcols;
		/** < for synthesis, number of columns in a frame (0 if unknown). */
		public boolean isempty;
		/** < indicates am empty Dt */
		public Mat Cvs = new Mat();
		/** < Cvs precomputed value */
		public boolean isCvs;

		/** < Cvs computed */
		public Dytex() {
			isempty = true;
		}

		// constructor
		public Dytex(DytexOptions opt) {
			dtopt = opt;
			if (dtopt.Yopt == Ymean_type.NONZERO_YMEAN) {
				Ymean = new Mat(dtopt.m, 1, CV_64F);
			} else {
				Ymean = new Mat(0, 0, CV_64F);
			}
			A = new Mat(dtopt.n, dtopt.n, CV_64F);
			C = new Mat(dtopt.m, dtopt.n, CV_64F);
			mu0 = new Mat(dtopt.n, 1, CV_64F);
			R = new CovMatrix(dtopt.m, dtopt.Ropt);
			Q = new CovMatrix(dtopt.n, cov_type.COV_FULL);
			S0 = new CovMatrix(dtopt.n, dtopt.Sopt);
			vrows = 0;
			vcols = 0;
			isempty = false;
			isCvs = false;
			switch (dtopt.Yopt) {
			case NONZERO_YMEAN:
			case ZERO_YMEAN:
				break;
			default:
				MipavUtil.displayError("bad Yopt");
				System.exit(-1);
			}
		}
	}

	public class Mat {
		/*
		 * ! includes several bit-fields: - the magic signature - continuity
		 * flag - depth - number of channels
		 */
		public int flags;
		// ! the array dimensionality, >= 2
		public int dims;
		// ! the number of rows and columns or (-1, -1) when the array has more
		// than 2 dimensions
		public int depth, rows, cols;
		public int channels = 1;
		public int elemSize;
		public int size[];
		public int type;
		public int step[] = new int[3];
		public int bytesPerRow;
		// ! pointer to the data
		public byte data[];
		public byte byte2D[][];
		public byte byte2DC[][][];
		public double double2D[][];
		public double double2DC[][][];
		public byte byte3D[][][];
		public byte byte3DC[][][][];
		public double double3D[][][];
		public double double3DC[][][][];

		// ! pointer to the reference counter;
		// when array points to user-allocated data, the pointer is NULL
		public int refcount[];

		public Mat() {

		}

		public Mat(int rows, int cols, int type) {
			this.rows = rows;
			this.cols = cols;
			this.type = type;
			dims = 2;
			size = new int[] { rows, cols };
			if ((type == CV_8U) || (type == CV_8UC1)) {
				byte2D = new byte[rows][cols];
			} else if (type == CV_64F) {
				double2D = new double[rows][cols];
			} else if (type == CV_64FC3) {
				double2DC = new double[rows][cols][3];
			}
		}

		public Mat(int dims, int size[], int type) {
			int x, y, z;
			this.dims = dims;
			this.size = size;
			this.type = type;
			if (dims == 2) {
				this.rows = size[0];
				this.cols = size[1];
			} else if (dims == 3) {
				this.depth = size[0];
				this.rows = size[1];
				this.cols = size[2];
				step[0] = 1;
			}
			if (dims == 2) {
				if ((type == CV_8U) || (type == CV_8UC1)) {
					byte2D = new byte[rows][cols];
				} else if (type == CV_64F) {
					double2D = new double[rows][cols];
				} else if (type == CV_64FC3) {
					double2DC = new double[rows][cols][3];
				}
			} // if (dims == 2)
			else if (dims == 3) {
				if ((type == CV_8U) || (type == CV_8UC1)) {
					byte3D = new byte[depth][rows][cols];
					step[1] = cols;
				} else if (type == CV_64F) {
					double3D = new double[depth][rows][cols];
					step[1] = 8 * cols;
				} else if (type == CV_64FC3) {
					double3DC = new double[depth][rows][cols][3];
					step[1] = 24 * cols;
				}
			} // else if (dims == 3)
		}

		public Mat(int dims, int size[], int type, int channels) {
			int x, y, z;
			this.dims = dims;
			this.size = size;
			this.type = type;
			this.channels = channels;
			if (dims == 2) {
				this.rows = size[0];
				this.cols = size[1];
			} else if (dims == 3) {
				this.depth = size[0];
				this.rows = size[1];
				this.cols = size[2];
				step[0] = 1;
			}
			if (dims == 2) {
				if (type == CV_8UC) {
					byte2DC = new byte[rows][cols][channels];
				} else if (type == CV_64FC) {
					double2DC = new double[rows][cols][channels];
				}
			} // if (dims == 2)
			else if (dims == 3) {
				if (type == CV_8UC) {
					byte3DC = new byte[depth][rows][cols][channels];
					step[1] = cols;
				} else if (type == CV_64FC) {
					double3DC = new double[depth][rows][cols][channels];
					step[1] = 8 * cols;
				}
			} // else if (dims == 3)
		}

		public Mat(double d2D[][]) {
			this.rows = d2D.length;
			this.cols = d2D[0].length;
			this.type = CV_64F;
			dims = 2;
			size = new int[] { rows, cols };
			double2D = new double[rows][cols];
			for (int r = 0; r < rows; r++) {
				for (int c = 0; c < cols; c++) {
					double2D[r][c] = d2D[r][c];
				}
			}
		}

		public void create(int rows, int cols, int type) {
			int x, y;
			this.rows = rows;
			this.cols = cols;
			this.type = type;
			dims = 2;
			size = new int[] { rows, cols };
			if ((type == CV_8U) || (type == CV_8UC1)) {
				byte2D = new byte[rows][cols];
			} else if (type == CV_64F) {
				double2D = new double[rows][cols];
			} else if (type == CV_64FC3) {
				double2DC = new double[rows][cols][3];
			}
		}

		public void create(int dims, int size[], int type, int channels) {
			this.dims = dims;
			this.size = size;
			this.type = type;
			this.channels = channels;
			if (dims == 2) {
				this.rows = size[0];
				this.cols = size[1];
			} else if (dims == 3) {
				this.depth = size[0];
				this.rows = size[1];
				this.cols = size[2];
				step[0] = 1;
			}
			if (dims == 2) {
				if (type == CV_8UC) {
					byte2DC = new byte[rows][cols][channels];
				} else if (type == CV_64FC) {
					double2DC = new double[rows][cols][channels];
				}
			} else if (dims == 3) {
				if (type == CV_8UC) {
					byte3DC = new byte[depth][rows][cols][channels];
					step[1] = cols;
				} else if (type == CV_64FC) {
					double3DC = new double[depth][rows][cols][channels];
					step[1] = 8 * cols * channels;
				}
			}
		}

		public void create(int dims, int size[], int type) {
			int x, y, z;
			this.dims = dims;
			this.size = size;
			this.type = type;
			if (dims == 2) {
				this.rows = size[0];
				this.cols = size[1];
			} else if (dims == 3) {
				this.depth = size[0];
				this.rows = size[1];
				this.cols = size[2];
				step[0] = 1;
			}
			if (dims == 2) {
				if (type == CV_8U) {
					byte2D = new byte[rows][cols];
				} else if (type == CV_64F) {
					double2D = new double[rows][cols];
				} else if (type == CV_64FC3) {
					double2DC = new double[rows][cols][3];
				}
			} // if (dims == 2)
			else if (dims == 3) {
				if (type == CV_8U) {
					byte3D = new byte[depth][rows][cols];
					step[1] = cols;
				} else if (type == CV_64F) {
					double3D = new double[depth][rows][cols];
					step[1] = 8 * cols;
				} else if (type == CV_64FC3) {
					double3DC = new double[depth][rows][cols][3];
					step[1] = 24 * cols;
				}
			} // else if (dims == 3)
		}

		public void init(double val) {
			if (dims == 2) {
				if (type == CV_64F) {
					for (int r = 0; r < rows; r++) {
						for (int c = 0; c < cols; c++) {
							double2D[r][c] = val;
						}
					}
				}
			} else if (dims == 3) {
				if (type == CV_64F) {
					for (int d = 0; d < depth; d++) {
						for (int r = 0; r < rows; r++) {
							for (int c = 0; c < cols; c++) {
								double3D[d][r][c] = val;
							}
						}
					}
				}
			}
		}

		public void divide(double val) {
			if (dims == 2) {
				if (type == CV_64F) {
					for (int r = 0; r < rows; r++) {
						for (int c = 0; c < cols; c++) {
							double2D[r][c] /= val;
						}
					}
				}
			} else if (dims == 3) {
				if (type == CV_64F) {
					for (int d = 0; d < depth; d++) {
						for (int r = 0; r < rows; r++) {
							for (int c = 0; c < cols; c++) {
								double3D[d][r][c] /= val;
							}
						}
					}
				}
			}
		}

		public void multiply(double val) {
			if (dims == 2) {
				if (type == CV_64F) {
					for (int r = 0; r < rows; r++) {
						for (int c = 0; c < cols; c++) {
							double2D[r][c] *= val;
						}
					}
				}
			} else if (dims == 3) {
				if (type == CV_64F) {
					for (int d = 0; d < depth; d++) {
						for (int r = 0; r < rows; r++) {
							for (int c = 0; c < cols; c++) {
								double3D[d][r][c] *= val;
							}
						}
					}
				}
			}
		}
		
		public void release() {
			if (dims == 2) {
				if (type == CV_64F) {
					for (int r = rows-1; r >= 0; r--) {
						double2D[r] = null;
					}
					double2D = null;
				}
				else if (type == CV_64FC3) {
					for (int r = rows-1; r >= 0; r--) {
						for (int c = cols-1; c >= 0; c--) {
							double2DC[r][c] = null;
						}
						double2DC[r] = null;
					}
					double2DC = null;
				}
				else if (type == CV_8U) {
					for (int r = rows-1; r >= 0; r--) {
						byte2D[r] = null;
					}
					byte2D = null;   	
				}
			} // if (dims == 2)
			else if (dims == 3) {
			    if (type == CV_64F) {
			    	for (int d = depth-1; d >= 0; d--) {
						for (int r = rows-1; r >= 0; r--) {
							double3D[d][r] = null;
						}
						double3D[d] = null;
					}
					double3D = null;	
			    }
			    else if (type == CV_64FC3) {
			    	for (int d = depth-1; d >= 0; d--) {
			    		for (int r = rows-1; r >= 0; r--) {
			    			for (int c = cols-1; c >= 0; c--) {
			    				double3DC[d][r][c] = null;
			    			}
			    			double3DC[d][r] = null;
			    		}
			    		double3DC[d] = null;
			    	}
			    	double3DC = null;
			    }
			    else if (type == CV_8U) {
			    	for (int d = depth-1; d >= 0; d--) {
						for (int r = rows-1; r >= 0; r--) {
							byte3D[d][r] = null;
						}
						byte3D[d] = null;
					}
					byte3D = null;		
			    }
			} // else if (dims == 3)
			size = null;
			step = null;
			refcount = null;
		}
	}

	public Mat plus(Mat A, Mat B) {
		int r, c;
		if (A.rows != B.rows) {
			MipavUtil.displayError("A.rows != B.rows in Mat plus");
			System.exit(-1);
		}
		if (A.cols != B.cols) {
			MipavUtil.displayError("A.cols != B.cols in Mat plus");
			System.exit(-1);
		}
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] + B.double2D[r][c];
			}
		}
		return dest;
	}
	
	public Mat plus(Mat A, double b) {
		int r, c;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] + b;
			}
		}
		return dest;
	}

	public Mat divide(Mat A, Mat B) {
		int d, r, c;
		if (A.rows != B.rows) {
			MipavUtil.displayError("A.rows != B.rows in Mat divide");
			System.exit(-1);
		}
		if (A.cols != B.cols) {
			MipavUtil.displayError("A.cols != B.cols in Mat divide");
			System.exit(-1);
		}
		Mat dst = null;
		if (A.type == CV_64F) {
			if (A.dims == 2) {
				dst = new Mat(A.rows, A.cols, CV_64F);
				for (r = 0; r < A.rows; r++) {
					for (c = 0; c < A.cols; c++) {
						dst.double2D[r][c] = A.double2D[r][c] / B.double2D[r][c];
					}
				}
			} else if (A.dims == 3) {
				if (A.depth != B.depth) {
					MipavUtil.displayError("A.depth != B.depth in Mat divide");
					System.exit(-1);
					dst = new Mat(A.dims, A.size, CV_64F);
					for (d = 0; d < A.depth; d++) {
						for (r = 0; r < A.rows; r++) {
							for (c = 0; c < A.cols; c++) {
								dst.double3D[d][r][c] = A.double3D[d][r][c] / B.double3D[d][r][c];
							}
						}
					}
				}
			}
		}
		return dst;
	}

	public Mat minus(Mat A, Mat B) {
		int r, c;
		if (A.rows != B.rows) {
			MipavUtil.displayError("A.rows != B.rows in Mat minus");
			System.exit(-1);
		}
		if (A.cols != B.cols) {
			MipavUtil.displayError("A.cols != B.cols in Mat minus");
			System.exit(-1);
		}
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] - B.double2D[r][c];
			}
		}
		return dest;
	}
	
	public Mat minus(Mat A, double b) {
		int r, c;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] - b;
			}
		}
		return dest;
	}

	public Mat elementTimes(Mat A, Mat B) {
		int r, c;
		if (A.rows != B.rows) {
			System.out.println("A.rows != B.rows in Mat elementTimes");
			System.exit(-1);
		}
		if (A.cols != B.cols) {
			System.out.println("A.cols != B.cols in Mat elementTimes");
			System.exit(-1);
		}
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] * B.double2D[r][c];
			}
		}
		return dest;
	}

	public Mat times(Mat A, Mat B) {
		int i, r, c;
		if (A.cols != B.rows) {
			MipavUtil.displayError("A.cols != B.rows in Mat times");
			System.exit(-1);
		}
		int rows = A.rows;
		int cols = B.cols;
		int type = A.type;
		int inner = A.cols;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				for (i = 0; i < inner; i++) {
					dest.double2D[r][c] += (A.double2D[r][i] * B.double2D[i][c]);
				}
			}
		}
		return dest;
	}

	public Mat[] times(Mat A[], double q) {
		int i, r, c;
		int rows = A[0].rows;
		int cols = A[0].cols;
		int type = A[0].type;
		Mat dest[] = new Mat[A.length];
		for (i = 0; i < A.length; i++) {
			dest[i] = new Mat(rows, cols, type);
			for (r = 0; r < rows; r++) {
				for (c = 0; c < cols; c++) {
					dest[i].double2D[r][c] = A[i].double2D[r][c] * q;
				}
			}
		}

		return dest;
	}

	public Mat times(Mat A, double q) {
		int r, c;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] * q;
			}
		}
		return dest;
	}

	public Mat divide(Mat A, double q) {
		int r, c;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(rows, cols, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[r][c] = A.double2D[r][c] / q;
			}
		}
		return dest;
	}

	public Mat transpose(Mat A) {
		int r, c;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		Mat dest = new Mat(cols, rows, type);
		for (r = 0; r < rows; r++) {
			for (c = 0; c < cols; c++) {
				dest.double2D[c][r] = A.double2D[r][c];
			}
		}
		return dest;
	}
	
	public Mat col(Mat A, int colNum) {
		int r;
		int rows = A.rows;
		int type = A.type;
		Mat dest = new Mat(rows, 1, type);
		for (r = 0; r < rows; r++) {
		    dest.double2D[r][0] = A.double2D[r][colNum];
		}
		return dest;
	}
	
	public double sum(Mat A) {
		int d,r,c;
		double sum = 0.0;
		int rows = A.rows;
		int cols = A.cols;
		int dims = A.dims;
		if (dims == 2) {
		    for (r = 0; r < rows; r++) {
		    	for (c = 0; c < cols; c++) {
		    		sum += A.double2D[r][c];
		    	}
		    }
		}
		else {
			int depth = A.depth;
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						sum += A.double3D[d][r][c];
					}
				}
			}
		}
		return sum;
	}
	
	public int countNonZero(Mat A) {
		int d,r,c;
		int count = 0;
		int rows = A.rows;
		int cols = A.cols;
		int dims = A.dims;
		if (dims == 2) {
		    for (r = 0; r < rows; r++) {
		    	for (c = 0; c < cols; c++) {
		    		if (A.double2D[r][c] != 0.0) {
		    		    count++;	
		    		}
		    	}
		    }
		}
		else {
			int depth = A.depth;
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						if (A.double3D[d][r][c] != 0.0) {
			    		    count++;	
			    		}
					}
				}
			}
		}
		return count;
	}


	public Mat clone(Mat A) {
		int d, r, c;
		Mat dest;
		int rows = A.rows;
		int cols = A.cols;
		int type = A.type;
		int dims = A.dims;
		int size[] = A.size;
		if (dims == 2) {
			dest = new Mat(rows, cols, type);
			for (r = 0; r < rows; r++) {
				for (c = 0; c < cols; c++) {
					dest.double2D[r][c] = A.double2D[r][c];
				}
			}
		} else {
			dest = new Mat(dims, size, type);
			int depth = A.depth;
			for (d = 0; d < depth; d++) {
				for (r = 0; r < rows; r++) {
					for (c = 0; c < cols; c++) {
						dest.double3D[d][r][c] = A.double3D[d][r][c];
					}
				}
			}
		}
		return dest;
	}

	public double trace(Mat A) {
		int r;
		double ret = 0.0;
		for (r = 0; r < Math.min(A.rows, A.cols); r++) {
			ret += A.double2D[r][r];
		}
		return ret;
	}

	/*
	 * public Mat repeat(Mat img, int nf) { Mat v; if (nf <= 0) {
	 * MipavUtil.displayError("nf = " + nf + " in public Mat repeat");
	 * System.exit(-1); } v = create(nf, img.rows, img.cols, img.type); for (int
	 * j=0; j<nf; j++) { Mat f = frame(v, j); copyTo(img,f); } return v; }
	 */

	public Mat[] repeat(Mat img, int nf) {
		Mat v[];
		if (nf <= 0) {
			MipavUtil.displayError("nf = " + nf + " in public Mat repeat");
			System.exit(-1);
		}
		v = create(nf, img.rows, img.cols, img.type);
		for (int j = 0; j < nf; j++) {
			Mat f = v[j];
			copyTo(img, f);
		}
		return v;
	}

	public void repeat(Mat src, int ny, int nx, Mat dst) {
		int y, x, r, c;
		dst.create(ny * src.rows, nx * src.cols, src.type);
		for (y = 0; y < ny; y++) {
			for (x = 0; x < nx; x++) {
				for (r = 0; r < src.rows; r++) {
					for (c = 0; c < src.cols; c++) {
						dst.double2D[y * src.rows + r][x * src.cols + c] = src.double2D[r][c];
					}
				}
			}
		}
	}
	
	// scale each row(i) by w(i)
	void scaleRows(Mat m, Mat w) {
	 int c;
	 int wr,wc;
	  if (!(m.rows == w.rows*w.cols)) {
		  MipavUtil.displayError("!(m.rows == w.rows*w.cols) in scaleRows");
		  System.exit(-1);
	  }
	  for (int i=0; i<m.rows; i++) {
		wr = i/w.cols;
		wc = i % w.cols;
		for (c = 0; c < m.cols; c++) {
	        m.double2D[i][c] *= w.double2D[wr][wc];
		}
	  }
	}

	public class DytexMix {
		public DytexOptions opt = new DytexOptions();
		/*
		 * ! \brief DT components in the mixture. \see Dytex
		 */
		Vector<Dytex> dt = new Vector<Dytex>();
		/*
		 * ! \brief DT components priors.
		 */
		public Vector<Double> alpha = new Vector<Double>();
		/*
		 * ! \brief Class of each training video.
		 */
		public Vector<Integer> classes = new Vector<Integer>();
		
		/*!
		 * \brief DytexKalmanFilter cache bank for computing log likelihood of new patches given this DTM.	 	 
		 */
		Vector<DytexKalmanFilter> kfb = new Vector<DytexKalmanFilter>();

		public DytexMix() {

		}

		public DytexMix(DytexOptions opt) {
			this.opt = opt;
		}
	}

	public void read(DytexMix dtm) {
		readHeader("DytexMix");
		read(dtm.opt);
		int K[] = new int[1];
		read("K", K);
		dtm.alpha.clear();
		for (int i = 0; i < K[0]; i++) {
			double temp[] = new double[1];
			read("alpha", temp);
			dtm.alpha.add(temp[0]);
		}

		dtm.dt.clear();
		for (int i = 0; i < K[0]; i++) {
			Dytex tmpd = new Dytex();
			read(tmpd);
			dtm.dt.add(tmpd);
		}
		dtm.classes.clear();
		read("classes", dtm.classes);
	}
	
	/*!
	 * \brief
	 * writes a DytexMix object with default name "DytexMix".
	 * 
	 * \param dtm
	 * value of DytexMix.
	 *  
	 * \see
	 * Bufferer::read(DytexMix &dtm) | Bufferer::write(const Dytex &dt)
	 */
	public void write(DytexMix dtm)
	{		
		long spos=writeHeader("DytexMix");
		write(dtm.opt);

		write("K",(int)dtm.alpha.size());		
		//writing alpha
		for(int i=0;i<dtm.alpha.size();i++)
		{
			write("alpha",dtm.alpha.get(i));		
		}

		//writing DTs
		for(int i=0;i<dtm.dt.size();i++)
		{
			write(dtm.dt.get(i));		
		}

		write("classes",dtm.classes);

		writeSize(spos);
	}

	public void read(String name, Vector<Integer> vec) {
		readHeader(name);
		int len;
		try {
			len = getInt(endian);

			for (int i = 0; i < len; i++) {
				int temp;
				temp = getInt(endian);
				vec.add(temp);
			}
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
	}
	
	/*!
	 * \brief
	 * Writes a vector of int.
	 * 
	 * \param name
	 * name of the int vector object.
	 * 
	 * \param vec
	 * list of ints.
	 *  
	 * \see
	 *  Bufferer::read(string name,std::vector<int> &vec)
	 */
	public void write(String name,Vector<Integer> vec)
	
	{
		long spos=writeHeader(name);		
		int len=vec.size();

		try {
			writeInt(len,endian);
	
			for(int i=0;i<len;i++)
			{
				int temp=vec.get(i);
				writeInt(temp,endian);
			}
		}
		catch (IOException e) {
			MipavUtil.displayError("In write(String name, Vector<Integer> vec) IOException = " + e);
			System.exit(-1);
		}

		writeSize(spos);
	}
	
	

	public void read(Dytex dt) {
		readHeader("Dytex");
		read(dt.dtopt);
		read("Ymean", dt.Ymean);
		read("A", dt.A);
		read("C", dt.C);
		read("mu0", dt.mu0);
		read(dt.R);
		read(dt.Q);
		read(dt.S0);
		int vrows[] = new int[1];
		read("vrows", vrows);
		dt.vrows = vrows[0];
		int vcols[] = new int[1];
		read("vrows", vcols);
		dt.vcols = vcols[0];
	}
	
	/*!
	 * \brief
	 * writes a Dytex object with default name "Dytex".
	 * 
	 * \param dt
	 * value of Dytex.
	 *  
	 * \see
	 * Bufferer::read(Dytex &dt) | Bufferer::write(const DytexMix &dtm)
	 */
	public void write(Dytex dt)
	{
		long spos=writeHeader("Dytex");
		write(dt.dtopt); 
		write("Ymean",dt.Ymean);
		write("A",dt.A);
		write("C",dt.C);
		write("mu0",dt.mu0);
		write(dt.R);
		write(dt.Q);
		write(dt.S0);
		write("vrows",dt.vrows);
		write("vrows",dt.vcols);
		writeSize(spos);
	}

	public void read(CovMatrix cm) {
		readHeader("CovMatrix");
		int n[] = new int[1];
		read("n", n);
		cm.n = n[0];
		byte temp[] = new byte[1];
		read("covopt", temp);
		cm.covopt = getCov_type(temp[0]);

		read("regopt", temp);
		cm.regopt = getCov_reg_type(temp[0]);

		double regval[] = new double[1];
		read("regval", regval);
		cm.regval = regval[0];
		read("mtx", cm.mtx);
	}
	
	/*!
	 * \brief
	 * writes a CovMatrix object with default name "CovMatrix".
	 * 
	 * \param cm
	 * value of CovMatrix.
	 *  
	 * \see
	 * Bufferer::read(CovMatrix &cm)
	 */
	public void write(CovMatrix cm)
	{
		long spos=writeHeader("CovMatrix");

		write("n",cm.n);
		write("covopt",(byte)cm.covopt.ordinal());
		write("regopt",(byte)cm.regopt.ordinal());
		write("regval",cm.regval);
		write("mtx",cm.mtx);

		writeSize(spos);
	}

	public void read(String name, double val[]) {
		readHeader(name);
		try {
			val[0] = getDouble(endian);
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
	}
	
	/*!
	 * \brief
	 * writes a 8-byte double object.
	 * 
	 * \param name
	 * name of the object.
	 * 
	 * \param val
	 * value of double.
	 *  
	 * \see
	 * Bufferer::read(string name,double &val)
	 */
	public void write(String name,double val)
	{
		long spos=writeHeader(name);
		try {
		    writeDouble(val,endian);
		}
		catch (IOException e) {
			MipavUtil.displayError("In write(String name,double val) IOException " + e);
			System.exit(-1);
		}
		writeSize(spos);
	}
	
	/**
     * Writes a double as eight bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeDouble(final double data, final boolean bigEndian) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, bigEndian);
    }
    
    /**
     * Writes a long as eight bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeLong(final long data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteLongBuffer[0] = (byte) (data >>> 56);
            byteLongBuffer[1] = (byte) (data >>> 48);
            byteLongBuffer[2] = (byte) (data >>> 40);
            byteLongBuffer[3] = (byte) (data >>> 32);
            byteLongBuffer[4] = (byte) (data >>> 24);
            byteLongBuffer[5] = (byte) (data >>> 16);
            byteLongBuffer[6] = (byte) (data >>> 8);
            byteLongBuffer[7] = (byte) (data & 0xff);
        } else {
            byteLongBuffer[0] = (byte) (data & 0xff);
            byteLongBuffer[1] = (byte) (data >>> 8);
            byteLongBuffer[2] = (byte) (data >>> 16);
            byteLongBuffer[3] = (byte) (data >>> 24);
            byteLongBuffer[4] = (byte) (data >>> 32);
            byteLongBuffer[5] = (byte) (data >>> 40);
            byteLongBuffer[6] = (byte) (data >>> 48);
            byteLongBuffer[7] = (byte) (data >>> 56);
        }

        raFile.write(byteLongBuffer);
    }

	public void read(String name, Mat mtx) {
		boolean isempty[] = new boolean[1];

		Point3i dims = new Point3i(1, 1, 1);
		readHeader(name);
		read("isempty", isempty);
		if (isempty[0]) // empty
		{
			return;
		}

		int type = 0;
		// int els=0;
		try {
			type = getInt(endian);

			read(dims);
			// els = getInt(endian);
			getInt(endian);

			int sz[] = { dims.x, dims.y, dims.z };
			if (dims.x == 1)
				mtx.create(dims.y, dims.z, type);
			else
				mtx.create(3, sz, type);

			// reading data
			double tmpD;
			byte tmpU;
			if (dims.x == 1) {
				for (int i = 0; i < dims.y; i++) {
					for (int j = 0; j < dims.z; j++) {
						switch (type) {
						case CV_64F:
							tmpD = getDouble(endian);
							mtx.double2D[i][j] = tmpD;
							break;
						case CV_8U:
							tmpU = raFile.readByte();
							mtx.byte2D[i][j] = tmpU;
							break;
						case CV_64FC3:
							tmpD = getDouble(endian);
							mtx.double2DC[i][j][0] = tmpD;
							tmpD = getDouble(endian);
							mtx.double2DC[i][j][1] = tmpD;
							tmpD = getDouble(endian);
							mtx.double2DC[i][j][2] = tmpD;
							break;
						default:
							MipavUtil.displayError("type not handled yet");
						}
					}
				}
			} else {
				for (int i = 0; i < dims.x; i++) {
					for (int j = 0; j < dims.y; j++) {
						for (int k = 0; k < dims.z; k++) {
							switch (type) {
							case CV_64F:
								tmpD = getDouble(endian);
								mtx.double3D[i][j][k] = tmpD;
								break;
							case CV_8U:
								tmpU = raFile.readByte();
								mtx.byte3D[i][j][k] = tmpU;
								break;

							case CV_64FC3:
								tmpD = getDouble(endian);
								mtx.double3DC[i][j][k][0] = tmpD;
								tmpD = getDouble(endian);
								mtx.double3DC[i][j][k][1] = tmpD;
								tmpD = getDouble(endian);
								mtx.double3DC[i][j][k][2] = tmpD;
								break;
							default:
								MipavUtil.displayError("type not handled yet");
							}
						}
					}
				}
			}
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

	}
	
	/*!
	 * \brief
	 * writes a openCV Mat object.
	 * 
	 * \param name
	 * name of the object.
	 * 
	 * 2 & 3 dimensional mat with CV_64F, CV_8U and CV_64FC3 types are supported
	 *
	 * \param mtx
	 * mat object to write.
	 *  
	 * \see
	 * Bufferer::read(string name,Mat &mtx)
	 */
	public void write(String name,Mat mtx)
	{
		long spos=writeHeader(name);

		boolean isempty;
		if((mtx == null) || (total(mtx) == 0.0))
		{
			isempty=true;				
			write("isempty",isempty);
			writeSize(spos);
			return;
		}
		isempty=false;
		write("isempty",isempty);
		if((mtx.dims<2)||(mtx.dims>3)) {
			MipavUtil.displayError("mtx.dims = " + mtx.dims + " instead of the required 2 or 3");
			System.exit(-1);
		}
		if(!( (mtx.type==CV_64F) || (mtx.type==CV_8U) || (mtx.type==CV_64FC3))) {
			MipavUtil.displayError("mtx.type = " + mtx.type + " inastead of the required CV_64F, CV_8U, or CV_64FC3");
			System.exit(-1);
		}
		Point3i dims = new Point3i(1,1,1);
		
		if(mtx.dims==2)
		{
			dims.y=mtx.rows;
			dims.z=mtx.cols;
		}
		else 
		{
			dims.x=mtx.size[0];
			dims.y=mtx.size[1];
			dims.z=mtx.size[2];
		}
		int type=mtx.type;	
		int els=mtx.elemSize;	
		
		try {
			writeInt(type,endian);	
			write(dims);
			writeInt(els,endian);
			
			//writing data
			double tmpD;
			byte tmpU;
			if(mtx.dims==2)
			{
				for(int i=0;i<mtx.rows;i++)
				{
					for(int j=0;j<mtx.cols;j++)
					{
						switch(type)
						{
						case CV_64F:
							tmpD=mtx.double2D[i][j];				
							writeDouble(tmpD,endian);				
							break;
						case CV_8U:
							tmpU=mtx.byte2D[i][j];				
							raFile.writeByte(tmpU);				
							break;
					    case CV_64FC3:
							tmpD=mtx.double2DC[i][j][0];				
							writeDouble(tmpD,endian);	
							tmpD=mtx.double2DC[i][j][1];
							writeDouble(tmpD,endian);	
							tmpD=mtx.double2DC[i][j][2];
							writeDouble(tmpD,endian);								
							break;
						default:
							MipavUtil.displayError("type not handled yet in write(String,Mat)");
							System.exit(-1);
						}
					}
				}
			}
			else
			{
				for(int i=0;i<dims.x;i++)
				{
					for(int j=0;j<dims.y;j++)
					{
						for(int k=0;k<dims.z;k++)
						{
							switch(type)
							{
							case CV_64F:
								tmpD=mtx.double3D[i][j][k];				
								writeDouble(tmpD,endian);				
								break;
							case CV_8U:
								tmpU=mtx.byte3D[i][j][k];				
								raFile.writeByte(tmpU);				
								break;
	
							case CV_64FC3:
								tmpD=mtx.double3DC[i][j][k][0];				
								writeDouble(tmpD,endian);	
								tmpD=mtx.double3DC[i][j][k][1];
								writeDouble(tmpD,endian);	
								tmpD=mtx.double3DC[i][j][k][2];
								writeDouble(tmpD,endian);									
								break;
							default:
								MipavUtil.displayError("type not handled yet in write(String,Mat)");
								System.exit(-1);
							}
						}
					}
				}
			}
		}
		catch (IOException e) {
		    MipavUtil.displayError("In write(String,Mat) IOException " + e);
		    System.exit(-1);
		}
		writeSize(spos);
	}

	public void read(Point3i p) {
		readHeader("Point3i");
		int x[] = new int[1];
		int y[] = new int[1];
		int z[] = new int[1];
		read("x", x);
		p.x = x[0];
		read("y", y);
		p.y = y[0];
		read("z", z);
		p.z = z[0];
	}
	
	/*!
	 * \brief
	 * writes a Point3i object with default name "Point3i".
	 * 
	 * \param p
	 * value of Point3i.
	 *  
	 * \see
	 * Bufferer::read(Point3i &p)
	 */
	public void write(Point3i p)
	{	
		long spos=writeHeader("Point3i");	
		write("x",p.x);
		write("y",p.y);
		write("z",p.z);
		writeSize(spos);
	}

	public void read(String name, boolean val[]) {
		byte b = 0;
		readHeader(name);
		try {
			b = raFile.readByte();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		if (b == 0) {
			val[0] = false;
		} else {
			val[0] = true;
		}
		return;
	}
	
	/*!
	 * \brief
	 * writes a 1-byte bool object.
	 * 
	 * \param name
	 * name of the object.
	 * 
	 * \param val
	 * value of bool.
	 *  
	 * \see
	 * Bufferer::read(string name,bool &val)
	 */
	public void write(String name,boolean val)
	{
		long spos=writeHeader(name);
		try {
			if (!val) {
				raFile.writeByte(0);
			}
			else {
				raFile.writeByte(1);
			}
		}
		catch (IOException e) {
			MipavUtil.displayError("In write(String name, boolean val) IOException " + e);
			System.exit(-1);
		}
		writeSize(spos);
	}

	public void read(DytexOptions opt) {
		readHeader("DytexOptions");
		int n[] = new int[1];
		read("n", n);
		opt.n = n[0];
		int m[] = new int[1];
		read("m", m);
		opt.m = m[0];
		byte temp[] = new byte[1];
		read("Ropt", temp);
		opt.Ropt = getCov_type(temp[0]);
		read("Sopt", temp);
		opt.Sopt = getCov_type(temp[0]);
		read("Yopt", temp);
		opt.Yopt = getYmean_type(temp[0]);
	}
	
	/*!
	 * \brief
	 * writes a DytexOptions object with default name "DytexOptions".
	 * 
	 * \param opt
	 * value of DytexOptions.
	 *  
	 * \see
	 * Bufferer::read(DytexOptions &opt) | Bufferer::write(const DytexRegOptions &opt)
	 */
	public void write(DytexOptions opt)
	{
		long spos=writeHeader("DytexOptions");
		write("n",opt.n);
		write("m",opt.m);
		write("Ropt",(byte)opt.Ropt.ordinal());
		write("Sopt",(byte)opt.Sopt.ordinal());
		write("Yopt",(byte)opt.Yopt.ordinal());
		writeSize(spos);
	}

	public void read(String name, int val[]) {
		readHeader(name);
		try {
			val[0] = getInt(endian);
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
	}
	
	/*!
	 * \brief
	 * writes a 4-byte integer object.
	 * 
	 * \param name
	 * name of the object.
	 * 
	 * \param val
	 * value of integer.
	 *  
	 * \see
	 * Bufferer::read(string name,int &val)
	 */
	public void write(String name,int val)
	{
		long spos=writeHeader(name);
		try {
		    writeInt(val,endian);
		}
		catch(IOException e) {
			MipavUtil.displayError("In write(String name, int val) IOexception " + e);
			System.exit(-1);
		}
		writeSize(spos);
	}
	
	/*!
	 * \brief
	 *  writes size of the object.
	 * 
	 * \param spos
	 * location in file where to write the size.
	 *   
	 * This function calculates and writes the size if the object from current location of file pointer and spos.
	 *  
	 * \see
	 * Bufferer::writeHeader(const string &str)
	 */
	public void writeSize(long spos)
	{
		try {
			long epos=raFile.getFilePointer();
			int size=(int)(epos-spos-4);
			raFile.seek(spos);
			writeInt(size,endian);
			raFile.seek(epos);  //REST BACK
		}
		catch(IOException e) {
			MipavUtil.displayError("In writeSize IOException " + e);
			System.exit(-1);
		}
	}

	public void read(String name, byte val[]) {
		readHeader(name);
		try {
			val[0] = raFile.readByte();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

	}
	
	/*!
	 * \brief
	 * writes a 1-byte char object.
	 * 
	 * \param name
	 * name of the object.
	 * 
	 * \param val
	 * value of char.
	 *  
	 * \see
	 * Bufferer::read(string name,unsigned char &val)
	 */
	public void write(String name,byte val)
	{
		long spos=writeHeader(name);
		try {
		    raFile.writeByte(val);
		}
		catch (IOException e) {
			MipavUtil.displayError("In write(String name,byte val) IOExcetpion " + e);
			System.exit(-1);
		}
		writeSize(spos);
	}

	public void readHeader(String str) {
		int i;
		long fileLength = 0;
		byte b = 0;
		String tstr;
		// match header
		byte tempB[] = new byte[100];
		try {
			fileLength = raFile.length();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		// The original source code skips the delimiter,
		// but this code reads the delimiter
		// so there is no need to skip one character
		for (i = 0; i < Math.min(99, fileLength); i++) {
			try {
				b = raFile.readByte();
			} catch (IOException e) {
				MipavUtil.displayError(e + " ");
				System.exit(-1);
			}
			if (b != 0) {
				tempB[i] = b;
			} else {
				break;
			}
		}
		tempB[i] = 0;
		tstr = new String(tempB, 0, i);
		if ((tstr == null) || (tstr.length() == 0)) {
			MipavUtil.displayError("No header string found");
			System.exit(-1);
		} else if (!str.equals(tstr)) {
			MipavUtil.displayError("Header string = " + tstr + " instead of the required " + str);
			System.exit(-1);
		}

		// match version
		for (i = 0; i < 100; i++) {
			tempB[i] = 0;
		}
		for (i = 0; i < Math.min(99, fileLength); i++) {
			try {
				b = raFile.readByte();
			} catch (IOException e) {
				MipavUtil.displayError(e + " ");
				System.exit(-1);
			}
			if (b != 0) {
				tempB[i] = b;
			} else {
				break;
			}
		}
		tempB[i] = 0;
		tstr = new String(tempB, 0, i);
		if ((tstr == null) || (tstr.length() == 0)) {
			MipavUtil.displayError("No version string found");
			System.exit(-1);
		} else if (!tstr.equals("1.0")) {
			MipavUtil.displayError("Version string = " + tstr + " instead of the required 1.0");
			System.exit(-1);
		}

		// read/skip size of the type
		// int size;
		try {
			// size = getInt(endian);
			getInt(endian);
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		// System.out.println("size = " + size);
	}
	
	/*!
	 * \brief
	 * This function will Write a header for objects before writing their data.
	 * 
	 * \param str
	 * name of the object to follow.
	 * 
	 * \returns
	 * location in the file where to write the size of the object after counting its bytes at the end of writing process.
	 * 
	 * header consist of name of the object (a string), verion of the object (a string) and size of the objects (in bytes)
	 * 
	 * \see
	 * Bufferer::readHeader(const string &str)
	 */
	public long writeHeader(String str)
	{
		byte buf[];
		int zero = 0;
		long spos = 0;
		try {
			raFile.write(str.getBytes());
			raFile.writeByte(zero);
			raFile.write((new String("1.0")).getBytes());
			raFile.writeByte(zero);
			spos=raFile.getFilePointer();
			int dsize=0;
			writeInt(dsize,endian); //Write dummy size
		}
		catch (IOException e) {
			MipavUtil.displayError("In writeHeader IOException " + e);
			System.exit(-1);
		}
		return spos;
	}
	
	/**
     * Writes an int as four bytes to a file.
     * 
     * @param data Data to be written to file.
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @exception IOException if there is an error writing the file
     */
    public final void writeInt(final int data, final boolean bigEndian) throws IOException {

        if (bigEndian) {
            byteIntBuffer[0] = (byte) (data >>> 24);
            byteIntBuffer[1] = (byte) (data >>> 16);
            byteIntBuffer[2] = (byte) (data >>> 8);
            byteIntBuffer[3] = (byte) (data & 0xff);
        } else {
            byteIntBuffer[0] = (byte) (data & 0xff);
            byteIntBuffer[1] = (byte) (data >>> 8);
            byteIntBuffer[2] = (byte) (data >>> 16);
            byteIntBuffer[3] = (byte) (data >>> 24);
        }

        raFile.write(byteIntBuffer);
    }

	private void resize(Vector<Mat> matVec, int n) {
		if (matVec.size() < n) {
			while (matVec.size() < n) {
				matVec.add(new Mat());
			}
		} else if (matVec.size() > n) {
			while (matVec.size() > n) {
				matVec.remove(matVec.size() - 1);
			}
		}

	}

	private void resizeDouble(Vector<Double> doubleVec, int n) {
		if (doubleVec.size() < n) {
			while (doubleVec.size() < n) {
				doubleVec.add(new Double(0.0));
			}
		} else if (doubleVec.size() > n) {
			while (doubleVec.size() > n) {
				doubleVec.remove(doubleVec.size() - 1);
			}
		}

	}

	/**
	 * Reads four signed bytes from file.
	 * 
	 * @param bigEndian
	 *            <code>true</code> indicates big endian byte order,
	 *            <code>false</code> indicates little endian.
	 * 
	 * @return The value of the integer read from the file.
	 * 
	 * @exception IOException
	 *                if there is an error reading the file
	 */
	public final int getInt(final boolean bigEndian) throws IOException {

		raFile.readFully(byteIntBuffer);

		if (bigEndian) {
			return (((byteIntBuffer[0] & 0xff) << 24) | ((byteIntBuffer[1] & 0xff) << 16)
					| ((byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big
																						// Endian
		} else {
			return (((byteIntBuffer[3] & 0xff) << 24) | ((byteIntBuffer[2] & 0xff) << 16)
					| ((byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
		}
	}

	/**
	 * Reads eight unsigned bytes from file.
	 * 
	 * @param bigEndian
	 *            <code>true</code> indicates big endian byte order,
	 *            <code>false</code> indicates little endian.
	 * 
	 * @return The value of the double read from the file.
	 * 
	 * @exception IOException
	 *                if there is an error reading the file
	 */
	public final double getDouble(final boolean bigEndian) throws IOException {
		raFile.readFully(byteDoubleBuffer);

		long tmpLong;

		if (bigEndian) {
			tmpLong = (((byteDoubleBuffer[0] & 0xffL) << 56) | ((byteDoubleBuffer[1] & 0xffL) << 48)
					| ((byteDoubleBuffer[2] & 0xffL) << 40) | ((byteDoubleBuffer[3] & 0xffL) << 32)
					| ((byteDoubleBuffer[4] & 0xffL) << 24) | ((byteDoubleBuffer[5] & 0xffL) << 16)
					| ((byteDoubleBuffer[6] & 0xffL) << 8) | (byteDoubleBuffer[7] & 0xffL));

			return (Double.longBitsToDouble(tmpLong));
		} else {
			tmpLong = (((byteDoubleBuffer[7] & 0xffL) << 56) | ((byteDoubleBuffer[6] & 0xffL) << 48)
					| ((byteDoubleBuffer[5] & 0xffL) << 40) | ((byteDoubleBuffer[4] & 0xffL) << 32)
					| ((byteDoubleBuffer[3] & 0xffL) << 24) | ((byteDoubleBuffer[2] & 0xffL) << 16)
					| ((byteDoubleBuffer[1] & 0xffL) << 8) | (byteDoubleBuffer[0] & 0xffL));

			return (Double.longBitsToDouble(tmpLong));
		}
	}
	
	/*!
     * \brief
     * main function for video segmentation using Dynamic textures.
     * 
     * \param argc
     * number of input arguments.
     * 
     * \param argv
     * only one argument i.e the path of the parameter file.
     * 
     */
    // paramsFile "C:/temporal texture/libdt-v1.0/libdt-v1.0/testdata/vidsegm/ocean-fire-noborder/params.txt";
    // paramsFile "C:/temporal texture/libdt-v1.0/libdt-v1.0/testdata/vidsegm/riversteamfire/params.txt";
    public int test_videoSegm(String paramFile)
    {
    	VidSeqSegmParams params = new VidSeqSegmParams();
    		
    	if(paramFile != null)
    	{
    		params.updateFromFile(paramFile);
    	}


    	//Train the DTM using traing video	
    	String trainVidPath = params.vidpath;

    	String trainDtmPath=params.savepath+params.savename+"/train.dtm";

        File file = new File(params.savepath); // param.savepath ends in File.separatorChar
        if (!file.exists()) {
            file.mkdir();
        }

    	File file2 = new File(params.savepath+params.savename+File.separatorChar);
    	if (!file2.exists()) {
    		file2.mkdir();
    	}
    	
    	File file3 = new File(trainDtmPath);
        if (!file3.exists())
    	{
        	try {
        	    file3.createNewFile();
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("In test_videoSegm file3.createNewFile() IOException " + e);
        		System.exit(-1);
        	}
        	try {
                raFile = new RandomAccessFile(file3, "rw");
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("In test_videoSegm raFile = new RandomAccessFile(file3, \"rw\") IOException " + e);
        		System.exit(-1);
        	}

            
    		//Do the training
    		VidSegm tvs = new VidSegm(params.winxy,params.winz,params.stepxy,params.stepz,params.ntype,params.bkvar,params.K,
    				params.n,params.reg,params.nfrm,params.bkvarf);		
    		learnDTM(tvs,trainVidPath,false);
    		//Bufferer buf(trainDtmPath,fstream::out | fstream::binary);
    		write(tvs.dtm);		
    		//buf.close();
    		try {
    		    raFile.close();
    		}
    		catch (IOException e) {
        		MipavUtil.displayError("In test_videoSegm raFile.close() IOException " + e);
        		System.exit(-1);
        	}
    	}

    	
    	Vector<String> paths = new Vector<String>();	
    	for(int i=0;i<params.totalvids;i++)
    	{	
    		paths.add(String.format(params.vidpath,i)+null);		
    	}
    	
    	
    	String vidPath2=params.savepath+params.savename+"/";
    	
    	
    	//load existing	dtm	
    	DytexMix dtm = new DytexMix();
    	File file4 = new File(trainDtmPath);
    	try {
            raFile = new RandomAccessFile(file4, "r");
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("In test_videoSegm raFile = new RandomAccessFile(file4, \"r\") IOException " + e);
    		System.exit(-1);
    	}
    	read(dtm);
    	try {
		    raFile.close();
		}
		catch (IOException e) {
    		MipavUtil.displayError("In test_videoSegm raFile.close() IOException " + e);
    		System.exit(-1);
    	}
    	for(int p=0;p<dtm.dt.size();p++)
    	{
    		dtm.dt.get(p).vrows=params.winxy;
    		dtm.dt.get(p).vcols=params.winxy;
    	}

    	VidSegm vs = new VidSegm(params.winxy,params.winz,params.stepxy,params.stepz,params.ntype,params.bkvar,params.K,params.n,params.reg,params.nfrm,params.bkvarf);		
    	vs.dtm=dtm;
    	segmentVideoSequence(vs, paths,params.stepxy2,params.stepz2,params.filtxy,params.filtz,vidPath2);

    	return 1;
    }


	

	class VidSeqSegmParams {
		public int inputType;
		public String vidpath;
		public int trainvidnum;
		public int totalvids;
		public int winxy;
		public int winz;
		public int stepxy;
		public int stepz;
		public norm_type ntype;
		public double bkvar;
		public int bkvarf;
		public int K;
		public int n;
		public double reg;
		public int nfrm;
		public int stepxy2;
		public int stepz2;
		public int filtxy;
		public int filtz;
		public boolean fsave;
		public String savepath;
		public String savename;

		public VidSeqSegmParams() {
			totalvids = 1;
			inputType = 0;
			vidpath = "C:/temporal texture/libdt-v1.0/libdt-v1.0/testdata/vidsegm/ocean-fire-noborder/ocean-fire-noborder.y";
			trainvidnum = 1;
			winxy = 5;
			winz = 5;
			stepxy = 4;
			stepz = 4;
			ntype = norm_type.NORM_ZM;
			bkvar = 0;
			bkvarf = 0;
			K = 2;
			n = 2;
			reg = 50;
			nfrm = 0;
			stepxy2 = 4;
			stepz2 = 4;
			filtxy = 5;
			filtz = 5;
			fsave = true;
			savepath = "C:/temporal texture/libdt-v1.0/libdt-v1.0/testdata/vidsegm/ocean-fire-noborder/";
			savename = "rslt";
		}

		/*
		 * ! \brief update segmentation parameters from params file.
		 */

		void updateFromFile(String fpath) {
			int intSave;
			ConfigFile cfg = new ConfigFile(fpath);

			if (cfg.keyExists("totalvids"))
				totalvids = cfg.getIntValueOfKey("totalvids");

			if (cfg.keyExists("trainvidnum"))
				trainvidnum = cfg.getIntValueOfKey("trainvidnum");

			if (cfg.keyExists("savepath"))
				savepath = cfg.getStringValueOfKey("savepath");

			if (cfg.keyExists("savename"))
				savename = cfg.getStringValueOfKey("savename");

			if (cfg.keyExists("vidpath"))
				vidpath = cfg.getStringValueOfKey("vidpath");

			if (cfg.keyExists("winxy"))
				winxy = cfg.getIntValueOfKey("winxy");

			if (cfg.keyExists("winz"))
				winz = cfg.getIntValueOfKey("winz");

			if (cfg.keyExists("stepxy"))
				stepxy = cfg.getIntValueOfKey("stepxy");

			if (cfg.keyExists("stepz"))
				stepz = cfg.getIntValueOfKey("stepz");

			if (cfg.keyExists("ntype"))
				ntype = getNorm_type(cfg.getIntValueOfKey("ntype"));

			if (cfg.keyExists("bkvar"))
				bkvar = cfg.getDoubleValueOfKey("bkvar");

			if (cfg.keyExists("bkvarf"))
				bkvarf = cfg.getIntValueOfKey("bkvarf");

			if (cfg.keyExists("K"))
				K = cfg.getIntValueOfKey("K");

			if (cfg.keyExists("n"))
				n = cfg.getIntValueOfKey("n");

			if (cfg.keyExists("reg"))
				reg = cfg.getDoubleValueOfKey("reg");

			if (cfg.keyExists("nfrm"))
				nfrm = cfg.getIntValueOfKey("nfrm");

			if (cfg.keyExists("stepxy2"))
				stepxy2 = cfg.getIntValueOfKey("stepxy2");

			if (cfg.keyExists("stepz2"))
				stepz2 = cfg.getIntValueOfKey("stepz2");

			if (cfg.keyExists("filtxy"))
				filtxy = cfg.getIntValueOfKey("filtxy");

			if (cfg.keyExists("filtz"))
				filtz = cfg.getIntValueOfKey("filtz");

			if (cfg.keyExists("fsave")) {
				intSave = cfg.getIntValueOfKey("fsave");
				if (intSave == 0) {
					fsave = false;
				} else {
					fsave = true;
				}
			}
		}

	}

	class ConfigFile {
		private HashMap<String, String> contents = new HashMap<String, String>();
		private String fName;

		void removeComment(String line) {
			int pos = line.indexOf(';');
			if (pos != -1)
				line = line.substring(0, pos);
		}

		boolean onlyWhitespace(String line) {
			line = line.trim();
			return line.isEmpty();
		}

		boolean validLine(String line) {
			String temp = line;
			while ((temp.substring(0, 1).equals(" ")) || (temp.substring(0, 1).equals("\t"))) {
				temp = temp.substring(1);
			}
			if (temp.substring(0, 1).equals("="))
				return false;

			int pos = temp.indexOf('=');
			for (int i = pos + 1; i < temp.length(); i++)
				if (!temp.substring(i, i + 1).equals(" "))
					return true;

			return false;
		}

		String extractKey(int sepPos, String line) {
			String key = line.substring(0, sepPos);
			int index1 = key.indexOf('\t');
			int index2 = key.indexOf(' ');
			int index;
			if ((index1 != -1) && (index2 != -1)) {
				index = Math.min(index1, index2);
			} else if (index1 != -1) {
				index = index1;
			} else if (index2 != -1) {
				index = index2;
			} else {
				index = -1;
			}
			if (index != -1) {
				key = key.substring(0, index);
			}
			return key;
		}

		String extractValue(int sepPos, String line) {
			String value = line.substring(sepPos + 1);
			while ((value.substring(0, 1).equals(" ")) || (value.substring(0, 1).equals("\t"))) {
				value = value.substring(1);
			}
			int index1 = value.lastIndexOf('\t');
			int index2 = value.lastIndexOf(' ');
			int index;
			if ((index1 != -1) && (index2 != -1)) {
				index = Math.max(index1, index2);
			} else if (index1 != -1) {
				index = index1;
			} else if (index2 != -1) {
				index = index2;
			} else {
				index = -1;
			}
			if (index != -1) {
				value = value.substring(0, index + 1);
			}
			return value;
		}

		void extractContents(String line) {
			String temp = line;
			while ((temp.substring(0, 1).equals(" ")) || (temp.substring(0, 1).equals("\t"))) {
				temp = temp.substring(1);
			}
			int sepPos = temp.indexOf('=');

			String key, value;
			key = extractKey(sepPos, temp);
			value = extractValue(sepPos, temp);

			if (!contents.containsKey(key))
				contents.put(key, value);
			else {
				MipavUtil.displayError("CFG: Can only have unique key names!");
				System.exit(-1);
			}
		}

		void parseLine(String line, int lineNo) {
			int pos = line.indexOf('=');
			if (pos == -1) {
				MipavUtil.displayError("CFG: Couldn't find = separator on line: " + lineNo);
				System.exit(-1);
			}

			if (!validLine(line)) {
				MipavUtil.displayError("CFG: Bad format for line: " + lineNo);
				System.exit(-1);
			}

			extractContents(line);
		}

		private void ExtractKeys() {
			File file = new File(fName);
			try {
				raFile = new RandomAccessFile(file, "r");
			} catch (FileNotFoundException e) {
				MipavUtil.displayError("CFG: File " + fName + " couldn't be found!\n");
				System.exit(-1);
			}

			int lineNo = 0;
			try {
				long fileLength = raFile.length();
				while (raFile.getFilePointer() < fileLength - 1) {
					lineNo++;
					String temp = raFile.readLine();

					if (temp.isEmpty())
						continue;

					removeComment(temp);
					if (onlyWhitespace(temp))
						continue;

					parseLine(temp, lineNo);
				}

				raFile.close();
			} catch (IOException e) {
				System.err.println("In extractKeys " + e);
				System.exit(-1);
			}
		}

		public ConfigFile(String fName) {
			this.fName = fName;
			ExtractKeys();
		}

		boolean keyExists(String key) {
			return contents.containsKey(key);
		}

		int getIntValueOfKey(String key) {
			String value = contents.get(key);
			if (value != null) {
				return Integer.valueOf(value).intValue();
			} else {
				return 0;
			}
		}

		double getDoubleValueOfKey(String key) {
			String value = contents.get(key);
			if (value != null) {
				return Double.valueOf(value).doubleValue();
			} else {
				return 0.0;
			}
		}

		String getStringValueOfKey(String key) {
			return contents.get(key);
		}
	}

	/** Normalization mode for each patch. */
	enum norm_type {
		NORM_NONE(0),
		/** < no normalization. */
		NORM_ZM(1),
		/** < each pixel is zero-mean in time. */
		NORM_ZMUV(2),
		/**
		 * < each pixel is zero-mean in time, and all pixels have unit-variance.
		 */
		NORM_ILLEGAL(3);

		public final int norm_code;

		norm_type(int norm_code) {
			this.norm_code = norm_code;
		}
	}

	public norm_type getNorm_type(int norm_code) {
		if (norm_code == 0) {
			return norm_type.NORM_NONE;
		} else if (norm_code == 1) {
			return norm_type.NORM_ZM;
		} else if (norm_code == 2) {
			return norm_type.NORM_ZMUV;
		} else {
			MipavUtil.displayError("Illegal number = " + norm_code + " for norm_type");
			return norm_type.NORM_ILLEGAL;
		}
	}

	/**
	 * Class for specifying spatio-temporal patches (cubes). \sa PatchExtractor,
	 * PatchBatchExtractor
	 */
	class PatchOptions {

		// patch options
		public Point3i win;
		/** < patch size (x, y, z). */
		public Point3i step;
		/** < step size between patches (x, y, z). */
		public norm_type normopt;
		/** < normalization method. */
		public double minvar;
		/** < minimum variance. */
		public int minvarf;

		/** < frames to skip in checking variance. */

		PatchOptions(int winxy, int winz, int stepxy, int stepz, norm_type normopt, double minvar, int minvarf) {
			win = new Point3i();
			win.x = winxy;
			win.y = winxy;
			win.z = winz;
			step = new Point3i();
			step.x = stepxy;
			step.y = stepxy;
			step.z = step.z;
			this.normopt = normopt;
			this.minvar = minvar;
			this.minvarf = minvarf;
			checkValues();
		}

		void checkValues() {
			if (win.x <= 0) {
				MipavUtil.displayError("win.x <= 0");
				System.exit(-1);
			}
			if (win.y <= 0) {
				MipavUtil.displayError("win.y <= 0");
				System.exit(-1);
			}
			if (win.z <= 0) {
				MipavUtil.displayError("win.z <= 0");
				System.exit(-1);
			}
			if (step.x <= 0) {
				MipavUtil.displayError("step.x <= 0");
				System.exit(-1);
			}
			if (step.y <= 0) {
				MipavUtil.displayError("step.y <= 0");
				System.exit(-1);
			}
			if (step.z <= 0) {
				MipavUtil.displayError("step.z <= 0");
				System.exit(-1);
			}
		}

	}

	/*
	 * ! \brief Video segmentatin class.
	 * 
	 * Can be used to segment a video or video sequence using dynamic textures.
	 * 
	 * 
	 * \see DytexMix.
	 */
	class VidSegm {
		// variables

		/*
		 * ! \brief dynamix texture mixture learned from training video.
		 */
		public DytexMix dtm;
		/*
		 * ! \brief initial training video segmentation.
		 */
		public Mat initSegm = new Mat();
		/*
		 * ! \brief patch options for dtm learning and segmentation.
		 */
		public PatchOptions popt;
		/*
		 * ! \brief number of segments.
		 */
		public int K;
		/*
		 * ! \brief number of states.
		 */
		public int n;
		/*
		 * ! \brief regularization value for the dtm.
		 */
		public double regV;
		/*
		 * ! \brief number of frames to use for the training video.
		 */
		public int nfrm;

		/*
		 * ! \brief offset to center of patch.
		 */
		public Point3i coff;
		/*
		 * ! \brief all x-locations on step grid.
		 */
		public Vector<Integer> allx;
		/*
		 * ! \brief all y-locations on step grid.
		 */
		public Vector<Integer> ally;
		/*
		 * ! \brief all z-locations on step grid.
		 */
		public Vector<Integer> allz;
		/*
		 * ! \brief size of the video.
		 */
		public Point3i vidsize;
		/*
		 * ! \brief coordinates of top-left of all patches.
		 */
		public Vector<Point3i> locall;
		/*
		 * ! \brief mask for all patches.
		 */
		public Vector<Boolean> locall_mask;
		/*
		 * ! \brief classes for all valid patches.
		 */
		Vector<Integer> allclasses;

		/*
		 * ! \brief initialize a video segmentor object.
		 * 
		 * \param winxy xy patch size.
		 * 
		 * \param winz z patch size.
		 * 
		 * \param stepxy xy step size.
		 * 
		 * \param stepz z step size.
		 * 
		 * \param nt patch normalization tyep.
		 * 
		 * \param bkv minimum background variance.
		 * 
		 * \param K number of segments.
		 * 
		 * \param n number of states.
		 * 
		 * \param reg dtm regularization value.
		 * 
		 * \param nf number of frames to use for training.
		 * 
		 * \param bkvf number of frames to look for variance test (make patches
		 * more strict).
		 * 
		 * \see learnDTM.
		 */
		public VidSegm(int winxy, int winz, int stepxy, int stepz, norm_type nt, double bkv, int K, int n, double reg,
				int nf, int bkvf) {
			popt = new PatchOptions(winxy, winz, stepxy, stepz, nt, bkv, bkvf);
			dtm = new DytexMix(new DytexOptions(n, winxy * winxy, cov_type.COV_IID, cov_type.COV_DIAG,
					nt == norm_type.NORM_NONE ? Ymean_type.NONZERO_YMEAN : Ymean_type.ZERO_YMEAN));
			this.K = K;
			this.regV = reg;
			this.n = n;
			nfrm = nf;
		}
	}

	/*
	 * ! \brief learn the dtm using training video.
	 * 
	 * \param vpath training video dat file path.
	 * 
	 * \param doseg do the initial segmentation or not.
	 * 
	 * Can be used for the initial segmentation of the training video.
	 * 
	 * 
	 * \see segmentVideoSequence.
	 */
	void learnDTM(VidSegm tvs, String vpath, boolean dosegm) {
		int i,j;
		// load training video
		Mat smask[];
		Mat img[] = loaddat(vpath, "t");

		// Get the required video frames
		if (tvs.nfrm != 0) {
			img = subvid(img, 0, tvs.nfrm);
		}

		System.out.println(
				"Training video size frames: " + img.length + " rows: " + img[0].size[0] + " cols: " + img[0].size[1]);

		// Get patches
		Range box_z = new Range();
		box_z.all = true;
		Range box_y = new Range();
		box_y.all = true;
		Range box_x = new Range();
		box_x.all = true;
		PatchBatchExtractor pbe = new PatchBatchExtractor(img, tvs.popt, box_z, box_y, box_x);
		System.out.println("Found Patches size: " + pbe.patches.size());

		// Learn the mixture
		DytexRegOptions ropt = new DytexRegOptions(cov_reg_type.COV_REG_MIN, tvs.regV, cov_reg_type.COV_REG_MIN,
				tvs.regV, cov_reg_type.COV_REG_MIN, tvs.regV);
		EMOptions emopt = new EMOptions(tvs.K, ropt, 1e-4, Verbose_mode.COMPACT);

		long s_time = System.currentTimeMillis();
		learnWithSplitting(tvs.dtm, pbe.patches, emopt);
		long e_time = System.currentTimeMillis() - s_time;
		System.out.println("Total Time Taken :" + (double) e_time / 1000.0 + " seconds");

		if(dosegm) { //Do the initial segmentation Mat
		    smask=segm_mask(pbe, tvs.dtm.classes);
		    smask=maxvotefilt(smask,5,5,5,tvs.K);
		    colorMask(img,smask,tvs.initSegm,1); 
		}

	}

	// function to load dat file
	Mat[] loaddat(String filename, String opt) {
		int i, r, c;
		Mat output[]; // output matrix
		Mat chframe[]; // Single frame channels

		// loading descriptor from file
		DatDescriptor desc = loaddatinfo(filename, checkOptionInStr(opt, "d"));

		// parameters for algorithm
		int rows = (int) desc.dimensions.get(0).doubleValue();
		int cols = (int) desc.dimensions.get(1).doubleValue();
		int chns = (int) desc.channels;
		int tframes = (int) desc.data_sets;
		int elbytes = 0;
		// int sz[]={tframes,rows,cols};

		chframe = new Mat[chns];
		for (i = 0; i < chns; i++) {
			chframe[i] = new Mat();
		}

		if (desc.data_type.equals("unsigned_1")) {
			elbytes = 1;
			for (i = 0; i < chns; i++)
				chframe[i].create(rows, cols, CV_8UC1);
		} else {
			elbytes = 8;
			for (i = 0; i < chns; i++)
				chframe[i].create(rows, cols, CV_64FC1);
		}

		output = new Mat[tframes];
		int sz2D[] = new int[] { rows, cols };
		for (i = 0; i < tframes; i++) {
			output[i] = new Mat();
		}
		if ((desc.data_type.equals("unsigned_1")) && (checkOptionInStr(opt, "t") != 0)) {
			for (i = 0; i < tframes; i++) {
				output[i].create(2, sz2D, CV_8UC, chns);
			}
		} else {
			for (i = 0; i < tframes; i++) {
				output[i].create(2, sz2D, CV_64FC, chns);
			}
		}

		// reading data from file
		filename = filename + "/data";
		File ifile = new File(filename);
		try {
			raFile = new RandomAccessFile(ifile, "r");
		} catch (FileNotFoundException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		// read data from dat file

		try {
			raFile.seek(0L);
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}
		byte buf[] = new byte[rows * cols * elbytes];
		for (i = 0; i < desc.data_sets; i++) // for all frames
		{
			for (int j = 0; j < chns; j++) // for all channels
			{
				// ifile.read ((char*)chframe[j].data, rows*cols*elbytes);
				// //read one channel data from file
				try {
					raFile.read(buf);
				} catch (IOException e) {
					MipavUtil.displayError(e + " ");
					System.exit(-1);
				}
				if (elbytes == 1) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							chframe[j].byte2D[r][c] = buf[r * cols + c];
							if (output[0].type == CV_8UC) {
								output[i].byte2DC[r][c][j] = buf[r * cols + c];
							} else if (output[0].type == CV_64FC) {
								output[i].double2DC[r][c][j] = (double) (buf[r * cols + c] & 0xff);
							}
						}
					}
				} // if (elbytes == 1)
				else if (elbytes == 8) {
					for (r = 0; r < rows; r++) {
						for (c = 0; c < cols; c++) {
							try {
								chframe[j].double2D[r][c] = getDouble(endian);
							} catch (IOException e) {
								MipavUtil.displayError(e + " ");
								System.exit(-1);
							}
							output[i].double2DC[r][c][j] = chframe[j].double2D[r][c];
						}
					}
				}
			}

		}

		try {
			raFile.close();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

		return output;
	}

	int checkOptionInStr(String str, String opt) {
		int j = str.indexOf(opt);
		if (j >= 0)
			return 1;
		else
			return 0;
	}

	/**
	 * Structure to represent Dat file Descriptor parameters
	 */
	class DatDescriptor {
		public String data_type;
		public String byte_order;
		public double data_sets;
		public double channels;
		public Vector<Double> dimensions;
		public String feature;
		public String history;

		public DatDescriptor() {
			data_type = "";
			byte_order = "";
			data_sets = 0.0;
			channels = 0.0;
			dimensions.clear();
			feature = "";
			history = "";
		}
	}

	// equivalent to loaddatinfo matlab function
	DatDescriptor loaddatinfo(String filename, int verbose) {
		// default descriptor
		DatDescriptor desc = new DatDescriptor();
		filename = filename + "/descriptor";
		File dfile = new File(filename);
		try {
			raFile = new RandomAccessFile(dfile, "r");
		} catch (FileNotFoundException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

		// read and parse each line of the descriptor file
		while (true) {
			String line, str, key, valuestr, temp, temp2;
			Vector<String> value = new Vector<String>();

			try {
				line = raFile.readLine();
			} catch (IOException e) {
				break;
			}

			if (line == "")
				continue;

			String ans[];
			if ((line.charAt(0) == '(') && (line.charAt(line.length() - 1) == ')')) {
				str = line.substring(1, line.length() - 2);
				ans = strtok2(str, " ");
				key = ans[0];
				valuestr = ans[1];
				while (true) {
					ans = strtok2(valuestr, " ");
					temp = ans[0];
					temp2 = ans[1];
					value.add(temp);
					if (temp2.length() == 0) {
						break;
					}
					valuestr = temp2;
				}
				// save into descriptor variables
				if (key.equals("_data_type"))
					desc.data_type = value.get(0);
				else if (key.equals("_byte_order"))
					desc.byte_order = value.get(0);
				else if (key.equals("_data_sets"))
					desc.data_sets = Double.valueOf(value.get(0)).doubleValue();
				else if (key.equals("_channels"))
					desc.channels = Double.valueOf(value.get(0)).doubleValue();
				else if (key.equals("_dimensions")) {
					desc.dimensions.add(Double.valueOf(value.get(0)).doubleValue());
					desc.dimensions.add(Double.valueOf(value.get(1)).doubleValue());
				} else if (key.equals("_feature"))
					desc.feature = value.get(0);
				else
					System.err.println("loaddat:badkey: unrecognized descriptor key: " + key);
			}
		}

		try {
			raFile.close();
		} catch (IOException e) {
			MipavUtil.displayError(e + " ");
			System.exit(-1);
		}

		if (verbose != 0) {
			System.out.println("data_type:  " + desc.data_type);
			System.out.println("byte_order: " + desc.byte_order);
			System.out.println("data_sets:  " + desc.data_sets);
			System.out.println("channels:   " + desc.channels);
			System.out.println("dimensions: " + desc.dimensions.get(0) + " x " + desc.dimensions.get(1));
			System.out.println("feature:    " + desc.feature);
		}

		return desc;
	}

	String[] strtok2(String str, String del) {
		String key;
		String valuestr;
		String ans[] = new String[2];
		int ind = str.indexOf(del, 0);
		if (ind == -1) {
			key = str;
			valuestr = "";
		} else {
			key = str.substring(0, ind);
			valuestr = str.substring(ind + 1, str.length());
		}
		ans[0] = key;
		ans[1] = valuestr;
		return ans;
	}

	/**
	 * Class for extracting spatio-temporal patches (cubes) from a video (batch
	 * mode). This extractor works in "batch" mode, where all patches extracted
	 * from a video and stored. It is basically a wrapper around PatchExtractor,
	 * which stores all the obtained patches. \sa PatchExtractor, PatchOptions
	 */
	class PatchBatchExtractor {

		public PatchOptions patopt;
		/** < patch options. */
		public Vector<Point3i> loc;
		/** < coordinates of top-left of each patch. */
		public Point3i coff;
		/** < offset to center of patch. */
		public Vector<Integer> allx;
		/** < all x-locations on step grid. */
		public Vector<Integer> ally;
		/** < all y-locations on step grid. */
		public Vector<Integer> allz;
		/** < all z-locations on step grid. */
		public Vector<Mat[]> patches;
		/** < vector of all patches, corresponding to loc. */
		public Vector<Mat[]> patchesall;
		/** < vector of all patches, corresponding to loc. */
		public Range box_x;
		/** < bounding box (x) for patch locations (top-left corner). */
		public Range box_y;
		/** < bounding box (y) for patch locations. */
		public Range box_z;
		/** < bounding box (z) for patch locations. */
		public Point3i vidsize;
		/** < size of the video. */
		public Vector<Point3i> locall;
		/** < coordinates of top-left of all patches. */
		public Vector<Boolean> locall_mask;

		/** < mask for all patches. */

		/*
		 * Batch patch extraction
		 */

		// constructor
		PatchBatchExtractor(Mat vid[], PatchOptions patopt, Range box_z, Range box_y, Range box_x) {
			this.patopt = patopt;
			if (vid.length < 2) {
				MipavUtil.displayError("vid.length < 2 in PatchBatchExtractor");
				System.exit(-1);
			}
			int i;

			// remember settings
			this.box_x = box_x;
			this.box_y = box_y;
			this.box_z = box_z;
			this.vidsize = new Point3i(vid[0].size[1], vid[0].size[0], vid.length);

			// internal bounding box size and offsets
			Point3i vbox_size = null;
			Point3i vbox_off = null;
			// subvideo from internal bounding box
			Mat boxvid[];

			// check bounding box:
			if ((box_z.all) && (box_y.all) && (box_z.all)) {
				// using full video
				vbox_size = new Point3i(vid[0].size[1], vid[0].size[0], vid.length);
				vbox_off = new Point3i(0, 0, 0);

				boxvid = vid;
			} else {
				// using sub-video
				Range vbox_z = new Range();
				Range vbox_x = new Range();
				Range vbox_y = new Range();
				// adjust box to align with patch locations
				if (box_z.all) {
					vbox_off.z = 0;
					vbox_size.z = vid.length;
					vbox_z.all = true;
				} else {
					vbox_off.z = (box_z.start % patopt.step.z == 0 ? box_z.start
							: ((box_z.start / patopt.step.z) + 1) * patopt.step.z);
					vbox_size.z = box_z.end - vbox_off.z;
					vbox_z = new Range(vbox_off.z, Math.min(box_z.end + patopt.win.z, vid.length));
				}

				if (box_y.all) {
					vbox_off.y = 0;
					vbox_size.y = vid[0].size[1];
					vbox_y.all = true;
					;
				} else {
					vbox_off.y = (box_y.start % patopt.step.y == 0 ? box_y.start
							: ((box_y.start / patopt.step.y) + 1) * patopt.step.y);
					vbox_size.y = box_y.end - vbox_off.y;
					vbox_y = new Range(vbox_off.y, Math.min(box_y.end + patopt.win.y, vid[0].size[1]));
				}

				if (box_x.all) {
					vbox_off.x = 0;
					vbox_size.x = vid.length;
					vbox_x.all = true;
				} else {
					vbox_off.x = (box_x.start % patopt.step.x == 0 ? box_x.start
							: ((box_x.start / patopt.step.x) + 1) * patopt.step.x);
					vbox_size.x = box_x.end - vbox_off.x;
					vbox_x = new Range(vbox_off.x, Math.min(box_x.end + patopt.win.x, vid.length));
				}

				boxvid = subvid(vid, vbox_z, vbox_y, vbox_x);

				if (debug) {
					if (vbox_z.all) {
						System.out.println("box range.z = all");
					} else {
						System.out.println(
								"box range.z = " + vbox_z.start + " inclusive to " + vbox_z.end + " exclusive");
					}
					if (vbox_y.all) {
						System.out.println("box range.y = all");
					} else {
						System.out.println(
								"box range.y = " + vbox_y.start + " inclusive to " + vbox_y.end + " exclusive");
					}
					if (vbox_x.all) {
						System.out.println("box range.x = all");
					} else {
						System.out.println(
								"box range.x = " + vbox_x.start + " inclusive to " + vbox_x.end + " exclusive");
					}
				}
			}

			if (debug) {
				System.out.println("box size: " + vbox_size);
				System.out.println("box off:  " + vbox_off);
			}

			// create the patch extractor
			PatchExtractor pe = new PatchExtractor(patopt, boxvid[0].size[1], boxvid.length);

			// copy some info
			coff = pe.coff;
			allx = pe.allx;
			ally = pe.ally;
			int xsize = allx.size();
			int ysize = ally.size();

			// figure out possible z
			int zsize = (boxvid.length - patopt.win.z) / patopt.step.z + 1;
			allz.ensureCapacity(zsize);
			for (int z = 0; z <= boxvid.length - patopt.win.z; z += patopt.step.z) {
				allz.add(z);
			}

			// offset allx, ally, allz
			for (i = 0; i < allx.size(); i++)
				allx.set(i, allx.get(i) + vbox_off.x);
			for (i = 0; i < ally.size(); i++)
				ally.set(i, ally.get(i) + vbox_off.y);
			for (i = 0; i < allz.size(); i++)
				allz.set(i, allz.get(i) + vbox_off.z);

			// reserve space
			int total = zsize * xsize * ysize;
			loc.ensureCapacity(total);
			patches.ensureCapacity(total);
			patchesall.ensureCapacity(total);
			if (debug) {
				System.out.println("reserve total: " + total);
			}

			// loop through frames
			int frame = 0;
			Mat clip[];
			while (frame < boxvid.length) {
				// get next clip to add
				int clipz = (frame == 0 ? patopt.win.z : patopt.step.z);

				if (debug) {
					System.out.println("  @f=" + frame + ": read chunk of " + clipz + " frames");
				}

				if (frame + clipz > boxvid.length) {

					if (debug) {
						System.out.println("  not enough frames");
					}
					break;
				}
				Range ry = new Range();
				ry.all = true;
				Range rx = new Range();
				rx.all = true;
				clip = subvid(boxvid, new Range(frame, frame + clipz), ry, rx);

				if (debug) {
					dumpMatSize(clip);
				}
				// add frames
				if (!addFrames(pe, clip)) {
					// we should not get here
					MipavUtil.displayError("something wrong on addFrames(pe,clip) in PatchBatchExtractor!");
					System.exit(-1);

				} else {
					// now add to our patch vector
					Vector<Mat[]> mypat = getPatches(pe);
					for (i = 0; i < mypat.size(); i++) {
						// create a copy of the patch
						if (pe.locyx_mask.get(i) == true) // only valid patches
						{
							Mat p[] = new Mat[mypat.get(i).length];
							copyTo(mypat.get(i), p);
							patches.add(p);
							// add the location (with possible offset)
							loc.add(new Point3i(pe.locyx.get(i).x + vbox_off.x, pe.locyx.get(i).y + vbox_off.y,
									pe.locz + vbox_off.z));

						}
						// save all locations
						locall.add(new Point3i(pe.locyx.get(i).x + vbox_off.x, pe.locyx.get(i).y + vbox_off.y,
								pe.locz + vbox_off.z));
						locall_mask.add(pe.locyx_mask.get(i));

						// save all patches
						Mat p[] = new Mat[mypat.get(i).length];
						copyTo(mypat.get(i), p);
						patchesall.add(p);
					}
				}

				frame += clipz;
			}
		}

	}

	Vector<Mat[]> getPatches(PatchExtractor pe) {
		if (pe.curz < pe.patopt.win.z) {
			MipavUtil.displayError("not enough frames added yet!");
			System.exit(-1);
		}
		if ((pe.curz - pe.patopt.win.z) % pe.patopt.step.z != 0) {
			MipavUtil.displayError("getPatch at wrong time!");
			System.exit(-1);
		}

		return pe.patches;
	}

	class Range {
		public int start;
		public int end;
		public boolean all = false;

		public Range() {

		}

		public Range(int start, int end) {
			this.start = start;
			this.end = end;
		}
	}

	enum fill_type {
		FILL_NN,
		/** < use nearest-neighbor. */
		FILL_NNFILL,
		/** < use nearest-neighbor (fill in patches). */
		FILL_VOTE, /** < overlapping patches vote. */
	}

	/**
	 * Class for extracting spatio-temporal patches (cubes) from a video (online
	 * mode). The extractor works in an "online" mode, where frames are added
	 * sequentially. When enough frames are added a true flag is returned, which
	 * indicates that new patches have been formed. Normalization of patches is
	 * handled efficiently, and access is given to the current video buffer.
	 * Also included is a function for converting a set of labels corresponding
	 * to each patche to an image.
	 * 
	 * The actual patches will be in different places, depending on the
	 * normalization used: 1) norm_none --> patches are sub-videos of the buffer
	 * (vbuf); 2) norm_zm --> patches are sub-videos of the buffer (vbuf_zm); 3)
	 * norm_zmuv --> patches are local copies. Note that in some cases, the
	 * patches are just references to a sub-videos of the video buffer. Hence,
	 * it is not allowed to modify the patches.
	 * 
	 * \sa PatchOptions, PatchBatchExtractor
	 */
	class PatchExtractor {

		/**
		 * fill type for generating images from patch labels. \sa
		 * patchLabelsToImage
		 */

		public PatchOptions patopt;
		/** < options for extracting patches. */
		public int vrows, /** < number of rows in the video frame. */
				vcols;
		/** < number of columns in the video frame. */
		public Vector<Point2i> locyx = new Vector<Point2i>();
		/** < (y,x) coordinates of top-left of each patch. */
		public Vector<Boolean> locyx_mask = new Vector<Boolean>();
		/** < patch mask depending on pixel variance. */
		public int locz;
		/**
		 * < current z location of the patches (z of the first frame of the
		 * patch).
		 */
		public int curz;
		/** < z location for next frame added. */
		public int nextz;
		/** < value of curz that will form a new patch. */
		public Point3i coff;
		/** < offset to center of patch. */
		public Vector<Integer> allx = new Vector<Integer>();
		/** < all x-locations on step grid. */
		public Vector<Integer> ally = new Vector<Integer>();
		/** < all y-locations on step grid. */

		public Mat vbuf[];
		/**
		 * < video buffer to store current frames [vrows,vcols,patopt.win.z]
		 * type=OPT_MAT_TYPE.
		 */
		public Mat vbuf_zm[];
		/**
		 * < video buffer w/o mean [vrows,vcols,patopt.win.z] type=OPT_MAT_TYPE.
		 */
		public Vector<Mat[]> patches = new Vector<Mat[]>();
		/** < the set of patches. */
		public boolean flag_zm;
		/** < flag if vbuf_zm is used. */
		public Mat frame_center;
		/** < center frame of the video buffer (reference to vbuf). */
		public Mat frame_patches[];

		/** < center frames of the video buffer (reference to vbuf). */

		// there are several modes:
		// 1) norm_none --> patches are sub-videos of the buffer (vbuf)
		// 2) norm_zm --> patches are sub-videos of the buffer (vbuf_zm)
		// 3) norm_zmuv --> patches are local copies

		// constructor
		public PatchExtractor(PatchOptions patopt, int vrows, int vcols) {
			this.patopt = patopt;
			this.vrows = vrows;
			this.vcols = vcols;
			int i, j;

			if (vrows < patopt.win.y) {
				MipavUtil.displayError("vrows < patopt.win.y in PatchExtractor");
				System.exit(-1);
			}
			if (vcols < patopt.win.x) {
				MipavUtil.displayError("vcols < patopt.win.x in PatchExtractor");
				System.exit(-1);
			}

			// initialize the location structure
			getLoc();

			// initialize the buffers

			vbuf = new Mat[patopt.win.z];
			for (i = 0; i < patopt.win.z; i++) {
				vbuf[i] = new Mat(vrows, vcols, CV_64F);
			}
			if (patopt.normopt != norm_type.NORM_NONE) {
				vbuf_zm = new Mat[patopt.win.z];
				for (i = 0; i < patopt.win.z; i++) {
					vbuf_zm[i] = new Mat(vrows, vcols, CV_64F);
				}
				flag_zm = true;
			} else {
				flag_zm = false;
			}

			// initialize patches
			if (patches.size() > locyx.size()) {
				while (patches.size() > locyx.size()) {
					patches.remove(patches.size() - 1);
				}
			} else if (patches.size() < locyx.size()) {
				while (patches.size() < locyx.size()) {
					patches.add(new Mat[1]);
				}
			}
			switch (patopt.normopt) {
			case NORM_NONE:
				// setup patches on the vbuf buffer
				for (i = 0; i < locyx.size(); i++) {
					patches.set(i,
							subvid(vbuf, new Range(0, patopt.win.z),
									new Range(locyx.get(i).y, locyx.get(i).y + patopt.win.y),
									new Range(locyx.get(i).x, locyx.get(i).x + patopt.win.x)));
				}
				break;
			case NORM_ZM:
				// setup patches on the vbuf_zm buffer
				for (i = 0; i < locyx.size(); i++) {
					patches.set(i,
							subvid(vbuf_zm, new Range(0, patopt.win.z),
									new Range(locyx.get(i).y, locyx.get(i).y + patopt.win.y),
									new Range(locyx.get(i).x, locyx.get(i).x + patopt.win.x)));
				}
				break;
			case NORM_ZMUV:
				// setup patches (allocate new memory)
				for (i = 0; i < locyx.size(); i++) {
					Mat temp[] = new Mat[patopt.win.z];
					for (j = 0; j < patopt.win.z; j++) {
						temp[j] = new Mat(patopt.win.y, patopt.win.x, CV_64F);
					}
					patches.set(i, temp);
				}
				break;
			default:
				MipavUtil.displayError("bad option in PatchExtractor");
				System.exit(-1);
			}

			// reset the extractor
			reset();

			// initialize pointer to center frame
			frame_center = vbuf[coff.z];

			// initialize pointer to center chunk of frames
			// The number of frames will be the z-step size.
			// TODO: Except on the first call, which will also include the
			// beginning frames.
			int tmp = patopt.step.z / 2;
			Range ry = new Range();
			ry.all = true;
			Range rx = new Range();
			rx.all = true;
			frame_patches = subvid(vbuf, new Range(coff.z - tmp, coff.z - tmp + patopt.step.z), ry, rx);
			// dumpMatSize(frame_patches);
		}

		// get locations
		void getLoc() {
			int i;

			if (debug) {
				System.out.println("size: " + vrows + "," + vcols);
			}

			// coff = floor(winsize/2)

			// BUG FIXED
			// coff = (Point3i)(patopt.win * 0.5);

			coff.x = (int) (patopt.win.x * 0.5);
			coff.y = (int) (patopt.win.y * 0.5);
			coff.z = (int) (patopt.win.z * 0.5);

			// zrange = 1:step(3):(sizez-winsize(3)+1);
			// yrange = 1:step(1):(sizey-winsize(1)+1);
			// xrange = 1:step(2):(sizex-winsize(2)+1);

			int ysize = (vrows - patopt.win.y) / patopt.step.y + 1;
			int xsize = (vcols - patopt.win.x) / patopt.step.x + 1;
			int maxsize = xsize * ysize;
			locyx.ensureCapacity(maxsize);
			locyx_mask.ensureCapacity(maxsize);
			allx.ensureCapacity(xsize);
			ally.ensureCapacity(ysize);

			if (debug) {
				System.out.println("x=" + xsize + ", y=" + ysize + " max=" + maxsize);
			}

			// locyx (iterate over x then y)
			for (int y = 0; y <= vrows - patopt.win.y; y += patopt.step.y) {
				for (int x = 0; x <= vcols - patopt.win.x; x += patopt.step.x) {
					locyx.add(new Point2i(x, y));
					locyx_mask.add(true); // initialize mask
					if (y == 0)
						allx.add(x);
				}
				ally.add(y);
			}

			if (debug) {
				System.out.println("locyx.size() =" + locyx.size());
				for (i = 0; i < locyx.size(); i++) {
					System.out.println("locyx.get(" + i + ").x = " + locyx.get(i).x + " locyx.get(" + i + ").y = "
							+ locyx.get(i).y);
				}
				System.out.println("allx.size() = " + allx.size());
				for (i = 0; i < (int) allx.size(); i++) {
					System.out.println("allx.get(" + i + ")" + allx.get(i));
				}
				System.out.println();
				System.out.println("ally.size() = " + ally.size());
				for (i = 0; i < (int) ally.size(); i++) {
					System.out.println("ally.get(" + i + ")" + ally.get(i));
				}
				System.out.println();
			} // if (debug)

		}

		void reset() {
			int i;
			locz = -patopt.step.z;
			curz = 0;
			nextz = patopt.win.z;
			if (vbuf != null) {
				for (i = vbuf.length - 1; i >= 0; i--) {
					vbuf[i] = null;
				}
			}
			vbuf = null;
			if (flag_zm) {
				if (vbuf_zm != null) {
					for (i = vbuf_zm.length - 1; i >= 0; i--) {
						vbuf_zm[i] = null;
					}
				}
				vbuf_zm = null;
			}
		}

	}

	boolean addFrames(PatchExtractor pe, Mat invid[]) {
		// get number of frames
		int sizez = 1;
		int sizex = 1;
		int sizey = 1;
		int dims = 2;
		int r, c;
		if (invid.length == 1) {
			if (invid[0].dims == 2) {
				dims = 2;
				sizez = 1;
				sizey = invid[0].size[0];
				sizex = invid[0].size[1];
			} else {
				sizez = invid[0].size[0];
				sizey = invid[0].size[1];
				sizex = invid[0].size[2];
				dims = 3;
			}
		} else if (invid.length > 1) {
			if (invid[0].dims == 2) {
				sizez = invid.length;
				sizey = invid[0].size[0];
				sizex = invid[0].size[1];
				dims = 3;
			} else {
				sizez = invid.length * invid[0].size[0];
				sizey = invid[0].size[1];
				sizex = invid[0].size[2];
				dims = 3;
			}
		}

		Mat vid[];
		if (sizex == pe.vcols && sizey == pe.vrows) {
			vid = invid;
		} else {
			if (sizex < pe.vcols || sizey < pe.vrows) {
				MipavUtil.displayError("video not large enough!");
				System.exit(-1);
			}

			System.out.println("WARNING: addFrames: video is too large!  cropping.");
			vid = subvid(invid, 0, sizez, 0, pe.vrows, 0, pe.vcols);
		}

		// check if we are adding too much
		if (pe.curz + sizez > pe.nextz + pe.patopt.step.z) {
			MipavUtil.displayError("adding too many frames!");
			System.exit(-1);
		}

		// pop frames from the front (only if we're not replacing the full
		// buffer)
		if (sizez < pe.patopt.win.z) {
			for (int z = sizez; z < pe.patopt.win.z; z++) {
				Mat fb = pe.vbuf[z];
				Mat ff = pe.vbuf[z - sizez];
				copyTo(fb, ff);
				// cout << "push z: " << z << " --> " <<z-sizez << "\n";
			}
		}

		// add frames on the back
		Mat vtmp;
		if (dims == 2) {
			Mat fb = pe.vbuf[pe.patopt.win.z - 1];
			if (vid[0].type == CV_64F) {
				// just copy
				copyTo(vid[0], fb);
			} else if (vid[0].type == CV_8U) {
				// convert first
				vtmp = new Mat(sizey, sizex, CV_64F);
				for (r = 0; r < sizey; r++) {
					for (c = 0; c < sizex; c++) {
						vtmp.double2D[r][c] = (vid[0].byte2D[r][c] & 0xff);
					}
				}
				copyTo(vtmp, fb);
			}

		} else {
			for (int z = 0; z < sizez; z++) {
				Mat fb = pe.vbuf[pe.patopt.win.z - sizez + z];
				Mat vf = vid[z];
				if (vf.type == CV_64F) {
					// just copy
					copyTo(vf, fb);
				} else if (vf.type == CV_8U) {
					// convert first
					vtmp = new Mat(sizey, sizex, CV_64F);
					for (r = 0; r < sizey; r++) {
						for (c = 0; c < sizex; c++) {
							vtmp.double2D[r][c] = (vf.byte2D[r][c] & 0xff);
						}
					}
					copyTo(vtmp, fb);
				}
				// cout << "newframe " << z << " --> " << patopt.win.z-sizez+z
				// << "\n";
			}
		}

		// update current z position
		pe.curz += sizez;

		// check if we made a new patch
		if (pe.curz >= pe.nextz) {
			// if yes, then process it and return true
			processNextPatch(pe);
			return true;

		} else {

			// no new patch yet
			return false;
		}
	}

	// process patches
	void processNextPatch(PatchExtractor pe) {
		// update locations
		pe.locz += pe.patopt.step.z; // next locz of patches
		pe.nextz += pe.patopt.step.z; // next new patch

		// update location mask using minvar
		if (pe.patopt.minvar != 0) {

			for (int k = 0; k < pe.locyx.size(); k++) {
				// get the patch from buffer
				Mat mypat[] = subvid(pe.vbuf, new Range(0, pe.patopt.win.z),
						new Range(pe.locyx.get(k).y, pe.locyx.get(k).y + pe.patopt.win.y),
						new Range(pe.locyx.get(k).x, pe.locyx.get(k).x + pe.patopt.win.x));

				/*
				 * Mat test1; mypat.copyTo(test1); int t1=test1.size[0]; int
				 * t2=test1.size[1]; int t3=test1.size[2]; mypat=test1;
				 */

				// compute variance for each pixel
				Vector<Double> pixvar = new Vector<Double>();
				for (int i = 0; i < mypat[0].size[1]; i++)
					for (int j = 0; j < mypat[0].size[0]; j++) {
						// Mat
						// tmpMd=MatVid::subvid(mypat,Range::all(),Range(j,j+1),Range(i,i+1));
						Mat tmpM[] = subvid(mypat, new Range(pe.patopt.minvarf, pe.patopt.win.z - pe.patopt.minvarf),
								new Range(j, j + 1), new Range(i, i + 1));

						// Mat
						// tmpM=MatVid::subvid(mypat,Range(2,8),Range(j,j+1),Range(i,i+1));
						// Mat
						// tmpM=MatVid::subvid(mypat,Range(5,15),Range(j,j+1),Range(i,i+1));
						// Mat
						// tmpM=MatVid::subvid(mypat,Range(1,19),Range(j,j+1),Range(i,i+1));

						double var = computeVariance3dDouble(tmpM);
						pixvar.add(var);
					}
				double mv, minvar2;
				if (pe.patopt.minvar > 0) {
					mv = meanOfStdVector(pixvar);
					minvar2 = pe.patopt.minvar;
				} else {
					mv = minOfStdVector(pixvar);
					minvar2 = -pe.patopt.minvar;
				}
				// if below minimum, update the mask
				if (mv <= minvar2) {
					pe.locyx_mask.set(k, false);

				} else {
					pe.locyx_mask.set(k, true);
				}
			}
		}

		// process patches
		switch (pe.patopt.normopt) {
		case NORM_NONE:
			// do nothing
			break;
		case NORM_ZM:
		case NORM_ZMUV: {
			// compute the mean frame
			Mat Ymean = new Mat();
			reduce(pe.vbuf, Ymean, CV_REDUCE_AVG);

			// subtract mean
			for (int z = 0; z < pe.patopt.win.z; z++) {
				Mat f = pe.vbuf[z];
				pe.vbuf_zm[z] = minus(f, Ymean);
			}

			// also apply variance normalization
			if (pe.patopt.normopt == norm_type.NORM_ZMUV) {
				// for each patch...
				for (int i = 0; i < pe.locyx.size(); i++) {
					// get the patch in zero-mean buffer
					Mat zmpatch[] = subvid(pe.vbuf_zm, new Range(0, pe.patopt.win.z),
							new Range(pe.locyx.get(i).y, pe.locyx.get(i).y + pe.patopt.win.y),
							new Range(pe.locyx.get(i).x, pe.locyx.get(i).x + pe.patopt.win.x));

					// copy and normalize
					copyTo(zmpatch, pe.patches.get(i));
					double pstd = Math.sqrt(norm2(pe.patches.get(i)) / (total(pe.patches.get(i)) - 1.0));
					pe.patches.set(i, times(pe.patches.get(i), (1.0 / pstd)));
				}
			}
		}
			break;
		default:
			MipavUtil.displayError("bad option in processNextPatch");
			System.exit(-1);
		}
	}

	double norm2(Mat vid[]) {
		if (vid[0].type != CV_64F) {
			MipavUtil.displayError("vid[0].type is not the required CV_64F in norm2");
			System.exit(-1);
		}

		double tmp = 0.0;
		for (int z = 0; z < vid.length; z++)
			for (int y = 0; y < vid[0].rows; y++)
				for (int x = 0; x < vid[0].cols; x++) {
					double p = vid[x].double2D[y][x];
					tmp += p * p;
				}
		return tmp;
	}
	
	double total(Mat vid) {
		

		double tmp = 0.0;
		if (vid.type == CV_64F) {
			if (vid.dims == 2) {
			for (int y = 0; y < vid.rows; y++)
				for (int x = 0; x < vid.cols; x++) {
					double p = vid.double2D[y][x];
					tmp += p;
				}
			}
			else if (vid.dims == 3) {
				for (int z = 0; z < vid.depth; z++)
				for (int y = 0; y < vid.rows; y++)
					for (int x = 0; x < vid.cols; x++) {
						double p = vid.double3D[z][y][x];
						tmp += p;
					}	
			}
		}
		if (vid.type == CV_8U) {
			if (vid.dims == 2) {
			for (int y = 0; y < vid.rows; y++)
				for (int x = 0; x < vid.cols; x++) {
					double p = vid.byte2D[y][x];
					tmp += p;
				}
			}
			else if (vid.dims == 3) {
				for (int z = 0; z < vid.depth; z++)
				for (int y = 0; y < vid.rows; y++)
					for (int x = 0; x < vid.cols; x++) {
						double p = vid.byte3D[z][y][x];
						tmp += p;
					}	
			}
		}
		return tmp;
	}

	double total(Mat vid[]) {
		if (vid[0].type != CV_64F) {
			MipavUtil.displayError("vid[0].type is not the required CV_64F in norm2");
			System.exit(-1);
		}

		double tmp = 0.0;
		for (int z = 0; z < vid.length; z++)
			for (int y = 0; y < vid[0].rows; y++)
				for (int x = 0; x < vid[0].cols; x++) {
					double p = vid[x].double2D[y][x];
					tmp += p;
				}
		return tmp;
	}

	double computeVariance3dDouble(Mat[] input) {
		int d, r, c;
		int depth = input.length;
		int rows = input[0].rows;
		int cols = input[0].cols;
		double sum = 0.0;
		int count = 0;
		double mean;
		double diff;
		for (d = 0; d < depth; d++) {
			for (r = 0; r < rows; r++) {
				for (c = 0; c < cols; c++) {
					sum += input[d].double2D[r][c];
					count++;
				}
			}
		}
		mean = sum / count;
		double variance = 0.0;
		for (d = 0; d < depth; d++) {
			for (r = 0; r < rows; r++) {
				for (c = 0; c < cols; c++) {
					diff = (input[d].double2D[r][c] - mean);
					variance += (diff * diff);
				}
			}
		}
		variance = variance / (count - 1);
		return variance;
	}

	void dumpMatSize(Mat m[]) {
		System.out.println("m.length = " + m.length);
		System.out.println("dims=" + m[0].dims + " (" + m[0].rows + " x " + m[0].cols + ")");
		System.out.println("m[0].size.length = " + m[0].size.length);
		for (int n = 0; n < m[0].dims; n++) {
			System.out.println("m[0].size[" + n + "] = " + m[0].size[n]);
		}
		System.out.println("m[0].step.length = " + m[0].step.length);
		for (int n = 0; n < m[0].step.length; n++) {
			System.out.println("m[0].step[" + n + "] = " + m[0].step[n]);
		}
		System.out.println("m[0].type = " + m[0].type);
	}

	double meanOfStdVector(Vector<Double> v) {
		int i;
		double sum = 0.0;
		double mean;
		int count = 0;
		for (i = 0; i < v.size(); i++) {
			sum += v.get(i);
			count++;
		}
		mean = sum / count;
		return mean;
	}

	double minOfStdVector(Vector<Double> v) {
		int i;
		double min = Double.MAX_VALUE;
		for (i = 0; i < v.size(); i++) {
			if (v.get(i) < min) {
				min = v.get(i);
			}
		}
		return min;
	}

	/*
	 * ! \brief EM learninig options.
	 *
	 * \remarks In EM implementation few options are not implemented and their
	 * default values are used instead \see DytexMix
	 */
	class EMOptions {
		/*
		 * ! \brief number of clusters.
		 */
		public int K;
		/*
		 * ! \brief regularization options. \see DytexRegOptions
		 */
		public DytexRegOptions regopt;
		/*
		 * ! \brief termination parameter.
		 */
		public double termvalue;
		/*
		 * ! \brief termination value for the EMBEST.
		 */
		public double termvalBest;
		/*
		 * ! \brief maximum number of iterations.
		 */
		public int maxiter;

		/*
		 * ! \brief verbose value.
		 */
		public Verbose_mode verbose;
		/*
		 * ! \brief empty cluster splitting options. \see DytexSplitParams
		 */
		public DytexSplitParams emptySplitOpt = new DytexSplitParams();
		/*
		 * ! \brief cluster splitting options. \see DytexSplitParams
		 */
		public DytexSplitParams splitOpt = new DytexSplitParams();

		// initialize EM options
		/*
		 * ! \brief initialize EMOptions object.
		 * 
		 * \param K number of clusters.
		 * 
		 * \param regopt regularization options.
		 * 
		 * \param termvalue termination parameter.
		 * 
		 * \param verbose verbose value.
		 * 
		 * \see DytexOptions | DytexMix | HEMOptions
		 */
		public EMOptions(int K, DytexRegOptions regopt, double termvalue, Verbose_mode verbose) {
			// setting parameters
			this.K = K;
			this.verbose = verbose;
			this.termvalue = termvalue;
			this.termvalBest = 1e-5;
			maxiter = 500;

			// setting empty cluster splitting options
			emptySplitOpt.crit = Split_crit.SPLITP;
			emptySplitOpt.pert = 0.01;
			emptySplitOpt.mode = Split_mode.MODEP;
			emptySplitOpt.vars = Split_vars.VARC;

			// splitting schedule [1 2 4 8 16....]
			splitOpt.sched.clear();

			if ((K > 2) && (K % 2 == 0)) {
				splitOpt.sched.add(1);
				splitOpt.sched.add(2);
				while (true) {
					if (splitOpt.sched.get(splitOpt.sched.size() - 1) == K) {
						break;
					}
					int tmp = splitOpt.sched.get(splitOpt.sched.size() - 1) * 2;
					splitOpt.sched.add(tmp);
				}
			}
		}
	}
	
	/*!
	 * \brief
	 * learn a DTM uisng EM given a set of patches
	 * 
	 * \param Yin
	 * cell array of video
	 * 
	 * \param emopt
	 * EM learning options.
	 *  
	 * Learns a dytex mixture using several runs of EM with component splitting procedure:
	 * 
	 * \remarks
	 * After initializing a DTM with DytexOptions, this is the interface function to be called 
	 * in order to learn a mixture from a set of video patches.
	 * 
	 * \see
	 * reduceWithSplitting | EMOptions
	 */
	void learnWithSplitting(DytexMix dtm, Vector<Mat[]> Yin,EMOptions emopt)
	{
		int i,j;
		double pert;	
		pert=emopt.splitOpt.pert;
		//initialize splitting sequence
		if(emopt.splitOpt.sched.isEmpty())
		{
			for(i=1;i<=emopt.K;i++)
				emopt.splitOpt.sched.add(i);
		}
		//check for valid splitting sched
		if(emopt.splitOpt.sched.get(0)!=1)
		{
			MipavUtil.displayError("schedule must start with 1!");
			System.exit(-1);
		}
		Vector<Integer> tmp = new Vector<Integer>();
		//vector<int>::iterator it;
		for(i=1;i<emopt.splitOpt.sched.size();i++)
			tmp.add(emopt.splitOpt.sched.get(i)/emopt.splitOpt.sched.get(i-1));
		
		for (j = 0; j < tmp.size(); j++) {
    		if (tmp.get(j) > 2) {
    			MipavUtil.displayError("Cannot grow K more than 2 times previous");
    			System.exit(-1);
    		}
    	}

		System.out.println("Growing schedule: ");
		for (j = 0; j < emopt.splitOpt.sched.size(); j++) {
    		System.out.print(emopt.splitOpt.sched.get(j) + " ");
    	}
    	System.out.print("\n");
		System.out.println("Ks: "+ emopt.K);	

		int Kiter=1;

		while(dtm.dt.size()<emopt.K)
		{
			//do first cluster
			if(Kiter==1)
			{			 
				System.out.println("*** EM: K=" + (dtm.dt.size()+1) + "******************");
			}
			else
			{
				Vector<Integer> mysplits = new Vector<Integer>();
				
				//splitting current mixture
				while(dtm.dt.size()<emopt.splitOpt.sched.get(Kiter-1))
				{
					DytexSplitParams splitopt = new DytexSplitParams();
					splitopt.crit=emopt.splitOpt.crit;				
					splitopt.ignore=mysplits;
					splitopt.target=-1;
					splitopt.pert=emopt.splitOpt.pert;
					splitopt.mode=emopt.splitOpt.mode;
					splitopt.vars=emopt.splitOpt.vars;
					int c1[] = new int[1];
					int c2[] = new int[1];
					dytex_mix_split(dtm,splitopt,c2,c1);
					mysplits.add(c1[0]);
					mysplits.add(c2[0]);
				}
				System.out.println("*** EM: K=" + dtm.dt.size() + "******************");
			}
			runEM(dtm,Yin,emopt);  //RUN EM uisng current Mixture		
			Kiter++;
		}

		//RUN EM again on once on final solution
		emopt.termvalue=emopt.termvalBest;
		runEM(dtm,Yin,emopt);
		//initialize kalman caches
		setupKFB(dtm, Yin.get(0).length);
	}


	

	/*
	 * ! \brief run EM for a mixture of DT
	 * 
	 * \param Yin cell array of video
	 * 
	 * \param emopt EM learning options
	 * 
	 * \remarks in general, this should not be called.use learnWithSplitting
	 * instead
	 * 
	 * \see learnWithSplitting | runHEM
	 */
	void runEM(DytexMix dtm, Vector<Mat[]> Yin, EMOptions emopt) {
		int i,j,r,c;
		long elapsedtime;	
		//used to display info in change in classes during EM loop
		int numlastclasses=5;			
		//erase old class info
		dtm.classes.clear();  
		
		//options
		Ymean_type FlagYmean   =   dtm.opt.Yopt;
		Verbose_mode FlagVerbose = emopt.verbose;	
		int n=dtm.opt.n;
		int N=Yin.size();
		//min total probability for blank cluster
		double MINPROB =(((double)1.0)/(((double)2.0)*(double)N));  
		
		//           PREPROCESSING               //
		if(FlagVerbose != Verbose_mode.QUIET)
			System.out.println("Converting images...");

		//convert movie into a data matrix Y(:,i,t)
		//this makes running the Kalman filter a lot faster
		Mat foo;
		int mysx[] = new int[1];
		int mysy[] = new int[1];
		int mysc[] = new int[1];
		int mycnorm[] = new int[1];
		int tau = 1;
		int dy = 1;
		Mat Y[] = null;
		Vector<Mat> Ymean = new Vector<Mat>();
		for(int yi=0;yi<N;yi++)
		{
			foo=convertmovie(Yin.get(yi),mysx,mysy,mysc,mycnorm);
			tau=foo.cols;
			dy=foo.rows;
			if(yi==0)
			{
				int sz[]={tau,dy,N};				
				Y=create(tau,dy,N,foo.type); 
			}
			else
			{
				if(tau!=Y.length) {
					MipavUtil.displayError("In runEM Y has videos with different lengths!");
					System.exit(-1);
				}
			}

			if(FlagYmean == Ymean_type.NONZERO_YMEAN) 
			{
				Mat tmpM = new Mat();
				reduce(foo,tmpM,2,CV_REDUCE_AVG);
				Ymean.add(tmpM);
			}
			
			for(j=0;j<dy;j++)
				for(int k=0;k<tau;k++)
					Y[k].double2D[j][yi]=foo.double2D[j][k];	
		}
		//

		//               INITIALIZATION                              //
		//initialization of first DT in the blank mixture using Sub-optimal methhod
		if(dtm.dt.size()==0)  
		{
			Vector<Integer> randind = new Vector<Integer>(N);
			for(i=0;i<N;i++)
				randind.add(i);

			if(FlagVerbose != Verbose_mode.QUIET)
				System.out.println("Initializing First DT with Sub-optimal:");
			
			Vector<Mat> tmpY = extractY(Y, randind);
			Dytex tmpC= initcluster_doretto(tmpY, dtm.opt);
			dtm.dt.add(tmpC);
			dtm.alpha.add(1.0);
		}
		int K=dtm.dt.size(); //current mixture size
		//Regularize the initializations
		for(i=0;i<K;i++)
		{		
		    setRegularizer(dtm.dt.get(i),emopt.regopt);
			regularize(dtm.dt.get(i),false);							
		}
		//
			
		//             RUN EM loooop                   /
		
		//initialize convergence measures	
		Vector<Double>datalikelihood = new Vector<Double>();
		for (i = 0; i < emopt.maxiter+1; i++) {
			datalikelihood.add(0.0);
		}
		Vector<Double>ddtall = new Vector<Double>();
		for (i = 0; i < emopt.maxiter; i++) {
			ddtall.add(0.0);
		}
		Vector<Double>pdtall = new Vector<Double>();
		for (i = 0; i < emopt.maxiter; i++) {
			pdtall.add(0.0);
		}
		Vector<Vector<Integer>> lastclasses = new Vector<Vector<Integer>>();
		for (i = 0; i < numlastclasses; i++) {
			Vector<Integer> intVector = new Vector<Integer>();
			for (j = 0; j < N; j++) {
				intVector.add(0);
			}
			lastclasses.add(intVector);
		}
		int lastclassesind = 0;
		Mat Z;
		Mat logZ = new Mat();

		//initialize blanks
		Vector<Double>blank = new Vector<Double>();
		for (i = 0; i < K; i++) {
			blank.add(0.0);
		}
		for(j=0;j<K;j++)
		{
			if(dtm.dt.get(j).isempty)
				blank.set(j,1.0);
		}

		//initialize loop
		long starttime = System.currentTimeMillis();
		int iter=0;
		double ddLL,dpLL;
		//loop EM********************
		while(true)
		{
			if(FlagVerbose == Verbose_mode.VERBOSE)
			{
				System.out.println("***** iter " + iter + " *****");
				System.out.println("E-Step: Running Kalman filter: ");
			}
			
			//state expectation
		    Vector<Vector<Mat> > xthat = new Vector<Vector<Mat>>();
			//state covariance
			Vector<Mat[]> Vthat = new Vector<Mat[]>();
			Vector<Mat[]> Vtt1hat = new Vector<Mat[]>();
			//log-likelihoods of each Y
			Mat LL = new Mat(N,K,CV_64F);

			String di3opt="";
			//%%%% E-Step %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			for(j=0;j<K;j++)
			{
				if (FlagVerbose == Verbose_mode.VERBOSE)
					System.out.print(j + " ");

				//option to subtract Ymean
				if (FlagYmean == Ymean_type.NONZERO_YMEAN)
					di3opt = "y";
				else
					di3opt = "";
				
				if (blank.get(j)==0)
				{
					//compute likelihood of each video for this cluster
					Vector<Mat> xt = new Vector<Mat>();
					Mat Vt[] = new Mat[Y.length];
					for (i = 0; i < Y.length; i++) {
						Vt[i] = new Mat();
					}
					Mat Vtt1[] = new Mat[Y.length-1];
					for (i = 0; i < Y.length-1; i++) {
						Vtt1[i] = new Mat();
					}
					Mat tmpL = new Mat();
					Dytex test1=dtm.dt.get(j);
					//conditional state inference using Kalman smoothing filter
					dytex_kalman(Y, dtm.dt.get(j), di3opt,xt,Vt,Vtt1,tmpL);	
					xthat.add(xt);
					Vthat.add(Vt);
					Vtt1hat.add(Vtt1);
					for (r = 0; r < LL.rows; r++) {
					    LL.double2D[r][j] = tmpL.double2D[0][r] + Math.log(dtm.alpha.get(j));
					}
				}
				else {
			        for (r = 0; r < LL.rows;r++) {
					    LL.double2D[r][j] = -1e300; // a very small number...
			        }
				}				
			}
					
			//% soft assignment and data likelihood (no constraints)
			Mat tmp = transpose(logtrick(transpose(LL)));				
			logZ.create(tmp.rows,K,CV_64F);
			for(j=0;j<K;j++)
			{
				for (r = 0; r < logZ.rows; r++) {
				    logZ.double2D[r][j] = LL.double2D[r][j] - tmp.double2D[r][0];
				}
			}
			double stmp = 0.0;
			for (r = 0; r < tmp.rows; r++) {
				stmp += tmp.double2D[r][0];
			}
			datalikelihood.set(iter,stmp);
			Z = new Mat(logZ.rows,logZ.cols,CV_64F);
			for (r = 0; r < logZ.rows; r++) {
				for (c = 0; c < logZ.cols;c++) {
					Z.double2D[r][c] = Math.exp(logZ.double2D[r][c]);
				}
			}		

			if (FlagVerbose == Verbose_mode.VERBOSE)
				System.out.println();

			//hard assignment
			dtm.classes.clear();
			for(i=0;i<Z.rows;i++)
			{
				double max = Z.double2D[i][0];
				int maxL = 0;
				for (c = 1; c < Z.cols; c++) {
					if (Z.double2D[i][c] > max) {
						max = Z.double2D[i][c];
						maxL = c;
					}
				}				
				dtm.classes.add(maxL+1);
			}

			//Check Convergence
			if(iter>0)
			{
				//compute change in log-likelihood
				ddLL=datalikelihood.get(iter)-datalikelihood.get(iter-1);
				dpLL=Math.abs(ddLL/datalikelihood.get(iter-1));
			}
			else
			{
				ddLL = Double.MAX_VALUE;
				dpLL = Double.MAX_VALUE;			
				
			}
			//class assignment info
			lastclasses.set(lastclassesind,dtm.classes);

			//count the number of class changes
			Vector<Integer> dclass = new Vector<Integer>();
			for(int ii=0;ii<numlastclasses;ii++)
			{
				int sum=0;
				for(i=0;i<lastclasses.get(0).size();i++)
				{
					if(lastclasses.get(ii).get(i)!=lastclasses.get(lastclassesind).get(i))
						sum++;
				}
				dclass.add(sum);
			}

			String dclassstr="";			
			for(i=lastclassesind+1;i<numlastclasses;i++)
			{
				dclassstr=dclassstr+String.valueOf(dclass.get(i))+" ";
			}
			for(i=0;i<lastclassesind;i++)
			{
				dclassstr=dclassstr+String.valueOf(dclass.get(i))+" ";
			}

			//% lastclassind points to the oldest classes
			lastclassesind = lastclassesind+1;
			if (lastclassesind>=numlastclasses)
				lastclassesind = 0;


			//output strings
			String outstr2 = "dclass = " + dclassstr;
			String outstr1s = "L= " + datalikelihood.get(iter) + " (pL= " + dpLL + ")";
			String outstr3;			
			if(FlagVerbose==Verbose_mode.COMPACT)
			{
				outstr3="iter= "+(iter+1)+"; " + outstr1s + "; " + outstr2 + ";  ";
				System.out.println(outstr3);
			}
			else if(FlagVerbose==Verbose_mode.VERBOSE)
			{			
				System.out.println(outstr2);
			}
			// check if negative change in log-likelihood!
			if (ddLL<0)
			{
				System.out.println("WARNING -- change in log likelihood is negative???");	
			}	


			//%%% check convergence conditions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			int breakout = 0;
			String termstr = null;
			if (iter >= emopt.maxiter)
			{
				termstr = "***** done -- max iter reached\n";
				breakout = 1;
			}

			//only this convergence condition
			if ( (ddLL >= 0) && (dpLL < emopt.termvalue) )
			{
				termstr = "***** done -- small percent change in data likelihood\n";
				breakout = 1;
			}
				
			//%%% convergence condition was reached... %%%%%%%%%%%%%%%%%%%%%%%%%%%%
			if (breakout != 0)
			{
				if (FlagVerbose != Verbose_mode.QUIET)
				{
					if (FlagVerbose == Verbose_mode.COMPACT)
					{
						System.out.println();				
					}
					System.out.println(termstr);
				}	
				break;
			}

			//%%% M-Step %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		

			//1) update prior probabilities
			//total soft assignments per cluster
			Mat Nhat = new Mat();
			reduce(Z,Nhat,0,CV_REDUCE_SUM);
			tmp=divide(Nhat,((double)N));		
			for(i=0;i<dtm.alpha.size();i++)
				dtm.alpha.set(i,tmp.double2D[0][i]);

			
			//%%% loop through each cluster and compute aggregate statistics%%%
			for(j=0;j<K;j++)
			{		
				//output of Kalman filters
				Vector<Mat> jxthat = new Vector<Mat>();
				Mat jYmean = new Mat();
				Mat jz;
				Mat PhiAll,Phi,sphi,Psi,Gamma,Eta,Xi;
				Mat Lambda = new Mat();
				//check if this is cluster is blank
				if(dtm.alpha.get(j)<=MINPROB)
				{
					blank.set(j,1.0);
					dtm.dt.get(j).isempty=true;
					if (FlagVerbose != Verbose_mode.QUIET)
						System.out.println("blank");
				}			
				else // % --- standard M-step: learn the parameters -------------------------
				{
					//get output of Kalman filters
					jxthat=xthat.get(j);
					Mat jVthat[]=Vthat.get(j);
					Mat jVtt1hat[]=Vtt1hat.get(j);
					jz = new Mat(Z.rows,1,CV_64F);
					for (r = 0; r < Z.rows; r++) {
					    jz.double2D[r][0]=Z.double2D[r][j];
					}
					double jN=Nhat.double2D[0][j];

					if(FlagYmean == Ymean_type.NONZERO_YMEAN)
						jYmean=dtm.dt.get(j).Ymean;

					//compute aggregated statistics %%%
					//% Initialize Phi and phi
					PhiAll=new Mat(n,n,CV_64F);
					Phi=new Mat(n,n,CV_64F); // varphi
					sphi=new Mat(n,n,CV_64F); //lowercase phi

					Mat tmp1 = new Mat();
					reduce(jVthat,tmp1,CV_REDUCE_SUM);
					Mat tmp1a = minus(tmp1,jVthat[0]);
					Mat tmp1b = minus(tmp1,jVthat[jVthat.length-1]);

					//Initialize Psi
					Psi=new Mat(n,n,CV_64F);
					Mat tmp3 = new Mat();
					reduce(jVtt1hat,tmp3,CV_REDUCE_SUM);

					//Initialize Gamma and Lambda
					Gamma = new Mat(dy,n,CV_64F);

					switch(dtm.opt.Ropt)
					{
					case COV_IID:
						Lambda = new Mat(1,1,CV_64F);
						break;
					default:
						MipavUtil.displayError("TO DO");
						System.exit(-1);
					}

					//Initialize Xi
					Xi = new Mat(n,1,CV_64F);
					//Initialize Eta
					Eta = new Mat(n,n,CV_64F);

					// aggregate over all samples     
					for(i=0;i<N;i++)
					{
						//current X and Y and Z
						Mat myx=jxthat.get(i);
						Mat myy;
						double myz;
						Mat tmpM = new Mat(dy,tau,CV_64F);
						for(int p=0;p<Y.length;p++)
							for(int q=0;q<Y[0].rows;q++)
								tmpM.double2D[q][p]=Y[p].double2D[q][i];

						if (FlagYmean  == Ymean_type.NONZERO_YMEAN)
						{							
							Mat tmpM2 = new Mat();
							repeat(jYmean,1,tau,tmpM2);
							myy=minus(tmpM,tmpM2);
						}
						else
						{
							myy=tmpM;
						}

						myz=jz.double2D[i][0];

						// update Phi and phi
						Mat tmp2 = times(myx,transpose(myx));
						Mat myxcol = new Mat(myx.rows,1,CV_64F);
						for (r = 0; r < myx.rows; r++) {
							myxcol.double2D[r][0] = myx.double2D[r][0];
						}
						Mat tmp2a = minus(tmp2,times(myxcol,transpose(myxcol)));
						for (r = 0; r < myx.rows; r++) {
							myxcol.double2D[r][0] = myx.double2D[r][myx.cols-1];
						}
						Mat tmp2b = minus(tmp2,times(myxcol,transpose(myxcol)));

						Phi    = plus(Phi,times(plus(tmp1a,tmp2a),myz));
						sphi   = plus(sphi,times(plus(tmp1b,tmp2b),myz));
						PhiAll = plus(PhiAll,times(plus(tmp1,tmp2),myz));

						// update Psi
						Mat myxcol1 = new Mat(myx.rows,myx.cols-1,CV_64F);
						for (r = 0; r < myx.rows; r++) {
							for (c = 1; c < myx.cols; c++) {
								myxcol1.double2D[r][c-1] = myx.double2D[r][c]
;							}
						}
						Mat myxcol0 = new Mat(myx.rows,myx.cols-1,CV_64F);
						for (r = 0; r < myx.rows; r++) {
							for (c = 0; c < myx.cols-1; c++) {
								myxcol0.double2D[r][c] = myx.double2D[r][c]
;							}
						}
						Mat tmp4 = times(myxcol1,transpose(myxcol0));
						Psi = plus(Psi,times(plus(tmp3,tmp4),myz));

						//Gamma and Lambda
						Gamma = plus(Gamma,times(times(myy,transpose(myx)),myz));
						double scatmp;

						switch(dtm.opt.Ropt)
						{
						case COV_IID:

							tmpM = times(myy,myy);
							scatmp=sum(tmpM);
							Lambda=plus(Lambda,(myz*scatmp));
							break;
						default:
							MipavUtil.displayError("TO DO");
							System.exit(-1);
						}

						// Xi and Eta					
						// using only initial state samples
						myxcol0 = col(myx, 0);
						Xi  = plus(Xi,times(myxcol0,myz));
						Eta = plus(Eta,times(plus(jVthat[0],times(myxcol0,transpose(myxcol0))),myz));
					}

					//Compute New parameters

					//2) C parameter
					Mat iPhiAll=new Mat((new Matrix(PhiAll.double2D)).inverse().getArray());
					Mat newC=times(Gamma,iPhiAll);
					dtm.dt.get(j).C=newC;

					//3) R parameter
					Mat rnew;
					switch(dtm.opt.Ropt)
					{
					case COV_IID:			
						rnew = divide(minus(Lambda,trace(times(iPhiAll,times(transpose(Gamma),Gamma)))),(dy*tau*jN));
						dtm.dt.get(j).R.mtx=rnew;
						dtm.dt.get(j).R.covopt=cov_type.COV_IID;						
						break;
					default:
						MipavUtil.displayError("TO DO");
						System.exit(-1);
					}

					//4) A parameter
					Mat sphiinv = new Mat((new Matrix(sphi.double2D)).inverse().getArray());
					Mat newA = times(Psi,sphiinv);
					dtm.dt.get(j).A = newA;

					// 5) Q parameter
					Mat Q = divide(minus(Phi,times(newA,transpose(Psi))),( (tau-1)*jN));
					dtm.dt.get(j).Q.mtx= Q;

					// 6) mu parameter
					Mat newmu = divide(Xi,jN);
					dtm.dt.get(j).mu0 = newmu;

					//7) S parameter 
					Mat newS = minus(divide(Eta,jN),times(newmu,transpose(newmu)));

					switch(dtm.opt.Sopt)
					{
					case COV_DIAG:			
						for (r = 0; r < Math.min(newS.rows,newS.cols); r++) {
							dtm.dt.get(j).S0.mtx.double2D[r][0] = newS.double2D[r][r];
						}
						dtm.dt.get(j).S0.covopt=cov_type.COV_DIAG;
						break;
					default:
						MipavUtil.displayError("TO DO");
					}

					// 8) Ymean parameter
					Mat newYmean = new Mat(dy,1,CV_64F);
					if (FlagYmean == Ymean_type.NONZERO_YMEAN)
					{
						for(i=0;i<N;i++)
						{
							Mat tmpM = new Mat();
							reduce(jxthat.get(i),tmpM,1,CV_REDUCE_AVG);							
							newYmean = plus(newYmean,times(minus(Ymean.get(i),times(newC,tmpM)),(jz.double2D[i][0])));
						}					
						newYmean = divide(newYmean,jN);
					}

					dtm.dt.get(j).Ymean = newYmean;

					// regularize the new parameters
					setRegularizer(dtm.dt.get(j),emopt.regopt);
					regularize(dtm.dt.get(j),false);			

				}	
			}
			if (FlagVerbose == Verbose_mode.VERBOSE)
				System.out.println();

			//%%% handle empty clusters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
			//find the largest cluster and split it to fill the blank cluster
			for(j=0;j<K;j++)
			{
				if(blank.get(j) != 0.0)
				{
					if(FlagVerbose != Verbose_mode.QUIET)
						System.out.println("Cluster "+j+"Is blank");			

					DytexSplitParams splitopt = new DytexSplitParams();				
					splitopt.crit=emopt.emptySplitOpt.crit;
					splitopt.pert=emopt.emptySplitOpt.pert;
					splitopt.mode=emopt.emptySplitOpt.mode;
					splitopt.vars=emopt.emptySplitOpt.vars;
					splitopt.ignore.clear();
					splitopt.target=j+1;
					int c1[] = new int[1];
					int c2[] = new int[1];
					dytex_mix_split(dtm,splitopt,c1,c2);

					blank.set(j,0.0);						
				}
			}	

			elapsedtime=System.currentTimeMillis()-starttime;
			if(FlagVerbose == Verbose_mode.VERBOSE)
			{
				System.out.println("Elapsed Time: " + elapsedtime + " milliseconds");
			}

			iter=iter+1;  //finish current EM iteration
		}
		
		//End of EM loop
		if(FlagVerbose != Verbose_mode.QUIET)
		{
			System.out.print("alpha= ");
			for(i=0;i<dtm.alpha.size();i++)
				System.out.print(dtm.alpha.get(i)+"  ");

			System.out.println();
		}
		
	}

	/*
	 * ! \brief convert a movie or video clip into a 2d matrix
	 * 
	 * \param Y movie or video clip
	 * 
	 * \param sx original size x of movie
	 * 
	 * \param sy original size y of movie
	 * 
	 * \param sc color space of movie
	 * 
	 * \param cnorm color space of movie
	 * 
	 * \returns converted video
	 * 
	 * convert 3D/4D to column vectors
	 * 
	 * \remarks used by runEM
	 * 
	 * \see runEM | extractY
	 */
	Mat convertmovie(Mat Y[], int sx[], int sy[], int sc[], int cnorm[]) {
		Mat Yout = new Mat();
		if (Y.length >= 2) {
			sy[0] = Y[0].rows;
			sx[0] = Y[0].cols;
			sc[0] = 1;
			Yout.create(sy[0] * sx[0], Y.length, Y[0].type);
			// copy data column wise
			int index = 0;
			int index2 = 0;
			for (int k = 0; k < Y[0].cols; k++) {
				for (int j = 0; j < Y[0].rows; j++) {
					index = 0;
					for (int i = 0; i < Y.length; i++) {
						Yout.double2D[index2][index] = Y[i].double2D[j][k];
						index++;
					}
					index2++;
				}
			}
		}
		// double minV,maxV;
		// minMaxLoc(Yout,&minV,&maxV); // Yout is not created unless sc[0] = 1
		// if ((sc[0]==3) && (maxV<=1.0))
		// {
		// cnorm[0]=1;
		// }
		// else
		// {
		cnorm[0] = 0;
		// }
		return Yout;
	}

	/*
	 * ! \brief extract patches at inds locations from a single Mat format
	 * 
	 * \param Yin patches in Yit format.
	 * 
	 * \param inds ids of patches to get.
	 * 
	 * \returns individual patches
	 * 
	 * \see Separate items with the '|' character.
	 */
	Vector<Mat> extractY(Mat Yin[], Vector<Integer> inds) {
		Vector<Mat> Yout = new Vector<Mat>();
		int dy = Yin[0].rows;
		int tau = Yin.length;

		for (int i = 0; i < inds.size(); i++) {
			Mat tmpM = new Mat(dy, tau, Yin[0].type);
			// copy data
			for (int j = 0; j < dy; j++) {
				for (int k = 0; k < tau; k++) {
					tmpM.double2D[j][k] = Yin[k].double2D[j][inds.get(i)];
				}
			}
			Yout.add(tmpM);
		}

		return Yout;
	}

	/*
	 * ! \brief use suboptimal method to learn a single DT from a set of videos
	 * 
	 * \param Y Input patches.
	 * 
	 * \param param DT learning options.
	 * 
	 * \returns learned DT
	 * 
	 * \see Dytex
	 */
	Dytex initcluster_doretto(Vector<Mat> Y, DytexOptions param) {
		Dytex mydt = new Dytex(param);
		learnLeastSquares(mydt, Y);
		return mydt;
	}

	// learning (multiple videos)
	void learnLeastSquares(Dytex mydt, Vector<Mat> videos) {
	  int r,c;
	  // --- process video ---
	  int nump = videos.size(); // number of videos
	  int tau[] = new int[nump];				// length of each video

	  int inds[] = new int[nump + 1];		// index boundaries for each video

	  int tauall;

	  inds[0] = 0;

	  // figure out the size of all videos
	  for (int p=0; p<nump; p++) {
	    int mytau = 0;
	    if (videos.get(p).dims == 3)
	      mytau = videos.get(p).size[0];
	    else if (videos.get(p).dims == 2)
	      mytau = videos.get(p).cols;
	    else {
	      MipavUtil.displayError("In learnLeastSquares unknown size");
	      System.exit(-1);
	    }
	    
	    tau[p]    = mytau;
	    inds[p+1] = inds[p]+mytau;    
	  }
	  tauall = inds[nump];

	  //cout << "tau = "; for (int p=0; p<nump; p++) cout << tau[p] << " "; cout << "\n";
	  //cout << "inds = "; for (int p=0; p<nump; p++)  cout << inds[p] << " "; cout << "\n";

	  // allocate space
	  Mat Yall = new Mat(mydt.dtopt.m, tauall, CV_64F);
	  Mat tmp1 = new Mat();
	  
	  //cout << "Yall: "; dumpMatSize(Yall);

	  // initialize Ymean accumulator
	  if (mydt.dtopt.Yopt == Ymean_type.NONZERO_YMEAN) {
		  for (r = 0; r < mydt.Ymean.rows; r++) {
	          mydt.Ymean.double2D[r][0] = 0.0;
		  }
	  }

	  // for each video, process it, compute and subtract the mean
	  for (int p=0; p<nump; p++) { 
		// get video p in Yall
		  Mat Yall_p = new Mat(mydt.dtopt.m, inds[p+1]-inds[p], CV_64F);
		  for (r = 0; r < mydt.dtopt.m; r++) {
			  for (c = inds[p]; c < inds[p+1]; c++) {
				  Yall_p.double2D[r][c-inds[p]] = Yall.double2D[r][c];
			  }
		  }
	    //System.out.println("Yall_p: " +  dumpMatSize(Yall_p));

	    processVideoTraining(mydt, videos.get(p), Yall_p, tmp1);

	    // update accumulator
	    if (mydt.dtopt.Yopt == Ymean_type.NONZERO_YMEAN) {
	    	for (r = 0; r < mydt.Ymean.rows; r++) {
	            mydt.Ymean.double2D[r][0] += tmp1.double2D[r][0];
	    	}
	    }
	  }

	  // normalize mean
	  if (mydt.dtopt.Yopt == Ymean_type.NONZERO_YMEAN) {
		for (r = 0; r < mydt.Ymean.rows; r++) {
	        mydt.Ymean.double2D[r][0] /= nump;
		}
	  }

	  // --- special case when n=0 ---
	  if (mydt.dtopt.n == 0) {
	    // only need to calculate R
	    calculate(mydt.R,Yall, true);
		tau = null;
		inds = null;
	    return;
	  }

	  // --- do PCA on Yall ---
	  Mat Xall;
	  if (Yall.rows > Yall.cols) {
	    // ... using SVD
	    //cout << "lls:use SVD\n";
	    Matrix YallMat = new Matrix(Yall.double2D);
		SingularValueDecomposition svd = new SingularValueDecomposition(YallMat);
		Mat matU = new Mat(svd.getU().getArray());
		// C = u(1:n)
		mydt.C.create(matU.rows, mydt.dtopt.n, CV_64F);
	    for (r = 0; r < matU.rows; r++) {
	    	for (c = 0; c < mydt.dtopt.n; c++) {
	    		mydt.C.double2D[r][c] = matU.double2D[r][c];
	    	}
	    }
	    //cout << "singular values: " << svd.w << "\n";

	    // X = w(1:n)*vt(1:n,:)
	    Mat matVT = new Mat(svd.getV().transpose().getArray());
	    Xall = new Mat(mydt.dtopt.n,matVT.cols,CV_64F);
	    for (r = 0; r < mydt.dtopt.n; r++) {
	    	for (c = 0; c <matVT.cols; c++) {
	    		Xall.double2D[r][c] = matVT.double2D[r][c];
	    	}
	    }
		double singularValues[] = svd.getSingularValues();
		double scaleFactor;
		for (r = 0; r < mydt.dtopt.n; r++) {
		    scaleFactor = singularValues[r];
		    for (c = 0; c < Xall.cols; c++) {
		    	Xall.double2D[r][c] *= scaleFactor;
		    }
		}

	    //Mat Xall2 = C.t()*Yall;   // X = C'*Yall    
	    //cout << "Xall err=" << norm(Xall, Xall2) << "\n";

	  } else {
	    // ... using eig
	    //cout << "lls:use eig\n";

	    Mat tmp = new Mat();
	    mulTransposed(Yall, tmp, false);
	    Matrix tmpMat = new Matrix(tmp.double2D);
	    SingularValueDecomposition svd = new SingularValueDecomposition(tmpMat);
		Mat matU = new Mat(svd.getU().getArray());
	    // C = u(1:n)
	    mydt.C.create(matU.rows, mydt.dtopt.n, CV_64F);
	    for (r = 0; r < matU.rows; r++) {
	    	for (c = 0; c < mydt.dtopt.n; c++) {
	    		mydt.C.double2D[r][c] = matU.double2D[r][c];
	    	}
	    }
	    
	    //cout << "singular values: " << svd.w << "\n";

	    // X = C'*Yall
	    Xall = times(transpose(mydt.C),Yall);
	  }

	  // --- estimate initial condition ---
	  if (nump == 1) {
	    // only one video (just copy it)
		for (r = 0; r < Xall.rows; r++) {
			mydt.mu0.double2D[r][0] = Xall.double2D[r][0];
		}
		for (r = 0; r < mydt.S0.mtx.rows; r++) {
			for (c = 0; c < mydt.S0.mtx.cols; c++) {
	            mydt.S0.mtx.double2D[r][c] = 0.0;
			}
		}

	  } else {
	    // extract first X from each image and store in Xall0
	    Mat Xall0 = new Mat(mydt.dtopt.n, nump, CV_64F);
	    Mat Xall0c = new Mat(mydt.dtopt.n,1,CV_64F);
	    for (int p=0; p<nump; p++) {
	     for (r = 0; r < mydt.dtopt.n; r++) {
	          Xall0c.double2D[r][0] = Xall0.double2D[r][p];
	     }
	     for (r = 0; r < mydt.dtopt.n; r++) {
	          Xall0c.double2D[r][0] = Xall.double2D[r][inds[p]];
	     }
	    }
	    // estimate the initial mean
	    reduce(Xall0, mydt.mu0, 1, CV_REDUCE_AVG);
	    // estimate the initial covariance
	    for (int p=0; p<nump; p++) {
	      for (r = 0; r < mydt.dtopt.n; r++) {
	           Xall0.double2D[r][p] -= mydt.mu0.double2D[r][0];     // subtract inital mean
	      }
	    }
	    calculate(mydt.S0,Xall0, true); // calculate covariance
	  }
	  
	  // --- estimate A ---
	  Mat Phi1all; // store X(1:end-1)
	  Mat Phi2all; // store X(2:end)

	  if (nump == 1) {
	    // only one video, just make pointers (no memory copied)
		Phi1all = new Mat(Xall.rows,Xall.cols-1,CV_64F);
		for (r = 0; r < Xall.rows; r++) {
			for (c = 0; c < Xall.cols-1; c++) {
				Phi1all.double2D[r][c] = Xall.double2D[r][c];
			}
		}
	    Phi2all = new Mat(Xall.rows, Xall.cols-1,CV_64F);
	    for (r = 0; r < Xall.rows; r++) {
			for (c = 1; c < Xall.cols; c++) {
				Phi2all.double2D[r][c-1] = Xall.double2D[r][c];
			}
		}

	  } else {
	    // copy to Phi1all and Phi2all
	    // allocate space
		Phi1all = new Mat();
	    Phi1all.create(mydt.dtopt.n, tauall-nump, CV_64F); // store X(1:end-1)
	    Phi2all = new Mat();
	    Phi2all.create(mydt.dtopt.n, tauall-nump, CV_64F); // store X(2:end)

	    //cout << "Phi1all: "; dumpMatSize(Phi1all);
	    //cout << "Phi2all: "; dumpMatSize(Phi2all);

	    // for each video
	    for (int p=0; p<nump; p++) {
	      Mat Phi1 = new Mat(Xall.rows,inds[p+1]-1-inds[p],CV_64F);
	      for (r = 0; r < Xall.rows; r++) {
	    	  for (c = inds[p]; c < inds[p+1]-1; c++) {
	    		  Phi1.double2D[r][c-inds[p]] = Xall.double2D[r][c];
	    	  }
	      }
	      Mat pPhi1all = new Mat(Phi1all.rows,inds[p+1]-(p+1)-(inds[p]-p),CV_64F);
	      for (r = 0; r < Phi1all.rows; r++) {
	    	  for (c = inds[p]-p; c < inds[p+1]-(p+1); c++) {
	    		  pPhi1all.double2D[r][c-(inds[p]-p)] = Phi1all.double2D[r][c];
	    	  }
	      }
	      //cout << "Phi1: "; dumpMatSize(Phi1);
	      //cout << "pPhi1all: "; dumpMatSize(pPhi1all);
	      copyTo(Phi1,pPhi1all);
	      
	      Mat Phi2 = new Mat(Xall.rows,inds[p+1]-(inds[p]+1),CV_64F);
	      for (r = 0; r < Xall.rows; r++) {
	    	  for (c = inds[p]+1; c < inds[p+1]; c++) {
	    		  Phi2.double2D[r][c-(inds[p]+1)] = Xall.double2D[r][c];
	    	  }
	      }
	      Mat pPhi2all = new Mat(Phi2all.rows,inds[p+1]-(p+1)-(inds[p]-p),CV_64F);
	      for (r = 0; r < Phi2all.rows; r++) {
	    	  for (c = inds[p]-p; c < inds[p+1]-(p+1); c++) {
	    		  pPhi2all.double2D[r][c-(inds[p]-p)] = Phi2all.double2D[r][c];
	    	  }
	      }
	      //cout << "Phi2: "; dumpMatSize(Phi2);
	      //cout << "pPhi2all: "; dumpMatSize(pPhi2all);
	      copyTo(Phi2,pPhi2all);
	    } // for (int p=0; p<nump; p++)
	  }

	  Mat iPhi1all;
	  Matrix PhiallMat = new Matrix(Phi1all.double2D);
	  iPhi1all = new Mat(PhiallMat.inverse().getArray());
	  mydt.A = times(Phi2all,iPhi1all);
	  
	  // --- estimate Q ---
	  Mat V = minus(Phi2all,times(mydt.A,Phi1all));
	  calculate(mydt.Q,V, false);
	  
	  // --- estimate R ---
	  Mat errory = minus(Yall,times(mydt.C,Xall));
	  calculate(mydt.R,errory, true);   // (to match matlab)

	  // clean up
	  tau = null;
	  inds = null;
	}

	// convert a video into a vector time-series
	// also subtract the empirical mean if necessary, and return it.
	void processVideoTraining(Dytex mydt, Mat vin, Mat Yout, Mat Yout_mean) {
		int r, c;
		Mat Ytmp = null;

		switch (vin.dims) {
		case 3:
			// vectorize
			Ytmp = vectorize(vin, true);

			// save/check video size
			if (mydt.vrows == 0 && mydt.vcols == 0) {
				mydt.vrows = vin.size[1];
				mydt.vcols = vin.size[2];
			} else {
				if (mydt.vrows != vin.size[1] || mydt.vcols != vin.size[2]) {
					MipavUtil.displayError(
							"In processVideoTraining mydt.vrows != vin.size[1] || mydt.vcols != vin.size[2]");
					System.exit(-1);
				}
			}
			break;

		case 2:
			// already vectorized
			Ytmp = vin;
			break;

		default:
			MipavUtil.displayError("Unsupported video dims (color) in processVideoTraining");
			System.exit(-1);
		}

		// cout << "Ytmp: "; dumpMatSize(Ytmp);

		// --- check dimensions ---
		// CV_Assert(Ytmp.rows == Yout.rows);
		// CV_Assert(Ytmp.cols == Yout.cols);

		// --- convert to double/float ---
		Yout = new Mat(Ytmp.rows, Ytmp.cols, CV_64F);
		if (Ytmp.type == CV_8U) {
			// cout << "llS convertTo float\n";
			Yout.double2D = new double[Ytmp.rows][Ytmp.cols];
			for (r = 0; r < Ytmp.rows; r++) {
				for (c = 0; c < Ytmp.cols; c++) {
					Yout.double2D[r][c] = (Ytmp.byte2D[r][c] & 0xff);
				}
			}
		} else {
			copyTo(Ytmp, Yout); // copy to Yall
		}

		// --- compute the mean for the video ---
		switch (mydt.dtopt.Yopt) {
		case NONZERO_YMEAN:
			// compute the mean
			reduce(Yout, Yout_mean, 1, CV_REDUCE_AVG);
			// subtract mean from Y
			for (c = 0; c < Yout.cols; c++) {
				for (r = 0; r < Yout.rows; r++) {
					Yout.double2D[r][c] -= Yout_mean.double2D[r][0];
				}
			}
			break;

		case ZERO_YMEAN:
			// do nothing
			break;
		default:
			MipavUtil.displayError("In processVideoTraining bad Yopt");
			System.exit(-1);
		}
	}

	Mat vectorize(Mat vid, boolean flag_col) {
		int r;
		if (vid.dims != 3) {
			MipavUtil.displayError("vid.dims = " + vid.dims + "instead of the required 3 in vectorize");
			System.exit(-1);
		}

		// dumpMatSize(vid);
		Mat vv = new Mat();

		// column-wise frames
		if (flag_col) {
			vv.create(vid.size[1] * vid.size[2], vid.size[0], vid.type);
			// dumpMatSize(vv);

			// copy each frame into a column
			// TODO: could be faster?
			for (int f = 0; f < vid.size[0]; f++) {
				Mat myframe = frame(vid, f);

				// copy column-wise
				for (int y = 0; y < myframe.cols; y++) {
					// get column part in vv
					Mat vvcol = new Mat(vid.size[1], 1, CV_64F);
					for (r = y * vid.size[1]; r < (y + 1) * vid.size[1]; r++) {
						vvcol.double2D[r - y * vid.size[1]][0] = vv.double2D[r][f];
					}
					// dumpMatSize(vvcol);

					// get column part of frame
					Mat mycol = new Mat(myframe.rows, 1, CV_64F);
					for (r = 0; r < myframe.rows; r++) {
						mycol.double2D[r][0] = myframe.double2D[r][y];
					}
					// dumpMatSize(mycol);

					// copy
					copyTo(mycol, vvcol);
				}
			}

		} else {
			MipavUtil.displayError("not implemented in vectorize");
			System.exit(-1);
		}

		return vv;
	}

	// calculate a covariance matrix from data
	void calculate(CovMatrix cov, Mat data, boolean use_unbiased) {
		int r, c;
		int N = 0;

		// check data
		if (data.rows != cov.n) {
			MipavUtil.displayError("data.rows != cov.n in caclulate");
			System.exit(-1);
		}

		// calculate sum
		switch (cov.covopt) {
		case COV_FULL:

			mulTransposed(data, cov.mtx, false); // data*data^T
			N = data.cols;
			break;

		case COV_DIAG:
		case COV_IID: {
			Mat tmp = new Mat();
			// square: data.*data
			tmp = elementTimes(data, data);

			if (cov.covopt == cov_type.COV_IID) {
				// sum everything
				cov.mtx = new Mat(1, 1, CV_64F);
				for (r = 0; r < tmp.rows; r++) {
					for (c = 0; c < tmp.cols; c++) {
						cov.mtx.double2D[0][0] += tmp.double2D[r][c];
					}
				}
				N = data.cols * data.rows;
			} else {
				// sum over rows
				reduce(tmp, cov.mtx, 1, CV_REDUCE_SUM);
				N = data.cols;
			}
		}
			break;

		default:
			MipavUtil.displayError("In calculate unknown cov");
			System.exit(-1);
		}

		// average
		if (use_unbiased)
			divide(cov.mtx, (double) (N - 1));
		else
			divide(cov.mtx, (double) N);
	}

	void mulTransposed(Mat src, Mat dst, boolean transposeFirst) {
		int c1, c2, r, r1, r2, c;
		if (transposeFirst) {
			dst.create(src.cols, src.cols, CV_64F);
			for (c1 = 0; c1 < src.cols; c1++) {
				for (c2 = 0; c2 < src.cols; c2++) {
					for (r = 0; r < src.rows; r++) {
						dst.double2D[c1][c2] += (src.double2D[r][c1] * src.double2D[r][c2]);
					}
				}
			}
		} else {
			dst.create(src.rows, src.rows, CV_64F);
			for (r1 = 0; r1 < src.rows; r1++) {
				for (r2 = 0; r2 < src.rows; r2++) {
					for (c = 0; c < src.cols; c++) {
						dst.double2D[r1][r2] += (src.double2D[r1][c] * src.double2D[r2][c]);
					}
				}
			}
		}
	}
	
	 public enum kf_mode {KF_CACHE,       /**< cache filter matrices. */
			KF_COMPUTE_WITH_CACHE,      /**< compute LL from cache. */
			KF_COMPUTE_WITHOUT_CACHE    /**< compute LL w/o using cache. */
			};
			
			/** class for running the Kalman filter for dynamic texture inference.
		    There are two ways to use the class:
		    1) instantiate with a DT to create a Kalman filter where most matrices are 
		       pre-computed.  The KF instance can then be used for fast inference;
		    2) use the static functions to compute inference on a DT.
		    In general, the first method should be used if the DT will not change and
		    the KF will be used many times.  The second method should be used if 
		    inference is required only once before the DT changes.
		*/
		class DytexKalmanFilter
		{
		  // cache for instantiated Kalman filter
		  public Mat           cache_Kt[];     /**< cache for an instantiated Kalman filter: Kalman gain matrices [n x m x tau]. */
		  public Mat           cache_detMt;  /**< cache for an instantiated Kalman filter: determinant term [1 x tau]. */
		  public Mat           cache_invMt[];  /**< cache for an instantiated Kalman filter: inverse covariance matrix [m x m x tau]. */
		  public Dytex         cache_dt;    /**< cache for an instantiated Kalman filter: DT reference (use a pointer). */
		  
		// note this requires that you keep dt around!
		public DytexKalmanFilter(Dytex dt, int tau)
		  {
		    // initialize the Kalman cache
		    cache_Kt       = create(tau, dt.dtopt.n, dt.dtopt.m, CV_64F);
		    cache_detMt    = new Mat(1, tau, CV_64F);
		    cache_invMt    = create(tau, dt.dtopt.m, dt.dtopt.m, CV_64F);
		    cache_dt       = dt;

		    // create cache
		    loglike_internal(dt, null, null, tau, cache_Kt, cache_detMt, cache_invMt, false, kf_mode.KF_CACHE);
		  }
		
		// do one of the following:
	//   1) compute log-likelihood of videos under dt 
	//   2) cache kalman filter matrices for a dt
	//   3) compute log-likelihood using kalman filter cache
	//
	// result stored in lik (1 x size(videos))
	// mahal=true will compute Mahalanobis distance instead LL
	// this follows dytex_kalmanff_ll
	// note: it computes the inverse directly, so not for big videos.
//	       this is faster when the videos are small
	void loglike_internal(Dytex dt, Vector<Mat> videos, Mat lik, 
						 int cache_tau, Mat cache_Kt[], Mat cache_detMt, Mat cache_invMt[], 
						 boolean mahal, kf_mode kfmode) {
	  int i, r, c;

	  // process videos (vectorize and subtract mean)
	  int YN = (kfmode == kf_mode.KF_CACHE ? 0 : videos.size());

	  Mat Y[] = new Mat[YN];
	  for (i = 0; i < YN; i++) {
		  Y[i] = new Mat();
	  }
	  //Mat Y[YN];
	  int tau = 0;
	  
	  if (kfmode == kf_mode.KF_CACHE)
	    tau = cache_tau;

	  else {
	    // process videos
	    for (i=0; i<YN; i++) {
	      processVideoTesting(dt,videos.get(i), Y[i]);
	      
	      // check that all tau are equal
	      // [TODO: make it so we can handle different tau]
	      if (i==0)
		tau = Y[i].cols;
	      else
		    if (!(tau == Y[i].cols)) {
		    	MipavUtil.displayError("!(tau == Y[i].cols) in loglike_internal");
		    	System.exit(-1);
		    }
	    }  

	    // make sure there is enough cache
	    if (kfmode == kf_mode.KF_COMPUTE_WITH_CACHE)
	      if(!(tau <= cache_Kt.length)) {
	    	  MipavUtil.displayError("!(tau <= cache_kt.length) in loglike_internal)");
	    	  System.exit(-1);
	      }
	  }

	  // setup matrices for inversion lemma (when computing Kalman filter matrices)
	  Mat iRf = null;
	  Mat CRC = null;
	  Mat RC = new Mat();
	  if (kfmode == kf_mode.KF_CACHE || kfmode == kf_mode.KF_COMPUTE_WITHOUT_CACHE) {
	    CovMatrix iR = inv(dt.R);
	    iRf = toFullMatrix(iR);
	    postmultiply(iR,dt.C,RC);
		CRC = times(transpose(dt.C),RC);
	  }

	  // initialize covariance matrices
	  Mat Vt1t = toFullMatrix(dt.S0);  // Vt1t = S0
	  Mat Vtt = new Mat(dt.dtopt.n, dt.dtopt.n, CV_64F);


	  // initialize hidden states (if computing LL)
	  Mat xtt = null;
	  Mat YCx, Kt;
	  Mat term2 = null;
	  Mat icv = new Mat();
	  Mat xt1t = new Mat();
	  if (kfmode == kf_mode.KF_COMPUTE_WITH_CACHE || kfmode == kf_mode.KF_COMPUTE_WITHOUT_CACHE) {
	    // xt1t = mu0
	    if (YN == 1)
	      copyTo(dt.mu0,xt1t);
	    else
	      repeat(dt.mu0, 1, YN, xt1t);
	    
	    xtt = new Mat(dt.dtopt.n, YN, CV_64F);
	    lik.create(1, YN, CV_64F);
	    term2 = new Mat(1, YN, CV_64F);
	    YCx = new Mat(dt.dtopt.m, 1, CV_64F);
	  }
	  
	  // initialize Kalman gain (if not cached)
	  if ((kfmode == kf_mode.KF_CACHE) || (kfmode == kf_mode.KF_COMPUTE_WITHOUT_CACHE)) {
	    Kt = new Mat(dt.dtopt.n, dt.dtopt.m, CV_64F);
	    icv = new Mat(dt.dtopt.m, dt.dtopt.m, CV_64F);
	  }

	  double detterm = 0.0;  

	  // run forward pass
	  for (int t=0; t<tau; t++) {  

	    /*
	    cout << "--- t=" << t << " ---\n";
	    cout << "xt1t = " << xt1t << "\n";
	    cout << "Vt1t = " << Vt1t << "\n";
	    waitKey(1000);
	    */

	    if (kfmode == kf_mode.KF_COMPUTE_WITH_CACHE) {
	      // recall the cache
	      Kt  = cache_Kt[t];
	      icv = cache_invMt[t];
	      detterm = cache_detMt.double2D[0][t];
	      
	    } else {
	      // compute kalman gain, etc
	      
	      if (t>0) {
		// update step:  Vt1t = A*Vtt*A' + Q;
		Vt1t = plus(times(times(dt.A,Vtt),transpose(dt.A)),dt.Q.mtx);
	      }

	      // compute inverse
	      switch(dt.dtopt.Ropt) {
	      case COV_IID:
	      case COV_DIAG:
		// all zeros?
		if (countNonZero(Vt1t) == 0) {
		  //icv = iRf;
		  copyTo(iRf,icv); // [TODO: make more efficient]
		} else {
		  // tmpS = inv(inv(Vt1t) + CRC);
		  Mat Vt1tinv = new Mat(new Matrix(Vt1t.double2D).inverse().getArray());
		  Mat tmpSinv = plus(Vt1tinv,CRC);
		  Mat tmpS = new Mat(new Matrix(tmpSinv.double2D).inverse().getArray());
		  //icv  = iRf - RC*tmpS*RC';
		  icv = minus(iRf,times(times(RC,tmpS),transpose(RC)));
		}
		break;
	      case COV_FULL:
		// icv = inv(C*Vt1t*C' + dtkf.Rhat);
	    Mat icvinv = plus(times(times(dt.C,Vt1t),transpose(dt.C)),dt.R.mtx);
	    icv = new Mat(new Matrix(icvinv.double2D).inverse().getArray());
		break;
	      }

	      // Kt  = Vt1t*C'*icv;
	      Kt = times(times(Vt1t,transpose(dt.C)),icv);

	      // Vtt = Vt1t - Kt*C*Vt1t;
	      Vtt = minus(Vt1t,times(times(Kt,dt.C),Vt1t));
	      
	      if ((kfmode == kf_mode.KF_CACHE) || !mahal) {
		// term1 = 0.5*logdet(icv);             % the logdet term
	    Matrix icvMat = new Matrix(icv.double2D);
	    double icvdet = icvMat.det();
		detterm = 0.5*Math.log(icvdet);
		// TODO: custom logdet function
		
		// term1 = term1 - 0.5*(dy*log(2*pi));  % the Gaussian constant
		detterm -= 0.5*(dt.dtopt.m*Math.log(2*Math.PI));
	      }

	      // store cache
	      if (kfmode == kf_mode.KF_CACHE) {
		Mat tmp1 = cache_Kt[t];
		copyTo(Kt,tmp1);
		Mat tmp2 = cache_invMt[t];
		copyTo(icv,tmp2);
		cache_detMt.double2D[0][t] = detterm;
	      }
	    }

	    if (kfmode == kf_mode.KF_COMPUTE_WITH_CACHE || kfmode == kf_mode.KF_COMPUTE_WITHOUT_CACHE) {
	      if (t>0) {
		// update step: xt1t = A*xtt;
		xt1t = times(dt.A,xtt);
	      }

	      // TODO: can be sped up using Yit format?
	      for (i=0; i<YN; i++) {
		// YCx = Y(:,:,t) - C*xt1t;  
	   
		YCx =minus(col(Y[i],t),times(dt.C,col(xt1t,i)));
		
		// xtt = xt1t + Kt*YCx;
		Mat rs = plus(col(xt1t,i),times(Kt,YCx));
		for (r = 0; r < xtt.rows; r++) {
		    xtt.double2D[r][i] = rs.double2D[r][0];	
		}
		
		// mahal dist
		// term2 = sum((icv*YCx).*YCx, 1);
		rs = times(times(transpose(YCx),icv),YCx);
		for (r = 0; r < term2.rows; r++) {
			term2.double2D[r][i] = rs.double2D[r][0];
		}
	      }
	    
	      // compute Mahalanobis
	      if (mahal) {
		      lik = plus(lik,term2);
		
	      } else {
		// compute Log-likelihood

		// L     = L - 0.5*term2 + term1;
	    	  for (r = 0; r < term2.rows; r++) {
	    		  for (c = 0; c < term2.cols; c++) {
	    			   lik.double2D[r][c] += -0.5*term2.double2D[r][c] + detterm;  
	    		  }
	    	  }
	      }
	    }

	  } // end for t

	  
	  // clean up
	  if( Y != null ) {
		  Y = null;
	  }
	}

		}
		 
			  //conditional state inference using Kalman smoothing filter
		/*!
		 * \brief
		 * conditional state inference using Kalman smoothing filter.
		 * 
		 * \param Y
		 * cell array of video in format Y(:,i,t) w/ mean subtracted
		 * 
		 * \param dt
		 * dynamic texture.
		 * 
		 * \param di3opt
		 * explicitly subtract mean or not.
		 * 
		 * \param xt
		 * output: state expectation: E(x_t|y_1,...y_tau); (cell array entries for each sequence Y)
		 * 
		 * \param Vt
		 * state covariance: cov(x_t|y_1,...,y_tau); (3-dim matrix)
		 * 
		 * \param Vt1
		 * state covariance: cov(x_t,x_{t-1}|y_1,...,y_tau); (3-dim matrix)
		 * 
		 * \param L
		 * vector of log-likelihoods of videos in Y
		 * 
		 * 
		 * \remarks
		 * Calculates loglikelihood as well as E-Step statistics
		 * 
		 * \see
		 * DytexMix
		 */
	    public void dytex_kalman(Mat Y2[], Dytex dt, String opt,Vector<Mat> xt,Mat Vt[],Mat Vt1[],Mat L) {
	    	int i,r,c;
	    	double value;
	    	Mat Y[] = new Mat[Y2.length];
	    	for (i = 0; i < Y.length; i++) {
	    		Y[i] = new Mat();
	    	}
	    	copyTo(Y2,Y);

	    	//Set output modes
	    	boolean doL=true; //compute logliklihood
	    	boolean doX=true; //also compute stats

	    	Mat A=dt.A;
	    	Mat C=dt.C;
	    	Mat Q=dt.Q.mtx;
	    	int dx=C.cols;
	    	int dy=C.rows;

	    	Mat Cu;
	    	Mat Cs = null;
	    	Mat Cv = null;
	    	Mat R;
	    	Mat iR = null;
	    	Mat CR = null;
	    	Mat CRC = null;
	        cov_type Ropt = cov_type.COV_IID;
	    	Mat mu0 = null;
	    	Mat V0 = null;
	    	if(dt.Q.n!=0)
	    	{
	    		Ropt=dt.R.covopt;
	    		switch(Ropt)
	    		{
	    			case COV_IID:
	    				R=dt.R.mtx;
	    				iR = new Mat(R.rows,R.cols,CV_64F);
	    				for (r = 0; r < R.rows; r++) {
	    					for (c = 0; c < R.cols; c++) {
	    						iR.double2D[r][c] = 1.0/R.double2D[r][c];
	    					}
	    				}
	    				CR=times(transpose(C),iR.double2D[0][0]);
	    				CRC=times(CR,C);
	    				if(doL)
	    				{
	    					Mat tmpM = new Mat(iR.rows,iR.cols,CV_64F);
	    					for (r = 0; r < iR.rows; r++) {
	    						for (c = 0; c < iR.cols; c++) {
	    							tmpM.double2D[r][c] = Math.sqrt(iR.double2D[r][c]);
	    						}
	    					}
	    					Mat tmpM1=times(C,tmpM.double2D[0][0]);
	    					Matrix tmpM1Matrix = new Matrix(tmpM1.double2D);
	    					SingularValueDecomposition svd = new SingularValueDecomposition(tmpM1Matrix);
	    					Matrix matV = svd.getV();
	    					double singularValues[] = svd.getSingularValues();
	    					Matrix matU = svd.getU();
	    					Cu = new Mat(matU.getArray());	
	    					Cv= new Mat(matV.getArray());
	    					Cs= new Mat(singularValues.length,singularValues.length,CV_64F);
	    					for (r = 0; r < singularValues.length; r++) {
	    						Cs.double2D[r][r] = singularValues[r];
	    					}	
	    				}
	    				else
	    				{
	    					Cv = new Mat(1,1,CV_64F);
	    					Cv.double2D[0][0] = 1.0;
	    					Cs = new Mat(1,1,CV_64F);
	    					Cs.double2D[0][0] = 1.0;
	    				}
	    			break;

	    			case COV_DIAG:
	    				//TO DO
	    			break;

	    			case COV_FULL:
	    				//TO DO
	    			case COV_ILLEGAL:
	    			break;
	    		}

	    		mu0=dt.mu0;
	    		switch(dt.S0.covopt)
	    		{
	    			case COV_DIAG:
	    				V0 = new Mat(dt.S0.mtx.rows,dt.S0.mtx.rows,dt.S0.mtx.type);
	    				for (r = 0; r < dt.S0.mtx.rows; r++) {
	    					V0.double2D[r][r] = dt.S0.mtx.double2D[r][0];
	    				}	
	    			break;
	    			case COV_FULL:
	    				V0=dt.S0.mtx;				
	    			break;
	    			default:
	    				MipavUtil.displayError("bad Sopt");
	    				System.exit(-1);
	    			break;
	    		}
	    	}

	    	Mat Ymean=dt.Ymean;

	    	// already in Y(:,i,t) format (mean subtracted too)
	    	int YN=Y[0].cols;
	    	int tau=Y.length;

	    	// explicitly subtract mean
	    	if(opt.substring(0,1).equalsIgnoreCase("y"))
	    	{
	    		for(int t=0;t<tau;t++)
	    		{
	    			Mat tmpM = new Mat();
	    			repeat(Ymean,1,YN,tmpM);
	    			Y[t]=minus(Y[t],tmpM);
	    		}

	    	}
	    	
	    	// special case: n=0
	    	if  (dt.Q.n==0)  
	    	{
	    		//TO DO
	    	}

	    	//OpenCV implementation of Kalman Filter

	    	//%%% initialize storage %%%%%%%%%%%%%%%%%%%%%%%%%	
	    	Mat xt1t[] = new Mat[tau];
	    	for (i = 0; i < tau; i++) {
	    		xt1t[i] = new Mat(dx,YN,CV_64F);
	    	}
	    	Mat xtt[] = new Mat[tau];
	    	for (i = 0; i < tau; i++) {
	    		xtt[i] = new Mat(dx,YN,CV_64F);
	    	}
	    	Mat Vt1t[] = new Mat[tau];
	    	for (i = 0; i < tau; i++) {
	    		Vt1t[i] = new Mat(dx,dx,CV_64F);
	    	}	
	    	Mat Vtt[] = new Mat[tau];
	    	for (i = 0; i < tau; i++) {
	    		Vtt[i] = new Mat(dx,dx,CV_64F);
	    	}
	    	
	    	Mat Kt;
	    	if( (Ropt!=cov_type.COV_IID) && (Ropt!=cov_type.COV_DIAG) )
	    	{
	    		Kt=new Mat(dx,dy,CV_64F);
	    	}
	    	Mat L1= new Mat(1,YN,CV_64F);

	    	 //%%% initialize forward pass %%%%%%%%%%%%%%%%%%%%
	    	Mat tmpM=xt1t[0];
	    	Mat tmpM2 = new Mat(dx,1,CV_64F);
	    	for(i=0;i<YN;i++)
	    	{
	    		for (r = 0; r < dx; r++) {
	    			tmpM.double2D[r][i] = mu0.double2D[r][0];
	    		}
	    	}
	    	tmpM=Vt1t[0];
	    	copyTo(V0,tmpM);


	    	Mat Phi = new Mat();
	    	//RUN Forward Pass
	    	for(int t=0;t<tau;t++)
	    	{
	    		if(t>0)
	    		{
	    			tmpM=xt1t[t];
	    			tmpM2=times(A,xtt[t-1]);
	    			copyTo(tmpM2,tmpM);

	    			tmpM=Vt1t[t];
	    			tmpM2=plus(times(times(A,Vtt[t-1]),transpose(A)),Q);
	    			copyTo(tmpM2,tmpM);
	    		}

	    		
	    		if( (Ropt==cov_type.COV_IID) || (Ropt==cov_type.COV_DIAG) )
	    		{
	    			//Using Matrix iNversion lemma
	    			tmpM=Vt1t[t];
	    			
	    			//check if any element is zero 
	    			boolean flag=true;
	    			for(i=0;i<tmpM.rows;i++)
	    				for(int j=0;j<tmpM.rows;j++)
	    				{
	    					if(tmpM.double2D[i][j]!=0)
	    						flag=false;
	    				}
	    			if(flag==true)
	    			{
	    				copyTo(tmpM,Phi);
	    			}
	    			else
	    			{
	    				Mat tmpMinv = new Mat((new Matrix(tmpM.double2D)).inverse().getArray());
	    				tmpM2=plus(tmpMinv,CRC);
	    				Mat tmpM3=new Mat((new Matrix(tmpM2.double2D)).inverse().getArray());
	    			    copyTo(tmpM3,Phi);
	    			}
	    			
	    			// expansion using matrix inversion lemma...
	    			Mat CYCx= minus(times(CR,Y[t]),times(CRC,xt1t[t]));
	    			xtt[t]=minus(plus(xt1t[t],times(Vt1t[t],CYCx)),times(times(times(Vt1t[t],CRC),Phi),CYCx));						

	    			// expansion using matrix inversion lemma...
	    			tmpM=Vt1t[t];
	    		    Vtt[t]=plus(minus(tmpM,times(times(tmpM,CRC),tmpM)),times(times(times(times(tmpM,CRC),Phi),CRC),tmpM));
	    			tmpM2=Vtt[t];

	    			//% compute the log-likelihood efficiently: p(y_t | y_1,...,y_{t-1})
	    			if(doL)
	    			{
	    				Q = times(times(times(times(Cs,transpose(Cv)),Vt1t[t]),Cv),Cs);
	    				double[] eigenvalue = new double[Q.cols];
	    				double[][] eigenvector = new double[Q.rows][Q.cols];
	    				Eigenvalue.decompose(Q.double2D, eigenvector, eigenvalue);
	    				double E;
	    				double term1 = 0.0;
	    				for (i = 0; i < eigenvalue.length; i++) {
	    					E = Math.log(eigenvalue[i]+1.0);
	    					term1 += E;
	    				}
	    				
	    				
	    				Mat term2 = null;
	    				Mat YCx=minus(Y[t],times(C,xt1t[t]));
	    				if(Ropt==cov_type.COV_IID)
	    				{
	    					term1 = term1 - dy*Math.log(iR.double2D[0][0]);
	    					tmpM = new Mat(YCx.rows,YCx.cols,CV_64F);
	    					for (r = 0; r < YCx.rows; r++) {
	    						for (c = 0; c < YCx.cols; c++) {
	    							tmpM.double2D[r][c] = YCx.double2D[r][c]*YCx.double2D[r][c];
	    						}
	    					}				
	    					
	    					Mat abc2 = new Mat();
	    					reduce(tmpM,abc2,0,CV_REDUCE_SUM);
	    					tmpM=abc2;

	    					term2=times(tmpM,iR.double2D[0][0]);
	    				}
	    				else
	    				{
	    					MipavUtil.displayError("TO DO");
	    					System.exit(-1);
	    				}
	    								

	    				tmpM = times(times(Phi,CYCx),CYCx);				
	    				Mat abc2 = new Mat();
	    				reduce(tmpM,abc2,0,CV_REDUCE_SUM);
	    				tmpM=abc2;
	    				
	    				term2=minus(term2,tmpM);
	    				for (r = 0; r < L1.rows; r++) {
	    					for (c = 0; c < L1.cols; c++) {
	    						L1.double2D[r][c] = L1.double2D[r][c] - 0.5*(term1 + term2.double2D[r][c]);
	    					}
	    				}
	    				value = 0.5*(dy*Math.log(2.0*Math.PI)); //% the Gaussian constant
	    				for (r = 0; r < L1.rows; r++) {
	    					for (c = 0; c < L1.cols; c++) {
	    						L1.double2D[r][c] = L1.double2D[r][c] - value;
	    					}
	    				}
	    			}
	    		}
	    		else //for Ropt=full, we need to compute normally
	    		{
	    			MipavUtil.displayError("TO DO");
	    			System.exit(-1);
	    		}

	    	}

	    	if(!doX)
	    	{
	    		copyTo(L1,L);
	    		return;
	    	}
	    	else
	    	{
	    		//Backward pass
	    		Mat xthat[] = new Mat[tau];
	    		for (i = 0; i < tau; i++) {
	    			xthat[i] = new Mat(dx,YN,CV_64F);
	    		}
	    		Mat Vthat[] = new Mat[tau];
	    		for (i = 0; i < tau; i++) {
	    			Vthat[i] = new Mat(dx,dx,CV_64F);
	    		}
	    		Mat Vtt1hat[] = new Mat[tau-1];
	    		for (i = 0; i < tau-1; i++) {
	    			Vtt1hat[i] = new Mat(dx,dx,CV_64F);
	    		}
	    		Mat Jt[] = new Mat[tau];
	    		for (i = 0; i < tau; i++) {
	    			Jt[i] = new Mat(dx,dx,CV_64F);
	    		}

	    		//initialize		
	    		tmpM=xthat[tau-1];
	    		copyTo(xtt[tau-1],tmpM);

	    		tmpM=Vthat[tau-1];
	    		copyTo(Vtt[tau-1],tmpM);
	    		if( (Ropt==cov_type.COV_IID) || (Ropt==cov_type.COV_DIAG) )
	    		{
	    			Mat eyeMat = new Mat(dx,dx,CV_64F);
	    			for (i = 0; i < dx; i++) {
	    				eyeMat.double2D[i][i] = 1.0;
	    			}
	    			Vtt1hat[tau-2]=times(times(plus(minus(eyeMat,times(Vt1t[tau-1],CRC)),times(times(times(Vt1t[tau-1],CRC),Phi),CRC)),A),Vtt[tau-2]);
	    			tmpM=Vtt1hat[tau-2];
	    		}
	    		else
	    		{
	    			MipavUtil.displayError("TO DO");
	    			System.exit(-1);
	    		}

	    		for(int t=tau-1;t>0;t--)
	    		{
	    			tmpM=new Mat((new Matrix(Vt1t[t].double2D)).inverse().getArray());
	    			Jt[t-1]=times(times(Vtt[t-1],transpose(A)),tmpM);			
	    			xthat[t-1]= plus(xtt[t-1],times(Jt[t-1],(minus(xthat[t],times(A,xtt[t-1])))));
	    			Vthat[t-1] = plus(Vtt[t-1],times(times(Jt[t-1],(minus(Vthat[t],Vt1t[t]))),transpose(Jt[t-1])));
	    			
	    			if(t<tau-1)
	    			{
	    				Vtt1hat[t-1] = plus(times(Vtt[t],transpose(Jt[t-1])),times(times(Jt[t],(minus(Vtt1hat[t],times(A,Vtt[t])))),transpose(Jt[t-1])));
	    			}
	    			
	    		}
	    		//%%% convert xthat into cell array %%%
	    		Vector<Mat> xthatout = new Vector<Mat>();
	    		for(i=0;i<YN;i++)
	    		{
	    			tmpM = new Mat(dx,tau,CV_64F);
	    			for(int j=0;j<dx;j++)
	    				for(int k=0;k<tau;k++)
	    					tmpM.double2D[j][k]=xthat[k].double2D[j][i];

	    			xthatout.add(tmpM);
	    		}

	    		xt=xthatout;
	    		copyTo(Vthat,Vt);
	    		copyTo(Vtt1hat,Vt1);
	    		if(doL)
	    		    copyTo(L1,L);
	        }
	    }
	    
	    /*!
	     * \brief
	     * create cached Kalman filter for all DTs in the mixture.
	     * 
	     * \param tau
	     * length to compute.
	     *   
	     * \see
	     * DytexMix::getClasses(std::vector<Mat> Y,std::vector<int> & classes)
	     */
	    void setupKFB(DytexMix dtm, int tau)
	    {
	    	//Setup the kfb for each dt
	    	dtm.kfb.clear();
	    	for(int i=0;i<dtm.dt.size();i++)
	    	{
	    		DytexKalmanFilter kf = new DytexKalmanFilter(dtm.dt.get(i),tau);
	    		dtm.kfb.add(kf);
	    	}
	    }
	    
	 // convert a video into a vector time-series, subtract the Ymean in the dytex
	    void processVideoTesting(Dytex dt, Mat vin, Mat Yout) {
	      int r,c;
	      Mat Ytmp = null;
	      
	      switch(vin.dims) {
	      case 3:
	        // check video size
	        if (!(dt.vrows == 0 && dt.vcols == 0)) {
	          if (!(dt.vrows == vin.rows && dt.vcols == vin.cols)) {
	        	  MipavUtil.displayError("!(dt.vrows == vin.rows && dt.vcols == vin.cols) in processingVideoTesting");
	        	  System.exit(-1);
	          }
	        }
	        // vectorize
	        Ytmp = vectorize(vin, true);
	        break;
	        
	      case 2:
	        // already vectorized
	        Ytmp = vin;
	        break;
	        
	      default:
	        MipavUtil.displayError("Unsupported video dims (color) in processVideoTesting");
	        System.exit(-1);
	      }
	      
	      //cout << "Ytmp: "; dumpMatSize(Ytmp);

	      // --- check dimensions ---
	      //CV_Assert(Ytmp.rows == Yout.rows);
	      //CV_Assert(Ytmp.cols == Yout.cols);
	        
	      // --- convert to double/float ---
	      if (Ytmp.type == CV_8U) {
	        //cout << "llS convertTo float\n";
	  			// cout << "llS convertTo float\n";
	    		Yout.double2D = new double[Ytmp.rows][Ytmp.cols];
	  			for (r = 0; r < Ytmp.rows; r++) {
	  				for (c = 0; c < Ytmp.cols; c++) {
	  					Yout.double2D[r][c] = (Ytmp.byte2D[r][c] & 0xff);
	  				}
	  			}	
	      } else {
	        copyTo(Ytmp,Yout); // copy to Yall
	      }
	        
	      // --- subtract mean ---
	      switch (dt.dtopt.Yopt) {
	      case NONZERO_YMEAN:
	        // subtract mean from Y
	        for (int t=0; t<Yout.cols; t++) {
	          for (r = 0; r < dt.Ymean.rows; r++) {
	          Yout.double2D[r][t] -= dt.Ymean.double2D[r][0];
	          }
	        }
	        break;
	        
	      case ZERO_YMEAN:
	        // do nothing
	        break;
	      default:
	        MipavUtil.displayError("bad Yopt");
	        System.exit(-1);
	      }
	    }
	    
	    Mat[] segm_mask(PatchBatchExtractor pbe, Vector<Integer> oldclasses)
	    {
	    	int i;
	    	Mat smask[];
	    	int filledges=0;
	    	//fill in implicit background class
	    	Vector<Point3i> myloc = new Vector<Point3i>(); 
	    	Vector<Integer> classes = new Vector<Integer>();
	    	
	    	myloc=pbe.locall;
	    	int index=0;
	    	for(i=0;i<pbe.locall_mask.size();i++)
	    	{
	    		if(pbe.locall_mask.get(i))
	    		{
	    			classes.add(oldclasses.get(index));
	    			index++;
	    		}
	    		else
	    		{
	    			classes.add(0);
	    		}
	    	}

	    	
	    	// get locations
	    Vector<Point3i> loc = new Vector<Point3i>();
	    Point3i sum = null;
	    	for(i=0;i<myloc.size();i++)
	    	{
	    		sum.x = myloc.get(i).x + pbe.coff.x;
	    		sum.y = myloc.get(i).y + pbe.coff.y;
	    		sum.z = myloc.get(i).z + pbe.coff.z;
	    		loc.add(sum);
	    	}
	    	Point3i winsize=pbe.patopt.win;
	    	Point3i step=pbe.patopt.step;

	    	//check for single z-layer, and force mask to be 2d.
	    	if(pbe.allz.size()==1)
	    	{
	    		for(i=0;i<loc.size();i++)
	    			loc.get(i).z=0;

	    		winsize.z=1;
	    		step.z=1;
	    		pbe.vidsize.z=1;
	    	}

	    	int numclasses = classes.get(0);
	    	for (i = 1; i < classes.size(); i++)  {
	    		if (classes.get(i) > numclasses) {
	    			numclasses = classes.get(i);
	    		}
	    	}

	    	
	    	smask=create(pbe.vidsize.z,pbe.vidsize.y,pbe.vidsize.x,CV_8UC1);
	    	int zsize=step.z;

	    	Point3i rwin = null;
	    	rwin.x=winsize.x/2;
	    	rwin.y=winsize.y/2;
	    	rwin.z=zsize/2;

	    	Point2i box_y = new Point2i(-rwin.x,winsize.x-1-rwin.x);
	    	Point2i box_x = new Point2i(-rwin.y,winsize.y-1-rwin.y);
	    	Point2i box_z = new Point2i(-rwin.z,zsize-1-rwin.z);

	    	int cy=rwin.x+1;
	    	int cx=rwin.y+1;

	    	//the structuring element
	    	Mat se = new Mat(winsize.x,winsize.y,CV_8UC1);

	    	//the distance matrix
	    	Mat sed = new Mat(winsize.x,winsize.y,CV_64F);

	    	Vector<Double> ssc = new Vector<Double>();
	    	for(i=0;i<sed.rows;i++)
	    		for(int j=0;j<sed.cols;j++)
	    		{
	    			sed.double2D[i][j]=Math.sqrt(  Math.pow((double)(i+1-cy),2) + Math.pow((double)(j+1-cx),2) );
	    			ssc.add(sed.double2D[i][j]);
	    		}
	    		
	    	//growing schedule
	    	Collections.sort(ssc);
	    	for (i = 1; i < ssc.size(); i++) {
	    		if (ssc.get(i) == ssc.get(i-1)) {
	    			ssc.remove(i);
	    			i--;
	    		}
	    	}

	    	//offsets
	    	Vector<Integer> locoffs = new Vector<Integer>();
	    	for(i=0;i<loc.size();i++)
	    	{
	    		int temp=loc.get(i).y-rwin.x+pbe.vidsize.y*(loc.get(i).x-rwin.y)+pbe.vidsize.y*pbe.vidsize.x*(loc.get(i).z-rwin.z);
	    		locoffs.add(temp);
	    	}

	    	//classes
	    	Vector<Integer> cs=classes;
	    	Collections.sort(cs);
	    	for (i = 1; i < cs.size(); i++) {
	    		if (cs.get(i) == cs.get(i-1)) {
	    			cs.remove(i);
	    			i--;
	    		}
	    	}

	    	for(int seri=0;seri<ssc.size();seri++)
	    	{
	    		System.out.println("segm_mask: nn fill "+ seri + "/" + ssc.size());
	    		//build the structure element
	    			
	    		for(i=0;i<sed.rows;i++)
	    			for(int j=0;j<sed.cols;j++)
	    			{
	    				if(sed.double2D[i][j]==ssc.get(seri))
	    					se.byte2D[i][j]=1;
	    				else
	    					se.byte2D[i][j]=0;
	    			}

	    				
	    			Mat se3[]=repeat(se,zsize);
	    			int s1=se3.length;
	    			int s2=se3[0].rows;
	    			int s3=se3[0].cols;
	    			Vector<Double> se3i = new Vector<Double>();
	    			for(int kk=0;kk<s1;kk++)
	    				for(int jj=0;jj<s3;jj++)
	    					for(int ii=0;ii<s2;ii++)
	    					{
	    						if(se3[kk].byte2D[ii][jj]==1)
	    						{
	    							double temp=ii+1+jj*pbe.vidsize.y+kk*pbe.vidsize.y*pbe.vidsize.x;
	    							se3i.add(temp);
	    						}
	    					}
	    			for(i=0;i<classes.size();i++)
	    			{
	    				int c=classes.get(i);
	    				int off=locoffs.get(i);

	    				//find zero elements in smask that are active in se3
	    				Vector<Integer> tempi = new Vector<Integer>();
	    				for(int j=0;j<se3i.size();j++)
	    				{
	    					int temp=(int)(se3i.get(j)+off-1);
	    					int fNo=temp/(smask[0].rows*smask[0].cols);
	    					temp=temp-fNo*(smask[0].rows*smask[0].cols);
	    					int colNo=temp/smask[0].rows;
	    					int rowNo=temp%smask[0].rows;
	    					if(smask[fNo].byte2D[rowNo][colNo]==0)
	    						tempi.add(j);
	    				}
	    					
	    				//if found any zero elements, 
	    				for(int j=0;j<tempi.size();j++)
	    				{
	    					int temp=(int)(se3i.get(tempi.get(j))+off-1);
	    					int fNo=temp/(smask[0].rows*smask[0].cols);
	    					temp=temp-fNo*(smask[0].rows*smask[0].cols);
	    					int colNo1=temp/smask[0].rows;
	    					int rowNo1=temp%smask[0].rows;
	    					// fill location
	    					if( (c==1) || (c==2) )
	    						c=c;
	    					smask[fNo].byte2D[rowNo1][colNo1]=(byte)c;							
	    				}

	    			}
	    			
	    	}
	    				
	    	//%%% now fill in the borders if they are missing %%%
	    	double minx = Double.MAX_VALUE;
	    	double maxx = -Double.MAX_VALUE;
	    	double miny = Double.MAX_VALUE;
	    	double maxy = -Double.MAX_VALUE;
	    	for (i = 0; i < pbe.allx.size(); i++) {
	    		if (pbe.allx.get(i) < minx) {
	    			minx = pbe.allx.get(i);
	    		}
	    		if (pbe.allx.get(i) > maxx) {
	    			maxx = pbe.allx.get(i);
	    		}
	    	}
	    	for (i = 0; i < pbe.ally.size(); i++) {
	    		if (pbe.ally.get(i) < miny) {
	    			miny = pbe.ally.get(i);
	    		}
	    		if (pbe.ally.get(i) > maxy) {
	    			maxy = pbe.ally.get(i);
	    		}
	    	}
	        minx=minx+pbe.coff.y+box_x.x;
	    	maxx=maxx+pbe.coff.y+box_x.y+2;
	    	miny=miny+pbe.coff.x+box_y.x;
	    	maxy=maxy+pbe.coff.x+box_y.y+2;

	    	if(1<=minx)
	    	{
	    		// fill in left edge
	    		for(i=0;i<smask.length;i++)
	    			for(int j=0;j<smask[0].rows;j++)
	    				for(int k=0;k<minx;k++)
	    					smask[i].byte2D[j][k]=smask[i].byte2D[j][(int)minx];

	    	}

	    	if(maxx<=pbe.vidsize.x)
	    	{
	    		//fill in right edge
	    		for(i=0;i<smask.length;i++)
	    			for(int j=0;j<smask[0].rows;j++)
	    				for(int k=(int)(maxx-1);k<pbe.vidsize.x;k++)
	    					smask[i].byte2D[j][k]=smask[i].byte2D[j][(int)(maxx-2)];

	    	}

	    	

	    	if(1<=miny)
	    	{
	    		// fill in top edge
	    		for(i=0;i<smask.length;i++)
	    			for(int j=0;j<miny;j++)
	    				for(int k=0;k<smask[0].cols;k++)
	    					smask[i].byte2D[j][k]=smask[i].byte2D[(int)miny][k];

	    	}

	    	if(maxy<=pbe.vidsize.y)
	    	{
	    		// fill in bottom edge
	    		for(i=0;i<smask.length;i++)
	    			for(int j=(int)(maxy-1);j<pbe.vidsize.y;j++)
	    				for(int k=0;k<smask[0].cols;k++)
	    					smask[i].byte2D[j][k]=smask[i].byte2D[(int)(maxy-2)][k];

	    	}

	    			if(pbe.vidsize.z>1)
	    			{
	    				double minz = Double.MAX_VALUE;
	    		    	double maxz = -Double.MAX_VALUE;
	    		    	for (i = 0; i < pbe.allz.size(); i++) {
	    		    		if (pbe.allz.get(i) < minz) {
	    		    			minz = pbe.allz.get(i);
	    		    		}
	    		    		if (pbe.allz.get(i) > maxx) {
	    		    			maxz = pbe.allz.get(i);
	    		    		}
	    		    	}
	    				minz=minz+pbe.coff.z+box_z.x;
	    				maxz=maxz+pbe.coff.z+box_z.y+2;

	    				if(1<=minz)
	    				{
	    					//fill in first frames
	    					for(i=0;i<minz;i++)
	    						for(int j=0;j<smask[0].rows;j++)
	    							for(int k=0;k<smask[0].cols;k++)
	    							smask[i].byte2D[j][k]=smask[(int)minz].byte2D[j][k];
	    				}

	    				if(maxz<=pbe.vidsize.z)
	    				{
	    					//fill in last frames
	    					for(i=(int)(maxz-1);i<pbe.vidsize.z;i++)
	    						for(int j=0;j<smask[0].rows;j++)
	    							for(int k=0;k<smask[0].cols;k++)
	    							smask[i].byte2D[j][k]=smask[(int)(maxz-2)].byte2D[j][k];
	    				}
	    			}


	    /*			
	    	FILE *fid=fopen("d:/bad/dump_mask_openCV.bin","wb");
	    	for(int i=0;i<smask.size[0];i++)
	    		for(int j=0;j<smask.size[2];j++)
	    			for(int k=0;k<smask.size[1];k++)
	    			{
	    				double temp=smask.at<uchar>(i,k,j);
	    				fwrite(&temp,8,1,fid);
	    			}
	    			fclose(fid);*/

	    			/*if(smask.length==1)
	    			{
	    				Mat tempM;
	    				MatVid::frame(smask,0).copyTo(tempM);
	    				return tempM;
	    			}
	    			else
	    			{
	    				return smask;
	    			}*/
	    			return smask;
	    }
	    
	    /*!
	     * \brief
	     * maximum vote filter.
	     * 
	     * \param I
	     * input video.
	     * 
	     * \param sx
	     * x filter size.
	     * 
	     * \param sy
	     * y filter size.
	     * 
	     * \param sz
	     * z filter size.
	     * 
	     * \param nclass
	     * number of classes in the video.
	     * 
	     * \returns
	     * filtered video.
	     * 
	     * Can be used to smooth out the segmentation results.
	     *  
	     * 
	     * \see
	     * segmentVideo.
	     */
	    Mat[] maxvotefilt(Mat I[],int sx,int sy,int sz,int numclass)
	    {
	    	int row,col;
	    	//in case of 2d video, make it 2d
	    	//if(I.dims==2)
	    	//{
	    	//	Mat tmp=MatVid::create(1,I.rows,I.cols,CV_8UC1);
	    	//	Mat tmp2=MatVid::frame(tmp,0);
	    	//	I.copyTo(tmp2);
	    	//	I=tmp;
	    	//}

	    	//initializing filtered mask
	    	Mat J[] = create(I.length,I[0].rows,I[0].cols,CV_8UC1);;

	    	//filling some sizes
	    	int rs1=sx/2;
	    	int rs2=sy/2;
	    	int rs3=sz/2;
	    	Point2i box_y = new Point2i(-rs1,-rs1+sx-1);
	    	Point2i box_x = new Point2i(-rs2,-rs2+sy-1);
	    	Point2i box_z = new Point2i(0,0);
	    	if(sz>0)
	    	{
	    		box_z.x=-rs3;
	    		box_z.y=-rs3+sz-1;
	    	}

	    	//memory for each class
	        Vector<Mat[]> count = new Vector<Mat[]>();
	    	for(int i=0;i<numclass+1;i++)
	    	{	
	    		Mat tmp[] = create(I.length,I[0].rows,I[0].cols,CV_8UC1);
	    		count.add(tmp);
	    	}

	    	//applying maxvote
	    	for(int c=0;c<=numclass;c++)
	    	{
	    		Mat foo[]=create(I.length,I[0].rows,I[0].cols,CV_8UC1);		
	    		for(int i=0;i<I.length;i++)
	    		{
	    			for(int j=0;j<I[0].rows;j++)
	    			{
	    				for(int k=0;k<I[0].cols;k++)
	    				{
	    					if(I[i].byte2D[j][k]==c)
	    					{
	    						foo[i].byte2D[j][k]=1;
	    					}
	    					else
	    					{
	    						foo[i].byte2D[j][k]=0;
	    					}
	    				}
	    			}
	    		}		
	    		for(int z=box_z.x;z<=box_z.y;z++)
	    		{
	    			for(int y=box_y.x;y<=box_y.y;y++)
	    			{
	    				for(int x=box_x.x;x<=box_x.y;x++)
	    				{
	    					Point yind = new Point((y+1)>1?(y+1):1, (y+I[0].rows)<(I[0].rows)?(y+I[0].rows):(I[0].rows));
	    					Point xind = new Point((x+1)>1?(x+1):1, (x+I[0].cols)<(I[0].cols)?(x+I[0].cols):(I[0].cols));
	    					Point zind = new Point((z+1)>1?(z+1):1, (z+I.length)<(I.length)?(z+I.length):(I.length));

	    					Point yind2 = new Point((-y+1)>1?(-y+1):1, (-y+I[0].rows)<(I[0].rows)?(-y+I[0].rows):(I[0].rows));
	    					Point xind2 = new Point((-x+1)>1?(-x+1):1, (-x+I[0].cols)<(I[0].cols)?(-x+I[0].cols):(I[0].cols));
	    					Point zind2 = new Point((-z+1)>1?(-z+1):1, (-z+I.length)<(I.length)?(-z+I.length):(I.length));

	    					Vector<Integer> xxind = new Vector<Integer>();
	    					Vector<Integer> yyind = new Vector<Integer>();
	    					Vector<Integer> zzind = new Vector<Integer>();
	    					Vector<Integer> xxind2 = new Vector<Integer>();
	    					Vector<Integer> yyind2 = new Vector<Integer>();
	    					Vector<Integer> zzind2 = new Vector<Integer>();

	    					for(int i=xind.x;i<=xind.y;i++)
	    						xxind.add(i);

	    					for(int i=yind.x;i<=yind.y;i++)
	    						yyind.add(i);

	    					for(int i=zind.x;i<=zind.y;i++)
	    						zzind.add(i);

	    					for(int i=xind2.x;i<=xind2.y;i++)
	    						xxind2.add(i);

	    					for(int i=yind2.x;i<=yind2.y;i++)
	    						yyind2.add(i);

	    					for(int i=zind2.x;i<=zind2.y;i++)
	    						zzind2.add(i);			

	    					if(zzind.isEmpty())
	    						zzind.add(1);

	    					if(zzind2.isEmpty())
	    						zzind2.add(1);


	    					for(int kk=0;kk<zzind.size();kk++)
	    					{
	    						Mat tmp1=count.get(c)[zzind.get(kk)-1];
	    						Mat tmp2 = new Mat(yind.y-yind.x+1,xind.y-xind.x+1,CV_8U);
	    						for (row = yind.x-1; row < yind.y; row++) {
	    							for (col = xind.x-1; col < xind.y; col++) {
	    								tmp2.byte2D[row-(yind.x-1)][col-(xind.x-1)] = tmp1.byte2D[row][col];
	    							}
	    						}
	    						Mat tmp3=foo[zzind2.get(kk)-1];
	    						Mat tmp4 = new Mat(yind2.y-yind2.x+1,xind2.y-xind2.x+1,CV_8U);
	    						for (row = yind2.x-1; row < yind2.y; row++) {
	    						    for (col = xind2.x-1; col < xind2.y; col++) {
	    						    	tmp4.byte2D[row-(yind2.x-1)][col-(xind2.x-1)] = tmp3.byte2D[row][col];
	    						    }
	    						}
	    						tmp2=plus(tmp2,tmp4);
	    					}

	    				}
	    			}
	    		}
	    	}

	    	for(int i=0;i<count.get(0).length;i++)
	    	{
	    		for(int j=0;j<count.get(0)[0].rows;j++)
	    		{
	    			for(int k=0;k<count.get(0)[0].cols;k++)
	    			{
	    				short maxV=0;
	    				int maxind=0;
	    				for(int m=0;m<count.size();m++)
	    				{
	    					if((count.get(m)[i].byte2D[j][k] & 0xff) >maxV)
	    					{
	    						maxV=(short)(count.get(m)[i].byte2D[j][k] & 0xff);
	    						maxind=m;
	    					}
	    				}

	    				J[i].byte2D[j][k]=(byte)maxind;
	    			}
	    		}
	    	}

	    	//make 2d
	    	//if(J.size[0]==1)
	    	//{
	    	//	Mat tmp=MatVid::frame(J,0);
	    	//	Mat tmp2;
	    	//	tmp.copyTo(tmp2);
	    	//	return tmp2;
	    	//}
	    	//else
	    	//{
	    	//	return J;
	    	//}
	    	return J;
	    }

	 // colorize an image based on a segmentation mask
	 // (same as colormask.m)
	 // TODO: add option to stop reindexing the mask (useful for consistent colors in videos)
	 void colorMask(Mat img[], Mat mask[], Mat oimg, int border) {
	   int r,c,ch,z;
	   if (img[0].channels != 1) {
	     MipavUtil.displayError("too many channels in colorMask");
	     System.exit(-1);
	   }
	   if (mask[0].type != CV_8UC1) {
	     MipavUtil.displayError("mask[0] not 8-bit in colorMask");
	     System.exit(-1);
	   }

	   int mdims = mask[0].dims;
	   int idims = img[0].dims;

	   // check compatability of dimensions
	   if ((mdims == 2) && (idims == 2) && (mask.length == 1) && (img.length == 1) && (mask.length == 1)) {
	     if (img[0].rows != mask[0].rows) {
	    	 MipavUtil.displayError("img[0].rows != mask[0].rows in colorMask");
	    	 System.exit(-1);
	     }
	     if (img[0].cols == mask[0].cols) {
	    	 MipavUtil.displayError("img[0].cols != mask[0].cols in colorMask");
	    	 System.exit(-1);
	     }
	   } else if ((mdims == 2) && (idims == 2) && (mask.length == 1) && (img.length > 1) && (mask.length == 1)) {
		   if (img[0].rows != mask[0].rows) {
		    	 MipavUtil.displayError("img[0].rows != mask[0].rows in colorMask");
		    	 System.exit(-1);
		     }
		     if (img[0].cols == mask[0].cols) {
		    	 MipavUtil.displayError("img[0].cols != mask[0].cols in colorMask");
		    	 System.exit(-1);
		     } 
	   } else if ((mdims == 2) && (idims == 2) && (mask.length == 1) && (img.length > 1) && (mask.length > 1)) {
		   if (img.length != mask.length) {
			   MipavUtil.displayError("img.length != mask.length in colorMask");
			   System.exit(-1);
		   }
		   if (img[0].rows != mask[0].rows) {
		    	 MipavUtil.displayError("img[0].rows != mask[0].rows in colorMask");
		    	 System.exit(-1);
		     }
		     if (img[0].cols == mask[0].cols) {
		    	 MipavUtil.displayError("img[0].cols != mask[0].cols in colorMask");
		    	 System.exit(-1);
		     } 
	   } else {
	     MipavUtil.displayError("Bad inputs in colorMask");
	     System.exit(-1);
	   }


	   // build RGB tables (one time only)
	   if (g_mask_lut[0][0] == null); {
	     System.out.println("\n** building RGB tables **");
	     
	     for (int i=0; i<MAXCOL; i++) {
	       for (c=0; c<3; c++) {
	    	 g_mask_lut[i][c] = new Mat();
	         g_mask_lut[i][c].create(1,256,CV_8UC1);
	         for (int p=0; p<256; p++) {
	           g_mask_lut[i][c].byte2D[0][p] = (byte)(p*g_rgbtable[i][c]);
	         }
	       }
	     }
	   }

	   // build list of mask values --> color
	   byte mask_flag[] = new byte[256];
	   byte mask_values[] = new byte[256];
	   // initialize array
	   for (int i=0; i<256; i++)
	     mask_flag[i] = 0;

	   // mark mask values
	   if (mask.length == 1) {
	     // mask image
	     for (int y=0; y<mask[0].rows; y++)
	       for (int x=0; x<mask[0].cols; x++)
	         mask_flag[mask[0].byte2D[y][x] & 0xff] = 1;
	   } else {
	     // mask video
	     for (z=0; z<mask.length; z++) 
	       for (int y=0; y<mask[0].rows; y++)
	         for (int x=0; x<mask[0].cols; x++)
	           mask_flag[mask[z].byte2D[y][x] & 0xff] = 1;  
	   }
	   
	   // convert mask values to color index
	   
	   mask_flag[0] = 1;  //always one for the background

	   int nummask = 0;
	   

	   for (int i=0; i<256; i++) {
	     mask_values[i] = (byte)((mask_flag[i] != 0) ? (nummask++) % MAXCOL : 0);
	     //cout << (int)(mask_values[i]) << " ";
	   }
	   
	   //dumpMatSize(img);
	   //dumpMatSize(mask);
	   //cout << "nm = " << nummask << "\n";

	   // allocate output video
	   Mat bgr[][] = new Mat[3][img.length];
	   for (int i = 0; i < 3; i++) {
		   for (int j = 0; j < img.length; j++) {
			   bgr[i][j] = new Mat();
		   }
	   }
	   for (int i=0; i<3; i++) {
	     if (img.length == 1)      // output image
	       bgr[i][0].create(img[0].rows, img[0].cols, CV_8UC1);
	     else       // output video
	       for (int j = 0; j < img.length; j++) {
	           bgr[i][j].create(img[0].rows, img[0].cols, CV_8UC1);  
	       }
	   }
	   
	   // use lookup table  
	   if (img.length == 1) {
	     // image
	     for (int y=0; y<img[0].rows; y++) {
	       for (int x=0; x<img[0].cols; x++) {
	         int p = (img[0].byte2D[y][x] & 0xff);
	         int m = (mask_values[mask[0].byte2D[y][x] & 0xff] & 0xff);
	         for (int i=0; i<3; i++)
	           bgr[2-i][0].byte2D[y][x] = g_mask_lut[m][i].byte2D[0][p];
	       }
	     }
	   } else {
	     // video
	     for (z=0; z<img.length; z++) {
	       for (int y=0; y<img[0].rows; y++) {
	         for (int x=0; x<img[0].cols; x++) {
	           int p = (img[z].byte2D[y][x] & 0xff);
	           int m;
	           if (mask.length == 2)
	             m = (mask_values[mask[0].byte2D[y][x] & 0xff] & 0xff);    // mask image
	           else
	             m = (mask_values[mask[z].byte2D[y][x] & 0xff] & 0xff);  // mask video
	           for (int i=0; i<3; i++)
	             bgr[2-i][z].byte2D[y][x] = g_mask_lut[m][i].byte2D[0][p];
	         }
	       }
	     }
	   }
	   
	   // make border
	   if (border>0) {
	     if (mask.length == 1) {
	       // mask image & ...
	       Mat bord = mask2Border(mask[0], border, mask_flag);
	       if (img.length == 1) {
	         // ...output image
	         for (int i=0; i<3; i++) {
	          for (r = 0; r < img[0].rows; r++) {
	        	  for (c = 0; c < img[0].cols; c++) {
	        	      bgr[i][0].byte2D[r][c] = (byte)(bord.byte2D[r][c] | bord.byte2D[r][c]);
	        	  }
	          }
	         }
	           
	       } else {
	         // ...output video
	         for (z=0; z<img.length; z++) {
	           for (int i=0; i<3; i++) {
	             Mat tmp = bgr[i][z];
	             for (r = 0; r < img[0].rows; r++) {
	            	 for (c = 0; c < img[0].cols; c++) {
	            		 tmp.byte2D[r][c] = (byte)(tmp.byte2D[r][c] | bord.byte2D[r][c]);
	            	 }
	             }
	           }
	         }
	       }
	         
	     } else {
	       // mask video
	       for (z=0; z<img.length; z++) {
	         Mat bord = mask2Border(mask[z], border, mask_flag);
	         for (int i=0; i<3; i++) {
	           Mat tmp = bgr[i][z];
	           for (r = 0; r < img[0].rows; r++) {
	            	 for (c = 0; c < img[0].cols; c++) {
	            		 tmp.byte2D[r][c] = (byte)(tmp.byte2D[r][c] | bord.byte2D[r][c]);
	            	 }
	             }
	         }        
	       }
	     }
	   }

	   // make color image
	   int sz[] = new int[]{img.length,img[0].rows,img[0].cols};
	   oimg.create(3,sz,CV_8UC,3);
	   
	   for (z = 0; z < img.length; z++) {
		   for (r = 0; r < img[0].rows; r++) {
			   for (c = 0; c < img[0].cols; c++) {
				   for (ch = 0; ch < 3; ch++) {
				       oimg.byte3DC[z][r][c][ch] = bgr[ch][z].byte2D[r][c]; 
				   }
			   }
		   }
	   }
	 }

	 // mask_flag = unsigned char[256] w/ non-zero for present mask values
	 Mat mask2Border(Mat mask, int border, byte mask_flag[]) {
	   int r,c;
	   int x,y;
	   if (border <= 0) {
		   MipavUtil.displayError("border = " + border + " in mask2Border");
		   System.exit(-1);
	   }
	   
	   Mat el;
	   if (border == 1) {
	     el = new Mat(2,2,CV_8UC1);
	     for (r = 0; r < 2; r++) {
	    	 for (c = 0; c < 2; c++) {
	    		 el.byte2D[r][c] = 1;
	    	 }
	     }
	   }
	   else {
	     //el = getStructuringElement(MORPH_ELLIPSE, Size(2*border+1, 2*border+1));
		   double distance;
		   double radius = (2.0*border+1.0)/2.0;
		   el = new Mat(2*border+1,2*border+1,CV_8UC1);
		   for (r = 0; r < 2*border+1; r++) {

	            for (c = 0; c < 2*border+1; c++) {
	                distance = Math.sqrt(((r - border) * (r - border)) + ((c - border) * (c - border)));

	                if (distance < radius) {
	                    el.byte2D[r][c] = 1;
	                }
	            }
	        }
	   }
	   
	   if (mask_flag == null) {
	     MipavUtil.displayError("should provide mask_values in mask2Border");
	     System.out.println();
	   }
	   
	   Mat out = new Mat(mask.rows, mask.cols, CV_8UC1);
	   int extents[] = new int[]{mask.rows,mask.cols};
	   int length = mask.rows*mask.cols;
	   byte maskc[] = new byte[length];
	   byte maskcd[] = new byte[length];
	   ModelImage maskcImage = new ModelImage(ModelStorageBase.BYTE, extents, "maskcImage");
	   AlgorithmMorphology2D algoMorph2D;
	   
	   for (c=0; c<256; c++) {
	     if (mask_flag[c] != 0) {      
	       //cout << "mask=" << c << "; ";
	       // maskc = (mask == c);                // segment c
	       for (y = 0; y < mask.rows; y++) {
	    	   for (x = 0; x < mask.cols; x++) {
	    		   if ((mask.byte2D[y][x] & 0xff) == c) {
	    			   maskc[y*mask.cols + x] = 1;
	    		   }
	    		   else {
	    			   maskc[y*mask.cols + x] = 0;
	    		   }
	    	   }
	       }
	       try {
	    	   maskcImage.importData(0, maskc, true);
	       }
	       catch (IOException e) {
	    	   MipavUtil.displayError("IOExcetpion " + e + " on maskcImage.importData(0, maskc, true)");
	    	   System.exit(-1);
	       }
	       int itersDilate = 1;
	       boolean entireImage = true;
	       algoMorph2D = new AlgorithmMorphology2D(maskcImage,AlgorithmMorphology2D.SIZED_CIRCLE,(float)(2*border+1),AlgorithmMorphology2D.DILATE,
	    		   itersDilate,0,0,0,entireImage);
	       algoMorph2D.run();
	       algoMorph2D.finalize();
	       try {
	           maskcImage.exportData(0, length, maskcd);
	       }
	       catch (IOException e) {
	    	   MipavUtil.displayError("IOExcetpion " + e + " on maskcImage.exportData(0, length, maskcd)");
	    	   System.exit(-1);
	       }
	       for (r = 0; r < mask.rows; r++) {
	    	   for (c = 0; c < mask.cols; c++) {
	    		   maskcd[r*mask.cols+c] = (byte)(maskcd[r*mask.cols+c] ^ maskc[r*mask.cols+c]);
	    	   }
	       }
	       for (r = 0; r < mask.rows; r++) {
	    	   for (c = 0; c < mask.cols; c++) {
	    		   out.byte2D[r][c] = (byte)(out.byte2D[r][c] | maskcd[r*mask.cols+c]);
	    	   }
	       }
	       //dilate(maskc, maskcd, el);          // dilate slightly
	       /*bitwise_xor(maskcd, maskc, maskcd); // take difference
	       bitwise_or(out, maskcd, out);       // accumulate*/
	     }
	   }

	   return out;
	 }
	 
	 /*!
	  * \brief
	  * segments a set of videos in a sequence.
	  * 
	  * \param path
	  * path vector of all videos in order.
	  * 
	  * \param stepxy
	  * segmentation step xy size.
	  * 
	  * \param stepz
	  * segmentation step z size.
	  * 
	  * \param filtxy
	  * max vote filter xy size.
	  * 
	  * \param filtz
	  * max vote filter z size.
	  * 
	  * \param opath
	  * path of the output video, mask files.
	  * 
	  * can also be used to segment just one video.
	  * 
	  * \remarks
	  * dtm must be ready before calling this function.
	  * 
	  * \see
	  * learnDTM.
	  */
	 public void segmentVideoSequence(VidSegm vs, Vector<String> paths,int stepxy,int stepz,int filtxy,int filtz,String vidPath2)
	 {
		/*int j;
	 	//segment all videos in the sequence
	 	for(int i=0;i<paths.size();i++)
	 	{
	 		System.out.println("Segmenting: " + paths.get(i));
	 		Mat segMask;
	 		Mat segVideo;

	 		//first video in the sequence, special case
	 		if(i==0)
	 		{
	 			Mat postFrames[];
	 			//is there any next video in sequence
	 			if(paths.size()>1)
	 			{
	 				Mat vid2[]=loaddat(paths.get(i+1),"t");				
	 				int vsize=vid2.length;
	 				postFrames = new Mat[vs.popt.win.z-stepz];
	 				for (j = 0; j < postFrames.length; j++) {
	 					postFrames[j] = new Mat();
	 					copyTo(vid2[j],postFrames[j]);
	 				}
	 				for (j = 0; j < vid2.length; j++) {
	 					vid2[j].release();
	 				}
	 				vid2 = null;
	 			}
	 			System.out.println("PostFrames :" + postFrames.length);
	 			Mat vid1[]=loaddat(paths.get(i),"t");			
	 			System.out.println("Vid1-Frames" + vid1.length);
	 			Mat newVid[];
	 			if(paths.size()>1)
	 			{
	 				newVid = new Mat[postFrames.length+vid1.length];
	 				for (j = 0; j < newVid.length; j++) {
	 					newVid[j] = new Mat(vid1[0].rows,vid1[0].cols,vid1[0].type);
	 				}
	 			}
	 			else
	 			{
	 				newVid = new Mat[vid1.length];
	 				for (j = 0; j < newVid.length; j++) {
	 					newVid[j] = new Mat(vid1[0].rows,vid1[0].cols,vid1[0].type);
	 				}
	 			}
	 			System.out.println("newVid-Frames" + newVid.length);

	 			for (j = 0; j < vid1.length; j++) {
	 				copyTo(vid1[j],newVid[j]);
	 			}

	 			if(paths.size()>1)
	 			{
	 				for (j = vid1.length; j < newVid.length; j++) {
	 					copyTo(postFrames[j-vid1.length],newVid[j]);
	 				}
	 			}

	 			int vid1size=vid1.length;
	 			for (j = 0; j < vid1size; j++) {
	 				vid1[j].release();
	 			}
	 			vid1 = null;

	 			Mat segMask2 = new Mat();
	 			Mat segVideo2;
	 			segVideo2=segmentVideo(vs, newVid,stepxy,stepz,filtxy,filtz,segMask2);
	 			(MatVid::subvid(segMask2,Range(0,vid1size),Range::all(),Range::all())).copyTo(segMask);
	 			(MatVid::subvid(segVideo2,Range(0,vid1size),Range::all(),Range::all())).copyTo(segVideo);		

	 		}
	 		else if(i==(paths.size()-1)) //last video special case
	 		{
	 			Mat vid1=MatVid::loaddat(paths[i-1],"t");				
	 			int vsize=vid1.size[0];
	 			cout<<"Vid1-Frames"<<vid1.size[0]<<endl;
	 			Mat tempM=MatVid::subvid(vid1,Range( (vsize-popt.win.z)+stepz,vsize),Range::all(),Range::all());
	 			Mat preFrames;
	 			tempM.copyTo(preFrames);
	 			vid1.release();
	 			cout<<"Pre-Frames"<<preFrames.size[0]<<endl;			

	 			Mat vid2=MatVid::loaddat(paths[i],"t");	
	 			cout<<"Vid2-Frames"<<vid2.size[0]<<endl;

	 			Mat newVid=MatVid::create(preFrames.size[0]+vid2.size[0],vid2.size[1],vid2.size[2],vid1.type());
	 			cout<<"neVid-Frames"<<newVid.size[0]<<endl;

	 			Mat t1=MatVid::subvid(newVid,Range(0,preFrames.size[0]),Range::all(),Range::all());
	 			preFrames.copyTo(t1);
	 			Mat t2=MatVid::subvid(newVid,Range(preFrames.size[0],newVid.size[0]),Range::all(),Range::all());
	 			vid2.copyTo(t2);
	 						
	 			vid2.release();
	 			Mat segMask2,segVideo2;
	 			segVideo2=segmentVideo(newVid,stepxy,stepz,filtxy,filtz,segMask2);
	 						
	 			(MatVid::subvid(segMask2,Range(preFrames.size[0],segMask2.size[0]),Range::all(),Range::all())).copyTo(segMask);
	 			(MatVid::subvid(segVideo2,Range(preFrames.size[0],segVideo2.size[0]),Range::all(),Range::all())).copyTo(segVideo);
	 			
	 		}
	 		else //rest of the videos
	 		{
	 			//PRe
	 			Mat vid1=MatVid::loaddat(paths[i-1],"t");				
	 			int vsize=vid1.size[0];
	 			cout<<"Vid1-Frames"<<vid1.size[0]<<endl;
	 			Mat tempM=MatVid::subvid(vid1,Range( (vsize-popt.win.z)+stepz,vsize),Range::all(),Range::all());
	 			Mat preFrames;
	 			tempM.copyTo(preFrames);
	 			vid1.release();
	 			cout<<"Pre-Frames"<<preFrames.size[0]<<endl;

	 			//Post
	 			Mat vid3=MatVid::loaddat(paths[i+1],"t");				
	 			int vsize3=vid3.size[0];
	 			cout<<"Vid3-Frames"<<vid3.size[0]<<endl;			
	 			Mat tempM3=MatVid::subvid(vid3,Range(0,popt.win.z-stepz),Range::all(),Range::all());
	 			Mat postFrames;
	 			tempM3.copyTo(postFrames);			
	 			vid3.release();
	 			cout<<"Post-Frames"<<postFrames.size[0]<<endl;

	 			
	 			Mat vid2=MatVid::loaddat(paths[i],"t");	
	 			cout<<"Vid2-Frames"<<vid2.size[0]<<endl;

	 			Mat newVid=MatVid::create(preFrames.size[0]+vid2.size[0]+postFrames.size[0],vid2.size[1],vid2.size[2],vid1.type());
	 			cout<<"neVid-Frames"<<newVid.size[0]<<endl;

	 			Mat t1=MatVid::subvid(newVid,Range(0,preFrames.size[0]),Range::all(),Range::all());
	 			preFrames.copyTo(t1);
	 			Mat t2=MatVid::subvid(newVid,Range(preFrames.size[0],newVid.size[0]-postFrames.size[0]),Range::all(),Range::all());
	 			vid2.copyTo(t2);
	 			Mat t3=MatVid::subvid(newVid,Range(newVid.size[0]-postFrames.size[0],newVid.size[0]),Range::all(),Range::all());
	 			postFrames.copyTo(t3);
	 			
	 			vid2.release();
	 			Mat segMask2,segVideo2;
	 			segVideo2=segmentVideo(newVid,stepxy,stepz,filtxy,filtz,segMask2);
	 					
	 			(MatVid::subvid(segMask2,Range(preFrames.size[0],segMask2.size[0]-postFrames.size[0]),Range::all(),Range::all())).copyTo(segMask);
	 			(MatVid::subvid(segVideo2,Range(preFrames.size[0],segVideo2.size[0]-postFrames.size[0]),Range::all(),Range::all())).copyTo(segVideo);
	 		}

	 		//save things for current video segmentation in the output place
	 		char carr[100];
	 		sprintf(carr,"%d",i);
	 		string vidid(carr);
	 		string dirpath=vidPath2 + vidid + string("_segm");
	 		PlatInd::makedir(dirpath);

	 		string fpath=dirpath+string("/")+string("vid") + vidid+ string("_segm_");
	 		Mat smask=segMask;	
	 		
	 		if(smask.dims==2)
	 		{
	 			Mat img=smask;

	 			char buffer [100];
	 			sprintf(buffer,"%03d",0);

	 			string filename=fpath+buffer+".png";

	 			Mat frame=Mat::zeros(img.rows,img.cols,CV_8UC1);
	 			for(int j=0;j<img.rows;j++)
	 				for(int k=0;k<img.cols;k++)
	 				{
	 					if(img.at<unsigned char>(j,k)==1)
	 						frame.at<unsigned char>(j,k)=127;
	 					else if(img.at<unsigned char>(j,k)==2)
	 						frame.at<unsigned char>(j,k)=255;
	 					else
	 						frame.at<unsigned char>(j,k)=0;
	 				}
	 				imwrite(filename,frame);
	 		}
	 		else
	 		{		
	 			for(int m=0;m<smask.size[0];m++)
	 			{
	 				Mat img=MatVid::frame(smask,m);

	 				char buffer [100];
	 				sprintf(buffer,"%03d",m);

	 				string filename=fpath+buffer+".png";

	 				Mat frame=Mat::zeros(img.rows,img.cols,CV_8UC1);
	 				for(int j=0;j<img.rows;j++)
	 					for(int k=0;k<img.cols;k++)
	 					{
	 						if(img.at<unsigned char>(j,k)==1)
	 							frame.at<unsigned char>(j,k)=127;
	 						else if(img.at<unsigned char>(j,k)==2)
	 							frame.at<unsigned char>(j,k)=255;
	 						else
	 							frame.at<unsigned char>(j,k)=0;
	 					}
	 					imwrite(filename,frame);

	 			}
	 		}
	 		//save avi and dat for windows pc
	 		#ifdef _WIN32
	 			string avipath=vidPath2 + vidid + string(".avi");
	 			string datpath=vidPath2 + vidid + string(".dat");
	 			MatVid::saveavi(avipath,segVideo);
	 			MatVid::savedat(datpath,segVideo,"unsigned_1");	
	 		#endif
	 	}*/
	 }
	 
	 /*!
	  * \brief
	  * Memory efficient single video segmentation function
	  * 
	  * \param vid
	  * video to segment.
	  * 
	  * \param stepxy
	  * xy scale/step for patches.
	  * 
	  * \param stepz
	  * z scale/setep for patches.
	  * 
	  * \param filtxy
	  * xy size of the maxvote smoothing filter applied to the segmentation mask.
	  * 
	  * \param filtz
	  * z size of the maxvote smoothing filter applied to the segmentation mask.
	  * 
	  * \param osmask
	  * output segmentation mask.
	  * 
	  * \returns
	  * color segmented video
	  *   
	  * Segments the video in chunks and saves classes and locations for producing overall segmentation mask and color segmentation .
	  * 
	  * \remarks
	  * In general one should use segmentVideoSequence function and not call this directly.
	  * 
	  * \see
	  * segmentVideoSequence.
	  */
	 Mat segmentVideo(VidSegm vs,Mat vid[],int stepxy,int stepz,int filtxy,int filtz, Mat osmask)
	 {
	 	//turn on segmentation timer for current video
	 	/*long startTime = System.currentTimeMillis();

	 	//reset current video segmentation results
	 	Vector<Mat> patches = new Vector<Mat>();     	
	 	vs.allclasses.clear();
	 	vs.allx.clear();       
	 	vs.ally.clear();       
	 	vs.allz.clear();       	
	 	vs.vidsize = new Point3i(vid.length, vid[0].rows, vid[0].cols);
	 	vs.locall.clear();     
	 	vs.locall_mask.clear();
	 	
	 	
	 	//segmented video
	 	Mat segm;
	 	
	 	//update patch option steps for segmentation
	 	PatchOptions npopt(popt);
	 	npopt.step=Point3i(stepxy,stepxy,stepz);
	 	popt=npopt;

	 	//initialize Kalman Cashe for the training mixture
	 	dtm.setupKFB(popt.win.z);

	 	//option for box segmenttaion; set to all video here
	 	Range box_z = Range::all();
	 	Range box_y = Range::all();
	 	Range box_x = Range::all();
	 		     
	 	// internal bounding box size and offsets
	 	Point3i vbox_size, vbox_off;
	 	// subvideo from internal bounding box
	 	Mat     boxvid;

	 	// check bounding box: 
	 	if ((box_z == Range::all()) && (box_y == Range::all()) && (box_z == Range::all())) 
	 	{
	 		// using full video
	 		vbox_size = Point3i(vid.size[2], vid.size[1], vid.size[0]);
	 		vbox_off  = Point3i(0, 0, 0);

	 		boxvid = vid;
	 	} 
	 	else 
	 	{
	 		// using sub-video
	 		Range   vbox_z, vbox_x, vbox_y;
	 		// adjust box to align with patch locations
	 		if (box_z == Range::all()) 
	 		{
	 			vbox_off.z  = 0;
	 			vbox_size.z = vid.size[0];
	 			vbox_z      = Range::all();
	 		} 
	 		else 
	 		{
	 			vbox_off.z  = (box_z.start % popt.step.z == 0 ? box_z.start : ((box_z.start/popt.step.z)+1)*popt.step.z);
	 			vbox_size.z = box_z.end - vbox_off.z;
	 			vbox_z      = Range(vbox_off.z, MIN(box_z.end+popt.win.z, vid.size[0]) );
	 		}

	 		if (box_y == Range::all()) 
	 		{
	 			vbox_off.y  = 0;
	 			vbox_size.y = vid.size[1];
	 			vbox_y      = Range::all();
	 		} 
	 		else 
	 		{
	 			vbox_off.y  = (box_y.start % popt.step.y == 0 ? box_y.start : ((box_y.start/popt.step.y)+1)*popt.step.y);
	 			vbox_size.y = box_y.end - vbox_off.y;
	 			vbox_y      = Range(vbox_off.y, MIN(box_y.end+popt.win.y, vid.size[1]) );
	 		}
	        
	 		if (box_x == Range::all()) 
	 		{
	 			vbox_off.x  = 0;
	 			vbox_size.x = vid.size[2];
	 			vbox_x      = Range::all();
	 		} 
	 		else 
	 		{
	 			vbox_off.x  = (box_x.start % popt.step.x == 0 ? box_x.start : ((box_x.start/popt.step.x)+1)*popt.step.x);
	 			vbox_size.x = box_x.end - vbox_off.x;
	 			vbox_x      = Range(vbox_off.x, MIN(box_x.end+popt.win.x, vid.size[2]) );
	 		}    

	 		boxvid = MatVid::subvid(vid, vbox_z, vbox_y, vbox_x);  	
	 	}
	 	
	 	// create the patch extractor
	 	PatchExtractor pe(popt, boxvid.size[1], boxvid.size[2]);  
	 	// copy some info
	 	coff = pe.coff;
	 	allx = pe.allx;
	 	ally = pe.ally;
	 	int xsize = allx.size();
	 	int ysize = ally.size();

	 	// figure out possible z
	 	int zsize = (boxvid.size[0]-popt.win.z) / popt.step.z + 1;
	 	allz.reserve(zsize);
	 	for (int z=0; z<=boxvid.size[0]-popt.win.z; z+=popt.step.z) 
	 	{
	 		allz.push_back(z);
	 	}
	 	// offset allx, ally, allz
	 	for (unsigned int i=0; i<allx.size(); i++)
	 		allx[i] += vbox_off.x;
	 	for (unsigned int i=0; i<ally.size(); i++)
	 		ally[i] += vbox_off.y;
	 	for (unsigned int i=0; i<allz.size(); i++)
	 		allz[i] += vbox_off.z;

	   
	 	// loop through frames, segmentation in parts to save memory
	 	int frame=0;
	 	Mat clip;  
	 	while (frame < boxvid.size[0]) 
	 	{
	 		// get next clip to add
	 		int clipz = (frame == 0 ? popt.win.z : popt.step.z);

	 		if (frame+clipz > boxvid.size[0]) 
	 		{
	 			break;
	 		}
	 		clip = MatVid::subvid(boxvid, Range(frame, frame+clipz), Range::all(), Range::all());
	 		// add frames
	 		if (!pe.addFrames(clip)) 
	 		{
	 			// we should not get here
	 			CV_Error(-1, "something wrong!");

	 		} 
	 		else 
	 		{
	 			// now add to our patch vector
	 			const vector<Mat> &mypat = pe.getPatches();  	 	    
	 			for (unsigned int i=0; i<mypat.size(); i++) 
	 			{
	 				// create a copy of the patch
	 				if(pe.locyx_mask[i]==true) //only valid patches
	 				{			
	 					Mat p;
	 					mypat[i].copyTo(p);
	 					patches.push_back(p);					
	 				}
	 				//save all locations
	 				locall.push_back(Point3i(pe.locyx[i].x + vbox_off.x,pe.locyx[i].y + vbox_off.y, pe.locz       + vbox_off.z));
	 				locall_mask.push_back(pe.locyx_mask[i]);		  
	 			}
	 			cout<<"Found Patches "<<patches.size()<<"/"<<mypat.size()<<endl;
	 			//Keep classes and discard patches
	 			std::vector<int> tmpclasses;
	 			dtm.getClasses(patches,tmpclasses);
	 			for(int cc=0;cc<patches.size();cc++)
	 			{
	 				allclasses.push_back(tmpclasses[cc]);
	 			}
	 			patches.clear();
	 		}
	 		frame += clipz;
	 	}
	 	//Get the segmentation mask
	 	osmask=segm_mask();
	 	//filter the mask
	 	osmask=maxvotefilt(osmask,filtxy,filtxy,filtz,K);
	 	//Get color segmentation
	 	colorMask(vid,osmask,segm,1); 

	 	int ttime=timerOff();
	 	cout<<"segmentation time for cur video is: "<<ttime<<endl;
	 	return segm;*/
		 return null;
	 }

}