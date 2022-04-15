package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;

import java.util.*;

/**
Copyright (c) 1995 The Board of Trustees of Purdue University.
Permission to use, copy, modify, and distribute this software and its documentation for any purpose,
without fee, and without written agreement is hereby granted, provided that the above copyright notice and
the following two paragraphs appear in all copies of this software.
In no event shall Purdue University be liable to any party for direct, indirect, special, incidental, or
consequential damages arising out of the use of this software and its documentation, even if Purdue University
has been advised of the possibility of such damage.
Purdue University specifically disclaims any warranties, including, but not limited to, the implied warranties
 of merchantability and fitness for a particular purpose. The software provided hereunder is on an “as
is” basis, and Purdue Univeristy has no obligation to provide maintenance, support, updates, enhancements,
or modifications.

This is a port of cluster-3.6.7 developed by:
Charles A. Bouman; School of ECE, Purdue University
Michael Shapiro; NCSA
Gregory W. Cook; School of ECE, Purdue University
C. Brian Atkins; School of ECE, Purdue University
Hui Cheng; School of ECE, Purdue University
Jennifer G. Dy; School of ECE, Purdue University
Sean Borman; Department of Electrical Engineering, University of Notre Dame

This is software for doing unsupervised
clustering. This is done by estimating the parameters
of a Gaussian mixture model using the EM algorithm.

*/

public class AlgorithmGaussianMixtureModelEM extends AlgorithmBase {
	// Set level of diagnostic printing
	private int clusterMessageVerboseLevel = 2;
	// initial number of clusters for each class
	private int init_num_of_subclasses;
	// number of classes
	private String input_file_directory;
	private String input_file_name;
	private String output_file_directory;
	private String output_file_name;
	// controls clustering model
	// full - (default) use full convariance matrices
	// diag - use diagonal convariance matrices
	private boolean full = true;
	// 0 - (default) estimate number of clusters
	// n - use n clusters in mixture model with n < init_number_of_subclasses
	private int number_of_clusters;
	private final int SIGNATURE_TYPE_MIXED = 1;
	
	public AlgorithmGaussianMixtureModelEM() {
		
	}
	
	public AlgorithmGaussianMixtureModelEM(int clusterMessageVerboseLevel, int init_num_of_subclasses,
			String input_file_directory, String input_file_name,
			String output_file_directory, String output_file_name,
			boolean full, int number_of_clusters) {
		this.clusterMessageVerboseLevel = clusterMessageVerboseLevel;
		this.init_num_of_subclasses = init_num_of_subclasses;
		this.input_file_directory = input_file_directory;
		this.input_file_name = input_file_name;
		this.output_file_directory = output_file_directory;
		this.output_file_name = output_file_name;
		this.full = full;
		this.number_of_clusters = number_of_clusters;
	}
	
	public void runAlgorithm() {
		SigSet S;
		int i,k;
		ClassSig Sig;
		int nclasses;
		int vector_dimension;
		File info_file;
		RandomAccessFile raFile;
		String str = null;
		String tmpStr;
		int nextIndex;
		int index;
		String classKPathName;
		int num_of_samples;
		if (number_of_clusters <0) {
			MipavUtil.displayError("number_of_clusters < 0");
			setCompleted(false);
			return;
		}
		
		if (number_of_clusters > init_num_of_subclasses) {
			MipavUtil.displayError("number_of_clusters > init_num_of_subclasses");
			setCompleted(false);
			return;
		}
		
		info_file = new File(input_file_directory + File.separator + input_file_name);
		
		try {
		    raFile = new RandomAccessFile(info_file, "r");
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "raFile = new RandomAccessFile(info_file, \"r\")");
			setCompleted(false);
			return;
		}
		
		try {
		    raFile.seek(0);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "raFile.seek(0)");
			setCompleted(false);
			return;
		}
		try {
            nclasses = Integer.valueOf(raFile.readLine().trim()).intValue();
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "nclasses = Integer.valueOf(raFile.readLine().trim()).intValue()");
			setCompleted(false);
			return;
		}
		try {
            vector_dimension = Integer.valueOf(raFile.readLine().trim()).intValue();
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "vector_dimension = Integer.valueOf(raFile.readLine().trim()).intValue()");
			setCompleted(false);
			return;
		}
		
		/* Initialize SigSet data structure */
		S = new SigSet();
	    I_InitSigSet(S);
	    I_SigSetNBands (S, vector_dimension);
	    I_SetSigTitle (S, "test signature set");
	    
	    /* Allocate memory for cluster signatures */
	    for(k=0; k<nclasses; k++) {
	      Sig = I_NewClassSig(S);
	      I_SetClassTitle (Sig, "test class signature");
	      for(i=0; i<init_num_of_subclasses; i++)
	        I_NewSubSig (S, Sig);
	    }
	    
	    /* Read data for each class */
	    for(k=0; k<nclasses; k++) {
	    	str = null;
	    	// data file name space number of samples
	    	try {
	    	    str = raFile.readLine().trim();	
	    	}
	    	catch (IOException e) {
				MipavUtil.displayError("IOException " + e + "String str = raFile.readLine().trim()");
				setCompleted(false);
				return;
			}
	    	
	    	nextIndex = str.indexOf(" ", 0);
	    	classKPathName = str.substring(0, nextIndex).trim();
	    	index = nextIndex + 1;
	    	tmpStr = str.substring(index, str.length()).trim();
	    	num_of_samples = Integer.valueOf(tmpStr).intValue();
	    	
	    	Sig = S.cSig.get(k);
	    } // for(k=0; k<nclasses; k++)
	}
	
	/* SigSet (Signature Set) data stucture used throughout package.         */
	/*   ClassSig (Class Signature) data stucture holds the parameters       */
	/*       of a single Gaussian mixture model. SigSet.nclasses is the      */
	/*       number of ClassSig's in a SigSet.                               */
	/*     SubSig (Subsignature) data stucture holds each component of a     */
	/*         Gaussian mixture model. SigSet.ClassSig[k].nsubclasses is the */
	/*         number of SubSig's in a ClassSig.                             */
	class SigSet {
	    int nbands;	
	    int nclasses;
	    String title;
	    Vector<ClassSig> cSig;
	    
	    public SigSet() {
	    	
	    }
	}
	
	class ClassSig {
	    long classnum;	
	    String title;
	    int used;
	    int type;
	    int nsubclasses;
	    Vector<SubSig> sSig;
	    ClassData cData;
	    
	    public ClassSig() {
	    	
	    }
	}
	
	class SubSig {
		double N;       /* expected number of pixels in subcluster */
        double pi;      /* probability of component in GMM */
        double means[];  /* mean of component in GMM */
        double R[][];     /* convarance of component in GMM */
        double Rinv[][];  /* inverse of R */
        double cnst;    /* normalizing constant for multivariate Gaussian */
        int used;
        
        public SubSig() {
        	
        }
	}
	
	class ClassData {
        int npixels;
        double SummedWeights;
        double x[][]; /* list of pixel vectors:     x[npixels][nbands] */
        double p[][]; /* prob pixel is in subclass: p[npixels][subclasses] */
        double w[]; /* weight of pixel:           w[npixels] */
        
        public ClassData() {
        	
        }
    } 
	
	private void I_InitSigSet(SigSet S)
	{
	  S.nbands = 0;
	  S.nclasses = 0;
	  S.cSig = null;
	  S.title = null;
	}
	
	private void I_SigSetNBands (SigSet S, int nbands) {
		S.nbands = nbands;	
	}
	

	private void I_SetSigTitle(SigSet S, String title)
	{
	  if (title == null) title = "";
	  if (S.title != null) {
		  S.title = null;
	  }
	  S.title = title;
	}
	
	private void I_SetClassTitle(ClassSig C,  String title)
	{
	  if (title == null) title = "";
	  if (C.title != null) {
		  C.title = null;
	  }
	  C.title = title;
	}

	private ClassSig I_NewClassSig(SigSet S) {
	  int i;
	  if (S.nclasses == 0) {
	    S.cSig = new Vector<ClassSig>();
	  }
	  ClassSig Sp = new ClassSig();
	  S.nclasses++;

	  Sp.classnum = 0;
	  Sp.nsubclasses = 0;
	  Sp.used = 1;
	  Sp.type = SIGNATURE_TYPE_MIXED;
	  Sp.title = null;
	  Sp.cData = new ClassData();
	  Sp.cData.npixels = 0;
	  Sp.cData.SummedWeights = 0.0;
	  Sp.cData.x = null;
	  Sp.cData.p = null;
	  Sp.cData.w = null;
	  S.cSig.add(Sp);
	    
	  return Sp;
	}
	
	private SubSig I_NewSubSig(SigSet S, ClassSig C)
	{
	  int i;

	  if (C.nsubclasses == 0) {
	    C.sSig = new Vector<SubSig>();
	  }
	  SubSig Sp = new SubSig();
	  C.nsubclasses++;
	  
	  Sp.used = 1;
	  Sp.R = new double[S.nbands][S.nbands];
	  Sp.Rinv = new double[S.nbands][S.nbands];
	  Sp.means = new double[S.nbands];
	  Sp.N = 0;
	  Sp.pi = 0;
	  Sp.cnst = 0;
	  C.sSig.add(Sp);
	  return Sp;
	}
	
}