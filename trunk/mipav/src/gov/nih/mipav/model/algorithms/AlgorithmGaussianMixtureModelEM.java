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
	private double COVAR_DYNAMIC_RANGE = 1E5;
	private final int CLUSTER_FULL = 1; /* Use full covariance matrix in clustering */
	private final int CLUSTER_DIAG = 0; /* Use diagonal covariance matrix in clustering */
	
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
		int i,j,k;
		ClassSig Sig = null;
		int nclasses;
		int vector_dimension;
		File info_file;
		RandomAccessFile raFile;
		String str = null;
		String tmpStr;
		int nextIndex;
		int index;
		String classKName;
		int num_of_samples;
		File data_file;
		RandomAccessFile raDataFile;
		double Rmin;
		int max_num[] = new int[1];
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
	    	classKName = str.substring(0, nextIndex).trim();
	    	index = nextIndex + 1;
	    	tmpStr = str.substring(index, str.length()).trim();
	    	num_of_samples = Integer.valueOf(tmpStr).intValue();
	    	
	    	Sig = S.cSig.get(k);
	    	
	    	I_AllocClassData (S, Sig, num_of_samples);
	    	
	    	/* Read Data */
	    	data_file = new File(input_file_directory + File.separator + classKName);
			
			try {
			    raDataFile = new RandomAccessFile(data_file, "r");
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + "raDataFile = new RandomAccessFile(data_file, \"r\")");
				setCompleted(false);
				return;
			}
			
			try {
			    raDataFile.seek(0);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException " + e + "raDataFile.seek(0)");
				setCompleted(false);
				return;
			}
			
			for(i=0; i<Sig.cData.npixels; i++) {
				try {
		    	    str = raDataFile.readLine().trim();
		    	    index = 0;
		    	}
		    	catch (IOException e) {
					MipavUtil.displayError("IOException " + e + "String str = raDataFile.readLine().trim()");
					setCompleted(false);
					return;
				}
		        for(j=0; j<vector_dimension; j++) {
		        	nextIndex = str.indexOf(" ", index);

	                if (nextIndex != -1) {
	                    tmpStr = str.substring(index, nextIndex).trim();
	                    index = nextIndex + 1;
	                } else { // spaces trimmed from end
	                    tmpStr = str.substring(index, str.length()).trim();
	                    index = nextIndex;
	                }
	                Sig.cData.x[i][j] = Double.valueOf(tmpStr).doubleValue();
		        }
		        
		      }
		      try {
		    	  raDataFile.close();
		      }
		      catch (IOException e) {
					MipavUtil.displayError("IOException " + e + "raDataFile.close()");
					setCompleted(false);
					return;
			  }
		      
		      /* Set unity weights and compute SummedWeights */
		      Sig.cData.SummedWeights = 0.0;
		      for(i=0; i<Sig.cData.npixels; i++) {
		        Sig.cData.w[i] = 1.0;
		        Sig.cData.SummedWeights += Sig.cData.w[i];
		      }
	    } // for(k=0; k<nclasses; k++)
	    
	    try {
	    	  raFile.close();
	      }
	      catch (IOException e) {
				MipavUtil.displayError("IOException " + e + "raFile.close()");
				setCompleted(false);
				return;
		  }
	    
	    /* Compute the average variance over all classes */
	    Rmin = 0;
	    for(k=0; k<nclasses; k++) {
	      Sig = S.cSig.get(k);
	      Rmin += AverageVariance(Sig, vector_dimension);
	    }
	    Rmin = Rmin/(COVAR_DYNAMIC_RANGE*nclasses);
	    
	    /* Perform clustering for each class */
	    for(k=0; k<nclasses; k++) {

	      Sig = S.cSig.get(k);
	      
	      if(clusterMessageVerboseLevel >= 1) {
	          System.out.println("Start clustering class " + k + "\n");
	      }
	      
	    } // for(k=0; k<nclasses; k++)

	    if (!full) {
	    	/* assume covariance matrices to be diagonal */
	    	subcluster(S,k,number_of_clusters,CLUSTER_DIAG,Rmin,max_num);
	    }
	    else {
	    	/* no assumption for covariance matrices */	
	    }
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
	
	private ClassData I_AllocClassData(SigSet S, ClassSig C, int npixels)
	{

	  ClassData Data = C.cData;
	  Data.npixels = npixels;
	  Data.x = new double[npixels][S.nbands];
	  Data.p = new double[npixels][C.nsubclasses];
	  Data.w = new double[npixels];
	  return Data;
	}
	
	private double AverageVariance(ClassSig Sig, int nbands)
	{
	     int     i,b1;
	     double  mean[];
	     double R[][];
	     double Rmin;

	     /* Compute the mean of variance for each band */
	     mean = new double[nbands];
	     R = new double[nbands][nbands];
	     
	     for(b1=0; b1<nbands; b1++) {
	         mean[b1] = 0.0;
	         for(i=0; i<Sig.cData.npixels; i++) {
	           mean[b1] += (Sig.cData.x[i][b1])*(Sig.cData.w[i]);
	         }
	         mean[b1] /= Sig.cData.SummedWeights;
	     }
	     
	     for(b1=0; b1<nbands; b1++) {
	         R[b1][b1] = 0.0;
	         for(i=0; i<Sig.cData.npixels; i++) {
	           R[b1][b1] += (Sig.cData.x[i][b1])*(Sig.cData.x[i][b1])*(Sig.cData.w[i]);
	         }
	         R[b1][b1] /= Sig.cData.SummedWeights;
	         R[b1][b1] -= mean[b1]*mean[b1];
	     }
	     
	     /* Compute average of diagonal entries */
	     Rmin = 0.0;
	     for(b1=0; b1<nbands; b1++) 
	       Rmin += R[b1][b1];

	     Rmin = Rmin/(nbands);
	     
	     mean = null;
	     for (i = 0; i < R.length; i++) {
	    	 R[i] = null;
	     }
         R = null;
         
	     return Rmin;

	}
	
	private int subcluster(
	    SigSet S, /* Input: structure contataining input data */ 
	    int Class_Index,  /* Input: index corresponding to class to be processed */
	    int desired_num,  /* Input: desired number of subclusters. */
	                      /*      0=>ignore this input. */
	    int option,       /* Input: type of clustering to use */
	                      /*      option=1=CLUSTER_FULL=>full covariance matrix */
	                      /*      option=0=CLUSTER_DIAG=>diagonal covariance matrix */
	    double Rmin,      /* Minimum value for diagonal elements of convariance */
	    int Max_num[])     /* Output: maximum number of allowed subclusters */
		{
		int nparams_clust;
	    int ndata_points;
	    int min_i,min_j;
	    int status;
	    int nbands;
	    double rissanen;
	    double min_riss;
	    ClassSig Sig;
	    // static struct SigSet Smin;
	    SigSet Smin;

	    status = 0;
	    
	    /* set class pointer */
	    Sig = S.cSig.get(Class_Index);

	    /* set number of bands */
	    nbands = S.nbands;
	    
	    /* compute number of parameters per cluster */
	    nparams_clust = (int)(1+nbands+0.5*(nbands+1)*nbands);
	    if(option==CLUSTER_DIAG) nparams_clust = 1+nbands+nbands;
	    
	    /* compute number of data points */
	    ndata_points = Sig.cData.npixels*nbands;

	    /* compute maximum number of subclasses */
	    ndata_points = Sig.cData.npixels*nbands;
	    Max_num[0] = (ndata_points+1)/nparams_clust - 1;

	    /* check for too many subclasses */
	    if(Sig.nsubclasses > (Max_num[0]/2) )
	    {
	      Sig.nsubclasses = Max_num[0]/2;
	      System.err.println("Too many subclasses for class index " + Class_Index);
	      System.err.println("         number of subclasses set to " + Sig.nsubclasses + "\n");
	      status = -2;
	    }
	    
	    /* initialize clustering */
	    seed(Sig,nbands,Rmin,option);
	    
	    return status;
		
		}
	
	/******************************************************************/
	/* Computes initial values for parameters of Gaussian Mixture     */
	/* model. The subroutine returns the minimum allowed value for    */
	/* the diagonal entries of the convariance matrix of each class.  */
	/*****************************************************************/
	private void seed(ClassSig Sig, int nbands, double Rmin, int option)
	{
	     int     i,b1,b2;
	     double  period;
	     double  mean[];
	     double R[][];
	     
	     /* Compute the mean of variance for each band */
	     mean = new double[nbands];
	     R = new double[nbands][nbands];
	     
	     for(b1=0; b1<nbands; b1++) {
	         mean[b1] = 0.0;
	         for(i=0; i<Sig.cData.npixels; i++) {
	           mean[b1] += (Sig.cData.x[i][b1])*(Sig.cData.w[i]);
	         }
	         mean[b1] /= Sig.cData.SummedWeights;
	     }
	     
	     for(b1=0; b1<nbands; b1++) 
	         for(b2=0; b2<nbands; b2++) {
	           R[b1][b2] = 0.0;
	           for(i=0; i<Sig.cData.npixels; i++) {
	             R[b1][b2] += (Sig.cData.x[i][b1])*(Sig.cData.x[i][b2])*(Sig.cData.w[i]);
	           }
	           R[b1][b2] /= Sig.cData.SummedWeights;
	           R[b1][b2] -= mean[b1]*mean[b2];
	     }
	     
	     /* If diagonal clustering is desired, then diagonalize matrix */
	     if(option==CLUSTER_DIAG) DiagonalizeMatrix(R,nbands);
	     
	     /* Compute the sampling period for seeding */
	     if(Sig.nsubclasses>1) {
	       period = (Sig.cData.npixels-1)/(Sig.nsubclasses-1.0);
	     }
	     else period =0;
	     
	     /* Seed the means and set the covarience components */
	     for(i=0; i<Sig.nsubclasses; i++) {
	       for(b1=0; b1<nbands; b1++) {
	         Sig.sSig.get(i).means[b1] = Sig.cData.x[(int)(i*period)][b1];
	       }

	       for(b1=0; b1<nbands; b1++)
	       for(b2=0; b2<nbands; b2++) {
	         Sig.sSig.get(i).R[b1][b2] = R[b1][b2];
	       }
	       for(b1=0; b1<nbands; b1++) {
	         Sig.sSig.get(i).R[b1][b1] += Rmin;
	       }
	       Sig.sSig.get(i).pi = 1.0/Sig.nsubclasses;
	     }

	     mean = null;
	     for (i = 0; i < R.length; i++) {
	    	 R[i] = null;
	     }
	     R = null;

	     /*compute_constants(Sig,nbands);
	     normalize_pi(Sig);*/

	}
	
	private void DiagonalizeMatrix(double R[][], int nbands)
	{
	    int b1,b2;

	    for(b1=0; b1<nbands; b1++)
	    for(b2=0; b2<nbands; b2++)
	      if(b1!=b2) R[b1][b2] = 0;
	}
	
}