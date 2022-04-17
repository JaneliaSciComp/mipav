package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

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
		
	}
	
	public void clust() {
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
		File output_file;
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
	      
		    if (!full) {
		    	/* assume covariance matrices to be diagonal */
		    	subcluster(S,k,number_of_clusters,CLUSTER_DIAG,Rmin,max_num);
		    }
		    else {
		    	/* no assumption for covariance matrices */	
		    	subcluster(S,k,number_of_clusters,CLUSTER_FULL,Rmin,max_num);
		    }
		    
		    if(clusterMessageVerboseLevel >= 2) {
		        System.out.println("Maximum number of subclasses = " + max_num[0]);
		    }
		    
		    I_DeallocClassData(S, Sig);
	    
	    } // for(k=0; k<nclasses; k++)
	    
	    /* Write out result to output parameter file */
        output_file = new File(output_file_directory + File.separator + output_file_name);
		
		try {
		    raFile = new RandomAccessFile(output_file, "rw");
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "raFile = new RandomAccessFile(output_file, \"rw\")");
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
		
		I_WriteSigSet(raFile, S);
		
		try {
	    	  raFile.close();
        }
        catch (IOException e) {
			MipavUtil.displayError("IOException " + e + "raFile.close()");
			setCompleted(false);
			return;
	    }
		
		/* De-allocate cluster signature memory */
	    I_DeallocSigSet(S); 
        setCompleted(true);
	    return;

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
	    boolean used;
	    int type;
	    int nsubclasses;
	    Vector<SubSig> sSig;
	    ClassData cData;
	    
	    public ClassSig() {
	        used = false;	
	    }
	}
	
	class SubSig {
		double N;       /* expected number of pixels in subcluster */
        double pi;      /* probability of component in GMM */
        double means[];  /* mean of component in GMM */
        double R[][];     /* convarance of component in GMM */
        double Rinv[][];  /* inverse of R */
        double cnst;    /* normalizing constant for multivariate Gaussian */
        boolean used;
        
        public SubSig() {
            used = false;	
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
	  Sp.used = true;
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
	    int min_i[] = new int[1];
	    int min_j[] = new int[1];
	    int status;
	    int nbands;
	    double rissanen;
	    double min_riss;
	    ClassSig Sig;
	    // static struct SigSet Smin;
	    SigSet Smin = new SigSet();

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
	    
	    /* EM algorithm */
	    min_riss = refine_clusters(Sig,nbands,Rmin,option);
	    
	    if(clusterMessageVerboseLevel >= 2) {
	        System.out.println("Subclasses = " + Sig.nsubclasses + "; Rissanen = " + min_riss);
	    }
	    
	    /* Save contents of Class Signature to Smin */
	    save_ClassSig(Sig,Smin,nbands);
	    
	    if(desired_num==0) {
	        while(Sig.nsubclasses>1) {
	          reduce_order(Sig,nbands,min_i,min_j);

	          if(clusterMessageVerboseLevel >= 2) {
	            System.out.println("Combining Subclasses ("+min_i+","+min_j+")");
	          }

	          rissanen = refine_clusters(Sig,nbands,Rmin,option);

	          if(clusterMessageVerboseLevel >= 2) {
	  	        System.out.println("Subclasses = " + Sig.nsubclasses + "; Rissanen = " + rissanen);
	  	      }

	          if(rissanen<min_riss)
	          {
	            min_riss = rissanen;

	            /* Delete old Smin, and save new Smin */
	            I_DeallocSigSet(Smin);
	            save_ClassSig(Sig,Smin,nbands);
	          }
	        }
	      }
	      else {
	        while( (Sig.nsubclasses>desired_num)&&(Sig.nsubclasses>0) ) {
	          reduce_order(Sig,nbands,min_i,min_j);

	          if(clusterMessageVerboseLevel >= 2) {
		          System.out.println("Combining Subclasses ("+min_i+","+min_j+")");
		      }
	   
	          rissanen = refine_clusters(Sig,nbands,Rmin,option);

	          if(clusterMessageVerboseLevel >= 2) {
		  	      System.out.println("Subclasses = " + Sig.nsubclasses + "; Rissanen = " + rissanen);
		  	  }

	          /* Delete old Smin, and save new Smin */
	          I_DeallocSigSet(Smin);
	          save_ClassSig(Sig,Smin,nbands);
	        }  
	      }
	    
	    /* Deallocate memory for class, and replace with solution */
	    while(Sig.nsubclasses>0) I_DeallocSubSig(Sig);
	    Sig.sSig = Smin.cSig.get(0).sSig;
	    Sig.nsubclasses = Smin.cSig.get(0).nsubclasses;

	    /* return warning status */
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

	     compute_constants(Sig,nbands);
	     normalize_pi(Sig);

	}
	
	private void DiagonalizeMatrix(double R[][], int nbands)
	{
	    int b1,b2;

	    for(b1=0; b1<nbands; b1++)
	    for(b2=0; b2<nbands; b2++)
	      if(b1!=b2) R[b1][b2] = 0;
	}
	
	/**********************************************************/
	/* invert matrix and compute Sig->SubSig[i].cnst          */
	/**********************************************************/
	private void compute_constants(ClassSig Sig, int nbands)
	{
	   int i;
	   int b1,b2;
	   double det_man[] = new double[1];
	   int det_exp[] = new int[1];

	   int indx[];
	   double y[][];
	   double col[];


	   /* allocate memory first time subroutine is called */
	   indx = new int[nbands];
	   y = new double[nbands][nbands]; 
	   col = new double[nbands];

	   /* invert matrix and compute constant for each subclass */
	   for(i=0; i<Sig.nsubclasses; i++) {
	     for(b1=0; b1<nbands; b1++)
	     for(b2=0; b2<nbands; b2++)
	       Sig.sSig.get(i).Rinv[b1][b2] = Sig.sSig.get(i).R[b1][b2];

	     clust_invert(Sig.sSig.get(i).Rinv,nbands,det_man,det_exp,indx,y,col);

	     Sig.sSig.get(i).cnst = (-nbands/2.0)*Math.log(2*Math.PI) 
	                            - 0.5*Math.log(det_man[0]) - 0.5*det_exp[0]*Math.log(10.0);
	   } 
	}
	
	/***********************************************************/
	/* inverts a matrix of arbitrary size input as a 2D array. */ 
	/***********************************************************/
	private int clust_invert(
	  double a[][],      /* input/output matrix */
	  int    n,        /* dimension */
	  double det_man[], /* determinant mantisa */
	  int    det_exp[], /* determinant exponent */
	  /* scratch space */
	  int    indx[],    /* indx = G_alloc_ivector(n);  */
	  double y[][],      /* y = G_alloc_matrix(n,n); */
	  double col[]      /* col = G_alloc_vector(n); */
	)
	{
		int  i,j;
		double  d_man[] = new double[1];
	        int d_exp;

	        d_exp = 0;
	        if(ludcmp(a,n,indx,d_man) != 0) {
	          for(j=0; j<n; j++) {
	            d_man[0] *= a[j][j];
	            while( Math.abs(d_man[0])>10 ) {
	              d_man[0] = d_man[0]/10;
	              d_exp++;
	            }
	            while( (Math.abs(d_man[0])<0.1)&&(Math.abs(d_man[0])>0) ) {
	              d_man[0] = d_man[0]*10;
	              d_exp--;
	            }
	          }
	          det_man[0] = d_man[0];
	          det_exp[0] = d_exp;
		  for(j=0; j<n; j++) {
		    for(i=0; i<n; i++) col[i]=0.0;
		    col[j]=1.0;
		    lubksb(a,n,indx,col);
		    for(i=0; i<n; i++) y[i][j]=col[i];
		  } 

		  for(i=0; i<n; i++)
		  for(j=0; j<n; j++) a[i][j]=y[i][j];
	          return(1);
	        }
	        else {
	          det_man[0] = 0.0;
	          det_exp[0] = 0;
	          return(0);
	        }
	}
	
	private int ludcmp(double a[][],int n,int indx[],double d[])
	{
		double TINY = 1.0e-20;

	    int i,j,k;
	    int imax = 0;
	    double big,dum,sum,temp;
	    double vv[];

	    vv=new double[n];
	    d[0]=1.0;
	    for (i=0;i<n;i++) 
	    {
	        big=0.0;
	        for (j=0;j<n;j++)
	            if ((temp=Math.abs(a[i][j])) > big)
			big=temp;
	        if (big == 0.0)
		    return 0; /* Singular matrix  */
	        vv[i]=1.0/big;
	    }
	    for (j=0;j<n;j++) 
	    {
	        for (i=0;i<j;i++) 
		{
	            sum=a[i][j];
	            for (k=0;k<i;k++)
			sum -= a[i][k]*a[k][j];
	            a[i][j]=sum;
	        }
	        big=0.0;
	        for (i=j;i<n;i++) 
		{
	            sum=a[i][j];
	            for (k=0;k<j;k++)
	                sum -= a[i][k]*a[k][j];
	            a[i][j]=sum;
	            if ( (dum=vv[i]*Math.abs(sum)) >= big) 
		    {
	                big=dum;
	                imax=i;
	            }
	        }
	        if (j != imax) 
		{
	            for (k=0;k<n;k++) 
		    {
	                dum=a[imax][k];
	                a[imax][k]=a[j][k];
	                a[j][k]=dum;
	            }
	            d[0] = -(d[0]);
	            vv[imax]=vv[j];
	        }
	        indx[j]=imax;

	 	/* Change made 3/27/98 for robustness */
	        if ( (a[j][j]>=0)&&(a[j][j]<TINY) ) a[j][j]= TINY;
	        if ( (a[j][j]<0)&&(a[j][j]>-TINY) ) a[j][j]= -TINY;

	        if (j != n-1) 
		{
	            dum=1.0/(a[j][j]);
	            for (i=j+1;i<n;i++)
			a[i][j] *= dum;
	        }
	    }
	    vv = null;
	    return(1);
	}


	
	private void lubksb(double a[][],int n,int indx[],double b[])
	{
	    int i,ii,ip,j;
	    double sum;

	    ii = -1;
	    for (i=0;i<n;i++)
	    {
	        ip=indx[i];
	        sum=b[ip];
	        b[ip]=b[i];
	        if (ii >= 0)
	            for (j=ii;j<i;j++)
			sum -= a[i][j]*b[j];
        else if (sum != 0)
		    ii=i;
	        b[i]=sum;
	    }
	    for (i=n-1;i>=0;i--)
	    {
	        sum=b[i];
	        for (j=i+1;j<n;j++)
		    sum -= a[i][j]*b[j];
	        b[i]=sum/a[i][i];
	    }
	}




	private void normalize_pi(ClassSig Sig)
	{
	    int i;
	    double sum;

	    sum = 0.0;
	    for(i=0; i<Sig.nsubclasses; i++) sum += Sig.sSig.get(i).pi;

	    if(sum>0) {
	      for(i=0; i<Sig.nsubclasses; i++) Sig.sSig.get(i).pi /= sum;
	    }
	    else {
	      for(i=0; i<Sig.nsubclasses; i++) Sig.sSig.get(i).pi = 0.0;
	    }
	}
	
	/*****************************************************************/
	/* Computes ML clustering of data using Gaussian Mixture model.  */
	/* Returns the values of the Rissen constant for the clustering. */
	/*****************************************************************/
	private double refine_clusters(
	    ClassSig Sig, 
	    int nbands, 
	    double Rmin, 
	    int option)
	{
	     int nparams_clust;
	     int num_params;
	     int ndata_points;
	     boolean repeat;
	     double rissanen_const;
	     double change,ll_new,ll_old;
	     double epsilon;

	     /* compute number of parameters per cluster */
	     nparams_clust = (int)(1+nbands+0.5*(nbands+1)*nbands);
	     if(option==CLUSTER_DIAG) nparams_clust = 1+nbands+nbands;

	     /* compute number of data points */
	     ndata_points = Sig.cData.npixels*nbands;

	     /* compute epsilon */
	     epsilon = nparams_clust*Math.log((double)ndata_points);
	     epsilon *= 0.01;

	     /* Perform initial regrouping */
	     ll_new = regroup(Sig,nbands);

	     /* Perform EM algorithm */
	     change = 2*epsilon;
	     do {
	       ll_old = ll_new;
	       reestimate(Sig,nbands,Rmin,option);

	       ll_new = regroup(Sig,nbands);
	       change = ll_new-ll_old;
	       repeat = change>epsilon;
	     } while(repeat);

	     /* compute Rissanens expression */
	     if(Sig.nsubclasses>0) {
	       num_params = Sig.nsubclasses*nparams_clust - 1;
	       rissanen_const = -ll_new + 0.5*num_params*Math.log((double)ndata_points);
	       return(rissanen_const);
	     }
	     else {
	       return((double)0);
	     }
	}
	
	private double regroup(ClassSig Sig, int nbands)
	{
	   int s;
	   int i;
	   double tmp;
	   double maxlike = 0.0;
	   double likelihood;
	   double subsum;
	   ClassData Data;

	   /* set data pointer */
	   Data = Sig.cData;

	   /* compute likelihoods */
	   likelihood = 0;
	   for(s=0; s<Data.npixels; s++)
	   {

	     for(i=0; i<Sig.nsubclasses; i++)
	     {
	       tmp = loglike(Data.x[s],Sig.sSig.get(i),nbands);
	       Data.p[s][i] = tmp;
	       if(i==0) maxlike = tmp;
	       if(tmp>maxlike) maxlike = tmp;
	     }

	     subsum = 0;
	     for(i=0; i<Sig.nsubclasses; i++)
	     {
	       tmp = Math.exp( Data.p[s][i]-maxlike )*Sig.sSig.get(i).pi;
	       subsum += tmp;
	       Data.p[s][i] = tmp;
	     }
	     likelihood += Math.log(subsum) + maxlike;

	     for(i=0; i<Sig.nsubclasses; i++)
	       Data.p[s][i] /= subsum;
	   }

	   return(likelihood);
	}
	
	private double loglike(double x[], SubSig sSig, int nbands)
	{
	    int b1,b2;
	    double diff1,diff2;
	    double sum;

	    sum = 0;
	    for(b1=0; b1<nbands; b1++) 
	    for(b2=0; b2<nbands; b2++)
	    {
	      diff1 = x[b1]-sSig.means[b1];
	      diff2 = x[b2]-sSig.means[b2];
	      sum += diff1*diff2*sSig.Rinv[b1][b2];
	    }

	    sum = -0.5*sum + sSig.cnst;
	    return(sum);
	}
	
	private void reestimate(ClassSig Sig, int nbands, double Rmin, int option)
	{
	     int i;
	     int s;
	     int b1,b2;
	     double diff1,diff2;
	     ClassData Data;

	     /* set data pointer */
	     Data = Sig.cData;
	     
	     /* Compute N */
	     for(i=0; i<Sig.nsubclasses; i++) 
	     {
	       Sig.sSig.get(i).N = 0;
	       for(s=0; s<Data.npixels; s++)
	         Sig.sSig.get(i).N += (Data.p[s][i])*(Data.w[s]);
	       Sig.sSig.get(i).pi = Sig.sSig.get(i).N;
	     }

	     /* Compute means and variances for each subcluster */
	     for(i=0; i<Sig.nsubclasses; i++) 
	     {
	       /* Compute mean */
	       for(b1=0; b1<nbands; b1++) 
	       {
	         Sig.sSig.get(i).means[b1] = 0;
	         for(s=0; s<Data.npixels; s++)
	           Sig.sSig.get(i).means[b1] += Data.p[s][i]*Data.x[s][b1]*Data.w[s];
	         Sig.sSig.get(i).means[b1] /= Sig.sSig.get(i).N;
	       }
		
	       /* Compute R */
	       for(b1=0; b1<nbands; b1++) 
	       for(b2=b1; b2<nbands; b2++)
	       {
	         Sig.sSig.get(i).R[b1][b2] = 0;
	         for(s=0; s<Data.npixels; s++)
	         {
	           diff1 = Data.x[s][b1] - Sig.sSig.get(i).means[b1];
	           diff2 = Data.x[s][b2] - Sig.sSig.get(i).means[b2];
	           Sig.sSig.get(i).R[b1][b2] += Data.p[s][i]*diff1*diff2*Data.w[s];
	         }
	         Sig.sSig.get(i).R[b1][b2] /= Sig.sSig.get(i).N;
	         Sig.sSig.get(i).R[b2][b1] = Sig.sSig.get(i).R[b1][b2];
	       }

	       /* Regularize matrix */
	       for(b1=0; b1<nbands; b1++) {
	         Sig.sSig.get(i).R[b1][b1] += Rmin;
	       }

	       if(option==CLUSTER_DIAG) DiagonalizeMatrix(Sig.sSig.get(i).R,nbands);
	     }

	     /* Normalize probabilities for subclusters */
	     normalize_pi(Sig);

	     /* Compute constants */
	     compute_constants(Sig,nbands);
	     normalize_pi(Sig);
	}

	/**********************/
	/* saves Sig1 to Sig2 */
	/**********************/
	private void save_ClassSig(
	    ClassSig Sig1,
	    SigSet S,
	    int nbands)
	{
	    ClassSig Sig2;

	    I_InitSigSet (S);
	    I_SigSetNBands (S, nbands);
	    Sig2 = I_NewClassSig(S);
	    while(Sig2.nsubclasses<Sig1.nsubclasses) I_NewSubSig(S, Sig2);
	    copy_ClassSig(Sig1,Sig2,nbands);
	}
	
	/*********************/
	/* copy Sig1 to Sig2 */
	/*********************/
	private void copy_ClassSig(
	    ClassSig Sig1,
	    ClassSig Sig2,
	    int nbands)
	{
	    int i;

	    Sig2.classnum = Sig1.classnum;
	    Sig2.title = Sig1.title;
	    Sig2.used = Sig1.used;
	    Sig2.type = Sig1.type;
	    Sig2.nsubclasses = Sig1.nsubclasses;
	    for(i=0; i<Sig1.nsubclasses; i++)
	      copy_SubSig((Sig1.sSig.get(i)),(Sig2.sSig.get(i)),nbands);
	}
	
	/***************************/
	/* copy SubSig1 to SubSig2 */
	/***************************/
	private void copy_SubSig(
	    SubSig SubSig1,
	    SubSig SubSig2,
	    int nbands)
	{
	    int b1,b2;

	    SubSig2.N = SubSig1.N;
	    SubSig2.pi = SubSig1.pi;
	    SubSig2.cnst = SubSig1.cnst;
	    SubSig2.used = SubSig1.used;

	    for(b1=0; b1<nbands; b1++)
	      SubSig2.means[b1] = SubSig1.means[b1];

	    for(b1=0; b1<nbands; b1++)
	    for(b2=0; b2<nbands; b2++)
	    {
	      SubSig2.R[b1][b2] = SubSig1.R[b1][b2];
	      SubSig2.Rinv[b1][b2] = SubSig1.Rinv[b1][b2];
	    }
	}
	
	private void reduce_order(
		    ClassSig Sig,
		    int nbands,
		    int min_ii[],
		    int min_jj[])
		{
		    int i,j;
		    int min_i = 0;
		    int min_j = 0;
		    double dist;
		    double min_dist = 0.0;
		    SubSig SubSig1,SubSig2;

		    SigSet S = new SigSet();
		    ClassSig Sig3;
		    SubSig SubSig3;

		    /* allocate scratch space first time subroutine is called */
		    I_InitSigSet (S);
		    I_SigSetNBands (S, nbands);
		    Sig3 = I_NewClassSig(S);
		    SubSig3 = I_NewSubSig (S, Sig3);

		    if(Sig.nsubclasses>1)
		    {
		      /* find the closest subclasses */
		      for(i=0; i<Sig.nsubclasses-1; i++)
		      for(j=i+1; j<Sig.nsubclasses; j++)
		      {
		        dist = distance((Sig.sSig.get(i)),(Sig.sSig.get(j)),nbands);
		        if((i==0)&&(j==1))
		        {
		          min_dist = dist;
		          min_i = i;
		          min_j = j;
		        }
		        if(dist<min_dist)
		        {
		          min_dist = dist;
		          min_i = i;
		          min_j = j;
		        }
		      }

		      /* Save result for output */
		      min_ii[0] = min_i; min_jj[0] = min_j;

		      /* Combine Subclasses */
		      SubSig1 = Sig.sSig.get(min_i);
		      SubSig2 = Sig.sSig.get(min_j);
		      add_SubSigs(SubSig1,SubSig2,SubSig3,nbands);
		      copy_SubSig(SubSig3,SubSig1,nbands);

		      /* remove extra subclass */
		      for(i=min_j; i<Sig.nsubclasses-1; i++)
		        copy_SubSig((Sig.sSig.get(i+1)),(Sig.sSig.get(i)),nbands);

		      /* Remove last Subclass */
		      /* (Sig->nsubclasses)--; */
		      I_DeallocSubSig(Sig); 

		      /* Rerun compute_constants */
		      compute_constants(Sig,nbands);
		      normalize_pi(Sig);
		    }
		}

	private double distance(
		    SubSig SubSig1,
		    SubSig SubSig2,
		    int nbands)
		{
		    double dist;

		    SigSet S = new SigSet();
		    ClassSig Sig3;
		    SubSig SubSig3;


		    /* allocate scratch space first time subroutine is called */
		    I_InitSigSet (S);
		    I_SigSetNBands (S, nbands);
		    Sig3 = I_NewClassSig(S);
		    SubSig3 = I_NewSubSig (S, Sig3);

		    /* form SubSig3 by adding SubSig1 and SubSig2 */
		    add_SubSigs(SubSig1,SubSig2,SubSig3,nbands);

		    /* compute constant for SubSig3 */
		    compute_constants(Sig3,nbands);

		    /* compute distance */
		    dist = SubSig1.N*SubSig1.cnst + SubSig2.N*SubSig2.cnst
		           - SubSig3.N*SubSig3.cnst;

		    return(dist);
		}

	/*******************************************/
	/* add SubSig1 and SubSig2 to form SubSig3 */
	/*******************************************/
	private void add_SubSigs(
	    SubSig SubSig1,
	    SubSig SubSig2,
	    SubSig SubSig3,
	    int nbands)
	{
	    int b1,b2;
	    double wt1,wt2;
	    double tmp;

	    wt1 = SubSig1.N/(SubSig1.N + SubSig2.N);
	    wt2 = 1 - wt1;

	    /* compute means */
	    for(b1=0; b1<nbands; b1++)
	      SubSig3.means[b1] = wt1*SubSig1.means[b1] + wt2*SubSig2.means[b1];

	    /* compute covariance */
	    for(b1=0; b1<nbands; b1++)
	    for(b2=b1; b2<nbands; b2++)
	    {
	      tmp = (SubSig3.means[b1]-SubSig1.means[b1])
	            *(SubSig3.means[b2]-SubSig1.means[b2]);
	      SubSig3.R[b1][b2] = wt1*(SubSig1.R[b1][b2] + tmp);
	      tmp = (SubSig3.means[b1]-SubSig2.means[b1])
	            *(SubSig3.means[b2]-SubSig2.means[b2]);
	      SubSig3.R[b1][b2] += wt2*(SubSig2.R[b1][b2] + tmp);
	      SubSig3.R[b2][b1] = SubSig3.R[b1][b2];
	    }

	    /* compute pi and N */
	    SubSig3.pi = SubSig1.pi + SubSig2.pi;
	    SubSig3.N = SubSig1.N + SubSig2.N;
	}

	private void I_DeallocSubSig(ClassSig C)
	{
	  int i;
	  SubSig Sp = C.sSig.remove(C.sSig.size()-1);
	  C.nsubclasses--;
      for (i = 0; i < Sp.R.length; i++) {
    	  Sp.R[i] = null;
      }
      Sp.R = null;
      for (i = 0; i < Sp.Rinv.length; i++) {
    	  Sp.Rinv[i] = null;
      }
      Sp.Rinv = null;
	  Sp.means = null;
	}
	
	private void I_DeallocSigSet(SigSet S)
	{
	  while(S.nclasses>0) {
	    I_DeallocClassSig( S );
	  }
	}
	
	private void I_DeallocClassSig(SigSet S)
	{
	  ClassSig Sp = S.cSig.remove(S.cSig.size()-1);
	  S.nclasses--;
	    
	  I_DeallocClassData( S, Sp ); 
	    
	  while(Sp.nsubclasses>0) {
	    I_DeallocSubSig(Sp);
	  }
	}
	
	private void I_DeallocClassData(SigSet S, ClassSig C)
	{
      int i;
	  ClassData Data = C.cData;
	  for (i = 0; i < Data.x.length; i++) {
		  Data.x[i] = null;
	  }
	  Data.x = null;
	  for (i = 0; i < Data.p.length; i++) {
		  Data.p[i] = null;
	  }
	  Data.p = null;
	  Data.w = null;
	  Data.npixels = 0;
	  Data.SummedWeights = 0.0;
	}
	
	private void I_WriteSigSet(RandomAccessFile raFile, SigSet S)
	{
	  ClassSig Cp;
	  SubSig Sp;
	  int i,j, b1, b2;
	  String val;

	  try {
		  raFile.writeBytes("title: " + I_GetSigTitle(S) + "\n");
		  raFile.writeBytes("nbands: " + String.valueOf(S.nbands) + "\n");
		  for (i=0; i < S.nclasses; i++)
		  {
		    Cp = S.cSig.get(i);
		    if (!Cp.used) continue;
		    raFile.writeBytes("class:\n");
		    raFile.writeBytes(" classnum: " + String.valueOf(Cp.classnum) + "\n");
		    raFile.writeBytes(" classtitle: " + I_GetClassTitle(Cp) + "\n");
		    raFile.writeBytes(" classtype: " + String.valueOf(Cp.type) + "\n");
	
		    for (j = 0; j < Cp.nsubclasses; j++) {
		      Sp = Cp.sSig.get(j);
		      raFile.writeBytes(" subclass:\n");
		      raFile.writeBytes("  pi: " + String.valueOf(Sp.pi) + "\n");
		      raFile.writeBytes("  means:");
		      for (b1 = 0; b1 < S.nbands; b1++)
				raFile.writeBytes(" " + String.valueOf(Sp.means[b1]));
		      raFile.writeBytes("\n");
		      raFile.writeBytes("  covar:\n");
		      for (b1 = 0; b1 < S.nbands; b1++) {
			      raFile.writeBytes("   ");
			      for (b2 = 0; b2 < S.nbands; b2++)
			          raFile.writeBytes(" " + String.valueOf(Sp.R[b1][b2]));
			      raFile.writeBytes("\n");
		      } // for (b1 = 0; b1 < S.nbands; b1++)
		      raFile.writeBytes(" endsubclass:\n");
		    } // for (j = 0; j < Cp.nsubclasses; j++)
		    raFile.writeBytes("endclass:\n");
		  } // for (i=0; i < S.nclasses; i++)
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IOExeption " + e + " in I_WriteSigSet");
		  System.exit(-1);
	  }
	}

	private String I_GetSigTitle(SigSet S)
	{
	  if (S.title != null) return S.title;
	  else return "";
	}
	
	private String I_GetClassTitle(ClassSig C)
	{
	  if (C.title != null) return C.title;
	  else return "";
	}
}