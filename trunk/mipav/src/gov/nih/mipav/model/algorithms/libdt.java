package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;
import java.time.format.DateTimeFormatter;  
import java.time.LocalDateTime; 
import javax.vecmath.*;

/**
libdt - OpenCV library for Dynamic Textures - version 1.0

Copyright (c) 2011-2014 Antoni B. Chan, Adeel Mumtaz, City University of Hong Kong
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither name of copyright holders nor the names of its contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

public class libdt extends AlgorithmBase {
	private RandomAccessFile raFile;
	private boolean endian = false;
	/** byte array for int * */
    private final byte[] byteIntBuffer = new byte[4];
    /** byte array for double * */
    private final byte[] byteDoubleBuffer = new byte[8];
    private final int CV_8U = 0;
    private final int CV_64F = 6;
    private final int CV_64FC3 = 22;
	
	public libdt() {
		
	}
/*!
 * <Full 20 trial BoS classification on UCLA9 Eight Class Data Set!!>
 * 
 * Copyright (c) 2014 by <Adeel Mumtaz/ VISAL@City University of Hong Kong>
 * libdt - OpenCV library for Dynamic Textures - version 1.0
 */

/**
#include <iostream>
#include <iomanip>
#include <string>
#include <iterator>
#include<fstream>
#include<math.h>
#include <ctime>

#include "opencv/cv.h"
#include "opencv/cxcore.h"
#include "opencv/highgui.h"

#include "options.h"
#include "utils/libutils.h"
#include "stats/libstats.h"
#include "video/libvideo.h"
#include "dytex/libdytex.h"
#include "platInd/platInd.hpp"
#include "bufferer/Bufferer.hpp"

using namespace cv;
using namespace std;
*/



/**
int main(int argc, char** argv)
{
	cout<<"Experiment Started: "<<getTime()<<endl;

	
	string fpath="../../testdata/HEM/47fa110.dtm";

	//load existing	dtm	
	DytexMix dtm;
	Bufferer buf(fpath,fstream::in | fstream::binary);
	buf.read(dtm);	
	buf.close();

	

	//setting up HEM to reduce mixture to only 4 components
	DytexRegOptions ropt(CovMatrix::COV_REG_MIN,0.01,CovMatrix::COV_REG_MIN,0.01,CovMatrix::COV_REG_MIN,0.01,CovMatrix::COV_REG_ADD,0.999);
	HEMOptions hopt(4,ropt,0.0001,DytexOptions::NONZERO_YMEAN,HEMOptions::COMPACT);

	//split schedule of 1,2,4
	for(int i=1;i<=4;i=i*2)
	hopt.splitOpt.sched.push_back(i);


	//run The HEM 
	DytexMix emout=dtm.reduceWithSplitting(hopt);
	
		
	for(int i=0;i<emout.alpha.size();i++)
		cout<<"Alpha "<<i+1<<" is "<<std::fixed<<std::setprecision(14)<<emout.alpha[i]<<endl;
	
	
	cout<<"Experiment Finish: "<<getTime()<<endl;


}




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
    	}
    	catch (FileNotFoundException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);
    	}
    	//load existing	dtm	
    	DytexMix dtm = new DytexMix();
    	read(dtm);
    	try {
    		raFile.close();
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);
    	}
    	
    	//setting up HEM to reduce mixture to only 4 components
    	DytexRegOptions ropt = new DytexRegOptions (cov_reg_type.COV_REG_MIN,0.01,cov_reg_type.COV_REG_MIN,0.01,cov_reg_type.COV_REG_MIN,0.01,
    			cov_reg_type.COV_REG_ADD,0.999);
    	HEMOptions hopt = new HEMOptions(4,ropt,0.0001,Ymean_type.NONZERO_YMEAN,Verbose_mode.COMPACT);
    }
    
    /*!
	 * \brief
	 * Verbose modes.	 
	 */
	public enum Verbose_mode{QUITE,COMPACT,VERBOSE};
    
    /*!
     * \brief
     * HEM learninig options.
     *
     * \remarks
     * In HEM implementation few options are not implemented and their default values
     * are used instead
     * \see
     * DytexMix | EMOptions
     */
    public class HEMOptions
    {
    	/*!
    	 * \brief
    	 * number of clusters.	 
    	 */
    	public int K;
    	/*!
    	 * \brief
    	 * number of virtual samples.	 
    	 */
    	public int N;
    	/*!
    	 * \brief
    	 * temporal length of virtual samples.	 
    	 */
    	public int tau;
    	/*!
    	 * \brief
    	 * regularization options.	 
    	 * \see
    	 * DytexRegOptions
    	 */
    	public DytexRegOptions regopt;
    	/*!
    	 * \brief
    	 * termination parameter.	 
    	 */
    	double termvalue;
    	/*!
    	 * \brief
    	 * termination value for the HEMBEST.	 
    	 */
    	double termvalBest;  //termination value for the EMBEST
    	/*!
    	 * \brief
    	 * maximum number of iterations.	 
    	 */
    	int maxiter;
    	/*!
    	 * \brief
    	 * Verbose mode.	 
    	 */
    	Verbose_mode verbose;
    	/*!
    	 * \brief
    	 * empty cluster splitting options.	 
    	 * \see
    	 * DytexSplitParams
    	 */
    	DytexSplitParams emptySplitOpt = new DytexSplitParams();
    	/*!
    	 * \brief
    	 * cluster splitting options.	 
    	 * \see
    	 * DytexSplitParams
    	 */
    	DytexSplitParams splitOpt = new DytexSplitParams();
    	/*!
    	 * \brief
    	 * assume DT are zero-mean or not.	 
    	 */
        public Ymean_type Ymean;

        /*!
         * \brief
         * initialize HEMOptions object.
         * 
         * \param K
         * number of clusters.
         * 
         * \param regopt
         * regularization options.
         * 
         * \param termvalue
         * termination parameter.
         * 
         * \param ymean
         * assume DT are zero-mean or not.	 
         * 
         * \param verbose
         * verbose value.
         * 
         * \see
         * DytexOptions | DytexMix | EMOptions
         */
        public HEMOptions(int K,DytexRegOptions reg,double termvalue,Ymean_type ym,Verbose_mode verbose)
        {
        	//setting parameters
        	this.K=K;
        	this.verbose=verbose;
        	this.termvalue=termvalue;
        	this.termvalBest=1e-5;  //default
        	maxiter=500;

        	//setting empty cluster splitting options
        	emptySplitOpt.crit=Split_crit.SPLITP;	
        	emptySplitOpt.pert=0.01;
        	emptySplitOpt.mode=Split_mode.MODEP;
        	emptySplitOpt.vars=Split_vars.VARC;
        	Ymean=ym;
        	N=1000;
        	tau=20;

        }
    };
    
    /*!
	 * \brief
	 * splitting criteria options.
	 * 'SPLITQ' -- split component with largest Q variance
	 * 'SPLITP' -- split component with largest prior
	 */
	public enum Split_crit{SPLITQ,SPLITP};
	
	/*!
	 * \brief
	 *  perturbation mode options.	 
	 * 'MODEP' -- = scale up principal axis
	 */
	public enum Split_mode{MODEP};
	
	/*!
	 * \brief
	 * perturbation variable option, 'VARC' - observation matrix.	 
	 */
	public enum Split_vars{VARC};
    
    /*!
     * \brief
     * Component splitting options in EM and HEM.
     *
     * \see
     * DytexMix
     */
    public class DytexSplitParams
    {
    	/*!
    	 * \brief
    	 * the growing schedule.	 
    	 */
    	public Vector<Integer> sched = new Vector<Integer>();	
    	
    	/*!
    	 * \brief
    	 * splitting criteria.	 
    	 */
    	public Split_crit crit;
    	
    	/*!
    	 * \brief
    	 * perturbation mode.	 
    	 */
    	public Split_mode mode;
    	/*!
    	 * \brief
    	 * perturbation amount.	 
    	 */
    	public double pert;
    	
    	/*!
    	 * \brief
    	 * variables to perturb.	 
    	 */
    	public Split_vars vars;	
    	/*!
    	 * \brief
    	 * indices of components to ignore for splitting.	 
    	 */
    	Vector<Integer> ignore = new Vector<Integer>();
    	/*!
    	 * \brief
    	 * the index of the new component.	 
    	 */
    	public int target;

    	//initialize DytexSplitParams
    	public DytexSplitParams() {
    		
    	};
    };
    
    /** Options for modeling the observation mean, Ymean.  */
	public enum Ymean_type {ZERO_YMEAN (0),    /**< assume observations are already zero-mean, i.e. Ymean=0. */
	                   NONZERO_YMEAN (1),  /**< model non-zero observation mean. */
	                   ILLEGAL_YMEAN (2);
	       public final int Ymean_code;
	          
	       Ymean_type(int Ymean_code) {
	    		this.Ymean_code = Ymean_code;
	    	}
	}
	
	 public Ymean_type getYmean_type(int Ymean_code) {
	    	if (Ymean_code == 0) {
	    		return Ymean_type.ZERO_YMEAN;
	    	}
	    	else if (Ymean_code == 1) {
	    		return Ymean_type.NONZERO_YMEAN;
	    	}
	    	
	    	else {
	    		MipavUtil.displayError("Illegal number = " + Ymean_code + " for Ymean_type");
	    		return Ymean_type.ILLEGAL_YMEAN;
	    	}
	    }
    
    public class DytexOptions {  	
    	// DT options  
    	public int n[] = new int[1];    /**< dimension of the state space, x_t. */
    	public int m[] = new int[1];    /**< dimension of the observation space, y_t. */
    	public cov_type Ropt; /**< covariance type for R (usually COV_IID). */
    	public cov_type Sopt; /**< covariance type for S (usually COV_DIAG). */
    	Ymean_type Yopt; /**< option to model observation mean. */
    	public DytexOptions() {
    		
    	}
    }
    
    /** Class for specifying the regularization methods for a Dytex. 
    \sa CovMatrix, Dytex, DytexOptions
    */
    public class DytexRegOptions {
    	//options
    	  public cov_reg_type Ropt,  /**< Regularization method for R. */
    	                          Qopt,  /**< Regularization method for Q. */
    	                          Sopt,  /**< Regularization method for S. */
    							  Aopt;  /**< Regularization method for S. */
    	  public double Rval,  /**< Regularization value for R. */
    	         Qval,  /**< Regularization value for Q. */
    	         Sval,  /**< Regularization value for S. */
    			 Aval;  /**< Regularization value for S. */
    	public DytexRegOptions(cov_reg_type Ropt, double Rval, 
    			  cov_reg_type Qopt, double Qval, 
    			  cov_reg_type Sopt, double Sval,cov_reg_type Aopt, double Aval) {
    		   this.Ropt = Ropt;
    		   this.Rval = Rval;
    		   this.Qopt = Qopt;
    		   this.Qval = Qval;
    		   this.Sopt = Sopt;
    		   this.Sval = Sval;
    		   this.Aopt = Aopt;
    		   this.Aval = Aval;
    	}
    	
    }
    
    /** type of covariance matrix.  */
    public enum cov_type     {COV_FULL   (0),  /**< Full covariance matrix.     */
                       COV_DIAG  (1),  /**< diagonal covariance matrix. */
                       COV_IID (2),
                       COV_ILLEGAL (3);   /**< iid covariance matrix.      */
    	
    	public final int cov_code;
    	
    	cov_type(int cov_code) {
    		this.cov_code = cov_code;
    	}
    } 
    
    public cov_type getCov_type(int cov_code) {
    	if (cov_code == 0) {
    		return cov_type.COV_FULL;
    	}
    	else if (cov_code == 1) {
    		return cov_type.COV_DIAG;
    	}
    	else if (cov_code == 2) {
    		return cov_type.COV_IID;
    	}
    	else {
    		MipavUtil.displayError("Illegal number = " + cov_code + " for cov_type");
    		return cov_type.COV_ILLEGAL;
    	}
    }
    
    /** regularization method. */                     
    public enum cov_reg_type {COV_REG_NONE (0),  /**< no regularization */
                       COV_REG_MIN (1),   /**< enforce a minimum eigenvalue of regval */
                       COV_REG_ADD (2),   /**< add a constant regval to the diagonal */
                       COV_REG_ILLEGAL (3);
    	public final int cov_reg_code;
    	
    	cov_reg_type(int cov_reg_code) {
    		this.cov_reg_code = cov_reg_code;
    	}
    }
    
    public cov_reg_type getCov_reg_type(int cov_reg_code) {
    	if (cov_reg_code == 0) {
    		return cov_reg_type.COV_REG_NONE;
    	}
    	else if (cov_reg_code == 1) {
    		return cov_reg_type.COV_REG_MIN;
    	}
    	else if (cov_reg_code == 2) {
    		return cov_reg_type.COV_REG_ADD;
    	}
    	else {
    		MipavUtil.displayError("Illegal number = " + cov_reg_code + " for cov_reg_type");
    		return cov_reg_type.COV_REG_ILLEGAL;
    	}
    }
    
    public class CovMatrix {
        public int n;    /**< dimension of the (square) covariance matrix. */
        public Mat mtx = new Mat();  /**< storage for the covariance matrix. 
									        full matrix is [n x n]; 
									        diagonal matrix is [n x 1]; 
									        iid matrix is [1 x 1]. */
    	public cov_type     covopt;  /**< type of covariance matrix */
    	public cov_reg_type regopt;  /**< type of regularization to be used */
    	public double regval;  /**< regularization value */
    	public CovMatrix() {
    		
    	}
    }
    
    /** Dynamic Texture class.
    This is the class for a standard dynamic texture.  It serves as the base class
    for the online version of the DT.  It includes functions for:
    1) estimating parameters;
    2) regularizing;
    3) synthesizing video;
    4) pre-processing video for usage with DT (for other classes).
    */
    public class Dytex
    {
    	// DT options  
    	public DytexOptions dtopt = new DytexOptions();   /**< options for the Dytex */
    	// DT parameters
    	  public Mat Ymean = new Mat();    /**< observation mean */
    	  public Mat A = new Mat();        /**< transition matrix */
    	  public Mat C = new Mat();        /**< observation matrix */
    	  public Mat mu0 = new Mat();      /**< initial state mean */
    	  public CovMatrix R = new CovMatrix();       /**< observation noise covariance */
    	  public CovMatrix Q = new CovMatrix();        /**< state noise covariance */
    	  public CovMatrix S0 = new CovMatrix();       /**< initial state covariance */
    	 
    	  // video parameters (for synthesizing)
    	  // set to 0,0 if unknown
    	public int vrows,  /**< for synthesis, number of rows in a frame (0 if unknown).  */
    	      vcols;  /**< for synthesis, number of columns in a frame (0 if unknown). */
    	public Dytex() {
    		
    	}
    }
    
    public class Mat {
    	 /*! includes several bit-fields:
        - the magic signature
        - continuity flag
        - depth
        - number of channels
    */
   public int flags;
   //! the array dimensionality, >= 2
   public int dims;
   //! the number of rows and columns or (-1, -1) when the array has more than 2 dimensions
   public int depth, rows, cols;
   public int size[];
   public int type;
   //! pointer to the data
   public byte data[];
   public byte byte2D[][];
   public double double2D[][];
   public Vector3d Vector3d2D[][];
   public byte byte3D[][][];
   public double double3D[][][];
   public Vector3d Vector3d3D[][][];

   //! pointer to the reference counter;
   // when array points to user-allocated data, the pointer is NULL
   public int refcount[];
    	public Mat() {
    		
    	}
    	
    	public void create(int rows, int cols, int type) {
    		int x, y;
    	    this.rows = rows;
    	    this.cols = cols;
    	    this.type = type;
    	    if (type == CV_8U) {
    	        byte2D = new byte[rows][cols];	
    	    }
    	    else if (type == CV_64F) {
    	        double2D = new double[rows][cols];	
    	    }
    	    else if (type == CV_64FC3) {
    	        Vector3d2D = new Vector3d[rows][cols];	
    	        for (x = 0; x < rows; x++) {
    	        	for (y = 0; y < cols; y++) {
    	        		Vector3d2D[x][y] = new Vector3d();
    	        	}
    	        }
    	    }
    	}
    	
    	public void create(int dims, int size[], int type) {
    		int x, y , z;
    		this.dims = dims;
    		this.size = size;
    		this.type = type;
    		if (dims == 2) {
    			this.rows = size[0];
    			this.cols = size[1];
    		}
    		else if (dims == 3) {
    			this.depth = size[0];
    			this.rows = size[1];
    			this.cols = size[2];
    		}
    		if (dims == 2) {
    			if (type == CV_8U) {
        	        byte2D = new byte[rows][cols];	
        	    }
        	    else if (type == CV_64F) {
        	        double2D = new double[rows][cols];	
        	    }
        	    else if (type == CV_64FC3) {
        	        Vector3d2D = new Vector3d[rows][cols];	
        	        for (x = 0; x < rows; x++) {
        	        	for (y = 0; y < cols; y++) {
        	        		Vector3d2D[x][y] = new Vector3d();
        	        	}
        	        }
        	    }	
    		} // if (dims == 2)
    		else if (dims == 3) {
    			if (type == CV_8U) {
        	        byte3D = new byte[depth][rows][cols];	
        	    }
        	    else if (type == CV_64F) {
        	        double3D = new double[depth][rows][cols];	
        	    }
        	    else if (type == CV_64FC3) {
        	        Vector3d3D = new Vector3d[depth][rows][cols];
        	        for (x = 0; x < depth; x++) {
	        	        for (y = 0; y < rows; y++) {
	        	        	for (z = 0; z < cols; z++) {
	        	        		Vector3d3D[x][y][z] = new Vector3d();
	        	        	}
	        	        }
        	        }
        	    }	
    		} // else if (dims == 3)
    	}
    }
    
    public class DytexMix {
    	public DytexOptions opt = new DytexOptions();
    	/*!
    	 * \brief DT components in the mixture.	 
    	 * \see Dytex
    	 */
    	Vector<Dytex> dt = new Vector<Dytex>();
    	/*!
    	 * \brief DT components priors.	 	 
    	 */
    	public Vector<Double> alpha = new Vector<Double>();
    	/*!
    	 * \brief Class of each training video.	 	 
    	 */
    	public Vector<Integer> classes = new Vector<Integer>();
    	public DytexMix() {
    		
    	}
    }
    
    public void read(DytexMix dtm) {
    	readHeader("DytexMix");
    	read(dtm.opt);
    	int K[] = new int[1];
    	read("K",K);
    	dtm.alpha.clear();
    	for(int i=0;i<K[0];i++)
    	{
    		double temp[] = new double[1];
    		read("alpha",temp);
    		dtm.alpha.add(temp[0]);
    	}

    	dtm.dt.clear();
    	for(int i=0;i<K[0];i++)
    	{
    		Dytex tmpd = new Dytex();
    		read(tmpd);
    		dtm.dt.add(tmpd);
    	}
    	dtm.classes.clear();
    	read("classes",dtm.classes);
    }
    
    public void read(String name,Vector<Integer> vec)
    {
    	readHeader(name);		
    	int len;
    	try {
	    	len = getInt(endian);
	
	    	for(int i=0;i<len;i++)
	    	{
	    		int temp;
	    		temp = getInt(endian);
	    		vec.add(temp);
	    	}
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    }
    
    public void read(Dytex dt)
    {
    	readHeader("Dytex");
    	read(dt.dtopt); 
    	read("Ymean",dt.Ymean);
    	read("A",dt.A);
    	read("C",dt.C);
    	read("mu0",dt.mu0);
    	read(dt.R);
    	read(dt.Q);
    	read(dt.S0);
    	int vrows[] = new int[1];
    	read("vrows",vrows);
    	dt.vrows = vrows[0];
    	int vcols[] = new int[1];
    	read("vrows",vcols);
    	dt.vcols = vcols[0];
    }
    
    public void read(CovMatrix cm)
    {
    	readHeader("CovMatrix");
    	int n[] = new int[1];
    	read("n",n);
    	cm.n = n[0];
    	byte temp[] = new byte[1];
    	read("covopt",temp);
    	cm.covopt= getCov_type(temp[0]);
    	
    	read("regopt",temp);
    	cm.regopt= getCov_reg_type(temp[0]);

    	double regval[] = new double[1];
    	read("regval",regval);
    	cm.regval = regval[0];
    	read("mtx",cm.mtx);
    }
    
    public void read(String name,double val[])
    {
    	readHeader(name);
    	try {
    	    val[0] = getDouble(endian);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    }
    
    
    public void read(String name,Mat mtx)
    {
    	boolean isempty[] = new boolean[1];

    	Point3i dims = new Point3i(1,1,1);
    	readHeader(name);		
    	read("isempty",isempty);
    	if(isempty[0]) //empty
    	{
    		return;
    	}

    	int type=0;	
    	int els=0;
    	try {
	    	type = getInt(endian);
	
	    	read(dims);
	    	els = getInt(endian);
	    	
	    	int sz[]={dims.x,dims.y,dims.z};
	    	if(dims.x==1)
	    		mtx.create(dims.y,dims.z,type);
	    	else
	    		mtx.create(3,sz,type);
	    		
	
	    	//reading data
	    	double tmpD;
	    	byte tmpU;
	    	Vector3d tmpDV = new Vector3d();
	    	if(dims.x==1)
	    	{
	    		for(int i=0;i<dims.y;i++)
	    		{
	    			for(int j=0;j<dims.z;j++)
	    			{
	    				switch(type)
	    				{
	    				case CV_64F:								
	    					tmpD = getDouble(endian);				
	    					mtx.double2D[i][j] =tmpD;
	    					break;
	    				case CV_8U:								
	    					tmpU = raFile.readByte();				
	    					mtx.byte2D[i][j]=tmpU;
	    					break;
	    				case CV_64FC3:
	    					tmpD = getDouble(endian);
	    					tmpDV.x = tmpD;
	    					tmpD = getDouble(endian);
	    					tmpDV.y = tmpD;
	    					tmpD = getDouble(endian);
	    					tmpDV.z = tmpD;
	    					mtx.Vector3d2D[i][j] = tmpDV;
	    					break;
	    				default:
	    					MipavUtil.displayError("type not handled yet");
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
	    						tmpD = getDouble(endian);				
	    						mtx.double3D[i][j][k] = tmpD;
	    						break;
	    					case CV_8U:								
	    						tmpU = raFile.readByte();				
	    						mtx.byte3D[i][j][k]=tmpU;
	    						break;
	
	    					case CV_64FC3:
	    						tmpD = getDouble(endian);
	        					tmpDV.x = tmpD;
	        					tmpD = getDouble(endian);
	        					tmpDV.y = tmpD;
	        					tmpD = getDouble(endian);
	        					tmpDV.z = tmpD;
	        					mtx.Vector3d3D[i][j][k] = tmpDV;
	    						break;
	    					default:
	    						MipavUtil.displayError("type not handled yet");
	    					}
	    				}
	    			}
	    		}
	    	}
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}

    }
    
    public void read(Point3i p)
    {
    	readHeader("Point3i");
    	int x[] = new int[1];
    	int y[] = new int[1];
    	int z[] = new int[1];
    	read("x",x);
    	p.x = x[0];
    	read("y",y);
    	p.y = y[0];
    	read("z",z);
    	p.z = z[0];
    }
    
    public void read(String name,boolean val[])
    {
    	byte b = 0;
    	readHeader(name);
    	try {
    	    b = raFile.readByte();
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    	if (b == 0) {
    		val[0] = false;
    	}
    	else {
    		val[0] = true;
    	}
    	return;
    }
    
    public void read(DytexOptions opt) {
    	readHeader("DytexOptions");
    	read("n",opt.n);
    	read("m",opt.m);
    	byte temp[] = new byte[1];
    	read("Ropt",temp);
    	opt.Ropt= getCov_type(temp[0]);
    	read("Sopt",temp);
    	opt.Sopt=getCov_type(temp[0]);
    	read("Yopt",temp);
    	opt.Yopt= getYmean_type(temp[0]);
    }
    
    public void read(String name,int val[])
    {
    	readHeader(name);
    	try {
    	    val[0] = getInt(endian);	
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    }
    
    public void read(String name, byte val[])
    {
    	readHeader(name);
    	try {
    	    val[0] = raFile.readByte();	
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}

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
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    	// The original source code skips the delimiter,
    	// but this code reads the delimiter
    	// so there is no need to skip one character
    	for (i = 0; i < Math.min(99, fileLength); i++) {
    		try {
    	        b = raFile.readByte();
    		}
    		catch (IOException e) {
        		MipavUtil.displayError(e + " ");
        		System.exit(-1);	
        	}
    		if (b != 0) {
    			tempB[i] = b;
    		}
    		else {
    			break;
    		}
    	}
    	tempB[i] = 0;
    	tstr = new String(tempB,0,i);
    	if ((tstr == null) || (tstr.length() == 0)) {
    		MipavUtil.displayError("No header string found");
    		System.exit(-1);
    	}
    	else if (!str.equals(tstr)) {
    		MipavUtil.displayError("Header string = " + tstr + " instead of the required " + str);
    		System.exit(-1);
    	}
    	
    	//match version	
    	for (i = 0; i < 100; i++) {
    		tempB[i] = 0;
    	}
    	for (i = 0; i < Math.min(99, fileLength); i++) {
    		try {
    	        b = raFile.readByte();
    		}
    		catch (IOException e) {
        		MipavUtil.displayError(e + " ");
        		System.exit(-1);	
        	}
    		if (b != 0) {
    			tempB[i] = b;
    		}
    		else {
    			break;
    		}
    	}
    	tempB[i] = 0;
    	tstr = new String(tempB,0,i);
    	if ((tstr == null) || (tstr.length() == 0)) {
    		MipavUtil.displayError("No version string found");
    		System.exit(-1);
    	}
    	else if (!tstr.equals("1.0")) {
    		MipavUtil.displayError("Version string = " + tstr + " instead of the required 1.0");
    		System.exit(-1);
    	}
    	
    	//read/skip size of the type
    	int size;
    	try {
    	    size = getInt(endian);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		System.exit(-1);	
    	}
    	//System.out.println("size = " + size);
    }
    
    /**
     * Reads four signed bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }
    
    /**
     * Reads eight unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the double read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final double getDouble(final boolean bigEndian) throws IOException {
        raFile.readFully(byteDoubleBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteDoubleBuffer[0] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[1] & 0xffL) << 48) | ( (byteDoubleBuffer[2] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[3] & 0xffL) << 32) | ( (byteDoubleBuffer[4] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[5] & 0xffL) << 16) | ( (byteDoubleBuffer[6] & 0xffL) << 8) | (byteDoubleBuffer[7] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = ( ( (byteDoubleBuffer[7] & 0xffL) << 56)
                    | ( (byteDoubleBuffer[6] & 0xffL) << 48) | ( (byteDoubleBuffer[5] & 0xffL) << 40)
                    | ( (byteDoubleBuffer[4] & 0xffL) << 32) | ( (byteDoubleBuffer[3] & 0xffL) << 24)
                    | ( (byteDoubleBuffer[2] & 0xffL) << 16) | ( (byteDoubleBuffer[1] & 0xffL) << 8) | (byteDoubleBuffer[0] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        }
    }
}