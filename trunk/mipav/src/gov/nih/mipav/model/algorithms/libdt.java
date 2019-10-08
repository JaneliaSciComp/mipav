package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;
import java.time.format.DateTimeFormatter;  
import java.time.LocalDateTime; 

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

/*!
 * \brief
 * reads a DytexMix object from current location in file.
 * 
 * \param dtm
 * where to copy read DytexMix.
 * 
 * \see
 * Bufferer::write(const DytexMix &dtm) | Bufferer::read(Dytex &dt)
 */
/*void Bufferer::read(DytexMix &dtm)
{
	readHeader("DytexMix");
	read(dtm.opt);
	int K;
	read("K",K);
	
	dtm.alpha.clear();
	for(int i=0;i<K;i++)
	{
		double temp;
		read("alpha",temp);
		dtm.alpha.push_back(temp);
	}

	dtm.dt.clear();
	for(int i=0;i<K;i++)
	{
		Dytex tmpd;
		read(tmpd);
		dtm.dt.push_back(tmpd);
	}
	dtm.classes.clear();
	read("classes",dtm.classes);

}

/*!
 * \brief
 * reads a DytexOptions object from current location in file.
 * 
 * \param opt
 * where to copy read DytexOptions.
 * 
 * \see
 * Bufferer::write(const DytexOptions &opt)
 */
/**
void Bufferer::read(DytexOptions &opt)
{
	readHeader("DytexOptions");
	read("n",opt.n);
	read("m",opt.m);
	unsigned char temp;
	read("Ropt",temp);
	opt.Ropt=(CovMatrix::cov_type)temp;
	read("Sopt",temp);
	opt.Sopt=(CovMatrix::cov_type)temp;
	read("Yopt",temp);
	opt.Yopt=(DytexOptions::Ymean_type)temp;
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
    		return;
    	}
    	//load existing	dtm	
    	DytexMix dtm = new DytexMix();
    	read(dtm);
    	try {
    		raFile.close();
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		return;
    	}
    }
    
    /** Options for modeling the observation mean, Ymean.  */
	public enum Ymean_type {ZERO_YMEAN,    /**< assume observations are already zero-mean, i.e. Ymean=0. */
	                   NONZERO_YMEAN;  /**< model non-zero observation mean. */
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
    
    public class CovMatrix {
    	public cov_type     covopt;  /**< type of covariance matrix */
    	public CovMatrix() {
    		
    	}
    }
    
    public class DytexMix {
    	public DytexOptions opt = new DytexOptions();
    	public DytexMix() {
    		
    	}
    }
    
    public void read(DytexMix dtm) {
    	readHeader("DytexMix");
    	read(dtm.opt);
    }
    
    public void read(DytexOptions opt) {
    	readHeader("DytexOptions");
    	read("n",opt.n);
    	read("m",opt.m);
    	byte temp[] = new byte[1];
    	read("Ropt",temp);
    	opt.Ropt= getCov_type(temp[0]);
    }
    
    public void read(String name,int val[])
    {
    	readHeader(name);
    	try {
    	    val[0] = getInt(endian);	
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		return;	
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
    		return;	
    	}

    }
    
    public void readHeader(String str) {
    	int i;
    	long fileLength;
    	byte b;
    	String tstr;
    	// match header
    	byte tempB[] = new byte[100];
    	try { 
    	    fileLength = raFile.length();
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		return;	
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
        		return;	
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
    		return;
    	}
    	else if (!str.equals(tstr)) {
    		MipavUtil.displayError("Header string = " + tstr + " instead of the required " + str);
    		return;
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
        		return;	
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
    		return;
    	}
    	else if (!tstr.equals("1.0")) {
    		MipavUtil.displayError("Version string = " + tstr + " instead of the required 1.0");
    		return;
    	}
    	
    	//read/skip size of the type
    	int size;
    	try {
    	    size = getInt(endian);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError(e + " ");
    		return;	
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
}