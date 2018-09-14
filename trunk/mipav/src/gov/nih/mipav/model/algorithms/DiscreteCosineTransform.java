package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;

/**
 * This is a port from FORTRAN to Java of:
 *     ALGORITHM 749, COLLECTED ALGORITHMS FROM ACM.
*      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
*      VOL. 21, NO. 4, December, 1995, P.  372--378.
*
* This file contains 4 files separated by lines of the form
*         C*** filename
*
* The filenames in this file are:
*
* driver.f             driverd.f            fct.f               
* fctd.f                                                        
*  driver.f and fct.f are float versions and driverd.f and fctd.f are double versions. 
* <p>
* From <a href="http://www.acm.org/publications/policies/softwarecrnotice/">the ACM website</a>:
* 
* <pre>
* ACM Software License Agreement
* 
*  All software, both binary and source published by the Association for Computing Machinery (hereafter, Software) is copyrighted by the Association (hereafter, ACM) and ownership of all right, title and interest in and to the Software remains with ACM. By using or copying the Software, User agrees to abide by the terms of this Agreement.
*  Noncommercial Use
* 
*  The ACM grants to you (hereafter, User) a royalty-free, nonexclusive right to execute, copy, modify and distribute both the binary and source code solely for academic, research and other similar noncommercial uses, subject to the following conditions:
* 
*  1. User acknowledges that the Software is still in the development stage and that it is being supplied &quot;as is,&quot; without any support services from ACM. Neither ACM nor the author makes any representations or warranties, express or implied, including, without limitation, any representations or warranties of the merchantability or fitness for any particular purpose, or that the application of the software, will not infringe on any patents or other proprietary rights of others.
*  2. ACM shall not be held liable for direct, indirect, incidental or consequential damages arising from any claim by User or any third party with respect to uses allowed under this Agreement, or from any use of the Software.
*  3. User agrees to fully indemnify and hold harmless ACM and/or the author(s) of the original work from and against any and all claims, demands, suits, losses, damages, costs and expenses arising out of the User's use of the Software, including, without limitation, arising out of the User's modification of the Software.
*  4. User may modify the Software and distribute that modified work to third parties provided that: (a) if posted separately, it clearly acknowledges that it contains material copyrighted by ACM (b) no charge is associated with such copies, (c) User agrees to notify ACM and the Author(s) of the distribution, and (d) User clearly notifies secondary users that such modified work is not the original Software.
*  5. User agrees that ACM, the authors of the original work and others may enjoy a royalty-free, non-exclusive license to use, copy, modify and redistribute these modifications to the Software made by the User and distributed to third parties as a derivative work under this agreement.
*  6. This agreement will terminate immediately upon User's breach of, or non-compliance with, any of its terms. User may be held liable for any copyright infringement or the infringement of any other proprietary rights in the Software that is caused or facilitated by the User's failure to abide by the terms of this agreement.
*  7. This agreement will be construed and enforced in accordance with the law of the state of New York applicable to contracts performed entirely within the State. The parties irrevocably consent to the exclusive jurisdiction of the state or federal courts located in the City of New York for all disputes concerning this agreement. 
* 
*  Commercial Use
* 
*  Any User wishing to make a commercial use of the Software must contact ACM at permissions@acm.org to arrange an appropriate license. Commercial use includes (1) integrating or incorporating all or part of the source code into a product for sale or license by, or on behalf of, User to third parties, or (2) distribution of the binary or source code to third parties for use with a commercial product sold or licensed by, or on behalf of, User.
* 
*  Revised 6/98
* </pre>
* 
* </p>
*/

public class DiscreteCosineTransform {

    // ~ Static fields/initializers
    // -----------------------

    private int transformLength;
    
    private final int randomData = 1;
    
    private final int sinusoidalData = 2;
    
    // testDataType must be either randomData or sinusoidalData
    private int testDataType;
    
    private boolean printData;
    
    private boolean doTest = false;

    public DiscreteCosineTransform(int transformLength, int testDataType, boolean printData) {
        this.transformLength = transformLength;
        this.testDataType = testDataType;
        this.printData = printData;
        doTest = true;
    }
    
    public void run() {
    	if (doTest) {
    		runTest();
    		return;
    	}
    }
    
    public void runTest() {
    	int i;
    	double F[];
    	double C[];
    	double FSAVE[];
    	double DCTSAV[];
    	int IX;
    	int IY;
    	int IZ;
    	if ((testDataType != randomData) && (testDataType != sinusoidalData)) {
    		MipavUtil.displayError("testDataType must be randomData or sinusoidalData");
    		return;	
    	}
    	
    	// Initialize seeds for random number generator
    	IX = 1234;
    	IY = 42;
    	IZ = 1000;
    	
    	// Initialize data and save it for later
    	F = new double[transformLength];
    	C = new double[transformLength];
    	FSAVE = new double[transformLength];
    	DCTSAV = new double[transformLength];
    	
    	for (i = 0; i < transformLength; i++) {
    	    if (testDataType == randomData) {
    	        F[i] = RANDOM(IX, IY, IZ);	
    	    }
    	    else if (testDataType == sinusoidalData) {
    	        F[i] = 10.0*Math.sin(2.0*Math.PI*i/(double)transformLength);	
    	    }
    	    FSAVE[i] = F[i];
    	} // for (i = 0; i < transformLength; i++)
    }
    
    public double RANDOM(int IX,int IY, int IZ) {
           double r;
           double rfloor;
           double ans;
    //     THIS IS THE WICHMANN & HILL RANDOM NUMBER GENERATOR
    //     ALGORITHM AS-183, APPLIED STATISTICS VOL 31 NO 2, PP 188-190
    
          IX = 171 * (IX % 177) - 2  * (IX /177);
          IY = 172 * (IY % 176) - 35 * (IY/176);
          IZ = 170 * (IZ % 178) - 63 * (IZ/178);
    
          if (IX < 0) IX = IX + 30269;
          if (IY < 0) IY = IY + 30307;
          if (IZ < 0) IZ = IZ + 30323;
    
          r = (double)IX/ 30269.0 + (double)IY / 30307.0 + (double)IZ / 30323.0;
          rfloor = Math.floor(r);
          ans = r - rfloor;
          return ans;
    }

}