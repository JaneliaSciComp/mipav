package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;
import java.io.IOException;

/**
 * The java code is ported from C++ code downloaded from http://coewww.rutgers.edu/riul/research/code.html.
 * The relevant web page section says:
 * Edge Detection and Image SegmentatiON (EDISON) System

C++ code, can be used through a graphical interface or command line.
The system is described in Synergism in low level vision.
For comments, please contact Bogdan Georgescu or Chris M. Christoudias.

The EDISON system contains the image segmentation/edge preserving filtering algorithm described in the
paper Mean shift: A robust approach toward feature space analysis and the edge detection algorithm
 described in the paper Edge detection with embedded confidence.

* This code was ported by William Gandler.
 * 
 * References

-------------------------------------------------------------------------------------------------

[1] D. Comanicu, P. Meer: "Mean shift: A robust approach toward feature space analysis".
    IEEE Trans. Pattern Anal. Machine Intell., May 2002.

[2] P. Meer, B. Georgescu: "Edge detection with embedded confidence". IEEE Trans. Pattern Anal.
    Machine Intell., 28, 2001.

[3] C. Christoudias, B. Georgescu, P. Meer: "Synergism in low level vision". 16th International
    Conference of Pattern Recognition, Track 1 - Computer Vision and Robotics, Quebec City,
    Canada, August 2001.
 */

public class AlgorithmEmbeddedConfidenceEdgeDetection extends AlgorithmBase {
	private static final double RED_WEIGHT = 0.299;
	private static final double  GREEN_WEIGHT = 0.587;
	private static final double BLUE_WEIGHT = 0.114;
	private static final int MAX_FILTS = 31;
	private static final int MAX_CUSTT = 30;
	private static final int NO_ANGLES = 361;



	private int x_;
	private int y_;
	private boolean havePerm_;
	private double smofil_[] = new double[MAX_FILTS];
	private double diffil_[] = new double[MAX_FILTS];
	private double wdx_[] = new double[MAX_FILTS*MAX_FILTS];
	private double wdy_[] = new double[MAX_FILTS*MAX_FILTS];
	private double mN_[][] = new double[MAX_FILTS][MAX_FILTS];
	private double mQ_[][] = new double[MAX_FILTS][MAX_FILTS];
	private double lookTable_[][] = new double[NO_ANGLES][];

	private int WL_;
	private int WW_;
	private int nhcust_;
	private int nlcust_;  
	private float tcustx_[];
	private float tcusty_[];



    private int filtDim;
	
	public AlgorithmEmbeddedConfidenceEdgeDetection(ModelImage destImage, ModelImage srcImage, int filtDim) {
		super(destImage, srcImage);
		this.filtDim = filtDim;
	}
	
	public void runAlgorithm() {
		int nDims;
		int xDim;
		int yDim;
		int sliceSize;
		int zDim;
		int tDim;
		float rgb[] = null;
		int z;
		int t;
		int i;
		int cf;
		double imageMin;
		double imageMax;
		double buffer[] = null;
		boolean rescale;
		BgImage cim;
		double a;
		double b;
		short sbuf[];
		float confMap[];
		float rank[];
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(),"Embedded Confidence Edge Detection ...");
        
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        if (srcImage.isColorImage()) {
        	cf = 4;
        }
        else {
        	cf = 1;
        }
        imageMin = srcImage.getMin();
        imageMax = srcImage.getMax();
        if (srcImage.isColorImage()) {
            sbuf = new short[3*sliceSize];
        }
        else {
        	sbuf = new short[sliceSize];
        }
        
        if ((imageMin < 0) || (imageMax > 255)) {
            rescale = true;
            buffer = new double[cf*sliceSize];
        }
        else {
        	rescale = false;
        }
        confMap = new float[sliceSize];
        rank = new float[sliceSize];
        
        // a*imageMin + b = 0
		// a*imageMax + b = 255
		a = 255.0/(imageMax - imageMin);
		b = -a*imageMin;
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        	   if (rescale) {
        		   try {
        			   srcImage.exportData(cf*(t*zDim + z)*sliceSize, cf*sliceSize, buffer);
        		   }
        		   catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
       			   }
        		  
        		   if (srcImage.isColorImage()) {
        		       for (i = 0; i < sliceSize; i++) {
        		           sbuf[3*i] = (short)Math.round(a*buffer[4*i+1] + b);
        		           sbuf[3*i+1] = (short)Math.round(a*buffer[4*i+2] + b);
        		           sbuf[3*i+2] = (short)Math.round(a*buffer[4*i+3] + b);
        		       }
        		   } // if (srcImage.isColorImage())
        		   else {
        			   for (i = 0; i < sliceSize; i++) {
        				   sbuf[i] = (short)Math.round(a*buffer[i] + b);
        			   }
        		   }
        	   } // if (rescale)
        	   else {
        		   try {
        			   srcImage.exportData(cf*(t*zDim + z)*sliceSize, cf*sliceSize, sbuf);
        		   }
        		   catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
       			   }      
        	   }
        	   
        	   cim = new BgImage(sbuf, xDim, yDim, srcImage.isColorImage());
        	   bgEdgeDetect(filtDim);
        	   computeEdgeInfo(cim, confMap, rank);
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        
	}
	
	private void bgEdgeDetect(int filtDim)
	{
	   havePerm_ = false;
	   WL_ = filtDim;
	   WW_ = 2*WL_+1;
	   nhcust_ = 0;
	   nlcust_ = 0;
	   tcustx_ = new float[MAX_CUSTT];
	   tcusty_ = new float[MAX_CUSTT];
	   createFilters();
	   createLookTable();
	}
	
	private void createFilters()
	{
	   int i,j;
	   double w;
	   for (i=-WL_; i<=WL_; i++)
	   {
	      w = Math.pow(2,(-2*WL_))*factorial(2*WL_)/(factorial(WL_-i)*factorial(WL_+i));
	      smofil_[i+WL_] = w;
	      diffil_[i+WL_] = (2*i*w)/WL_;
	   }
	   for (j=0; j<WW_; j++)
	   {
	      for (i=0; i<WW_; i++)
	      {
	         wdy_[j+i*WW_] = wdx_[i+j*WW_] = smofil_[j]*diffil_[i];
	      }
	   }
	   
	   double norms = 0;
	   double normd = 0;
	   for (i=0; i<WW_; i++)
	   {
	      norms += smofil_[i]*smofil_[i];
	      normd += diffil_[i]*diffil_[i];
	   }
	   
	   for (j=0; j<WW_; j++)
	   {
	      for (i=0; i<WW_; i++)
	      {
	         mQ_[i][j] = (smofil_[j]*smofil_[i])/norms + (diffil_[j]*diffil_[i])/normd;
	         mN_[i][j] = (i==j) ? 1-mQ_[i][j] : -mQ_[i][j];
	      }
	   }
	}
	
	private double factorial(double num)
	{
	   if (num==0 || num==1)
	      return 1;
	   return (num * factorial(num - 1));
	}

	private void createLookTable()
	{
	   Preferences.debug("Creating angle lookup table\n", Preferences.DEBUG_ALGORITHM);
	   int i;
	   for (i=-180; i<=180; i++)
	   {
	      lookTable_[i+180] = new double[WW_*WW_];
	      generateMaskAngle(lookTable_[i+180], (double) i);
	   }
	}
	
	private void generateMaskAngle(double a[],double theta) {
		   int sflag;
		   int i,j,k;
		   double cval[] = new double[4];
		   double corner[][] = new double[2][4];
		   double sinv,cosv;
		   double intrs[][] = new double[2][4];
		   int scor[] = new int[4];
		   int nscor[] = new int[4];
		   int sind,rowind,colind;
		   double cordi[][] = new double[2][4];
		   int lsigind,corin;
		   int sigind[] = new int[4];
		   double diffin[] = new double[2];
		   double comcoor;

		   /*theta = theta*Math.PI/180.0;
		   sinv = Math.sin(theta);
		   cosv = Math.cos(theta);
		   
		   for (i=0; i<WW_*WW_; i++)
		      a[i]=0;
		   
		   for (i=WL_; i>=-WL_; i--)
		   {
		      for(j=-WL_; j<=WL_; j++)
		      {
		         corner[0][0] = j-0.5;
		         corner[0][1] = j+0.5;
		         corner[0][2] = j+0.5;
		         corner[0][3] = j-0.5;
		         
		         corner[1][0] = i+0.5;
		         corner[1][1] = i+0.5;
		         corner[1][2] = i-0.5;
		         corner[1][3] = i-0.5;
		         
		         cval[0] = -sinv*corner[0][0]+cosv*corner[1][0];
		         cval[1] = -sinv*corner[0][1]+cosv*corner[1][1];
		         cval[2] = -sinv*corner[0][2]+cosv*corner[1][2];
		         cval[3] = -sinv*corner[0][3]+cosv*corner[1][3];
		         
		         scor[0] = my_sign(cval[0]);
		         scor[1] = my_sign(cval[1]);
		         scor[2] = my_sign(cval[2]);
		         scor[3] = my_sign(cval[3]);
		         
		         sind = 0;
		         if (scor[0]!=0)
		            nscor[sind++] = scor[0];
		         if (scor[1]!=0)
		            nscor[sind++] = scor[1];
		         if (scor[2]!=0)
		            nscor[sind++] = scor[2];
		         if (scor[3]!=0)
		            nscor[sind++] = scor[3];
		         
		         sflag = 0;
		         for (k=1;k<sind;k++)
		         {
		            if (nscor[k]!=nscor[0])
		               sflag++;
		         }
		         
		         rowind = i+WL_;
		         colind = j+WL_;
		         
		         if (sflag==0)
		         {
		            if (nscor[0]==1)
		               a[colind+rowind*WW_] = 1.0;
		            else
		               a[colind+rowind*WW_] = 0.0;
		         }
		         
		         if (sflag!=0)
		         {
		            for (k=0; k<4; k++)
		               intrs[0][k] = intrs[1][k] = 0.0;
		            
		            if(scor[0]==0)
		            {
		               intrs[0][0] = corner[0][0];
		               intrs[1][0] = corner[1][0];
		            }
		            if (scor[0]*scor[1]<0)
		            {
		               intrs[0][0] = corner[1][0]*cosv/sinv;
		               intrs[1][0] = corner[1][0];
		            }
		            if (scor[1]==0)
		            {
		               intrs[0][1] = corner[0][1];
		               intrs[1][1] = corner[1][1];
		            }
		            if (scor[1]*scor[2]<0)
		            {
		               intrs[0][1] = corner[0][1];
		               intrs[1][1] = corner[0][1]*sinv/cosv;
		            }
		            if (scor[2]==0)
		            {
		               intrs[0][2] = corner[0][2];
		               intrs[1][2] = corner[1][2];
		            }
		            if (scor[2]*scor[3]<0)
		            {
		               intrs[0][2] = corner[1][2]*cosv/sinv;
		               intrs[1][2] = corner[1][2];
		            }
		            if (scor[3]==0)
		            {
		               intrs[0][3] = corner[0][3];
		               intrs[1][3] = corner[1][3];
		            }
		            if (scor[3]*scor[0]<0)
		            {
		               intrs[0][3] = corner[0][3];
		               intrs[1][3] = corner[0][3]*sinv/cosv;
		            }
		            
		            corin = 0;
		            if (fabs(intrs[0][0])>TOL_E || fabs(intrs[1][0])>TOL_E)
		            {
		               cordi[0][corin] = intrs[0][0];
		               cordi[1][corin++] = intrs[1][0];
		            }
		            if (fabs(intrs[0][1])>TOL_E || fabs(intrs[1][1])>TOL_E)
		            {
		               cordi[0][corin] = intrs[0][1];
		               cordi[1][corin++] = intrs[1][1];
		            }
		            if (fabs(intrs[0][2])>TOL_E || fabs(intrs[1][2])>TOL_E)
		            {
		               cordi[0][corin] = intrs[0][2];
		               cordi[1][corin++] = intrs[1][2];
		            }
		            if (fabs(intrs[0][3])>TOL_E || fabs(intrs[1][3])>TOL_E)
		            {
		               cordi[0][corin] = intrs[0][3];
		               cordi[1][corin++] = intrs[1][3];
		            }
		            
		            lsigind=0;
		            if (scor[0]>0)
		               sigind[lsigind++] = 0;
		            if (scor[1]>0)
		               sigind[lsigind++] = 1;
		            if (scor[2]>0)
		               sigind[lsigind++] = 2;
		            if (scor[3]>0)
		               sigind[lsigind++] = 3;
		            
		            if (lsigind==1)
		            {
		               a[colind+rowind*WW_] = 0.5*fabs(cordi[0][0]-cordi[0][1])*fabs(cordi[1][0]-cordi[1][1]);
		            }
		            if (lsigind==2)
		            {
		               diffin[0] = (int) fabs(cordi[0][0]-cordi[0][1]);
		               diffin[1] = (int) fabs(cordi[1][0]-cordi[1][1]);
		               if (diffin[0]==1)
		               {
		                  comcoor = corner[1][sigind[0]];
		                  a[colind+rowind*WW_] = 0.5*(fabs(comcoor-cordi[1][0])+fabs(comcoor-cordi[1][1]));
		               }
		               if (diffin[1]==1)
		               {
		                  comcoor = corner[0][sigind[0]];
		                  a[colind+rowind*WW_] = 0.5*(fabs(comcoor-cordi[0][0])+fabs(comcoor-cordi[0][1]));
		               }
		            }
		            if(lsigind==3)
		            {
		               a[colind+rowind*WW_] = 1.0-0.5*fabs(cordi[0][0]-cordi[0][1])*fabs(cordi[1][0]-cordi[1][1]);
		            }
		         }
		      }
		   }
		   
		   //A=A-mean(mean(A));
		   comcoor = 0;
		   for (i=0; i<WW_*WW_; i++)
		      comcoor += a[i];
		   comcoor /= WW_*WW_;
		   for (i=0; i<WW_*WW_; i++)
		      a[i] -= comcoor;
		   
		   //A=A/norm(A,'fro')
		   comcoor = 0;
		   for (i=0; i<WW_*WW_; i++)
		      comcoor += a[i]*a[i];
		   comcoor = Math.sqrt(comcoor);
		   for (i=0; i<WW_*WW_; i++)
		      a[i] /= comcoor;*/
		}
	
	private int my_sign(double val)
	{
	   /*if(val>TOL_E)
	      return 1;
	   if(val<-TOL_E)
	      return -1;*/
	   return 0;
	}



     
	
	//Computes confedence map and rank
	//Pre : cim is an image
	//Post: confidence map and rank has been computed for cim
//	      and stored into confMap and rank respectively
	private void computeEdgeInfo(BgImage cim, float confMap[], float rank[])
	{
	   x_ = cim.x_;
	   y_ = cim.y_;
	   Preferences.debug("Computing confidence map...\n", Preferences.DEBUG_ALGORITHM);   
	   float pGx[], pGy[], pTemp[];
	   BgImage tcim = new BgImage(x_, y_,false);
	   if (cim.colorIm_)
	   {
		   tcim.setImageFromRGB(cim.im_, x_, y_, false);
	   } else
	   {
		   tcim = cim;
	   }

	   pGx = new float[x_*y_];
	   pGy = new float[x_*y_];   
	   pTemp = new float[x_*y_];
	   
	   // compute gradient images
	   Preferences.debug("...smooth-differentiation filtering\n", Preferences.DEBUG_ALGORITHM);
	   gaussDiffFilter(tcim, pGx, pGy, pTemp);   

	   // compute confidences (subspace estimate)
	   Preferences.debug("...subspace estimate\n", Preferences.DEBUG_ALGORITHM);
	   /*SubspaceEstim(pTemp, pGx, pGy, confMap);

	   // compute edge strength from gradient image
	   Preferences.debug("...edge strengths\n", Preferences.DEBUG_ALGORITHM);
	   Strength(pGx, pGy, pTemp);
	   
	   // compute ranks of the strengths
	   Preferences.debug("...computing ranks\n", Preferences.DEBUG_ALGORITHM);
	   CompRanks(pTemp, rank);*/
	 
	   //de-allocate memory
	   pTemp = null;
	   pGy = null;
	   pGx = null;
	}
	
	private void gaussDiffFilter(BgImage cim, float grx[], float gry[], float rezIm[])
	{
	   
	   double sf[]; //smooth filter
	   double df[]; //diff filter
	   short im[];
	   
	   double tim[];
	   double sum = 0;
	   //double sum1 = 0;
	   int i, j, k;
	   
	   //create kernels
	   sf = smofil_;
	   df = diffil_;
	   
	   im = cim.im_;
	   tim = new double[x_*y_];
	   for (i=0; i<x_*y_; i++)
	   {
	      grx[i] = gry[i] = 0;
	      tim[i] = 0;
	      rezIm[i] = im[i];
	   }
	   
	   //filter image x
	   //smooth on y
	   for (i=0; i<x_; i++)
	   {
	      for (j=WL_; j<(y_-WL_); j++)
	      {
	         sum = 0;
	         for (k=-WL_; k<=WL_; k++)
	            sum += im[(j+k)*x_+i]*sf[k+WL_];
	         tim[j*x_+i] = sum;
	      }
	   }
	   //diff on x
	   for (j=0; j<y_; j++)
	   {
	      for (i=WL_; i<(x_-WL_); i++)
	      {
	         sum = 0;
	         for (k=-WL_; k<=WL_; k++)
	            sum += tim[j*x_+i+k]*df[k+WL_];
	         grx[j*x_+i] = (float) (sum);
	      }
	   }

	   //filter image y
	   for (i=0; i<x_*y_;i++)
	      tim[i] = 0;
	   im = cim.im_;
	   //smooth on x
	   for (j=0; j<y_; j++)
	   {
	      for (i=WL_; i<(x_-WL_); i++)
	      {
	         sum = 0;
	         for (k=-WL_; k<=WL_; k++)
	            sum += im[j*x_+i+k]*sf[k+WL_];
	         tim[j*x_+i] = sum;
	      }
	   }
	   //diff on y
	   for (i=0; i<x_; i++)
	   {
	      for (j=WL_; j<(y_-WL_); j++)
	      {
	         sum = 0;
	         for (k=-WL_; k<=WL_; k++)
	            sum += tim[(j+k)*x_+i]*df[k+WL_];
	         gry[j*x_+i] = (float) (sum);
	      }
	   }
	   tim = null;
	}


	
	private class BgImage
	{
		   int x_;
		   int y_;
		   // Originally unsigned char so store 0 to 255
		   short im_[];
		   boolean hasIm_;
		   boolean colorIm_; // false - bw image
		                  // true  - color RGB image
		   
		   public BgImage(int x,int y, boolean colorIm)
		   {
			   colorIm_ = colorIm;
			   if (colorIm_ == false)
			      im_ = new short[x*y];
			   else
			      im_ = new short[x*y*3];
			   x_ = x;
			   y_ = y;
			   hasIm_ = true;
			}

		   
		   public BgImage(short im[], int x,int y, boolean colorIm)
		   {
			  int i;
			  cleanData();

		      colorIm_ = colorIm;
		      if (colorIm_ == false)
		         im_ = new short[x*y];
		      else
		         im_ = new short[x*y*3];
		      x_ = x;
		      y_ = y;
		      hasIm_ = true;
		      for (i = 0; i < im.length; i++) {
		    	  im_[i] = im[i];
		      }
		   }
		   
		   public void cleanData()
		   {
			   if (hasIm_)
			   {
			      im_ = null;
			      x_ = y_ = 0;
			      hasIm_ = false;
			      colorIm_ = false;
			   }
			}
		   
		  public void setImageFromRGB(short im[], int x, int y, boolean colorIm)
		   {
		      privateResize(x, y, colorIm);

		      int i;
		     
		      if (colorIm_ == false)
		      {
		         for (i=0; i<x*y; i++)
		   	  {
		   		 im_[i] = (short) (im[3*i]*RED_WEIGHT + im[3*i+1]*GREEN_WEIGHT + im[3*i+2]*BLUE_WEIGHT);
		   	  }
		      }
		      else
		      {
		         for (i=0; i<x*y*3; i++)
		            im_[i] = im[i];
		      }
		   }

		  public void privateResize(int width, int height, boolean color)
		  {
		     if ((hasIm_ == false) || (width != x_) || (height != y_) || (color != colorIm_))
		     {
		        cleanData();
		        x_ = width;
		        y_ = height;
		        colorIm_ = color;
		        if (color == false)
		           im_ = new short[x_*y_];
		        else
		           im_ = new short[x_*y_*3];
		        hasIm_ = true;
		     }
		  }


	}
	
}