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
	private static final double GREEN_WEIGHT = 0.587;
	private static final double BLUE_WEIGHT = 0.114;

	private static final int MAX_FILTS = 31;
	private static final int MAX_CUSTT = 30;
	private static final int NO_ANGLES = 361;
	private static final double TOL_E = 2.2e-8;
    private static final double ZERO_TRESH = 0.0000000001;
    private static final double HYST_LOW_CUT = 0.0;


    // default values for edge detection
    private static final double CONF_NMX = 0.5;
    private static final double RANK_NMX = 0.5;
    private static final double CONF_H = 0.96;
    private static final double RANK_H = 0.93;
    private static final double CONF_L = 0.91;
    private static final double RANK_L = 0.99;
    private static final int NMIN = 5;
    private static final int KERNEL_SIZE = 2;

    private static final int FC_ELLIPSE = 0;
    private static final int FC_VERT_LINE = 1;
    private static final int FC_HORIZ_LINE = 2;
    private static final int FC_LINE= 3;
    private static final int FC_SQUARE_BOX = 4;
    private static final int FC_CUSTOM = 5; 
	
	private static final int gNb[][]= new int[][]
		{
		   {1, 0},
		   {1, 1},
		   {1,-1},
		   {0, 1},
		   {0,-1},
		  {-1, 0},
		  {-1, 1},
		  {-1,-1}
		};
    private BgImage cbgImage_;
    private BgEdgeDetect cbgEdgeDetect_;

    // Window side is 2*kernelSize + 1
    // In the reference:
    // The 512 by 512 basket image was processed with a 7 by 7 gradient operator.
    // The 256 by 256 cameraman image was processed with a 5 by 5 gradient operator.
    // The 512 by 438 grater image was processed with a 7 by 7 gradient operator.
    // The 548 by 509 golf cart image was processed with a 7 by 7 gradient operator.
    private int kernelSize = KERNEL_SIZE;
    // nmxr, nmxc threshold for non-maxima-suppresion rank, confidence
    private double nmxr = RANK_NMX;
    private double nmxc = CONF_NMX;
    // rh, ch, threshold for hyst. high; rank, confidence
    private double rh = RANK_H;
    private double ch = CONF_H;
    // rl, cl, threshold for hyst. low; rank, confidence
    private double rl = RANK_L;
    private double cl = CONF_L;
    // nMin, min number of pixels on an edge
    private int nMin = NMIN;
    // nmxType, hystTypeHigh, hystTypeLow, type of nmx curve, hyst. high curve, hyst low curve
    //  in (FC_ELLIPSE, FC_VERT_LINE, FC_HORIZ_LINE, FC_LINE, FC_SQUARE_BOX, FC_CUSTOM)
    private int nmxType = FC_ELLIPSE;
    private int hystTypeHigh = FC_SQUARE_BOX;
    private int hystTypeLow = FC_ELLIPSE;
	
	public AlgorithmEmbeddedConfidenceEdgeDetection(ModelImage destImage, ModelImage srcImage, int kernelSize,
			double nmxr, double nmxc, double rh, double ch, double rl, double cl, int nMin,
			int nmxType, int hystTypeHigh, int hystTypeLow) {
		super(destImage, srcImage);
		this.kernelSize = kernelSize;
		this.nmxr = nmxr;
		this.nmxc = nmxc;
		this.rh = rh;
		this.ch = ch;
		this.rl = rl;
		this.cl = cl;
		this.nMin = nMin;
		this.nmxType = nmxType;
		this.hystTypeHigh = hystTypeHigh;
		this.hystTypeLow = hystTypeLow;
	}
	
	public void runAlgorithm() {
		int nDims;
		int xDim;
		int yDim;
		int sliceSize;
		int zDim;
		int tDim;
		int z;
		int t;
		int i;
        int cf;
		double imageMin;
		double imageMax;
		double buffer[] = null;
		boolean rescale;
		double a;
		double b;
		short sbuf[];
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
        sbuf = new short[sliceSize];
        
        if ((imageMin < 0) || (imageMax > 255)) {
            rescale = true;
        }
        else {
        	rescale = false;
        }
        if (rescale || srcImage.isColorImage()) {
        	buffer = new double[cf*sliceSize];	
        }
        
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
        		    	   sbuf[i] = (short)Math.round(a*(RED_WEIGHT*buffer[4*i+1] + GREEN_WEIGHT*buffer[4*i+2] 
        		    			   + BLUE_WEIGHT*buffer[4*i+3]) + b);
        		       }
        		   } // if (srcImage.isColorImage())
        		   else {
        			   for (i = 0; i < sliceSize; i++) {
        				   sbuf[i] = (short)Math.round(a*buffer[i] + b);
        			   }
        		   }
        	   } // if (rescale)
        	   else if (srcImage.isColorImage()) {
        		   try {
        			   srcImage.exportData(cf*(t*zDim + z)*sliceSize, cf*sliceSize, buffer);
        		   }
        		   catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
       			   }  
        		   for (i = 0; i < sliceSize; i++) {
        			   sbuf[i] = (short)Math.round(RED_WEIGHT*buffer[4*i+1] + GREEN_WEIGHT*buffer[4*i+2] 
    		    			   + BLUE_WEIGHT*buffer[4*i+3]);
    		       }
        	   } // else if (srcImage.isColorImage())
        	   else {
        		   try {
        			   srcImage.exportData((t*zDim + z)*sliceSize, sliceSize, sbuf);
        		   }
        		   catch(IOException e) {
	        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
	        			setCompleted(false);
	        			return;
       			   }      
        	   }
        	   
        	   cbgImage_ = new BgImage(sbuf, xDim, yDim, srcImage.isColorImage());
        	   cbgEdgeDetect_ = new BgEdgeDetect(kernelSize);
        	   BgEdgeList cbgEdgeList_  = new BgEdgeList();
        	   cbgEdgeDetect_.doEdgeDetect(cbgImage_, cbgEdgeList_);
  
        	   // get binary edge image
        	   BgImage tempImage = new BgImage(cbgImage_.x_, cbgImage_.y_, false);
        	   cbgEdgeList_.setBinImage(tempImage);
        	   int edgex[];
        	   int edgey[];
        	   int nEdgep[] = new int[1];
        	   edgex = new int[(cbgImage_.x_) * (cbgImage_.y_)];
        	   edgey = new int[(cbgImage_.x_) * (cbgImage_.y_)];
        	   cbgEdgeList_.getAllEdgePoints(edgex, edgey, nEdgep);
        	   byte edgeBuffer[] = new byte[sliceSize];
        	   for (i = 0; i < nEdgep[0]; i++) {
        		   edgeBuffer[edgex[i] + xDim*edgey[i]] = 1;
        	   }
        	   edgey = null;
        	   edgex = null;
        	   
        	   
        	   if (destImage != null) {
               	try {
               		destImage.importData((t*zDim+z)*sliceSize, edgeBuffer, false);
               	}
               	catch (IOException e) {
               		MipavUtil.displayError("IOException " + e + " on destImage.importData");
               		setCompleted(false);
               		return;
               	}
               }
               else {
               	try {
               		srcImage.importData((t*zDim+z)*sliceSize, edgeBuffer, false);
               	}
               	catch (IOException e) {
               		MipavUtil.displayError("IOException " + e + " on srcImage.importData");
               		setCompleted(false);
               		return;
               	}	
               }

        	  
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        
        if (destImage != null) {
			destImage.calcMinMax();
		}
		else {
			srcImage.calcMinMax();
		}
        
        setCompleted(true);
		return;
        
	}
		
	private double factorial(double num)
	{
	   if (num==0 || num==1)
	      return 1;
	   return (num * factorial(num - 1));
	}

	private int my_sign(double val)
	{
	   if(val>TOL_E)
	      return 1;
	   if(val<-TOL_E)
	      return -1;
	   return 0;
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

	}
	
	private class BgEdge
	{
		   int edge_[];
		   //double grad_[];
		   //boolean isGradSet_;
		   //boolean isMarkSet_;
		   //byte mark_[];
		   int nPoints_;
		   BgEdge next_;

		   public void setPoints(float points[], int npoints)
		   {
		      if (nPoints_>0) edge_ = null;
		      nPoints_ = npoints;
		      edge_=new int[npoints*2];

		      for (int i=0; i<2*npoints; i++)
		         edge_[i] = (int) points[i];
		   }

		}
	
	private class BgEdgeList

	{

	   int nEdges_;

	   BgEdge edgelist_;

	   BgEdge crtedge_;
	   
	   public void addEdge(float edge[], int nPoints)

	   {

	      BgEdge tedge;

	      tedge = new BgEdge();

	      tedge.setPoints(edge, nPoints);

	      if (nEdges_==0)

	      {

	         nEdges_ = 1;

	         edgelist_ = tedge;

	         crtedge_ = tedge;

	      }

	      else

	      {

	         nEdges_++;

	         crtedge_.next_ = tedge;

	         crtedge_ = tedge;

	      }

	   }
	   
	   public void setBinImage(BgImage image)

	   {

	      int i, j;

	      int ix, iy;

	      int x, y;

	      

	      x = image.x_;

	      y = image.y_;

	      short im[] =image.im_;

	      int imIndex = 0;

	      for (i=0; i<x; i++)

	      {

	         for (j=0;j<y;j++)

	         {

	            im[imIndex++] = 0;

	         }

	      }

	      

	      im = image.im_;

	      crtedge_=edgelist_;

	      for (i=0; i<nEdges_; i++)

	      {
	         int edge_Index = 0;

	         for (j=0; j<crtedge_.nPoints_; j++)

	         {

	            ix = crtedge_.edge_[edge_Index++];

	            iy = crtedge_.edge_[edge_Index++];

	            im[iy*x+ix] = 255;

	         }

	         crtedge_=crtedge_.next_;

	      }

	   }
	   
	  public void getAllEdgePoints(int x[], int y[], int n[])

	   {

	      int length;

	      int i,j;

	      BgEdge crtedge;

	      int edgep[];

	      

	      crtedge = edgelist_;

	      int nIndex = 0;
	      n[nIndex] = 0;

	      for (i=0; i<nEdges_; i++)

	      {

	         length = crtedge.nPoints_;

	         edgep = crtedge.edge_;

	         for (j=0; j<length; j++)

	         {

	            x[n[nIndex]] = edgep[2*j];

	            y[n[nIndex]] = edgep[2*j+1];

	            n[nIndex]++;

	         }

	         crtedge = crtedge.next_;

	      }

	   }
	}
	
	// main class, edge detection
	public class BgEdgeDetect {
		
	double smofil_[] = new double[MAX_FILTS];
	   double diffil_[] = new double[MAX_FILTS];
	   double wdx_[] = new double[MAX_FILTS*MAX_FILTS];
	   double wdy_[] = new double[MAX_FILTS*MAX_FILTS];
	   double mN_[][] = new double[MAX_FILTS][MAX_FILTS];
	   double mQ_[][] = new double[MAX_FILTS][MAX_FILTS];
	   double lookTable_[][] = new double[NO_ANGLES][];

	   int WW_;
	   int WL_;
	   float confTr_;
	   float rankTr_;

	   float custx_[];
	   float custy_[];
	   float tcustx_[];
	   float tcusty_[];
	   int ncust_;

	   float hcustx_[];
	   float hcusty_[];
	   int nhcust_;
	   float lcustx_[];
	   float lcusty_[];
	   int nlcust_;   

	   int x_;
	   int y_;
	   float permConf_[];
	   float permRank_[];
	   float permNmxRank_[];
	   float permNmxConf_[];
	   boolean havePerm_;
	   
	   float te_[];
	   float tm_[];
	   double low_;
	   float tc_[];
	   int tc_Index;
	   float tl_[];
	   int npt_;

	   float grx_[];
	   float gry_[];
	   float permGx_[];
	   float permGy_[]; 

	   public BgEdgeDetect(int filtDim)
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
	   
	// main function for edge detection
		// cim input image
	    // cel edge list (will be filled with pixels on edges)

		public void doEdgeDetect(BgImage cim, BgEdgeList cel)
		{
		x_ = cim.x_;
		y_ = cim.y_;
		Preferences.debug("Start edge detection...\n", Preferences.DEBUG_ALGORITHM);   
		permGx_ = new float[x_*y_];
		permGy_ = new float[x_*y_];
		permConf_ = new float[x_*y_];
		permRank_ = new float[x_*y_];
		permNmxRank_ = new float[x_*y_];
		permNmxConf_ = new float[x_*y_];
		havePerm_ = true;
		float tr[];
		float tc[];
		float tdh[];
		float tdl[];
		
		tr = new float[x_*y_];
		tc = new float[x_*y_];
		tdh = new float[x_*y_];
		tdl = new float[x_*y_];
		
		// compute gradient images
		Preferences.debug("...smooth-differentiation filtering\n", Preferences.DEBUG_ALGORITHM);
		gaussDiffFilter(cim, permGx_, permGy_, tr);   
		
		// compute confidences (subspace estimate)
		Preferences.debug("...subspace estimate\n", Preferences.DEBUG_ALGORITHM);
		subspaceEstim(tr, permGx_, permGy_, permConf_);
		
		// compute edge strength from gradient image
		Preferences.debug("...edge strengths\n", Preferences.DEBUG_ALGORITHM);
		strength(permGx_, permGy_, tr);
		
		// compute ranks of the strengths
		Preferences.debug("...computing ranks\n", Preferences.DEBUG_ALGORITHM);
		compRanks(tr, permRank_);
		
		// new nonmaxima supression
		Preferences.debug("...nonmaxima supression: ", Preferences.DEBUG_ALGORITHM);
		
		// select appropriate function
		//float (BgEdgeDetect::*fcomp)(float,float,float,float);
		//float (BgEdgeDetect::*feval)(float,float);
		switch(nmxType)
		{
		case FC_ELLIPSE:
		//fcomp = &BgEdgeDetect::EllipseComp;
		//feval = &BgEdgeDetect::EllipseEval;
		Preferences.debug("arc\n", Preferences.DEBUG_ALGORITHM);
		break;
		case FC_VERT_LINE:
		//fcomp = &BgEdgeDetect::VerticalLineComp;
		//feval = &BgEdgeDetect::VerticalLineEval;
		Preferences.debug("vertical line\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_HORIZ_LINE:
		//fcomp = &BgEdgeDetect::HorizontalLineComp;
		//feval = &BgEdgeDetect::HorizontalLineEval;
		Preferences.debug("horizontal line\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_SQUARE_BOX:
		//fcomp = &BgEdgeDetect::SquareComp;
		//feval = &BgEdgeDetect::SquareEval;
		Preferences.debug("box\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_LINE:
		//fcomp = &BgEdgeDetect::LineComp;
		//feval = &BgEdgeDetect::LineEval;		
		Preferences.debug("line\n", Preferences.DEBUG_ALGORITHM);
		break;
		case FC_CUSTOM:
		custx_ = hcustx_;
		custy_ = hcusty_;
		ncust_ = nhcust_;
		//fcomp = &BgEdgeDetect::CustomRegionComp;
		//feval = &BgEdgeDetect::CustomRegionEval;
		Preferences.debug("custom", Preferences.DEBUG_ALGORITHM);
		break;
		default:
		Preferences.debug("Type not known\n", Preferences.DEBUG_ALGORITHM);
		return;
		}
		
		confTr_ = (float) nmxc;
		rankTr_ = (float) nmxr;
		//newNonMaxSupress(permRank_, permConf_, permGx_, permGy_, permNmxRank_, permNmxConf_, fcomp);
		newNonMaxSupress(permRank_, permConf_, permGx_, permGy_, permNmxRank_, permNmxConf_, nmxType);
		// new hysteresis thresholding
		Preferences.debug("...hysteresis thresholding, high: ", Preferences.DEBUG_ALGORITHM);
		
		// select function, high curve
		switch(hystTypeHigh)
		{
		case FC_ELLIPSE:
		//fcomp = &BgEdgeDetect::EllipseComp;
		//feval = &BgEdgeDetect::EllipseEval;
		Preferences.debug("arc", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_VERT_LINE:
		//fcomp = &BgEdgeDetect::VerticalLineComp;
		//feval = &BgEdgeDetect::VerticalLineEval;
		Preferences.debug("vertical line", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_HORIZ_LINE:
		//fcomp = &BgEdgeDetect::HorizontalLineComp;
		//feval = &BgEdgeDetect::HorizontalLineEval;
		Preferences.debug("horizontal line", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_SQUARE_BOX:
		//fcomp = &BgEdgeDetect::SquareComp;
		//feval = &BgEdgeDetect::SquareEval;
		Preferences.debug("box", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_LINE:
		//fcomp = &BgEdgeDetect::LineComp;
		//feval = &BgEdgeDetect::LineEval;		
		Preferences.debug("line", Preferences.DEBUG_ALGORITHM);  		
		break;
		case FC_CUSTOM:
		custx_ = hcustx_;
		custy_ = hcusty_;
		ncust_ = nhcust_;
		//fcomp = &BgEdgeDetect::CustomRegionComp;
		//feval = &BgEdgeDetect::CustomRegionEval;
		Preferences.debug("custom", Preferences.DEBUG_ALGORITHM);
		break;
		}  
		
		confTr_ = (float) ch;
		rankTr_ = (float) rh;
		//StrConfEstim(permNmxRank_, permNmxConf_, tdh, feval);
		strConfEstim(permNmxRank_, permNmxConf_, tdh, hystTypeHigh);
		
		Preferences.debug("  low: ", Preferences.DEBUG_ALGORITHM);
		
		// select function, low curve
		switch(hystTypeLow)
		{
		case FC_ELLIPSE:
		//fcomp = &BgEdgeDetect::EllipseComp;
		//feval = &BgEdgeDetect::EllipseEval;
		Preferences.debug("arc\n", Preferences.DEBUG_ALGORITHM);
		break;
		case FC_VERT_LINE:
		//fcomp = &BgEdgeDetect::VerticalLineComp;
		//feval = &BgEdgeDetect::VerticalLineEval;
		Preferences.debug("vertical line\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_HORIZ_LINE:
		//fcomp = &BgEdgeDetect::HorizontalLineComp;
		//feval = &BgEdgeDetect::HorizontalLineEval;
		Preferences.debug("horizontal line\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_SQUARE_BOX:
		//fcomp = &BgEdgeDetect::SquareComp;
		//feval = &BgEdgeDetect::SquareEval;
		Preferences.debug("box\n", Preferences.DEBUG_ALGORITHM);		
		break;
		case FC_LINE:
		//fcomp = &BgEdgeDetect::LineComp;
		//feval = &BgEdgeDetect::LineEval;		
		Preferences.debug("line\n", Preferences.DEBUG_ALGORITHM);  		
		break;
		case FC_CUSTOM:
		custx_ = lcustx_;
		custy_ = lcusty_;
		ncust_ = nlcust_;
		//fcomp = &BgEdgeDetect::CustomRegionComp;
		//feval = &BgEdgeDetect::CustomRegionEval;
		Preferences.debug("custom\n", Preferences.DEBUG_ALGORITHM);
		break;  		
		} 
		confTr_ = (float) cl;
		rankTr_ = (float) rl;
		
		//strConfEstim(permNmxRank_, permNmxConf_, tdl, feval);
		strConfEstim(permNmxRank_, permNmxConf_, tdl, hystTypeLow);
		
		grx_ = permGx_;
		gry_ = permGy_;
		
		newHysteresisTr(tdh, tdl, cel, nMin, tr, tc);
		
		Preferences.debug("Done edge detection.\n", Preferences.DEBUG_ALGORITHM);
		
		tdl = null;
		tdh = null;
		tr = null;
		tc = null;
	}
		
		private float ellipseEval(float x, float y)
		{
		   return ((x*x)/(rankTr_*rankTr_)+(y*y)/(confTr_*confTr_)-1);
		}


		private float verticalLineEval(float x, float y)
		{
		   return (x-rankTr_);
		}
		
		private float horizontalLineEval(float x, float y)
		{
		   return(y-confTr_);
		}
		
		private float lineEval(float x, float y)
		{
		   return (confTr_*x+rankTr_*y-confTr_*rankTr_);
		}

		private float squareEval(float x, float y)
		{
		   if ((x/rankTr_)>(y/confTr_))
		      return(x-rankTr_);
		   else
		      return(y-confTr_);
		}
		
		private float customRegionEval(float r,float c)
		{
		   //evaluate user region function
		   //returns -1 if inside +1 if outside
		   
		   if ((r+c)<=ZERO_TRESH)
		      return -1;
		   int i;
		   int crossings=0;
		   float x;
		   
		   //shift to origin
		   for (i=0; i<ncust_; i++)
		   {
		      tcustx_[i]=custx_[i]-r;
		      tcusty_[i]=custy_[i]-c;
		   }
		   
		   for (i=0; i<(ncust_-1); i++)
		   {
		      if ( (tcusty_[i]  >0 && tcusty_[i+1]<=0) ||
		           (tcusty_[i+1]>0 && tcusty_[i]  <=0) )
		      {
		         x = (tcustx_[i]*tcusty_[i+1]-tcustx_[i+1]*tcusty_[i])/(tcusty_[i+1]-tcusty_[i]);
		         if (x>0)
		            crossings++;
		      }	   
		   }		
		   
		   if ((crossings % 2) ==1)
		      return -1;
		   else
		      return 1;
		}



		private float fcomp(int fcompType, float x0, float y0, float x, float y) {
			switch (fcompType) {
			case FC_ELLIPSE:
				return ellipseComp(x0, y0, x, y);
			case FC_VERT_LINE:
				return verticalLineComp(x0,y0, x, y);
			case FC_HORIZ_LINE:
				return horizontalLineComp(x0, y0, x, y);
			case FC_LINE:
				return lineComp(x0, y0, x, y);
			case FC_SQUARE_BOX:
				return squareComp(x0, y0, x, y);
			case FC_CUSTOM:
				return customRegionComp(x0, y0, x, y);
			default:
				return -1.0f;
			}
		}
		
		private float ellipseComp(float x0, float y0, float x, float y)
		{
		//   return (EllipseEval(x,y)-EllipseEval(x0,y0));
		   return ((x*x-x0*x0)/(rankTr_*rankTr_)+(y*y-y0*y0)/(confTr_*confTr_));
		}
		
		private float verticalLineComp(float x0, float y0, float x, float y)
		{
		//   return (VerticalLineEval(x,y)-VerticalLineEval(x0,y0));
		   return (x-x0);

		}
		
		private float horizontalLineComp(float x0, float y0, float x, float y)
		{
		//   return (HorizontalLineEval(x,y)-HorizontalLineEval(x0,y0));
		   return (y-y0);
		}

		private float lineComp(float x0, float y0, float x, float y)
		{
		//   return (LineEval(x,y)-LineEval(x0,y0));
		   return (confTr_*(x-x0)+rankTr_*(y-y0));
		}

		private float squareComp(float x0, float y0, float x, float y)
		{
		//   return(SquareEval(x,y)-SquareEval(x0,y0));
		   float tret;
		   tret = ((x/rankTr_)>(y/confTr_)) ? x-rankTr_ : y-confTr_;
		   tret -= ((x0/rankTr_)>(y0/confTr_)) ? x0-rankTr_ : y0-confTr_;
		   return tret;
		}
		
		private float customRegionComp(float r0, float c0, float r, float c)
		{
		   return 0.0f;
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

			   theta = theta*Math.PI/180.0;
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
			            if (Math.abs(intrs[0][0])>TOL_E || Math.abs(intrs[1][0])>TOL_E)
			            {
			               cordi[0][corin] = intrs[0][0];
			               cordi[1][corin++] = intrs[1][0];
			            }
			            if (Math.abs(intrs[0][1])>TOL_E || Math.abs(intrs[1][1])>TOL_E)
			            {
			               cordi[0][corin] = intrs[0][1];
			               cordi[1][corin++] = intrs[1][1];
			            }
			            if (Math.abs(intrs[0][2])>TOL_E || Math.abs(intrs[1][2])>TOL_E)
			            {
			               cordi[0][corin] = intrs[0][2];
			               cordi[1][corin++] = intrs[1][2];
			            }
			            if (Math.abs(intrs[0][3])>TOL_E || Math.abs(intrs[1][3])>TOL_E)
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
			               a[colind+rowind*WW_] = 0.5*Math.abs(cordi[0][0]-cordi[0][1])*Math.abs(cordi[1][0]-cordi[1][1]);
			            }
			            if (lsigind==2)
			            {
			               diffin[0] = (int) Math.abs(cordi[0][0]-cordi[0][1]);
			               diffin[1] = (int) Math.abs(cordi[1][0]-cordi[1][1]);
			               if (diffin[0]==1)
			               {
			                  comcoor = corner[1][sigind[0]];
			                  a[colind+rowind*WW_] = 0.5*(Math.abs(comcoor-cordi[1][0])+Math.abs(comcoor-cordi[1][1]));
			               }
			               if (diffin[1]==1)
			               {
			                  comcoor = corner[0][sigind[0]];
			                  a[colind+rowind*WW_] = 0.5*(Math.abs(comcoor-cordi[0][0])+Math.abs(comcoor-cordi[0][1]));
			               }
			            }
			            if(lsigind==3)
			            {
			               a[colind+rowind*WW_] = 1.0-0.5*Math.abs(cordi[0][0]-cordi[0][1])*Math.abs(cordi[1][0]-cordi[1][1]);
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
			      a[i] /= comcoor;
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
		
		private void strength(float grx[], float gry[], float strength[])
		{
		   int i,j;
		   float itgx[];
		   float itgy[];
		   float its[];
		   double val;
		   int itgxIndex = 0;
		   int itgyIndex = 0;
		   int itsIndex = 0;
		   itgx = grx;
		   itgy = gry;
		   its = strength;
		   for (j=0; j<y_; j++)
		      for(i=0; i<x_; i++)
		      {
		         val = Math.sqrt((double) (itgx[itgxIndex]*itgx[itgxIndex++])+((double) (itgy[itgyIndex]*itgy[itgyIndex++])));
		         its[itsIndex++]=(float) (val);
		      }
		}
		
		private void newNonMaxSupress(float rank[], float conf[], float grx[], float gry[], float nmxRank[], float nmxConf[],
	            int fcompType)
	{
		int i,j;
		float itr[];
		float itc[];
		float itgx[];
		float itgy[];
		float itnmxr[];
		float itnmxc[];
		float alpha,r1,c1,r2,c2,lambda;
		itr = rank;
		itc = conf;
		itgx = grx;
		itgy = gry;
		itnmxr = nmxRank;
		itnmxc = nmxConf;
		int itrIndex = 0;
		int itcIndex = 0;
		int itgxIndex = 0;
		int itgyIndex = 0;
		int itnmxrIndex = 0;
		int itnmxcIndex = 0;
		
		for (i=0; i<x_*y_; i++)
		{
		itnmxr[i] = itnmxc[i] = 0;
		}
		for(i=0; i<x_; i++)
		{
		itr[i] = itc[i] = 0;
		itr[(y_-1)*x_+i] = itc[(y_-1)*x_+i] = 0;
		}
		for(j=0; j<y_; j++)
		{
		itr[j*x_] = itc[j*x_] = 0;
		itr[j*x_+x_-1] = itc[j*x_+x_-1] = 0;
		}
		
		for (j=0; j<y_; j++)
		{
		for (i=0; i<x_; i++, itrIndex++, itcIndex++, itgxIndex++, itgyIndex++, itnmxrIndex++, itnmxcIndex++)
		{
		if (itr[itrIndex]>0 && itc[itcIndex]>0)
		{
		alpha = (float) Math.atan2(itgy[itgyIndex], itgx[itgxIndex]);
		alpha = (alpha<0) ? alpha+(float)Math.PI : alpha;
		if (alpha<=Math.PI/4)
		{
		lambda = (float) Math.tan(alpha);
		r1 = (1-lambda)*itr[itrIndex+1]+lambda*itr[itrIndex+x_+1];
		c1 = (1-lambda)*itc[itcIndex+1]+lambda*itc[itcIndex+x_+1];
		r2 = (1-lambda)*itr[itrIndex-1]+lambda*itr[itrIndex-x_-1];
		c2 = (1-lambda)*itc[itcIndex-1]+lambda*itc[itcIndex-x_-1];
		if (fcomp(fcompType, itr[itrIndex], itc[itcIndex], r1, c1)<0 && fcomp(fcompType, itr[itrIndex], itc[itcIndex], r2, c2)<=0)
		{
		itnmxr[itnmxrIndex] = itr[itrIndex];
		itnmxc[itnmxcIndex] = itc[itcIndex];
		}
		}
		else if (alpha<=Math.PI/2)
		{
		lambda = (float) Math.tan(Math.PI/2-alpha);
		r1 = (1-lambda)*itr[itrIndex+x_]+lambda*itr[itrIndex+x_+1];
		c1 = (1-lambda)*itc[itcIndex+x_]+lambda*itc[itcIndex+x_+1];
		r2 = (1-lambda)*itr[itrIndex-x_]+lambda*itr[itrIndex-x_-1];
		c2 = (1-lambda)*itc[itcIndex-x_]+lambda*itc[itcIndex-x_-1];
		if (fcomp(fcompType,itr[itrIndex], itc[itcIndex], r1, c1)<0 && fcomp(fcompType,itr[itrIndex], itc[itcIndex], r2, c2)<=0)
		{
		itnmxr[itnmxrIndex] = itr[itrIndex];
		itnmxc[itnmxcIndex] = itc[itcIndex];
		}
		
		}
		else if(alpha<=3*Math.PI/4)
		{
		lambda = (float) Math.tan(alpha-Math.PI/2);
		r1 = (1-lambda)*itr[itrIndex+x_]+lambda*itr[itrIndex+x_-1];
		c1 = (1-lambda)*itc[itcIndex+x_]+lambda*itc[itcIndex+x_-1];
		r2 = (1-lambda)*itr[itrIndex-x_]+lambda*itr[itrIndex-x_+1];
		c2 = (1-lambda)*itc[itcIndex-x_]+lambda*itc[itcIndex-x_+1];
		if (fcomp(fcompType, itr[itrIndex], itc[itcIndex], r1, c1)<0 && fcomp(fcompType, itr[itrIndex], itc[itcIndex], r2, c2)<=0)
		{
		itnmxr[itnmxrIndex] = itr[itrIndex];
		itnmxc[itnmxcIndex] = itc[itcIndex];
		}
		}
		else
		{
		lambda = (float) Math.tan(Math.PI-alpha);
		r1 = (1-lambda)*itr[itrIndex-1]+lambda*itr[itrIndex+x_-1];
		c1 = (1-lambda)*itc[itcIndex-1]+lambda*itc[itcIndex+x_-1];
		r2 = (1-lambda)*itr[itrIndex+1]+lambda*itr[itrIndex-x_+1];
		c2 = (1-lambda)*itc[itcIndex+1]+lambda*itc[itcIndex-x_+1];
		if (fcomp(fcompType, itr[itrIndex], itc[itcIndex], r1, c1)<0 && fcomp(fcompType, itr[itrIndex], itc[itcIndex], r2, c2)<=0)
		{
		itnmxr[itnmxrIndex] = itr[itrIndex];
		itnmxc[itnmxcIndex] = itc[itcIndex];
		}
		}
		}
		}
		}
	}
		
		private void strConfEstim(float ranks[], float confidence[], float rezult[],
	            int fevalType)
		{
		int i;
		for (i=0; i<x_*y_; i++)
		{
		switch(fevalType) {
		case FC_ELLIPSE:
		    rezult[i] = ellipseEval(ranks[i], confidence[i]);
		    break;
		case FC_VERT_LINE:
			rezult[i] = verticalLineEval(ranks[i], confidence[i]);
			break;
		case FC_HORIZ_LINE:
			rezult[i] = horizontalLineEval(ranks[i], confidence[i]);
			break;
		case FC_LINE:
			rezult[i] = lineEval(ranks[i], confidence[i]);
			break;
		case FC_SQUARE_BOX:
			rezult[i] = squareEval(ranks[i], confidence[i]);
			break;
		case FC_CUSTOM:
			rezult[i] = customRegionEval(ranks[i], confidence[i]);
			break;
		}
		}
		}
		
		private void compRanks(float strength[], float ranks[])
		{
		   int index[];
		   float ra[];
		   ra = new float[x_*y_];
		   index = new int[x_*y_];
		   int ii;
		   
		   for (ii=0; ii<x_*y_; ii++)
		   {
		      index[ii] = ii;
		      ranks[ii] = 0;
		      ra[ii] = strength[ii];
		   }

		   //heap sort with ranks (from numerical recipies)
		   long i, ir, j, l;
		   long n;
		   n = x_*y_;
		   float rra;
		   int irra;

		   if (n<2)
		      return;
		   l = (n>>1)+1;
		   ir = n;
		   for (;;)
		   {
		      if (l>1)
		      {
		         rra = ra[(int)(--l-1)];
		         irra = index[(int)(l-1)];
		      }
		      else
		      {
		         rra = ra[(int)(ir-1)];
		         irra = index[(int)(ir-1)];
		         ra[(int)(ir-1)] = ra[1-1];
		         index[(int)(ir-1)] = index[1-1];
		         if (--ir==1)
		         {
		            ra[1-1] = rra;
		            index[1-1] = irra;
		            break;
		         }
		      }
		      i = l;
		      j = l+l;
		      while (j<=ir)
		      {
		         if (j<ir && ra[(int)(j-1)]<ra[(int)(j+1-1)])
		            j++;
		         if (rra<ra[(int)(j-1)])
		         {
		            ra[(int)(i-1)] = ra[(int)(j-1)];
		            index[(int)(i-1)] = index[(int)(j-1)];
		            i = j;
		            j <<= 1;
		         }
		         else
		            j = ir+1;
		      }
		      ra[(int)(i-1)] = rra;
		      index[(int)(i-1)] = irra;
		   }
		   
		   //setranks
		   irra = 1;
		   for (ii=1; ii<x_*y_; ii++)
		   {
		      if (ra[ii]>ZERO_TRESH)
		      {
		         ranks[index[ii]] = (float) irra;
		         if (ra[ii]>ra[ii-1])
		            irra++;
		      }
		   }
		   irra--;
		   for (ii=0; ii<x_*y_; ii++) 
		      ranks[ii] /= irra;

		  index = null;
		  ra = null;
		}
		
		private void newHysteresisTr(float edge[], float low[], BgEdgeList cel, int nMin, float mark[], float coord[])
		{
		   float tm[];
		   float te[];
		   int i,j;
		   int tmIndex = 0;
		   int teIndex = 0;
		   
		   for (i=0, tm=mark; i<x_*y_; i++,tmIndex++)
		      tm[tmIndex]=0;
		   
		   te_ = te = edge;
		   tm_ = tm = mark;
		   tmIndex = 0;
		   tl_ = low;
		   
		   for (j=0; j<y_; j++)
		   {
		      for (i=0; i<x_; i++, tmIndex++, teIndex++)
		      {
		         if ((tm[tmIndex]==0) && (te[teIndex]>HYST_LOW_CUT))
		         {
		            //found an edge start
		            npt_ = 0;
		            tm[tmIndex] = 1;
		            tc_ = coord;
		            tc_Index = 0;
		            newEdgeFollow(i, j);
		            //store the edge
		            if (npt_>=nMin) cel.addEdge(coord, npt_);
		         }
		      }
		   }
		}
		
		private void newEdgeFollow(int ii,int jj)
		{
		   int i;
		   int iin, jjn;
		   for (i=0; i<8; i++)
		   {
		      iin = ii+gNb[i][0];
		      jjn = jj+gNb[i][1];
		      if ((tm_[jjn*x_+iin]==0) && ((tl_[jjn*x_+iin])>0))
		      {
		         tm_[jjn*x_+iin] = 1;
		         newEdgeFollow(iin, jjn);
		      }
		   }
		   tc_[tc_Index++] = (float) ii;
		   tc_[tc_Index++] = (float) jj;
		   npt_++;
		}
		
		private void subspaceEstim(float im[], float grx[], float gry[], float cee[])
		{
		   // im original image
		   // grx, gry gradient of image
		   // cee confidence edge estimate
		   
		   float itim[];
		   float itgx[];
		   float itgy[];
		   float itcee[];
		   itim = im;
		   itgx = grx;
		   itgy = gry;
		   itcee = cee;
		   int itimIndex = 0;
		   int itgxIndex = 0;
		   int itgyIndex = 0;
		   int itceeIndex = 0;

		   double tae[];
		   double ti[];

		   ti = new double[WW_*WW_];

		   int i,j,l,c;
		   double v1;
		   double angleEdge;
		   int WW2 = WW_*WW_;
		   
		   itimIndex += WL_*x_;
		   itgxIndex += WL_*x_;
		   itgyIndex += WL_*x_;
		   itceeIndex += WL_*x_;

		   for (j=WL_; j<(y_-WL_); j++)
		   {
		      for (i=0; i<WL_; i++)
		         itcee[itceeIndex+i] = 0;
		      itimIndex += WL_;
		      itgxIndex += WL_;
		      itgyIndex += WL_;
		      itceeIndex += WL_;


		      for (i=WL_; i<(x_-WL_); i++, itimIndex++, itgxIndex++, itgyIndex++, itceeIndex++)
		      {
		         if ((Math.abs(itgx[itgxIndex])+Math.abs(itgy[itgyIndex]))>TOL_E)
		         {
		            angleEdge = (-Math.atan2(itgx[itgxIndex], itgy[itgyIndex]))*180.0/Math.PI;
		            tae = lookTable_[(int) (angleEdge+180.49)];
		            
		            //A=A-mean(A)
		            v1=0;
		            for (l=0; l<WW_; l++)
		            {
		               for (c=0; c<WW_; c++)
		               {
		                  v1 += ti[l*WW_+c] = itim[itimIndex+(l-WL_)*x_+c-WL_];
		               }
		            }
		            v1 /= WW2;
		            for (l=0; l<WW2; l++)
		               ti[l] -= v1;
		            
		            //A/norm(A,'fro')
		            v1 = 0;
		            for (l=0; l<WW2; l++)
		               v1 += ti[l]*ti[l];
		            v1 = Math.sqrt(v1);
		            for (l=0; l<WW2; l++)
		               ti[l] /= v1;

		            //global
		            v1 = 0;
		            for (l=0; l<WW2; l++)
		               v1 += tae[l]*ti[l];
		            v1 = Math.abs(v1);
		            itcee[itceeIndex] = (float) v1;
		         }
		         else
		         {
		            itcee[itceeIndex] = 0;
		         }
		      }
		      for (i=0; i<WL_; i++)
		         itcee[itceeIndex+i] = 0;
		      itimIndex += WL_;
		      itgxIndex += WL_;
		      itgyIndex += WL_;
		      itceeIndex += WL_;
		   }
		   WW2 = x_*y_;
		   for (j=0; j<(WL_*x_); j++)
		   {
		      cee[j] = 0;
		      cee[WW2-j-1] = 0;
		   }   

		   
		   ti = null;
		}
		
	}

	
}