package gov.nih.mipav.model.algorithms.registration;
 
/**
 *
 *  This algorithm handles registration algorithms
 *	of images with the Demons algorithm
 *	and a simple multiscale technique
 *	(Demons variant from Tom Vercauteren's 2009 paper, 
 *   "Diffeomorphic demons: Efficient non-parametric image registration by
 *  Tom Vercauteren, Xavier Pennec, Aymeric Perchant, and Nicholas Ayache,
 *  NeuroImage, 45, 2009, S61-S72.)
 *
 *
 *	@version    November 2009
 *	@author     Pierre-Louis Bazin
 *		
 *
 */
 
public class DemonsRegistrationLite {
		
	// numerical quantities
	private static final float INF=1e30f;
	private static final float ZERO=1e-30f;
	
	// convenience tags
	private static final int X=0;
	private static final int Y=1;
	private static final int Z=2;
	private static final int T=3;
	
	// flag labels
	public static final	int FLUID = 301;
	public static final	int DIFFUSION = 302;
	
	
	// data buffers
	private float[]	image;  		// source image
	private float[] target;			// target image
	private float[] fixed;  			// fixed image (multiscale)
	private float[] moving;				// moving image (multiscale)
	private	float[][] c;				// transform from target space to original space (multiscale)
	private	float[][] s;				// transform from target space to original space (multiscale)
	private	float[][] u;				// transform update from target space to original space (multiscale)
	private int[] nix,niy,niz;   	// image dimensions (pyramid)
	private int[] ntx,nty,ntz;   	// target dimensions (pyramid)
	private float rix,riy,riz;   	// image resolutions (no pyramid)
	private float rtx,rty,rtz;   	// target resolutions (no pyramid)
	private	float imin, imax;
	private	float tmin, tmax;
	
	private int nmx,nmy,nmz;   	// image dimensions (no pyramid)
	private int nfx,nfy,nfz;   	// target dimensions (no pyramid)
	private float rmx,rmy,rmz;   	// image resolutions (no pyramid)
	private float rfx,rfy,rfz;   	// target resolutions (no pyramid)
	private float[][] transform;		// prior transform matrix (for instance from prior alignment)		
	
	// pyramid handling
	private	int	levels;		// number of levels in the pyramid
	private	int Niter;		// iterations for the first pass at a level
	
	// parameters
	private	float smoothingKernel; 	// smoothing kernel size
	private	float spatialScale; 		// scale of transformation
	private	int	regType;
	
	// computation variables
	private	float sigma2; 		// scale of transformation
	

	private	float[][] gaussKernel;
	private	int	kx,ky,kz;
    
	// computation flags
	private boolean isWorking;
	private boolean isCompleted;
	
	// for debug and display
	private static final boolean debug=true;
	private static final boolean verbose=true;
    private boolean do2D = false;
    
	/**
	 *  constructor
     *  note: the membership function mems_ must have sum = 0 on the masked areas
	 */
	public DemonsRegistrationLite(float[] image_, float[] target_,
									int nix_, int niy_, int niz_,
									float rix_, float riy_, float riz_,
									int ntx_, int nty_, int ntz_,
									float rtx_, float rty_, float rtz_,
									float smoothing_,
									float scale_,
									int lev_, int Ni_,
									int reg_, 
									float[][] trans_) {
	
		smoothingKernel = smoothing_;
		spatialScale = scale_;
		sigma2 = spatialScale*spatialScale;
		
		levels = lev_;
		Niter = Ni_;
        
        if (niz_ == 1) {
            do2D = true;
        }
        else {
            do2D = false;
        }
		
		transform = trans_;
        
        if (do2D) {
            rix = rix_;
            riy = riy_;
                
            rtx = rtx_;
            rty = rty_;
                
            regType = reg_;
            
            gaussKernel = separableGaussianKernel(smoothingKernel/rtx,smoothingKernel/rty);
            
            // init all the arrays : allocate the pyramids
            try {
                
                    nix = new int[levels];
                    niy = new int[levels];
                    ntx = new int[levels];
                    nty = new int[levels];
                    // compute the pyramid dimensions
                    nix[0] = nix_;
                    niy[0] = niy_;
                    ntx[0] = ntx_;
                    nty[0] = nty_;
                    for (int l=1;l<levels;l++) {
                        nix[l] = (int)Math.floor(nix[l-1]/2.0);
                        niy[l] = (int)Math.floor(niy[l-1]/2.0);
                        ntx[l] = (int)Math.floor(ntx[l-1]/2.0);
                        nty[l] = (int)Math.floor(nty[l-1]/2.0);
                    }
                    
                    preprocessInputImages2D(image_, target_);
            
            } catch (OutOfMemoryError e){
                isWorking = false;
                finalize();
                System.out.println(e.getMessage());
                return;
            }
        } // if (do2D)
        else { // !do2D
		
    		rix = rix_;
    		riy = riy_;
    		riz = riz_;
    			
    		rtx = rtx_;
    		rty = rty_;
    		rtz = rtz_;
    			
    		regType = reg_;
    		
    		
            gaussKernel = separableGaussianKernel(smoothingKernel/rtx,smoothingKernel/rty,smoothingKernel/rtz);
    		
    		// init all the arrays : allocate the pyramids
    		try {
                
        			nix = new int[levels];
        			niy = new int[levels];
        			niz = new int[levels];
        			ntx = new int[levels];
        			nty = new int[levels];
        			ntz = new int[levels];
                    // compute the pyramid dimensions
        			nix[0] = nix_;
        			niy[0] = niy_;
        			niz[0] = niz_;
        			ntx[0] = ntx_;
        			nty[0] = nty_;
        			ntz[0] = ntz_;
        			for (int l=1;l<levels;l++) {
        				nix[l] = (int)Math.floor(nix[l-1]/2.0);
        				niy[l] = (int)Math.floor(niy[l-1]/2.0);
        				niz[l] = (int)Math.floor(niz[l-1]/2.0);
        				ntx[l] = (int)Math.floor(ntx[l-1]/2.0);
        				nty[l] = (int)Math.floor(nty[l-1]/2.0);
        				ntz[l] = (int)Math.floor(ntz[l-1]/2.0);
        			}
        			
        			preprocessInputImages(image_, target_);
    		
    		} catch (OutOfMemoryError e){
    			isWorking = false;
                finalize();
    			System.out.println(e.getMessage());
    			return;
    		}
        } // else !do2D
		isWorking = true;
		//if (debug) MipavUtil.displayMessage("Demons:initialisation\n");
	}

	final public void finalize() {
		image = null;
		target = null;
		fixed = null; moving = null;
		s = null; u = null; c = null;
		System.gc();
	}
    
   public final float[][] getCurrentTransform() {
	   return s;
   }
   public final float[][] getCurrentUpdate() {
		return u;
	}
    
	public final boolean isWorking() { return isWorking; }
	public final boolean isCompleted() { return isCompleted; }
    
    private final void preprocessInputImages2D(float[] img, float[] trg) {
        // find min, max
        imin = INF;
        imax = -INF;
        for (int xy=0;xy<nix[0]*niy[0];xy++) {
            if (img[xy]<imin) imin = img[xy];
            if (img[xy]>imax) imax = img[xy];
        }
        tmin = INF;
        tmax = -INF;
        for (int xy=0;xy<ntx[0]*nty[0];xy++) {
            if (trg[xy]<tmin) tmin = trg[xy];
            if (trg[xy]>tmax) tmax = trg[xy];
        }
        
        // normalize in [0,1]
        image = img;
        target = trg;
        for (int xy=0;xy<nix[0]*niy[0];xy++) {
            image[xy] = (image[xy]-imin)/(imax-imin);
        }
        for (int xy=0;xy<ntx[0]*nty[0];xy++) {
            target[xy] = (target[xy]-tmin)/(tmax-tmin);
        }
        
    }
    
	private final void preprocessInputImages(float[] img, float[] trg) {
		// find min, max
		imin = INF;
		imax = -INF;
		for (int xyz=0;xyz<nix[0]*niy[0]*niz[0];xyz++) {
			if (img[xyz]<imin) imin = img[xyz];
			if (img[xyz]>imax) imax = img[xyz];
		}
		tmin = INF;
		tmax = -INF;
		for (int xyz=0;xyz<ntx[0]*nty[0]*ntz[0];xyz++) {
			if (trg[xyz]<tmin) tmin = trg[xyz];
			if (trg[xyz]>tmax) tmax = trg[xyz];
		}
		
		// normalize in [0,1]
		image = img;
		target = trg;
		for (int xyz=0;xyz<nix[0]*niy[0]*niz[0];xyz++) {
			image[xyz] = (image[xyz]-imin)/(imax-imin);
		}
		for (int xyz=0;xyz<ntx[0]*nty[0]*ntz[0];xyz++) {
			target[xyz] = (target[xyz]-tmin)/(tmax-tmin);
		}
		
	}
    
    /** create an image pyramid from the original data, initialize Jacobians, etc */
    private final void initializeImages2D(int lvl) {

        int scale = 1;
        
        if (lvl>0) {
            for (int l=0;l<lvl;l++) scale *= 2;
            
            moving = subsample(image, nix[0], niy[0], scale);
            fixed = subsample(target, ntx[0], nty[0], scale);
        } else {
            moving = image;
            fixed = target;
        }
        
        nmx = nix[lvl];
        nmy = niy[lvl];
        
        nfx = ntx[lvl];
        nfy = nty[lvl];
        
        rmx = rix*scale;
        rmy = riy*scale;

        rfx = rtx*scale;
        rfy = rty*scale;

        //if (debug) MipavUtil.displayMessage("scale "+(lvl+1)+": \n img ("+nmx+"x"+nmy+"x"+", "+rmx+"|"+rmy+") \n trg ("+nfx+"x"+nfy+"x", "+rfx+"|"+rfy+") \n");
                    
    }
	
	/** create an image pyramid from the original data, initialize Jacobians, etc */
	private final void initializeImages(int lvl) {

		int scale = 1;
		
		if (lvl>0) {
			for (int l=0;l<lvl;l++) scale *= 2;
			
			moving = subsample(image, nix[0], niy[0], niz[0], scale);
			fixed = subsample(target, ntx[0], nty[0], ntz[0], scale);
		} else {
			moving = image;
			fixed = target;
		}
		
		nmx = nix[lvl];
		nmy = niy[lvl];
		nmz = niz[lvl];
		
		nfx = ntx[lvl];
		nfy = nty[lvl];
		nfz = ntz[lvl];
		
		rmx = rix*scale;
		rmy = riy*scale;
		rmz = riz*scale;

		rfx = rtx*scale;
		rfy = rty*scale;
		rfz = rtz*scale;

		//if (debug) MipavUtil.displayMessage("scale "+(lvl+1)+": \n img ("+nmx+"x"+nmy+"x"+nmz+", "+rmx+"|"+rmy+"|"+rmz+") \n trg ("+nfx+"x"+nfy+"x"+nfz+", "+rfx+"|"+rfy+"|"+rfz+") \n");
					
	}
    
    /** initialize the transform from previous estimate, if exists */
    private final void initializeTransform2D(int lvl) {
        // initialization: start from prior transform or zero
        float[][] sn = new float[2][nfx*nfy];
        if (transform==null) {
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                sn[X][xy] = x;
                sn[Y][xy] = y;
            }
        } else {
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                sn[X][xy] = transform[X][X]*x + transform[X][Y]*y + transform[X][2];
                sn[Y][xy] = transform[Y][X]*x + transform[Y][Y]*y + transform[Y][2];
            }
        }
        
        if (lvl==levels-1) {
            s = sn;
        } else {
            // sample from previous scale
            for (int x=0;x<ntx[lvl+1];x++) for (int y=0;y<nty[lvl+1];y++) {
                int xy  = x+ntx[lvl+1]*y;
                int xy2  = 2*x+nfx*2*y;
                
                /* debug
                if (xy2>=nfx*nfy*nf) {
                    MipavUtil.displayMessage("too big: ("+x+","+y+") -> "+xy2+" ["+nfx+"|"+nfy+"]\n");
                    return;
                }
                if (xy>=ntx[lvl+1]*nty[lvl+1]) {
                    MipavUtil.displayMessage("too big: ("+x+","+y+") -> "+xy+" ["+ntx[lvl+1]+"|"+nty[lvl+1]+"]\n");
                    return;
                }
                */
                
                // no scaling of the coordinates: it is done in the resolution computations
                sn[X][xy2] = 2.0f*s[X][xy];
                sn[Y][xy2] = 2.0f*s[Y][xy];
                
                sn[X][xy2+1] = 2.0f*s[X][xy]+1.0f;
                sn[Y][xy2+1] = 2.0f*s[Y][xy];
                
                sn[X][xy2+nfx] = 2.0f*s[X][xy];
                sn[Y][xy2+nfx] = 2.0f*s[Y][xy]+1.0f;
                
                sn[X][xy2+1+nfx] = 2.0f*s[X][xy]+1.0f;
                sn[Y][xy2+1+nfx] = 2.0f*s[Y][xy]+1.0f;
                
            }
            s = null;
            s = sn;
        }
        
        // update always zero
        u = new float[2][nfx*nfy];
        for (int xy=0;xy<nfx*nfy;xy++) {
            u[X][xy] = 0.0f;
            u[Y][xy] = 0.0f;
        }
        
        // composite update always zero
        c = new float[2][nfx*nfy];
        for (int xy=0;xy<nfx*nfy;xy++) {
            c[X][xy] = 0.0f;
            c[Y][xy] = 0.0f;
        }
        
        return;
    }
	
    /** initialize the transform from previous estimate, if exists */
    private final void initializeTransform(int lvl) {
		// initialization: start from prior transform or zero
		float[][] sn = new float[3][nfx*nfy*nfz];
		if (transform==null) {
			for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
				int xyz = x+nfx*y+nfx*nfy*z;
				sn[X][xyz] = x;
				sn[Y][xyz] = y;
				sn[Z][xyz] = z;
			}
		} else {
			for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
				int xyz = x+nfx*y+nfx*nfy*z;
				sn[X][xyz] = transform[X][X]*x + transform[X][Y]*y + transform[X][Z]*z + transform[X][T];
				sn[Y][xyz] = transform[Y][X]*x + transform[Y][Y]*y + transform[Y][Z]*z + transform[Y][T];
				sn[Z][xyz] = transform[Z][X]*x + transform[Z][Y]*y + transform[Z][Z]*z + transform[Z][T];
			}
		}
		
		if (lvl==levels-1) {
			s = sn;
		} else {
			// sample from previous scale
			for (int x=0;x<ntx[lvl+1];x++) for (int y=0;y<nty[lvl+1];y++) for (int z=0;z<ntz[lvl+1];z++) {
				int xyz  = x+ntx[lvl+1]*y+ntx[lvl+1]*nty[lvl+1]*z;
				int xyz2  = 2*x+nfx*2*y+nfx*nfy*2*z;
				
				/* debug
				if (xyz2>=nfx*nfy*nfz) {
					MipavUtil.displayMessage("too big: ("+x+","+y+","+z+") -> "+xyz2+" ["+nfx+"|"+nfy+"|"+nfz+"]\n");
					return;
				}
				if (xyz>=ntx[lvl+1]*nty[lvl+1]*ntz[lvl+1]) {
					MipavUtil.displayMessage("too big: ("+x+","+y+","+z+") -> "+xyz+" ["+ntx[lvl+1]+"|"+nty[lvl+1]+"|"+ntz[lvl+1]+"]\n");
					return;
				}
				*/
				
				// no scaling of the coordinates: it is done in the resolution computations
				sn[X][xyz2] = 2.0f*s[X][xyz];
				sn[Y][xyz2] = 2.0f*s[Y][xyz];
				sn[Z][xyz2] = 2.0f*s[Z][xyz];
				
				sn[X][xyz2+1] = 2.0f*s[X][xyz]+1.0f;
				sn[Y][xyz2+1] = 2.0f*s[Y][xyz];
				sn[Z][xyz2+1] = 2.0f*s[Z][xyz];
				
            	sn[X][xyz2+nfx] = 2.0f*s[X][xyz];
				sn[Y][xyz2+nfx] = 2.0f*s[Y][xyz]+1.0f;
				sn[Z][xyz2+nfx] = 2.0f*s[Z][xyz];
				
            	sn[X][xyz2+nfx*nfy] = 2.0f*s[X][xyz];
				sn[Y][xyz2+nfx*nfy] = 2.0f*s[Y][xyz];
				sn[Z][xyz2+nfx*nfy] = 2.0f*s[Z][xyz]+1.0f;
				
				sn[X][xyz2+1+nfx] = 2.0f*s[X][xyz]+1.0f;
				sn[Y][xyz2+1+nfx] = 2.0f*s[Y][xyz]+1.0f;
				sn[Z][xyz2+1+nfx] = 2.0f*s[Z][xyz];
				
            	sn[X][xyz2+nfx+nfx*nfy] = 2.0f*s[X][xyz];
				sn[Y][xyz2+nfx+nfx*nfy] = 2.0f*s[Y][xyz]+1.0f;
				sn[Z][xyz2+nfx+nfx*nfy] = 2.0f*s[Z][xyz]+1.0f;
				
            	sn[X][xyz2+nfx*nfy+1] = 2.0f*s[X][xyz]+1.0f;
				sn[Y][xyz2+nfx*nfy+1] = 2.0f*s[Y][xyz];
				sn[Z][xyz2+nfx*nfy+1] = 2.0f*s[Z][xyz]+1.0f;
				
            	sn[X][xyz2+1+nfx+nfx*nfy] = 2.0f*s[X][xyz]+1.0f;
				sn[Y][xyz2+1+nfx+nfx*nfy] = 2.0f*s[Y][xyz]+1.0f;
				sn[Z][xyz2+1+nfx+nfx*nfy] = 2.0f*s[Z][xyz]+1.0f;
				
            }
			s = null;
			s = sn;
        }
		
		// update always zero
		u = new float[3][nfx*nfy*nfz];
		for (int xyz=0;xyz<nfx*nfy*nfz;xyz++) {
			u[X][xyz] = 0.0f;
			u[Y][xyz] = 0.0f;
			u[Z][xyz] = 0.0f;
		}
		
		// composite update always zero
		c = new float[3][nfx*nfy*nfz];
		for (int xyz=0;xyz<nfx*nfy*nfz;xyz++) {
			c[X][xyz] = 0.0f;
			c[Y][xyz] = 0.0f;
			c[Z][xyz] = 0.0f;
		}
		
		return;
    }
    
    /**
     * compute the image position given the target
     * for a given level l
     * performs only one iteration
     */
    final public void registerImageToTarget2D(int lvl, int iterations) {
        
        //if (debug) MipavUtil.displayMessage("initialize all parameters \n");
        initializeImages2D(lvl);
        initializeTransform2D(lvl);
        
        for (int t=0;t<iterations;t++) {
            
            //if (debug) MipavUtil.displayMessage("iteration "+(t+1)+"\n");
            
            //if (debug) MipavUtil.displayMessage("update: ");
            
            for (int x=1;x<nfx-1;x++) for (int y=1;y<nfy-1;y++)  {
                int xy = x+nfx*y;
            
                // compute the update field
                float xs = s[X][xy]*rfx/rmx;
                float ys = s[Y][xy]*rfy/rmy;
            
                float xsmx = s[X][xy-1]*rfx/rmx;
                float ysmx = s[Y][xy-1]*rfy/rmy;
        
                float xspx = s[X][xy+1]*rfx/rmx;
                float yspx = s[Y][xy+1]*rfy/rmy;
        
                float xsmy = s[X][xy-nfx]*rfx/rmx;
                float ysmy = s[Y][xy-nfx]*rfy/rmy;
        
                float xspy = s[X][xy+nfx]*rfx/rmx;
                float yspy = s[Y][xy+nfx]*rfy/rmy;
                
                u[X][xy] = 0.0f;
                u[Y][xy] = 0.0f;
                
                float mov = linearInterpolation(moving,0.0f,xs,ys,nmx,nmy);
                
                float diff = (fixed[xy] - mov);
                
                float Jx, Jy;
                
                // symmetric forces
                Jx = 0.25f/rfx*(fixed[xy+1]      - fixed[xy-1]);
                Jy = 0.25f/rfy*(fixed[xy+nfx]    - fixed[xy-nfx]);
                
                Jx += 0.25f/rmx*(linearInterpolation(moving,0.0f,xspx,yspx,nmx,nmy)-linearInterpolation(moving,0.0f,xsmx,ysmx,nmx,nmy));
                Jy += 0.25f/rmy*(linearInterpolation(moving,0.0f,xspy,yspy,nmx,nmy)-linearInterpolation(moving,0.0f,xsmy,ysmy,nmx,nmy));
            
                float J2 = Jx*Jx+Jy*Jy;
                float den = (float)Math.max(ZERO,diff*diff/sigma2 + J2);
                
                u[X][xy] += diff*Jx;
                u[Y][xy] += diff*Jy;

                u[X][xy] /= den;
                u[Y][xy] /= den;
                
                //maxU = Math.max(maxU, u[X][xy]*u[X][xy]+u[Y][xy]*u[Y][xy]);
            }
            //if (debug) MipavUtil.displayMessage(""+maxU+"\n");
            
            if (regType==FLUID) {
                //if (debug) MipavUtil.displayMessage("fluid regularization \n");
            
                // smooth the result with a gaussian kernel
                u[X] = separableConvolution(u[X],nfx,nfy,gaussKernel,kx,ky);
                u[Y] = separableConvolution(u[Y],nfx,nfy,gaussKernel,kx,ky);
            }
            
            // compose the transformations
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                
                float xu = x+u[X][xy];
                float yu = y+u[Y][xy];
                
                // note: if outside, extrapolate as X+u
                c[X][xy] = linearInterpolation(s[X],xu,xu,yu,nfx,nfy) - x;
                c[Y][xy] = linearInterpolation(s[Y],yu,xu,yu,nfx,nfy) - y;
            }
            
            if (regType==DIFFUSION) {
                // smooth the result with a gaussian kernel
                c[X] = separableConvolution(c[X],nfx,nfy,gaussKernel,kx,ky);
                c[Y] = separableConvolution(c[Y],nfx,nfy,gaussKernel,kx,ky);
            } 
            
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                
                s[X][xy] = x + c[X][xy];
                s[Y][xy] = y + c[Y][xy];
            }
        }
            
        return;
    } // 
    
	
    /**
	 * compute the image position given the target
	 * for a given level l
     * performs only one iteration
	 */
    final public void registerImageToTarget(int lvl, int iterations) {
		
		//if (debug) MipavUtil.displayMessage("initialize all parameters \n");
        initializeImages(lvl);
		initializeTransform(lvl);
		
		for (int t=0;t<iterations;t++) {
			
			//if (debug) MipavUtil.displayMessage("iteration "+(t+1)+"\n");
			
			//if (debug) MipavUtil.displayMessage("update: ");
			
			for (int x=1;x<nfx-1;x++) for (int y=1;y<nfy-1;y++) for (int z=1;z<nfz-1;z++) {
				int xyz = x+nfx*y+nfx*nfy*z;
			
				// compute the update field
				float xs = s[X][xyz]*rfx/rmx;
				float ys = s[Y][xyz]*rfy/rmy;
				float zs = s[Z][xyz]*rfz/rmz;
			
				float xsmx = s[X][xyz-1]*rfx/rmx;
				float ysmx = s[Y][xyz-1]*rfy/rmy;
				float zsmx = s[Z][xyz-1]*rfz/rmz;
		
				float xspx = s[X][xyz+1]*rfx/rmx;
				float yspx = s[Y][xyz+1]*rfy/rmy;
				float zspx = s[Z][xyz+1]*rfz/rmz;
		
				float xsmy = s[X][xyz-nfx]*rfx/rmx;
				float ysmy = s[Y][xyz-nfx]*rfy/rmy;
				float zsmy = s[Z][xyz-nfx]*rfz/rmz;
		
				float xspy = s[X][xyz+nfx]*rfx/rmx;
				float yspy = s[Y][xyz+nfx]*rfy/rmy;
				float zspy = s[Z][xyz+nfx]*rfz/rmz;
		
				float xsmz = s[X][xyz-nfx*nfy]*rfx/rmx;
				float ysmz = s[Y][xyz-nfx*nfy]*rfy/rmy;
				float zsmz = s[Z][xyz-nfx*nfy]*rfz/rmz;
		
				float xspz = s[X][xyz+nfx*nfy]*rfx/rmx;
				float yspz = s[Y][xyz+nfx*nfy]*rfy/rmy;
				float zspz = s[Z][xyz+nfx*nfy]*rfz/rmz;
				
				u[X][xyz] = 0.0f;
				u[Y][xyz] = 0.0f;
				u[Z][xyz] = 0.0f;
				
				float mov = linearInterpolation(moving,0.0f,xs,ys,zs,nmx,nmy,nmz);
				
				float diff = (fixed[xyz] - mov);
				
				float Jx, Jy, Jz;
				
				// symmetric forces
				Jx = 0.25f/rfx*(fixed[xyz+1] 	  - fixed[xyz-1]);
				Jy = 0.25f/rfy*(fixed[xyz+nfx] 	  - fixed[xyz-nfx]);
				Jz = 0.25f/rfz*(fixed[xyz+nfx*nfy] - fixed[xyz-nfx*nfy]);
				
				Jx += 0.25f/rmx*(linearInterpolation(moving,0.0f,xspx,yspx,zspx,nmx,nmy,nmz)-linearInterpolation(moving,0.0f,xsmx,ysmx,zsmx,nmx,nmy,nmz));
				Jy += 0.25f/rmy*(linearInterpolation(moving,0.0f,xspy,yspy,zspy,nmx,nmy,nmz)-linearInterpolation(moving,0.0f,xsmy,ysmy,zsmy,nmx,nmy,nmz));
				Jz += 0.25f/rmz*(linearInterpolation(moving,0.0f,xspz,yspz,zspz,nmx,nmy,nmz)-linearInterpolation(moving,0.0f,xsmz,ysmz,zsmz,nmx,nmy,nmz));
			
				float J2 = Jx*Jx+Jy*Jy+Jz*Jz;
				float den = (float)Math.max(ZERO,diff*diff/sigma2 + J2);
				
				u[X][xyz] += diff*Jx;
				u[Y][xyz] += diff*Jy;
				u[Z][xyz] += diff*Jz;

				u[X][xyz] /= den;
				u[Y][xyz] /= den;
				u[Z][xyz] /= den;
				
				//maxU = Math.max(maxU, u[X][xyz]*u[X][xyz]+u[Y][xyz]*u[Y][xyz]+u[Z][xyz]*u[Z][xyz]);
			}
			//if (debug) MipavUtil.displayMessage(""+maxU+"\n");
			
			if (regType==FLUID) {
				//if (debug) MipavUtil.displayMessage("fluid regularization \n");
			
				// smooth the result with a gaussian kernel
				u[X] = separableConvolution(u[X],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
				u[Y] = separableConvolution(u[Y],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
				u[Z] = separableConvolution(u[Z],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
			}
			
			// compose the transformations
			for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
				int xyz = x+nfx*y+nfx*nfy*z;
				
				float xu = x+u[X][xyz];
				float yu = y+u[Y][xyz];
				float zu = z+u[Z][xyz];
				
				// note: if outside, extrapolate as X+u
				c[X][xyz] = linearInterpolation(s[X],xu,xu,yu,zu,nfx,nfy,nfz) - x;
				c[Y][xyz] = linearInterpolation(s[Y],yu,xu,yu,zu,nfx,nfy,nfz) - y;
				c[Z][xyz] = linearInterpolation(s[Z],zu,xu,yu,zu,nfx,nfy,nfz) - z;
			}
			
			if (regType==DIFFUSION) {
				// smooth the result with a gaussian kernel
				c[X] = separableConvolution(c[X],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
				c[Y] = separableConvolution(c[Y],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
				c[Z] = separableConvolution(c[Z],nfx,nfy,nfz,gaussKernel,kx,ky,kz);
			} 
			
			for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
				int xyz = x+nfx*y+nfx*nfy*z;
				
				s[X][xyz] = x + c[X][xyz];
				s[Y][xyz] = y + c[Y][xyz];
				s[Z][xyz] = z + c[Z][xyz];
			}
		}
			
        return;
    } // 
    
    /** 
	 *	runs the gaussian pyramid algorithm
	 */
	public final void runGaussianPyramid() {        
        // going down to 0
		for (int l=levels-1;l>=0;l--) {
			//if (debug) MipavUtil.displayMessage("gaussian pyramid : level"+(l+1)+"\n");
            if (do2D) {
                registerImageToTarget2D(l,Niter);
            }
            else {
			    registerImageToTarget(l,Niter);
            }
		}
    }//runGaussianPyramid
    
    /** 
	 *	returns the transformed image
	 */
	public final float[] exportTransformedImage() {
		float 	xs,ys,zs;
        float[] img = null;
        if (do2D) {
            img = new float[nfx*nfy];
            
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                xs = s[X][xy]*rfx/rmx;
                ys = s[Y][xy]*rfy/rmy;
                
                // compute interpolated values
                img[xy] = imin + (imax-imin)*linearInterpolation(image,0.0f,xs,ys,nmx,nmy);
            }
        }
        else { // else !do2D
    		img = new float[nfx*nfy*nfz];
    		
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
    			int xyz = x+nfx*y+nfx*nfy*z;
                xs = s[X][xyz]*rfx/rmx;
    			ys = s[Y][xyz]*rfy/rmy;
    			zs = s[Z][xyz]*rfz/rmz;
    			
    			// compute interpolated values
    			img[xyz] = imin + (imax-imin)*linearInterpolation(image,0.0f,xs,ys,zs,nmx,nmy,nmz);
    		}
        } // else !do2D
		return img;
	} // exportTransformedImage

    /** 
	 *	returns the transform field v defined as X' = X+v (=s)
	 */
	public final float[] exportTransformField() {
        float vec[] = null;
        if (do2D) {
            vec = new float[nfx*nfy*2];
            
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) {
                int xy = x+nfx*y;
                vec[xy+X*nfx*nfy] = s[X][xy]-x;
                vec[xy+Y*nfx*nfy] = s[Y][xy]-y;           
            }     
        } // if (do2D)
        else { // !do2D
    		vec = new float[nfx*nfy*nfz*3];
    		
            for (int x=0;x<nfx;x++) for (int y=0;y<nfy;y++) for (int z=0;z<nfz;z++) {
    			int xyz = x+nfx*y+nfx*nfy*z;
    			vec[xyz+X*nfx*nfy*nfz] = s[X][xyz]-x;
    			vec[xyz+Y*nfx*nfy*nfz] = s[Y][xyz]-y;
    			vec[xyz+Z*nfx*nfy*nfz] = s[Z][xyz]-z;			
     		}
        } // else !do2D
		return vec;
	} // exportTransformedImage
    
    private float[][] separableGaussianKernel(float sx, float sy) {
        float sum;
        
        // kernel size
        kx = (int)Math.ceil(Math.max(3.0f*sx-0.5f,0.0f));
        ky = (int)Math.ceil(Math.max(3.0f*sy-0.5f,0.0f));
        
        //MipavUtil.displayMessage("kernel size: "+kx+"x"+ky+"x"+kz+"\n");
        //MipavUtil.displayMessage("scale: "+sx+"x"+sy+"x"+sz+"\n");
        // create the kernel
        float[][] kernel = new float[2][];
        kernel[0] = new float[2*kx+1]; 
        kernel[1] = new float[2*ky+1]; 
        
        sum = 0.0f;
        for (int i=-kx;i<=kx;i++) {
            kernel[0][kx+i] = (float)Math.exp( - 0.5f*(i*i)/(sx*sx) );
            //MipavUtil.displayMessage("exp("+( - 0.5f*(i*i)/(sx*sx) )+") = "+kernel[0][kx+i]+"\n");
            sum += kernel[0][kx+i];
        }
        for (int i=-kx;i<=kx;i++) kernel[0][kx+i] /= sum;
        
        sum = 0.0f;
        for (int j=-ky;j<=ky;j++) {
            kernel[1][ky+j] = (float)Math.exp( - 0.5f*(j*j)/(sy*sy) );
            sum += kernel[1][ky+j];
        }
        for (int j=-ky;j<=ky;j++) kernel[1][ky+j] /= sum;
        
        return kernel;
    }

	
	private float[][] separableGaussianKernel(float sx, float sy, float sz) {
		float sum;
		
		// kernel size
		kx = (int)Math.ceil(Math.max(3.0f*sx-0.5f,0.0f));
		ky = (int)Math.ceil(Math.max(3.0f*sy-0.5f,0.0f));
		kz = (int)Math.ceil(Math.max(3.0f*sz-0.5f,0.0f));
		
		//MipavUtil.displayMessage("kernel size: "+kx+"x"+ky+"x"+kz+"\n");
		//MipavUtil.displayMessage("scale: "+sx+"x"+sy+"x"+sz+"\n");
		// create the kernel
		float[][] kernel = new float[3][];
		kernel[0] = new float[2*kx+1]; 
		kernel[1] = new float[2*ky+1]; 
		kernel[2] = new float[2*kz+1]; 
		
		sum = 0.0f;
		for (int i=-kx;i<=kx;i++) {
			kernel[0][kx+i] = (float)Math.exp( - 0.5f*(i*i)/(sx*sx) );
			//MipavUtil.displayMessage("exp("+( - 0.5f*(i*i)/(sx*sx) )+") = "+kernel[0][kx+i]+"\n");
			sum += kernel[0][kx+i];
		}
		for (int i=-kx;i<=kx;i++) kernel[0][kx+i] /= sum;
		
		sum = 0.0f;
		for (int j=-ky;j<=ky;j++) {
			kernel[1][ky+j] = (float)Math.exp( - 0.5f*(j*j)/(sy*sy) );
			sum += kernel[1][ky+j];
		}
		for (int j=-ky;j<=ky;j++) kernel[1][ky+j] /= sum;
		
		sum = 0.0f;
		for (int l=-kz;l<=kz;l++) {
			kernel[2][kz+l] = (float)Math.exp( - 0.5f*(l*l)/(sz*sz) );
			sum += kernel[2][kz+l];
		}
		for (int l=-kz;l<=kz;l++) kernel[2][kz+l] /= sum;
		
		return kernel;
	}
    
    /**
     *  scale down by a factor
     */
    private float[] subsample(float[] image, int nx, int ny, int factor) {
        int nsx,nsy;
        float[] sub;
        float scale = factor*factor;
        
        nsx = (int)Math.floor(nx/(float)factor);
        nsy = (int)Math.floor(ny/(float)factor);
        sub = new float[nsx*nsy];
            
        for (int x=0;x<nsx;x++) for (int y=0;y<nsy;y++) {
            sub[x + nsx*y] = 0.0f;
            for (int i=0;i<factor;i++) for (int j=0;j<factor;j++) {
                sub[x + nsx*y] += image[x*factor+i + nx*(y*factor+j)];
            }
            sub[x + nsx*y] /= scale;
        }
        return sub;
    }

		/**
	 *	scale down by a factor
	 */
	private float[] subsample(float[] image, int nx, int ny, int nz, int factor) {
		int nsx,nsy,nsz;
		float[] sub;
		float scale = factor*factor*factor;
		
		nsx = (int)Math.floor(nx/(float)factor);
		nsy = (int)Math.floor(ny/(float)factor);
		nsz = (int)Math.floor(nz/(float)factor);
		sub = new float[nsx*nsy*nsz];
			
        for (int x=0;x<nsx;x++) for (int y=0;y<nsy;y++) for (int z=0;z<nsz;z++) {
			sub[x + nsx*y + nsx*nsy*z] = 0.0f;
			for (int i=0;i<factor;i++) for (int j=0;j<factor;j++) for (int l=0;l<factor;l++) {
				sub[x + nsx*y + nsx*nsy*z] += image[x*factor+i + nx*(y*factor+j) + nx*ny*(z*factor+l)];
			}
			sub[x + nsx*y + nsx*nsy*z] /= scale;
		}
		return sub;
	}
    
    /**
     *  linear interpolation, with value outside the image
     */
    private float linearInterpolation(float[] image, float value, float x, float y, int nx, int ny) {
        float alpha,beta,nalpha,nbeta,val;
        int x0,y0;

        // if out of boundary, replace all with zero
        if ( (x<0) || (x>nx-2) || (y<0) || (y>ny-2)) 
            return value;
        
        x0 = (int)Math.floor(x);
        y0 = (int)Math.floor(y);

        alpha = x - x0;
        nalpha = 1.0f - alpha;

        beta = y - y0;
        nbeta = 1.0f - beta;

        int xy0 = x0 + y0*nx;
        
        val = nalpha*nbeta*image[xy0] 
            + alpha*nbeta*image[xy0+1] 
            + nalpha*beta*image[xy0+nx] 
            + alpha*beta*image[xy0+1+nx]; 

        return val;
    }

	/**
	 *	linear interpolation, with value outside the image
	 */
	private float linearInterpolation(float[] image, float value, float x, float y, float z, int nx, int ny, int nz) {
		float alpha,beta,gamma,nalpha,nbeta,ngamma,val;
		int x0,y0,z0;

        // if out of boundary, replace all with zero
        if ( (x<0) || (x>nx-2) || (y<0) || (y>ny-2) || (z<0) || (z>nz-2) ) 
            return value;
        
		x0 = (int)Math.floor(x);
		y0 = (int)Math.floor(y);
		z0 = (int)Math.floor(z);

		alpha = x - x0;
		nalpha = 1.0f - alpha;

		beta = y - y0;
		nbeta = 1.0f - beta;

		gamma = z - z0;
		ngamma = 1.0f - gamma;

		int xyz0 = x0 + y0*nx + z0*nx*ny;
		
		val = nalpha*nbeta*ngamma*image[xyz0] 
			+ alpha*nbeta*ngamma*image[xyz0+1] 
			+ nalpha*beta*ngamma*image[xyz0+nx] 
			+ nalpha*nbeta*gamma*image[xyz0+nx*ny] 
			+ alpha*beta*ngamma*image[xyz0+1+nx] 
			+ nalpha*beta*gamma*image[xyz0+nx+nx*ny] 
			+ alpha*nbeta*gamma*image[xyz0+1+nx*ny] 
			+ alpha*beta*gamma*image[xyz0+1+nx+nx*ny];

		return val;
	}
    
    /**
    *   convolution with a separable kernel (the kernel is 3x{kx,ky,kz})
     */
    private float[] separableConvolution(float[] image, int nx, int ny, float[][] kernel, int kx, int ky) {
        float[] result = new float[nx*ny];
        float[] temp = new float[nx*ny];
        
        //MipavUtil.displayMessage("kernel size: "+kx+"x"+ky+"x"\n");
        
        for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) {
            int xy = x+nx*y;
            temp[xy] = 0.0f; 
            for (int i=-kx;i<=kx;i++) {
                if ( (x+i>=0) && (x+i<nx) ) {
                    temp[xy] += image[xy+i]*kernel[0][kx+i];
                }
            }
        }
        for (int x=0;x<nx;x++) for (int y=0;y<ny;y++){
            int xy = x+nx*y;
            result[xy] = 0.0f;   
            for (int i=-ky;i<=ky;i++) {
                if ( (y+i>=0) && (y+i<ny) ) {
                    result[xy] += temp[xy+i*nx]*kernel[1][ky+i];
                }
            }
        }
        
        temp = null;

        return result;
    }

		/**
	*	convolution with a separable kernel (the kernel is 3x{kx,ky,kz})
	 */
	private float[] separableConvolution(float[] image, int nx, int ny, int nz, float[][] kernel, int kx, int ky, int kz) {
		float[] result = new float[nx*ny*nz];
		float[] temp = new float[nx*ny*nz];
		
		//MipavUtil.displayMessage("kernel size: "+kx+"x"+ky+"x"+kz+"\n");
		
		for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
			int xyz = x+nx*y+nx*ny*z;
			result[xyz] = 0.0f;	
			for (int i=-kx;i<=kx;i++) {
				if ( (x+i>=0) && (x+i<nx) ) {
					result[xyz] += image[xyz+i]*kernel[0][kx+i];
				}
			}
		}
		for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
			int xyz = x+nx*y+nx*ny*z;
			temp[xyz] = 0.0f;	
			for (int i=-ky;i<=ky;i++) {
				if ( (y+i>=0) && (y+i<ny) ) {
					temp[xyz] += result[xyz+i*nx]*kernel[1][ky+i];
				}
			}
		}
		for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
			int xyz = x+nx*y+nx*ny*z;
			result[xyz] = 0.0f;	
			for (int i=-kz;i<=kz;i++) {
				if ( (z+i>=0) && (z+i<nz) ) {
					result[xyz] += temp[xyz+i*nx*ny]*kernel[2][kz+i];
				}
			}
		}
		temp = null;

		return result;
	}
		

}
