package gov.nih.mipav.model.algorithms;

import java.awt.Color;
import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**VLFeat is distributed under the BSD license:
Copyright (C) 2007-11, Andrea Vedaldi and Brian Fulkerson
Copyright (C) 2012-13, The VLFeat Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** 
 * 
 * This is ported from files by Andrea Vedaldi and Brian Fulkerson Copyright 2007-2013.
 * These files are part of the VLFeat library and are made available under the terms
 * of the BSD license.
 *
 */

  /**
   * MSER computes the Maximally Stable extremal regions.  In the simplest case, MSER reads an image file,
   * computes the MSERs, and writes them to a file of region seeds.  Alternatively, the frames option can
   * be used to compute elliptical frames instead of region seeds.  It is also possible to use frames and
   * seeds in combination to save both seeds and frames.
   * 
   * region/seed is an index pointing to one of the image pixels (pixels are enumerated in row major order, with
   * 0 corresponding to the upper-left corner of the image).  A MSER containing a given seed is the largest connect
   * subset of pixels whose value is not smaller than the seed value.
   * 
   * frame is an ellipse approximating a MSER and is specified by 5 floating point numbers: coordinate of the center
   * x, y, and elements S11, S12, S22 of the co-variance matrix.
   * 
   * Reference: J. Matas, O. Chum, M. Urban, and T. Pajdla.  Robust wide baseline stereo form maximally stable
   * extremal regions.  In "Proc. BMVC", 2002.
   *
   */


public class AlgorithmMSER extends AlgorithmBase {
	
	private final static int VL_MSER_PIX_MAXVAL = 256;
	
	private final static int VL_MSER_VOID_NODE = Integer.MAX_VALUE;
	
	private final static int POINTS_ONLY = 1;
	private final static int ELLIPSES_ONLY = 2;
	private final static int POINTS_AND_ELLIPSES = 3;
	
	// Must be a non-negative number
	private double delta;
	
	private double epsilon;
	
	// Keep or remove duplicates
	private boolean duplicates;
	
	// Maximum region (relative) area.  Must be in the [0,1] range.
	private double max_area;
	
	// Minimum region (relative) area.  Must be in the [0,1] range.
	private double min_area;
	
	// Maximum absolute region stability.  Must be a non-negative number.
	private double max_variation;
	
	// Must be in the [0,1] range.
	private double min_diversity;
	
	// Enable or disable bright_on_dark regions.  bright_on_dark must be 0 or 1.  Default 1.
	private int bright_on_dark = 1;
	
	// Enable or disable dark_on_bright regions.  dark_on_bright must be 0 or 1.  Default 1.
	private int dark_on_bright = 1;
	
	private int outputVOIType;
	
	private int exit_code = 0;
	
	private int ndims = 2;
	
	private int dims[] = new int[ndims];
	
	private short data[];
	
	private int sliceSize;
	
	public AlgorithmMSER(ModelImage srcImage, double delta, double epsilon, boolean duplicates,
			double max_area, double min_area, double max_variation, double min_diversity, int bright_on_dark, int dark_on_bright,
			int outputVOIType) {
	    super(null, srcImage);	
	    this.delta = delta;
	    this.epsilon = epsilon;
	    this.duplicates = duplicates;
	    this.max_area = max_area;
	    this.min_area = min_area;
	    this.max_variation = max_variation;
	    this.min_diversity = min_diversity;
	    this.bright_on_dark = bright_on_dark;
	    this.dark_on_bright = dark_on_bright;
	    this.outputVOIType = outputVOIType;
	}
	
	
	/**
     * Start algorithm.
     */
    public void runAlgorithm() {
    	VlMserFilt filt;
    	VlMserFilt filtinv;
    	int regions[];
    	int regionsinv[];
    	double frames[];
    	double framesinv[];
    	int nregions = 0;
    	int nregionsinv = 0;
    	int nframes = 0;
    	int nframesinv = 0;
    	int i, j, dof;
    	VOI newPtVOI;
    	VOI newEllipseVOI;
    	int xArr[] = new int[1];
    	int yArr[] = new int[1];
    	int zArr[] = new int[1];
    	float xArrFloat[];
    	float yArrFloat[];
    	float zArrFloat[];
    	int numVOIPoint1 = 0;
    	int numVOIEllipse1 = 0;
    	double centerX;
		double centerY;
		double S11;
		double S12;
		double S22;
		double diff;
		double root;
		double sum;
		double eigenValueLarge;
		double eigenValueSmall;
		double semiMajorAxis;
		double semiMinorAxis;
		double phi; // Angle of major axis with x axis
		double cosphi;
		double sinphi;
		double alpha;
		double cosalpha;
		double sinalpha;
		float xf;
		float yf;
		int n;

        if (srcImage == null) {
            displayError("MSER: Source Image is null");

            return;
        }
        
        dims[0] = srcImage.getExtents()[0];
        dims[1] = srcImage.getExtents()[1];
        sliceSize = dims[0] * dims[1];
        // The data type of the image pixels has to be an integer.  The maximum
        // integer value is VL_MSER_PIX_MAXVAL = 256.
        data = new short[sliceSize];
        try {
        	srcImage.exportData(0, sliceSize, data);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceSize, data)");
        	setCompleted(false);
        	return;
        }
        
        filt = vl_mser_new(ndims, dims);
        filtinv = vl_mser_new(ndims, dims);
        
        if (delta >= 0) {
        	filt.delta = (short)delta;
        }
        
        if (max_area >= 0) {
        	filt.max_area = max_area;
        }
        
        if (min_area >= 0) {
        	filt.min_area = min_area;
        }
        
        if (max_variation >= 0) {
        	filt.max_variation = max_variation;
        }
        
        if (min_diversity >= 0) {
        	filt.min_diversity = min_diversity;
        }
        
        if (delta >= 0) {
        	filtinv.delta = (short)delta;
        }
        
        if (max_area >= 0) {
        	filtinv.max_area = max_area;
        }
        
        if (min_area >= 0) {
        	filtinv.min_area = min_area;
        }
        
        if (max_variation >= 0) {
        	filtinv.max_variation = max_variation;
        }
        
        if (min_diversity >= 0) {
        	filtinv.min_diversity = min_diversity;
        }
        
        Preferences.debug("MSER: parameters:\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("MSER: delta = " + filt.delta + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("MSER: max_area = " + filt.max_area + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("MSER: min_area = " + filt.min_area + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("MSER: max_variation = " + filt.max_variation + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("MSER: min_diversity = " + filt.min_diversity + "\n", Preferences.DEBUG_ALGORITHM);
        
        if (dark_on_bright != 0) {
        	vl_mser_process(filt, data);
        	
        	// Save result
        	nregions = filt.nmer;
        	regions = filt.mer;
        	
        	if ((outputVOIType == POINTS_ONLY) || (outputVOIType == POINTS_AND_ELLIPSES)) {
        	    for (i = 0; i <  nregions; ++i) {
        	    	newPtVOI = new VOI((short) (i), String.valueOf(i), VOI.POINT, -1.0f);
                    newPtVOI.setColor(Color.RED);
                    xArr[0] = regions[i] % dims[0];
                    yArr[0] = regions[i] / dims[0];
                    zArr[0] = 0;
                    newPtVOI.importCurve(xArr, yArr, zArr);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                    ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(String.valueOf(i));
                    srcImage.registerVOI(newPtVOI);	
        	    } // for (i = 0; i <  nregions; ++i)
        	    numVOIPoint1 = nregions;
        	} // if ((outputVOIType == POINTS_ONLY) || (outputVOIType == POINTS_AND_ELLIPSES))
        	
        	if ((outputVOIType == ELLIPSES_ONLY) || (outputVOIType == POINTS_AND_ELLIPSES)) {
        		vl_mser_ell_fit(filt);
        		nframes = filt.nell;
        		dof = filt.dof;
        		frames = filt.ell;
        		for (i = 0; i < nframes; i++) {
        			centerX = frames[i*5];
        			centerY = frames[i*5+1];
        			S11 = frames[i*5+2];
        			S12 = frames[i*5+3];
        			S22 = frames[i*5+4];
        			diff = S11 - S22;
        			root = Math.sqrt(diff*diff + 4.0*S12*S12);
        			sum = S11 + S22;
        			eigenValueLarge = (sum + root)/2.0;
        			eigenValueSmall = (sum - root)/2.0;
        			semiMajorAxis = Math.sqrt(eigenValueLarge);
        			semiMinorAxis = Math.sqrt(eigenValueSmall);
        			xArrFloat = new float[720];
    			    yArrFloat = new float[720];
    			    zArrFloat = new float[720];
    			    newEllipseVOI = new VOI((short)(numVOIPoint1 + i), String.valueOf(i), VOI.CONTOUR, -1.0f);
    			    newEllipseVOI.setColor(Color.RED);
        			if ((S11 == S22) && (S12 == 0.0)) {
        				// Ellipse is a circle
        				n = 0;
        			    for (j = 0; j < 720; j++) {
                            alpha = j * Math.PI/360.0;
                            cosalpha = Math.cos(alpha);
                            sinalpha = Math.sin(alpha);
                            xf = (float)(centerX + semiMajorAxis * cosalpha);
                            if ((xf >= 0) && (xf <= dims[0]-1)) {
                                yf = (float)(centerY + semiMajorAxis * sinalpha);
                                if ((yf >= 0) && (yf <= dims[1]-1)) {
                                    xArrFloat[n] = xf;
                                    yArrFloat[n++] = yf;
                                }
                            }
                        }
                        newEllipseVOI.importCurve(xArrFloat, yArrFloat, zArrFloat);
                        ((VOIContour)(newEllipseVOI.getCurves().elementAt(0))).setFixed(true);
                        srcImage.registerVOI(newEllipseVOI);
        			}
        			else  {
        			    if (S11 == S22) {
        			        phi = Math.PI/4.0;	
        			    }
        			    else {
        			    	phi = Math.atan2(2*S12, S11 - S22)/2.0;
        			    }
        			    cosphi = Math.cos(phi);
        			    sinphi = Math.sin(phi);
        			    
        			    n = 0;
        			    for (j = 0; j < 720; j++) {
                            alpha = j * Math.PI/360.0;
                            cosalpha = Math.cos(alpha);
                            sinalpha = Math.sin(alpha);
                            xf = (float)(centerX + semiMajorAxis * cosalpha * cosphi - semiMinorAxis * sinalpha * sinphi);
                            if ((xf >= 0) && (xf <= dims[0]-1)) {
                                yf = (float)(centerY + semiMajorAxis * cosalpha * sinphi + semiMinorAxis * sinalpha * cosphi);
                                if ((yf >= 0) && (yf <= dims[1]-1)) {
                                    xArrFloat[n] = xf;
                                    yArrFloat[n++] = yf;
                                }
                            }
                        }
                        newEllipseVOI.importCurve(xArrFloat, yArrFloat, zArrFloat);
                        ((VOIContour)(newEllipseVOI.getCurves().elementAt(0))).setFixed(true);
                        srcImage.registerVOI(newEllipseVOI);
        			}
        		} // for (i = 0; i < nframes; i++) 
        		numVOIEllipse1 = nframes;
        	} // if ((outputVOIType == ELLIPSES_ONLY) || (outputVOIType == POINTS_AND_ELLIPSES))
        } // if (dark_on_bright != 0)
    }
    
    private void vl_mser_ell_fit(VlMserFilt f) {
        // shortcuts
    	int nel = f.nel;
    	int dof = f.dof;
    	int dims[] = f.dims;
    	int ndims = f.ndims;
    	int subs[] = f.subs;
    	int njoins = f.njoins;
    	int joins[] = f.joins;
    	VlMserReg r[] = f.r;
    	int mer[] = f.mer;
    	int nmer = f.nmer;
    	double acc[] = f.acc;
    	double ell[] = f.ell;
    	
    	int d, index, i, j;
    	
    	// Already fit?
    	if (f.nell == f.nmer) {
    		return;
    	}
    	
    	// Make room
    	if (f.rell < f.nmer) {
    		if (f.ell != null) {
    			f.ell = null;
    		}
    		f.ell = new double[f.nmer * f.dof];
    		f.rell = f.nmer;
    	} // if (f.rell < f.nmer)
    	
    	if (f.acc == null) {
    		f.acc = new double[f.nel];
    	}
    	
    	acc = f.acc;
    	ell = f.ell;
    	
    	// Integrate moments
    	// For each dof
    	for (d = 0; d < f.dof; ++d) {
    	    
    		// Start from the upper-left pixel(0,0,,,0)
    		subs = new int[ndims];
    		
    		// Step 1: Fill acc pretending each region has only one pixel
    		if (d < ndims) {
    		    // 1 -order
    			
    		    for (index = 0; index < nel; ++index) {
    		        acc[index] = subs[d];
    		        adv(ndims, dims, subs);
    		    }
    		} // if (d < ndims)
    		else {
    			// 2-order
    			// Map the dof d to a second order moment E[x_i x_j]
    			i = d - ndims;
    			j = 0;
    			while (i > j) {
    				i -= j + 1;
    				j++;
    			}
    			// Initialize acc with x_i * x_j
    			for (index = 0; index < nel; ++index) {
    				acc[index] = subs[i] *subs[j];
    				adv(ndims, dims, subs);
    			}
    		} // else
    		
    		// Step 2: integrate
    		for (i = 0; i < njoins; ++i) {
    			index = joins[i];
    			int parent = r[index].parent;
    			acc[parent] += acc[index];
    		}
    		
    		// Step 3: Save back to ellipse
    		for (i = 0; i < nmer; ++i) {
    			int idx = mer[i];
    			ell[d + dof*i] = acc[idx];
    		}
    	} // for (d = 0; d < f.dof; ++d) next dof
    	
    	// Compute central moments
    	for (index = 0; index < nmer; ++index) {
    	    // float *pt = ell + index * dof;
    		int idx = mer[index];
    		double area = r[idx].area;
    		
    		for (d = 0; d < dof; ++d) {
    		    ell[index * dof + d] /= area;
    		    
    		    if (d >= ndims) {
    		    	// Remove squared mean from moment to get variance
    		    	i = d - ndims;
    		    	j = 0;
    		    	while (i > j) {
    		    		i -= j+1;
    		    		j++;
    		    	}
    		    	ell[index * dof + d] -= ell[index * dof + i] * ell[index * dof + j];
    		    } // if (d >= ndims)
    		} // for (d = 0; d < dof; ++d)
    	} // for (index = 0; index < nmer; ++index)
    	
    	// Save back
    	f.nell = nmer;
    }
    
    /**
     * Advance N-dimensional subscript
     * @param ndims
     * @param dims
     * @param subs subscript to advance
     */
    private void adv(int ndims, int dims[], int subs[]) {
    	int d = 0;
    	while (d < ndims) {
    		if (++subs[d] < dims[d]) {
    			return;
    		}
    		subs[d++] = 0;
    	}
    }
    
    private void vl_mser_process(VlMserFilt f, final short im[]) {
        // shortcuts
    	int nel = f.nel;
    	int perm[] = f.perm;
    	int joins[] = f.joins;
    	int ndims = f.ndims;
    	int dims[] = f.dims;
    	int subs[] = f.subs;
    	int dsubs[] = f.dsubs;
    	int strides[] = f.strides;
    	VlMserReg r[] = f.r;
    	VlMserExtrReg er[] = f.er;
    	int mer[]= f.mer;
    	int delta = f.delta;
    	
    	int njoins = 0;
    	int ner = 0;
    	int nmer = 0;
    	int nbig = 0;
    	int nsmall = 0;
    	int nbad = 0;
    	int ndup = 0;
    	
    	int i, j, k;
    	
    	// Delete any previously computed ellipsoid
    	f.nell = 0;
    	
    	// Sort pixels by intensity
    	{
	    	int buckets[] = new int[VL_MSER_PIX_MAXVAL];
	    	
	    	// Compute bucket size (how many pixels for each intensity value)
	    	for (i = 0; i < nel; ++i) {
	    		++buckets[im[i]];
	    	}
	    	
	    	// Cumulatively add bucket sizes
	    	for (i = 1; i < VL_MSER_PIX_MAXVAL; ++i) {
	    		buckets[i] += buckets[i-1];
	    	}
	    	
	    	// Empty buckets computing pixel ordering
	    	for (i = nel; i >= 1; ) {
	    		short v = im[--i];
	    		j = --buckets[v];
	    		perm[j] = i;
	    	}
    	}
	    	
    	// Initialize the forest with all void nodes
    	for (i = 0; i < nel; ++i) {
    		r[i].parent = VL_MSER_VOID_NODE;
    	}
    	
    	// Compute regions and count extremal regions
    	
    	// In the following:
    	
    	// idx: index of the current pixel
    	// val: intensity of the current pixel
    	// r_idx: index of the root of the current pixel
    	// n_idx: index of the neighbors of the current pixel
    	// nr_idex index of the root of the neighbor of the current pixel
    	
    	// Process each pixel by increasing intensity
    	for (i = 0; i < nel; ++i) {
    		// Pop next node xi
    		int idx = perm[i];
    		short val = im[idx];
    		int r_idx;
    		
    		// Add the pixel to the forest as a root for now
    		r[idx].parent = idx;
    		r[idx].shortcut = idx;
    		r[idx].area = 1;
    		r[idx].height = 1;
    		
    		r_idx = idx;
    		
    		// Convert the index idx into the subscript subs; also initialize
    		// dsubs to (-1, -1, ..., -1)
    		{
    			int temp = idx;
    			for (k = ndims - 1; k >= 0; --k) {
    				dsubs[k] = -1;
    				subs[k] = temp/strides[k];
    				temp = temp % strides[k];
    			} 
    		}
    		
    		//Examine the neighbors of the current pixel
    		done_all_neighbors: while (true) {
    			int n_idx = 0;
    			boolean good = true;
    			
    			// Compute the neighbor subscript as nsubs+sub, the corresponding neighborindex nindex and
    			// check that the neighbor is within the image domain.
    			
    			for (k = 0; k < ndims && good; ++k) {
    				int temp = dsubs[k] + subs[k];
    				good &= (0 <= temp) && (temp < dims[k]);
    				n_idx += temp * strides[k];
    			}
    			
    			// The neighbor should be processed if the following conditions are met:
    			// 1. The neighbor is within image boundaries.
    			// 2. The neighbor is indeed different from the current node
    			//    (the opposite happens when dsub = (0, 0, ..., 0)
    			// 3. The neighbor is already in the forest, meaning that is has already been processed.
    			if (good && n_idx != idx && r[n_idx].parent != VL_MSER_VOID_NODE) {
    			    short nr_val = 0;
    			    int nr_idx = 0;
    			    int hgt = r[r_idx].height;
    			    int n_hgt = r[nr_idx].height;
    			    
    			    // Now we join the two subtrees rooted at
    			    // R_IDX = ROOT(IDX)
    			    // NR_IDX = ROOT(N_IDX).
    			    // Note that R_IDX = ROOT(IDX) might change as we process more neighbors,
    			    // so we keep updating it.
    			    
    			    r_idx = climb(r, idx);
    			    nr_idx = climb(r, n_idx);
    			    
    			    // At this point we have three possibilities:
    			    // (A) ROOT(IDX) == ROOT(NR_IDX).  In this case the two trees have
    			    //     already been joined and we do not do anything.
    			    
    			    // (B) I(ROOT(IDX)) == I(ROOT(NR_IDX)).  In this case the pixel
    			    //     IDX is extending an extremal region with the same
    			    //     intensity value.  Since ROOT(NR_IDX) will NOT be an
    			    //     extremal region of the full image, ROOT(IDX) can be
    			    //     safely added as children of ROOT(NR_IDX) if this
    			    //     reduces the height according to the union rank
    			    //     heuristic.
    			    
    			    // (C) I(ROOT(IDX)) > I(ROOT(NR_IDX)).  In this case the pixel
    			    //     IDX is starting a new extremal region.  Thus ROOT (NR_IDX)
    			    //     will be an extremal region of the final image and the
    			    //     only possibility is to add ROOT(NR_IDX) as children of
    			    //     ROOT(IDX), which becomes parent.
    			    
    			    if (r_idx != nr_idx) { // skip if (A)
    			        nr_val = im[nr_idx];
    			        if (nr_val == val && hgt < n_hgt) {
    			            // ROOT(IDX) becomes the child
    			        	r[r_idx].parent = nr_idx;
    			        	r[r_idx].shortcut = nr_idx;
    			        	r[nr_idx].area += r[r_idx].area;
    			        	r[nr_idx].height = Math.max(n_hgt, hgt+1);
    			        	
    			        	joins[njoins++] = r_idx;
    			        } // if (nr_val == val && hgt < n_hgt)
    			        else{
    			        	// Cases ROOT(IDX) becomes the parent
    			        	r[nr_idx].parent = r_idx;
    			        	r[nr_idx].shortcut = r_idx;
    			        	r[r_idx].area += r[nr_idx].area;
    			        	r[r_idx].height = Math.max(hgt, n_hgt+1);
    			        	
    			        	joins[njoins++] = nr_idx;
    			        	
    			        	// Count if extremal
    			        	if (nr_val != val) {
    			        		++ner;
    			        	}
    			        } // Check b versus c
    			    } // if (r_idx != nr_idx) Check a vs b or c
    			} // if (good && n_idx != idx && r[n_idx].parent != VL_MSER_VOID_NODE) Neighbor done
    			
    			// Move to next neighbor
    			k = 0;
    			while (++dsubs[k] >1) {
    				dsubs[k++] = -1;
    				if (k == ndims) {
    					break done_all_neighbors;
    				}
    			} // while (++dsubs[k] >1)
    		} // done_all_neighbors: while (true)
    	} // for (i = 0; i < nel; ++i) Next pixel
    	
    	// The last root is extremal too.
    	++ner;
    	
    	// Save back
    	f.njoins = njoins;
    	f.stats.num_extremal = ner;
    	
    	// Extract extremal regions
    	// Extremal regions are extracted and stored into the array ER.  The 
    	// structure R is also updated so that .SHORTCUT indexes the
    	// corresponding extremal region if any (otherwise it is set to 
    	// VOID).
    	
    	// Make room
    	if (f.rer < ner) {
    		if (er != null) {
    			er = null;
    		}
    		f.er = er = new VlMserExtrReg[ner];
    		f.rer = ner;
    	} // if (f.rer < ner)
    	
    	// Save back
    	f.nmer = ner;
    	
    	// Count again
    	ner = 0;
    	
    	// Scan all regions Xi
    	for (i = 0; i < nel; ++i) {
    		
    		// Pop next node xi
    		int idx = perm[i];
    		
    		short val = im[idx];
    		int p_idx = r[idx].parent;
    		short p_val = im[p_idx];
    		
    		// Is extremal?
    		boolean is_extr = (p_val > val) || idx == p_idx;
    		
    		if (is_extr) {
    		    
    			// If so, add it.
                er[ner].index = idx;
                er[ner].parent = ner;
                er[ner].value = im[idx];
                er[ner].area = r[idx].area;
                
                // Link this region to this extremal region
                r[idx].shortcut = ner;
                
                // Increase count
                ++ner;
    		} // if (is_extr)
    		else {
    			// Link this region to void
    			r[idx].shortcut = VL_MSER_VOID_NODE;
    		}
    	} // for (i = 0; i < nel; ++i)
    	
    	// Link extremal regions in a tree
    	for (i = 0; i < ner; ++i) {
    		
    		int idx = er[i].index;
    		
    		do {
    			idx = r[idx].parent;
    		} while (r[idx].shortcut == VL_MSER_VOID_NODE);
    		
    		er[i].parent = r[idx].shortcut;
    		er[i].shortcut = i;
    	} // for (i = 0; i < ner; ++i) 
    	
    	// Compute variability of +DELTA branches
    	// For each extremal region xi of value VAL we look for the biggest
    	// parent that has value not greater than VAL+DELTA.  This is dubbed
    	// 'top parent'.
    	
    	for (i = 0; i < ner; ++i) {
    	
    		// Xj is the currentand Xj are the parents
    		int top_val = er[i].value + delta;
    		int top = er[i].shortcut;
    		
    		// Examine all parents
    		while (true) {
    			int next = er[top].parent;
    			int next_val = er[next].value;
    			
    			// Break if:
    			// There is no node above the top or
    			// the next node is above the top value.
    		    if (next == top || next_val > top_val) {
    		    	break;
    		    }
    		    
    		    // So next could be top
    		    top = next;
    		} // while (true)
    		
    		// Calculate branch variation
    		{
    			int area = er[i].area;
    			int area_top = er[top].area;
    			er[i].variation = (double)(area_top - area)/area;
    			er[i].max_stable = 1;
    		}
    		
    		// Optimization: since extremal regions are processed by
    		// increasing intensity, all next extremal regions being processed
    		// have value at least equal to the one of Xi.  If any of them has
    		// parent the parent of Xi (this comprises the parent itself), we
    		// can safely skip most intermediate node along the branch and
    		// skip directly to the top to start our search.
    		{
    			int parent = er[i].parent;
    			int curr = er[parent].shortcut;
    			er[parent].shortcut = Math.max(top, curr);
    		}
    	} // for (i = 0; i < ner; ++i)
    	
    	// Select maximally stable branches
    	
    	nmer = ner;
    	for (i = 0; i < ner; ++i) {
    	    int parent = er[i].parent;
    	    short val = er[i].value;
    	    double var = er[i].variation;
    	    short p_val = er[parent].value;
    	    double p_var = er[parent].variation;
    	    int loser;
    	    
    	    // Notice that R_parent = R_{l+1} only if p_val = val + 1.  If not,
    	    // this and the parent region coincide and there is nothing to do.
    	    if (p_val > val + 1) {
    	    	continue;
    	    }
    	    
    	    // Decide which one to keep and put that in loser
    	    if (var < p_var) {
    	    	loser = parent;
    	    }
    	    else {
    	    	loser = i;
    	    }
    	    
    	    // Make loser NON maximally stable
    	    if (er[loser].max_stable != 0) {
    	    	--nmer;
    	    	er[loser].max_stable = 0;
    	    }
    	} // for (i = 0; i < ner; ++i)
    	
        f.stats.num_unstable = ner - nmer;
        
        // Further filtering
        // It is critical for correct duplicate detection to remove regions
        // from the bottom (smallest one first).
        {
        	double max_area = f.max_area * nel;
        	double min_area = f.min_area * nel;
        	double max_var = f.max_variation;
        	double min_div = f.min_diversity;
        	
        	// Scan all extremal regions (intensity value order)
        	for (i = ner-1; i >= 0; --i) {
        		
        		// Process only maximally stable extremal regions
        		if (er[i].max_stable == 0) {
        			continue;
        		}
        		
        		if (er[i].variation >= max_var) {
        			++nbad;
        			er[i].max_stable = 0;
        			--nmer;
        			continue;
        		} // if (er[i].variation >= max_var)
        		
        		if (er[i].area > max_area) {
        			++nbig;
        			er[i].max_stable = 0;
        			--nmer;
        			continue;
        		} // if (er[i].area > max_area)
        		
        		if (er[i].area < min_area) {
        			++nsmall;
        			er[i].max_stable = 0;
        			--nmer;
        			continue;
        		} // if (er[i].area < min_area)
        		
        		// Remove duplicates
        		if (min_div < 1.0) {
        			int parent = er[i].parent;
        			int area, p_area;
        			double div;
        			
        			// Check all but the root user mser
        			if (parent != i) {
        			
        				// Search for the maximally stable parent region
        				while (er[parent].max_stable == 0) {
        					int next = er[parent].parent;
        					if (next == parent) {
        						break;
        					}
        					parent = next;
        				} // while (er[parent].max_stable == 0)
        				
        				// Compare with the parent region; if the current and parent
        				// regions are too similar, keep only the parent.
        				area = er[i].area;
        				p_area = er[parent].area;
        				div = (double)(p_area - area)/(double)area;
        				
        				if (div < min_div) {
        					++ndup;
        					er[i].max_stable = 0;
                			--nmer;
        				}
        			} // if (parent != i)
        		} // if (min_div < 1.0)
        	} // for (i = ner-1; i >= 0; --i) Check next region
        	
        	f.stats.num_abs_unstable = nbad;
        	f.stats.num_too_big = nbig;
        	f.stats.num_too_small = nsmall;
        	f.stats.num_duplicates = ndup;
        }
        
        // Save the result
        
        // Make room
        if (f.rmer < nmer) {
            if (mer != null) {
            	mer = null;
            }
            f.mer = mer = new int[nmer];
            f.rmer = nmer;
        } // if (f.rmer < nmer)
        
        // Save back
        f.nmer = nmer;
        
        j = 0;
        for (i = 0; i < ner; ++i) {
        	if (er[i].max_stable != 0) {
        		mer[j++] = er[i].index;
        	}
        }
    }
    
    
    /**
     * Climb the region forest to reach a root
     * 
     * The function climbs the regions' forest r[] starting from the node idx to the corresponding root.
     * 
     * To speed-up the operation, the function uses the 
     * VlMserReg::shortcut field to quickly jump to the root.  After the root is reached, all the used
     * shortcuts are updated.
     * 
     * @param r regions' forest
     * @param idx starting node
     * @return index of the reached root.
     */
     private int climb(VlMserReg r[], int idx) {
    	int prev_idx = idx;
    	int next_idx;
    	int root_idx;
    	
    	// Move towards root to find it
    	while (true) {
    		// Next jump to the root
    		next_idx = r[idx].shortcut;
    		
    		// Recycle shortcut to remember how we came here
    		r[idx].shortcut = prev_idx;
    		
    		// Stop if root is found
    		if (next_idx == idx) {
    			break;
    		}
    		
    		// Next guy
    		prev_idx = idx;
    		idx = next_idx;
    	}
    	
    	root_idx = idx;
    	
    	// Move backward to update shortcuts
    	while (true) {
    		
    		// Get previously visited one
    		prev_idx = r[idx].shortcut;
    		
    		// Update shortcut to point to the new root
    		r[idx].shortcut = root_idx;
    		
    		// Stop if the first visited node is reached
    		if (prev_idx == idx) {
    			break;
    		}
    		
    		// Next guy
    		idx = prev_idx;
    	}
    	
    	return root_idx;
    }
    
    private VlMserFilt vl_mser_new(int ndims, final int dims[]) {
       int strides[];
       int k;
       VlMserFilt f = new VlMserFilt();	
       f.ndims = ndims;
       f.dims = new int[ndims];
       f.subs = new int[ndims];
       f.dsubs = new int[ndims];
       f.strides = new int[ndims];
       
       // Shortcuts
       strides = f.strides;
       
       // Copy dims to f.dims
       for (k = 0; k < ndims; k++) {
    	   f.dims[k] = dims[k];
       }
       
       // Compute the strides to move into the N-dimensional image array
       strides[0] = 1;
       for (k = 1; k < ndims; ++k) {
    	   strides[k] = strides[k-1] * dims[k-1];
       }
       
       // Total number of pixels
       f.nel = strides[ndims-1] * dims[ndims-1];
       
       // dof of ellipsoids
       f.dof = ndims * (ndims + 1)/2 + ndims;
       
       // More buffers
       f.perm = new int[f.nel];
       f.joins = new int[f.nel];
       f.r = new VlMserReg[f.nel];
       
       f.er = null;
       f.rer = 0;
       f.mer = null;
       f.rmer = 0;
       f.ell = null;
       f.rell = 0;
       
       // Other parameters
       f.delta = 5;
       f.max_area = 0.75;
       f.min_area = 3.0/f.nel;
       f.max_variation = 0.25;
       f.min_diversity = 0.2;
       
       return f;
    }
    
    private class VlMserStats {
    	// Number of extremal regions
    	private int num_extremal;
    	// Number of unstable extremal regions
    	private int num_unstable;
    	// Number of regions that failed the absolute stability test
    	private int num_abs_unstable;
    	// Number of regions that failed the maximum size test
    	private int num_too_big;
    	// Number of regions that failed the minimum size test;
    	private int num_too_small;
    	// Number of regions that failed the duplicate test
    	private int num_duplicates;
    }
    
    /**
     * Extremal regions and maximally stable extremal regions are instances of image regions.
     * 
     * There is an image region for each pixel of the image.  Each regions is represented by an instance of
     * this structure.  Regions are stored into an array in pixel order.
     * 
     * Regions are arranged into a forest.  VlMserReg::parent points to the parent node, or the the node itself
     * if the node is a root.  VlMserReg::parent is the index of the node in the node array (which therefore is
     * also the index of the corresponding pixel).  VlMserReg::height is the distance of the farthest leaf.  If
     * the node itself is a leaf, then VlMserReg::height is zero.
     * 
     * VlMserReg::area is the area of the image region corresponding to this node.
     * 
     * VlMserReg::region is the extremal region identifier.  Not all regions are extremal regions.
     *
     */
    private class VlMserReg {
    	// Points to the parent region
    	private int parent;
    	// Points to a region closer to a root.
    	private int shortcut;
    	// Region height in the forest
    	private int height;
    	// Area of the region
    	private int area;
    }
    
    /**
     * Extremal regions (ER) are extracted from the region forest.  Each region is represented by an instance of
     * this structure.  The structures are stored into an array, in arbitrary order.
     * 
     * ER are arranged into a tree.  parent points to the parent ER, or to itself if the ER is the root.
     * 
     * An instance of the structure represents the extremal region of the level set of intensity VlMserExtrReg::value
     * and containing the pixel VlMserExtReg::index.
     * 
     * VlMserExtrReg::area is the area of the extremal region and VlMserExtrReg::area_top is the area of the extremal
     * region containing this region in the level set of intensity VlMserExtrReg::area + delta
     * 
     * VlMserExtrReg::variation is the relative area variation (area_top-area)/area.
     * 
     * VlMserExtrReg::max_stable is a flag signaling whether this region is also maximally stable.
     *
     */
    private class VlMserExtrReg {
    	// Index of the parent region
    	private int parent;
    	// Index of the pivot pixel
    	private int index;
    	// Value of pivot pixel
    	private short value;
    	// Shortcut used when building a tree
    	private int shortcut;
    	// Area of the region
    	private int area;
    	// Relative area variation
    	private double variation;
    	// Max stable number (=0 if not maxstable)
    	private int max_stable;
    }
    
    private class VlMserFilt {
    	public VlMserFilt() {
    		
    	}
    	
    	// Image data and meta data
    	// Number of dimensions
    	int ndims;
    	// Dimensions
    	int dims[];
    	// Number of image elements (pixels)
    	int nel;
    	// N-dimensional subscript
    	int subs[];
    	// Another subscript
    	int dsubs[];
    	// Strides to move in image data
    	int strides[];
    	// Pixel ordering
    	int perm[];
    	// Sequence of join ops
    	int joins[];
    	// Number of join ops
    	int njoins;
    	
    	// Regions
    	// Basic regions
    	VlMserReg r[];
    	// Extremal tree
    	VlMserExtrReg er[];
    	// Maximally stable extremal regions
    	int mer[];
    	// Number of extremal regions
        int ner;
        // Number of maximally stable extr. regions
        int nmer;
        // Size of er buffer
        int rer;
        // Size of mer buffer
        int rmer;
        
        // Ellipsoids fitting
        // Moment accumulator
        double acc[];
        // Ellipsoids list
        double ell[];
        // Size of the ell buffer
        int rell;
        // Number of ellipsoids extracted
        int nell;
        // Number of dof of ellipsoids
        int dof;
        
        // Configuration
        // Be verbose
        boolean verbose;
        // delta filter parameter
        int delta;
        // Badness test parameter
        double max_area;
        // Badness test parameter
        double min_area;
        // Badness test parameter
        double max_variation;
        // Minimum diversity
        double min_diversity;
        
        // Run statistic
        VlMserStats stats;
    }
}