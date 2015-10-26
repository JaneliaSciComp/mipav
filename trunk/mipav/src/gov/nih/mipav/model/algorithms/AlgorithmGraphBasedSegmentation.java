package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class AlgorithmGraphBasedSegmentation extends AlgorithmBase {
	
	/**
	 * The C++ source code for the port was  downloaded from Pedro F. Felzenswalb's web page 
	 * Graph Based Image Segmentation at http://cs.brown.edu/~pff/segment/
	 * 
	 * This code is ported with the permission of Pedro F. Felzenszwalb
	 * Reference: Pedro F. Felzenszwalb and Daniel P. Huttenlocher, 
	 * International Journal of Computer Vision, 59(2), September, 2004.
	 * 
	 * Port performed by William Gandler.
	 * 
	 * This program takes a color image and produces a segmentation
	 * with a random color assigned to each region
	 * 
	 * Typical parameters are: sigma = 0.5, k = 500, minSize = 20.
	 * Larger values for k result in larger components in the result.
	 */

    //~ Static fields/initializers -------------------------------------------------------------------------------------
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private float sigma;  // Used to smooth the input image before segmenting it
	
	private float k;  // Value for the threshold function
	
	private int minSize; // Minimum component size enforced by post-processing
	
	private int xDim;
	
	ViewUserInterface UI = ViewUserInterface.getReference();
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs graph based segmentation algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  sigma    Used to smooth the input image before segmenting it
     * @param  k        Value for the threshold function
     * @param  minSize  Minimum component size enforced by post-processing
     
     */
    public AlgorithmGraphBasedSegmentation(ModelImage destImg, ModelImage srcImg, float sigma,
    		float k, int minSize) {

        super(destImg, srcImg);
        this.sigma = sigma;
        this.k = k;
        this.minSize = minSize;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;

        
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int num_ccs; // Number of connected components in the segmentation
        ModelImage gaussianImage;
        AlgorithmGaussianBlur gaussianAlgo;
        boolean entireImage = true;
        boolean image25D = true;
        float smooth_r[];
        float smooth_g[];
        float smooth_b[];
        ArrayList<edge> edges = null;
        int i;
        int x;
        int y;
        int num;
        universe u;
        int index;
        int comp;
        
        fireProgressStateChanged("Graph Based Segmentation ...");
        
        float[] sigmas = new float[2];
		sigmas[0] = sigma;
		sigmas[1] = sigma;
		
		gaussianImage = (ModelImage) srcImage.clone();
        gaussianImage.setType(ModelStorageBase.ARGB_FLOAT);
        gaussianImage.reallocate(ModelStorageBase.ARGB_FLOAT);
		gaussianAlgo = new AlgorithmGaussianBlur(gaussianImage, srcImage, sigmas, entireImage, image25D);
		gaussianAlgo.setRed(true);
		gaussianAlgo.setGreen(true);
		gaussianAlgo.setBlue(true);

		gaussianAlgo.run();
		
		smooth_r = new float[sliceSize];
		smooth_g = new float[sliceSize];
		smooth_b = new float[sliceSize];
		
		try {
			gaussianImage.exportRGBData(1, 0, sliceSize, smooth_r);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on gaussian.exportData(1, 0, sliceSize, smooth_r");
			gaussianImage.disposeLocal();
			setCompleted(false);
			return;
		}
		
		try {
			gaussianImage.exportRGBData(2, 0, sliceSize, smooth_g);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on gaussian.exportData(2, 0, sliceSize, smooth_g");
			gaussianImage.disposeLocal();
			setCompleted(false);
			return;
		}
		
		try {
			gaussianImage.exportRGBData(3, 0, sliceSize, smooth_b);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException " + e + " on gaussian.exportData(3, 0, sliceSize, smooth_b");
			gaussianImage.disposeLocal();
			setCompleted(false);
			return;
		}
		
		gaussianImage.disposeLocal();
		gaussianImage = null;
		
		// Build graph
		edges = new ArrayList<edge>();
		
		
		num = 0;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
			    if (x < xDim-1) {
			    	edges.add(new edge());
			        edges.get(num).seta(y * xDim + x);
			        edges.get(num).setb(y * xDim + (x+1));
			        edges.get(num).setw(diff(smooth_r, smooth_g, smooth_b, x, y, x+1, y));
			        num++;
			    }
			    
			    if (y < yDim-1) {
			    	edges.add(new edge());
			    	edges.get(num).seta(y * xDim + x);
			    	edges.get(num).setb((y+1) * xDim + x);
			    	edges.get(num).setw(diff(smooth_r, smooth_g, smooth_b, x, y, x, y+1));
			    	num++;
			    }
			    
			    if ((x < xDim-1) && (y < yDim-1)) {
			    	edges.add(new edge());
			    	edges.get(num).seta(y * xDim + x);
			    	edges.get(num).setb((y+1) * xDim + (x+1));
			    	edges.get(num).setw(diff(smooth_r, smooth_g, smooth_b, x, y, x+1, y+1));
			    	num++;
			    }
			    
			    if ((x < xDim-1) && (y > 0)) {
			    	edges.add(new edge());
			    	edges.get(num).seta(y * xDim + x);
			    	edges.get(num).setb((y-1) * xDim + (x+1));
			    	edges.get(num).setw(diff(smooth_r, smooth_g, smooth_b, x, y, x+1, y-1));
			    	num++;
			    }
			} // for (x = 0; x < xDim; x++)
		} // for (y = 0; y < yDim; y++)
		
		smooth_r = null;
		smooth_g = null;
		smooth_b = null;
		
		// Segment
		u = segment_graph(sliceSize, num, edges, k);                                                                                                      
		
		// post process small components
	    for (i = 0; i < num; i++) {
	      int a = u.find(edges.get(i).geta());
	      int b = u.find(edges.get(i).getb());
	      if ((a != b) && ((u.getelts(a).getsize() < minSize) || (u.getelts(b).getsize() < minSize)))
	        u.join(a, b);
	    }
	    edges = null;
	    num_ccs = u.getnum();
	    
	    // Pick random colors for each component
	    short red[] = new short[sliceSize];
	    short green[] = new short[sliceSize];
	    short blue[] = new short[sliceSize];
	    RandomNumberGen randomGen = new RandomNumberGen();
	    for (i = 0; i < sliceSize; i++) {
	    	red[i] = (short)(randomGen.genUniformRandomNum(0, 255));
	    	green[i] = (short)(randomGen.genUniformRandomNum(0, 255));
	    	blue[i] = (short)(randomGen.genUniformRandomNum(0, 255));
	    }
	    
	    short destRed[] = new short[sliceSize];
	    short destGreen[] = new short[sliceSize];
	    short destBlue[] = new short[sliceSize];
	    
	    
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		index = x + y * xDim;
	    		comp = u.find(index);
	    		destRed[index] = red[comp];
	    		destGreen[index] = green[comp];
	    		destBlue[index] = blue[comp];
	    	}
	    }
	    
	    red = null;
	    green = null;
	    blue = null;
        u.delete();
        u = null;
        
        try {
        	destImage.importRGBData(1, 0, destRed, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on destImage.importRGBData(1, 0. destRed, true)");
        	setCompleted(false);
        	return;
        }
        
        try {
        	destImage.importRGBData(2, 0, destGreen, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on destImage.importRGBData(2, 0. destGreen, true)");
        	setCompleted(false);
        	return;
        }
        
        try {
        	destImage.importRGBData(3, 0, destBlue, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on destImage.importRGBData(3, 0. destBlue, true)");
        	setCompleted(false);
        	return;
        }
        
        UI.setDataText("Number of connected components in the segmentation = " + num_ccs + "\n");
        
        setCompleted(true);
        return;
    	
    }
    
    /*
     * Segment a graph
     *
     * Returns a disjoint-set forest representing the segmentation.
     *
     * num_vertices: number of vertices in graph.
     * num_edges: number of edges in graph
     * edges: array of edges.
     * k: constant for threshold function.
     */
    universe segment_graph(int num_vertices, int num_edges, ArrayList<edge> edges, 
    			float k) { 
      // sort edges by weight
      Collections.sort(edges, new edgeComparator());

      // make a disjoint-set forest
      universe u = new universe(num_vertices);

      // init thresholds
      float threshold[] = new float[num_vertices];
      for (int i = 0; i < num_vertices; i++)
        threshold[i] = k;

      // for each edge, in non-decreasing weight order...
      for (int i = 0; i < num_edges; i++) {
        edge pedge = edges.get(i);
        
        // components connected by this edge
        int a = u.find(pedge.geta());
        int b = u.find(pedge.getb());
        if (a != b) {
          if ((pedge.getw() <= threshold[a]) &&
    	  (pedge.getw() <= threshold[b])) {
    	u.join(a, b);
    	a = u.find(a);
    	threshold[a] = pedge.getw() + k/u.getelts(a).getsize();
          }
        }
      }

      // free up
      threshold = null;
      return u;
    }
       
    private float diff(float r[], float g[], float b[], int x1, int y1, int x2, int y2) {
    	double rdiff = r[x1 + y1 * xDim] - r[x2 + y2 * xDim];
    	double gdiff = g[x1 + y1 * xDim] - g[x2 + y2 * xDim];
    	double bdiff = b[x1 + y1 * xDim] - b[x2 + y2 * xDim];
    	return (float)Math.sqrt(rdiff*rdiff + gdiff*gdiff + bdiff*bdiff);
    }
    
    class edge {
    	  private float w;
    	  private int a, b;
    	  
    	  public void seta(int a) {
    		  this.a = a;
    	  }
    	  
    	  public int geta() {
    		  return a;
    	  }
    	  
    	  public void setb(int b) {
    		  this.b = b;
    	  }
    	  
    	  public int getb() {
    		  return b;
    	  }
    	  
    	  public void setw(float w) {
    		  this.w = w;
    	  }
    	  
    	  public float getw() {
    		  return w;
    	  }
    	};
    	
    	 private class edgeComparator implements Comparator<edge> {

    	        /**
    	         * DOCUMENT ME!
    	         * 
    	         * @param o1 DOCUMENT ME!
    	         * @param o2 DOCUMENT ME!
    	         * 
    	         * @return DOCUMENT ME!
    	         */
    		 
    		 public int compare(final edge o1, final edge o2) {
    	            final float a = o1.getw();
    	            final float b = o2.getw();

    	            if (a < b) {
    	                return -1;
    	            } else if (a > b) {
    	                return 1;
    	            } else {
    	                return 0;
    	            }
    	        }
    	 }
    	
    class uni_elt {
    	private int rank;
    	private int p;
    	private int size;
    	
    	public int getsize() {
    		return size;
    	}
    }
    
    class universe {
    	private uni_elt elts[];
    	private int num;
    	
    	public uni_elt getelts(int index) {
    		return elts[index];
    	}
    	
    	public int getnum() {
    		return num;
    	}
    	
    	public universe(int elements) {
    		elts = new uni_elt[elements];
    		for (int i = 0; i < elements; i++) {
    			elts[i] = new uni_elt();
    		}
    		num = elements;
    		for (int i = 0; i < elements; i++) {
    			elts[i].rank = 0;
    			elts[i].size = 1;
    			elts[i].p = i;
    		}
    	}
    	
    	public int find(int x) {
    	    int y = x;
    	    while (y != elts[y].p)
    	    	y = elts[y].p;
    	    elts[x].p = y;
    	    return y;
    	}
    	
    	public void join(int x, int y) {
    		  if (elts[x].rank > elts[y].rank) {
    		    elts[y].p = x;
    		    elts[x].size += elts[y].size;
    		  } else {
    		    elts[x].p = y;
    		    elts[y].size += elts[x].size;
    		    if (elts[x].rank == elts[y].rank)
    		      elts[y].rank++;
    		  }
    		  num--;
    		}
    	
    	public void delete() {
    		for (int i = 0; i < elts.length; i++) {
    			elts[i] = null;
    		}
    		elts = null;
    	}
    }

}