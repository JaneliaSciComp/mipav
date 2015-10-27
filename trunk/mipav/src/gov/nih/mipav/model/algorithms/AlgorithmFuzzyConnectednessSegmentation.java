package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class AlgorithmFuzzyConnectednessSegmentation extends AlgorithmBase {
	
	//~ Static fields/initializers -------------------------------------------------------------------------------------
	
    //~ Instance fields ------------------------------------------------------------------------------------------------
	private int L1Distance; // L1 distance of neighborhood
	
	private double distanceDecline; // distance decline factor
	
	private double gradientWeight;
	
	// The index in the image array of the seed
    private Vector<Integer> index_seeds;
    
    private Vector<Short> index_labels;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

	
	public AlgorithmFuzzyConnectednessSegmentation(ModelImage destImg, ModelImage srcImg, int L1Distance, double distanceDecline,
			double gradientWeight, Vector<Integer> index_seeds, Vector<Short> index_labels) {
		super(destImg, srcImg);
		this.L1Distance = L1Distance;
		this.distanceDecline = distanceDecline;
		this.gradientWeight = gradientWeight;
		this.index_seeds = index_seeds;
		this.index_labels = index_labels;
	}
	
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
    	int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x, y;
        int xa, ya;
        Vector<Integer>index1 = new Vector<Integer>();
        Vector<Integer>index2 = new Vector<Integer>();
        Vector<Double>affinity = new Vector<Double>();
        int yDist;
        int xDist;
        double adjacency;
        double aff;
        double f[] = new double[sliceSize];
        int p1;
        int p2;
        short fc;
        int i;
        int j;
        Vector<Integer>idx = new Vector<Integer>();
        Vector<Integer>index_seeds_previous = new Vector<Integer>();
        Vector<Short>index_labels_previous = new Vector<Short>();
        double maxK;
        double fp[] = new double[sliceSize];
        
        fireProgressStateChanged("Graph Based Segmentation ...");
        
        try {
        	srcImage.exportData(0, sliceSize, f);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceSize, f)");
        	setCompleted(false);
        	return;
        }
        
        // Compute adjacency according to (2.8) in Udupa '96.
        // Compute affinity according to (2.9) in Udupa '96.
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		p1 = x + y * xDim;
        	    for (ya = Math.max(0, y - L1Distance); ya <= Math.min(yDim-1, y + L1Distance); ya++) {
        	        yDist = Math.abs(y - ya);
        	        xDist = L1Distance - yDist;
        	        for (xa = Math.max(0, x - xDist); xa <= Math.min(xDim-1, x + xDist); xa++) {
        	        	p2 = xa + ya * xDim;
        	            adjacency = 1.0/(1.0 + distanceDecline*Math.sqrt(yDist*yDist + xDist*xDist));
        	            aff = adjacency/(1.0 + gradientWeight*Math.abs(f[p1] -f[p2]));
        	            index1.add(p1);
        	            index2.add(p2);
        	            affinity.add(aff);
        	        }
        	    }
        	}
        }
        
        while(!index_seeds.isEmpty()) {
        	index_labels_previous.clear();
        	index_labels_previous = (Vector<Short>) index_labels.clone();
        	index_seeds_previous.clear();
        	index_seeds_previous = (Vector<Integer>) index_seeds.clone();
            fc = index_labels.get(0);
            for (i = 1; i < index_labels.size(); i++) {
            	if (index_labels.get(i) > fc) {
            		fc = index_labels.get(i);
            	} 
            } // for (i = 1; i < index_labels.size(); i++)
            for (i = 0; i < index_labels.size(); i++) {
            	if (fc == index_labels.get(i)) {
            		idx.add(index_seeds.get(i));
            	}
            }
            for (i = index_labels.size()-1; i >= 0; i--) {
            	if (index_labels.get(i) == fc) {
            		index_labels.remove(i);
            		index_seeds.remove(i);
            	}
            }
            // Propagate affinity one layer
            j = 0;
            for (i = 0; i < sliceSize; i++) {
            	maxK = -Double.MAX_VALUE;
            	while (index1.get(j) == i) {
            		maxK = Math.max(affinity.get(j), maxK);
            		j++;
            	}
            	fp[i] = Math.min(fc, maxK);
            }
            for (i = 0; i < sliceSize; i++) {
            	j = 0;
                 while( (index_seeds_previous.get(j) != i) && (j < index_seeds_previous.size())) {
                	 j++;
                 }
                 if (j < index_seeds_previous.size()) {
                	 
                 }
            }
        } //  while(!index_seeds.isEmpty())
        
        setCompleted(true);
        return;
    }
}