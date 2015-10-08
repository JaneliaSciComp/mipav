package gov.nih.mipav.model.algorithms;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


/**
 This is a port of C code written by Camille Couprie in 2009.
 Camille Couprie has kindly granted the NIH MIPAV project permission to port her Power watershed code from C to Java
under a BSD license.
Porting performed by William Gandler.

This "powerwatershed" package provides implementation of several segmentation algorithms on 2D or 3D images.
The 3 algorithms are:
1.) Maximum Spanning Forest computed by Kruskal algorithm.
2.) Powerwatersheds (p=infinite, q= 2) : Maximum Spanning Forest computed by Kruskal algorithm and
    Random walker on plateaus.
3.) Maximum Spanning Forest computed by Prim algorithm using red and black trees.

Reference: "Power  watersheds: A new image segmentation framework extending graph cuts, random walker
            and optimal spanning forest" by Camille Couprie, Leo Grady, Laurent Najman, and Hugues Talbot,
            ICCV'09, 2009,
 */

public class AlgorithmPowerWatershed extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    private static int Kruskal = 1;
    
    private static int PW_qis2 = 2;
    
    private static int Prim = 3;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private int algo;
    
    // If true, multi-labels segmentation, else 2-labels segmentation
    private boolean multi;
    
    // The index in the image array of the seed
    private Vector<Integer> index_seeds;
    
    // For 2-labels 1 for white foreground and 2 for black background
    // For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
    private Vector<Short> index_labels;
    
    // Geodesic reconstruction
    private boolean geod;
    
    private boolean error = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs Power watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  algo Kruskal, PW_qis2, or Prim
     * @param  multi If true multi-labels, else two-labels
     * @param  index_seeds The index in the image array of the seed
     * @param  index_labels For 2-labels 1 for white foreground and 2 for black background
     *                      For multi-labels values from 1 to n with n <= 255 for segmentation in n labels
     * @param  geod  Geodesic reconstruction
     */
    public AlgorithmPowerWatershed(ModelImage destImg, ModelImage srcImg, int algo, boolean multi,
    		Vector<Integer> index_seeds, Vector<Short> index_labels, boolean geod) {

        super(destImg, srcImg);
        this.algo = algo;
        this.multi = multi;
        this.index_seeds = index_seeds;
        this.index_labels = index_labels;
        this.geod = geod;
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
        int M; // Number of edges
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        String imageName = srcImage.getImageName();
        int zDim;
        int edges[][];
        int weights[];
        int max_weight;
        boolean quicksort = false;
        int size_seeds = index_seeds.size();
        ModelImage colorImage = null;
        if (srcImage.getNDims() == 2) {
        	zDim = 1;
        }
        else {
        	zDim = srcImage.getExtents()[2];
        }
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }



        fireProgressStateChanged("Power Watershed ...");
        
        M = zDim * xDim * (yDim - 1) + zDim * (xDim - 1) * yDim + (zDim - 1) * xDim * yDim;
        edges = new int[2][M];
        
        compute_edges(edges, xDim, yDim, zDim);
        
        if (srcImage.isColorImage()) {
        	if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        		(srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_FLOAT)) {
        	    colorImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), "ColorImage");
        	    boolean image25D = true;
        	    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(colorImage, srcImage, srcImage.getMin(),
        	    		srcImage.getMax(), 0, 255, image25D);
        	    changeTypeAlgo.run();
          		changeTypeAlgo.finalize();
          		changeTypeAlgo = null;
          		FileInfoBase[] fileInfo = colorImage.getFileInfo();
                fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                fileInfo[0].setExtents(colorImage.getExtents());
                fileInfo[0].setMax(colorImage.getMax());
                fileInfo[0].setMin(colorImage.getMin());
                fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        	} // if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        	else {
        		colorImage = srcImage;
        	}
        } // if (srcImage.isColorImage())
        if (algo == Kruskal) {
            weights = new int[M];
            max_weight = 255;
            if (srcImage.isColorImage()) {
            	max_weight = color_standard_weights(colorImage, weights, edges, index_seeds, size_seeds, geod, quicksort);
            	if (error) {
            		setCompleted(false);
            		return;
            	}
            }
        } // if (algo == Kruskal)
        setCompleted(true);
        return;

        
    } // runAlgorithm
    
     private void compute_edges(  int edges[][], /*array of node indexes composing edges */
    				     int rs, /* rowsize of the image */
    				     int cs, /* colsize of the image */
    				     int ds) /* depth of the image */
    		{
    		  int i,j,k,l,M;
    		  M=0; 
    		  int rs_cs = rs*cs;
    		  for(k=0;k<ds;k++) 
    		    {
    		      for(i=0;i<cs;i++) 
    			{
    			  for(j=0;j<rs;j++)
    			    {
    			      if(i<(cs-1))
    				{
    				  edges[0][M]=j+i*rs+k*rs_cs;
    				  edges[1][M]=j+(i+1)*rs+k*rs_cs;
    				  M++;
    				}
    			    }
    			}
    		      for(i=0;i<cs;i++) 
    			{
    			  for(j=0;j<rs;j++)
    			    {
    			      if(j<(rs-1))
    				{
    				  edges[0][M]=j+i*rs+k*rs_cs;
    				  edges[1][M]=j+1+i*rs+k*rs_cs;
    				  M++;
    				}
    			    }
    			}
    		      if (k != ds-1)
    			for(l=k*rs*cs;l<(k+1)*rs_cs;l++) 
    			  {
    			    edges[0][M]=l;
    			    edges[1][M]=l+rs_cs;
    			    M++;
    			  }
    		    }
    		} // compute_edges;

    private int color_standard_weights(ModelImage image , /* image */
			    int weights[], /* array to store the values of weights on the edges */
			    int edges[][],       /* array of node indexes composing edges */ 
			    Vector<Integer> seeds,        /* vector of seeded nodes indexes */
			    int size_seeds,     /* nb of seeded nodes */
			    boolean geod,          /* if true, geodesic reconstruction is performed */
			    boolean quicksort)     /* true : bucket sort used; false : stochastic sort o(n log n) */
/* ================================================================================================== */
/* Computes weights inversely proportionnal to the image gradient for 2D color (ppm) images */
{
int maxi = 0;
int i,M, xDim, yDim, zDim;
xDim = image.getExtents()[0];
yDim = image.getExtents()[1];
if (image.getNDims() == 2) {
	zDim = 1;
}
else {
	zDim = image.getExtents()[2];
}

int wr, wg, wb;
int imgLength = xDim * yDim;
if (image.getNDims() > 2) {
	imgLength *= zDim;
}

short img_r[] = new short[imgLength];
short img_g[] = new short[imgLength];
short img_b[] = new short[imgLength];

try {
	image.exportRGBData(1, 0, imgLength, img_r);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(1, 0, imgLength, img_r)");
    error = true;
    return -1;
}
try {
	image.exportRGBData(2, 0, imgLength, img_g);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(2, 0, imgLength, img_g)");
    error = true;
    return -1;
}
try {
	image.exportRGBData(3, 0, imgLength, img_b);
}
catch (IOException e) {
    MipavUtil.displayError("IOException " + e + " on image.exportRGBData(3, 0, imgLength, img_b)");
    error = true;
    return -1;
}


M = zDim*xDim*(yDim-1)+zDim*(xDim-1)*yDim+(zDim-1)*yDim*xDim; 
	 
for (i=0;i<M;i++)
 {
   wr = Math.abs(img_r[edges[0][i]]-img_r[edges[1][i]]) ;
   wg = Math.abs(img_g[edges[0][i]]-img_g[edges[1][i]]) ;
   wb = Math.abs(img_b[edges[0][i]]-img_b[edges[1][i]]) ;
   weights[i] = wr*wr+wg*wg+wb*wb;
   if (weights[i]> maxi) (maxi) = weights[i];
 }
if (geod==false)
 { 
   for (i=0;i<M;i++)
	weights[i]= maxi-weights[i];
 }
else
 { 
   int j,k,n;
   int weights_tmp[] = new int[M];
   int seeds_function[] = new int[M];
   int numvoisins = 4;
   if (zDim>1) numvoisins = 6;
   for (i=0;i<M;i++)
	weights_tmp[i]=maxi-weights[i];
	
   for (j=0;j<size_seeds;j++)
	for (k=1;k<=numvoisins; k++)
	  {
	    //n = neighbor_node_edge(seeds.get(j), k, xDim, yDim, zDim);
	    //if (n != -1)
	      //seeds_function[n]=weights_tmp[n];
	  } 
   //gageodilate_union_find(seeds_function, weights_tmp, weights, edges, xDim, yDim, zDim, maxi, quicksort);
   weights_tmp = null;
   seeds_function = null;
 }
img_r = null;
img_g = null;
img_b = null;
return maxi;
}

    
}
