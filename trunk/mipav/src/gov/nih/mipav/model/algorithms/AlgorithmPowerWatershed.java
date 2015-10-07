package gov.nih.mipav.model.algorithms;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.filters.*;
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

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }



        fireProgressStateChanged("Power Watershed ...");

        
    }

    
}
