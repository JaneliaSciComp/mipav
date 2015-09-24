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
   * This is a port of the file AutoSeedWatershed.cpp which calls openCV written by Ravimal Bandara.  His web site is
   * titled Image Segmentation using Unsupervised Watershed Algorithm with an Over-segmentation Reduction Technique.
   * This code is written for 2D color images.  In response to the question:
   * Would it be possible to modify your code for use on black and white images? What changes would be necessary?
   * Ravimal Bandara responded:
   * Yes you can by creating a gray histogram instead of Hues Saturation histogram. But I am not sure about the accuracy
   * due to the grayscale histograms are less discriminative compared to the colour histogram.
   * His code is licensed under the Code Project Open License (CPOL).
   * @author ilb
   *
   */


public class AlgorithmAutoSeedWatershed extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private int segmentNumber;
	
	private int presentSegmentNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  segmentNumber
     */
    public AlgorithmAutoSeedWatershed(ModelImage destImg, ModelImage srcImg, int segmentNumber) {

        super(destImg, srcImg);
        this.segmentNumber = segmentNumber;
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

        fireProgressStateChanged("Watershed ...");

        presentSegmentNumber = 0;
        
        watershedSegment();
    }
    
    private void watershedSegment() {
    	// Convert the image to grayscale
    	// Y = 0.299 * R + 0.587 * G + 0.114 * B
    }

    

}
