import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PlugInAlgorithmNucleiStatistics is used to identify nuclei and output statistics for each nucleus
 * @version  May 9, 2013
 * @author   William Gandler
 * @see      AlgorithmBase 
 */
public class PlugInAlgorithmApplyTransferFunction extends AlgorithmBase {
  //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** The list of files to try to process with the algorithm. */
    private Vector<File> inputFiles;
    
    /**
     * 
     * @param srcImg
     */
    public PlugInAlgorithmApplyTransferFunction() {
        super(null, null);
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        inputFiles = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	
        if (threadStopped) {
            finalize();

            return;
        }

//        setCompleted(true);
    }
 
}