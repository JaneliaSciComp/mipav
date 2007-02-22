import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;

import javax.vecmath.*;


/**
 * This converts a cheshire overlay file to VOIs.
 *
 * @version  February 22, 2007
 * @author   jsensene
 * @see      AlgorithmBase
 *
 *           
 */
public class PlugInAlgorithmCheshireVOI extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private Vector cheshireFiles;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  resultImage    Result image model
     * @param  srcImg         Source image model.
     * @apram  cheshireFiles   Cheshire overlay files.
     */
    public PlugInAlgorithmCheshireVOI(ModelImage resultImage, ModelImage srcImg, Vector cheshireFiles) {
        super(resultImage, srcImg);
        this.cheshireFiles = cheshireFiles;
        
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
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();
        try {
            FileCheshireVOI[] cheshireArray = new FileCheshireVOI[cheshireFiles.size()];
            VOIVector voiList = new VOIVector();
            for(int i=0; i<cheshireFiles.size(); i++) {
               File tempFile = ((File)cheshireFiles.get(i));              
               cheshireArray[i] = new FileCheshireVOI(tempFile.getName(), tempFile.getParent()+"\\", srcImage);
               VOI[] voiListTemp = cheshireArray[i].readVOI();
               
               for(int j=0; j<voiListTemp.length; j++) {
                   fireProgressStateChanged((int)(100*(((double)(j+1))/((double)voiListTemp.length))));
                   voiList.add(voiListTemp[j]);
               }
            }
            srcImage.addVOIs(voiList);
        }
        catch (IOException e) {
            MipavUtil.displayError("Image directory info for this image is not correct");
            setCompleted(false);
            return;
        }
        
        destImage.disposeLocal();
        destImage = null;
        
        fireProgressStateChanged(100);
        
        finalize();
        setCompleted(true);
        
    } // end runAlgorithm()

    

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("CheshireToVOI(" + ")\n");
    }

}
