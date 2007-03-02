import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.*;

import java.util.*;


/**
 * This converts bulk cheshire overlay files from a specified directory to VOIs.
 *
 * @version  February 22, 2007
 * @author   jsensene
 * @see      AlgorithmBase
 */
public class PlugInAlgorithmCheshireVOI extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Image where converted cheshire overlays are stored. */
    private ModelImage cheshireComposite;

    /** Cheshire overlay files to process. */
    private Vector cheshireFiles;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  resultImage    Result image model
     * @param  srcImg         Source image model.
     * @param  cheshireFiles  DOCUMENT ME!
     *
     * @apram  cheshireFiles Cheshire overlay files.
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
            float highX = 0, highY = 0, highZ = 0;

            for (int i = 0; i < cheshireFiles.size(); i++) {
                File tempFile = ((File) cheshireFiles.get(i));
                cheshireArray[i] = new FileCheshireVOI(tempFile.getName(), tempFile.getParent() + "\\", srcImage);
                System.out.println(cheshireArray[i].isProgressBarVisible());

                VOI[] voiListTemp = cheshireArray[i].readVOI();
                Point3Df[] extrema = voiListTemp[i].maxWidth();

                for (int j = 0; j < extrema.length; j++) {

                    if (extrema[j].x > highX) {
                        highX = extrema[j].x;
                    }

                    if (extrema[j].y > highY) {
                        highY = extrema[j].y;
                    }

                    if (extrema[j].z > highZ) {
                        highZ = extrema[j].z;
                    }
                }

                for (int j = 0; j < voiListTemp.length; j++) {
                    fireProgressStateChanged((int) (100 * (((double) (j + 1)) / ((double) voiListTemp.length))));
                    voiList.add(voiListTemp[j]);
                }

                cheshireArray[i].fireProgressStateChanged(100);
                System.out.println(cheshireArray[i].isProgressBarVisible());
            }

            int[] dimExtents = new int[3];
            dimExtents[0] = ((int) (highX + 1)) * 2;
            dimExtents[1] = ((int) (highY + 1)) * 2;
            dimExtents[2] = ((int) (highZ + 1)) * 2;
            cheshireComposite = new ModelImage(0, dimExtents, "Cheshire Composite");
            cheshireComposite.addVOIs(voiList);
            new ViewJFrameImage(cheshireComposite);
            srcImage.addVOIs(voiList);
        } catch (IOException e) {
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
