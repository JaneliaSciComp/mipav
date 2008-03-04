package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;


/**
 * This algorithm calculates the center of mass for 2D and 3D black and white images.
 
 *
 * @version  1.0 February 26, 2008
 * @author   William Gandler
 */
public class AlgorithmCenterOfMass extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Array of two thresholds. threshold[0] = Minimum threshold, threshold[1] = Maximum threshold. */
    private float[] threshold;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCenterOfMass object.
     *
     * @param  srcImg       source image model
     * @param  threshold    array of two thresholds
     * @param  maskFlag     true indicates that the whole image should be processed
     */
    public AlgorithmCenterOfMass(ModelImage srcImg, float[] threshold, boolean maskFlag) {

        super(null, srcImg);

        this.threshold = threshold;
        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        threshold = null;
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
            setCompleted(false);

            return;
        }

        if (srcImage.getNDims() == 2) {
            calcInPlace2D();
        } else if (srcImage.getNDims() > 2) {
            calcInPlace3D();
        }
    }

    /**
     * Calculate center of mass of 2D black and white image.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        float[] buffer;
        int x;
        int y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        double xMass = 0.0;
        double yMass = 0.0;
        double valTot = 0.0;
        double xCOM;
        double yCOM;
        String comStr;
        DecimalFormat nf;
        ViewUserInterface UI = ViewUserInterface.getReference();
        
        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating center of mass ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Center of Mass: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Center of Mass: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (y = 0; (y < yDim) && !threadStopped; y++) {
            for (x = 0; x < xDim; x++) {
                i = x + y * xDim;
                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }  
    
                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {

                    xMass += x * buffer[i];
                    yMass += y * buffer[i];
                    valTot += buffer[i];
                } 
                
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
        
        xCOM = xMass * srcImage.getFileInfo()[0].getResolutions()[0]/valTot;
        yCOM = yMass * srcImage.getFileInfo()[0].getResolutions()[1]/valTot;
        comStr = nf.format(xCOM) + "\t" + nf.format(yCOM);
        UI.setDataText(srcImage.getImageName() + "  Center of Mass               \t= " + comStr + "\n");
        Preferences.debug(srcImage.getImageName() + "  Center of Mass               \t= " + comStr + "\n");

        setCompleted(true);
    }

    /**
     * Calculate center of mass of 3D black and white image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        int x;
        int y;
        int z;
        int off;
        int off2;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        double xMass = 0.0;
        double yMass = 0.0;
        double zMass = 0.0;
        double valTot = 0.0;
        double xCOM;
        double yCOM;
        double zCOM;
        String comStr;
        DecimalFormat nf;
        ViewUserInterface UI = ViewUserInterface.getReference();
        
        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating center of mass ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Center of Mass: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Center of Mass: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (z = 0; (z < zDim) && !threadStopped; z++) {
            off = z * sliceSize;
            for (y = 0; y < yDim; y++) {
                off2 = off + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = off2 + x;

                    if (((i % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                    }
        
                    if (((entireImage == true) || mask.get(i)) &&
                            ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {
                        xMass += x * buffer[i];
                        yMass += y * buffer[i];
                        zMass += z * buffer[i];
                        valTot += buffer[i];    
                    } 
                    
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        xCOM = xMass * srcImage.getFileInfo()[0].getResolutions()[0]/valTot;
        yCOM = yMass * srcImage.getFileInfo()[0].getResolutions()[1]/valTot;
        zCOM = zMass * srcImage.getFileInfo()[0].getResolutions()[2]/valTot;
        comStr = nf.format(xCOM) + "\t" + nf.format(yCOM) + "\t" + nf.format(zCOM);
        UI.setDataText(srcImage.getImageName() + "  Center of Mass               \t= " + comStr + "\n");
        Preferences.debug(srcImage.getImageName() + "  Center of Mass               \t= " + comStr + "\n");

        setCompleted(true);

    }

    
}
