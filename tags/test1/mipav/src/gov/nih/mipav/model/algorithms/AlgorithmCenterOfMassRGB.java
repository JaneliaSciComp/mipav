package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;

import java.io.*;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;


/**
 * * This algorithm calculates the red, green, and blue center of mass for 2D and 3D color images.
 
 *
 * @version  1.0 February 26, 2008
 * @author   William Gandler
 */
public class AlgorithmCenterOfMassRGB extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private float[] thresholdB;

    /** DOCUMENT ME! */
    private float[] thresholdG;

    /** Three arrays (R,G,B) containing minimum and maximum threshold for each channel. */
    private float[] thresholdR;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor for running algorithm to calculate red, green, and blue centers of mass.
     *
     * @param  srcImage     ModelImage source
     * @param  thresholdR   float[] red thresh
     * @param  thresholdG   float[] green thresh
     * @param  thresholdB   float[] blue thresh
     * @param  maskFlag     boolean whole image or voi
     */
    public AlgorithmCenterOfMassRGB(ModelImage srcImage, float[] thresholdR, float[] thresholdG, float[] thresholdB,
                                     boolean maskFlag) {
        super(null, srcImage);
        this.thresholdR = thresholdR;
        this.thresholdG = thresholdG;
        this.thresholdB = thresholdB;
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
     * Calculate red, green, and blue centers of mass for 2D color image.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        float[] buffer;
        int x;
        int y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        double xMassR = 0.0;
        double yMassR = 0.0;
        double valTotR = 0.0;
        double xCOMR;
        double yCOMR;
        double xMassG = 0.0;
        double yMassG = 0.0;
        double valTotG = 0.0;
        double xCOMG;
        double yCOMG;
        double xMassB = 0.0;
        double yMassB = 0.0;
        double valTotB = 0.0;
        double xCOMB;
        double yCOMB;
        String comStrR;
        String comStrG;
        String comStrB;
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
            length = srcImage.getSliceSize() * 4;
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating centers of mass ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Center Of Mass: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Center Of Mass: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        for (y = 0; (y < yDim) && !threadStopped; y++) {
            for (x = 0; x < xDim; x++) {
                i = 4*(y*xDim + x);
                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    if ((buffer[i+1] >= thresholdR[0]) && (buffer[i+1] <= thresholdR[1])) {
                        xMassR += x * buffer[i+1];
                        yMassR += y * buffer[i+1];
                        valTotR += buffer[i+1];
                    }
                    if ((buffer[i+2] >= thresholdG[0]) && (buffer[i+2] <= thresholdG[1])) {
                        xMassG += x * buffer[i+2];
                        yMassG += y * buffer[i+2];
                        valTotG += buffer[i+2];
                    }
                    if ((buffer[i+3] >= thresholdB[0]) && (buffer[i+3] <= thresholdB[1])) {
                        xMassB += x * buffer[i+3];
                        yMassB += y * buffer[i+3];
                        valTotB += buffer[i+3];
                    }
                }      
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
        
        xCOMR = xMassR * srcImage.getFileInfo()[0].getResolutions()[0]/valTotR;
        yCOMR = yMassR * srcImage.getFileInfo()[0].getResolutions()[1]/valTotR;
        comStrR = nf.format(xCOMR) + "\t" + nf.format(yCOMR);
        xCOMG = xMassG * srcImage.getFileInfo()[0].getResolutions()[0]/valTotG;
        yCOMG = yMassG * srcImage.getFileInfo()[0].getResolutions()[1]/valTotG;
        comStrG = nf.format(xCOMG) + "\t" + nf.format(yCOMG);
        xCOMB = xMassB * srcImage.getFileInfo()[0].getResolutions()[0]/valTotB;
        yCOMB = yMassB * srcImage.getFileInfo()[0].getResolutions()[1]/valTotB;
        comStrB = nf.format(xCOMB) + "\t" + nf.format(yCOMB);

        UI.setDataText("  Center of Mass               \t= " + comStrR + " R, " +
                comStrG + " G, " + comStrB + " B " + "\n");    

        
        setCompleted(true);
    }

    /**
     * Calculates red, green, and blue centers of mass in 3D color image.
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
        double xMassR = 0.0;
        double yMassR = 0.0;
        double zMassR = 0.0;
        double valTotR = 0.0;
        double xCOMR;
        double yCOMR;
        double zCOMR;
        double xMassG = 0.0;
        double yMassG = 0.0;
        double zMassG = 0.0;
        double valTotG = 0.0;
        double xCOMG;
        double yCOMG;
        double zCOMG;
        double xMassB = 0.0;
        double yMassB = 0.0;
        double zMassB = 0.0;
        double valTotB = 0.0;
        double xCOMB;
        double yCOMB;
        double zCOMB;
        String comStrR;
        String comStrG;
        String comStrB;
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
            length = srcImage.getSliceSize() * srcImage.getExtents()[2] * 4;
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating centers of mass ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Center Of Mass: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Center Of Mass: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        for (z = 0; (z < zDim) && !threadStopped; z++) {
            off = 4 * z * srcImage.getSliceSize();
            for (y = 0; y < yDim; y++) {
                off2 = off + 4 * y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = off2 + 4 * x;
                    if (((i % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                    }
        
                    if ((entireImage == true) || mask.get(i / 4)) {
                        if ((buffer[i+1] >= thresholdR[0]) && (buffer[i+1] <= thresholdR[1])) {
                            xMassR += x * buffer[i+1];
                            yMassR += y * buffer[i+1];
                            zMassR += z * buffer[i+1];
                            valTotR += buffer[i+1];
                        }
                        if ((buffer[i+2] >= thresholdG[0]) && (buffer[i+2] <= thresholdG[1])) {
                            xMassG += x * buffer[i+2];
                            yMassG += y * buffer[i+2];
                            zMassG += z * buffer[i+2];
                            valTotG += buffer[i+2];
                        }
                        if ((buffer[i+3] >= thresholdB[0]) && (buffer[i+3] <= thresholdB[1])) {
                            xMassB += x * buffer[i+3];
                            yMassB += y * buffer[i+3];
                            zMassB += z * buffer[i+3];
                            valTotB += buffer[i+3];
                        }
                    } 
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
        
        xCOMR = xMassR * srcImage.getFileInfo()[0].getResolutions()[0]/valTotR;
        yCOMR = yMassR * srcImage.getFileInfo()[0].getResolutions()[1]/valTotR;
        zCOMR = zMassR * srcImage.getFileInfo()[0].getResolutions()[2]/valTotR;
        comStrR = nf.format(xCOMR) + "\t" + nf.format(yCOMR) + "\t" + nf.format(zCOMR);
        xCOMG = xMassG * srcImage.getFileInfo()[0].getResolutions()[0]/valTotG;
        yCOMG = yMassG * srcImage.getFileInfo()[0].getResolutions()[1]/valTotG;
        zCOMG = zMassG * srcImage.getFileInfo()[0].getResolutions()[2]/valTotG;
        comStrG = nf.format(xCOMG) + "\t" + nf.format(yCOMG) + "\t" + nf.format(zCOMG);
        xCOMB = xMassB * srcImage.getFileInfo()[0].getResolutions()[0]/valTotB;
        yCOMB = yMassB * srcImage.getFileInfo()[0].getResolutions()[1]/valTotB;
        zCOMB = zMassB * srcImage.getFileInfo()[0].getResolutions()[2]/valTotB;
        comStrB = nf.format(xCOMB) + "\t" + nf.format(yCOMB) + "\t" + nf.format(zCOMB);

        UI.setDataText("  Center of Mass               \t= " + comStrR + " R, " +
                comStrG + " G, " + comStrB + " B " + "\n");   

        
        
        setCompleted(true);

    }

    public float[] getThresholdR(){
    	return thresholdR;
}
    public float[] getThresholdG(){
    	return thresholdG;
    }
    public float[] getThresholdB(){
    	return thresholdB;
    }

}
