import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;

import java.io.*;

import java.util.*;
import javax.swing.JOptionPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 *
 * @version  April 16, 2009
 * @author   William Gandler
 * @see      AlgorithmBase
 *
 
 */
public class PlugInAlgorithmAxonExtraction extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int redRadius = 3;
    
    private int greenRadius = 3;
    
    
    private int xDim = srcImage.getExtents()[0];
    private int yDim = srcImage.getExtents()[1];
    private int zDim = srcImage.getExtents()[2];
    private int length = xDim * yDim * zDim;
    private int xySlice = xDim * yDim;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redRadius      Radius of red disc in preprocessing morphology
     */
    public PlugInAlgorithmAxonExtraction(ModelImage srcImg, int redRadius, int greenRadius) {
        super(null, srcImg);
        this.redRadius = redRadius;
        this.greenRadius = greenRadius;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
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

        

       calc3D();
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void calc3D() {
        long time;
        int x, y, z;
        short shortBuffer[];
        short shortOpen[];
        short shortClose[];
        byte byteBuffer[];
        float redBuffer[];
        float greenBuffer[];
        short blueBuffer[];
        int extents2D[];
        ModelImage shortImage;
        int color;
        AlgorithmGrayScaleMorphology2D openAlgo2D;
        AlgorithmGrayScaleMorphology2D closeAlgo2D;
        int kernelSize;
        int itersD = 1;
        int itersE = 1;
        int i;
        boolean pointsEntered;
        Vector[] curves;
        int nPts;
        int s;
        Vector3f[] pt = null;
        Vector3f[] tmpPt = null;
        int ptNum = 0;
        int pos;
        int redPts = 0;
        int greenPts = 0;
        int bluePts = 0;
        int indeterminateColorPts = 0;
        int redNum = 0;
        int greenNum = 0;
        int redX[] = null;
        int redY[] = null;
        int redZ[] = null;
        int greenX[] = null;
        int greenY[] = null;
        int greenZ[] = null;
        int c;
        int cPts;
        int cX[];
        int cY[];
        int cZ[];
        JDialogSetPoints choice;
        float colorBuffer[];
        float sigmas[] = new float[3];
        ModelImage colorImage;
        AlgorithmHessian hessianAlgo;
        double[][] hess = null;
        AlgorithmEigensolver eigenSystemAlgo = null;
        double[] evals = new double[3];
        double[] magEvals = new double[3];
        double lambda1;
        double lambda2;
        double lambda3;
        double magLambda2;
        double similarity[];
        double maxSimilarity = 0.0;
        int zPos;
        int yPos;
        double cost[];

        time = System.currentTimeMillis();

        if (threadStopped) {
            finalize();

            return;
        }
        
        // Do 2.5D morphological preprocessing on red and green structures
        // Use a disc
        // I = I + Itop - Ibottom
        // Itop = I - Iopen
        // Ibottom = Iclose - I
        // I = I + I - Iopen - Iclose + I = 3*I - Iopen - IClose
        shortBuffer = new short[xySlice];
        shortOpen = new short[xySlice];
        shortClose = new short[xySlice];
        byteBuffer = new byte[xySlice];
        extents2D = new int[2];
        extents2D[0] = xDim;
        extents2D[1] = yDim;
        shortImage = new ModelImage(ModelImage.SHORT, extents2D, "short_image");
        for (color = 1; color <= 2; color++) {
            if (((color == 1) && (redRadius >= 1)) || ((color == 2) && (greenRadius >= 1))) {
                if (color == 1) {
                    kernelSize = redRadius;
                }
                else {
                    kernelSize = greenRadius;
                }
                for (z = 0; z < zDim; z++) {
                    fireProgressStateChanged(100 *((color - 1)* zDim +  z)/(2 * zDim));
                    // Do morphological preprocessing on red and green
                    
                        try {
                            srcImage.exportRGBData(color, 4*z*xySlice, xySlice, shortBuffer); // export color data
                        } catch (IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: source image locked", true);
                
                            return;
                        }
                        
                        System.arraycopy(shortBuffer, 0, shortOpen, 0, shortBuffer.length);
                        System.arraycopy(shortBuffer, 0, shortClose, 0, shortBuffer.length);
                        
                        try {
                            shortImage.importData(0, shortOpen, true);
                        }
                        catch(IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: shortImage locked", true);
                            
                            return;
                        }
                        
                        openAlgo2D = new AlgorithmGrayScaleMorphology2D(shortImage, AlgorithmMorphology2D.SIZED_CIRCLE, kernelSize, AlgorithmMorphology2D.OPEN,
                                itersD, itersE, 0, 0, true);
                        openAlgo2D.run();
                        openAlgo2D.finalize();
                        openAlgo2D = null;
                        
                        try {
                            shortImage.exportData(0, xySlice, shortOpen);
                        }
                        catch(IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: shortImage locked", true);
                            
                            return;
                        }
                        
                        try {
                            shortImage.importData(0, shortClose, true);
                        }
                        catch(IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: shortImage locked", true);
                            
                            return;
                        }
                        
                        closeAlgo2D = new AlgorithmGrayScaleMorphology2D(shortImage, AlgorithmMorphology2D.SIZED_CIRCLE, kernelSize, AlgorithmMorphology2D.CLOSE,
                                itersD, itersE, 0, 0, true);
                        closeAlgo2D.run();
                        closeAlgo2D.finalize();
                        closeAlgo2D = null;
                        
                        try {
                            shortImage.exportData(0, xySlice, shortClose);
                        }
                        catch(IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: shortImage locked", true);
                            
                            return;
                        }
                        
                        for (i = 0; i < xySlice; i++) {
                            shortBuffer[i] = (short)(3 * shortBuffer[i] - shortOpen[i] - shortClose[i]); 
                            if (shortBuffer[i] > 255) {
                                shortBuffer[i] = 255;
                            }
                            else if (shortBuffer[i] < 0) {
                                shortBuffer[i] = 0;
                            }
                            byteBuffer[i] = (byte)(shortBuffer[i]);
                        }
                        
                        try {
                            srcImage.importRGBData(color, 4*z*xySlice, byteBuffer, false); // import color data
                        } catch (IOException error) {
                            shortBuffer = null;
                            shortOpen = null;
                            shortClose = null;
                            errorCleanUp("Algorithm Axon extraction reports: source image locked", true);
                
                            return;
                        }
                    } // for (z = 0; z < zDim; z++)
            } // if (((color == 1) && (redRadius >= 1)) || ((color == 2) && (greenRadius >= 1)))
        } // for (color = 1; color <= 2; color++)
        shortBuffer = null;
        shortOpen = null;
        shortClose = null;
        srcImage.calcMinMax();
        

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmAxonExtraction preprocessing elapsed time in seconds = " + (time/1000.0));
        
        pointsEntered = false;
        
        
   
        while (!pointsEntered) {
            choice = new JDialogSetPoints(srcImage.getParentFrame()); 
            
            while (!choice.okayPressed() ) {
                ;
            }
            
            if (srcImage.getVOIs().size() == 0) {
                JOptionPane.showMessageDialog(null, "No extraction points were entered", "Points Not Entered",
                                      JOptionPane.ERROR_MESSAGE);   
          
            }
            else {
                pointsEntered = true;
            }
        } // while (!pointsEntered)
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0);
        
        curves = srcImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slices

        nPts = 0;
        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            nPts += curves[i].size();
        }
        Preferences.debug("nPts = " + nPts + "\n");
        pt = new Vector3f[nPts];
        
        ptNum = 0;
        for (s = 0; s < srcImage.getExtents()[2]; s++) {
            tmpPt = srcImage.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpPt.length; i++) {
                pt[ptNum++] = tmpPt[i];
            }
        }
        
        redBuffer = new float[length];
        try {
            srcImage.exportRGBData(1, 0, length, redBuffer); // export red data
        } catch (IOException error) {
            redBuffer = null;
            errorCleanUp("Algorithm Axon extraction reports: source image locked", true);

            return;
        }
        
        greenBuffer = new float[length];
        try {
            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm Axon extraction reports: source image locked", true);

            return;
        }
        
        blueBuffer = new short[length];
        try {
            srcImage.exportRGBData(3, 0, length, blueBuffer); // export blue data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            blueBuffer = null;
            errorCleanUp("Algorithm Axon extraction reports: source image locked", true);

            return;
        }
        
        for (i = 0; i < nPts; i++) {
            pos = Math.round(pt[i].X) + xDim * Math.round(pt[i].Y) + xySlice * Math.round(pt[i].Z);
            if ((redBuffer[pos] > greenBuffer[pos]) && (redBuffer[pos] > blueBuffer[pos])) {
                redPts++;
            }
            else if ((greenBuffer[pos] > redBuffer[pos]) && (greenBuffer[pos] > blueBuffer[pos])) {
                greenPts++;
            }
            else if ((blueBuffer[pos] > redBuffer[pos]) && (blueBuffer[pos] > greenBuffer[pos])) {
                bluePts++;
            }
            else {
                indeterminateColorPts++;
            }
        }
        
        if (redPts == 0) {
            Preferences.debug("No red presynaptic points were selected for extraction\n");
        }
        else if (redPts == 1) {
            Preferences.debug("1 red presynaptic point was selected for extraction\n");
        }
        else {
            Preferences.debug(redPts + " presynaptic points were selected for extraction\n");
        }
        
        if (greenPts == 0) {
            Preferences.debug("No green postsynaptic points were selected for extraction\n");
        }
        else if (greenPts == 1) {
            Preferences.debug("1 green postsynaptic point was selected for extraction \n");
        }
        else {
            Preferences.debug(greenPts + " postsynaptic points were selected for extraction\n");
        }
        
        if (bluePts == 0) {
            Preferences.debug("No blue presynaptic swelling points were selected\n");
        }
        else if (bluePts == 1) {
            Preferences.debug("1 blue presynaptic swelling point will not be extracted\n");
        }
        else {
            Preferences.debug(bluePts + " blue synaptic swelling points will not be extracted\n");
        }
        
        if (indeterminateColorPts == 0) {
            Preferences.debug("No points of indeterminate color were selected\n");
        }
        else if (indeterminateColorPts == 1) {
            Preferences.debug("1 point of indeterminate color will not be extracted\n");
        }
        else {
            Preferences.debug(indeterminateColorPts + " points of indeterminate color will not be extracted\n");
        }
        
        if (redPts > 0) {
            redX = new int[redPts];
            redY = new int[redPts];
            redZ = new int[redPts];
        }
        if (greenPts > 0) {
            greenX = new int[greenPts];
            greenY = new int[greenPts];
            greenZ = new int[greenPts];
        }
        
        for (i = 0; i < nPts; i++) {
            pos = Math.round(pt[i].X) + xDim * Math.round(pt[i].Y) + xySlice * Math.round(pt[i].Z);
            if ((redBuffer[pos] > greenBuffer[pos]) && (redBuffer[pos] > blueBuffer[pos])) {
                redX[redNum] = Math.round(pt[i].X);
                redY[redNum] = Math.round(pt[i].Y);
                redZ[redNum++] = Math.round(pt[i].Z);
            }
            else if ((greenBuffer[pos] > redBuffer[pos]) && (greenBuffer[pos] > blueBuffer[pos])) {
                greenX[greenNum] = Math.round(pt[i].X);
                greenY[greenNum] = Math.round(pt[i].Y);
                greenZ[greenNum++] = Math.round(pt[i].Z);    
            }
        } // for (i = 0; i < nPts; i++)
        
        sigmas[0] = 0.5f;
        sigmas[1] = 0.5f;
        sigmas[2] = 0.5f;
        colorImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "colorImage");
        eigenSystemAlgo = new AlgorithmEigensolver(3);
        similarity = new double[length];
        cost = new double[length];
        for (c = 1; c <= 2; c++) {
            maxSimilarity = 0.0;
            if (((c == 1) && (redPts >= 1)) || ((c == 2) && (greenPts >= 1))) {
                if (c == 1) {
                    cPts = redPts;
                    cX = redX;
                    cY = redY;
                    cZ = redZ;
                    colorBuffer = redBuffer;
                }
                else {
                    cPts = greenPts;
                    cX = greenX;
                    cY = greenY;
                    cZ = greenZ;
                    colorBuffer = greenBuffer;
                }
                try {
                    colorImage.importData(0, colorBuffer, true);
                }
                catch(IOException e) {
                    MipavUtil.displayError("IOException on colorImage.importData(0, colorBuffer, true");
                    setCompleted(false);
                    return;
                }
                hessianAlgo = new AlgorithmHessian(colorImage, sigmas);
                for (z = 0; z < zDim; z++) {
                    zPos = z * xySlice;
                    for (y = 0; y < yDim; y++) {
                        yPos = zPos + y * xDim;
                        for (x = 0; x < xDim; x++) {
                            pos = yPos + x;
                            // get the Hessian at this point
                            hess = hessianAlgo.hessian3D(colorBuffer, srcImage.getExtents(), x, y, z);

                            // fill up the eigenSolver matrix with the hessian
                            eigenSystemAlgo.setMatrix(0, 0, hess[0][0]);
                            eigenSystemAlgo.setMatrix(0, 1, hess[0][1]);
                            eigenSystemAlgo.setMatrix(0, 2, hess[0][2]);

                            eigenSystemAlgo.setMatrix(1, 0, hess[1][0]);
                            eigenSystemAlgo.setMatrix(1, 1, hess[1][1]);
                            eigenSystemAlgo.setMatrix(1, 2, hess[1][2]);

                            eigenSystemAlgo.setMatrix(2, 0, hess[2][0]);
                            eigenSystemAlgo.setMatrix(2, 1, hess[2][1]);
                            eigenSystemAlgo.setMatrix(2, 2, hess[2][2]);

                            // OK, solve the eigen system
                            eigenSystemAlgo.solve();
                            
                            // extract the eigenvalues from the AlgorithmEigensolver
                            evals[0] = eigenSystemAlgo.getEigenvalue(0);
                            magEvals[0] = Math.abs(evals[0]);

                            evals[1] = eigenSystemAlgo.getEigenvalue(1);
                            magEvals[1] = Math.abs(evals[1]);

                            evals[2] = eigenSystemAlgo.getEigenvalue(2);
                            magEvals[2] = Math.abs(evals[2]);
                            
                            // magLamda1 >= magLambda2 >= magLambda3
                            if ((magEvals[0] >= magEvals[1]) && (magEvals[0] >= magEvals[2])) {
                                lambda1 = evals[0];
                                if (magEvals[1] >= magEvals[2]) {
                                    lambda2 = evals[1];
                                    magLambda2 = magEvals[1];
                                    lambda3 = evals[2];
                                }
                                else {
                                    lambda2 = evals[2];
                                    magLambda2 = magEvals[2];
                                    lambda3 = evals[1];
                                }
                            } // if ((magEvals[0] >= magEvals[1]) && (magEvals[0] >= magEvals[2]))
                            else if ((magEvals[1] >= magEvals[0]) && (magEvals[1] >= magEvals[2])) {
                                lambda1 = evals[1];
                                if (magEvals[0] >= magEvals[2]) {
                                    lambda2 = evals[0];
                                    magLambda2 = magEvals[0];
                                    lambda3 = evals[2];
                                }
                                else {
                                    lambda2 = evals[2];
                                    magLambda2 = magEvals[2];
                                    lambda3 = evals[0];  
                                }
                            } // else if ((magEvals[1] >= magEvals[0]) && (magEvals[1] >= magEvals[2]))
                            else {
                                lambda1 = evals[2];
                                if (magEvals[0] >= magEvals[1]) {
                                    lambda2 = evals[0];
                                    magLambda2 = magEvals[0];
                                    lambda3 = evals[1];
                                }
                                else {
                                    lambda2 = evals[1];
                                    magLambda2 = magEvals[1];
                                    lambda3 = evals[0];  
                                }
                            } // else
                            if ((lambda1 < 0) && (lambda2 < 0)) {
                                if (lambda3 <= 0) {
                                    similarity[pos] = magLambda2 + lambda3;
                                }
                                else if (lambda3 < 4.0*magLambda2) {
                                    similarity[pos] = magLambda2 - lambda3/4.0;
                                }
                                else {
                                    similarity[pos] = 0.0;
                                }
                            } // if ((lambda1 < 0) && (lambda2 < 0))
                            else {
                                similarity[pos] = 0.0;
                            }
                            if (similarity[pos] > maxSimilarity) {
                                maxSimilarity = similarity[pos];
                            }
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)
                } // for (z = 0; z < zDim; z++)
                if (c == 1) {
                    for (pos = 0; pos < length; pos++) {
                        if ((greenBuffer[pos] > redBuffer[pos]) || (blueBuffer[pos] > redBuffer[pos])) {
                            cost[pos] = 1.0;
                        }
                        else {
                            cost[pos] = 1.0 - similarity[pos]/maxSimilarity;
                        }
                    }
                } // if (c == 1) 
                else {
                    for (pos = 0; pos < length; pos++) {
                        if ((redBuffer[pos] > greenBuffer[pos]) || (blueBuffer[pos] > greenBuffer[pos])) {
                            cost[pos] = 1.0;
                        }
                        else {
                            cost[pos] = 1.0 - similarity[pos]/maxSimilarity;
                        }
                    }    
                }
                for (i = 0; i < cPts; i++) {
                    
                } // for (i = 0; i < cPts; i++)
            } // if (((c == 1) && (redPts >= 1)) || ((c == 2) && (greenPts >= 1))) {
        } // for (c = 1; c <= 2; c++)
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmAxonExtraction processing elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }
    
    /**
     * DOCUMENT ME!
     */
    private void calc2D() {
        long time;
        
        
        fireProgressStateChanged("Synapse detection on image");

        time = System.currentTimeMillis();
        
         
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmAxonExtraction elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
        
    }
    
    /**
     * Confirmation Dialog on which the user hits okay when extraction points are selected.
     */
    class JDialogSetPoints extends JDialogBase {

        //~ Instance fields ------------------------------------------------------------------------------------------------

        
        
        /** Whether the window was closed through the user clicking the OK button (and not just killing the dialog). */
        private boolean okayPressed = false;
        
        
        //~ Constructors ---------------------------------------------------------------------------------------------------

        /**
         * Creates new dialog.
         *
         * @param  theParentFrame  Parent frame of dialog.
         */
        public JDialogSetPoints(Frame theParentFrame) {
            super(theParentFrame, false);
            init();
            setVisible(true);
        }

        //~ Methods --------------------------------------------------------------------------------------------------------

        /**
         * Checks to see if the OK or Cancel buttons were pressed.
         *
         * @param  event  Event that triggered this function.
         */
        public void actionPerformed(ActionEvent event) {

            if (event.getSource() == OKButton) {

                okayPressed = true;
            }

            dispose();
        }

        /**
         * Was the okay button pressed.
         *
         * @return  boolean was okay pressed
         */
        public boolean okayPressed() {
            return okayPressed;
        }

        /**
         * Creates and displays dialog.
         */
        private void init() {
            JLabel saveLabel;
            setTitle("Extraction Point Entry");

            
            saveLabel = new JLabel("Click okay after entering extraction points ");
            saveLabel.setFont(serif12);
            saveLabel.setForeground(Color.black);
            

            JPanel buttonPanel = new JPanel();
            buildOKButton();
            buttonPanel.add(OKButton);

            mainDialogPanel.add(saveLabel);
            mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

            getContentPane().add(mainDialogPanel);

            pack();
        }
    }
    
}
