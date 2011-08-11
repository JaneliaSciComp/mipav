package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.border.*;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import Jama.*;


/**
 * This algorithm operates on 3D black and white images and on 2D and 3D color images. The user can select 1 or both of
 * 2 output images: The first image is a filtered 3D image containing only the first n independent components, where n is
 * selected by the user after viewing images of the first 10 (or the number that exist if less than 10) independent
 * components. The second image is a 2D image created by averaging together the images in the 3D image. Note that red,
 * green, and blue are treated as separate 2D planes.
 * References:
 * 1.) Independent Component Analysis by Aapo Hyvarinen, Juha Karhunen, and Erkki Oja, John-Wiley & Sons, Inc.,
 * 2001. 
 */
public class AlgorithmIndependentComponents extends AlgorithmBase implements ActionListener, FocusListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage[] destImage;

    /** DOCUMENT ME! */
    private boolean displayAndAsk;

    /** if true create an image averaging together the slices in the filtered version. */
    private boolean doAveraging;

    /** if true create a filtered version of the original image. */
    private boolean doFilter;

    /** DOCUMENT ME! */
    private boolean haveColor;

    /** number of image planes present. */
    private int nPlanes;

    /** number of independent components displayed 10 or less than 10 if fewer exist. */
    private int nPresent;

    /** DOCUMENT ME! */
    private JButton OKButton;

    /** number of independent components to be retained. */
    private int pNumber;

    /** DOCUMENT ME! */
    private JDialog iNumberDialog;

    /** DOCUMENT ME! */
    private boolean pressedOK = false;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JTextField textNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmIndependentComponents object.
     *
     * @param  destImg        image model where result image is to stored
     * @param  srcImg         source image model
     * @param  doFilter       if true create a filtered version of the original image
     * @param  doAveraging    if true create an image averaging together the slices in the filtered version
     * @param  displayAndAsk  if true display independent component images and ask how many to retain
     * @param  pNumber        if displayAndAsk if false pNumber is the number of independent component images to retain
     */
    public AlgorithmIndependentComponents(ModelImage[] destImg, ModelImage srcImg, boolean doFilter, boolean doAveraging,
                                        boolean displayAndAsk, int pNumber) {

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;
        this.doFilter = doFilter;
        this.doAveraging = doAveraging;
        this.displayAndAsk = displayAndAsk;
        this.pNumber = pNumber;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String tmpStr;
        Object source = event.getSource();

        if (source == OKButton) {
            tmpStr = textNumber.getText();
            pNumber = Integer.parseInt(tmpStr);

            if (pNumber < 1) {
                MipavUtil.displayError("Number must be at least 1");
                textNumber.requestFocus();
                textNumber.selectAll();

                return;
            } else if (pNumber > nPlanes) {
                MipavUtil.displayError("Number must not exceed number of image planes");
                textNumber.requestFocus();
                textNumber.selectAll();

                return;
            } else {
                iNumberDialog.dispose();
                pressedOK = true;
            }
        }
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
     * Do nothing.
     *
     * @param  event  Focus event
     */
    public void focusGained(FocusEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  Focus event
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Start algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("IComponent: Source Image is null");
            setCompleted(false);

            return;
        }

        if (srcImage.isColorImage()) {
            haveColor = true;
        } else {
            haveColor = false;
        }

        if ((srcImage.getNDims() != 3) && (!haveColor)) {
            displayError("IComponent: Black and white source image must be 3D");
            setCompleted(false);

            return;
        }

        
        fireProgressStateChanged(srcImage.getImageName(), "Independent component ...");


        iComponent();

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     */
    public void updateFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
        } else if (resultImage.getNDims() == 3) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                int j = Math.min(i, image.getExtents()[2] - 1);
                fileInfo[i].setModality(image.getFileInfo()[j].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[j].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[j].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[j].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[j].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[j].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[j].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[j].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[j].getPhotometric());
            }
        } else if (resultImage.getNDims() == 4) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[i].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[i].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[i].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[i].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[i].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[i].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[i].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[i].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[i].getPhotometric());
            }
        }

    }  

    /**
     * DOCUMENT ME!
     *
     * @param  al  DOCUMENT ME!
     */
    private void createINumberDialog(ActionListener al) {
        JPanel panel;
        TitledBorder border;
        Font serif12, serif12B;
        JLabel labelNumber;

        iNumberDialog = new JDialog(ViewUserInterface.getReference().getActiveImageFrame(), "Select number", false);
        iNumberDialog.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) -
                                  (iNumberDialog.getBounds().width / 2),
                                  (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                  (iNumberDialog.getBounds().height / 2));
        iNumberDialog.getContentPane().setLayout(new GridBagLayout());

        iNumberDialog.setSize(300, 160);

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        panel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        panel.setForeground(Color.black);
        border = new TitledBorder("Number of independent components to retain");
        border.setTitleColor(Color.black);
        border.setBorder(new EtchedBorder());
        border.setTitleFont(serif12B);
        panel.setBorder(border);
        iNumberDialog.getContentPane().add(panel, gbc);

        textNumber = new JTextField();
        textNumber.setText("1");
        textNumber.setFont(serif12);
        textNumber.addFocusListener(this);
        panel.add(textNumber, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        labelNumber = new JLabel("1 - " + nPlanes);
        labelNumber.setForeground(Color.black);
        labelNumber.setFont(serif12);
        panel.add(labelNumber, gbc);

        JPanel buttonPanel = new JPanel();
        OKButton = new JButton("OK");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);
        OKButton.addActionListener(al);
        buttonPanel.add(OKButton);

        gbc.gridx = 0;
        gbc.gridy = 1;
        iNumberDialog.getContentPane().add(buttonPanel, gbc);
        iNumberDialog.setResizable(true);
        iNumberDialog.setVisible(true);

    }

    /**
     * DOCUMENT ME!
     */
    private void iComponent() {
        double[] mean;
        double[][] mm;
        int samples;
        int i, j, k, m;
        int z;
        int zDim;
        float[] values;
        double[] zvalues;
        int totalLength;
        double[][] covar;
        double[] x;
        int index;
        double temp;
        double[] tempRow;
        float[][] p;
        ModelImage[] pImage = null;
        int[] pExtents;
        ViewJFrameImage[] imageFrame = null;
        double[][] eigenInverse;
        double[][] pTrunc;
        float[] result;
        int iNumber;
        float vMin;
        float vMax;
        double typeMin;
        double typeMax;
        double a;
        double b;
        float rMin;
        float rMax;
        float scaledValues[] = null;
        int index2;
        int i2;
        int z2;

        if (haveColor) {

            if (srcImage.getNDims() == 2) {
                nPlanes = 3;
                zDim = 1;
            } else {
                zDim = srcImage.getExtents()[2];
                nPlanes = 3 * zDim;
            }
        } // if (haveColor)
        else { // not color
            zDim = srcImage.getExtents()[2];
            nPlanes = zDim;
        } // else not color

        try {
            mean = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            mean = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating mean");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {
            mean[i] = 0.0;
        }

        samples = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        if (haveColor) {
            totalLength = 4 * samples * zDim;
        } else {
            totalLength = samples * zDim;
        }

        fireProgressStateChanged("Exporting source data");

        try {
            values = new float[totalLength];
        } catch (OutOfMemoryError e) {
            values = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating values");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        try {
            srcImage.exportData(0, totalLength, values); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm IComponent: Image(s) locked");
            setCompleted(false);

            setThreadStopped(true);

            return;
        }

        fireProgressStateChanged(10);

        // Each element of the mean vector is the mean of 1 of the nPlanes
        fireProgressStateChanged("Calculating mean vector");

        if (haveColor) {

            for (z = 0; z < zDim; z++) {

                for (i = 1; i < 4; i++) {

                    for (j = 0; j < samples; j++) {
                        mean[(i - 1) + (3 * z)] += values[(4 * z * samples) + (4 * j) + i];
                    }

                    mean[(i - 1) + (3 * z)] /= samples;
                }
            }
        } // if haveColor
        else { // not color

            for (z = 0; z < zDim; z++) {

                for (j = 0; j < samples; j++) {
                    mean[z] += values[(z * samples) + j];
                }

                mean[z] /= samples;
            }
        } // else not color

        fireProgressStateChanged(20);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }
        
        // Set the values to zero mean
        fireProgressStateChanged("Making the variables zero mean");
        if (haveColor) {

            for (z = 0; z < zDim; z++) {

                for (i = 1; i < 4; i++) {

                    for (j = 0; j < samples; j++) {
                    	values[(4 * z * samples) + (4 * j) + i] -= mean[(i - 1) + (3 * z)];
                    }

                }
            }
        } // if haveColor
        else { // not color

            for (z = 0; z < zDim; z++) {

                for (j = 0; j < samples; j++) {
                	values[(z * samples) + j] -= mean[z];
                }

            }
        } // else not color
        

        // The elements of the covar matrix contain the covariances between the nPlanes.
        // The covariance matrix is real and symmetric, so finding a set of nPlanes
        // orthonormal eigenvectors is always possible.
        fireProgressStateChanged("Calculating covariance matrix");

        try {
            covar = new double[nPlanes][nPlanes];
        } catch (OutOfMemoryError e) {
            covar = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating covar");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < nPlanes; j++) {
                covar[i][j] = 0.0;
            }
        }

        try {
            x = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            x = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating x");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (i = 1; i < 4; i++) {
                        x[(i - 1) + (3 * z)] = values[(4 * z * samples) + (4 * j) + i];
                    }
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        covar[k][m] += x[k] * x[m];
                    }
                }
            }
        } // if haveColor
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                    x[z] = values[(z * samples) + j];
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        covar[k][m] += x[k] * x[m];
                    }
                }
            }
        } // else not color

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < nPlanes; j++) {
                covar[i][j] = (covar[i][j] / samples);
            }
        }

        fireProgressStateChanged(30);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Calculating eigenvalues and eigenvectors");

        double[] eigenvalue;
        double[][] V;
        double[] e1;
        double[][] D;
        Matrix matV;
        Matrix matD;
        Matrix matW; // Whitening matrix
        double w[][];
        try {
            eigenvalue = new double[covar.length];
            D = new double[covar.length][covar.length];
            V = new double[covar.length][covar.length];
            e1 = new double[covar.length];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating eig");
            setCompleted(false);
            setThreadStopped(true);
            return;
        }
        // covar = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
        // Whitening matrix = v * 1/sqrt(diagonal eigenvalues) * V'
        Eigenvalue.decompose( covar, V, eigenvalue, e1 );
        for (i = 0; i < covar.length; i++) {
        	D[i][i] = 1.0/Math.sqrt(eigenvalue[i]);
        }
        matV = new Matrix(V);
        matD = new Matrix(D);
        matW = (matV.times(matD)).times(matV.transpose());  
        w = matW.getArray();
        try {
            zvalues = new double[totalLength];
        } catch (OutOfMemoryError e) {
            zvalues = null;
            System.gc();
            displayError("Algorithm Independent component: Out of memory allocating zvalues");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }
        // A slightly stronger property than uncorrelatedness is whiteness.  Whiteness of a zero-mean random
        // vector means that components are uncorrelated and their variances equal unity.  Whitening is sometimes
        // called sphering.
        fireProgressStateChanged("Performing whitening");
        
        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (i = 1; i < 4; i++) {
                    	index = (i - 1) + (3 * z);
                    	for (z2 = 0; z2 < zDim; z2++) {
                    		for (i2 = 1; i2 < 4; i2++) {
                    			index2 = (i2 - 1) + (3 * z2);
                                zvalues[(4 * z * samples) + (4 * j) + i] += 
                                	w[index][index2] * values[(4 * z2 * samples) + (4 * j) + i2];
                    		}
                    	}
                    }
                }
            }
        } // if haveColor
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                	for (z2 = 0; z2 < zDim; z2++) {
                        zvalues[(z * samples) + j] += w[z][z2] * values[(z2 * samples) + j];
                	}
                }
            }
        } // else not color
        
       

        fireProgressStateChanged(90);
        fireProgressStateChanged("Importing averaged destination data");
        
        /*rMin = Float.MAX_VALUE;
        rMax = -Float.MAX_VALUE;
        for (i = 0; i < samples; i++) {
              if (result[i] < rMin) {
                  rMin = result[i];
              }
              if (result[i] > rMax) {
                  rMax = result[i];
              }
        }
        
        switch(destImage[iNumber].getType()) {
            case ModelStorageBase.BOOLEAN:
                typeMin = 0;
                typeMax = 1;
                break;
            case ModelStorageBase.BYTE:
                typeMin = -128;
                typeMax = 127;
                break;
            case ModelStorageBase.UBYTE:
                typeMin = 0;
                typeMax = 255;
                break;
            case ModelStorageBase.SHORT:
                typeMin = -32768;
                typeMax = 32767;
                break;
            case ModelStorageBase.USHORT:
                typeMin = 0;
                typeMax = 65535;
                break;
            case ModelStorageBase.INTEGER:
                typeMin = Integer.MIN_VALUE;
                typeMax = Integer.MAX_VALUE;
                break;
            case ModelStorageBase.UINTEGER:
                typeMin = 0;
                typeMax = 4294967295L;
                break;
            case ModelStorageBase.LONG:
                typeMin = Long.MIN_VALUE;
                typeMax = Long.MAX_VALUE;
                break;
            case ModelStorageBase.FLOAT:
                typeMin = -Float.MAX_VALUE;
                typeMax = Float.MAX_VALUE;
                break;
            case ModelStorageBase.DOUBLE:
                typeMin = -Double.MAX_VALUE;
                typeMax = Double.MAX_VALUE;
                break;
            default:
                typeMin = -Double.MAX_VALUE;
                typeMax = Double.MAX_VALUE;
        }
        
        if ((rMin < typeMin) || (rMax > typeMax)) {
            // typeMax = a * rMax + b;
            // typeMin = a * rMin + b;
            a = (typeMax - typeMin)/(rMax - rMin);
            b = typeMax - a * rMax;
            for (i = 0; i < samples; i++) {
                result[i] = (float)(a * result[i] + b);
            }
        }


        try {
            destImage[iNumber].importData(0, result, true);
        } catch (IOException error) {
            displayError("AlgorithmPrincipalComponents: IOException on averaged destination image import data");

            setCompleted(false);

            return;
        }*/

        setCompleted(true);
    }

}
