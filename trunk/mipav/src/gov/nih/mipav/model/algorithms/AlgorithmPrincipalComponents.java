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


/**
 * This algorithm operates on 3D black and white images and on 2D and 3D color images. The user can select 1 or both of
 * 2 output images: The first image is a filtered 3D image containing only the first n principal components, where n is
 * selected by the user after viewing images of the first 10 (or the number that exist if less than 10) principal
 * components. The second image is a 2D image created by averaging together the images in the 3D image. Note that red,
 * green, and blue are treated as separate 2D planes. This implementation assumes that the noise variance is equal in
 * every slice and that the noise is uncorrelated between slices. Principal components maximizes data variance in
 * components; it concentrates the information content of the data in as small a number of components as possible. This
 * algorithm is based on: 1.) Digital Image Processing, Third Edition by Rafael C. Gonzalez and Richard E. Woods,
 * Section 11.4 Use of Principal Components for Description, pp. 842 - 852, 2008 by Pearson Education, Inc.
 * 2.) Information about using a matchImage: 
 * Eigenfaces for Recognition by Matthew Turk and Alex Pentland Vision and Modeling Group The Media Laboratory
 * Massachusetts Institute of Technology
 * 3.) "The statistical properties of three noise removal procedures for multichannel remotely sensed data" by
 * M. Berman, CSIRO Division of Mathemetics and Statistics P.O. Box 218, Lindfield, N.S.W., 2070 Consulting Report
 * NSW/85/31/MB9 Note that this algorithm has 4 different names: Hotelling, eigenvector, principal component, and
 * Karhunen-Loeve transform. Also note that when the noise variance is the same in all bands and the noise is uncorrelated
 * between bands the principal component transformation is equivalent to the maximum noise fraction transformation.
 * In the more general case, the MNF transform is equivalent to a transformation of the data to a coordinate system in
 * which the noise covariance matrix is the identity matrix, followed by a principal components transformation. The
 * transform in this form is the noise-adjusted principal components transform. In forward order MNF maximizes the noise
 * content in successive components that are uncorrelated over the dataset or equivalently in reverse order MNF maximizes
 * the signal to noise ratio of each component. Information about the signal to noise ratio improvement is found in
 * "Information Extraction, SNR Improvement, and Data Compression in MultiSpectral Imagery", Patrick J. Ready and
 * Paul A. Wintz, IEEE Transactions on Communications, Vol. COM-21, No. 10, October, 1973, pp. 1123-1130.
 */
public class AlgorithmPrincipalComponents extends AlgorithmBase implements ActionListener, FocusListener {

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

    /** number of principal components displayed 10 or less than 10 if fewer exist. */
    private int nPresent;

    /** DOCUMENT ME! */
    private JButton OKButton;

    /** number of principal components to be retained. */
    private int pNumber;

    /** DOCUMENT ME! */
    private JDialog pNumberDialog;

    /** DOCUMENT ME! */
    private boolean pressedOK = false;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private JTextField textNumber;
    
    /** If this 2D black and white image is present, it is matched to the closest slice and/or color in the
     * source image model by looking for the closest match of the weights of the principal components.
     */
    private ModelImage matchImage = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new AlgorithmPrincipalComponents object.
     *
     * @param  destImg        image model where result image is to stored
     * @param  srcImg         source image model
     * @param  matchImage     match image match this image to the closest slice and/or color in the source image
     * @param  doFilter       if true create a filtered version of the original image
     * @param  doAveraging    if true create an image averaging together the slices in the filtered version
     * @param  displayAndAsk  if true display principal component images and ask how many to retain
     * @param  pNumber        if displayAndAsk if false pNumber is the number of principal component images to retain
     */
    public AlgorithmPrincipalComponents(ModelImage[] destImg, ModelImage srcImg, ModelImage matchImage, boolean doFilter, boolean doAveraging,
                                        boolean displayAndAsk, int pNumber) {

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;
        this.matchImage = matchImage;
        this.doFilter = doFilter;
        this.doAveraging = doAveraging;
        this.displayAndAsk = displayAndAsk;
        this.pNumber = pNumber;
    }

    /**
     * Creates a new AlgorithmPrincipalComponents object.
     *
     * @param  destImg        image model where result image is to stored
     * @param  srcImg         source image model
     * @param  doFilter       if true create a filtered version of the original image
     * @param  doAveraging    if true create an image averaging together the slices in the filtered version
     * @param  displayAndAsk  if true display principal component images and ask how many to retain
     * @param  pNumber        if displayAndAsk if false pNumber is the number of principal component images to retain
     */
    public AlgorithmPrincipalComponents(ModelImage[] destImg, ModelImage srcImg, boolean doFilter, boolean doAveraging,
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
                pNumberDialog.dispose();
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
            displayError("PComponent: Source Image is null");
            setCompleted(false);

            return;
        }

        if (srcImage.isColorImage()) {
            haveColor = true;
        } else {
            haveColor = false;
        }

        if ((srcImage.getNDims() != 3) && (!haveColor)) {
            displayError("PComponent: Black and white source image must be 3D");
            setCompleted(false);

            return;
        }

        
        fireProgressStateChanged(srcImage.getImageName(), "Principal component ...");


        pComponent();

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
    private void createPNumberDialog(ActionListener al) {
        JPanel panel;
        TitledBorder border;
        Font serif12, serif12B;
        JLabel labelNumber;

        pNumberDialog = new JDialog(ViewUserInterface.getReference().getActiveImageFrame(), "Select number", false);
        pNumberDialog.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) -
                                  (pNumberDialog.getBounds().width / 2),
                                  (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                  (pNumberDialog.getBounds().height / 2));
        pNumberDialog.getContentPane().setLayout(new GridBagLayout());

        pNumberDialog.setSize(300, 160);

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
        border = new TitledBorder("Number of principal components to retain");
        border.setTitleColor(Color.black);
        border.setBorder(new EtchedBorder());
        border.setTitleFont(serif12B);
        panel.setBorder(border);
        pNumberDialog.getContentPane().add(panel, gbc);

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
        pNumberDialog.getContentPane().add(buttonPanel, gbc);
        pNumberDialog.setResizable(true);
        pNumberDialog.setVisible(true);

    }

    /**
     * DOCUMENT ME!
     */
    private void pComponent() {
        double[] mean;
        double[][] mm;
        int samples;
        int i, j, k, m;
        int z;
        int zDim;
        float[] values;
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
        float matchValues[] = null;
        double matchMean;
        double weight[] = null;
        double diff;
        double diffSquared;
        double maxDiffSquared;
        int zClosest;
        int cClosest;
        double total;

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
            displayError("Algorithm Principal component: Out of memory allocating mean");
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
            displayError("Algorithm Principal component: Out of memory allocating values");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        try {
            srcImage.exportData(0, totalLength, values); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm PComponent: Image(s) locked");
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

        // The elements of the covar matrix contain the covariances between the nPlanes.
        // The covariance matrix is real and symmetric, so finding a set of nPlanes
        // orthonormal eigenvectors is always possible.
        fireProgressStateChanged("Calculating covariance matrix");

        try {
            covar = new double[nPlanes][nPlanes];
        } catch (OutOfMemoryError e) {
            covar = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating covar");
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
            displayError("Algorithm Principal component: Out of memory allocating x");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        try {
            mm = new double[nPlanes][nPlanes];
        } catch (OutOfMemoryError e) {
            mm = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating mm");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < nPlanes; j++) {
                mm[i][j] = mean[i] * mean[j];
            }
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
                covar[i][j] = (covar[i][j] / samples) - mm[i][j];
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
        double[][] eigenvector;
        double[] e1;
        try {
            eigenvalue = new double[covar.length];
            V = new double[covar.length][covar.length];
            eigenvector= new double[covar.length][covar.length];
            e1 = new double[covar.length];
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating eig");
            setCompleted(false);
            setThreadStopped(true);
            return;
        }
        Eigenvalue.decompose( covar, V, eigenvalue, e1 );
        
        // In EigenvalueDecomposition the columns of V represent the eigenvectors,
        // but in this algorithm we want the eigenvectors in rows.
        for ( i = 0; i < eigenvector.length; i++ )
        {
        	for ( j = 0; j < eigenvector[i].length; j++ )
        	{
        		eigenvector[i][j] = V[j][i];
        	}
        }

        // Arrange the eigenvalues and corresponding eigenvectors in descending order
        // so that ej >= ej+1
        try {
            tempRow = new double[nPlanes];
        } catch (OutOfMemoryError e) {
            tempRow = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating tempRow");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {
            index = i;

            for (j = i + 1; j < nPlanes; j++) {

                if (eigenvalue[j] > eigenvalue[i]) {
                    index = j;
                }
            } // for (j = i+1; j < nPlanes; j++)

            if (index != i) {
                temp = eigenvalue[i];
                eigenvalue[i] = eigenvalue[index];
                eigenvalue[index] = temp;

                for (j = 0; j < nPlanes; j++) {
                    tempRow[j] = eigenvector[i][j];
                    eigenvector[i][j] = eigenvector[index][j];
                    eigenvector[index][j] = tempRow[j];
                }
            } // if (index != i)
        } // for (i = 0; i < nPlanes; i++)

        // Print out the first 10 eigenvalues or all that are present if less than 10.
        nPresent = Math.min(10, nPlanes);
        Preferences.debug("The first " + nPresent + " eigenvalues for the principal components are:\n", 
        		Preferences.DEBUG_ALGORITHM);

        for (i = 0; i < nPresent; i++) {
            Preferences.debug(eigenvalue[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        fireProgressStateChanged(40);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Calculating principal components");

        // Principal component 1 is formed by performing the inner (dot)
        // product on the first row of eigenvector with the column vector
        // (x - mx)Transpose.
        try {
            p = new float[nPlanes][samples];
        } catch (OutOfMemoryError e) {
            p = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating p");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < samples; j++) {
                p[i][j] = 0.0f;
            }
        }

        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (i = 1; i < 4; i++) {
                        x[(i - 1) + (3 * z)] = values[(4 * z * samples) + (4 * j) + i] - mean[(i - 1) + (3 * z)];
                    }
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        p[k][j] += x[m] * eigenvector[k][m];
                    }
                }
            } // for (j = 0; j < samples; j++)
        } // if (haveColor)
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                    x[z] = values[(z * samples) + j] - mean[z];
                }

                for (k = 0; k < nPlanes; k++) {

                    for (m = 0; m < nPlanes; m++) {
                        p[k][j] += x[m] * eigenvector[k][m];
                    }
                }
            } // for (j = 0; j < samples; j++)
        } // else not color

        fireProgressStateChanged(50);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }

        if (displayAndAsk) {

            // Output pictures of the first 10 principal components or all that are present
            // if less than 10
            fireProgressStateChanged("Creating principal component images");

            try {
                pImage = new ModelImage[nPresent];
            } catch (OutOfMemoryError e) {
                pImage = null;
                System.gc();
                displayError("Algorithm Principal component: Out of memory allocating pImage");
                setCompleted(false);
                setThreadStopped(true);


                return;
            }

            pExtents = new int[2];
            pExtents[0] = srcImage.getExtents()[0];
            pExtents[1] = srcImage.getExtents()[1];

            try {
                imageFrame = new ViewJFrameImage[nPresent];
            } catch (OutOfMemoryError e) {
                imageFrame = null;
                System.gc();
                displayError("Algorithm Principal component: Out of memory allocating imageFrame");
                setCompleted(false);
                setThreadStopped(true);


                return;
            }

            for (i = nPresent - 1; (i >= 0) && (!threadStopped); i--) {
                pImage[i] = new ModelImage(ModelImage.FLOAT, pExtents, srcImage.getImageName() + "_p" + (i + 1));

                try {
                    pImage[i].importData(0, p[i], true);
                } catch (IOException error) {
                    displayError("AlgorithmPrincipalComponents: IOException on pImage[" + i + "] import data");

                    setCompleted(false);
                    setThreadStopped(true);

                    return;
                }

                updateFileInfo(srcImage, pImage[i]);
                pImage[i].clearMask();

                try {
                    imageFrame[i] = new ViewJFrameImage(pImage[i], null, new Dimension(610, 200 + i));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    displayError("AlgorithmPrincipalComponents: Out of memory creating imageFrame[" + i + "]");

                    setCompleted(false);
                    setThreadStopped(true);

                    return;
                }
            } // for (i = nPresent - 1; (i >= 0) && (!threadStopped); i--)

            fireProgressStateChanged(60);

            if (threadStopped) {
                finalize();

                setCompleted(false);

                return;
            }

            pNumber = -1;
            createPNumberDialog(this);

            while (!pressedOK) {

                try {
                    sleep(5L);
                } catch (InterruptedException error) { }
            }

            if (pNumber == -1) {
                displayError("AlgorithmPrincipalComponents: Error on createPNumberDialog");

                setCompleted(false);
                setThreadStopped(true);

                return;
            }
        } // if (displayAndAsk)

        fireProgressStateChanged("Performing inverse transformation");
        // Reconstruct the data with the inverse transform using only the number of principal components chosen Since
        // the rows of eigenvector are orthonormal vectors, the inverse of eignevector is simply equal to the transpose
        // of eigenvector Also remember that we only want the first pNumber rows of eigenvector or the first pNumber
        // columns of eigenInverse

        try {
            eigenInverse = new double[nPlanes][pNumber];
        } catch (OutOfMemoryError e) {
            eigenInverse = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating eigenInverse");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < nPlanes; i++) {

            for (j = 0; j < pNumber; j++) {
                eigenInverse[i][j] = eigenvector[j][i];
            }
        }

        for (i = 0; i < eigenvector.length; i++) {
            eigenvector[i] = null;
        }

        eigenvector = null;
        // p is [nPlanes][samples] so we only want the first pNumber rows
        // of p.

        try {
            pTrunc = new double[pNumber][samples];
        } catch (OutOfMemoryError e) {
            pTrunc = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating pTrunc");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < pNumber; i++) {

            for (j = 0; j < samples; j++) {
                pTrunc[i][j] = p[i][j];
            }
        }

        for (i = 0; i < p.length; i++) {
            p[i] = null;
        }

        p = null;

        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (i = 1; i < 4; i++) {
                        values[(4 * z * samples) + (4 * j) + i] = 0;

                        for (k = 0; k < pNumber; k++) {
                            values[(4 * z * samples) + (4 * j) + i] += (eigenInverse[(3 * z) + i - 1][k] *
                                                                            pTrunc[k][j]) + mean[(3 * z) + i - 1];
                        }
                    }
                }
            } // for (j = 0; j < samples; j++)
        } // if (haveColor)
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                    values[(z * samples) + j] = 0;

                    for (k = 0; k < pNumber; k++) {
                        values[(z * samples) + j] += (eigenInverse[z][k] * pTrunc[k][j]) + mean[z];
                    }
                }
            } // for (j = 0; j < samples; j++)
        } // else not color
        
        if (matchImage != null) {
            matchValues = new float[samples];
            
            try {
                matchImage.exportData(0, samples, matchValues); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm PComponent: matchImage locked");
                setCompleted(false);

                setThreadStopped(true);

                return;
            }
            
            matchMean = 0.0;
            for (j = 0; j < samples; j++) {
                matchMean += matchValues[j];
            }

            matchMean /= samples;
            
            weight = new double[pNumber];
            
            for (i = 0; i < pNumber; i++) {
                for (j = 0; j < samples; j++) {
                    weight[i] += (matchValues[j] - matchMean)*pTrunc[i][j];
                }
            }
            // Normalize weight
            total = 0.0;
            for (i = 0; i < pNumber; i++) {
                total += weight[i]*weight[i];
            }
            total = Math.sqrt(total);
            for (i = 0; i < pNumber; i++) {
                weight[i] /= total;
            }
            maxDiffSquared = Double.MAX_VALUE;
            zClosest = 0;
            if (haveColor) {
                // Normalize eigenInverse
                for (z = 0; z < zDim; z++) {
                    for (i = 1; i < 4; i++) {
                        total = 0.0;
                        for (k = 0; k < pNumber; k++) {
                            total += eigenInverse[(3*z) + i - 1][k]*eigenInverse[(3*z) + i - 1][k];
                        }
                        total = Math.sqrt(total);
                        for (k = 0; k < pNumber; k++) {
                            eigenInverse[(3*z) + i - 1][k] /= total;
                        }
                    }
                }
                cClosest = 1;
                for (z = 0; z < zDim; z++) {
                    for (i = 1; i < 4; i++) {
                        diffSquared = 0.0;
                        for (k = 0; k < pNumber; k++) {
                            diff = (weight[k] - eigenInverse[(3*z) + i - 1][k]);
                            diffSquared += (diff*diff);
                        }
                        if (diffSquared < maxDiffSquared) {
                            maxDiffSquared = diffSquared;
                            zClosest = z;
                            cClosest = i;
                        }
                    } // for (i = 1; i < 4; i++)
                } // for (z = 0; z < zDim; z++)
                if (zDim > 1) {
                    Preferences.debug("The closest slice to the matchImage is z = " + zClosest + "\n", Preferences.DEBUG_ALGORITHM);
                    ViewUserInterface.getReference().setDataText("The closest slice to the matchImage is z = " + zClosest + "\n");  
                }
                if (cClosest == 1) {
                    Preferences.debug("The closest color to the matchImage is red\n");
                    ViewUserInterface.getReference().setDataText("The closest color to the matchImage is red\n");
                }
                else if (cClosest == 2) {
                    Preferences.debug("The closest color to the matchImage is green\n");
                    ViewUserInterface.getReference().setDataText("The closest color to the matchImage is green\n");    
                }
                else {
                    Preferences.debug("The closest color to the matchImage is blue\n");
                    ViewUserInterface.getReference().setDataText("The closest color to the matchImage is blue\n");
                }
            } // if (haveColor)
            else { // not color
                // Normalize eigenInverse
                for (z = 0; z < zDim; z++) {
                    total = 0.0;
                    for (k = 0; k < pNumber; k++) {
                        total += eigenInverse[z][k]*eigenInverse[z][k];
                    }
                    total = Math.sqrt(total);
                    for (k = 0; k < pNumber; k++) {
                        eigenInverse[z][k] /= total;    
                    }       
                }
                for (z = 0; z < zDim; z++) {
                    diffSquared = 0.0;  
                    for (k = 0; k < pNumber; k++) {
                        diff = (weight[k] - eigenInverse[z][k]);
                        diffSquared += (diff*diff);
                    }
                    if (diffSquared < maxDiffSquared) {
                        maxDiffSquared = diffSquared;
                        zClosest = z;
                    }
                } // for (z = 0; z < zDim; z++)
                Preferences.debug("The closest slice to the matchImage is z = " + zClosest + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("The closest slice to the matchImage is z = " + zClosest + "\n");
            } // else not color
            Preferences.debug("The Euclidean squared matching error = " + maxDiffSquared + "\n");
            ViewUserInterface.getReference().setDataText("The Euclidean squared matching error = " + maxDiffSquared + "\n");
        } // if (matchImage != null)

        for (i = 0; i < eigenInverse.length; i++) {
            eigenInverse[i] = null;
        }

        eigenInverse = null;

        for (i = 0; i < pTrunc.length; i++) {
            pTrunc[i] = null;
        }

        pTrunc = null;
        mean = null;
        fireProgressStateChanged(80);

        if (threadStopped) {
            finalize();

            setCompleted(false);

            return;
        }

        iNumber = 0;
        
        if (doFilter) {
            
            vMin = Float.MAX_VALUE;
            vMax = -Float.MAX_VALUE;
            for (i = 0; i < totalLength; i++) {
                  if (values[i] < vMin) {
                      vMin = values[i];
                  }
                  if (values[i] > vMax) {
                      vMax = values[i];
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
            
            if ((vMin < typeMin) || (vMax > typeMax)) {
                scaledValues = new float[totalLength];
                // typeMax = a * vMax + b;
                // typeMin = a * vMin + b;
                a = (typeMax - typeMin)/(vMax - vMin);
                b = typeMax - a * vMax;
                for (i = 0; i < totalLength; i++) {
                    scaledValues[i] = (float)(a * values[i] + b);
                }
            }

            try {
                if ((vMin < typeMin) || (vMax > typeMax)) {
                    destImage[iNumber++].importData(0, scaledValues, true);
                    scaledValues = null;
                }
                else {
                    destImage[iNumber++].importData(0, values, true);
                }
            } catch (IOException error) {
                displayError("AlgorithmPrincipalComponents: IOException on filter destination image import data");

                setCompleted(false);

                return;
            }
        } // if (doFilter)

        if (!doAveraging) {

            setCompleted(true);
        } // if (!doAveraging)

        // Now simply average the reconstructed 3D slices to obtain an average
        fireProgressStateChanged("Averaging reconstructed data");

        try {
            result = new float[samples];
        } catch (OutOfMemoryError e) {
            result = null;
            System.gc();
            displayError("Algorithm Principal component: Out of memory allocating result");
            setCompleted(false);
            setThreadStopped(true);


            return;
        }

        for (i = 0; i < samples; i++) {
            result[i] = 0.0f;
        }

        if (haveColor) {

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {

                    for (i = 1; i < 4; i++) {
                        result[j] += values[(4 * z * samples) + (4 * j) + i];
                    }
                }

                result[j] /= nPlanes;
            }
        } // if (haveColor)
        else { // not color

            for (j = 0; j < samples; j++) {

                for (z = 0; z < zDim; z++) {
                    result[j] += values[(z * samples) + j];
                }

                result[j] /= nPlanes;
            }
        } // else not color

        fireProgressStateChanged(90);
        fireProgressStateChanged("Importing averaged destination data");
        
        rMin = Float.MAX_VALUE;
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
        }

        setCompleted(true);
    }

}
