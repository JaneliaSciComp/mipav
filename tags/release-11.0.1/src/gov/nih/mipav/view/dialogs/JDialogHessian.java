package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import javax.swing.*;

import WildMagic.LibFoundation.NumericalAnalysis.Eigenf;


/**
 * Dialog to get user input, then call the algorithm. It should be noted that the algorithms are executed in their own
 * thread.
 */
public class JDialogHessian extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8129223463637379259L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float gaussianScale = 0.5f;

    /** DOCUMENT ME! */
    JLabel hess00, hess01, hess02;

    /** DOCUMENT ME! */
    JLabel hess10, hess11, hess12;

    /** DOCUMENT ME! */
    JLabel hess20, hess21, hess22;

    /** DOCUMENT ME! */
    int locationX = 0, locationY = 0, locationZ = 0;

    /** DOCUMENT ME! */
    JButton runItButton, runItAllButton, runBatchButton;

    /** DOCUMENT ME! */
    int runMethod = 0;

    /** DOCUMENT ME! */
    JTextField textLocX, textLocY, textLocZ, textGaussScale;

    /** DOCUMENT ME! */
    private Eigenf eigenSystemAlgo = null;

    /** DOCUMENT ME! */
    private AlgorithmHessian hessianAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private AlgorithmImageHessian imgHessianAlgor = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHessian object.
     *
     * @param  frame  DOCUMENT ME!
     * @param  im     DOCUMENT ME!
     */
    public JDialogHessian(Frame frame, ModelImage im) {
        super(frame, false);

        image = im;
        init();
    } // end JDialogHessian(...)

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("RunPoint")) {

            if (setVariables()) {
                runMethod = 1;
                callAlgorithm();
            }
        } else if (command.equals("RunImage")) {

            if (setVariables()) {
                runMethod = 2;
                callAlgorithm();
            }
        } else if (command.equals("RunBatch")) {

            if (setVariables()) {
                runMethod = 3;
                callAlgorithm();
            }
        } else if (command.equals("Close")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    } // end actionPerformed(...)

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        // ((ViewJFrameWizard)parentFrame).updateImages(true);

        if (algorithm instanceof AlgorithmImageHessian) {
            image.clearMask();

            if ((imgHessianAlgor.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                // updateFileInfo fails if the resultImage has a different number of dimensions than image
                if (image.getNDims() == resultImage.getNDims()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Median: "+image.getImageName());
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // end if *imgHess.isCompleted() ...)
        } // end if (algorithm ...)
    } // end algorithmPerformed(...)

    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        float[] sigmas = null;
        String name = makeImageName(image.getImageName(), "_vs");

        if (runMethod == 1) {

            if (image.getNDims() == 2) {
                ptHessian2D();
            } else if (image.getNDims() == 3) {
                ptHessian3D();
            }
        } else if ((runMethod == 2) || (runMethod == 3)) {

            try {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name);

                if (image.getNDims() == 2) {
                    sigmas = new float[2];
                    sigmas[0] = sigmas[1] = gaussianScale;
                } else if (image.getNDims() == 3) {
                    sigmas = new float[3];
                    sigmas[0] = sigmas[1] = sigmas[2] = gaussianScale;
                }

                imgHessianAlgor = new AlgorithmImageHessian(resultImage, image, runMethod, sigmas);
                // imgHessianAlgor = new AlgorithmImageHessian(image);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                imgHessianAlgor.addListener(this);

                createProgressBar(image.getImageName(), imgHessianAlgor);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (imgHessianAlgor.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    imgHessianAlgor.run();
                } // end if (isRunInSeparateThread())
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Hessian: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            } // end try()=catch()
        } // end if(runMethod == 1){}-else{}
    } // end callAlgorithm()

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */

    private void init() {

        int width, height, depth;
        int[] extents = image.getExtents();

        width = extents[0];
        height = extents[1];

        if (image.getNDims() == 3) {
            depth = extents[2];
        } else {
            depth = 1;
        }

        setForeground(Color.black);
        setTitle("Hessian");

        // place everything into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // holds all on this dialog
        JPanel locationPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(5, 10, 5, 10);
        locationPanel.setLayout(gbl);

        locationPanel.setForeground(Color.black);
        locationPanel.setBorder(buildTitledBorder("Location"));

        JLabel labelLocX = new JLabel("Location X");

        labelLocX.setForeground(Color.black);
        labelLocX.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelLocX, gbc);
        locationPanel.add(labelLocX);

        textLocX = new JTextField();
        textLocX.setText(String.valueOf(width / 2));
        textLocX.setColumns(5);
        textLocX.setMaximumSize(textLocX.getPreferredSize());
        textLocX.setHorizontalAlignment(JTextField.RIGHT);
        textLocX.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textLocX, gbc);
        locationPanel.add(textLocX);

        JLabel labelLocY = new JLabel("Location Y");

        labelLocY.setForeground(Color.black);
        labelLocY.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelLocY, gbc);
        locationPanel.add(labelLocY);

        textLocY = new JTextField();
        textLocY.setText(String.valueOf(height / 2));
        textLocY.setColumns(5);
        textLocY.setMaximumSize(textLocX.getPreferredSize());
        textLocY.setHorizontalAlignment(JTextField.RIGHT);
        textLocY.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textLocY, gbc);
        locationPanel.add(textLocY);

        JLabel labelLocZ = new JLabel("Location Z");

        labelLocZ.setForeground(Color.black);
        labelLocZ.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelLocZ, gbc);
        locationPanel.add(labelLocZ);

        textLocZ = new JTextField();
        textLocZ.setText(String.valueOf(depth / 2));
        textLocZ.setColumns(5);
        textLocZ.setMaximumSize(textLocX.getPreferredSize());
        textLocZ.setHorizontalAlignment(JTextField.RIGHT);
        textLocZ.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textLocZ, gbc);
        locationPanel.add(textLocZ);

        // Disable the Z stuff for 2D images
        if (image.getNDims() == 2) {
            labelLocZ.setEnabled(false);
            textLocZ.setEnabled(false);
        }

        JLabel labelGaussScale = new JLabel("Gauss Scale");

        labelGaussScale.setForeground(Color.black);
        labelGaussScale.setFont(serif12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelGaussScale, gbc);
        locationPanel.add(labelGaussScale);

        textGaussScale = new JTextField();
        textGaussScale.setText(String.valueOf(gaussianScale));
        textGaussScale.setColumns(5);
        textGaussScale.setMaximumSize(textLocX.getPreferredSize());
        textGaussScale.setHorizontalAlignment(JTextField.RIGHT);
        textGaussScale.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textGaussScale, gbc);
        locationPanel.add(textGaussScale);

        // make the runItButton
        runItButton = new JButton("Run Point");
        runItButton.addActionListener(this);
        runItButton.setActionCommand("RunPoint");
        gbc.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(runItButton, gbc);
        locationPanel.add(runItButton);

        // make the runItAllButton
        runItAllButton = new JButton("Run Image");
        runItAllButton.addActionListener(this);
        runItAllButton.setActionCommand("RunImage");
        gbc.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(runItAllButton, gbc);
        locationPanel.add(runItAllButton);

        // make the runBatch
        runBatchButton = new JButton("Run Batch");
        runBatchButton.addActionListener(this);
        runBatchButton.setActionCommand("RunBatch");
        gbc.anchor = GridBagConstraints.CENTER;
        gbl.setConstraints(runBatchButton, gbc);
        locationPanel.add(runBatchButton);

        // add the filter panel to the setupBox
        setupBox.add(locationPanel);

        JPanel hessianPanel = new JPanel(new GridBagLayout());

        hessianPanel.setForeground(Color.black);
        hessianPanel.setBorder(buildTitledBorder("Hessian"));

        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        hess00 = new JLabel("-------");
        hess00.setForeground(Color.black);
        hess00.setFont(serif12);
        hessianPanel.add(hess00, gbc);

        gbc.gridx++;
        hess01 = new JLabel("-------");
        hess01.setForeground(Color.black);
        hess01.setFont(serif12);
        hessianPanel.add(hess01, gbc);

        gbc.gridx++;
        hess02 = new JLabel("-------");
        hess02.setForeground(Color.black);
        hess02.setFont(serif12);
        hessianPanel.add(hess02, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        hess10 = new JLabel("-------");
        hess10.setForeground(Color.black);
        hess10.setFont(serif12);
        hessianPanel.add(hess10, gbc);

        gbc.gridx++;
        hess11 = new JLabel("-------");
        hess11.setForeground(Color.black);
        hess11.setFont(serif12);
        hessianPanel.add(hess11, gbc);

        gbc.gridx++;
        hess12 = new JLabel("-------");
        hess12.setForeground(Color.black);
        hess12.setFont(serif12);
        hessianPanel.add(hess12, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        hess20 = new JLabel("-------");
        hess20.setForeground(Color.black);
        hess20.setFont(serif12);
        hessianPanel.add(hess20, gbc);

        gbc.gridx++;
        hess21 = new JLabel("-------");
        hess21.setForeground(Color.black);
        hess21.setFont(serif12);
        hessianPanel.add(hess21, gbc);

        gbc.gridx++;
        hess22 = new JLabel("-------");
        hess22.setForeground(Color.black);
        hess22.setFont(serif12);
        hessianPanel.add(hess22, gbc);

        setupBox.add(hessianPanel);

        // make the close button
        JPanel buttonPanel = new JPanel();

        buildCloseButton();
        closeButton.addActionListener(this);
        closeButton.setActionCommand("Close");
        buttonPanel.add(closeButton);
        setupBox.add(buttonPanel);

        getContentPane().add(setupBox, BorderLayout.CENTER);

        // add the setupBox to the dialog
        pack();
        // setSize(new Dimension(300, 500));

        setVisible(true);
        setResizable(true);
        System.gc();
    } // end init()

    /**
     * DOCUMENT ME!
     */
    private void ptHessian2D() {
        DecimalFormat fltFmt = new DecimalFormat("0.00");
        double[][] hess = null;

        // call the appropriate hessian routine and print to the screen
        if (image.getNDims() != 2) {
            return;
        }

        // Make algorithm
        float[] sigmas = new float[2];

        sigmas[0] = sigmas[1] = gaussianScale;
        hessianAlgo = new AlgorithmHessian(image, sigmas);

        // make a 3X3 eigenSolver
        eigenSystemAlgo = new Eigenf(2);

        double[] evals = new double[2];
        double[] magEvals = new double[2];
        double[][] evecs = new double[2][2];

        hess = hessianAlgo.hessian2D(locationX, locationY);

        // set the text in the Dialog to reflect the values of the Hessian
        hess00.setText(fltFmt.format(hess[0][0]));
        hess01.setText(fltFmt.format(hess[0][1]));

        hess10.setText(fltFmt.format(hess[1][0]));
        hess11.setText(fltFmt.format(hess[1][1]));

        // fill up the eigenSolver matrix with the hessian
        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);

        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);

        // OK, solve the eigen system
        eigenSystemAlgo.IncrSortEigenStuff();

        // extract the eigenvalues and vectors from the AlgorithmEigensolver
        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
        magEvals[0] = Math.abs(evals[0]);
        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
        magEvals[1] = Math.abs(evals[1]);

        evecs[0][0] = eigenSystemAlgo.GetEigenvector(0, 0);
        evecs[1][0] = eigenSystemAlgo.GetEigenvector(1, 0);

        evecs[0][1] = eigenSystemAlgo.GetEigenvector(0, 1);
        evecs[1][1] = eigenSystemAlgo.GetEigenvector(1, 1);

        double tmp;

        // put the smallest magnitude eigenvalue in element zero
        if (magEvals[1] < magEvals[0]) {
            tmp = evals[0];
            evals[0] = evals[1];
            evals[1] = tmp;
            tmp = magEvals[0];
            magEvals[0] = magEvals[1];
            magEvals[1] = tmp;
            tmp = evecs[0][0];
            evecs[0][0] = evecs[0][1];
            evecs[0][1] = tmp;
            tmp = evecs[1][0];
            evecs[1][0] = evecs[1][1];
            evecs[1][1] = tmp;
        }

        //System.out.print("lambda 1: " + fltFmt.format(evals[0]) + "   ");
        //System.out.print("evec 1: " + fltFmt.format(evecs[0][0]) + "   " + fltFmt.format(evecs[1][0]));
        //System.out.println("");

        //System.out.print("lambda 2: " + fltFmt.format(evals[1]) + "   ");
        //System.out.print("evec 2: " + fltFmt.format(evecs[0][1]) + "   " + fltFmt.format(evecs[1][1]));
        //System.out.println("");

    } // end ptHessian2D()

    /**
     * DOCUMENT ME!
     */
    private void ptHessian3D() {
        DecimalFormat fltFmt = new DecimalFormat("0.00");
        double[][] hess = null;

        // call the appropriate hessian routine and print to the screen
        if (image.getNDims() != 3) {
            return;
        }

        // Make algorithm
        float[] sigmas = new float[3];

        sigmas[0] = sigmas[1] = sigmas[2] = gaussianScale;
        hessianAlgo = new AlgorithmHessian(image, sigmas);

        // make a 3X3 eigenSolver
        eigenSystemAlgo = new Eigenf(3);

        double[] evals = new double[2];
        double[] magEvals = new double[2];
        //double[][] evecs = new double[2][2];

        hess = hessianAlgo.hessian3D(locationX, locationY, locationZ);

        /*
         * // add 1 into the zcomponent so the user thinks silce 0 is really slice 1 System.out.println("\nLocation:  X:
         * " + locationX +"  Y: " + locationY + "  Z: " + (locationZ+1)); System.out.println("Hessian: " +
         * fltFmt.format(hess[0][0]) + "  " + fltFmt.format(hess[0][1]) + "  " + fltFmt.format(hess[0][2]));
         * System.out.println("         " + fltFmt.format(hess[1][0]) + "  " + fltFmt.format(hess[1][1]) + "  " +
         * fltFmt.format(hess[1][2])); System.out.println("         " + fltFmt.format(hess[2][0]) + "  " +
         * fltFmt.format(hess[2][1]) + "  " + fltFmt.format(hess[2][2]));
         */
        // set the text in the Dialog to reflect the values of the Hessian
        hess00.setText(fltFmt.format(hess[0][0]));
        hess01.setText(fltFmt.format(hess[0][1]));
        hess02.setText(fltFmt.format(hess[0][2]));

        hess10.setText(fltFmt.format(hess[1][0]));
        hess11.setText(fltFmt.format(hess[1][1]));
        hess12.setText(fltFmt.format(hess[1][2]));

        hess20.setText(fltFmt.format(hess[2][0]));
        hess21.setText(fltFmt.format(hess[2][1]));
        hess22.setText(fltFmt.format(hess[2][2]));

        // fill up the eigenSolver matrix with the hessian
        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);
        eigenSystemAlgo.SetData(0, 2, (float)hess[0][2]);

        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);
        eigenSystemAlgo.SetData(1, 2, (float)hess[1][2]);

        eigenSystemAlgo.SetData(2, 0, (float)hess[2][0]);
        eigenSystemAlgo.SetData(2, 1, (float)hess[2][1]);
        eigenSystemAlgo.SetData(2, 2, (float)hess[2][2]);

        // OK, solve the eigen system
        eigenSystemAlgo.IncrSortEigenStuff();

        // extract the eigen values from the AlgorithmEigensolver
        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
        magEvals[0] = Math.abs(evals[0]);
        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
        magEvals[1] = Math.abs(evals[1]);
        evals[2] = eigenSystemAlgo.GetEigenvalue(2);
        magEvals[2] = Math.abs(evals[2]);

        int[] lut = new int[3];

        lut[0] = 0;
        lut[1] = 1;
        lut[2] = 2;

        int tmp;

        double blobRatio, areaRatio, structureness;
        @SuppressWarnings("unused")
        double vesselness;
        double expRa = 0, expRb = 0, expS = 0;

        double a = 0.5f;
        double b = 0.5f;
        double c = 14.0f;

        // put the smallest eigen val in the 0 element
        if (magEvals[1] < magEvals[0]) {
            tmp = lut[0];
            lut[0] = lut[1];
            lut[1] = tmp;
        }

        if (magEvals[2] < magEvals[0]) {
            tmp = lut[0];
            lut[0] = lut[2];
            lut[2] = tmp;
        }

        // put the next smallest element in the 1st element
        if (magEvals[2] < magEvals[1]) {
            tmp = lut[1];
            lut[1] = lut[2];
            lut[2] = tmp;
        }

        //System.out.println("Sorted Eigenvalues");
        //System.out.print("lambda 1: " + fltFmt.format(evals[lut[0]]) + "  ");
        //System.out.print("lambda 2: " + fltFmt.format(evals[lut[1]]) + "  ");
        //System.out.println("lambda 3: " + fltFmt.format(evals[lut[2]]));

        blobRatio = magEvals[lut[0]] / (Math.sqrt(magEvals[lut[1]] * magEvals[lut[2]]));
        areaRatio = magEvals[lut[1]] / magEvals[lut[2]];
        structureness = Math.sqrt((evals[lut[0]] * evals[lut[0]]) + (evals[lut[1]] * evals[lut[1]]) +
                                  (evals[lut[2]] * evals[lut[2]]));

        /*
         * System.out.print("blobRatio: " + fltFmt.format(magEvals[lut[0]]) + " / sqrt( " +
         * fltFmt.format(magEvals[lut[1]]) + " * " + fltFmt.format(magEvals[lut[2]]) + " )"); System.out.println("   " +
         * fltFmt.format(blobRatio));
         *
         * System.out.print("areaRatio: " + fltFmt.format(magEvals[lut[1]]) + " / " + fltFmt.format(magEvals[lut[2]]));
         * System.out.println("   " + fltFmt.format(areaRatio)); System.out.println("structureness: " +
         * fltFmt.format(structureness));
         */
        if ((evals[lut[1]] > 0) || (evals[lut[2]] > 0)) {
            vesselness = 0.0f;
        } else {
            expRa = Math.exp(-((areaRatio * areaRatio) / (2 * a * a)));
            expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
            expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
            vesselness = (1.0 - expRa) * expRb * (1.0 - expS);
        }

        /*
         * System.out.println("expRa: " + fltFmt.format(expRa) + "  (1.0 - expRa): " + fltFmt.format((1.0 - expRa)));
         * System.out.println("expRb: " + fltFmt.format(expRb)); System.out.println("expS: " + fltFmt.format(expS) + "
         * (1.0 - expS): " + fltFmt.format((1.0 - expS))); System.out.println("vesselness: " +
         * fltFmt.format(vesselness));
         *
         * System.out.println("");
         */

        /*
         * // extract the eigen vectors from the AlgorithmEigensolver
         *
         * evecs[0][0] = eigenSystemAlgo.getEigenvector(0, 0); evecs[1][0] = eigenSystemAlgo.getEigenvector(1, 0);
         * evecs[2][0] = eigenSystemAlgo.getEigenvector(2, 0);
         *
         * evecs[0][1] = eigenSystemAlgo.getEigenvector(0, 1); evecs[1][1] = eigenSystemAlgo.getEigenvector(1, 1);
         * evecs[2][1] = eigenSystemAlgo.getEigenvector(2, 1);
         *
         * evecs[0][2] = eigenSystemAlgo.getEigenvector(0, 2); evecs[1][2] = eigenSystemAlgo.getEigenvector(1, 2);
         * evecs[2][2] = eigenSystemAlgo.getEigenvector(2, 2);
         *
         * System.out.println("------------------------------------------------------------------");
         * System.out.println("Eigenvalues             Eigenvectors"); System.out.println(fltFmt.format(evals[lut[0]]) +
         * "      [ " + fltFmt.format(evecs[0][lut[0]]) + "   " + fltFmt.format(evecs[1][lut[0]]) + "   " +
         * fltFmt.format(evecs[2][lut[0]]) +" ]"); System.out.println(fltFmt.format(evals[lut[1]]) + "      [ " +
         * fltFmt.format(evecs[0][lut[1]]) + "   " + fltFmt.format(evecs[1][lut[1]]) + "   " +
         * fltFmt.format(evecs[2][lut[1]]) +" ]"); System.out.println(fltFmt.format(evals[lut[2]]) + "      [ " +
         * fltFmt.format(evecs[0][lut[2]]) + "   " + fltFmt.format(evecs[1][lut[2]]) + "   " +
         * fltFmt.format(evecs[2][lut[2]]) +" ]"); System.out.println("");
         */
    } // end ptHessian3D()

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        // sanity check to insure we can call the hessian algorithm
        if ((image.getNDims() != 2) && (image.getNDims() != 3)) {
            return false;
        }

        // read strings from the text fields and convert to an integers
        String tmpStr = textLocX.getText();

        locationX = Integer.valueOf(tmpStr).intValue();

        tmpStr = textLocY.getText();
        locationY = Integer.valueOf(tmpStr).intValue();

        if (image.getNDims() == 3) {

            // subtract one since the slider is offset by one
            tmpStr = textLocZ.getText();
            locationZ = Integer.valueOf(tmpStr).intValue() - 1;
        }

        tmpStr = textGaussScale.getText();
        gaussianScale = Float.valueOf(tmpStr).floatValue();

        return true;
    } // end setVariables()

}
