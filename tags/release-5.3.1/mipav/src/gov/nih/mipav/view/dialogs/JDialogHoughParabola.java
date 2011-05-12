package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with xv, yv, phi, p output for parabola detection in
 * binary image, where [(y - yv)*cos(phi) - (x - xv)*sin(phi)]**2 =
 * 4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]
 * where vx, vy are the coordinates of the parabola vertex
 * p is the distance between the vertex and focus of the parabola
 */
public class JDialogHoughParabola extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmHoughParabola hAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xvBins;

    /** DOCUMENT ME! */
    private JTextField xvText;

    /** DOCUMENT ME! */
    private int yvBins;
    
    private JTextField yvText;
    
    private int phiBins;

    /** DOCUMENT ME! */
    private JTextField phiText;
    
    private double phiConstant;
    
    private JTextField phiConstantText;
    
    private int pBins;
    
    private JTextField pText;
    
    private float pMin;
    
    private JTextField pMinText;
    
    private float pMax;
    
    private JTextField pMaxText;
    
    private JTextField maxBufferText;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;
    
    private int numParabolas;
    
    private JTextField numParabolasText;
    
    /** Maximum number of points to take from each side of a point on a curve in determining a tangent */
    private int sidePointsForTangent;
    
    private JTextField sideText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughParabola object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogHoughParabola(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHoughParabola(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("HoughPar004");
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }


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


        if (algorithm instanceof AlgorithmHoughParabola) {
            Preferences.debug("Hough Parabola: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((hAlgo.isCompleted() == true) && (resultImage != null)) {


                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // insertScriptLine(algorithm);
        } // if (algorithm instanceof AlgorithmHoughParabola)

        if (hAlgo != null) {
            hAlgo.finalize();
            hAlgo = null;
        }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        
    }


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    private void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_hough_parabola");
            resultImage = new ModelImage(image.getType(), image.getExtents(), name);
            resultImage.setImageName(name);

            // Make algorithm
            hAlgo = new AlgorithmHoughParabola(resultImage, image, xvBins, yvBins,
                                               phiBins, phiConstant, pBins, pMin, pMax,
                                               sidePointsForTangent, maxBufferSize, numParabolas);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            hAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), hAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (hAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                hAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Hough Parabola: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel mainLabel;
        JLabel mainLabel2;
        JLabel xvLabel;
        JLabel yvLabel;
        JLabel phiLabel;
        JLabel phiConstantLabel;
        JLabel pLabel;
        JLabel pMinLabel;
        JLabel pMaxLabel;
        JLabel sideLabel;
        JLabel maxBufferLabel;
        JLabel numParabolasLabel;
        int xDim = Math.min(512, image.getExtents()[0]);
        int yDim = Math.min(512, image.getExtents()[1]);
        int rDim = Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1]));
        setForeground(Color.black);
        setTitle("Hough transform for parabola detection");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Hough transform parameters"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainLabel = new JLabel("[(y - vy)*cos(phi) - (x - vx)*sin(phi)]^2 = ");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc);
        
        mainLabel2 = new JLabel("4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);
        gbc.gridy = 1;
        paramPanel.add(mainLabel2, gbc);

        xvLabel = new JLabel("Desired vx dimension of Hough transform image ");
        xvLabel.setForeground(Color.black);
        xvLabel.setFont(serif12);
        xvLabel.setEnabled(true);
        gbc.gridy = 2;
        paramPanel.add(xvLabel, gbc);

        xvText = new JTextField(10);
        xvText.setText(String.valueOf(2*xDim));
        xvText.setFont(serif12);
        xvText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(xvText, gbc);

        yvLabel = new JLabel("Desired vy dimension of Hough transform image ");
        yvLabel.setForeground(Color.black);
        yvLabel.setFont(serif12);
        yvLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(yvLabel, gbc);

        yvText = new JTextField(10);
        yvText.setText(String.valueOf(2*yDim));
        yvText.setFont(serif12);
        yvText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(yvText, gbc);
        
        phiLabel = new JLabel("Desired phi dimension of Hough transform image ");
        phiLabel.setForeground(Color.black);
        phiLabel.setFont(serif12);
        phiLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(phiLabel, gbc);

        phiText = new JTextField(10);
        phiText.setText("360");
        phiText.setFont(serif12);
        phiText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(phiText, gbc);
        
        phiConstantLabel = new JLabel("Phi value in degrees if phi dimension = 1 ");
        phiConstantLabel.setForeground(Color.black);
        phiConstantLabel.setFont(serif12);
        phiConstantLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(phiConstantLabel, gbc);

        phiConstantText = new JTextField(10);
        phiConstantText.setText("90.0");
        phiConstantText.setFont(serif12);
        phiConstantText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(phiConstantText, gbc);
        
        pLabel = new JLabel("Desired p dimension of Hough transform image ");
        pLabel.setForeground(Color.black);
        pLabel.setFont(serif12);
        pLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(pLabel, gbc);

        pText = new JTextField(10);
        pText.setText(String.valueOf(rDim));
        pText.setFont(serif12);
        pText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(pText, gbc);
        
        pMinLabel = new JLabel("Minimum p value ");
        pMinLabel.setForeground(Color.black);
        pMinLabel.setFont(serif12);
        pMinLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramPanel.add(pMinLabel, gbc);

        pMinText = new JTextField(10);
        pMinText.setText("0.1");
        pMinText.setFont(serif12);
        pMinText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(pMinText, gbc);
        
        pMaxLabel = new JLabel("Maximum p value ");
        pMaxLabel.setForeground(Color.black);
        pMaxLabel.setFont(serif12);
        pMaxLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 8;
        paramPanel.add(pMaxLabel, gbc);

        pMaxText = new JTextField(10);
        pMaxText.setText(String.valueOf(Math.max(image.getExtents()[0], image.getExtents()[1])));
        pMaxText.setFont(serif12);
        pMaxText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(pMaxText, gbc);
        
        sideLabel = new JLabel("Maximum curve points on each side for tangent ");
        sideLabel.setForeground(Color.black);
        sideLabel.setFont(serif12);
        sideLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 9;
        paramPanel.add(sideLabel, gbc);

        sideText = new JTextField(10);
        sideText.setText("3");
        sideText.setFont(serif12);
        sideText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(sideText, gbc);
        
        maxBufferLabel = new JLabel("Maximum Hough transform in megabytes ");
        maxBufferLabel.setForeground(Color.black);
        maxBufferLabel.setFont(serif12);
        maxBufferLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 10;
        paramPanel.add(maxBufferLabel, gbc);
        
        maxBufferText = new JTextField(10);
        maxBufferText.setText("256");
        maxBufferText.setFont(serif12);
        maxBufferText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxBufferText, gbc);

        
        numParabolasLabel = new JLabel("Number of parabolas ");
        numParabolasLabel.setForeground(Color.black);
        numParabolasLabel.setFont(serif12);
        numParabolasLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 11;
        paramPanel.add(numParabolasLabel, gbc);
        
        numParabolasText = new JTextField(3);
        numParabolasText.setText("1");
        numParabolasText.setFont(serif12);
        numParabolasText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(numParabolasText, gbc);

        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (!testParameter(xvText.getText(), 5, 1000000)) {
            xvText.requestFocus();
            xvText.selectAll();

            return false;
        } else {
            xvBins = Integer.valueOf(xvText.getText()).intValue();
        }

        if (!testParameter(yvText.getText(), 5, 1000000)) {
            yvText.requestFocus();
            yvText.selectAll();

            return false;
        } else {
            yvBins = Integer.valueOf(yvText.getText()).intValue();
        }
        
        if (!testParameter(phiText.getText(), 1, 1000000)) {
            phiText.requestFocus();
            phiText.selectAll();

            return false;
        } else {
            phiBins = Integer.valueOf(phiText.getText()).intValue();
        }
        
        if (!testParameter(phiConstantText.getText(), -360.0, 360.0)) {
            phiConstantText.requestFocus();
            phiConstantText.selectAll();

            return false;
        } else {
            phiConstant = Double.valueOf(phiConstantText.getText()).doubleValue() * Math.PI/180.0;
        }
        
        if (!testParameter(pText.getText(), 5, 1000000)) {
            pText.requestFocus();
            pText.selectAll();

            return false;
        } else {
            pBins = Integer.valueOf(pText.getText()).intValue();
        }
        
        if (!testParameter(pMinText.getText(), 1.0E-5, Math.max(image.getExtents()[0], image.getExtents()[1]))) {
            pMinText.requestFocus();
            pMinText.selectAll();

            return false;
        } else {
            pMin = Float.valueOf(pMinText.getText()).floatValue();
        }
        
        if (!testParameter(pMaxText.getText(), pMin, Math.max(image.getExtents()[0], image.getExtents()[1]))) {
            pMaxText.requestFocus();
            pMaxText.selectAll();

            return false;
        } else {
            pMax = Float.valueOf(pMaxText.getText()).floatValue();
        }
        
        if (!testParameter(sideText.getText(), 1, 10)) {
            sideText.requestFocus();
            sideText.selectAll();

            return false;
        } else {
            sidePointsForTangent = Integer.valueOf(sideText.getText()).intValue();
        }
        
        if (!testParameter(maxBufferText.getText(), 1, 10000)) {
            maxBufferText.requestFocus();
            maxBufferText.selectAll();

            return false;
        } else {
            maxBufferSize = Integer.valueOf(maxBufferText.getText()).intValue();
        }
        
        if (!testParameter(numParabolasText.getText(), 1, 100)) {
            numParabolasText.requestFocus();
            numParabolasText.selectAll();

            return false;
        } else {
            numParabolas = Integer.valueOf(numParabolasText.getText()).intValue();
        }

        return true;
    }
}
