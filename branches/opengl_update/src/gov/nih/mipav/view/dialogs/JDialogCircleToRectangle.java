package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input of circle center and point on circle curve and output xDim and yDim of rectangle created
 * from transformed circle.
 */
public class JDialogCircleToRectangle extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] extents = new int[2];

    /** DOCUMENT ME! */
    private AlgorithmCircleToRectangle cAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private double[] xSource = new double[2];

    /** DOCUMENT ME! */
    private JTextField xText;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private double[] ySource = new double[2];

    /** DOCUMENT ME! */
    private JTextField yText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogCircleToRectangle object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogCircleToRectangle(ModelImage image) {
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
    public JDialogCircleToRectangle(Frame theParentFrame, ModelImage im) {
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
            //MipavUtil.showHelp("CMCR001");
            MipavUtil.showWebHelp("Transform:_Conformal_Mapping_Algorithms#Applying_the_Circle_to_Rectangle_algorithm");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
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


        if (algorithm instanceof AlgorithmCircleToRectangle) {
            Preferences.debug("Circle To Rectangle: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((cAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmCircleToRectangle)

        if (cAlgo != null) {
            cAlgo.finalize();
            cAlgo = null;
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
            String name = makeImageName(image.getImageName(), "_rectangle");
            extents[0] = xDim;
            extents[1] = yDim;
            resultImage = new ModelImage(image.getType(), extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            cAlgo = new AlgorithmCircleToRectangle(resultImage, image, xSource, ySource);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            cAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (cAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                cAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Circle To Rectangle: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel point1Label;
        JLabel point2Label;
        JLabel xLabel;
        JLabel yLabel;
        setForeground(Color.black);
        setTitle("Circle To Rectangle");

        JPanel pointPanel = new JPanel(new GridBagLayout());
        pointPanel.setBorder(buildTitledBorder("Select Points"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        point1Label = new JLabel("Put point 1 at circle center");
        point1Label.setForeground(Color.black);
        point1Label.setFont(serif12);
        pointPanel.add(point1Label, gbc4);

        point2Label = new JLabel("Put point 2 on circle curve");
        point2Label.setForeground(Color.black);
        point2Label.setFont(serif12);
        gbc4.gridy = 1;
        pointPanel.add(point2Label, gbc4);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Output dimensions"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        xLabel = new JLabel("X dimension of output image ");
        xLabel.setForeground(Color.black);
        xLabel.setFont(serif12);
        xLabel.setEnabled(true);
        paramPanel.add(xLabel, gbc6);

        xText = new JTextField(10);
        xText.setText(String.valueOf(image.getExtents()[0]));
        xText.setFont(serif12);
        xText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(xText, gbc6);

        yLabel = new JLabel("Y dimension of output image ");
        yLabel.setForeground(Color.black);
        yLabel.setFont(serif12);
        yLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 1;
        paramPanel.add(yLabel, gbc6);

        yText = new JTextField(10);
        yText.setText(String.valueOf(image.getExtents()[1]));
        yText.setFont(serif12);
        yText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(yText, gbc6);

        getContentPane().add(pointPanel, BorderLayout.NORTH);
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
        int i;
        Vector<VOIBase> curves;
        int nPts;
        Vector3f[] pts = null;

        if (!testParameter(xText.getText(), 5, 1000000)) {
            xText.requestFocus();
            xText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xText.getText()).intValue();
        }

        if (!testParameter(yText.getText(), 5, 1000000)) {
            yText.requestFocus();
            yText.selectAll();

            return false;
        } else {
            yDim = Integer.valueOf(yText.getText()).intValue();
        }

        if (image.getVOIs() == null) {
            MipavUtil.displayError("No VOIs found.  2 points needed");

            return false;
        }

        if (image.getVOIs().size() == 0) {
            MipavUtil.displayError("No VOIs found.  2 points needed");

            return false;
        }

        curves = image.getVOIs().VOIAt(0).getCurves();
        nPts = curves.size();

        if (nPts != 2) {
            MipavUtil.displayError("Number of points = " + nPts + " instead of required 2");

            return false;
        }

        pts = image.getVOIs().VOIAt(0).exportAllPoints();

        for (i = 0; i < 2; i++) {
            xSource[i] = pts[i].X;
            ySource[i] = pts[i].Y;
        }

        return true;
    }
}
