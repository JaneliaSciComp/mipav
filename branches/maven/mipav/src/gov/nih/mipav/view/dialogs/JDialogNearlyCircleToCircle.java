package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input of a nearly circular region which will be conformally mapped to a circle.
 */
public class JDialogNearlyCircleToCircle extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int[] extents = new int[2];

    /** DOCUMENT ME! */
    private AlgorithmNearlyCircleToCircle ncAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private JTextField xText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogNearlyCircleToCircle object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogNearlyCircleToCircle(ModelImage image) {
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
    public JDialogNearlyCircleToCircle(Frame theParentFrame, ModelImage im) {
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
            //MipavUtil.showHelp("CMNCC001");
            MipavUtil.showWebHelp("Transform:_Conformal_Mapping_Algorithms#Applying_the_Nearly_Circle_to_Circle_algorithm");
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


        if (algorithm instanceof AlgorithmNearlyCircleToCircle) {
            Preferences.debug("Nearly Circle To Circle: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((ncAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmNearlyCircleToCircle)

        if (ncAlgo != null) {
            ncAlgo.finalize();
            ncAlgo = null;
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
            String name = makeImageName(image.getImageName(), "_circle");
            extents[0] = xDim;
            extents[1] = xDim;
            resultImage = new ModelImage(image.getType(), extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            ncAlgo = new AlgorithmNearlyCircleToCircle(resultImage, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            ncAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (ncAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                ncAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Nearly Circle To Circle: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel input1Label;
        JLabel xLabel;
        setForeground(Color.black);
        setTitle("Nearly Circle To Circle Conformal Mapping");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setBorder(buildTitledBorder("Input"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        input1Label = new JLabel("Draw nearly circular region");
        input1Label.setForeground(Color.black);
        input1Label.setFont(serif12);
        inputPanel.add(input1Label, gbc4);

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

        xLabel = new JLabel("X and Y dimensions of output image ");
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

        /*yLabel = new JLabel("Y dimension of output image ");
         * yLabel.setForeground(Color.black); yLabel.setFont(serif12); yLabel.setEnabled(true); gbc6.gridx = 0;
         * gbc6.gridy = 1; paramPanel.add(yLabel, gbc6);
         *
         * yText = new JTextField(10); yText.setText(String.valueOf(image.getExtents()[1])); yText.setFont(serif12);
         * yText.setEnabled(true); gbc6.gridx = 1;paramPanel.add(yText, gbc6);*/

        getContentPane().add(inputPanel, BorderLayout.NORTH);
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
        ViewVOIVector VOIs;
        int nVOIs;
        int nContourVOIs = 0;

        if (!testParameter(xText.getText(), 5, 1000000)) {
            xText.requestFocus();
            xText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xText.getText()).intValue();
        }

        /*if (!testParameter(yText.getText(), 5, 1000000)) {
         *  yText.requestFocus(); yText.selectAll();
         *
         * return false; } else { yDim = Integer.valueOf(yText.getText()).intValue();}*/

        VOIs = image.getVOIs();

        if (VOIs == null) {
            MipavUtil.displayError("No VOIs present.  1 contour VOI needed");

            return false;
        }

        nVOIs = VOIs.size();

        if (nVOIs == 0) {
            MipavUtil.displayError("No VOIs present.  1 contour VOI needed");

            return false;
        }

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                nContourVOIs++;
            }
        }

        if (nContourVOIs == 0) {
            MipavUtil.displayError("No contour VOIs present.  1 contour VOI needed");

            return false;
        }

        if (nContourVOIs > 1) {
            MipavUtil.displayError(nContourVOIs + " contour VOIs present, but only 1 contour VOI allowed");

            return false;
        }

        return true;
    }
}
