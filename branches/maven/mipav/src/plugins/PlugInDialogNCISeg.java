import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * NCI segmentation dialog.
 */
public class PlugInDialogNCISeg extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5016769547250838205L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton blueButton = null;

    /** DOCUMENT ME! */
    private JRadioButton brownButton = null;

    /** DOCUMENT ME! */
    private float diffThreshold = 25f;

    /** DOCUMENT ME! */
    private boolean doBrown = false;

    /** DOCUMENT ME! */
    private ModelImage imageA = null; // source image

    /** DOCUMENT ME! */
    private PlugInAlgorithmNCISeg nciAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JLabel threshLabelA = null;

    /** DOCUMENT ME! */
    private JLabel threshLabelB = null;

    /** DOCUMENT ME! */
    private float threshold = 50f;

    /** DOCUMENT ME! */
    private JTextField thresholdFieldA = null;

    /** DOCUMENT ME! */
    private JTextField thresholdFieldB = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor required for dynamic instantiation during script execution.
     */
    public PlugInDialogNCISeg() { }

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public PlugInDialogNCISeg(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, true);

        imageA = imA;
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

        if (event.getSource().equals(brownButton) || event.getSource().equals(blueButton)) {

            if (brownButton.isSelected()) {
                doBrown = true;
                thresholdFieldA.setText("50.0");
                thresholdFieldB.setText("25.0");
            } else {
                doBrown = false;
                thresholdFieldA.setText("240.0");
                thresholdFieldB.setText("10.0");
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        imageA.clearMask();

        if (algorithm instanceof PlugInAlgorithmNCISeg) {

            if (nciAlgo.isCompleted() == true) {
                // The algorithm has completed and produced a new image to be
                // displayed.

                new ViewJFrameImage(resultImage);
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        // crop out the last VOI

        try {

            resultImage = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), (imageA.getImageName() + "_NCI_Seg"));

            // Make algorithm
            nciAlgo = new PlugInAlgorithmNCISeg(resultImage, imageA, doBrown, threshold, diffThreshold);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed of failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            nciAlgo.addListener(this);

            createProgressBar(imageA.getImageName(), " ...", nciAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (nciAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                nciAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        imageA = scriptParameters.retrieveInputImage();

        parentFrame = imageA.getParentFrame();

        doBrown = scriptParameters.getParams().getBoolean("doBrown");
        threshold = scriptParameters.getParams().getFloat("threshold");
        diffThreshold = scriptParameters.getParams().getFloat("diffThreshold");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageA);
        scriptParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("doBrown", doBrown));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("diffThreshold", diffThreshold));
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setTitle("NCI Segmentation");

        ButtonGroup group = new ButtonGroup();

        blueButton = new JRadioButton("Blue", true);
        blueButton.setFont(serif12);
        group.add(blueButton);

        brownButton = new JRadioButton("Brown", false);
        brownButton.setFont(serif12);
        group.add(brownButton);

        blueButton.addActionListener(this);
        brownButton.addActionListener(this);

        JPanel calcPanel = new JPanel();
        calcPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1.0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.gridx = 0;
        calcPanel.add(blueButton, gbc2);

        gbc2.gridx = 1;
        calcPanel.add(brownButton, gbc2);

        calcPanel.setBorder(buildTitledBorder("Color to quantify"));

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        threshLabelA = new JLabel("Threshold value:  ");
        threshLabelA.setFont(serif12);

        thresholdFieldA = new JTextField(4);
        thresholdFieldA.setFont(serif12);
        thresholdFieldA.setText("240.0");
        MipavUtil.makeNumericsOnly(thresholdFieldA, true);

        threshLabelB = new JLabel("Red/green difference threshold:  ");
        threshLabelB.setFont(serif12);

        thresholdFieldB = new JTextField(4);
        thresholdFieldB.setFont(serif12);
        thresholdFieldB.setText("10.0");
        MipavUtil.makeNumericsOnly(thresholdFieldB, true);

        JPanel optionPanel = new JPanel();
        optionPanel.setLayout(new GridBagLayout());
        optionPanel.setBorder(this.buildTitledBorder("Options"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        optionPanel.add(threshLabelA, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        optionPanel.add(thresholdFieldA, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.anchor = GridBagConstraints.WEST;
        optionPanel.add(threshLabelB, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        optionPanel.add(thresholdFieldB, gbc);

        contentBox.add(calcPanel);
        contentBox.add(optionPanel);
        contentBox.add(buildButtons());
        this.getContentPane().add(contentBox);
        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        try {
            threshold = Float.parseFloat(thresholdFieldA.getText());
        } catch (Exception e) {
            MipavUtil.displayError("Threshold must be between 0.0 and 255.0");

            return false;
        }

        try {
            diffThreshold = Float.parseFloat(thresholdFieldB.getText());
        } catch (Exception e) {
            MipavUtil.displayError("Difference threshold must be between 0.0 and 255.0");

            return false;
        }

        return true;
    }
}
