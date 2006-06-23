package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogIteratedBlindDeconvolution extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7390265931793649147L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    AlgorithmIteratedBlindDeconvolution ibdAlgor;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private int numberIterations = 100;

    /** DOCUMENT ME! */
    private ModelImage originalImage;

    /** DOCUMENT ME! */
    private ModelImage psfImage = null; // point spread function image

    /** DOCUMENT ME! */
    private JRadioButton radioEntireImage, radioVOIRegion;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textNumberIterations;

    /** DOCUMENT ME! */
    private ViewUserInterface UI = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new JDialogIteratedBlindDeconvolution object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  im              DOCUMENT ME!
     */
    public JDialogIteratedBlindDeconvolution(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        originalImage = im;
        UI = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    } // end JDialogIteratedBlindDeconvolution(...)

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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10086");
        } // end if()-else
    } // end actionPerformed(...)


    /**
     * DOCUMENT ME!
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        if (((algorithm instanceof AlgorithmIteratedBlindDeconvolution) && (ibdAlgor.isCompleted() == true)) &&
                (resultImage != null)) {

            updateFileInfo(originalImage, resultImage);
            resultImage.clearMask();

            try {
                imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }

            insertScriptLine(algorithm);

            ibdAlgor.finalize();
            ibdAlgor = null;
        }
    } // end algorithmPerformed(...)


    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) { }
    }


    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try { }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);

        if (!srcImageKey.equals(destImageKey)) { }
    }


    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name;
        name = makeImageName(originalImage.getImageName(), "_ibd");

        try {
            resultImage = new ModelImage(originalImage.getType(), originalImage.getExtents(), name, UI);

            psfImage = new ModelImage(originalImage.getType(), originalImage.getExtents(), name, UI);

            ibdAlgor = new AlgorithmIteratedBlindDeconvolution(resultImage, originalImage, psfImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            ibdAlgor.addListener(this);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (ibdAlgor.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!UI.isAppFrameVisible()) {
                    ibdAlgor.setProgressBarVisible(false);
                }

                ibdAlgor.run();
            } // end if (runInSeperateThread) {} else {}

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog IteratedBlindDeconvolution: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }

        dispose();
    } // end callAlgorithm()


    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Iterated Blind Deconvolution");

        JPanel parameterPanel = new JPanel(new GridBagLayout());

        parameterPanel.setForeground(Color.black);
        parameterPanel.setBorder(buildTitledBorder("Parameters"));

        // Maximum number of iterations
        JLabel labelNumberIterations = new JLabel("Number of Iterations ");
        labelNumberIterations.setFont(serif12);

        textNumberIterations = new JTextField();
        textNumberIterations.setColumns(5);
        textNumberIterations.setMaximumSize(labelNumberIterations.getPreferredSize());
        textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
        textNumberIterations.setText(Integer.toString(numberIterations));
        textNumberIterations.setFont(serif12);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = gbc.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridy = 0;
        gbc.gridx = 0;
        parameterPanel.add(labelNumberIterations, gbc);
        gbc.gridx = 1;
        parameterPanel.add(textNumberIterations, gbc);


        ButtonGroup regionGroup = new ButtonGroup();

        radioEntireImage = new JRadioButton("Entire image", entireImage);
        radioEntireImage.setFont(MipavUtil.font12);
        radioEntireImage.setActionCommand("EntireImage");
        radioEntireImage.addActionListener(this);
        regionGroup.add(radioEntireImage);

        radioVOIRegion = new JRadioButton("VOI regions", !entireImage);
        radioVOIRegion.setFont(MipavUtil.font12);
        radioVOIRegion.setActionCommand("VOIRegion");
        radioVOIRegion.addActionListener(this);
        regionGroup.add(radioVOIRegion);

        gbc.gridy++;
        gbc.gridx = 0;
        parameterPanel.add(radioEntireImage, gbc);
        gbc.gridx = 1;
        parameterPanel.add(radioVOIRegion, gbc);

        gbc.gridx = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        gbc.fill = gbc.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridy = 0;
        mainPanel.add(parameterPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {
        String tmpStr;
        tmpStr = textNumberIterations.getText();
        numberIterations = Integer.parseInt(tmpStr);

        return true;
    } // end setVariables()


} // end class JDialogIteratedBlindDeconvolution
