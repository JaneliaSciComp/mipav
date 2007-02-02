package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create a filtered image using only selected principal components in the reconstruction and also to create
 * an averaged image slice by simple averaging of the reconstructed image. The source image must be a 3D black and white
 * image or a 2D or 3D color image
 */
public class JDialogPrincipalComponents extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7032654276398964355L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox averagedImageCheckbox;

    /** DOCUMENT ME! */
    private boolean displayAndAsk = true;

    /** DOCUMENT ME! */
    private JCheckBox displayPCheckbox;

    /** DOCUMENT ME! */
    private boolean doAveraging;

    /** DOCUMENT ME! */
    private boolean doFilter;

    /** DOCUMENT ME! */
    private JCheckBox filteredImageCheckbox;

    /** DOCUMENT ME! */
    private ViewJFrameImage[] imageFrame = null;

    /** DOCUMENT ME! */
    private int imageNumber;

    /** DOCUMENT ME! */
    private int imageType;

    /** DOCUMENT ME! */
    private int iNumber;

    /** DOCUMENT ME! */
    private JLabel labelNumber;

    /** DOCUMENT ME! */
    private int nPlanes;

    /** DOCUMENT ME! */
    private AlgorithmPrincipalComponents pComponentAlgo;

    /** DOCUMENT ME! */
    private int pNumber = 1;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private ModelImage srcImage = null; // source image

    /** DOCUMENT ME! */
    private JTextField textNumber;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPrincipalComponents() { }

    /**
     * Creates new dialog to get info to run principal components algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  image           Source image
     */
    public JDialogPrincipalComponents(Frame theParentFrame, ModelImage image) {
        super(theParentFrame, true);
        srcImage = image;
        userInterface = ViewUserInterface.getReference();
        init();
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == displayPCheckbox) {

            if (displayPCheckbox.isSelected()) {
                textNumber.setEnabled(false);
                labelNumber.setEnabled(false);
            } else {
                textNumber.setEnabled(true);
                labelNumber.setEnabled(true);
            }
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
        int i;

        try {
            imageFrame = new ViewJFrameImage[imageNumber];
        } catch (OutOfMemoryError e) {
            imageFrame = null;
            MipavUtil.displayError("JDialogPrincipalComponents: Out of memory allocating imageFrame");

            return;
        }

        if (algorithm instanceof AlgorithmPrincipalComponents) {

            if ((pComponentAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                for (i = 0; i < imageNumber; i++) {

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("ExtractSlices: Out of memory: unable to open new frame #" + i);
                    }
                }
            } // for (i = 0; i < imageNumber; i++)
            else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultImage.length; i++) {
                    resultImage[i].disposeLocal(); // clean up memory
                    resultImage[i] = null;
                }

                resultImage = null;
                System.gc();
            }
        }

        // Update frame
        if (parentFrame != null) {
            ((ViewJFrameBase) parentFrame).updateImages(true);
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        pComponentAlgo.finalize();
        pComponentAlgo = null;
        dispose();
    }

    /**
     * run.
     */
    protected void callAlgorithm() {

        try {
            System.gc();

            imageNumber = 0;

            if (doFilter) {
                imageNumber++;
            }

            if (doAveraging) {
                imageNumber++;
            }

            try {
                resultImage = new ModelImage[imageNumber];
            } catch (OutOfMemoryError e) {
                resultImage = null;
                MipavUtil.displayError("JDialogPrincipalComponents: Out of Memory allocating resultImage array");

                return;
            }

            iNumber = 0;

            if (doFilter) {

                try {
                    resultImage[iNumber++] = new ModelImage(srcImage.getType(), srcImage.getExtents(),
                                                            srcImage.getImageName() + "_PCFiltered",
                                                            srcImage.getUserInterface());
                } catch (OutOfMemoryError e) {

                    for (int i = 0; i < resultImage.length; i++) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }

                    resultImage = null;
                    MipavUtil.displayError("JDialogPrincipalComponents: Out of Memory allocating resultImage for filter");

                    return;
                }
            }

            if (doAveraging) {
                int[] destExtents = new int[2];
                destExtents[0] = srcImage.getExtents()[0];
                destExtents[1] = srcImage.getExtents()[1];

                if (srcImage.getType() == ModelImage.ARGB) {
                    imageType = ModelImage.UBYTE;
                } else if (srcImage.getType() == ModelImage.ARGB_USHORT) {
                    imageType = ModelImage.USHORT;
                } else if (srcImage.getType() == ModelImage.ARGB_FLOAT) {
                    imageType = ModelImage.FLOAT;
                } else {
                    imageType = srcImage.getType();
                }

                try {
                    resultImage[iNumber] = new ModelImage(imageType, destExtents,
                                                          srcImage.getImageName() + "_PCAveraged",
                                                          srcImage.getUserInterface());
                } catch (OutOfMemoryError e) {

                    for (int i = 0; i < resultImage.length; i++) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }

                    resultImage = null;
                    MipavUtil.displayError("JDialogPrincipalComponents: Out of Memory allocating resultImage for averaging");

                    return;
                }
            } // if (doAveraging)

            // Make algorithm:
            try {
                pComponentAlgo = new AlgorithmPrincipalComponents(resultImage, srcImage, doFilter, doAveraging,
                                                                  displayAndAsk, pNumber);
            } catch (OutOfMemoryError e) {
                pComponentAlgo = null;
                MipavUtil.displayError("JDialogPrincipalComponents: Out of Memory allocating pComponentAlgo");

                return;
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            pComponentAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), pComponentAlgo);
            
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (pComponentAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                pComponentAlgo.run();
            }

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (int i = 0; i < resultImage.length; i++) {
                    resultImage[i].disposeLocal(); // Clean up memory of result image
                    resultImage[i] = null;
                }

                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog PComponent: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);
        
        for (int i = 0; i < getResultImage().length; i++) {
            AlgorithmParameters.storeImageInRecorder(getResultImage()[i]);
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_filter", doFilter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_averaging", doAveraging));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_display_and_ask", displayAndAsk));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_components", pNumber));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        userInterface = srcImage.getUserInterface();
        parentFrame = srcImage.getParentFrame();
        
        setDoFilter(scriptParameters.getParams().getBoolean("do_filter"));
        setDoAveraging(scriptParameters.getParams().getBoolean("do_averaging"));
        setDisplayAndAsk(scriptParameters.getParams().getBoolean("do_display_and_ask"));
        setPNumber(scriptParameters.getParams().getInt("num_components"));
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        for (int i = 0; i < getResultImage().length; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  displayAndAsk  DOCUMENT ME!
     */
    public void setDisplayAndAsk(boolean displayAndAsk) {
        this.displayAndAsk = displayAndAsk;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doAveraging  DOCUMENT ME!
     */
    public void setDoAveraging(boolean doAveraging) {
        this.doAveraging = doAveraging;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doFilter  DOCUMENT ME!
     */
    public void setDoFilter(boolean doFilter) {
        this.doFilter = doFilter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pNumber  DOCUMENT ME!
     */
    public void setPNumber(int pNumber) {
        this.pNumber = pNumber;
    }

    /**
     * Initializes GUI components and adds them to the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Principal component");

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Image output"));

        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;

        filteredImageCheckbox = new JCheckBox("Filtered image");
        filteredImageCheckbox.setFont(serif12);
        filteredImageCheckbox.setSelected(true);
        optionsPanel.add(filteredImageCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        averagedImageCheckbox = new JCheckBox("Averaged image plane");
        averagedImageCheckbox.setFont(serif12);
        averagedImageCheckbox.setSelected(true);
        optionsPanel.add(averagedImageCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        displayPCheckbox = new JCheckBox("Display principal components");
        displayPCheckbox.setFont(serif12);
        displayPCheckbox.setSelected(true);
        displayPCheckbox.addActionListener(this);
        optionsPanel.add(displayPCheckbox, gbc);

        if (srcImage.isColorImage()) {

            if (srcImage.getNDims() == 2) {
                nPlanes = 3;
            } else {
                nPlanes = 3 * srcImage.getExtents()[2];
            }
        } else {
            nPlanes = srcImage.getExtents()[2];
        }

        gbc.gridx = 0;
        gbc.gridy = 3;
        labelNumber = new JLabel("Reconstruct using 1 - " + nPlanes + " components");
        labelNumber.setForeground(Color.black);
        labelNumber.setFont(serif12);
        labelNumber.setEnabled(false);
        optionsPanel.add(labelNumber, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 3;
        textNumber = new JTextField(5);
        textNumber.setText("1");
        textNumber.setFont(serif12);
        textNumber.setEnabled(false);
        textNumber.addFocusListener(this);
        optionsPanel.add(textNumber, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(optionsPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        System.gc();

        if (filteredImageCheckbox.isSelected()) {
            doFilter = true;
        } else {
            doFilter = false;
        }

        if (averagedImageCheckbox.isSelected()) {
            doAveraging = true;
        } else {
            doAveraging = false;
        }

        if ((!doFilter) && (!doAveraging)) {
            MipavUtil.displayError("Error: At least one image output must be selected");

            return false;
        }

        if (displayPCheckbox.isSelected()) {
            displayAndAsk = true;
        } else {
            displayAndAsk = false;
        }

        tmpStr = textNumber.getText();
        pNumber = Integer.parseInt(tmpStr);

        if (pNumber < 1) {
            MipavUtil.displayError("Number must be at least 1");
            textNumber.requestFocus();
            textNumber.selectAll();

            return false;
        } else if (pNumber > nPlanes) {
            MipavUtil.displayError("Number must not exceed number of image planes");
            textNumber.requestFocus();
            textNumber.selectAll();

            return false;
        }

        return true;
    }

} // end class JDialogPrincipalComponents
