package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The algorithm concatenates the images. The user has the option to
 * generate a new image or replace the source image. It should be noted that the algorithms are executed in their own
 * thread. See AlgorithmConcat comment for further information.
 *
 * @version  0.1 Dec 21, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmConcat
 */
public class JDialogConcat extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3284724231720657011L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private int displayLoc = NEW;

    /** DOCUMENT ME! */
    private boolean do3D = true;

    /** DOCUMENT ME! */
    private ModelImage imageA; // source image

    /** DOCUMENT ME! */
    private ModelImage imageB;

    /** DOCUMENT ME! */
    private AlgorithmConcat mathAlgo;

    /** DOCUMENT ME! */
    private JRadioButton radio3D;

    /** DOCUMENT ME! */
    private JRadioButton radio4D;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String selectedName = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConcat() { }

    /**
     * Creates new concatenatation dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogConcat(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        imageA = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
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
            //MipavUtil.showHelp("U4049");
            MipavUtil.showWebHelp("Slice_tools#Concatenating_images");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmConcat) {

            if ((mathAlgo.isCompleted() == true) && (mathAlgo.getResultImage() != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (displayLoc == NEW) {

                    try {
                        resultImage = mathAlgo.getResultImage();

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else {

                    // These next lines set the titles in all frames where the source image is displayed to
                    // image name so as to indicate that the image is now unlocked!
                    // The image frames are enabled and then registed to the userinterface.
                    resultImage = mathAlgo.getResultImage();

                    Vector<ViewImageUpdateInterface> imageFrames = imageA.getImageFrameVector();

                    for (int i = 0; i < imageFrames.size(); i++) {
                        ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                        if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                            userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                        }
                    }

                    Point pt;

                    if (parentFrame != null) {
                        pt = ((ViewJFrameBase) parentFrame).getLocation();
                    } else {
                        pt = new Point(Toolkit.getDefaultToolkit().getScreenSize().width / 2,
                                       Toolkit.getDefaultToolkit().getScreenSize().height / 2);
                    }

                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(pt.x, pt.y));

                    if (parentFrame != null) {
                        ((ViewJFrameBase) parentFrame).close();
                    } else {
                        ((ViewJFrameBase) imageA.getParentFrame()).close();
                    }

                    // Not so sure about this.
                    if (imageA.getLightBoxFrame() != null) {

                        try {
                            pt = imageA.getLightBoxFrame().getLocation();
                            imageA.getLightBoxFrame().close();
                            new ViewJFrameLightBox(imageFrame, "LightBox", resultImage,
                                                   imageFrame.getComponentImage().getLUTa(),
                                                   imageFrame.getComponentImage().getImageB(),
                                                   imageFrame.getComponentImage().getLUTb(),
                                                   imageFrame.getComponentImage().getResolutionX(),
                                                   imageFrame.getComponentImage().getResolutionY(),
                                                   new Dimension(pt.x, pt.y), imageFrame.getControls(), 
                                                   imageFrame.getVOIManager());
                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    }
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                /*Vector imageFrames = imageA.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));

                    }
                }*/

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                imageA.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        mathAlgo.finalize();
        mathAlgo = null;
        dispose();
    }

    /**
     * dispose memory.
     */
    public void disposeLocal() {

        if (mathAlgo != null) {
            mathAlgo.finalize();
            mathAlgo = null;
        }

        if (imageA != null) {
            imageA.disposeLocal();
        }

        imageA = null;

        if (imageB != null) {
            imageB.disposeLocal();
        }

        imageB = null;

        if (resultImage != null) {
            resultImage.disposeLocal();
        }

        resultImage = null;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxImage) {
            selectedName = (String) comboBoxImage.getSelectedItem();
            imageB = userInterface.getRegisteredImageByName(selectedName);

            if ((imageA.getNDims() >= 4) || (imageB.getNDims() >= 4)) {
                radio3D.setSelected(false);
                radio4D.setSelected(true);
                radio3D.setEnabled(false);
                radio4D.setEnabled(true);
            } else if ((imageA.getNDims() == 3) && (imageB.getNDims() == 3) &&
                           (imageA.getExtents()[2] == imageB.getExtents()[2])) {
                radio3D.setSelected(true);
                radio4D.setSelected(false);
                radio3D.setEnabled(true);
                radio4D.setEnabled(true);
            } else {
                radio3D.setSelected(true);
                radio4D.setSelected(false);
                radio3D.setEnabled(true);
                radio4D.setEnabled(false);
            }

        }
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets imageB.
     *
     * @param  im  Image B.
     */
    public void setImageB(ModelImage im) {
        imageB = im;
    }

    /**
     * Once all the necessary variables are set, call the Concat algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if ((imageA.getNDims() <= 4) && (imageB.getNDims() <= 4)) {
            int[] destExtents = null;
            ModelImage destImage = null;

            if ((imageA.getNDims() == 2) && (imageB.getNDims() == 2)) {
                destExtents = new int[3];
                destExtents[0] = imageA.getExtents()[0];
                destExtents[1] = imageA.getExtents()[1];
                destExtents[2] = 2;
            } else if (((imageA.getNDims() == 2) && (imageB.getNDims() == 3)) ||
                           ((imageA.getNDims() == 3) && (imageB.getNDims() == 2))) {
                destExtents = new int[3];
                destExtents[0] = imageA.getExtents()[0];
                destExtents[1] = imageA.getExtents()[1];

                if (imageA.getNDims() > imageB.getNDims()) {
                    destExtents[2] = imageA.getExtents()[2] + 1;
                } else {
                    destExtents[2] = imageB.getExtents()[2] + 1;
                }
            } else if ((imageA.getNDims() == 3) && (imageB.getNDims() == 3) && do3D) {
                destExtents = new int[3];
                destExtents[0] = imageA.getExtents()[0];
                destExtents[1] = imageA.getExtents()[1];
                destExtents[2] = imageA.getExtents()[2] + imageB.getExtents()[2];
            } else if ((imageA.getNDims() == 3) && (imageB.getNDims() == 3) && !do3D) {
                destExtents = new int[4];
                destExtents[0] = imageA.getExtents()[0];
                destExtents[1] = imageA.getExtents()[1];
                destExtents[2] = imageA.getExtents()[2];
                destExtents[3] = 2;
            } else if (((imageA.getNDims() == 3) && (imageB.getNDims() == 4)) ||
                           ((imageA.getNDims() == 4) && (imageB.getNDims() == 3)) ||
                           ((imageA.getNDims() == 4) && (imageB.getNDims() == 4))) {
                destExtents = new int[4];
                destExtents[0] = imageA.getExtents()[0];
                destExtents[1] = imageA.getExtents()[1];
                destExtents[2] = imageA.getExtents()[2];

                if (imageA.getNDims() > imageB.getNDims()) {
                    destExtents[3] = imageA.getExtents()[3] + 1;
                } else if (imageB.getNDims() > imageA.getNDims()) {
                    destExtents[3] = imageB.getExtents()[3] + 1;
                } else {
                    destExtents[3] = imageA.getExtents()[3] + imageB.getExtents()[3];
                }
            } else {
                MipavUtil.displayError("The two images are not of the proper\ndimensionality to be concatenated.");

                return;
            }

            destImage = new ModelImage(imageA.getType(), destExtents, makeImageName(imageA.getImageName(), "_concat"));

            try {

                // Make algorithm
                mathAlgo = new AlgorithmConcat(imageA, imageB, destImage);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                mathAlgo.addListener(this);

                createProgressBar(imageA.getImageName(), mathAlgo);

                // Hide dialog
                setVisible(false);

                if (displayLoc == REPLACE) {

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = imageA.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (mathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }

                } else {

                    mathAlgo.run();

                }
            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog Concatenation: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        imageA = scriptParameters.retrieveInputImage(1);
        imageB = scriptParameters.retrieveInputImage(2);

        userInterface = ViewUserInterface.getReference();
        parentFrame = imageA.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {

            // replace processing not supported..
            // setDisplayLocReplace();
            setDisplayLocNew();
        }

        do3D = scriptParameters.getParams().getBoolean("do_3D_concat");
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageA);
        scriptParameters.storeInputImage(imageB);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_3D_concat", do3D));
    }

    /**
     * Builds a list of images to concatenate to image A.
     */
    private void buildComboBoxImage() {
        ViewUserInterface UI;

        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        // Add images from user interface that have the same correct dimensionality
        // Possibilities: 2D-2D,2D-3D,3D-2D,3D-3D,3D-4D,4D-3D,4D-4D
        // Note that 3D-3D could create a 3D or a 4D image.
        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
            ModelImage img = UI.getRegisteredImageByName(name);

            if (UI.getFrameContainingImage(img) != null) {

                if (!imageA.getImageName().equals(name)) {

                    if (imageA.isColorImage() == img.isColorImage()) {

                        if (((imageA.getNDims() == 2) || (imageA.getNDims() == 3)) &&
                                ((img.getNDims() == 2) || (img.getNDims() == 3)) &&
                                (imageA.getExtents()[0] == img.getExtents()[0]) &&
                                (imageA.getExtents()[1] == img.getExtents()[1])) {
                            comboBoxImage.addItem(name);
                        } else if (((imageA.getNDims() == 4) || (img.getNDims() == 4)) && (imageA.getNDims() >= 3) &&
                                       (img.getNDims() >= 3) && (imageA.getExtents()[0] == img.getExtents()[0]) &&
                                       (imageA.getExtents()[1] == img.getExtents()[1]) &&
                                       (imageA.getExtents()[2] == img.getExtents()[2])) {
                            comboBoxImage.addItem(name);
                        }
                    } // if (imageA.isColorImage() == img.isColorImage())
                } // if (!imageA.getImageName().equals(name))
            } // if (UI.getFrameContainingImage(img) != null)
        } // while (names.hasMoreElements())
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Concatenate Images");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("ImageB appended to ImageA"));

        JLabel labelUse = new JLabel("Image A:");
        labelUse.setForeground(Color.black);
        labelUse.setFont(serif12);

        JLabel labelImageA = new JLabel(imageA.getImageName());
        labelImageA.setForeground(Color.black);
        labelImageA.setFont(serif12);

        JLabel labelImageB = new JLabel("Image B: ");
        labelImageB.setForeground(Color.black);
        labelImageB.setFont(serif12);

        buildComboBoxImage();

        if (comboBoxImage.getItemCount() == 0) {
            MipavUtil.displayError("There are no images with the proper dimensions\nto concatenate to" +
                                   imageA.getImageName() + ".");

            return;
        }

        selectedName = (String) comboBoxImage.getSelectedItem();
        imageB = userInterface.getRegisteredImageByName(selectedName);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(5, 5, 5, 5);
        inputPanel.add(labelUse, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(labelImageA, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        inputPanel.add(labelImageB, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(comboBoxImage, gbc);

        JPanel dimensionPanel = new JPanel(new GridBagLayout());
        dimensionPanel.setForeground(Color.black);
        dimensionPanel.setBorder(buildTitledBorder("Dimensionality of Result"));

        ButtonGroup groupDim = new ButtonGroup();
        radio3D = new JRadioButton("3D");
        radio3D.setFont(serif12);
        groupDim.add(radio3D);

        radio4D = new JRadioButton("4D");
        radio4D.setFont(serif12);
        groupDim.add(radio4D);

        if ((imageA.getNDims() >= 4) || (imageB.getNDims() >= 4)) {
            radio3D.setSelected(false);
            radio4D.setSelected(true);
            radio3D.setEnabled(false);
            radio4D.setEnabled(true);
        } else if ((imageA.getNDims() == 3) && (imageB.getNDims() == 3) &&
                       (imageA.getExtents()[2] == imageB.getExtents()[2])) {
            radio3D.setSelected(true);
            radio4D.setSelected(false);
            radio3D.setEnabled(true);
            radio4D.setEnabled(true);
        } else {
            radio3D.setSelected(true);
            radio4D.setSelected(false);
            radio3D.setEnabled(true);
            radio4D.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        dimensionPanel.add(radio3D, gbc);
        gbc.gridy = 1;
        dimensionPanel.add(radio4D, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(dimensionPanel, BorderLayout.CENTER);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel);
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
        selectedName = (String) comboBoxImage.getSelectedItem();
        imageB = userInterface.getRegisteredImageByName(selectedName);

        if (radio3D.isSelected()) {
            do3D = true;
        } else {
            do3D = false;
        }

        displayLoc = NEW;

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Utilities.Slice tools");
            }

            public String getDescription() {
                return new String("Concatenates two images.");
            }

            public String getDescriptionLong() {
                return new String("Concatenates two images.");
            }

            public String getShortLabel() {
                return new String("Concat");
            }

            public String getLabel() {
                return new String("Concatenate");
            }

            public String getName() {
                return new String("Concatenate");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
   public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        /*imageA = scriptParameters.retrieveInputImage(1);
        imageB = scriptParameters.retrieveInputImage(2);

        userInterface = ViewUserInterface.getReference();
        parentFrame = imageA.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {

            // replace processing not supported..
            // setDisplayLocReplace();
            setDisplayLocNew();
        }

        do3D = scriptParameters.getParams().getBoolean("do_3D_concat");*/
        
        try {
        	
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));
            table.put(new ParameterBoolean("do_3D_concat", false));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));    
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
                return getResultImage().getImageName();
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
    
}
