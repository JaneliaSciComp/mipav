package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user can select having the algorithm applied to whole image
 * or to the VOI regions. It should be noted that the algorithms are executed in their own thread.
 *
 * @version  1.0; April 26, 2013
 */
public class JDialogSobel extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private AlgorithmSobel sobelAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    /** Flag indicating if slices should be blurred independently. */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;
    
    private JComboBox comboBoxKernelSize;
    
    private int kernelSize;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSobel() { }

    /**
     * Creates a new JDialogSobel object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSobel(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == image25DCheckbox) {
            if (image25DCheckbox.isSelected()) {
                // kernel size
                int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
                comboBoxKernelSize.removeAllItems();
                buildKernelSizeComboBox(true);
                comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection    
            }
            else {
                // kernel Size
                int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
                comboBoxKernelSize.removeAllItems();
                buildKernelSizeComboBox(false);
                // comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection
                comboBoxKernelSize.setSelectedIndex(0);
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showWebHelp("");
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

        if (algorithm instanceof AlgorithmSobel) {
            System.err.println("Sobel Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((sobelAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                try {

                    // resultImage.setImageName("Median: "+image.getImageName());
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
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
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        sobelAlgo.finalize();
        sobelAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }

    /**
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_sobelGradient");
        
        int destExtents[] = null;
        if (image.getNDims() == 2) {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0] - (kernelSize - 1);
            destExtents[1] = image.getExtents()[1] - (kernelSize - 1);
            destExtents[2] = 2;
        }
        else if ((image.getNDims() == 3) && image25D) {
            destExtents = new int[4];
            destExtents[0] = image.getExtents()[0] - (kernelSize - 1);
            destExtents[1] = image.getExtents()[1] - (kernelSize - 1);
            destExtents[2] = image.getExtents()[2];
            destExtents[3] = 2;
        }
        else if (image.getNDims() == 3) {
            destExtents = new int[4];
            destExtents[0] = image.getExtents()[0] - (kernelSize - 1);
            destExtents[1] = image.getExtents()[1] - (kernelSize - 1);
            destExtents[2] = image.getExtents()[2] - (kernelSize - 1);
            destExtents[3] = 3;    
        }
        resultImage = new ModelImage(ModelStorageBase.DOUBLE, destExtents, name);
        try {
            sobelAlgo = new AlgorithmSobel(resultImage, image, image25D, kernelSize);
            
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sobelAlgo.addListener(this);

            createProgressBar(image.getImageName(), sobelAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (sobelAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                sobelAlgo.run();
            }
        }
        catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Sobel: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
           
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }
    
    /**
     * Accessor that sets the kernel size.
     *
     * @param  size  Value to set size to (3 == 3x3, 5 == 5x5, etc.)
     */
    public void setKernelSize(int size) {
        kernelSize = size;
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }
        
        image25D = scriptParameters.doProcess3DAs25D();
        setKernelSize(scriptParameters.getParams().getInt("kernel_size"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), true);
        
        scriptParameters.storeProcess3DAs25D(image25D);
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        
    }

   

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Sobel Gradient Components");

        // place everything setting up the kernel & filtering into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // maskPanel holds all the "Options"
        JPanel maskPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        maskPanel.setLayout(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;

        maskPanel.setForeground(Color.black);
        maskPanel.setBorder(buildTitledBorder("Options")); // set the border ... "Options"
        
        if (image.getNDims() == 3) {
            image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
            image25DCheckbox.setFont(serif12);
            image25DCheckbox.setForeground(Color.black);
            image25DCheckbox.addActionListener(this);
            image25DCheckbox.setSelected(false);
            
            gbc.gridwidth = 2;
            gbc.anchor = GridBagConstraints.WEST;
            gbl.setConstraints(image25DCheckbox, gbc);
            maskPanel.add(image25DCheckbox);
            gbc.gridy++;
        }
        
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        JLabel labelKernelSize = createLabel("Kernel size:"); // make & set a label
        gbl.setConstraints(labelKernelSize, gbc);
        maskPanel.add(labelKernelSize); // add kernel label

        gbc.gridx = 1;
        comboBoxKernelSize = new JComboBox();
        comboBoxKernelSize.setFont(serif12);
        comboBoxKernelSize.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(comboBoxKernelSize, gbc);

        if (image.getNDims() == 2) {
            this.buildKernelSizeComboBox(true); // 2D images MUST be slice filtered
        } else {
            this.buildKernelSizeComboBox(false); // default setting for 3D+ images is volume filtering
        }

        maskPanel.add(comboBoxKernelSize); // add the comboboxt to the panel

        setupBox.add(maskPanel); // the parameters-panel is at the top of the box

        getContentPane().add(setupBox, BorderLayout.CENTER); // put the setupBox into the dialog

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
        //setResizable(false);
        System.gc();

    }
    
    /**
     * Creates the combo-box that allows user to select the size of the kernel (mask).
     *
     * @param  singleSlices  DOCUMENT ME!
     */
    private void buildKernelSizeComboBox(boolean singleSlices) {

        if (singleSlices) {
            comboBoxKernelSize.addItem("3x3");
            comboBoxKernelSize.addItem("5x5");
            //comboBoxKernelSize.addItem("7x7");
            //comboBoxKernelSize.addItem("9x9");
            //comboBoxKernelSize.addItem("11x11");
        } else {
            comboBoxKernelSize.addItem("3x3x3");
            //comboBoxKernelSize.addItem("5x5x5");
            //comboBoxKernelSize.addItem("7x7x7");
            //comboBoxKernelSize.addItem("9x9x9");
            //comboBoxKernelSize.addItem("11x11x11");
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        
        if (image25DCheckbox != null) {
            image25D = image25DCheckbox.isSelected();
        }
        else {
            image25D = true;
        }
        
        // associate kernel size with selectBox choice.
        this.determineKernelSize();

        return true;
    }
    
    /**
     * Associate one side of the kernel size with selectBox choice.
     */
    private void determineKernelSize() {

        if (comboBoxKernelSize.getSelectedIndex() == 0) {
            kernelSize = 3;
        } else if (comboBoxKernelSize.getSelectedIndex() == 1) {
            kernelSize = 5;
        } else if (comboBoxKernelSize.getSelectedIndex() == 2) {
            kernelSize = 7;
        } else if (comboBoxKernelSize.getSelectedIndex() == 3) {
            kernelSize = 9;
        } else if (comboBoxKernelSize.getSelectedIndex() == 4) {
            kernelSize = 11;
        }
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms Filters (spatial) Sobel gradient components");
            }

            public String getDescription() {
                return new String("Finds Sobel gradient components.");
            }

            public String getDescriptionLong() {
                return new String("Finds Sobel gradient components.");
            }

            public String getShortLabel() {
                return new String("Sobel gradient");
            }

            public String getLabel() {
                return new String("Sobel gradient components");
            }

            public String getName() {
                return new String("Sobel gradient components");
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

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterInt("kernel_size", 3));
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
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
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
