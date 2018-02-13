package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.EnumSet;
import java.util.Set;

import javax.swing.*;


/**
 * Dialog to call the AlgorithmConvert3Dto4D to convert a 3D data set into a 4D data set. This dialog will not be
 * visible because it does not require user input at this time. It was made a dialog object because it may in the future
 * require user input and to be consistent with the dialog/algorithm paradigm. In should be noted, that the algorithms
 * are executed in their own thread.** replaces image
 */
public class JDialogConvert3Dto4D extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5353653644655035651L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure3;

    /** DOCUMENT ME! */
    private JComboBox comboBoxUnitOfMeasure4;

    /** DOCUMENT ME! */
    private AlgorithmConvert3Dto4D convert3Dto4DAlgo;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private String imageName;

    /** DOCUMENT ME! */
    private JLabel labelVolumeLength;

    /** DOCUMENT ME! */
    private int measure3, measure4;

    /** DOCUMENT ME! */
    private float res3, res4;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JTextField textRes3;

    /** DOCUMENT ME! */
    private JTextField textRes4;

    /** DOCUMENT ME! */
    private JTextField textVolumeLength;

    /** DOCUMENT ME! */
    private int volumeLength;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConvert3Dto4D() { }

    /**
     * Creates new dialog, but dialog is not visible.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogConvert3Dto4D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        imageName = image.getImageName();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calls run on the algorithm from the script parser.
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
            //MipavUtil.showHelp("U4001");
            MipavUtil.showWebHelp("4_D_tools#Converting_3D_to_4D_images_and_vise_versa");
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

        if (algorithm instanceof AlgorithmConvert3Dto4D) {
            resultImage = convert3Dto4DAlgo.getResultImage();

            if ((convert3Dto4DAlgo.isCompleted() == true) && (resultImage != null)) {
                resultImage.setImageName(imageName + "_4D");

                try {
                    new ViewJFrameImage(resultImage, null, ViewUserInterface.getReference().getNewFrameLocation(resultImage.getExtents()[0], resultImage.getExtents()[1]));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        convert3Dto4DAlgo.finalize();
        convert3Dto4DAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }


    /**
     * Accessor that sets the resolution for the 3rd dimension.
     *
     * @param  resol3  Value to set resolution for the 3rd dimension [0:1000]
     */
    public void setResolution3Dim(float resol3) {
        res3 = resol3;
    }

    /**
     * Accessor that sets the resolution for the 4th dimension.
     *
     * @param  resol4  Value to set resolution for the 4th dimension [0:1000]
     */
    public void setResolution4Dim(float resol4) {
        res4 = resol4;
    }

    /**
     * Accessor that sets the resolution unit for the 3rd dimension.
     *
     * @param  measureUnit3  Value to set resolution unit for the 3rd dimension.
     */
    public void setResolutionUnit3Dim(int measureUnit3) {
        measure3 = measureUnit3;
    }

    /**
     * Accessor that sets the resolution unit for the 4th dimension.
     *
     * @param  measureUnit4  Value to set resolution unit for the 4th dimension.
     */
    public void setResolutionUnit4Dim(int measureUnit4) {
        measure4 = measureUnit4;
    }

    /**
     * Accessor that sets the length (number of slices) for the 3rd dimension.
     *
     * @param  volLength  number of image slices for the 3rd dimension.
     */
    public void setVolumeLength(int volLength) {
        volumeLength = volLength;
    }

    /**
     * Runs the algorithm.
     */
    protected void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            convert3Dto4DAlgo = new AlgorithmConvert3Dto4D(image, volumeLength, res3, res4, measure3, measure4);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            convert3Dto4DAlgo.addListener(this);

            createProgressBar(image.getImageName(), convert3Dto4DAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (convert3Dto4DAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                convert3Dto4DAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("JDialogConvert3Dto4D: unable to allocate enough memory");

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
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        volumeLength = scriptParameters.getParams().getInt("volume_length");
        res3 = scriptParameters.getParams().getFloat("3_dim_resolution");
        res4 = scriptParameters.getParams().getFloat("4_dim_resolution");
        measure3 = scriptParameters.getParams().getInt("3_dim_unit");
        measure4 = scriptParameters.getParams().getInt("4_dim_unit");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("volume_length", volumeLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("3_dim_resolution", res3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("4_dim_resolution", res4));
        scriptParameters.getParams().put(ParameterFactory.newParameter("3_dim_unit", measure3));
        scriptParameters.getParams().put(ParameterFactory.newParameter("4_dim_unit", measure4));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Convert from 3D to 4D");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel inputPanel = new JPanel(new GridLayout(1, 2));
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Parameters "));

        labelVolumeLength = new JLabel("Number of slices in the 3rd dimension of resulting 4D volume ");
        labelVolumeLength.setForeground(Color.black);
        labelVolumeLength.setFont(serif12);
        inputPanel.add(labelVolumeLength);
        textVolumeLength = new JTextField();
        textVolumeLength.setText("2");
        textVolumeLength.setFont(serif12);
        inputPanel.add(textVolumeLength);
        mainPanel.add(inputPanel, gbc);

        JPanel resolutionsPanel = new JPanel(new GridLayout(2, 2));
        resolutionsPanel.setForeground(Color.black);
        resolutionsPanel.setBorder(buildTitledBorder("Resolutions "));

        JLabel dim3 = new JLabel("3rd dim.");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        resolutionsPanel.add(dim3);

        textRes3 = new JTextField(5);
        textRes3.setText(String.valueOf(image.getFileInfo()[0].getResolutions()[2]));
        textRes3.setFont(serif12);
        textRes3.addFocusListener(this);
        resolutionsPanel.add(textRes3);

        JLabel dim4 = new JLabel("4th dim.");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        resolutionsPanel.add(dim4);

        textRes4 = new JTextField(5);
        textRes4.setText("1.0");
        textRes4.setFont(serif12);
        textRes4.addFocusListener(this);
        resolutionsPanel.add(textRes4);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(resolutionsPanel, gbc);

        JPanel comboPanel = new JPanel(new GridLayout(2, 2));
        comboPanel.setForeground(Color.black);
        comboPanel.setBorder(buildTitledBorder("Resolution units "));

        JLabel dim3u = new JLabel("3rd dim.");
        dim3u.setFont(serif12);
        dim3u.setForeground(Color.black);
        resolutionsPanel.add(dim3u);
        comboPanel.add(dim3u);

        comboBoxUnitOfMeasure3 = new JComboBox();
        comboBoxUnitOfMeasure3.setAlignmentX(Component.LEFT_ALIGNMENT);
        setComboBox(comboBoxUnitOfMeasure3);
        if(image.getNDims() >= 3) {
            String unitStr = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(2)).toString();
            comboBoxUnitOfMeasure3.setSelectedItem(unitStr);
            comboBoxUnitOfMeasure3.setEnabled(true);
        } else {
            comboBoxUnitOfMeasure3.setEnabled(false);
        }
        comboPanel.add(comboBoxUnitOfMeasure3);

        JLabel dim4u = new JLabel("4rd dim.");
        dim4u.setFont(serif12);
        dim4u.setForeground(Color.black);
        resolutionsPanel.add(dim4u);
        comboPanel.add(dim4u);

        comboBoxUnitOfMeasure4 = new JComboBox();
        comboBoxUnitOfMeasure4.setAlignmentX(Component.LEFT_ALIGNMENT);
        setComboBox(comboBoxUnitOfMeasure4);
        if(image.getNDims() >= 3) {
            String unitStr = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(2)).toString();
            comboBoxUnitOfMeasure4.setSelectedItem(unitStr);
            comboBoxUnitOfMeasure4.setEnabled(true);
        } else {
            comboBoxUnitOfMeasure4.setEnabled(false);
        }
        comboPanel.add(comboBoxUnitOfMeasure4);

        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(comboPanel, gbc);
        
        gbc.gridy++;
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Sets combo box choices that match resolution units listed in FileInfoBase and in the same order.
     *
     * @param  cBox  Combo box to setup to display the units.
     */
    private void setComboBox(JComboBox cBox) {

        cBox.setFont(serif12);
        cBox.setBackground(Color.white);
        for(Unit u : Unit.values()) {
            cBox.addItem(u.toString());
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textVolumeLength.getText();

        if (testParameter(tmpStr, 1, 1000)) {
            volumeLength = Integer.valueOf(tmpStr).intValue();
        } else {
            textVolumeLength.requestFocus();
            textVolumeLength.selectAll();

            return false;
        }

        tmpStr = textRes3.getText();

        if (testParameter(tmpStr, 0, 1000)) {
            res3 = Float.valueOf(tmpStr).floatValue();
        } else {
            textRes3.requestFocus();
            textRes3.selectAll();

            return false;
        }

        tmpStr = textRes4.getText();

        if (testParameter(tmpStr, 0, 1000)) {
            res4 = Float.valueOf(tmpStr).floatValue();
        } else {
            textRes4.requestFocus();
            textRes4.selectAll();

            return false;
        }

        switch (comboBoxUnitOfMeasure3.getSelectedIndex()) {

            case 0:
                measure3 = Unit.UNKNOWN_MEASURE.getLegacyNum();
                break;

            case 1:
                measure3 = Unit.INCHES.getLegacyNum();
                break;
                
            case 2:
                measure3 = Unit.MILS.getLegacyNum();
                break;

            case 3:
                measure3 = Unit.CENTIMETERS.getLegacyNum();
                break;

            case 4:
                measure3 = Unit.ANGSTROMS.getLegacyNum();
                break;

            case 5:
                measure3 = Unit.NANOMETERS.getLegacyNum();
                break;

            case 6:
                measure3 = Unit.MICROMETERS.getLegacyNum();
                break;

            case 7:
                measure3 = Unit.MILLIMETERS.getLegacyNum();
                break;

            case 8:
                measure3 = Unit.METERS.getLegacyNum();
                break;

            case 9:
                measure3 = Unit.KILOMETERS.getLegacyNum();
                break;

            case 10:
                measure3 = Unit.MILES.getLegacyNum();
                break;

            case 11:
                measure3 = Unit.NANOSEC.getLegacyNum();
                break;

            case 12:
                measure3 = Unit.MICROSEC.getLegacyNum();
                break;

            case 13:
                measure3 = Unit.MILLISEC.getLegacyNum();
                break;

            case 14:
                measure3 = Unit.SECONDS.getLegacyNum();
                break;

            case 15:
                measure3 = Unit.MINUTES.getLegacyNum();
                break;

            case 16:
                measure3 = Unit.HOURS.getLegacyNum();
                break;

            case 17:
                measure3 = Unit.HZ.getLegacyNum();
                break;

            default:

                measure3 = Unit.UNKNOWN_MEASURE.getLegacyNum();
        }

        switch (comboBoxUnitOfMeasure4.getSelectedIndex()) {

            case 0:
                measure4 = Unit.UNKNOWN_MEASURE.getLegacyNum();
                break;

            case 1:
                measure4 = Unit.INCHES.getLegacyNum();
                break;
                
            case 2:
                measure4 = Unit.MILS.getLegacyNum();
                break;

            case 3:
                measure4 = Unit.CENTIMETERS.getLegacyNum();
                break;

            case 4:
                measure4 = Unit.ANGSTROMS.getLegacyNum();
                break;

            case 5:
                measure4 = Unit.NANOMETERS.getLegacyNum();
                break;

            case 6:
                measure4 = Unit.MICROMETERS.getLegacyNum();
                break;

            case 7:
                measure4 = Unit.MILLIMETERS.getLegacyNum();
                break;

            case 8:
                measure4 = Unit.METERS.getLegacyNum();
                break;

            case 9:
                measure4 = Unit.KILOMETERS.getLegacyNum();
                break;

            case 10:
                measure4 = Unit.MILES.getLegacyNum();
                break;

            case 11:
                measure4 = Unit.NANOSEC.getLegacyNum();
                break;

            case 12:
                measure4 = Unit.MICROSEC.getLegacyNum();
                break;

            case 13:
                measure4 = Unit.MILLISEC.getLegacyNum();
                break;

            case 14:
                measure4 = Unit.SECONDS.getLegacyNum();
                break;

            case 15:
                measure4 = Unit.MINUTES.getLegacyNum();
                break;

            case 16:
                measure4 = Unit.HOURS.getLegacyNum();
                break;

            case 17:
                measure4 = Unit.HZ.getLegacyNum();
                break;

            default:
                measure4 = Unit.UNKNOWN_MEASURE.getLegacyNum();
        }
        
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
                return new String("Utilities.4D tools");
            }

            public String getDescription() {
                return new String("Converts a 3D image to a 4D image.");
            }

            public String getDescriptionLong() {
                return new String("Converts a 3D image to a 4D image.");
            }

            public String getShortLabel() {
                return new String("Convert3DTo4D");
            }

            public String getLabel() {
                return new String("Convert 3D to 4D");
            }

            public String getName() {
                return new String("Convert 3D to 4D");
            }
            
            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.NDIM_3);
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
            table.put(new ParameterInt("volume_length", 2));
            table.put(new ParameterFloat("3_dim_resolution", 1.5f));
            table.put(new ParameterFloat("4_dim_resolution", 1.0f));
            table.put(new ParameterBoolean("copy_all_image_info", true));
            
            //Go to FileInfoBase.java for information on 3_dim_unit and 4_dim_unit
            //There is no obvious way to display the string these integers represent in the dialog
            //8 == MILLIMETERS
            //13 == MICROSECONDS
            table.put(new ParameterInt("3_dim_unit", Unit.MILLIMETERS.getLegacyNum()));
            table.put(new ParameterInt("4_dim_unit", Unit.MICROSEC.getLegacyNum()));    
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
                // algo always produces a new result image
                return getResultImage().getImageName();
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
