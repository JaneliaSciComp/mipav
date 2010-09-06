package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The algorithm extracts a set of slices from a 2D mosaic. The user has the option to
 * generate a new image or replace the source image. It should be noted that the algorithms are executed in their own
 * thread. 
 *
 * @version  0.1 September 2, 2010
 * @author   William Gandler
 * @see      
 */
public class JDialogMosaicToSlices extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc = NEW;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    private int subXDim = 0;
    
    private int subYDim = 0;
    
    private int numberOfImagesInMosaic = 0;
    
    private JTextField textXDim;
    
    private JTextField textYDim;
    
    private JTextField textNumberImages;

    /** DOCUMENT ME! */
    private AlgorithmMosaicToSlices mathAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMosaicToSlices() { }

    /**
     * Creates new Mosaic To Slices dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMosaicToSlices(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmMosaicToSlices) {

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

                    Vector imageFrames = image.getImageFrameVector();

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
                        ((ViewJFrameBase) image.getParentFrame()).close();
                    }

                    // Not so sure about this.
                    if (image.getLightBoxFrame() != null) {

                        try {
                            pt = image.getLightBoxFrame().getLocation();
                            image.getLightBoxFrame().close();
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

                image.notifyImageDisplayListeners(null, true);
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

        if (image != null) {
            image.disposeLocal();
        }

        image = null;

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

        
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Once all the necessary variables are set, call the Concat algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
    	int destExtents[] = new int[3];
    	ModelImage destImage = null;
    	
    	destExtents[0] = subXDim;
    	destExtents[1] = subYDim;
    	destExtents[2] = numberOfImagesInMosaic;

        destImage = new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(), "_mosaic_to_slices"));

        try {

            // Make algorithm
            mathAlgo = new AlgorithmMosaicToSlices(image, destImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mathAlgo.addListener(this);

            createProgressBar(image.getImageName(), mathAlgo);

            // Hide dialog
            setVisible(false);

            if (displayLoc == REPLACE) {

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
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
        image = scriptParameters.retrieveInputImage(1);

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {

            // replace processing not supported..
            // setDisplayLocReplace();
            setDisplayLocNew();
        }
        
        subXDim = scriptParameters.getParams().getInt("sub_x_dim");
        subYDim = scriptParameters.getParams().getInt("sub_y_dim");
        numberOfImagesInMosaic = scriptParameters.getParams().getInt("number_of_images_in_mosaic");
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("sub_x_dim", subXDim));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sub_y_dim", subYDim));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_images_in_mosaic",numberOfImagesInMosaic));
    }

    

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
        	FileInfoDicom dicomInfo = (FileInfoDicom) image.getFileInfo(0);
        	FileDicomTagTable tagTable = dicomInfo.getTagTable();
        	if (tagTable.getValue("0018,1310") != null) {
        	    // Acquisition matrix
        		FileDicomTag tag = tagTable.get(new FileDicomKey("0018,1310"));
        		Object[] values = tag.getValueList();
        		int valNumber = values.length;	
        		if ((valNumber == 4) && (values instanceof Short[])) {
        			int frequencyRows = ((Short) values[0]).intValue();
        			Preferences.debug("frequencyRows = " + frequencyRows + "\n");
        			int frequencyColumns = ((Short) values[1]).intValue();
        			Preferences.debug("frequencyColumns = " + frequencyColumns + "\n");
        			int phaseRows = ((Short) values[2]).intValue();
        			Preferences.debug("phaseRows = " + phaseRows + "\n");
        			int phaseColumns = ((Short) values[3]).intValue();
        			Preferences.debug("phaseColumns = " + phaseColumns + "\n");
        			if ((frequencyRows > 0) && (phaseRows == 0)) {
        				subYDim = frequencyRows;
        			}
        			else if ((frequencyRows == 0) && (phaseRows > 0)) {
        				subYDim = phaseRows;
        			}
        			if ((frequencyColumns > 0) && (phaseColumns == 0)) {
        				subXDim = frequencyColumns;
        			}
        			else if ((frequencyColumns == 0) && (phaseColumns > 0)) {
        				subXDim = phaseColumns;
        			}
        		}
        	} // if (tagTable.getValue("0018,1310") != null)
        	if (tagTable.getValue("0019,100A") != null) {
        		FileDicomTag tag = tagTable.get(new FileDicomKey("0019,100A"));
        		Object value = tag.getValue(false);
        		if (value instanceof Short) {
        			numberOfImagesInMosaic = ((Short) value).intValue();
        			Preferences.debug("Number of images in mosaic = " + numberOfImagesInMosaic + "\n");
        		}	
        	} // if (tagTable.getValue("0019,100A") != null)
    	} // if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM)
        setForeground(Color.black);
        setTitle("Mosaic To Slices");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("Image"));

        JLabel labelUse = new JLabel("Image:");
        labelUse.setForeground(Color.black);
        labelUse.setFont(serif12);

        JLabel labelImage = new JLabel(image.getImageName());
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);

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
        inputPanel.add(labelImage, gbc);

        JPanel dimensionPanel = new JPanel(new GridBagLayout());
        dimensionPanel.setForeground(Color.black);
        dimensionPanel.setBorder(buildTitledBorder("X and Y Dimensions of Result"));
        
        JLabel labelXDim = new JLabel("X dimension of slices");
        labelXDim.setForeground(Color.black);
        labelXDim.setFont(serif12);
        
        textXDim = new JTextField(10);
        if (subXDim != 0) {
        	textXDim.setText(String.valueOf(subXDim));
        }
        textXDim.setFont(serif12);
        textXDim.setForeground(Color.black);
        
        JLabel labelYDim = new JLabel("Y dimension of slices");
        labelYDim.setForeground(Color.black);
        labelYDim.setFont(serif12);
        
        textYDim = new JTextField(10);
        if (subYDim != 0) {
        	textYDim.setText(String.valueOf(subYDim));
        }
        textYDim.setFont(serif12);
        textYDim.setForeground(Color.black);
        
        JLabel labelNumberImages = new JLabel("Number of images in mosaic");
        labelNumberImages.setForeground(Color.black);
        labelNumberImages.setFont(serif12);
        
        textNumberImages = new JTextField(10);
        if (numberOfImagesInMosaic != 0) {
        	textNumberImages.setText(String.valueOf(numberOfImagesInMosaic));
        }
        textNumberImages.setFont(serif12);
        textNumberImages.setForeground(Color.black);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        dimensionPanel.add(labelXDim, gbc);
        gbc.gridx = 1;
        dimensionPanel.add(textXDim, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        dimensionPanel.add(labelYDim, gbc);
        gbc.gridx = 1;
        dimensionPanel.add(textYDim, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        dimensionPanel.add(labelNumberImages, gbc);
        gbc.gridx = 1;
        dimensionPanel.add(textNumberImages, gbc);

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
        String tmpStr;
        
        tmpStr = textXDim.getText();
        try {
            subXDim = Integer.parseInt(tmpStr);
        }
        catch(NumberFormatException e) {
        	MipavUtil.displayError("New XDIM string is not a valid integer");
        	textXDim.requestFocus();
            textXDim.selectAll();

            return false;
        }
        if (subXDim < 3) {
            MipavUtil.displayError("New XDIM must be at least 3");
            textXDim.requestFocus();
            textXDim.selectAll();

            return false;
        } else if (subXDim > image.getExtents()[0]) {
            MipavUtil.displayError("New XDIM cannot exceed " + image.getExtents()[0]);
            textXDim.requestFocus();
            textXDim.selectAll();

            return false;
        }
        
        tmpStr = textYDim.getText();
        try {
            subYDim = Integer.parseInt(tmpStr);
        }
        catch(NumberFormatException e) {
        	MipavUtil.displayError("New YDIM string is not a valid integer");
        	textYDim.requestFocus();
            textYDim.selectAll();

            return false;
        }
        if (subYDim < 3) {
            MipavUtil.displayError("New YDIM must be at least 3");
            textYDim.requestFocus();
            textYDim.selectAll();

            return false;
        } else if (subYDim > image.getExtents()[1]) {
            MipavUtil.displayError("New YDIM cannot exceed " + image.getExtents()[1]);
            textYDim.requestFocus();
            textYDim.selectAll();

            return false;
        }
        
        tmpStr = textNumberImages.getText();
        try {
            numberOfImagesInMosaic = Integer.parseInt(tmpStr);
        }
        catch(NumberFormatException e) {
        	MipavUtil.displayError("New numberOfImagesInMosaic string is not a valid integer");
        	textNumberImages.requestFocus();
            textNumberImages.selectAll();

            return false;
        }
        if (numberOfImagesInMosaic < 1) {
            MipavUtil.displayError("New numberOfImagesInMosaic must be at least 1");
            textNumberImages.requestFocus();
            textNumberImages.selectAll();

            return false;
        } else if (numberOfImagesInMosaic > (subXDim*subYDim)) {
            MipavUtil.displayError("New YDIM cannot exceed (newXDim) * (newYDim)");
            textNumberImages.requestFocus();
            textNumberImages.selectAll();

            return false;
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
                return new String("Utilities.Mosaic to slices");
            }

            public String getDescription() {
                return new String("Mosaic to slices.");
            }

            public String getDescriptionLong() {
                return new String("Mosaic to slices.");
            }

            public String getShortLabel() {
                return new String("Mosaic to slices");
            }

            public String getLabel() {
                return new String("Mosaic to slices");
            }

            public String getName() {
                return new String("Mosaic to slices");
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
