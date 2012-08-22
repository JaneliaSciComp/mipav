package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm...
 *

 */
public class JDialogLightboxGen extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4508090288311270016L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    
    private JTextField textBoxR, textBoxG, textBoxB, textStart,
    textEnd, textRows, textColumns, textThickness, textPercent;
    
    /** DOCUMENT ME! */
    private ModelImage image; // first source image

    /** DOCUMENT ME! */
    private LightboxGenerator lightGen;
    
    int valueR, valueG, valueB, percent, startSlice, endSlice, columns, row, thickness;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLightboxGen() { }

    /**
     * Creates new match image dialog and displays.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogLightboxGen(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            //MipavUtil.showHelp("U4037");
            MipavUtil.showWebHelp("Matching_images");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

            if (resultImage != null) {
                resultImage = lightGen.getImage();
            }

            if (lightGen.getImage() != null) {
                //System.out.println("Lightbox Image completed.");
                


                // Display new images
                try {

                    if (resultImage != null) {
                        resultImage.calcMinMax();
                        new ViewJFrameImage(resultImage, null, new Dimension(25, 55));
                    }

                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frames");
                }
            } else {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result images
                }

            
        }

        // Update frames
        image.notifyImageDisplayListeners(null, true);

        // Write to script.
        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        lightGen.finalize();
        lightGen = null;

        dispose();
        System.gc();
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
     * Accessor that sets image A.
     *
     * @param  im  Image A.
     */
    public void setImage(ModelImage im) {
        image = im;
    }


    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm

            lightGen = new LightboxGenerator(image, startSlice, endSlice, percent, row,
            		columns, valueR, valueG, valueB, true, thickness);
            

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            lightGen.addListener(this);
            createProgressBar(image.getImageName(), lightGen);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (lightGen.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                lightGen.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Unable to allocate enough memory to run algorithms.");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {



        if (getResultImage() != null) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }

    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {


    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Lightbox Image Generator");

        JPanel inputPanel = new JPanel();
        inputPanel.setBorder(buildTitledBorder("Lightbox Options:"));

        inputPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridy = 1;
        gbc.gridwidth = 2;

        JLabel descript = new JLabel("This utility creates lightbox images in RBG format");
        inputPanel.add(descript, gbc);

        gbc.gridy = 2;
        gbc.gridwidth = 1;

        JLabel labelCol = new JLabel("Columns:");
        inputPanel.add(labelCol, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textColumns = new JTextField(4);
        inputPanel.add(textColumns, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);

        gbc.gridy = 3;
        gbc.gridwidth = 1;

        JLabel labelRows = new JLabel("Rows: ");
        inputPanel.add(labelRows, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textRows = new JTextField(4);
        inputPanel.add(textRows, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 4;
        gbc.gridwidth = 1;

        JLabel labelR = new JLabel("Border R Value: ");
        inputPanel.add(labelR, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textBoxR = new JTextField(4);
        inputPanel.add(textBoxR, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 5;
        gbc.gridwidth = 1;

        JLabel labelG = new JLabel("Border G Value: ");
        inputPanel.add(labelG, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textBoxG = new JTextField(4);
        inputPanel.add(textBoxG, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 6;
        gbc.gridwidth = 1;

        JLabel labelB = new JLabel("Border B Value: ");
        inputPanel.add(labelB, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textBoxB = new JTextField(4);
        inputPanel.add(textBoxB, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 7;
        gbc.gridwidth = 1;

        JLabel labelThick = new JLabel("Border Thickness (pixels): ");
        inputPanel.add(labelThick, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textThickness = new JTextField(4);
        inputPanel.add(textThickness, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 8;
        gbc.gridwidth = 1;

        JLabel labelStart = new JLabel("Start Slide: ");
        inputPanel.add(labelStart, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textStart = new JTextField(4);
        inputPanel.add(textStart, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 9;
        gbc.gridwidth = 1;

        JLabel labelEnd = new JLabel("End Slide: ");
        inputPanel.add(labelEnd, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textEnd = new JTextField(4);
        inputPanel.add(textEnd, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);
        
        gbc.gridy = 10;
        gbc.gridwidth = 1;

        JLabel labelPercent = new JLabel("Percent Size [10-100%]: ");
        inputPanel.add(labelPercent, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        textPercent = new JTextField(4);
        inputPanel.add(textPercent, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);


        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildOKCancelButtons());

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainPanel);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
    	
        try{
            columns = Integer.parseInt(textColumns.getText());
            row = Integer.parseInt(textRows.getText());
            thickness = Integer.parseInt(textThickness.getText());
            startSlice = Integer.parseInt(textStart.getText());
            endSlice = Integer.parseInt(textEnd.getText());
            percent = Integer.parseInt(textPercent.getText());
            valueR = Integer.parseInt(textBoxR.getText());
            valueB = Integer.parseInt(textBoxB.getText());
            valueG = Integer.parseInt(textBoxG.getText());
            
        


            if ((valueR < 0 || valueR > 255) || (valueB < 0 || valueB > 255) || (valueG < 0 || valueG > 255)) {

                MipavUtil.displayError("RBG values must be between 0 and 255.");
                return false;
            }
            
            if (percent <= 0 || percent > 100) {

                MipavUtil.displayError("Percent value must be between 10 to 100");
                return false;
            }
            
            if (columns <= 0 || row <= 0) {

                MipavUtil.displayError("Rows and/or Columns must be a positive number");
                return false;
            }
            
            if (image.is3DImage()) {

            } else {
                MipavUtil.displayError("Image must be 3D!");
                return false;
            }
            
            if (startSlice < 0 || endSlice >= image.getExtents()[2] || endSlice < startSlice)
            {
                MipavUtil.displayError("Start/End Slices out of bounds.");
                return false;
            }
            
            if (thickness < 0 || row < 0 || columns < 0)
            {
                MipavUtil.displayError("Thickness/Columns/Rows must be a positive value");
                return false;
            }

        return true;
    }
        catch (Exception e) {
        	MipavUtil.displayError("Error.");
			return false;
		}
    }

}
