package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogEdgeDetection3D extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textThreshold;
    
    // (FMAX/largestFMAX) must be >= threshold to be made an edge
    private double threshold = 0.12;

    /** DOCUMENT ME! */
    private JTextField textL;

    // Neighborhood size for the directional gradient averaging L (recommended empirically derived value
    // for the mask size of 5 x 5 x 5 = 2.1)
    // Y0 and Z0 have no units
    // X2 has units of x**2
    // A2 has units of x**-2
    // K2 has units of x**-1
    // FX has units of x**-1
    // L*L*K11 has units of x**-1
    // K11 has units of x**5/U2 = x**5/x**8 = x**-3
    // L*L has units of x**2
    // L has units of x
    private double L;
    
    private JTextField textdx;
   
    // dx, dy, dz
    // Size of the prism over which the data are integrated during the acquisition process
    // Dear Professor Milan Sonka:

    // I am a Java programmer at the National Institutes of Health who is implementing your 3d edge detector as 
    // specified in "Directional 3D Edge Detection in Anisotropic Data: Detector Design and Performance Assessment" 
    // for the MIPAV image processing package.  How long in terms of voxel lengths were the typical values of dx, 
    // dy, and dz, the sizes of the prism over which the data were integrated?

              // Sincerely,

           //  William Gandler
    
    // William,
    // I would think that all these should be parameters of your implementation.
    // And perhaps allow different values for each direction to support anisotropic data.

    // As I recall, the best sizes were about 5-9 for isotropic data.

    // Best,
    // Milan
    private double dx;
    
    private JTextField textdy;
    
    private double dy;
    
    private JTextField textdz;
    
    private double dz;
    
    // Mask size nx, ny, nz
    private JTextField textnx;
    
    private int nx = 5;
    
    private JTextField textny;
    
    private int ny = 5;
    
    private JTextField textnz;
    
    private int nz = 5;
    
    private boolean componentsRequired = false;
    private boolean anglesRequired = false;
    private boolean magnitudeRequired = false;

    /** DOCUMENT ME! */
    private AlgorithmEdgeDetection3D edgeAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEdgeDetection3D() { }

    /**
     * Creates new dialog for entering parameters for Sonka-Brejl 3D edge detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogEdgeDetection3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
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

        if (algorithm instanceof AlgorithmEdgeDetection3D) {
            System.err.println("Edge Detection 3D Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((edgeAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

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

        edgeAlgo.finalize();
        edgeAlgo = null;
        dispose();
    }

   

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    
    /**
     * 
     * @param threshold
     */
    public void setThreshold(double threshold) {
        this.threshold = threshold;
    }

    /**
     * Accessor that sets neighborhood size for the directional gradient averaging.
     *
     * @param  L  neighborhood size for the directional gradient averaging
     */
    public void setL(double L) {
        this.L = L;
    }
    
    /**
     * 
     * @param dx Size X of the prism over which the data is integrated
     */
    public void setdx(double dx) {
        this.dx = dx;
    }
    
    /**
     * 
     * @param dy Size Y of the prism over which the data is integrated
     */
    public void setdy(double dy) {
        this.dy = dy;
    }
    
    /**
     * 
     * @param dz Size Z of the prism over which the data is integrated
     */
    public void setdz(double dz) {
        this.dz = dz;
    }
    
    /**
     * 
     * @param ny Dimension Y of the mask
     */
    public void setny(int ny) {
        this.ny = ny;
    }
    
    /**
     * 
     * @param nz Dimension Z of the mask
     */
    public void setnz(int nz) {
        this.nz = nz;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_EdgeDetection3D");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
            }

            // Make algorithm
            edgeAlgo = new AlgorithmEdgeDetection3D(resultImage, image, dx, dy, dz, nx, ny, nz, L, threshold,
                           componentsRequired, anglesRequired, magnitudeRequired);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            edgeAlgo.addListener(this);

            createProgressBar(image.getImageName(), edgeAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (edgeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                edgeAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Edge Detection 3D: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

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
        image = scriptParameters.retrieveInputImage();
        threshold = scriptParameters.getParams().getDouble("thresh");
        L = scriptParameters.getParams().getDouble("l");
        dx = scriptParameters.getParams().getDouble("d_x");
        dy = scriptParameters.getParams().getDouble("d_y");
        dz = scriptParameters.getParams().getDouble("d_z");
        nx = scriptParameters.getParams().getInt("n_x");
        ny = scriptParameters.getParams().getInt("n_y");
        nz = scriptParameters.getParams().getInt("n_z");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("l", L));
        scriptParameters.getParams().put(ParameterFactory.newParameter("d_x", dx));
        scriptParameters.getParams().put(ParameterFactory.newParameter("d_y", dy));
        scriptParameters.getParams().put(ParameterFactory.newParameter("d_z", dz));
        scriptParameters.getParams().put(ParameterFactory.newParameter("n_x", nx));
        scriptParameters.getParams().put(ParameterFactory.newParameter("n_y", ny));
        scriptParameters.getParams().put(ParameterFactory.newParameter("n_z", nz));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Edge Detection 3D");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelThreshold = new JLabel("Threshold ( <= 1.0)");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelThreshold, gbc);

        textThreshold = new JTextField(10);
        textThreshold.setText("0.12");
        textThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold, gbc);

        JLabel labelL = new JLabel("Neighborhood size for the directional gradient averaging in x resolution multiples");
        labelL.setForeground(Color.black);
        labelL.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelL, gbc);

        textL = new JTextField(10);
        textL.setText("2.1");
        textL.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textL, gbc);
        
        JLabel labeldx = new JLabel("x length of prism over which the data is integrated in x resolution multiples");
        labeldx.setForeground(Color.black);
        labeldx.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labeldx, gbc);

        textdx = new JTextField(10);
        textdx.setText("7.0");
        textdx.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textdx, gbc);
        
        JLabel labeldy = new JLabel("y length of prism over which the data is integrated in y resolution multiples");
        labeldy.setForeground(Color.black);
        labeldy.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labeldy, gbc);

        textdy = new JTextField(10);
        textdy.setText("7.0");
        textdy.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textdy, gbc);
        
        JLabel labeldz = new JLabel("z length of prism over which the data is integrated in z resolution multiples");
        labeldz.setForeground(Color.black);
        labeldz.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(labeldz, gbc);

        textdz = new JTextField(10);
        textdz.setText("7.0");
        textdz.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textdz, gbc);
        
        JLabel labelnx = new JLabel("X width of mask (an odd integer)");
        labelnx.setForeground(Color.black);
        labelnx.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(labelnx, gbc);
        
        textnx = new JTextField(10);
        textnx.setText("5");
        textnx.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textnx, gbc);
        
        JLabel labelny = new JLabel("Y width of mask (an odd integer)");
        labelny.setForeground(Color.black);
        labelny.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(labelny, gbc);
        
        textny = new JTextField(10);
        textny.setText("5");
        textny.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textny, gbc);
        
        JLabel labelnz = new JLabel("Z width of mask (an odd integer)");
        labelnz.setForeground(Color.black);
        labelnz.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramPanel.add(labelnz, gbc);
        
        textnz = new JTextField(10);
        textnz.setText("5");
        textnz.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textnz, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(paramPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        
        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 0.0001, 1.0)) {
            threshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        tmpStr = textL.getText();

        if (testParameter(tmpStr, 1.0, 5.0)) {
            L = Double.valueOf(tmpStr).doubleValue() * image.getFileInfo(0).getResolutions()[0];
        } else {
            textL.requestFocus();
            textL.selectAll();

            return false;
        }
        
        tmpStr = textdx.getText();

        if (testParameter(tmpStr, 1.0, 20.0)) {
            dx = Double.valueOf(tmpStr).doubleValue() * image.getFileInfo(0).getResolutions()[0];
        } else {
            textdx.requestFocus();
            textdx.selectAll();

            return false;
        }
        
        tmpStr = textdy.getText();
        
        if (testParameter(tmpStr, 1.0, 20.0)) {
            dy = Double.valueOf(tmpStr).doubleValue() * image.getFileInfo(0).getResolutions()[1];
        } else {
            textdy.requestFocus();
            textdy.selectAll();

            return false;
        }
        
        tmpStr = textdz.getText();
        
        if (testParameter(tmpStr, 1.0, 20.0)) {
            dz = Double.valueOf(tmpStr).doubleValue() * image.getFileInfo(0).getResolutions()[2];
        } else {
            textdz.requestFocus();
            textdz.selectAll();

            return false;
        }
       
        tmpStr = textnx.getText();
        
        if (testParameter(tmpStr, 1, 19)) {
            nx = Integer.valueOf(tmpStr).intValue();
        } else {
            textnx.requestFocus();
            textnx.selectAll();
            
            return false;
        }
        
        if ((nx % 2) == 0) {
            MipavUtil.displayError("Mask X dimension must be an odd number");
            textnx.requestFocus();
            textnx.selectAll();
            
            return false;
        }
        
        tmpStr = textny.getText();
        
        if (testParameter(tmpStr, 1, 19)) {
            ny = Integer.valueOf(tmpStr).intValue();
        } else {
            textny.requestFocus();
            textny.selectAll();
            
            return false;
        }
        
        if ((ny % 2) == 0) {
            MipavUtil.displayError("Mask Y dimension must be an odd number");
            textny.requestFocus();
            textny.selectAll();
            
            return false;
        }
        
        tmpStr = textnz.getText();
        
        if (testParameter(tmpStr, 1, 19)) {
            nz = Integer.valueOf(tmpStr).intValue();
        } else {
            textnz.requestFocus();
            textnz.selectAll();
            
            return false;
        }
        
        if ((nz % 2) == 0) {
            MipavUtil.displayError("Mask Z dimension must be an odd number");
            textnz.requestFocus();
            textnz.selectAll();
            
            return false;
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
                return new String("Algorithms.EdgeDetection 3D");
            }

            public String getDescription() {
                return new String("Applies Edge Detection 3D.");
            }

            public String getDescriptionLong() {
                return new String("Applies Edge Detection 3D (Brejl-Sonka).");
            }

            public String getShortLabel() {
                return new String("EdgeDetection3D");
            }

            public String getLabel() {
                return new String("Edge Detection 3D");
            }

            public String getName() {
                return new String("Edge Detection 3D");
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
            table.put(new ParameterDouble("thresh", 0.12));
            table.put(new ParameterDouble("l", 2.1));
            table.put(new ParameterDouble("d_x", 7.0));
            table.put(new ParameterDouble("d_y", 7.0));
            table.put(new ParameterDouble("d_z", 7.0));
            table.put(new ParameterDouble("n_x", 5));
            table.put(new ParameterDouble("n_y", 5));
            table.put(new ParameterDouble("n_z", 5));
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
