package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. Algorithms are executed in their own thread.
 *
 * @see  AlgorithmNonlocalMeansFilter
 */
public class JDialogNonlocalMeansFilter extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, DialogDefaultsInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmNonlocalMeansFilter nlMeansFilterAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = true;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckBox;

    /** DOCUMENT ME! */
    private JLabel labelSimilarityWindowSide;

    /** DOCUMENT ME! */
    private JLabel labelNoiseStandardDeviation;

    /** DOCUMENT ME! */
    private JLabel labelSearchWindowSide;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private int similarityWindowSide = 7;

    /** Should be set equal to the noise standard deviation */
    private float noiseStandardDeviation = 10.0f;

    /** DOCUMENT ME! */
    private int searchWindowSide = 15;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textSimilarityWindowSide;

    /** DOCUMENT ME! */
    private JTextField textNoiseStandardDeviation;

    /** DOCUMENT ME! */
    private JTextField textSearchWindowSide;
    
    private JLabel labelDegree;
    
    private JTextField textDegree;
    
    private float degreeOfFiltering = 1.0f;
    
    private JCheckBox doRicianCheckBox;
    
    private boolean doRician = false;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogNonlocalMeansFilter() { }

    /**
     * Creates a new JDialogNonlocalMeansFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogNonlocalMeansFilter(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (source.equals(doRicianCheckBox)) {
            if (doRicianCheckBox.isSelected()) {
                labelDegree.setEnabled(true);
                textDegree.setEnabled(true);
            }
            else {
                labelDegree.setEnabled(false);
                textDegree.setEnabled(false);
            }
        } else { // else if (source == thresholdCheckbox)
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmNonlocalMeansFilter) {
            image.clearMask();

            if ((nlMeansFilterAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        nlMeansFilterAlgo.finalize();
        nlMeansFilterAlgo = null;
        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += searchWindowSide + delim;
        str += similarityWindowSide + delim;
        str += noiseStandardDeviation + delim;
        str += degreeOfFiltering + delim;
        str += doRician + delim;
        str += image25D;

        return str;
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
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (newImage != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                textSearchWindowSide.setText("" + MipavUtil.getInt(st));
                textSimilarityWindowSide.setText("" + MipavUtil.getInt(st));
                textNoiseStandardDeviation.setText("" +  MipavUtil.getFloat(st));
                textDegree.setText("" + MipavUtil.getFloat(st));
                doRician = MipavUtil.getBoolean(st);
                doRicianCheckBox.setSelected(doRician);
                textDegree.setEnabled(doRician);
                labelDegree.setEnabled(doRician);
                image25DCheckBox.setSelected(MipavUtil.getBoolean(st));

                if (MipavUtil.getBoolean(st)) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }

            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }


    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets whether 3D images are 3D or 2.5D filtered.
     *
     * @param  image25D  true for 2.5D filtering
     */
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }

    /**
     * Accessor that sets the similarity window side.
     *
     * @param  similarityWindowSide  Value to set similarity window side to.
     */
    public void setSimilarityWindowSide(int similarityWindowSide) {
        this.similarityWindowSide = similarityWindowSide;
    }

    /**
     * Accessor that sets the noise standard deviation.
     *
     * @param  noiseStandardDeviation  Value to set noiseStandardDeviation to.
     */
    public void setNoiseStandardDeviation(int noiseStandardDeviation) {
        this.noiseStandardDeviation = noiseStandardDeviation;
    }

    /**
     * Accessor that sets the search window side.
     *
     * @param  searchWindowSide  Value to set search window side to.
     */
    public void setSearchWindowSide(int searchWindowSide) {
        this.searchWindowSide = searchWindowSide;
    }
    
    /**
     * Accessor that sets degree of filtering
     * @param degreeOfFiltering
     */
    public void setDegreeOfFiltering(float degreeOfFiltering) {
        this.degreeOfFiltering = degreeOfFiltering;
    }
    
    /**
     * Accessor that sets doRician
     * @param doRician
     */
    public void setDoRician(boolean doRician) {
        this.doRician = doRician;
    }

    /**
     * Once all the necessary variables are set, call the Nonlocal Means filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_NonlocalMeans");
        int[] destExtents;

        if (image.getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
        } else {
            destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];
        }

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                if (image.isColorImage()) {
                    resultImage = new ModelImage(ModelImage.ARGB, destExtents, name);
                } else {
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);
                }

                // resultImage = (ModelImage)image.clone();
                // resultImage.setImageName(name);
                // Make algorithm
                nlMeansFilterAlgo = new AlgorithmNonlocalMeansFilter(resultImage, image, searchWindowSide,
                                         similarityWindowSide, noiseStandardDeviation, 
                                         degreeOfFiltering, doRician, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nlMeansFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), nlMeansFilterAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (nlMeansFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    nlMeansFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Nonlocal Means Filter: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                nlMeansFilterAlgo = new AlgorithmNonlocalMeansFilter(null, image, searchWindowSide, 
                                        similarityWindowSide, noiseStandardDeviation,
                                        degreeOfFiltering, doRician, image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                nlMeansFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), nlMeansFilterAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (nlMeansFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    nlMeansFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Nonlocal Means Filter: unable to allocate enough memory");

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
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        searchWindowSide = scriptParameters.getParams().getInt("search_window_side");
        similarityWindowSide = scriptParameters.getParams().getInt("similarity_window_side");
        noiseStandardDeviation = scriptParameters.getParams().getFloat("noise_standard_deviation");
        degreeOfFiltering = scriptParameters.getParams().getFloat("degree_of_filtering");
        doRician = scriptParameters.getParams().getBoolean("do_rician");
        image25D = scriptParameters.doProcess3DAs25D();
    }


    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("search_window_side", searchWindowSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("similarity_window_side", similarityWindowSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_standard_deviation", noiseStandardDeviation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("degree_of_filtering", degreeOfFiltering));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_rician", doRician));
        scriptParameters.storeProcess3DAs25D(image25D);
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Nonlocal Means Filter");

        JPanel mainPanel;
        mainPanel = new JPanel();
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

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        mainPanel.add(paramPanel, gbc);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        labelSearchWindowSide = createLabel("Search window side (odd)");

        paramPanel.add(labelSearchWindowSide, gbc2);

        gbc2.gridx = 1;
        textSearchWindowSide = createTextField("15");
        paramPanel.add(textSearchWindowSide, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 1;
        labelSimilarityWindowSide = createLabel("Similarity window side (odd) ");
        paramPanel.add(labelSimilarityWindowSide, gbc2);

        gbc2.gridx = 1;
        textSimilarityWindowSide = createTextField("7");
        paramPanel.add(textSimilarityWindowSide, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 2;
        labelNoiseStandardDeviation = createLabel("Noise standard deviation ");
        paramPanel.add(labelNoiseStandardDeviation, gbc2);

        gbc2.gridx = 1;
        textNoiseStandardDeviation = createTextField("10.0");
        paramPanel.add(textNoiseStandardDeviation, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy = 3;
        labelDegree = createLabel("Degree of filtering ");
        labelDegree.setEnabled(doRician);
        paramPanel.add(labelDegree, gbc2);
        
        gbc2.gridx = 1;
        textDegree = createTextField("1.414");
        textDegree.setEnabled(doRician);
        paramPanel.add(textDegree, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 4;
        doRicianCheckBox = new JCheckBox("Deal with Rician noise in MRI");
        doRicianCheckBox.setFont(serif12);
        doRicianCheckBox.setSelected(false);
        doRicianCheckBox.addActionListener(this);
        paramPanel.add(doRicianCheckBox, gbc2);
        
        if (image.getNDims() > 2) {
            gbc2.gridx = 0;
            gbc2.gridy = 5;
            gbc2.gridwidth = 2;

            image25DCheckBox = new JCheckBox("Process each slice independently (2.5D)");
            image25DCheckBox.setFont(serif12);
            paramPanel.add(image25DCheckBox, gbc2);
            image25DCheckBox.setSelected(false);
        } // if (image.getNDims > 2)

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setBounds(10, 16, 120, 25);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textSearchWindowSide.getText();

        if (testParameter(tmpStr, 5, 101)) {
            searchWindowSide = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Search window side must be between 5 and 101");
            textSearchWindowSide.requestFocus();
            textSearchWindowSide.selectAll();

            return false;
        }
        
        if ((searchWindowSide % 2) == 0) {
            MipavUtil.displayError("Search window side must be an odd number");
            textSearchWindowSide.requestFocus();
            textSearchWindowSide.selectAll();
            return false;
        }

        tmpStr = textSimilarityWindowSide.getText();

        if (testParameter(tmpStr, 3, 99)) {
            similarityWindowSide = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Similarity window side must be between 3 and 99");
            textSimilarityWindowSide.requestFocus();
            textSimilarityWindowSide.selectAll();

            return false;
        }
        
        if ((similarityWindowSide % 2) == 0) {
            MipavUtil.displayError("Similarity window side must be an odd number");
            textSimilarityWindowSide.requestFocus();
            textSimilarityWindowSide.selectAll();
            return false;
        }
        
        if (similarityWindowSide >= searchWindowSide) {
            MipavUtil.displayError("Similarity window side must be less than search window side");
            textSimilarityWindowSide.requestFocus();
            textSimilarityWindowSide.selectAll();
            return false;
        }

        tmpStr = textNoiseStandardDeviation.getText();

        if (testParameter(tmpStr, 0.001, 1000.0)) {
            noiseStandardDeviation = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Radius must be between 0.001 and 1000.0");
            textNoiseStandardDeviation.requestFocus();
            textNoiseStandardDeviation.selectAll();

            return false;
        }
        
        doRician = doRicianCheckBox.isSelected();
        
        if (doRician) {
            tmpStr = textDegree.getText();
            if (testParameter(tmpStr, 1.0, 10.0)) {
                degreeOfFiltering = Float.valueOf(tmpStr).floatValue();    
            }
            else {
                MipavUtil.displayError("Degree of filtering must be between 1.0 and 10.0");
                textDegree.requestFocus();
                textDegree.selectAll();
            }
        }

        if (image.getNDims() > 2) {
            image25D = image25DCheckBox.isSelected();
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Nonlocal Mean filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Nonlocal Mean filter.");
            }

            public String getShortLabel() {
                return new String("NonlocalMean");
            }

            public String getLabel() {
                return new String("Nonlocal Mean");
            }

            public String getName() {
                return new String("Nonlocal Mean");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterInt("search_window_side", 15));
            table.put(new ParameterInt("similarity_window_side", 7));
            table.put(new ParameterFloat("noise_standard_deviation",10f));
            table.put(new ParameterFloat("degree_of_filtering",1.414f));
            table.put(new ParameterBoolean("do_rician", false));
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
