package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogIndependentComponents extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmIndependentComponents aicAlgo;

    /** DOCUMENT ME! */
    private boolean changeRemoveIndex = true;

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private JPanelColorChannels colorPanel;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private double endTol;

    /** DOCUMENT ME! */
    private JRadioButton deflationaryOrthogonalization;

    /** DOCUMENT ME! */
    private JRadioButton maximumLikelihoodEstimation;

    /** DOCUMENT ME! */
    private JRadioButton symmetricOrthogonalization;

    /** DOCUMENT ME! */
    private JList imageList;

    /** DOCUMENT ME! */
    private JPanel imagePanel;

    /** DOCUMENT ME! */
    private ButtonGroup nonlinearFunctionGroup;

    /** DOCUMENT ME! */
    private JPanel nonlinearFunctionPanel;
    
    private int nonlinearFunction;
    
    private JRadioButton tanhButton;
    
    private JLabel labela1;
    
    private JTextField texta1;
    
    private double a1 = 1.0; // 1.0 <= a1 <= 2.0
    
    private JRadioButton expButton;
    
    private JRadioButton cubicButton;

    /** DOCUMENT ME! */
    private JLabel labelEndTol;

    /** DOCUMENT ME! */
    private JLabel labelMaxIter;

    /** DOCUMENT ME! */
    private JLabel labelICNumber;

    /** DOCUMENT ME! */
    private int maxIter;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** Number of independent components */
    private int icNumber;

    /** DOCUMENT ME! */
    private JPanel paramPanel; 

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private int removeIndex;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int icAlgorithm;

    /** DOCUMENT ME! */
    private ButtonGroup icAlgorithmGroup;

    /** DOCUMENT ME! */
    private JPanel icAlgorithmPanel;

    /** DOCUMENT ME! */
    private ModelImage[] srcImage = null; // all source images

    /** DOCUMENT ME! */
    private int srcNumber = 1;

    /** DOCUMENT ME! */
    private ModelImage[] tempImage = null;

    /** DOCUMENT ME! */
    private JTextField textEndTol;

    /** DOCUMENT ME! */
    private JTextField textMaxIter;

    /** DOCUMENT ME! */
    private JTextField textICNumber;

    
    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogIndependentComponents() { }

    // false = apply algorithm only to VOI regions

    /**
     * Creates a new JDialogIndependentComponents object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogIndependentComponents(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = new ModelImage[1];
        srcImage[0] = im;
        userInterface = ViewUserInterface.getReference();

        if (srcImage[0].getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = srcImage[0].getExtents()[0]; // X dim
            destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
        } else { // srcImage[0].getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = srcImage[0].getExtents()[0];
            destExtents[1] = srcImage[0].getExtents()[1];
            destExtents[2] = srcImage[0].getExtents()[2];
        }

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

        int i, j;

        if (command.equals("Choose")) {
            ModelImage newImage = open();

            if (!checkImage(newImage)) {
                return;
            }

            srcNumber++;
            tempImage = new ModelImage[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                tempImage[i] = srcImage[i];
            }

            srcImage = null;
            srcImage = new ModelImage[srcNumber];

            for (i = 0; i < (srcNumber - 1); i++) {
                srcImage[i] = tempImage[i];
            }

            tempImage = null;
            srcImage[srcNumber - 1] = newImage;
            model.addElement(srcImage[srcNumber - 1].getImageName());

            newImage = null;
            removeButton.setEnabled(true);
        } // if (command.equals("Choose"))
        else if ((command.equals("Remove")) && (removeIndex == 0)) {

            // Cannot remove original image
            MipavUtil.displayError("Cannot remove original loaded image");
        } else if ((command.equals("Remove")) && (removeIndex >= 1) && (removeIndex <= (srcNumber - 1))) {

            // changeRemoveIndex = false is needed because the model.removeElement
            // line causes valueChanged to execute.  Without changeRemoveIndex an
            // unselected element causes removeIndex to be changed to -1.
            changeRemoveIndex = false;
            model.removeElement(srcImage[removeIndex].getImageName());
            tempImage = new ModelImage[srcNumber - 1];

            for (i = 0, j = 0; i < srcNumber; i++) {

                if (i != removeIndex) {
                    tempImage[j++] = srcImage[i];
                }
            } // for ( i = 0, j=0; i < srcNumber; i++)

            srcImage[removeIndex].disposeLocal();
            srcImage[removeIndex] = null;
            changeRemoveIndex = true;
            srcImage = null;
            srcImage = new ModelImage[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                srcImage[i] = tempImage[i];
            }

            tempImage = null;
            srcNumber--;

            if (srcNumber == 1) {
                removeButton.setEnabled(false);
            }
        } // else if ((command.equals("Remove"))  && (removeIndex >= 1) && (removeIndex <= (srcNumber - 1)))
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        }
        else if ((source == tanhButton) || (source == expButton) || (source == cubicButton)) {
        	if (tanhButton.isSelected()) {
        	    labela1.setEnabled(true);
        	    texta1.setEnabled(true);
        	}
        	else {
        		labela1.setEnabled(false);
        		texta1.setEnabled(false);
        	}
        }
        else if ((source == symmetricOrthogonalization) || (source == deflationaryOrthogonalization) ||
        		 (source == maximumLikelihoodEstimation)) {
        	if ((symmetricOrthogonalization.isSelected()) || (maximumLikelihoodEstimation.isSelected())) {
        		labelICNumber.setEnabled(false);
        		textICNumber.setEnabled(false);
        	}
        	else {
        		labelICNumber.setEnabled(true);
        		textICNumber.setEnabled(true);
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
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[icNumber];

        if (algorithm instanceof AlgorithmIndependentComponents) {
            srcImage[0].clearMask();

            if ((aicAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
                for (i = 0; i < icNumber; i++) {
                    updateFileInfo(srcImage[0], resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage0 frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < icNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the end tol.
     *
     * @param  scale  Value to set end tol to.
     */
    public void setEndTol(double scale) {
        endTol = scale;
    }

    /**
     * Accessor that sets the max iterations.
     *
     * @param  max  The max iterations
     */
    public void setMaxIter(int max) {
        maxIter = max;
    }

    /**
     * Accessor that sets the number of independent components.
     *
     * @param  classes  The number of independent components.
     */
    public void setICNumber(int icNumber) {
        this.icNumber = icNumber;
    }

    /**
     * Accessor that sets the icAlgorithm type (SYMMETRIC_ORTHOGONALIZATION, DEFLATIONARY_ORTHOGONALIZATION,
     *                                          or MAXIMUM_LIKELIHOOD_ESTIMATION).
     *
     * @param  icAlgorithm  The independent component algorithm.
     */
    public void setICAlgorithm(int icAlgorithm) {
        this.icAlgorithm = icAlgorithm;
    }
    
    /**
     * Accessor that sets the nonlinear function type(tanh(a1*y), y*exp(-y*y/2), or y*y*y)
     * @param nonlinearFunction
     */
    public void setNonlinearFunction(int nonlinearFunction) {
    	this.nonlinearFunction = nonlinearFunction;
    }
    
    /**
     * Accessor that sets parameter a1 for tanh function
     * @param a1
     */
    public void seta1(double a1) {
    	this.a1 = a1;
    }

    /**
     * Accessor that sets the array of source images.
     *
     * @param  images  new source images.
     */
    public void setSourceImage(ModelImage[] images) {
        srcImage = images;
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) {

        if (changeRemoveIndex) {
            JList source = (JList) evt.getSource();
            removeIndex = source.getSelectedIndex();
        }
    }

    /**
     * Once all the necessary variables are set, call the Fuzzy C Means algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;
        System.gc();

        try {
            resultImage = new ModelImage[icNumber];

            for (i = 0; i < icNumber; i++) {
                String name = makeImageName(srcImage[0].getImageName(), "_icomponent" + (i + 1));

                resultImage[i] = new ModelImage(ModelStorageBase.DOUBLE, destExtents, name);
            }

            // Make algorithm
            aicAlgo = new AlgorithmIndependentComponents(resultImage, srcImage, icNumber, icAlgorithm, nonlinearFunction,
                                                         a1, endTol, maxIter,
                                                         colorPanel.isRedProcessingRequested(),
                                                         colorPanel.isGreenProcessingRequested(),
                                                         colorPanel.isBlueProcessingRequested());

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            aicAlgo.addListener(this);


            createProgressBar(srcImage[0].getImageName(), aicAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (aicAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                aicAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < icNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Independent Components: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < icNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        int numInputImages = scriptParameters.getParams().getInt("number_of_input_images");
        srcImage = new ModelImage[numInputImages];

        for (int i = 1; i <= numInputImages; i++) {
            srcImage[i - 1] = scriptParameters.retrieveInputImage(i);
        }

        if (srcImage[0].getNDims() == 2) { // source image is 2D
            destExtents = new int[2];
            destExtents[0] = srcImage[0].getExtents()[0]; // X dim
            destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
        } else { // srcImage[0].getNDims)() == 3
            destExtents = new int[3];
            destExtents[0] = srcImage[0].getExtents()[0];
            destExtents[1] = srcImage[0].getExtents()[1];
            destExtents[2] = srcImage[0].getExtents()[2];
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage[0].getParentFrame();

        setICNumber(scriptParameters.getParams().getInt("ic_number"));
        setEndTol(scriptParameters.getParams().getDouble("end_tolerance"));
        setMaxIter(scriptParameters.getParams().getInt("max_iterations"));
        setICAlgorithm(scriptParameters.getParams().getInt("ic_algorithm"));
        setNonlinearFunction(scriptParameters.getParams().getInt("nonlinear_function"));
        seta1(scriptParameters.getParams().getDouble("a_1"));

        colorPanel = new JPanelColorChannels(srcImage[0]);
        scriptParameters.setColorOptionsGUI(colorPanel);

        for (int i = 0; i < srcImage.length; i++) {

            if (!checkImage(srcImage[i])) {
                return;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", srcImage.length));

        for (int i = 0; i < srcImage.length; i++) {
            scriptParameters.storeInputImage(srcImage[i]);
        }

        for (int i = 0; i < icNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("ic_number", icNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end_tolerance", endTol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("ic_algorithm", icAlgorithm));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nonlinear_function", nonlinearFunction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("a_1", a1));
        scriptParameters.storeColorOptions(colorPanel);
    }

    /**
     * Checks the color and dimensionality of the new image vs. the original source image. All new images should have
     * the same color modality as the source and be of the same dimensions.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if ((srcImage[0].isColorImage() == true) && (testImage.isColorImage() == false)) {
            MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                                   ") unless the original file is color.");

            return false;
        }

        if (srcImage[0].getNDims() != testImage.getNDims()) {
            MipavUtil.displayError("Error! " + srcImage[0].getImageName() + " is " + srcImage[0].getNDims() +
                                   "D, while " + testImage.getImageName() + " is " + testImage.getNDims() + "D");

            return false;
        }

        for (int i = 0; i < srcImage[0].getNDims(); i++) {

            if ((testImage != null) && (destExtents[i] != testImage.getExtents()[i])) {
                MipavUtil.displayError("Error! For dimension = " + i + " " + srcImage[0].getImageName() +
                                       " has length = " + destExtents[i] + " while " + testImage.getImageName() +
                                       " has length = " + testImage.getExtents()[i]);

                return false;
            }
        }

        return true;

    }

   

    /**
     * A private helper function to get the current used FileFilter from JFileChooser.
     *
     * @param   chooser  DOCUMENT ME!
     * @param   index    the index of the choosable file filters.
     *
     * @return  the current used file filter.
     */
    private FileFilter getFileFilter(JFileChooser chooser, int index) {
        FileFilter[] filters = chooser.getChoosableFileFilters();
        String[] descriptions = ViewImageFileFilter.getDescriptions();

        for (int i = 0; i < filters.length; i++) {

            if (filters[i].getDescription().equals(descriptions[index])) {
                return filters[i];
            }
        }

        return null;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Independent Components");

        labelEndTol = new JLabel("End tolerance");
        labelEndTol.setForeground(Color.black);
        labelEndTol.setFont(serif12);

        textEndTol = new JTextField(10);
        textEndTol.setText("1.0E-6");
        textEndTol.setFont(serif12);

        labelMaxIter = new JLabel("Maximum number of iterations");
        labelMaxIter.setForeground(Color.black);
        labelMaxIter.setFont(serif12);

        textMaxIter = new JTextField(5);
        textMaxIter.setText("200");
        textMaxIter.setFont(serif12);

        JPanel upperPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelEndTol, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textEndTol, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMaxIter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMaxIter, gbc);

        icAlgorithmGroup = new ButtonGroup();
        symmetricOrthogonalization = new JRadioButton("Symmetric orthogonalization", true);
        symmetricOrthogonalization.setFont(serif12);
        symmetricOrthogonalization.addActionListener(this);
        icAlgorithmGroup.add(symmetricOrthogonalization);

        deflationaryOrthogonalization = new JRadioButton("Deflationary orthogonalization", false);
        deflationaryOrthogonalization.setFont(serif12);
        deflationaryOrthogonalization.addActionListener(this);
        icAlgorithmGroup.add(deflationaryOrthogonalization);
        
        labelICNumber = new JLabel("Number of independent components");
        labelICNumber.setForeground(Color.black);
        labelICNumber.setFont(serif12);
        labelICNumber.setEnabled(false);

        textICNumber = new JTextField(5);
        textICNumber.setText("2");
        textICNumber.setFont(serif12);
        textICNumber.setEnabled(false);

        maximumLikelihoodEstimation = new JRadioButton("Maximum likelihood estimation", false);
        maximumLikelihoodEstimation.setFont(serif12);
        maximumLikelihoodEstimation.addActionListener(this);
        icAlgorithmGroup.add(maximumLikelihoodEstimation);

        icAlgorithmPanel = new JPanel(new GridBagLayout());
        icAlgorithmPanel.setBorder(buildTitledBorder("IC Algorithm"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        icAlgorithmPanel.add(symmetricOrthogonalization, gbc);
        gbc.gridy = 1;
        icAlgorithmPanel.add(deflationaryOrthogonalization, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        icAlgorithmPanel.add(labelICNumber, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        icAlgorithmPanel.add(textICNumber, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        icAlgorithmPanel.add(maximumLikelihoodEstimation, gbc);
        
        nonlinearFunctionGroup = new ButtonGroup();
        tanhButton = new JRadioButton("tanh(a1*y)", true);
        tanhButton.setFont(serif12);
        tanhButton.addActionListener(this);
        nonlinearFunctionGroup.add(tanhButton);
        
        labela1 = new JLabel("a1 (1.0 - 2.0)");
        labela1.setForeground(Color.black);
        labela1.setFont(serif12);
        
        texta1 = new JTextField(10);
        texta1.setText("1.0");
        texta1.setFont(serif12);
        
        expButton = new JRadioButton("y*exp(-y*y/2)", false);
        expButton.setFont(serif12);
        expButton.addActionListener(this);
        nonlinearFunctionGroup.add(expButton);
        
        cubicButton = new JRadioButton("y*y*y", false);
        cubicButton.setFont(serif12);
        cubicButton.addActionListener(this);
        nonlinearFunctionGroup.add(cubicButton);
        
        nonlinearFunctionPanel = new JPanel(new GridBagLayout());
        nonlinearFunctionPanel.setBorder(buildTitledBorder("Nonlinear Function"));
         
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        nonlinearFunctionPanel.add(tanhButton, gbc);
        gbc.gridy = 1;
        nonlinearFunctionPanel.add(labela1, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        nonlinearFunctionPanel.add(texta1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        nonlinearFunctionPanel.add(expButton, gbc);
        gbc.gridy = 3;
        nonlinearFunctionPanel.add(cubicButton, gbc);
        

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        int ypos = 0;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = ypos++;
        gbc.gridwidth = 1;
        paramPanel.add(icAlgorithmPanel, gbc);
        gbc.gridy = ypos++;
        paramPanel.add(nonlinearFunctionPanel, gbc);

        colorPanel = new JPanelColorChannels(srcImage[0]);

        if (srcImage[0].isColorImage()) {
            gbc.gridx = 0;
            gbc.gridy = ypos++;
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            paramPanel.add(colorPanel, gbc);
        } // if (doColor)

        
        imagePanel = new JPanel(new BorderLayout());
        imagePanel.setBorder(buildTitledBorder("Load Image(s)"));

        model = new DefaultListModel();
        model.addElement(srcImage[0].getImageName());
        imageList = new JList(model);
        imageList.setVisibleRowCount(6);
        imageList.setPreferredSize(new Dimension(300, 120));
        imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList.addListSelectionListener(this);
        imagePanel.add(imageList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        imagePanel.add(chooserPanel, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(imagePanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage open() {
        JFileChooser chooser = null;
        FileIO fileIO = null;
        boolean multiFile = false;
        String fileName;
        String directory;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            FileFilter currentFileFilter = getFileFilter(chooser, Preferences.getFileFilter());
            chooser.setFileFilter(currentFileFilter);

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            fileIO = new FileIO();

            return fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int colorsRequested = 0;
        
        if (srcImage[0].isColorImage()) {
        	if (colorPanel.isRedProcessingRequested()) {
        		colorsRequested++;
        	}
        	if (colorPanel.isGreenProcessingRequested()) {
        		colorsRequested++;
        	}
        	if (colorPanel.isBlueProcessingRequested()) {
        		colorsRequested++;
        	}
        }
        
        if (symmetricOrthogonalization.isSelected()) {
            icAlgorithm = AlgorithmIndependentComponents.SYMMETRIC_ORTHOGONALIZATION;
        } else if (deflationaryOrthogonalization.isSelected()) {
            icAlgorithm = AlgorithmIndependentComponents.DEFLATIONARY_ORTHOGONALIZATION;
        } else {
            icAlgorithm = AlgorithmIndependentComponents.MAXIMUM_LIKELIHOOD_ESTIMATION;
        }
        
        if ((icAlgorithm == AlgorithmIndependentComponents.SYMMETRIC_ORTHOGONALIZATION) ||
            (icAlgorithm == AlgorithmIndependentComponents.MAXIMUM_LIKELIHOOD_ESTIMATION)) {
        	if (srcImage[0].isColorImage()) {
        	    icNumber = colorsRequested * srcImage.length;
        	}
        	else {
        		icNumber = srcImage.length;
        	}
        }
        else {
	        tmpStr = textICNumber.getText();
	
	        if (srcImage[0].isColorImage()) {
	        	if (testParameter(tmpStr, 1.0, colorsRequested * srcImage.length)) {
	                icNumber = Integer.valueOf(tmpStr).intValue();
	            } else {
	                textICNumber.requestFocus();
	                textICNumber.selectAll();
	
	                return false;
	            }	
	        }
	        else {
		        if (testParameter(tmpStr, 1.0, srcImage.length)) {
		            icNumber = Integer.valueOf(tmpStr).intValue();
		        } else {
		            textICNumber.requestFocus();
		            textICNumber.selectAll();
		
		            return false;
		        }
	        }
        }

        tmpStr = textEndTol.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, 1.0)) {
            endTol = Double.valueOf(tmpStr).doubleValue();
        } else {
            textEndTol.requestFocus();
            textEndTol.selectAll();

            return false;
        }

        tmpStr = textMaxIter.getText();

        if (testParameter(tmpStr, 1.0, 10000.0)) {
            maxIter = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxIter.requestFocus();
            textMaxIter.selectAll();

            return false;
        }
        
        if (tanhButton.isSelected()) {
        	nonlinearFunction = AlgorithmIndependentComponents.TANH_FUNCTION;
        }
        else if (expButton.isSelected()) {
        	nonlinearFunction = AlgorithmIndependentComponents.EXP_FUNCTION;
        }
        else {
        	nonlinearFunction = AlgorithmIndependentComponents.CUBIC_FUNCTION;
        }
        
        if (nonlinearFunction == AlgorithmIndependentComponents.TANH_FUNCTION) {
        	tmpStr = texta1.getText();

            if (testParameter(tmpStr, 1.0, 2.0)) {
                a1 = Double.valueOf(tmpStr).doubleValue();
            } else {
                texta1.requestFocus();
                texta1.selectAll();

                return false;
            }	
        }

        return true;
    }

}
