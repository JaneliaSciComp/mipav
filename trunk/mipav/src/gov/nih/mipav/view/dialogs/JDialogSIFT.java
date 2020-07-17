package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;

/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogSIFT extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	private String fileDir[];
    private String fileName[];
    private String selectedDirectory = null;
    private String selectedFileName = null;
    private String tempFileDir[];
    private String tempFileName[];
    private SIFT SIFTAlgo;
    private boolean verbose = false;
    private String outarg = null;
    private String framesarg = null;
    private String descriptorarg = null;
    private String metaarg = null;
    private String read_framesarg = null;
    private String gssarg = null;
    // Octaves
    private int O = 3;
    // Levels
    private int S = 3;
    // first_octave
    private int  omin = 0 ;
    private double   edge_thresh  = 10 ;
    private double   peak_thresh  = 0 ;
    private double   magnif       = 3.0 ;
    private boolean force_orientations = false ;
    private boolean writeFrames = false;
    private boolean readFrames = false;
    private boolean writeDescriptor = false;
    private boolean writeMeta = false;
    private boolean writeGss = false;
    
    private boolean changeRemoveIndex = true;
    
    private JButton removeButton;

    private int removeIndex;
    
    private int srcNumber = 1;
    
    private DefaultListModel<String> model;
    
    private JList imageList;
    
    private JPanel imagePanel;
    
    private JButton chooserButton;
    
    private JPanel paramPanel;
    
    ViewUserInterface userInterface;
    
    private JLabel labelNOctaves;
    
    private JTextField textNOctaves;
    
    private JLabel labelNLevels;
    
    private JTextField textNLevels;
    
    private JLabel labelFirstOctave;
    
    private JTextField textFirstOctave;
    
    private JLabel labelEdgeThresh;
    
    private JTextField textEdgeThresh;
    
    private JLabel labelPeakThresh;
    
    private JTextField textPeakThresh;
    
    private JLabel labelMagnification;
    
    private JTextField textMagnification;
    
    private JCheckBox orientationsCheckBox;
    
    private JCheckBox verboseCheckBox;
    
    private JCheckBox writeFramesCheckBox;
    
    private JCheckBox readFramesCheckBox;
    
    private JCheckBox writeDescriptorCheckBox;
    
    private JCheckBox writeMetaCheckBox;
    
    private JCheckBox writeGssCheckBox;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSIFT() { }

    // false = apply algorithm only to VOI regions

    /**
     * Creates a new JDialogMSFuzzyCMeans object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogSIFT(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        fileDir = new String[1];
        fileName = new String[1];
        fileDir[0] = im.getFileInfo()[0].getFileDirectory();
        fileName[0] = im.getFileInfo()[0].getFileName();
        userInterface = ViewUserInterface.getReference();
        init();
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Scale Invariant Feature Transform (SIFT)");
        
        labelNOctaves = new JLabel("Number of octaves");
        labelNOctaves.setForeground(Color.black);
        labelNOctaves.setFont(serif12);

        textNOctaves = new JTextField(5);
        textNOctaves.setText("3");
        textNOctaves.setFont(serif12);
        
        labelNLevels = new JLabel("Number of levels");
        labelNLevels.setForeground(Color.black);
        labelNLevels.setFont(serif12);

        textNLevels = new JTextField(5);
        textNLevels.setText("3");
        textNLevels.setFont(serif12);
        
        labelFirstOctave = new JLabel("Index of first octave");
        labelFirstOctave.setForeground(Color.black);
        labelFirstOctave.setFont(serif12);

        textFirstOctave = new JTextField(5);
        textFirstOctave.setText("0");
        textFirstOctave.setFont(serif12);
        
        labelEdgeThresh = new JLabel("Edge threshold");
        labelEdgeThresh.setForeground(Color.black);
        labelEdgeThresh.setFont(serif12);

        textEdgeThresh = new JTextField(10);
        textEdgeThresh.setText("10.0");
        textEdgeThresh.setFont(serif12);
        
        labelPeakThresh = new JLabel("Peak threshold");
        labelPeakThresh.setForeground(Color.black);
        labelPeakThresh.setFont(serif12);

        textPeakThresh = new JTextField(10);
        textPeakThresh.setText("0.0");
        textPeakThresh.setFont(serif12);
        
        labelMagnification = new JLabel("Magnification factor");
        labelMagnification.setForeground(Color.black);
        labelMagnification.setFont(serif12);

        textMagnification = new JTextField(10);
        textMagnification.setText("3.0");
        textMagnification.setFont(serif12);
        
        orientationsCheckBox = new JCheckBox("Force orientations");
        orientationsCheckBox.setFont(serif12);
        orientationsCheckBox.setSelected(false);
        
        verboseCheckBox = new JCheckBox("Be verbose");
        verboseCheckBox.setFont(serif12);
        verboseCheckBox.setSelected(false);
        
        writeFramesCheckBox = new JCheckBox("Write frames (.frame)");
        writeFramesCheckBox.setFont(serif12);
        writeFramesCheckBox.setSelected(false);
        
        readFramesCheckBox = new JCheckBox("Read frames (.frame)");
        readFramesCheckBox.setFont(serif12);
        readFramesCheckBox.setSelected(false);
        
        writeDescriptorCheckBox = new JCheckBox("Write descriptor (.descr)");
        writeDescriptorCheckBox.setFont(serif12);
        writeDescriptorCheckBox.setSelected(false);
        
        writeMetaCheckBox = new JCheckBox("Write meta (.meta)");
        writeMetaCheckBox.setFont(serif12);
        writeMetaCheckBox.setSelected(false);
        
        writeGssCheckBox = new JCheckBox("Write gss (_gss.pgm)");
        writeGssCheckBox.setFont(serif12);
        writeGssCheckBox.setSelected(false);
        
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
        upperPanel.add(labelNOctaves, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textNOctaves, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelNLevels, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textNLevels, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelFirstOctave, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textFirstOctave, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelEdgeThresh, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textEdgeThresh, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelPeakThresh, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textPeakThresh, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMagnification, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMagnification, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        upperPanel.add(orientationsCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.gridwidth = 2;
        upperPanel.add(verboseCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.gridwidth = 2;
        upperPanel.add(writeFramesCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.gridwidth = 2;
        upperPanel.add(readFramesCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 2;
        upperPanel.add(writeDescriptorCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 2;
        upperPanel.add(writeMetaCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 12;
        gbc.gridwidth = 2;
        upperPanel.add(writeGssCheckBox, gbc);
        
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
        
        imagePanel = new JPanel(new BorderLayout());
        imagePanel.setBorder(buildTitledBorder("Load Image(s)"));

        model = new DefaultListModel<String>();
        model.addElement(fileDir[0] + fileName[0]);
        imageList = new JList<>(model);
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
        
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(paramPanel, BorderLayout.NORTH);
        mainPanel.add(buildButtons(), BorderLayout.SOUTH);
        
        JScrollPane scrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        getContentPane().add(scrollPane);

        pack();
        setVisible(true);
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        int i, j;

        if (command.equals("Choose")) {
            selectFileName();
            if ((selectedFileName == null) || (selectedDirectory == null)) {
            	return;
            }
            srcNumber++;
            tempFileName = new String[srcNumber - 1];
            tempFileDir = new String[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                tempFileName[i] = fileName[i];
                tempFileDir[i] = fileDir[i];
                fileName[i] = null;
                fileDir[i] = null;
            }

            fileName = null;
            fileDir = null;
            fileName = new String[srcNumber];
            fileDir = new String[srcNumber];

            for (i = 0; i < (srcNumber - 1); i++) {
                fileName[i] = tempFileName[i];
                fileDir[i] = tempFileDir[i];
                tempFileName[i] = null;
                tempFileDir[i] = null;
            }

            tempFileName = null;
            tempFileDir = null;
            fileName[srcNumber - 1] = selectedFileName;
            fileDir[srcNumber - 1] = selectedDirectory;
            model.addElement(fileDir[srcNumber-1]+fileName[srcNumber-1]);

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
            model.removeElement(fileDir[removeIndex]+fileName[removeIndex]);
            tempFileName = new String[srcNumber - 1];
            tempFileDir = new String[srcNumber - 1];

            for (i = 0, j = 0; i < srcNumber; i++) {

                if (i != removeIndex) {
                    tempFileName[j] = fileName[i];
                    tempFileDir[j++] = fileDir[i];
                    fileName[i] = null;
                    fileDir[i] = null;
                }
            } // for ( i = 0, j=0; i < srcNumber; i++)

            fileName[removeIndex] = null;
            fileDir[removeIndex] = null;
            changeRemoveIndex = true;
            fileName = null;
            fileDir = null;
            fileName = new String[srcNumber - 1];
            fileDir = new String[srcNumber - 1];

            for (i = 0; i < (srcNumber - 1); i++) {
                fileName[i] = tempFileName[i];
                fileDir[i] = tempFileDir[i];
                tempFileName[i] = null;
                tempFileDir[i] = null;
            }

            tempFileName = null;
            tempFileDir = null;
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
            MipavUtil.showWebHelp("");
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private void selectFileName() {
    	int i;
        JFileChooser chooser = null;
        selectedFileName = null;
        selectedDirectory = null;

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

            chooser.setDialogTitle("Select Image File");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                selectedFileName = chooser.getSelectedFile().getName();
                selectedDirectory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                if ((selectedFileName != null) && (selectedDirectory != null)) {
	                for (i = 0; i < fileName.length; i++) {
	                	if ((selectedFileName.equals(fileName[i])) && (selectedDirectory.equals(fileDir[i]))) {
	                		System.err.println("This file has already been selected");
	                		selectedFileName = null;
	                		selectedDirectory = null;
	                		return;
	                	}
	                }
                userInterface.setDefaultDirectory(selectedDirectory);
                }
            } else {
                return;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return;
        }

        
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
    
    protected void callAlgorithm() {
        SIFTAlgo = new SIFT(fileDir, fileName, verbose, outarg, framesarg,
        		descriptorarg, metaarg, read_framesarg, gssarg, O, S,
        		omin, edge_thresh, peak_thresh, magnif, force_orientations,
        		writeFrames, readFrames, writeDescriptor, writeMeta,
        		writeGss);
        
        createProgressBar(fileName[0], SIFTAlgo);

        // Hide dialog
        setVisible(false);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (SIFTAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            SIFTAlgo.run();
        }
    }
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {           

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }
    
    private boolean setVariables() {
        String tmpStr;
        
        tmpStr = textNOctaves.getText();

        if (testParameter(tmpStr, 1.0, 12.0)) {
            O = Integer.valueOf(tmpStr).intValue();
        } else {
            textNOctaves.requestFocus();
            textNOctaves.selectAll();

            return false;
        }
        
        tmpStr = textNLevels.getText();

        if (testParameter(tmpStr, 1.0, 12.0)) {
            S = Integer.valueOf(tmpStr).intValue();
        } else {
            textNLevels.requestFocus();
            textNLevels.selectAll();

            return false;
        }
        
        tmpStr = textFirstOctave.getText();

        if (testParameter(tmpStr, 0.0, O-1)) {
            omin = Integer.valueOf(tmpStr).intValue();
        } else {
            textFirstOctave.requestFocus();
            textFirstOctave.selectAll();

            return false;
        }
        
        tmpStr = textEdgeThresh.getText();

        if (testParameter(tmpStr, 1.0, 1000.0)) {
            edge_thresh = Double.valueOf(tmpStr).doubleValue();
        } else {
            textEdgeThresh.requestFocus();
            textEdgeThresh.selectAll();

            return false;
        }
        
        tmpStr = textPeakThresh.getText();

        if (testParameter(tmpStr, 0.0, 1000.0)) {
            peak_thresh = Double.valueOf(tmpStr).doubleValue();
        } else {
            textPeakThresh.requestFocus();
            textPeakThresh.selectAll();

            return false;
        }
        
        tmpStr = textMagnification.getText();

        if (testParameter(tmpStr, 1.0, 1000.0)) {
            magnif = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMagnification.requestFocus();
            textMagnification.selectAll();

            return false;
        }
        
        force_orientations = orientationsCheckBox.isSelected();
        
        verbose = verboseCheckBox.isSelected();
        
        writeFrames = writeFramesCheckBox.isSelected();
        
        readFrames = readFramesCheckBox.isSelected();
        
        writeDescriptor = writeDescriptorCheckBox.isSelected();
        
        writeMeta = writeMetaCheckBox.isSelected();
        
        writeGss = writeGssCheckBox.isSelected();
        
        return true;
    }
    
    protected void setGUIFromParams() {
    	int i;
    	int numInputImages = scriptParameters.getParams().getInt("number_of_input_images");	
    	fileDir = new String[numInputImages];
    	fileName = new String[numInputImages];
    	for (i = 0; i < numInputImages; i++) {
    		 fileDir[i] = scriptParameters.getParams().getString("file_dir"+String.valueOf(i));
    		 fileName[i] = scriptParameters.getParams().getString("file_name"+String.valueOf(i));
    	}
    	verbose = scriptParameters.getParams().getBoolean("verbosity");
    	O = scriptParameters.getParams().getInt("octaves");
    	S = scriptParameters.getParams().getInt("levels");
    	omin = scriptParameters.getParams().getInt("first_octave");
    	edge_thresh = scriptParameters.getParams().getDouble("edge");
    	peak_thresh = scriptParameters.getParams().getDouble("peak");
    	magnif = scriptParameters.getParams().getDouble("magnification");
    	force_orientations = scriptParameters.getParams().getBoolean("orientations");
    	writeFrames = scriptParameters.getParams().getBoolean("write_frames");
    	readFrames = scriptParameters.getParams().getBoolean("read_frames");
    	writeDescriptor = scriptParameters.getParams().getBoolean("write_descriptor");
    	writeMeta = scriptParameters.getParams().getBoolean("write_meta");
    	writeGss = scriptParameters.getParams().getBoolean("write_gss");
    }
    
    protected void storeParamsFromGUI() throws ParserException {
    	int i;
    	scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_input_images", fileDir.length));	
    	
    	for (i = 0; i < fileDir.length; i++) {
    		scriptParameters.getParams().put(ParameterFactory.newParameter("file_dir"+String.valueOf(i), fileDir[i]));	
    		scriptParameters.getParams().put(ParameterFactory.newParameter("file_name"+String.valueOf(i), fileName[i]));	
    	}
    	scriptParameters.getParams().put(ParameterFactory.newParameter("verbosity", verbose));	
    	scriptParameters.getParams().put(ParameterFactory.newParameter("octaves", O));	
    	scriptParameters.getParams().put(ParameterFactory.newParameter("levels", S));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("first_octave", omin));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("edge", edge_thresh));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("peak", peak_thresh));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("magnification", magnif));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("orientations", force_orientations));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("write_frames", writeFrames));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("read_frames", readFrames));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("write_descriptor", writeDescriptor));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("write_meta", writeMeta));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("write_gss", writeGss));
    }

}