import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.border.LineBorder;


public class PlugInDialogCellFiring extends JDialogBase implements AlgorithmInterface, ActionListener {

    private JPanel mainPanel;

    private GridBagConstraints gbc;

    private JLabel fileLabel;
    
    private JLabel imageLabel;
    
    private JComboBox comboBoxImage;
    
    private ModelImage image = null;

    private JTextField filePathTextField;

    private JButton fileBrowseButton;

    private JTextArea outputTextArea;

    private JScrollPane scrollPane;

    private String fileName;

    String directory;

    private String currDir;

    private PlugInAlgorithmCellFiring alg;
    
    private boolean alreadyDisplayed = true;
    
    private JCheckBox displayInputCheckBox;
    
    private boolean displayInputImage;
    
    private JTextField downSampleXYText;
    
    private float downSampleXY = 1.0f;
    
    private JTextField downSampleZText;
    
    private float downSampleZ = 1.0f;
    
    private JCheckBox displayDownSampleCheckBox;
    
    private boolean displayDownSampleImage;
    
    private JCheckBox saveDownSampleCheckBox;
    
    private boolean saveDownSampleImage;
    
    private JCheckBox cropImageCheckBox;
    
    private boolean cropImage;
    
    private JCheckBox registrationCheckBox;
    
    private boolean registerImage;
    
    private JTextField earliestSlicesText;
    
    private int earliestSlices;
    
    private JCheckBox anistropicCheckBox;
    
    private boolean anistropicDiffusion;

    public PlugInDialogCellFiring() {

    }

    public PlugInDialogCellFiring(final boolean modal) {
        super(modal);
        init();
    }

    private void init() {
        setForeground(Color.black);
        setTitle("Cell Firing v1.0");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        fileLabel = new JLabel("File ");
        filePathTextField = new JTextField(35);
        filePathTextField.setEditable(false);
        filePathTextField.setBackground(Color.white);
        fileBrowseButton = new JButton("Browse");
        fileBrowseButton.addActionListener(this);
        fileBrowseButton.setActionCommand("FileBrowse");
        
        imageLabel = new JLabel("Image ");
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        
        buildComboBoxImage();
        Object selected = comboBoxImage.getSelectedItem();
        if(selected != null) {
            comboBoxImage.setSelectedItem(selected);
            String selectedName = (String) comboBoxImage.getSelectedItem();
            image = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
        }
        comboBoxImage.addActionListener(this);
        comboBoxImage.setActionCommand("ImageBrowse");
        
        displayInputCheckBox = new JCheckBox("Display input image", false);
        displayInputCheckBox.setFont(serif12);
        displayInputCheckBox.setEnabled(false);
        displayInputCheckBox.setForeground(Color.black);
        
        JLabel downSampleXYLabel = new JLabel("Down sampling in (X/Y) <= 1.0");
        downSampleXYText = new JTextField(15);
        downSampleXYText.setText("1.0");
        downSampleXYText.setFont(serif12);
        downSampleXYText.setForeground(Color.BLACK);
        
        JLabel downSampleZLabel = new JLabel("Down sampling in Z <= 1.0");
        downSampleZText = new JTextField(15);
        downSampleZText.setText("1.0");
        downSampleZText.setFont(serif12);
        downSampleZText.setForeground(Color.BLACK);
        
        displayDownSampleCheckBox = new JCheckBox("Display down sampled image", false);
        displayDownSampleCheckBox.setFont(serif12);
        displayDownSampleCheckBox.setForeground(Color.black);
        
        saveDownSampleCheckBox = new JCheckBox("Save down sampled image", false);
        saveDownSampleCheckBox.setFont(serif12);
        saveDownSampleCheckBox.setForeground(Color.black);
        
        cropImageCheckBox = new JCheckBox("Crop image with rectangular VOI", false);
        cropImageCheckBox.setFont(serif12);
        cropImageCheckBox.setForeground(Color.black);
        
        registrationCheckBox = new JCheckBox("2.5D OAR registration", true);
        registrationCheckBox.setFont(serif12);
        registrationCheckBox.setForeground(Color.black);
        
        JLabel earliestSlicesLabel = new JLabel("Earliest slices for subtraction average");
        earliestSlicesText = new JTextField(15);
        earliestSlicesText.setText("10");
        earliestSlicesText.setFont(serif12);
        earliestSlicesText.setForeground(Color.BLACK);
        
        anistropicCheckBox = new JCheckBox("Anisotropic diffusion", true);
        anistropicCheckBox.setFont(serif12);
        anistropicCheckBox.setForeground(Color.black);

        outputTextArea = new JTextArea();
        outputTextArea.setRows(15);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(fileLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(filePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(fileBrowseButton, gbc);
        
        gbc.gridx = 0;
        gbc.weightx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(imageLabel, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(comboBoxImage, gbc);

        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(displayInputCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(downSampleXYLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(downSampleXYText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(downSampleZLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(downSampleZText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(displayDownSampleCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 6;
        mainPanel.add(saveDownSampleCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 7;
        mainPanel.add(cropImageCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(registrationCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 9;
        mainPanel.add(earliestSlicesLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(earliestSlicesText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 10;
        mainPanel.add(anistropicCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 3;
        mainPanel.add(scrollPane, gbc);

        final JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setMinimumSize(getSize());

        setVisible(true);
        setResizable(false);

    }
    
    /**
     * Builds a list of images to operate on from the template image.
     */
    private void buildComboBoxImage() {
        ViewUserInterface UI;

        comboBoxImage.removeAllItems();

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            comboBoxImage.addItem(name);
        }
    }

    private void callAlgorithm() {
        
        
        alg = new PlugInAlgorithmCellFiring(image, alreadyDisplayed, displayInputImage, downSampleXY, 
        		downSampleZ, displayDownSampleImage, saveDownSampleImage, cropImage, registerImage, 
        		earliestSlices, anistropicDiffusion, outputTextArea);

        alg.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still
            // have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            alg.run();
        }
    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (alg.isCompleted()) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            OKButton.setEnabled(false);
            cancelButton.setText("Close");

            outputTextArea.append("Finished" + "\n");

        }

    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        if (command.equalsIgnoreCase("FileBrowse")) {
            final JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
            /*
             * if (currDir != null) { chooser.setCurrentDirectory(new File(currDir)); }
             */
            chooser.setDialogTitle("Choose File");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                Preferences.setImageDirectory(new File(currDir));
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory() + File.separator;
                filePathTextField.setText(currDir);
                if (image != null) {
                	image.disposeLocal();
                	image = null;
                }
                final FileIO fileIO = new FileIO();
                final boolean multiFile = false;
                image = fileIO.readImage(fileName, directory, multiFile, null);
                alreadyDisplayed = false;
                displayInputCheckBox.setEnabled(true);
            }
        } else if (command.equals("ImageBrowse")) {
        	 Object selected = comboBoxImage.getSelectedItem();
        	 if(selected != null) {
                 comboBoxImage.setSelectedItem(selected);
                 String selectedName = (String) comboBoxImage.getSelectedItem();
                 if (image != null) {
                	 image.disposeLocal();
                	 image = null;
                 }
                 image = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
                 alreadyDisplayed = true;
                 displayInputCheckBox.setEnabled(false);
                 displayInputCheckBox.setSelected(false);
             }
        } else if (command.equalsIgnoreCase("ok")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equalsIgnoreCase("cancel")) {

            dispose();
        } else {
            super.actionPerformed(e);
        }

    }

    private boolean setVariables() {
    	String tmpStr;

        if ((image == null) && (filePathTextField.getText().trim().equals(""))) {
            MipavUtil.displayError("File is required");
            return false;

        }
        
        displayInputImage = displayInputCheckBox.isSelected();
        
        tmpStr = downSampleXYText.getText();
        if (testParameter(tmpStr, 0.001, 1.0)) {
            downSampleXY = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("downSampleXY must be between 0.001 and 1.0");
            downSampleXYText.requestFocus();
            downSampleXYText.selectAll();

            return false;
        }
        
        tmpStr = downSampleZText.getText();
        if (testParameter(tmpStr, 0.001, 1.0)) {
            downSampleZ = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("downSampleZ must be between 0.001 and 1.0");
            downSampleZText.requestFocus();
            downSampleZText.selectAll();

            return false;
        }
        
        displayDownSampleImage = displayDownSampleCheckBox.isSelected();
        
        saveDownSampleImage = saveDownSampleCheckBox.isSelected();
        
        cropImage = cropImageCheckBox.isSelected();
        if (cropImage) {
        	VOIVector VOIs = image.getVOIs();
        	if (VOIs == null) {
        		MipavUtil.displayError("VOI vector is null");
        		return false;
        	}
        	if (VOIs.size() == 0) {
        		MipavUtil.displayError("VOI vector has size zero");
        		return false;
        	}
        	VOI rectVOI = VOIs.get(0);
        	if (rectVOI == null) {
        		MipavUtil.displayError("No VOI is present");
        		return false;
        	}
        }
        
        registerImage = registrationCheckBox.isSelected();
        
        int zDim = image.getExtents()[2];
        int newZDim = Math.round(downSampleZ * zDim);
        tmpStr = earliestSlicesText.getText();
        if (testParameter(tmpStr, 1, newZDim)) {
            earliestSlices = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("earliestSlices must be between 1 and " + newZDim);
            earliestSlicesText.requestFocus();
            earliestSlicesText.selectAll();

            return false;
        }
        
        anistropicDiffusion = anistropicCheckBox.isSelected();

        return true;
    }

}
