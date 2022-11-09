package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.DSC_MRI_toolbox;
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileDicomTagTable;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

public class JDialogDSC_MRI_toolbox extends JDialogScriptableBase {
	//~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private DSC_MRI_toolbox dsc;

    /** Source image. */
    private ModelImage image;
    
    private JTextField outputFileDirectoryText;

	private String outputFileDirectory;
	
	private JLabel teLabel;
	
	private JTextField teText;
	
	private double te;
	
	private String TEString = "0.025";
	
	private JLabel trLabel;
	
	private JTextField trText;
	
	private double tr;
	
	private String TRString = "1.55";
	
	private JLabel aif_nSliceLabel;
	
	private JTextField aif_nSliceText;
	
	private int aif_nSlice;
	
	private JLabel svdLabel;
	
	private JCheckBox svdCheckBox;
	
	private JCheckBox csvdCheckBox;
	
	private JCheckBox osvdCheckBox;
	
	private String[] deconv_method;
	
	private ViewUserInterface userInterface;
	
	private int nS;
	
	private int svdMethodsSelected = 0;
	
	private boolean do_svd = false;
	
	private boolean do_csvd = true;
	
	private boolean do_osvd = false;
	
	 /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogDSC_MRI_toolbox() { }

    /**
     * Construct the bilateral filter dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDSC_MRI_toolbox(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        nS = image.getExtents()[2];
        userInterface = ViewUserInterface.getReference();
        init();
        setVisible(true);
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	if (image.getFileInfo()[0] instanceof FileInfoDicom) {
    		FileInfoDicom dicomInfo = (FileInfoDicom) image.getFileInfo(0);
        	FileDicomTagTable tagTable = dicomInfo.getTagTable();
        	if (tagTable.getValue("0018,0081") != null) {
            	// Echo time in milliseconds
                FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0081"));
                String TEInMillisecondsString = (String)tag.getValue(false);
                te = Double.valueOf(TEInMillisecondsString).doubleValue()/1000.0;
                TEString = String.valueOf(te);
            }
        	
        	if (tagTable.getValue("0018,0080") != null) {
            	// Repetition time in milliseconds
            	FileDicomTag tag = tagTable.get(new FileDicomKey("0018,0080"));
            	String TRInMillisecondsString = (String)tag.getValue(false);
            	tr = Double.valueOf(TRInMillisecondsString).doubleValue()/1000.0;
            	TRString = String.valueOf(tr);
            }
    	} // if (image.getFileInfo()[0] instanceof FileInfoDicom)
        setForeground(Color.black);
        setTitle("DSC_MRI_toolbox");
        
        final GuiBuilder gui = new GuiBuilder(this);

        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));
        
        outputFileDirectoryText = gui.buildFileField("Directory containing output files: ", image.getFileInfo(0).getFileDirectory(),
        		false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(outputFileDirectoryText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        teLabel = new JLabel("Echo time te in seconds");
        teLabel.setFont(serif12);
        teLabel.setForeground(Color.black);
        inputPanel.add(teLabel, gbc);
        
        gbc.gridx = 1;
        teText = new JTextField(10);
        teText.setText(TEString);
        teText.setFont(serif12);
        teText.setForeground(Color.black);
        inputPanel.add(teText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        trLabel = new JLabel("Repetition time tr in seconds");
        trLabel.setFont(serif12);
        trLabel.setForeground(Color.black);
        inputPanel.add(trLabel, gbc);
        
        gbc.gridx = 1;
        trText = new JTextField(10);
        trText.setText(TRString);
        trText.setFont(serif12);
        trText.setForeground(Color.black);
        inputPanel.add(trText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        aif_nSliceLabel = new JLabel("Slice (0 - "+String.valueOf(nS-1)+") for AIF search");
        aif_nSliceLabel.setFont(serif12);
        aif_nSliceLabel.setForeground(Color.black);
        inputPanel.add(aif_nSliceLabel, gbc);
        
        gbc.gridx = 1;
        aif_nSliceText = new JTextField(10);
        aif_nSliceText.setText("6");
        aif_nSliceText.setFont(serif12);
        aif_nSliceText.setForeground(Color.black);
        inputPanel.add(aif_nSliceText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        svdLabel = new JLabel("Select 1,2 or 3 convolution methods:");
        svdLabel.setFont(serif12);
        svdLabel.setForeground(Color.black);
        inputPanel.add(svdLabel, gbc);
        
        gbc.gridy++;
        svdCheckBox = new JCheckBox("SVD", false);
        svdCheckBox.setFont(MipavUtil.font12);
        svdCheckBox.setForeground(Color.black);
        inputPanel.add(svdCheckBox, gbc);
        
        gbc.gridy++;
        csvdCheckBox = new JCheckBox("Block-circulant SVD", true);
        csvdCheckBox.setFont(MipavUtil.font12);
        csvdCheckBox.setForeground(Color.black);
        inputPanel.add(csvdCheckBox, gbc);
        
        gbc.gridy++;
        osvdCheckBox = new JCheckBox("Block-circulant SVD with oscillation index", false);
        osvdCheckBox.setFont(MipavUtil.font12);
        osvdCheckBox.setForeground(Color.black);
        inputPanel.add(osvdCheckBox, gbc);
        
        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(OKCancelPanel, BorderLayout.SOUTH);
        
        JScrollPane scrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        getContentPane().add(scrollPane);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	outputFileDirectory = outputFileDirectoryText.getText();
    	
    	tmpStr = teText.getText();
    	try {
    		te = Double.valueOf(tmpStr).doubleValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("te text does not have a proper double");
    	    teText.requestFocus();
    	    teText.selectAll();
    	    return false;
    	}
    	if (te <= 0) {
    		MipavUtil.displayError("te must be > 0");
    		teText.requestFocus();
    	    teText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = trText.getText();
    	try {
    		tr = Double.valueOf(tmpStr).doubleValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("tr text does not have a proper double");
    	    trText.requestFocus();
    	    trText.selectAll();
    	    return false;
    	}
    	if (tr <= 0) {
    		MipavUtil.displayError("tr must be > 0");
    		trText.requestFocus();
    	    trText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = aif_nSliceText.getText();
    	try {
    		aif_nSlice = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("AIF slice text does not have a proper integer");
    	    aif_nSliceText.requestFocus();
    	    aif_nSliceText.selectAll();
    	    return false;
    	}
    	if (aif_nSlice < 0) {
    		MipavUtil.displayError("aif_nSlice must be >= 0");
    		aif_nSliceText.requestFocus();
    	    aif_nSliceText.selectAll();
    	    return false;
    	}
    	if (aif_nSlice >= nS) {
    		MipavUtil.displayError("aif_nSlice must be < " + String.valueOf(nS));
    		aif_nSliceText.requestFocus();
    	    aif_nSliceText.selectAll();
    	    return false;
    	}
    	
    	svdMethodsSelected = 0;
    	if (svdCheckBox.isSelected()) {
    		do_svd = true;
    		svdMethodsSelected++;
    	}
    	else {
    		do_svd = false;
    	}
    	if (csvdCheckBox.isSelected()) {
    		do_csvd = true;
    		svdMethodsSelected++;
    	}
    	else {
    		do_csvd = false;
    	}
    	if (osvdCheckBox.isSelected()) {
    		do_osvd = true;
    		svdMethodsSelected++;
    	}
    	else {
    		do_osvd = false;
    	}
    	
    	if (svdMethodsSelected == 0) {
    		MipavUtil.displayError("A SVD method must be selected");
    		return false;
    	}
    	
    	return true;
    }
    
    protected void callAlgorithm() {
    	deconv_method = new String[svdMethodsSelected];
    	int index = 0;
    	if (do_svd) {
    		deconv_method[index++] = "SVD";
    	}
    	if (do_csvd) {
    		deconv_method[index++] = "cSVD";
    	}
    	if (do_osvd) {
    		deconv_method[index++] = "oSVD";
    	}
    	dsc = new DSC_MRI_toolbox(image, outputFileDirectory, te, tr, aif_nSlice, deconv_method);
    	
    	setVisible(false); // Hide dialog

        dsc.runAlgorithm();
    }
    
    protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();
    	outputFileDirectory = scriptParameters.getParams().getString("outputFileDirectory");
    	te = scriptParameters.getParams().getDouble("TE");
    	tr = scriptParameters.getParams().getDouble("TR");
    	aif_nSlice = scriptParameters.getParams().getInt("AIF_NSLICE");
    	do_svd = scriptParameters.getParams().getBoolean("svd");
    	do_csvd = scriptParameters.getParams().getBoolean("csvd");
    	do_osvd = scriptParameters.getParams().getBoolean("osvd");
    }
    
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);
    	scriptParameters.getParams().put(ParameterFactory.newParameter("outputFileDirectory", outputFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TE", te));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TR", tr));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("AIF_NSLICE", aif_nSlice));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("svd", do_svd));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("csvd", do_csvd));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("osvd", do_osvd));
    }
}