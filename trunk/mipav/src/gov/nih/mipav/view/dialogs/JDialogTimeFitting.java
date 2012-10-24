package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import java.util.Enumeration;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class JDialogTimeFitting extends JDialogScriptableBase implements AlgorithmInterface {
	
	private static final int LINEAR_FIT = 0;
	
	private static final int EXPONENTIAL_FIT = 1;
	
	private static final int GAUSSIAN_FIT = 2;
	
	private static final int LAPLACE_FIT = 3;
	
	private static final int LORENTZ_FIT = 4;
	
	private static final int MULTIEXPONENTIAL_FIT = 5;
	
	private static final int RAYLEIGH_FIT = 6;
	
	private int functionFit = LINEAR_FIT;
	
	/** source image. **/
    private ModelImage image;
    
    /** result image **/
    private ModelImage resultImage;
      
    /** boolean isMultifile **/
    private boolean isMultifile;
    
    private int nDims;
    
    private JTextField textImage;
    
    private JButton buttonImage;
    
    private ButtonGroup functionGroup;
    
    private JRadioButton linearFit;
    
    private JRadioButton exponentialFit;
    
    private JRadioButton gaussianFit;
    
    private JRadioButton laplaceFit;
    
    private JRadioButton lorentzFit;
    
    private JRadioButton multiExponentialFit;
    
    private JLabel numVariablesLabel;
    
    private JTextField numVariablesField;
    
    private int numVariables = 5;
    
    private JRadioButton rayleighFit;
    
    private JCheckBox logCheckBox;
    
    private boolean useLog = false;

	private JComboBox imageList;
	
	
	public JDialogTimeFitting() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}
	
	
	
	/**
	 *  action performed
	 */
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		String command = event.getActionCommand();
		 if (command.equals("OK")) {
			 if (setVariables()) {
			     callAlgorithm();
			 }
	     } 
		 else if (command.equals("Cancel")) {
	    	 if (image != null) {
	    		 image.disposeLocal();
	    		 image = null;
	    	 }
	         dispose();
	     } else if (command.equals("Help")) {
	            //MipavUtil.showHelp("");
	     } else if ((source == linearFit) || (source == exponentialFit) || (source == gaussianFit) ||
	                (source == laplaceFit) || (source == lorentzFit) || (source == multiExponentialFit) ||
	                (source == rayleighFit)) {
	    	if (multiExponentialFit.isSelected()) {
	    	    numVariablesLabel.setEnabled(true);
	    	    numVariablesField.setEnabled(true);
	    	}
	    	else {
	    	    numVariablesLabel.setEnabled(false);
                numVariablesField.setEnabled(false);    
	    	}
	     
	    } else {
            super.actionPerformed(event);
        }
	     

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int tDim;
		int volSize;
		int size4D;
		double destArray[][];
		int destExitStatusArray[];
		int i;
		int j;
		
	    xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        zDim = image.getExtents()[2];
        tDim = image.getExtents()[3];
        volSize = xDim * yDim * zDim;
        size4D = volSize * tDim;
        destArray = new double[4][volSize];
        destExitStatusArray = new int[volSize];
        for (i = 0; i < 4; i++) {
            for (j = 0; j < volSize; j++) {
                destArray[i][j] = Float.NaN;
            }
        }
		
		
	}
	
	private void loadImage() {
		ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
         JFileChooser chooser = fileChooser.getFileChooser();
         if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }
         chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
         chooser.setDialogTitle("Choose image");
         int returnValue = chooser.showOpenDialog(this);
         if (returnValue == JFileChooser.APPROVE_OPTION) { 	
         	FileIO fileIO = new FileIO();
         	isMultifile = fileChooser.isMulti();
         	image = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator,
         			                 isMultifile, null);
    		if (image.isComplexImage()) {
    			MipavUtil.displayError("Image cannot be a complex image");
    		    image.disposeLocal();
    		    image = null;
    		    return;	
    		}
    		if (image.isColorImage()) {
                MipavUtil.displayError("Image cannot be a color image");
                image.disposeLocal();
                image = null;
                return; 
            }
    		if (image.getNDims() != 4) {
    		    MipavUtil.displayError("Image dimensions is " + image.getNDims() + " instead of the required 4");
                image.disposeLocal();
                image = null;
                return;     
    		}
    		imageList.addItem(image.getImageName());
    		imageList.setSelectedItem(image.getImageName());
    		textImage.setText(image.getImageName());
         } 
     } 
	
	
	/**
	 *  algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
		dispose();
	}
	
	
	
	/**
	 * init
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

    	GuiBuilder gui = new GuiBuilder(this);
    	
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
    	setForeground(Color.black);
        setTitle("Time Fitting");
        
        JLabel choiceLabel = new JLabel("Choose a 4D black and white image file");
        choiceLabel.setForeground(Color.black);
        choiceLabel.setFont(serif12);
        mainPanel.add(choiceLabel, gbc);
        
        JLabel choiceLabel2 = new JLabel("At each volume point a function will be fitted to the time series");
        choiceLabel2.setForeground(Color.black);
        choiceLabel2.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel2, gbc);
        
        JLabel choiceLabel3 = new JLabel("a0 fitting is in the first output volume");
        choiceLabel3.setForeground(Color.black);
        choiceLabel3.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel3, gbc);
        
        JLabel choiceLabel4 = new JLabel("a1 fitting is in the second output volume and so on");
        choiceLabel4.setForeground(Color.black);
        choiceLabel4.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel4, gbc);
        
        buttonImage = new JButton("Choose an image");
        buttonImage.setForeground(Color.black);
        buttonImage.setFont(serif12B);
        buttonImage.addActionListener(this);
        buttonImage.setActionCommand("AddImageBrowse");
        buttonImage.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        
        Object[] imgList = new Object[ViewUserInterface.getReference().getRegisteredImagesNum()+1];
        imgList[0] = "Load image...";
        Enumeration<String> strEnum = ViewUserInterface.getReference().getRegisteredImageNames();
        for(int i=1; i<imgList.length; i++) {
        	imgList[i] = strEnum.nextElement();
        }
        imageList = gui.buildComboBox("Choose an image: ", imgList, 0);
        imageList.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
	            if(imageList.getSelectedIndex() == 0) {
	            	loadImage();
	            } else {
	            	textImage.setText(imageList.getSelectedItem().toString());
	            }
            }
        });
        mainPanel.add(imageList.getParent(), gbc);

        textImage = new JTextField();
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textImage, gbc);
        
        logCheckBox = new JCheckBox("Fit the log of intensity values", false);
        logCheckBox.setFont(serif12);
        logCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(logCheckBox, gbc);
        
        JLabel functionLabel = new JLabel("Choose a fitting function");
        functionLabel.setForeground(Color.black);
        functionLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(functionLabel, gbc);
        
        functionGroup = new ButtonGroup();
        linearFit = new JRadioButton("Fit linear (a0 + a1 * t)", true);
        linearFit.setFont(serif12);
        linearFit.setForeground(Color.black);
        linearFit.addActionListener(this);
        functionGroup.add(linearFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(linearFit, gbc);
        
        exponentialFit = new JRadioButton("Fit exponential (a0+a1*exp(a2*t))", false);
        exponentialFit.setFont(serif12);
        exponentialFit.setForeground(Color.black);
        exponentialFit.addActionListener(this);
        functionGroup.add(exponentialFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(exponentialFit, gbc);
        
        gaussianFit = new JRadioButton("Fit Gaussian (a0*exp(-(t-a1)^2/(2*(a2)^2)))", false);
        gaussianFit.setFont(serif12);
        gaussianFit.setForeground(Color.black);
        gaussianFit.addActionListener(this);
        functionGroup.add(gaussianFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(gaussianFit, gbc);
        
        laplaceFit = new JRadioButton("Fit Laplace (a0*exp(-|x-a1|/a2))", false);
        laplaceFit.setFont(serif12);
        laplaceFit.setForeground(Color.black);
        laplaceFit.addActionListener(this);
        functionGroup.add(laplaceFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(laplaceFit, gbc);
        
        lorentzFit = new JRadioButton("Fit Lorentz ((a0/PI) * a2)/((x-a1)*(x-a1) + a2*a2)", false);
        lorentzFit.setFont(serif12);
        lorentzFit.setForeground(Color.black);
        lorentzFit.addActionListener(this);
        functionGroup.add(lorentzFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(lorentzFit, gbc);
        
        multiExponentialFit = new JRadioButton("Fit Multiexponential a0 + sum of a[2*k+1]*exp(a[2*k+2]*x)", false);
        multiExponentialFit.setFont(serif12);
        multiExponentialFit.setForeground(Color.black);
        multiExponentialFit.addActionListener(this);
        functionGroup.add(multiExponentialFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(multiExponentialFit, gbc);
        
        numVariablesLabel = new JLabel("Number of variables (Only for Multiexponential)");
        numVariablesLabel.setFont(serif12);
        numVariablesLabel.setForeground(Color.black);
        numVariablesLabel.setEnabled(false);
        gbc.gridx = 0;
        mainPanel.add(numVariablesLabel, gbc);
        
        numVariablesField = new JTextField(5);
        numVariablesField.setText("5");
        numVariablesField.setFont(serif12);
        numVariablesField.setForeground(Color.black);
        numVariablesField.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy++;
        mainPanel.add(numVariablesField, gbc);
        
        rayleighFit = new JRadioButton("Fit Rayleigh Distribution a2 * (2/a1)*(x-a0)*exp(-(x-a0)*(x-a0)/a1)*u(x-a0)", false);
        rayleighFit.setFont(serif12);
        rayleighFit.setForeground(Color.black);
        rayleighFit.addActionListener(this);
        functionGroup.add(rayleighFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(rayleighFit, gbc);
    
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        
    }
    
    private boolean setVariables() {
		
    	image = ViewUserInterface.getReference().getRegisteredImageByName(imageList.getSelectedItem().toString());
    	
    	if(image == null) {
    		MipavUtil.displayError("No image with name "+imageList.getSelectedItem()+" was found.");
    		return false;
    	}
    	
    	nDims = image.getNDims();
    	if (nDims != 4) {
    	    MipavUtil.displayError("Image has " + nDims + " dimensions instead of the required 4");
    	    return false;
    	}
    	
    	if ((image.isColorImage()) || (image.isComplexImage())) {
    	    MipavUtil.displayError("Image must be a black and white image");
    	    return false;
    	}
        
        
    	useLog = logCheckBox.isSelected();
    	
        if (linearFit.isSelected()) {
            functionFit = LINEAR_FIT;
        }
        else if (exponentialFit.isSelected()) {
            functionFit = EXPONENTIAL_FIT;
        }
        else if (gaussianFit.isSelected()) {
            functionFit = GAUSSIAN_FIT;
        }
        else if (laplaceFit.isSelected()) {
            functionFit = LAPLACE_FIT;
        }
        else if (lorentzFit.isSelected()) {
            functionFit = LORENTZ_FIT;
        }
        else if (multiExponentialFit.isSelected()) {
            functionFit = MULTIEXPONENTIAL_FIT;
            String tmpStr = numVariablesField.getText();
            numVariables = Integer.valueOf(tmpStr).intValue();
            if ((numVariables % 2) == 0) {
                MipavUtil.displayError("Number of variables must be odd");
                return false;
            }
        }
        else if (rayleighFit.isSelected()) {
            functionFit = RAYLEIGH_FIT;
        }
    	
    	return true;
    }
    
    
    
    
	

	

	/**
	 * set GUI from params
	 */
	protected void setGUIFromParams(){
		
	}

	/**
	 * store params from gui
	 */
	protected void storeParamsFromGUI() throws ParserException {
		
	}
	
	 /**
     * get result image
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    
	
	/**
     * item staate changed
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        
    }



    
    /**
     *  windoe closing
     */
    public void windowClosing(WindowEvent event) {
        if (image != null) {
        	image.disposeLocal();
        	image = null;
        }
        dispose();
    }

	


	
}
