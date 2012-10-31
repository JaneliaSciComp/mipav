package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSM2;
import gov.nih.mipav.model.algorithms.AlgorithmTimeFitting;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
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
import java.io.IOException;

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
    
    private ModelImage exitStatusImage;
      
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
	
	private JLabel labelVOI;
	
	private JButton buttonVOIFile;
	
	private JTextField textVOIFile;
	
	private ViewUserInterface UI = ViewUserInterface.getReference();
	
	private String directoryVOI;
	
	private String fileNameVOI;
	
	private FileVOI fileVOI;
    private VOI[] voi;
    
    private AlgorithmTimeFitting tfAlgo;
    
    private JCheckBox initialCheckBox;
    
    private boolean previousFindInitial = true;
    
    private boolean findInitialFromData = true;
    
    private JLabel labela0;
    
    private JLabel labela1;
    
    private JLabel labela2;
    
    private JLabel labela3;
    
    private JLabel labela4;
    
    private JLabel labela5;
    
    private JLabel labela6;
    
    private JTextField texta0;
    
    private JTextField texta1;
    
    private JTextField texta2;
    
    private JTextField texta3;
    
    private JTextField texta4;
    
    private JTextField texta5;
    
    private JTextField texta6;
    
    private double initial[] = new double[9];
	
	
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
	    	    previousFindInitial = initialCheckBox.isSelected();
	    	    initialCheckBox.setEnabled(false);
	    	    initialCheckBox.setSelected(false);
	    	    numVariablesLabel.setEnabled(true);
	    	    numVariablesField.setEnabled(true);
	    	    labela0.setEnabled(true);
	    	    texta0.setEnabled(true);
	    	    labela1.setEnabled(true);
	    	    texta1.setEnabled(true);
	    	    labela2.setEnabled(true);
	    	    texta2.setEnabled(true);
	    	    labela3.setEnabled(true);
	    	    texta3.setEnabled(true);
	    	    labela4.setEnabled(true);
	    	    texta4.setEnabled(true);
	    	    labela5.setEnabled(true);
                texta5.setEnabled(true);
                labela6.setEnabled(true);
                texta6.setEnabled(true);
                return;
	    	}
    	    initialCheckBox.setEnabled(true);
    	    initialCheckBox.setSelected(previousFindInitial);
    	    numVariablesLabel.setEnabled(false);
            numVariablesField.setEnabled(false);
            labela3.setEnabled(false);
            texta3.setEnabled(false);
            labela4.setEnabled(false);
            texta4.setEnabled(false);
            labela5.setEnabled(false);
            texta5.setEnabled(false);
            labela6.setEnabled(false);
            texta6.setEnabled(false);
	    	if (source == linearFit) {
	    	    labela2.setEnabled(false);
	    	    texta2.setEnabled(false);
	    	}
	    	else {
	    	    labela2.setEnabled(!previousFindInitial);
                texta2.setEnabled(!previousFindInitial);
	    	}
	    	labela0.setEnabled(!previousFindInitial);
	    	texta0.setEnabled(!previousFindInitial);
	    	labela1.setEnabled(!previousFindInitial);
            texta1.setEnabled(!previousFindInitial);
	     } else if (source == initialCheckBox) {
	         if (!initialCheckBox.isSelected()) {
	             labela0.setEnabled(true);
	             texta0.setEnabled(true);
	             labela1.setEnabled(true);
	             texta1.setEnabled(true);
	             if (!linearFit.isSelected()) {
	                 labela2.setEnabled(true);
	                 texta2.setEnabled(true);
	             }
	         }
	         else {
	             labela0.setEnabled(false);
                 texta0.setEnabled(false);
                 labela1.setEnabled(false);
                 texta1.setEnabled(false);
                 labela2.setEnabled(false);
                 texta2.setEnabled(false);
	         }
	     } else if (command.equals("VOIFile")) {

	            try {
	                JFileChooser chooser = new JFileChooser();

	                if (UI.getDefaultDirectory() != null) {
	                    File file = new File(UI.getDefaultDirectory());

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

	                chooser.setDialogTitle("Open VOI file");
	                directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

	                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

	                if (returnValue == JFileChooser.APPROVE_OPTION) {
	                    fileNameVOI = chooser.getSelectedFile().getName();
	                    directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
	                    UI.setDefaultDirectory(directoryVOI);
	                } else {
	                    fileNameVOI = null;

	                    return;
	                }

	                if (fileNameVOI != null) {

	                    try {
	                        fileVOI = new FileVOI(fileNameVOI, directoryVOI, image);
	                    }
	                    catch (IOException e) {
	                        MipavUtil.displayError("IOException on new FileVOI(fileNameVOI, directoryVOI, image)");
	                        return;
	                    }

	                    try {
	                        voi = fileVOI.readVOI(false);
	                    }
	                    catch (IOException e) {
	                        MipavUtil.displayError("IOException on fileVOI.readVOI(false)");
	                        return;
	                    }
	                    
	                    if (voi.length > 1) {
	                        MipavUtil.displayError("Found " + voi.length + " vois in file instead of 1");
	                        return;
	                    }
	                    
	                    textVOIFile.setText(fileNameVOI);
	                    voi[0].setColor(0.0f);
	                    image.registerVOI(voi[0]);

	                    //  when everything's done, notify the image listeners
	                    image.notifyImageDisplayListeners();   
	                }

	                
	            } catch (OutOfMemoryError e) {
	                MipavUtil.displayError("Out of memory in JDialogTimeFitting.");

	                return;
	            }
	        } else {
            super.actionPerformed(event);
        }
	     

	}
	
	/**
	 *  call algorithm
	 */
	protected void callAlgorithm() {
		int i;
		
		try {
		
    		int resultExtents[] = new int[4];
            for (i = 0; i < 3; i++) {
                resultExtents[i] = image.getExtents()[i];
            }
            // Put chi-squared in the last fourth dimension slot
            resultExtents[3] = numVariables+1;
    		resultImage = new ModelImage(ModelStorageBase.DOUBLE, resultExtents, image.getImageName() + "_params");
    		int statusExtents[] = new int[3];
    		for (i = 0; i < 3; i++) {
    		    statusExtents[i] = image.getExtents()[i];
    		}
    		exitStatusImage = new ModelImage(ModelStorageBase.INTEGER, statusExtents, image.getImageName() + "_exit_status");
    		
    		tfAlgo = new AlgorithmTimeFitting(resultImage, image, exitStatusImage, useLog, functionFit, numVariables, 
    		                                  findInitialFromData, initial);
    
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            tfAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), tfAlgo);
            
            // Hide dialog
            setVisible(false);
            
            if (isRunInSeparateThread()) {
            
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (tfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
            } else {
                tfAlgo.run();
            }
        } catch (OutOfMemoryError x) {
        
        System.gc();
        MipavUtil.displayError("Dialog TimeFitting: unable to allocate enough memory");
        
        return;
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
		
	    if (algorithm instanceof AlgorithmTimeFitting) {
            Preferences.debug("Time fitting elapsed: " + algorithm.getElapsedTime() + "\n");
            image.clearMask();

            if ((tfAlgo.isCompleted() == true) && (resultImage != null)) {

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame for resultImage");
                }
                
                if (exitStatusImage != null) {
                    try {
                        new ViewJFrameImage(exitStatusImage, null, new Dimension(610, 220));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame for exitStatusImage");
                    }    
                }
            } else if ((resultImage != null) || (exitStatusImage != null)) {
                
                if (resultImage != null) {
                    // algorithm failed but result image still has garbage
                    resultImage.disposeLocal(); // clean up memory
                    resultImage = null;
                }
                
                if (exitStatusImage != null) {
                    exitStatusImage.disposeLocal();
                    exitStatusImage = null;
                }
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
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
        
        JLabel choiceLabel3 = new JLabel("Parameter a0 fitting is in the first output volume");
        choiceLabel3.setForeground(Color.black);
        choiceLabel3.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel3, gbc);
        
        JLabel choiceLabel4 = new JLabel("a1 fitting is in the second output volume and so on");
        choiceLabel4.setForeground(Color.black);
        choiceLabel4.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel4, gbc);
        
        JLabel choiceLabel5 = new JLabel("a chi-squared volume is the last volume");
        choiceLabel5.setForeground(Color.black);
        choiceLabel5.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(choiceLabel5, gbc);
        
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

        textImage = new JTextField(30);
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textImage, gbc);
        
        logCheckBox = new JCheckBox("Fit the log10 of intensity values", false);
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
        
        lorentzFit = new JRadioButton("Fit Lorentz (a0/((x-a1)*(x-a1) + a2*a2)", false);
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
        gbc.gridy++;
        mainPanel.add(numVariablesLabel, gbc);
        
        numVariablesField = new JTextField(5);
        numVariablesField.setText("5");
        numVariablesField.setFont(serif12);
        numVariablesField.setForeground(Color.black);
        numVariablesField.setEnabled(false);
        gbc.gridx = 1;
        mainPanel.add(numVariablesField, gbc);
        
        rayleighFit = new JRadioButton("Fit Rayleigh Distribution a2 *(x-a0)*exp(-(x-a0)*(x-a0)/a1)*u(x-a0)", false);
        rayleighFit.setFont(serif12);
        rayleighFit.setForeground(Color.black);
        rayleighFit.addActionListener(this);
        functionGroup.add(rayleighFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(rayleighFit, gbc);
        
        labelVOI = new JLabel("Optionally open a VOI file or draw a VOI");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labelVOI, gbc);
        
        buttonVOIFile = new JButton("Open an optional VOI file");
        buttonVOIFile.setForeground(Color.black);
        buttonVOIFile.setFont(serif12B);
        buttonVOIFile.addActionListener(this);
        buttonVOIFile.setActionCommand("VOIFile");
        buttonVOIFile.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(buttonVOIFile, gbc);
        
        textVOIFile = new JTextField(30);
        textVOIFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textVOIFile, gbc);
        
        initialCheckBox = new JCheckBox("Find initial from data", true);
        initialCheckBox.setFont(serif12);
        initialCheckBox.setForeground(Color.black);
        initialCheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(initialCheckBox, gbc);
        
        labela0 = new JLabel("Initial a0 value");
        labela0.setForeground(Color.black);
        labela0.setFont(serif12);
        labela0.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela0, gbc);
        
        texta0 = new JTextField(20);
        texta0.setFont(serif12);
        texta0.setText("0.0");
        texta0.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta0, gbc);
        
        labela1 = new JLabel("Initial a1 value");
        labela1.setForeground(Color.black);
        labela1.setFont(serif12);
        labela1.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela1, gbc);
        
        texta1 = new JTextField(20);
        texta1.setFont(serif12);
        texta1.setText("1.0");
        texta1.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta1, gbc);
        
        labela2 = new JLabel("Initial a2 value");
        labela2.setForeground(Color.black);
        labela2.setFont(serif12);
        labela2.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela2, gbc);
        
        texta2 = new JTextField(20);
        texta2.setFont(serif12);
        texta2.setText("1.0");
        texta2.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta2, gbc);
        
        labela3 = new JLabel("Initial a3 value");
        labela3.setForeground(Color.black);
        labela3.setFont(serif12);
        labela3.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela3, gbc);
        
        texta3 = new JTextField(20);
        texta3.setFont(serif12);
        texta3.setText("1.0");
        texta3.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta3, gbc);
        
        labela4 = new JLabel("Initial a4 value");
        labela4.setForeground(Color.black);
        labela4.setFont(serif12);
        labela4.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela4, gbc);
        
        texta4 = new JTextField(20);
        texta4.setFont(serif12);
        texta4.setText("1.0");
        texta4.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta4, gbc);
        
        labela5 = new JLabel("Initial a5 value");
        labela5.setForeground(Color.black);
        labela5.setFont(serif12);
        labela5.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela5, gbc);
        
        texta5 = new JTextField(20);
        texta5.setFont(serif12);
        texta5.setText("1.0");
        texta5.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta5, gbc);
        
        labela6 = new JLabel("Initial a6 value");
        labela6.setForeground(Color.black);
        labela6.setFont(serif12);
        labela6.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(labela6, gbc);
        
        texta6 = new JTextField(20);
        texta6.setFont(serif12);
        texta6.setText("1.0");
        texta6.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(texta6, gbc);
    
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        
    }
    
    private boolean setVariables() {
        String tmpStr;
		
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
            numVariables = 2;
        }
        else if (exponentialFit.isSelected()) {
            functionFit = EXPONENTIAL_FIT;
            numVariables = 3;
        }
        else if (gaussianFit.isSelected()) {
            functionFit = GAUSSIAN_FIT;
            numVariables = 3;
        }
        else if (laplaceFit.isSelected()) {
            functionFit = LAPLACE_FIT;
            numVariables = 3;
        }
        else if (lorentzFit.isSelected()) {
            functionFit = LORENTZ_FIT;
            numVariables = 3;
        }
        else if (multiExponentialFit.isSelected()) {
            functionFit = MULTIEXPONENTIAL_FIT;
            tmpStr = numVariablesField.getText();
            numVariables = Integer.valueOf(tmpStr).intValue();
            if ((numVariables % 2) == 0) {
                MipavUtil.displayError("Number of variables must be odd");
                return false;
            }
        }
        else if (rayleighFit.isSelected()) {
            functionFit = RAYLEIGH_FIT;
            numVariables = 3;
        }
        
        findInitialFromData = initialCheckBox.isSelected();
        
        if (!findInitialFromData) {
        
            tmpStr = texta0.getText();
            initial[0] = Double.valueOf(tmpStr).doubleValue();
            
            tmpStr = texta1.getText();
            initial[1] = Double.valueOf(tmpStr).doubleValue();
            
            if (numVariables >= 3) {
                tmpStr = texta2.getText();
                initial[2] = Double.valueOf(tmpStr).doubleValue();    
            }
            
            if (numVariables >= 4) {
                tmpStr = texta3.getText();
                initial[3] = Double.valueOf(tmpStr).doubleValue();    
            }
            
            if (numVariables >= 5) {
                tmpStr = texta4.getText();
                initial[4] = Double.valueOf(tmpStr).doubleValue();    
            }
            
            if (numVariables >= 6) {
                tmpStr = texta5.getText();
                initial[5] = Double.valueOf(tmpStr).doubleValue();    
            }
            
            if (numVariables >= 7) {
                tmpStr = texta6.getText();
                initial[6] = Double.valueOf(tmpStr).doubleValue();    
            }
        } // if (!findInitialFromData)
    	
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
     *  window closing
     */
    public void windowClosing(WindowEvent event) {
        if (image != null) {
        	image.disposeLocal();
        	image = null;
        }
        dispose();
    }

	


	
}
