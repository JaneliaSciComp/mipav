package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
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
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

public class JDialogTimeFitting extends JDialogScriptableBase implements AlgorithmInterface {
    
    /** This dialog only allows 1 constant and 3 exponentials in the MULTIEXPONENTIAL_FIT at the present.  From "Pade-Laplace 
     * Analysis in the Fitting of Multi-exponential Nuclear Magnetic Resonance Decay Curves" by N. J. Clayden:
     * "The problem of the correct number of exponentials required to describe a decay has a long history, for 25 years ago
     * Lanczos demonstrated that various three-exponential functions with similar time constants could be accurately reproduced
     * by two-exponential expressions with markedly different amplitudes and time constants.  Experimentally, the consequence of
     * this is that the signal-to-noise ratio must be excellent, perhaps unachievably so, if a true three-exponential decay is
     * not to be confused with a spurious two-exponential fit.  More recently, this conclusion has been tested for a non-linear 
     * least squares analytical method over a wide range of three-exponential functions with varying levels of experimental noise.
     * At practical signal-to-noise ratios, for example 1000:1, the time constants of three exponentials must differ by a factor 
     * of > two if a two-exponential fit is not to be statistically significant using non-linear least squares fitting."
     */
	
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
    
    private JTabbedPane tabbedPane;
    
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
    
    private JCheckBox b0CheckBox;
    
    private JCheckBox b1CheckBox;
    
    private JCheckBox b2CheckBox;
    
    private JCheckBox b3CheckBox;
    
    private JCheckBox b4CheckBox;
    
    private JCheckBox b5CheckBox;
    
    private JCheckBox b6CheckBox;
    
    private JLabel low0Label;
    
    private JTextField low0TextField;
    
    private JLabel high0Label;
    
    private JTextField high0TextField;
    
    private JLabel low1Label;
    
    private JTextField low1TextField;
    
    private JLabel high1Label;
    
    private JTextField high1TextField;
    
    private JLabel low2Label;
    
    private JTextField low2TextField;
    
    private JLabel high2Label;
    
    private JTextField high2TextField;
    
    private JLabel low3Label;
    
    private JTextField low3TextField;
    
    private JLabel high3Label;
    
    private JTextField high3TextField;
    
    private JLabel low4Label;
    
    private JTextField low4TextField;
    
    private JLabel high4Label;
    
    private JTextField high4TextField;
    
    private JLabel low5Label;
    
    private JTextField low5TextField;
    
    private JLabel high5Label;
    
    private JTextField high5TextField;
    
    private JLabel low6Label;
    
    private JTextField low6TextField;
    
    private JLabel high6Label;
    
    private JTextField high6TextField; 
    
    private double initial[] = new double[9];
    
    private boolean useBounds[] = new boolean[9];
    
    private double lowBounds[] = new double[9];
    
    private double highBounds[] = new double[9];
	
	
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
                b2CheckBox.setEnabled(true);
                b3CheckBox.setEnabled(true);
                b4CheckBox.setEnabled(true);
                b5CheckBox.setEnabled(true);
                b6CheckBox.setEnabled(true);
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
            b3CheckBox.setEnabled(false);
            b4CheckBox.setEnabled(false);
            b5CheckBox.setEnabled(false);
            b6CheckBox.setEnabled(false);
            b3CheckBox.setSelected(false);
            b4CheckBox.setSelected(false);
            b5CheckBox.setSelected(false);
            b6CheckBox.setSelected(false);
            low3Label.setEnabled(false);
            low3TextField.setEnabled(false);
            high3Label.setEnabled(false);
            high3TextField.setEnabled(false);
            low4Label.setEnabled(false);
            low4TextField.setEnabled(false);
            high4Label.setEnabled(false);
            high4TextField.setEnabled(false);
            low5Label.setEnabled(false);
            low5TextField.setEnabled(false);
            high5Label.setEnabled(false);
            high5TextField.setEnabled(false);
            low6Label.setEnabled(false);
            low6TextField.setEnabled(false);
            high6Label.setEnabled(false);
            high6TextField.setEnabled(false);
	    	if (source == linearFit) {
	    	    labela2.setEnabled(false);
	    	    texta2.setEnabled(false);
	    	    b2CheckBox.setEnabled(false);
	    	    b2CheckBox.setSelected(false);
	    	    low2Label.setEnabled(false);
	            low2TextField.setEnabled(false);
	            high2Label.setEnabled(false);
	            high2TextField.setEnabled(false);
	    	}
	    	else {
	    	    labela2.setEnabled(!previousFindInitial);
                texta2.setEnabled(!previousFindInitial);
                b3CheckBox.setEnabled(true);
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
	         
	     } else if (source == b0CheckBox) {
	         if (b0CheckBox.isSelected()) {
	             low0Label.setEnabled(true);
	             low0TextField.setEnabled(true);
	             high0Label.setEnabled(true);
	             high0TextField.setEnabled(true);    
	         }
	         else {
	             low0Label.setEnabled(false);
	             low0TextField.setEnabled(false);
	             high0Label.setEnabled(false);
	             high0TextField.setEnabled(false);
	         }
	     } else if (source == b1CheckBox) {
             if (b1CheckBox.isSelected()) {
                 low1Label.setEnabled(true);
                 low1TextField.setEnabled(true);
                 high1Label.setEnabled(true);
                 high1TextField.setEnabled(true);    
             }
             else {
                 low1Label.setEnabled(false);
                 low1TextField.setEnabled(false);
                 high1Label.setEnabled(false);
                 high1TextField.setEnabled(false);
             }
	     } else if (source == b2CheckBox) {
             if (b2CheckBox.isSelected()) {
                 low2Label.setEnabled(true);
                 low2TextField.setEnabled(true);
                 high2Label.setEnabled(true);
                 high2TextField.setEnabled(true);    
             }
             else {
                 low2Label.setEnabled(false);
                 low2TextField.setEnabled(false);
                 high2Label.setEnabled(false);
                 high2TextField.setEnabled(false);
             }
	     } else if (source == b3CheckBox) {
             if (b3CheckBox.isSelected()) {
                 low3Label.setEnabled(true);
                 low3TextField.setEnabled(true);
                 high3Label.setEnabled(true);
                 high3TextField.setEnabled(true);    
             }
             else {
                 low3Label.setEnabled(false);
                 low3TextField.setEnabled(false);
                 high3Label.setEnabled(false);
                 high3TextField.setEnabled(false);
             }
	     } else if (source == b4CheckBox) {
             if (b4CheckBox.isSelected()) {
                 low4Label.setEnabled(true);
                 low4TextField.setEnabled(true);
                 high4Label.setEnabled(true);
                 high4TextField.setEnabled(true);    
             }
             else {
                 low4Label.setEnabled(false);
                 low4TextField.setEnabled(false);
                 high4Label.setEnabled(false);
                 high4TextField.setEnabled(false);
             }
	     } else if (source == b5CheckBox) {
             if (b5CheckBox.isSelected()) {
                 low5Label.setEnabled(true);
                 low5TextField.setEnabled(true);
                 high5Label.setEnabled(true);
                 high5TextField.setEnabled(true);    
             }
             else {
                 low5Label.setEnabled(false);
                 low5TextField.setEnabled(false);
                 high5Label.setEnabled(false);
                 high5TextField.setEnabled(false);
             }
	     } else if (source == b6CheckBox) {
             if (b6CheckBox.isSelected()) {
                 low6Label.setEnabled(true);
                 low6TextField.setEnabled(true);
                 high6Label.setEnabled(true);
                 high6TextField.setEnabled(true);    
             }
             else {
                 low6Label.setEnabled(false);
                 low6TextField.setEnabled(false);
                 high6Label.setEnabled(false);
                 high6TextField.setEnabled(false);
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
    		                                  findInitialFromData, initial, useBounds, lowBounds, highBounds);
    
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
	
	private void init() {
	   setForeground(Color.black);
	   setTitle("Time Fitting");
	   
	   final JPanel fittingPanel = buildFittingPanel();
       final JPanel boundsPanel = buildBoundsPanel();
       
       tabbedPane = new JTabbedPane();
       tabbedPane.setFont(MipavUtil.font12B);
       tabbedPane.addTab("Fitting", fittingPanel);

       tabbedPane.addTab("Bounds", boundsPanel);

       final GridBagConstraints gbc = new GridBagConstraints();
       gbc.gridx = 0;
       gbc.gridy = 0;
       gbc.weightx = 1.0;
       gbc.weighty = 1.0;

       final JPanel buttonPanel = new JPanel();
       buttonPanel.add(buildButtons());

       getContentPane().add(tabbedPane);
       getContentPane().add(buttonPanel, BorderLayout.SOUTH);

       pack();
       setVisible(true);
       setResizable(false);

	}
	
	private JPanel buildFittingPanel() {
	    GuiBuilder gui = new GuiBuilder(this);
	    final JPanel fittingPanel = new JPanel(new GridBagLayout());
	    fittingPanel.setBorder(buildTitledBorder("Fitting"));
	    fittingPanel.setForeground(Color.black);
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
        
        JLabel choiceLabel = new JLabel("Choose a 4D black and white image file");
        choiceLabel.setForeground(Color.black);
        choiceLabel.setFont(serif12);
        fittingPanel.add(choiceLabel, gbc);
        
        JLabel choiceLabel2 = new JLabel("At each volume point a function will be fitted to the time series");
        choiceLabel2.setForeground(Color.black);
        choiceLabel2.setFont(serif12);
        gbc.gridy++;
        fittingPanel.add(choiceLabel2, gbc);
        
        JLabel choiceLabel3 = new JLabel("Parameter a0 fitting is in the first output volume");
        choiceLabel3.setForeground(Color.black);
        choiceLabel3.setFont(serif12);
        gbc.gridy++;
        fittingPanel.add(choiceLabel3, gbc);
        
        JLabel choiceLabel4 = new JLabel("a1 fitting is in the second output volume and so on");
        choiceLabel4.setForeground(Color.black);
        choiceLabel4.setFont(serif12);
        gbc.gridy++;
        fittingPanel.add(choiceLabel4, gbc);
        
        JLabel choiceLabel5 = new JLabel("a chi-squared volume is the last volume");
        choiceLabel5.setForeground(Color.black);
        choiceLabel5.setFont(serif12);
        gbc.gridy++;
        fittingPanel.add(choiceLabel5, gbc);
        
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
        fittingPanel.add(imageList.getParent(), gbc);

        textImage = new JTextField(30);
        textImage.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(textImage, gbc);
        
        logCheckBox = new JCheckBox("Fit the log10 of intensity values", false);
        logCheckBox.setFont(serif12);
        logCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(logCheckBox, gbc);
        
        JLabel functionLabel = new JLabel("Choose a fitting function");
        functionLabel.setForeground(Color.black);
        functionLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(functionLabel, gbc);
        
        functionGroup = new ButtonGroup();
        linearFit = new JRadioButton("Fit linear (a0 + a1 * t)", true);
        linearFit.setFont(serif12);
        linearFit.setForeground(Color.black);
        linearFit.addActionListener(this);
        functionGroup.add(linearFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(linearFit, gbc);
        
        exponentialFit = new JRadioButton("Fit exponential (a0+a1*exp(a2*t))", false);
        exponentialFit.setFont(serif12);
        exponentialFit.setForeground(Color.black);
        exponentialFit.addActionListener(this);
        functionGroup.add(exponentialFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(exponentialFit, gbc);
        
        gaussianFit = new JRadioButton("Fit Gaussian (a0*exp(-(t-a1)^2/(2*(a2)^2)))", false);
        gaussianFit.setFont(serif12);
        gaussianFit.setForeground(Color.black);
        gaussianFit.addActionListener(this);
        functionGroup.add(gaussianFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(gaussianFit, gbc);
        
        laplaceFit = new JRadioButton("Fit Laplace (a0*exp(-|t-a1|/a2))", false);
        laplaceFit.setFont(serif12);
        laplaceFit.setForeground(Color.black);
        laplaceFit.addActionListener(this);
        functionGroup.add(laplaceFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(laplaceFit, gbc);
        
        lorentzFit = new JRadioButton("Fit Lorentz (a0/((t-a1)*(t-a1) + a2*a2)", false);
        lorentzFit.setFont(serif12);
        lorentzFit.setForeground(Color.black);
        lorentzFit.addActionListener(this);
        functionGroup.add(lorentzFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(lorentzFit, gbc);
        
        multiExponentialFit = new JRadioButton("Fit Multiexponential a0 + sum of a[2*k+1]*exp(a[2*k+2]*t)", false);
        multiExponentialFit.setFont(serif12);
        multiExponentialFit.setForeground(Color.black);
        multiExponentialFit.addActionListener(this);
        functionGroup.add(multiExponentialFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(multiExponentialFit, gbc);
        
        numVariablesLabel = new JLabel("Number of variables (Only for Multiexponential)");
        numVariablesLabel.setFont(serif12);
        numVariablesLabel.setForeground(Color.black);
        numVariablesLabel.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(numVariablesLabel, gbc);
        
        numVariablesField = new JTextField(5);
        numVariablesField.setText("5");
        numVariablesField.setFont(serif12);
        numVariablesField.setForeground(Color.black);
        numVariablesField.setEnabled(false);
        gbc.gridx = 1;
        fittingPanel.add(numVariablesField, gbc);
        
        rayleighFit = new JRadioButton("Fit Rayleigh Distribution a0 *(t-a1)*exp(-(t-a1)*(t-a1)/a2)*u(t-a1)", false);
        rayleighFit.setFont(serif12);
        rayleighFit.setForeground(Color.black);
        rayleighFit.addActionListener(this);
        functionGroup.add(rayleighFit);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(rayleighFit, gbc);
        
        labelVOI = new JLabel("Optionally open a VOI file or draw a VOI");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labelVOI, gbc);
        
        buttonVOIFile = new JButton("Open an optional VOI file");
        buttonVOIFile.setForeground(Color.black);
        buttonVOIFile.setFont(serif12B);
        buttonVOIFile.addActionListener(this);
        buttonVOIFile.setActionCommand("VOIFile");
        buttonVOIFile.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(buttonVOIFile, gbc);
        
        textVOIFile = new JTextField(30);
        textVOIFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(textVOIFile, gbc);
        
        initialCheckBox = new JCheckBox("Find initial from data", true);
        initialCheckBox.setFont(serif12);
        initialCheckBox.setForeground(Color.black);
        initialCheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(initialCheckBox, gbc);
        
        labela0 = new JLabel("Initial a0 value");
        labela0.setForeground(Color.black);
        labela0.setFont(serif12);
        labela0.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela0, gbc);
        
        texta0 = new JTextField(20);
        texta0.setFont(serif12);
        texta0.setText("0.0");
        texta0.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta0, gbc);
        
        labela1 = new JLabel("Initial a1 value");
        labela1.setForeground(Color.black);
        labela1.setFont(serif12);
        labela1.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela1, gbc);
        
        texta1 = new JTextField(20);
        texta1.setFont(serif12);
        texta1.setText("1.0");
        texta1.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta1, gbc);
        
        labela2 = new JLabel("Initial a2 value");
        labela2.setForeground(Color.black);
        labela2.setFont(serif12);
        labela2.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela2, gbc);
        
        texta2 = new JTextField(20);
        texta2.setFont(serif12);
        texta2.setText("1.0");
        texta2.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta2, gbc);
        
        labela3 = new JLabel("Initial a3 value");
        labela3.setForeground(Color.black);
        labela3.setFont(serif12);
        labela3.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela3, gbc);
        
        texta3 = new JTextField(20);
        texta3.setFont(serif12);
        texta3.setText("1.0");
        texta3.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta3, gbc);
        
        labela4 = new JLabel("Initial a4 value");
        labela4.setForeground(Color.black);
        labela4.setFont(serif12);
        labela4.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela4, gbc);
        
        texta4 = new JTextField(20);
        texta4.setFont(serif12);
        texta4.setText("1.0");
        texta4.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta4, gbc);
        
        labela5 = new JLabel("Initial a5 value");
        labela5.setForeground(Color.black);
        labela5.setFont(serif12);
        labela5.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela5, gbc);
        
        texta5 = new JTextField(20);
        texta5.setFont(serif12);
        texta5.setText("1.0");
        texta5.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta5, gbc);
        
        labela6 = new JLabel("Initial a6 value");
        labela6.setForeground(Color.black);
        labela6.setFont(serif12);
        labela6.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        fittingPanel.add(labela6, gbc);
        
        texta6 = new JTextField(20);
        texta6.setFont(serif12);
        texta6.setText("1.0");
        texta6.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        fittingPanel.add(texta6, gbc);
	    return fittingPanel;
	}
	
	private JPanel buildBoundsPanel() {
	    final JPanel boundsPanel = new JPanel(new GridBagLayout());
	    boundsPanel.setBorder(buildTitledBorder("Bounds"));
        boundsPanel.setForeground(Color.black);
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
        
        b0CheckBox = new JCheckBox("Use lower and upper bounds on a0", false);
        b0CheckBox.setFont(serif12);
        b0CheckBox.setForeground(Color.black);
        b0CheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b0CheckBox, gbc);
        
        low0Label = new JLabel("a0 lower bound");
        low0Label.setFont(serif12);
        low0Label.setForeground(Color.black);
        low0Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low0Label, gbc);
        
        low0TextField = new JTextField(20);
        low0TextField.setText(" ");
        low0TextField.setFont(serif12);
        low0TextField.setForeground(Color.black);
        low0TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low0TextField, gbc);
        
        high0Label = new JLabel("a0 upper bound");
        high0Label.setFont(serif12);
        high0Label.setForeground(Color.black);
        high0Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high0Label, gbc);
        
        high0TextField = new JTextField(20);
        high0TextField.setText(" ");
        high0TextField.setFont(serif12);
        high0TextField.setForeground(Color.black);
        high0TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high0TextField, gbc);
        
        b1CheckBox = new JCheckBox("Use lower and upper bounds on a1", false);
        b1CheckBox.setFont(serif12);
        b1CheckBox.setForeground(Color.black);
        b1CheckBox.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b1CheckBox, gbc);
        
        low1Label = new JLabel("a1 lower bound");
        low1Label.setFont(serif12);
        low1Label.setForeground(Color.black);
        low1Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low1Label, gbc);
        
        low1TextField = new JTextField(20);
        low1TextField.setText(" ");
        low1TextField.setFont(serif12);
        low1TextField.setForeground(Color.black);
        low1TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low1TextField, gbc);
        
        high1Label = new JLabel("a1 upper bound");
        high1Label.setFont(serif12);
        high1Label.setForeground(Color.black);
        high1Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high1Label, gbc);
        
        high1TextField = new JTextField(20);
        high1TextField.setText(" ");
        high1TextField.setFont(serif12);
        high1TextField.setForeground(Color.black);
        high1TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high1TextField, gbc);
        
        b2CheckBox = new JCheckBox("Use lower and upper bounds on a2", false);
        b2CheckBox.setFont(serif12);
        b2CheckBox.setForeground(Color.black);
        b2CheckBox.addActionListener(this);
        b2CheckBox.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b2CheckBox, gbc);
        
        low2Label = new JLabel("a2 lower bound");
        low2Label.setFont(serif12);
        low2Label.setForeground(Color.black);
        low2Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low2Label, gbc);
        
        low2TextField = new JTextField(20);
        low2TextField.setText(" ");
        low2TextField.setFont(serif12);
        low2TextField.setForeground(Color.black);
        low2TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low2TextField, gbc);
        
        high2Label = new JLabel("a2 upper bound");
        high2Label.setFont(serif12);
        high2Label.setForeground(Color.black);
        high2Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high2Label, gbc);
        
        high2TextField = new JTextField(20);
        high2TextField.setText(" ");
        high2TextField.setFont(serif12);
        high2TextField.setForeground(Color.black);
        high2TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high2TextField, gbc);
        
        b3CheckBox = new JCheckBox("Use lower and upper bounds on a3", false);
        b3CheckBox.setFont(serif12);
        b3CheckBox.setForeground(Color.black);
        b3CheckBox.addActionListener(this);
        b3CheckBox.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b3CheckBox, gbc);
        
        low3Label = new JLabel("a3 lower bound");
        low3Label.setFont(serif12);
        low3Label.setForeground(Color.black);
        low3Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low3Label, gbc);
        
        low3TextField = new JTextField(20);
        low3TextField.setText(" ");
        low3TextField.setFont(serif12);
        low3TextField.setForeground(Color.black);
        low3TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low3TextField, gbc);
        
        high3Label = new JLabel("a3 upper bound");
        high3Label.setFont(serif12);
        high3Label.setForeground(Color.black);
        high3Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high3Label, gbc);
        
        high3TextField = new JTextField(20);
        high3TextField.setText(" ");
        high3TextField.setFont(serif12);
        high3TextField.setForeground(Color.black);
        high3TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high3TextField, gbc);
        
        b4CheckBox = new JCheckBox("Use lower and upper bounds on a4", false);
        b4CheckBox.setFont(serif12);
        b4CheckBox.setForeground(Color.black);
        b4CheckBox.addActionListener(this);
        b4CheckBox.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b4CheckBox, gbc);
        
        low4Label = new JLabel("a4 lower bound");
        low4Label.setFont(serif12);
        low4Label.setForeground(Color.black);
        low4Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low4Label, gbc);
        
        low4TextField = new JTextField(20);
        low4TextField.setText(" ");
        low4TextField.setFont(serif12);
        low4TextField.setForeground(Color.black);
        low4TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low4TextField, gbc);
        
        high4Label = new JLabel("a4 upper bound");
        high4Label.setFont(serif12);
        high4Label.setForeground(Color.black);
        high4Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high4Label, gbc);
        
        high4TextField = new JTextField(20);
        high4TextField.setText(" ");
        high4TextField.setFont(serif12);
        high4TextField.setForeground(Color.black);
        high4TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high4TextField, gbc);
        
        b5CheckBox = new JCheckBox("Use lower and upper bounds on a5", false);
        b5CheckBox.setFont(serif12);
        b5CheckBox.setForeground(Color.black);
        b5CheckBox.addActionListener(this);
        b5CheckBox.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b5CheckBox, gbc);
        
        low5Label = new JLabel("a5 lower bound");
        low5Label.setFont(serif12);
        low5Label.setForeground(Color.black);
        low5Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low5Label, gbc);
        
        low5TextField = new JTextField(20);
        low5TextField.setText(" ");
        low5TextField.setFont(serif12);
        low5TextField.setForeground(Color.black);
        low5TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low5TextField, gbc);
        
        high5Label = new JLabel("a5 upper bound");
        high5Label.setFont(serif12);
        high5Label.setForeground(Color.black);
        high5Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high5Label, gbc);
        
        high5TextField = new JTextField(20);
        high5TextField.setText(" ");
        high5TextField.setFont(serif12);
        high5TextField.setForeground(Color.black);
        high5TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high5TextField, gbc);
        
        b6CheckBox = new JCheckBox("Use lower and upper bounds on a6", false);
        b6CheckBox.setFont(serif12);
        b6CheckBox.setForeground(Color.black);
        b6CheckBox.addActionListener(this);
        b6CheckBox.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(b6CheckBox, gbc);
        
        low6Label = new JLabel("a6 lower bound");
        low6Label.setFont(serif12);
        low6Label.setForeground(Color.black);
        low6Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(low6Label, gbc);
        
        low6TextField = new JTextField(20);
        low6TextField.setText(" ");
        low6TextField.setFont(serif12);
        low6TextField.setForeground(Color.black);
        low6TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(low6TextField, gbc);
        
        high6Label = new JLabel("a6 upper bound");
        high6Label.setFont(serif12);
        high6Label.setForeground(Color.black);
        high6Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy++;
        boundsPanel.add(high6Label, gbc);
        
        high6TextField = new JTextField(20);
        high6TextField.setText(" ");
        high6TextField.setFont(serif12);
        high6TextField.setForeground(Color.black);
        high6TextField.setEnabled(false);
        gbc.gridx = 1;
        boundsPanel.add(high6TextField, gbc);
    
	    return boundsPanel;
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
        
        useBounds[0] = b0CheckBox.isSelected();
        if (useBounds[0]) {
            tmpStr = low0TextField.getText();
            lowBounds[0] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[0] > initial[0]) {
                MipavUtil.displayError("The a0 low bound cannot be greater than the a0 initial value");
                return false;
            }
            
            tmpStr = high0TextField.getText();
            highBounds[0] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[0] < initial[0]) {
                MipavUtil.displayError("The a0 high bound cannot be less than the a0 initial value");
                return false;
            }
            
            if (highBounds[0] < lowBounds[0]) {
                MipavUtil.displayError("The a0 high bound cannot be less than the a0 low bound");
                return false;
            }
        }
        
        useBounds[1] = b1CheckBox.isSelected();
        if (useBounds[1]) {
            tmpStr = low1TextField.getText();
            lowBounds[1] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[1] > initial[1]) {
                MipavUtil.displayError("The a1 low bound cannot be greater than the a1 initial value");
                return false;
            }
            
            tmpStr = high1TextField.getText();
            highBounds[1] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[1] < initial[1]) {
                MipavUtil.displayError("The a1 high bound cannot be less than the a1 initial value");
                return false;
            }
            
            if (highBounds[1] < lowBounds[1]) {
                MipavUtil.displayError("The a1 high bound cannot be less than the a1 low bound");
                return false;
            }
        }
        
        useBounds[2] = b2CheckBox.isSelected();
        if (useBounds[2]) {
            tmpStr = low2TextField.getText();
            lowBounds[2] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[2] > initial[2]) {
                MipavUtil.displayError("The a2 low bound cannot be greater than the a2 initial value");
                return false;
            }
            
            tmpStr = high2TextField.getText();
            highBounds[2] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[2] < initial[2]) {
                MipavUtil.displayError("The a2 high bound cannot be less than the a2 initial value");
                return false;
            }
            
            if (highBounds[2] < lowBounds[2]) {
                MipavUtil.displayError("The a2 high bound cannot be less than the a2 low bound");
                return false;
            }
        }
        
        useBounds[3] = b3CheckBox.isSelected();
        if (useBounds[3]) {
            tmpStr = low3TextField.getText();
            lowBounds[3] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[3] > initial[3]) {
                MipavUtil.displayError("The a3 low bound cannot be greater than the a3 initial value");
                return false;
            }
            
            tmpStr = high3TextField.getText();
            highBounds[3] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[3] < initial[3]) {
                MipavUtil.displayError("The a3 high bound cannot be less than the a3 initial value");
                return false;
            }
            
            if (highBounds[3] < lowBounds[3]) {
                MipavUtil.displayError("The a3 high bound cannot be less than the a3 low bound");
                return false;
            }
        }
        
        useBounds[4] = b4CheckBox.isSelected();
        if (useBounds[4]) {
            tmpStr = low4TextField.getText();
            lowBounds[4] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[4] > initial[4]) {
                MipavUtil.displayError("The a4 low bound cannot be greater than the a4 initial value");
                return false;
            }
            
            tmpStr = high4TextField.getText();
            highBounds[4] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[4] < initial[4]) {
                MipavUtil.displayError("The a4 high bound cannot be less than the a4 initial value");
                return false;
            }
            
            if (highBounds[4] < lowBounds[4]) {
                MipavUtil.displayError("The a4 high bound cannot be less than the a4 low bound");
                return false;
            }
        }
        
        useBounds[5] = b5CheckBox.isSelected();
        if (useBounds[5]) {
            tmpStr = low5TextField.getText();
            lowBounds[5] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[5] > initial[5]) {
                MipavUtil.displayError("The a5 low bound cannot be greater than the a5 initial value");
                return false;
            }
            
            tmpStr = high5TextField.getText();
            highBounds[5] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[5] < initial[5]) {
                MipavUtil.displayError("The a5 high bound cannot be less than the a5 initial value");
                return false;
            }
            
            if (highBounds[5] < lowBounds[5]) {
                MipavUtil.displayError("The a5 high bound cannot be less than the a5 low bound");
                return false;
            }
        }
        
        useBounds[6] = b6CheckBox.isSelected();
        if (useBounds[6]) {
            tmpStr = low6TextField.getText();
            lowBounds[6] = Double.valueOf(tmpStr).doubleValue();
            if (lowBounds[6] > initial[6]) {
                MipavUtil.displayError("The a6 low bound cannot be greater than the a6 initial value");
                return false;
            }
            
            tmpStr = high6TextField.getText();
            highBounds[6] = Double.valueOf(tmpStr).doubleValue();
            if (highBounds[6] < initial[6]) {
                MipavUtil.displayError("The a6 high bound cannot be less than the a6 initial value");
                return false;
            }
            
            if (highBounds[6] < lowBounds[6]) {
                MipavUtil.displayError("The a6 high bound cannot be less than the a6 low bound");
                return false;
            }
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
