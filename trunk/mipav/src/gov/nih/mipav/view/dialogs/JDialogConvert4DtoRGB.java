package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author pandyan
 * This class converts 4D datasets that have 3 or less 4th dim slices to an RGB image
 */
public class JDialogConvert4DtoRGB extends JDialogScriptableBase implements AlgorithmInterface {
	
	/** src image**/
	private ModelImage image;
	
	/** extents for extracted 3D subset **/
    private int[] destExtents;
	
	/** num x slices of src image **/
    private int xSlices;

    /** num y slices of src image **/
    private int ySlices;

    /** num z slices for src image **/
    private int zSlices;
    
    /** extraced 3D Model Images from src image **/
    private ModelImage[] channelImages;
    
    /** corresponding dropdown for each channel image **/
    private JComboBox[] channelColorsComboBoxes;
    
    /** ref to AlgorithmSubset **/
    private AlgorithmSubset subsetAlgo;
    
    /** main panel of dialog **/
    private JPanel mainPanel;
    
    /** input panel of dialog **/
    private JPanel inputPanel;
    
    /** ok/cancel panel of dialog **/
    private JPanel OKCancelPanel;
    
    /** re-map checkbox  **/
    private JCheckBox remapCheckBox;
    
    /** blue channel image **/
    private ModelImage imageB;

    /** green channel image **/
    private ModelImage imageG;

    /** red channel image **/
    private ModelImage imageR;
    
    /** result image **/
    private ModelImage resultImage;
    
    /** boolean for remap **/
    private boolean remapMode;
    
    /** ref to AlgorithmRGBConcat**/
    private AlgorithmRGBConcat mathAlgo;
    
    /** blank image **/
    private ModelImage blank;
    
    /** vol 1 channel for scripting **/
    private String vol1Channel = "blank";
    
    /** vol 2 channel for scripting **/
    private String vol2Channel = "blank";
    
    /** vol 3 channel for scriptin **/
    private String vol3Channel = "blank";
    
    /** RED CHANNEL String **/
    private final static String RED_CHANNEL = "RED CHANNEL";
    
    /** GREEN CHANNEL String **/
    private final static String GREEN_CHANNEL = "GREEN CHANNEL";
    
    /** BLUE CHANNEL String **/
    private final static String BLUE_CHANNEL = "BLUE CHANNEL";
    
    /** BLANK String **/
    private final static String BLANK = "blank";
    
    
    
    
    
    
	
	
	/**
	 * Empty Constructor (needed for scripting)
	 */
	public JDialogConvert4DtoRGB() {
		
	}
	
	
	
	/**
	 * Constructor
	 * @param theFrame
	 * @param image
	 */
	public JDialogConvert4DtoRGB(Frame theParentFrame, ModelImage image) {
		super(theParentFrame, false);
		this.image = image;
		init();
	}
	
	
	
	/**
	 * init
	 *
	 */
	public void init() {
		
		
		if(image.getExtents()[3] > 3) {
			MipavUtil.displayError("Convert 4D to RGB will only work if the 4th dimension has 3 or less slices");
			return;
		}
		
		extract3DSubsets();
		
		setTitle("Convert 4D to RGB");
		GridBagConstraints gbc = new GridBagConstraints();
		mainPanel = new JPanel();
		inputPanel = new JPanel();
		inputPanel.setLayout(new GridBagLayout());
		inputPanel.setBorder(buildTitledBorder("Volume / Color Channel"));
        channelColorsComboBoxes = new JComboBox[channelImages.length];
        int i = 0;
		for(i=0;i<channelImages.length;i++) {
			gbc.gridx = 0;
	        gbc.gridy = i;
	        gbc.insets = new Insets(3, 10, 3, 3);
	        JLabel label = new JLabel(channelImages[i].getImageName());
	        label.setFont(serif12);
	        inputPanel.add(label,gbc);
			gbc.gridx = 1;
	        gbc.gridy = i;
	        gbc.insets = new Insets(3, 10, 3, 10);
	        channelColorsComboBoxes[i] = new JComboBox();
	        channelColorsComboBoxes[i].addItem(RED_CHANNEL);
	        channelColorsComboBoxes[i].addItem(GREEN_CHANNEL);
	        channelColorsComboBoxes[i].addItem(BLUE_CHANNEL);
	        channelColorsComboBoxes[i].addItem(BLANK);
	        if(i==0) {
	        	channelColorsComboBoxes[i].setSelectedIndex(0);
	        }
	        if(i==1) {
	        	channelColorsComboBoxes[i].setSelectedIndex(1);
	        }
	        if(i==2) {
	        	channelColorsComboBoxes[i].setSelectedIndex(2);
	        }
	        inputPanel.add(channelColorsComboBoxes[i],gbc);
		}
		gbc.gridx = 0;
        gbc.gridy = i + 1;
        gbc.insets = new Insets(3, 10, 3, 3);
        gbc.anchor = GridBagConstraints.WEST;
        remapCheckBox = new JCheckBox("Remap data (0-255)", true);
        remapCheckBox.setFont(serif12);
		inputPanel.add(remapCheckBox,gbc);	
		mainPanel.add(inputPanel);
		OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton);
     
        buildHelpButton();
        OKCancelPanel.add(helpButton);
        
		getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
		pack();
		setResizable(false);
        setVisible(true);
	}
	
	
	
	
	/**
	 * method that extracts 3D subsets
	 * from the 4D dataset
	 *
	 */
	public void extract3DSubsets() {
		destExtents = new int[3];
		xSlices = image.getExtents()[0];
        ySlices = image.getExtents()[1];
        zSlices = image.getExtents()[2];
        destExtents[0] = xSlices;
        destExtents[1] = ySlices;
        destExtents[2] = zSlices;
        channelImages = new ModelImage[image.getExtents()[3]];
		for(int i=0;i<image.getExtents()[3];i++) {
			int num = i + 1;
			String resultString = image.getImageName() + "_Vol=" + num;
			channelImages[i] = new ModelImage(image.getType(), destExtents, resultString);
			subsetAlgo = new AlgorithmSubset(image, channelImages[i], AlgorithmSubset.REMOVE_T, i);
			subsetAlgo.setRunningInSeparateThread(false);
			subsetAlgo.run();
		}	
	}

	
	
	/**
	 * Call Algorithm.....calls the RGBConcat algorithm
	 */
	protected void callAlgorithm() {
		try {
            System.gc();
            resultImage = new ModelImage(ModelImage.ARGB, channelImages[0].getExtents(),
                                         makeImageName(image.getImageName(), "_rgb"));

            System.out.println("red channel image is " + imageR.getImageName());
            System.out.println("green channel image is " + imageG.getImageName());
            System.out.println("blue channel image is " + imageB.getImageName());
            mathAlgo = new AlgorithmRGBConcat(imageR, imageG, imageB, resultImage, remapMode, false, 255.0f, true);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mathAlgo.addListener(this);

            createProgressBar(imageR.getImageName(), mathAlgo);

            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (mathAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                mathAlgo.run();
            }

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog RGB concat: unable to allocate enough memory");
            finalize();
            return;
        }
    } 


	
	
	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	if (algorithm instanceof AlgorithmRGBConcat) {

            if ((mathAlgo.isCompleted() == true) && (resultImage != null)) {

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
                insertScriptLine();
                finalize();
            }
    	}
    }

    
    /**
     * set GUI...for scripting
     */
	protected void setGUIFromParams() {
		image = scriptParameters.retrieveInputImage();
		extract3DSubsets();
		setVol1Channel(scriptParameters.getParams().getString("vol1Channel"));
		setVol2Channel(scriptParameters.getParams().getString("vol2Channel"));
		setVol3Channel(scriptParameters.getParams().getString("vol3Channel"));
		//create blank model image
		blank = new ModelImage(ModelImage.SHORT, channelImages[0].getExtents(), makeImageName(image.getImageName(), ""));
		for(int i=0;i<channelImages.length;i++) {
			if(i==0) {
				if(vol1Channel.equals(RED_CHANNEL)) {
					setRedImage(channelImages[i]);
				}
				else if(vol1Channel.equals(GREEN_CHANNEL)) {
					setGreenImage(channelImages[i]);
				}
				else if(vol1Channel.equals(BLUE_CHANNEL)) {
					setBlueImage(channelImages[i]);
				}
			}
			if(i==1) {
				if(vol2Channel.equals(RED_CHANNEL)) {
					setRedImage(channelImages[i]);
				}
				else if(vol2Channel.equals(GREEN_CHANNEL)) {
					setGreenImage(channelImages[i]);
				}
				else if(vol2Channel.equals(BLUE_CHANNEL)) {
					setBlueImage(channelImages[i]);
				}
			}
			if(i==2) {
				if(vol3Channel.equals(RED_CHANNEL)) {
					setRedImage(channelImages[i]);
				}
				else if(vol3Channel.equals(GREEN_CHANNEL)) {
					setGreenImage(channelImages[i]);
				}
				else if(vol3Channel.equals(BLUE_CHANNEL)) {
					setBlueImage(channelImages[i]);
				}
			}
		}
		
		//set the blanks up
		if (imageR == null) {
			imageR = blank;
		}
		if (imageG == null) {
			imageG = blank;
		}
		if (imageB == null) {
			imageB = blank;
		}
		
        setRemapMode(scriptParameters.getParams().getBoolean("do_remap_values"));
	}

	
	/**
	 * store params...for scripting
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(image);
		scriptParameters.getParams().put(ParameterFactory.newParameter("vol1Channel", vol1Channel));
		scriptParameters.getParams().put(ParameterFactory.newParameter("vol2Channel", vol2Channel));	
		scriptParameters.getParams().put(ParameterFactory.newParameter("vol3Channel", vol3Channel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_remap_values", remapMode));
	}

	
	/**
	 * Action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("ok")) {
			boolean success = setVariables();
			if(success) {
				callAlgorithm();
			}
		} 
		else if(command.equalsIgnoreCase("cancel")) {
			finalize();
			dispose();
		}
		else if (command.equalsIgnoreCase("help")) {
			MipavUtil.showHelp("U4008");
		}

	}
	
	
	
	/**
	 * set variables
	 * @return boolean
	 */
	public boolean setVariables() {
		
		//check that there is not more than 1 red channel or more than 1 green channel
		//or more than 1 blue channel set
		int numReds = 0;
		int numBlues = 0;
		int numGreens = 0;
		for(int i=0;i<channelColorsComboBoxes.length;i++) {
			int channel = channelColorsComboBoxes[i].getSelectedIndex();
			if(channel == 0) {
				numReds++;
			}
			if(channel == 1) {
				numBlues++;
			}
			if(channel == 2) {
				numGreens++;
			}
		}
		if(numReds > 1 || numGreens > 1 || numBlues > 1) {
            MipavUtil.displayError("No duplicate color channels allowed");
            return false;
		}
		
		//go through channel images....get its corresponding checkbox and set the red, 
		//green, and blue images
		for(int i=0;i<channelImages.length;i++) {
			int channel = channelColorsComboBoxes[i].getSelectedIndex();
			if(channel == 0) {
				setRedImage(channelImages[i]);
			}
			if(channel == 1) {
				setGreenImage(channelImages[i]);
			}
			if(channel == 2) {
				setBlueImage(channelImages[i]);
			}
			
		}
		
		
		//set these vars for scripting purposes
		for(int i=0;i<channelColorsComboBoxes.length;i++) {
			if(i==0) {
				vol1Channel = (String)channelColorsComboBoxes[i].getSelectedItem();
			}
			if(i==1) {
				vol2Channel = (String)channelColorsComboBoxes[i].getSelectedItem();
			}
			if(i==2) {
				vol3Channel = (String)channelColorsComboBoxes[i].getSelectedItem();
			}
		}
		
		//create blank model image
		blank = new ModelImage(ModelImage.SHORT, channelImages[0].getExtents(), makeImageName(image.getImageName(), ""));
		
		//set the blanks up
		if (imageR == null) {
			imageR = blank;
		}
		if (imageG == null) {
			imageG = blank;
		}
		if (imageB == null) {
			imageB = blank;
		}
	
		//set the remapmode
		if (remapCheckBox.isSelected()) {
            remapMode = true;
        } else {
            remapMode = false;
        }
		
		return true;
	}
	
	/**
     * Accessor that sets the Blue Image Source.
     *
     * @param  im  image to set the Blue Image Source to.
     */
    public void setBlueImage(ModelImage im) {
        imageB = im;
    }
    
    /**
     * Accessor that sets the Green Image Source.
     *
     * @param  im  image to set the Green Image Source to.
     */
    public void setRedImage(ModelImage im) {
        imageR = im;
    }
    
    /**
     * Accessor that sets the Green Image Source.
     *
     * @param  im  image to set the Green Image Source to.
     */
    public void setGreenImage(ModelImage im) {
        imageG = im;
    }
    
	/**
	 * set vol1Channel
	 * @param vol1Channel
	 */
	public void setVol1Channel(String vol1Channel) {
		this.vol1Channel = vol1Channel;
	}
	
	/**
	 * set vol2Channel
	 * @param vol2Channel
	 */
	public void setVol2Channel(String vol2Channel) {
		this.vol2Channel = vol2Channel;
	}

	/**
	 * set vol3Channel
	 * @param vol3Channel
	 */
	public void setVol3Channel(String vol3Channel) {
		this.vol3Channel = vol3Channel;
	}

	/**
     * Accessor that sets the remap mode.
     *
     * @param  flag  <code>true</code> indicates remap data.
     */
    public void setRemapMode(boolean flag) {
        remapMode = flag;
    }
	
    /**
     * finalize
     */
	public void finalize() {
		if(channelImages != null) {
			for(int i=0;i<channelImages.length;i++) {
				channelImages[i].disposeLocal();
				channelImages[i] = null;
			}
		}
		 if (blank != null) {
			 blank.disposeLocal();
	         blank = null;
	     }
	
	}
	
	/** 
	 * window closing
	 */
	public void windowClosing(WindowEvent event) {
		finalize();
		dispose();
	}
	

}
