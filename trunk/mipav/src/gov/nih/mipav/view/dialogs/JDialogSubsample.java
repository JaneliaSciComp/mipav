package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Arrays;

import javax.swing.*;


/**
 * Dialog for subsampling a 2D or 3D or 4D image by 2, 4, or 8. With 4D images only the first 3 dimensions are
 * subsampled.
 *
 * @author   Sir Benjamin Link
 * @version  1.0
 */
public class JDialogSubsample extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3213495943646123969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmSubsample algoSub = null;

    /** DOCUMENT ME! */
    private JRadioButton by2Button = null; // subsample by 2

    /** DOCUMENT ME! */
    private JRadioButton by4Button = null; // subsample by 4

    /** DOCUMENT ME! */
    private JRadioButton by8Button = null; // subsample by 8
    
    /** User-Interface for displaying the original extent X */
	private JTextField textOriginalExtentX;

	/** User-Interface for displaying the original extent Y */
	private JTextField textOriginalExtentY;

	/** User-Interface for displaying the original extent Z */
	private JTextField textOriginalExtentZ;

	/** User-Interface for entering the expected extent X */
	private JTextField textExpectedExtentX;

	/** User-Interface for entering the expected extent Y */
	private JTextField textExpectedExtentY;

	/** User-Interface for entering the expected extent Z */
	private JTextField textExpectedExtentZ;

    /** DOCUMENT ME! */
    private int denom = 2; // denominator for subsampling

    /** DOCUMENT ME! */
    private boolean doVOI = false;

    /** DOCUMENT ME! */
    private ModelImage image = null; // sourceImage

    /** DOCUMENT ME! */
    private boolean lockZ = false;

    /** DOCUMENT ME! */
    private JCheckBox lockZBox = null;
    
    /** Number of dimensions in an image e.g 2D, 3D */
	private int dim;
	
	/** The boolean value to determine whether to pad the original image or not. */
	private boolean doPad;
    
    /** The extents of original image */
	private int[] extents = null;
	
	/** The extents of padded image */
	private int[] padExtents = null;
	
    /** DOCUMENT ME! */
    private int[] newExtents = null;

    /** DOCUMENT ME! */
    private float oXres, oYres, oZres;

    /** DOCUMENT ME! */
    private boolean processIndep = false; // use 2.5D?

    /** DOCUMENT ME! */
    private JCheckBox processIndepBox = null; // for processing 3D as 2.5D (slices remain unchanged)

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result Image

    /** DOCUMENT ME! */
    private float[] sigmas = null;

    /** DOCUMENT ME! */
    private float Sx, Sy, Sz;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JCheckBox voiCheckBox = null;

    /** DOCUMENT ME! */
    private TransMatrix xfrm = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSubsample() { }

    /**
     * Constructor for creating the dialog via a parent frame.
     *
     * @param  theParentFrame  the parent frame
     * @param  sourceImage     the source image
     */
    public JDialogSubsample(Frame theParentFrame, ModelImage sourceImage) {
        super(theParentFrame, false);
        this.image = sourceImage;
        this.userInterface = ViewUserInterface.getReference();
        extents = sourceImage.getExtents();
        dim = extents.length;
        padExtents = new int[dim];
        
        for (int i = 0; i < dim; i++) {
        	if (isDivisible(extents[i], denom)) {
        		padExtents[i] = extents[i];
        	} else {
        		padExtents[i] = extents[i] + 1;
        	}
        }
        
        if (Arrays.equals(padExtents, extents)) {
			doPad = false;
		} else {
			doPad = true;
		}
        
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Method for catching actions (button/script).
     *
     * @param  e  the action event
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10067");
        } else if (command.equals("Subsample by 2")) {
        	denom = 2;
        	for(int i = 0; i < dim; i++) {
        		if(isDivisible(extents[i], denom)) {
        			padExtents[i] = extents[i];
        		} else {
        			padExtents[i] = makeExtentDivisibleByDenom(extents[i], denom);
        		}
        	}
        	
        	textExpectedExtentX.setText(Integer.toString(padExtents[0]));
        	textExpectedExtentY.setText(Integer.toString(padExtents[1]));
            if (dim < 3) {
                textExpectedExtentZ.setText("1");
            }
            else {
        	    textExpectedExtentZ.setText(Integer.toString(padExtents[2]));
            }
        	
        	if (Arrays.equals(padExtents, extents)) {
    			doPad = false;
    		} else {
    			doPad = true;
    		}
        	
        	if (doPad) {
        		textExpectedExtentX.setEnabled(true);
            	textExpectedExtentY.setEnabled(true);
            	textExpectedExtentZ.setEnabled(true);
        	} else {
        		textExpectedExtentX.setEnabled(false);
            	textExpectedExtentY.setEnabled(false);
            	textExpectedExtentZ.setEnabled(false);
        	}
        	
        	
        } else if (command.equals("Subsample by 4")) {
        	denom = 4;
        	for(int i = 0; i < dim; i++) {
        		if(isDivisible(extents[i], denom)) {
        			padExtents[i] = extents[i];
        		} else {
        			padExtents[i] = makeExtentDivisibleByDenom(extents[i], denom);
        		}
        	}
        	
        	textExpectedExtentX.setText(Integer.toString(padExtents[0]));
        	textExpectedExtentY.setText(Integer.toString(padExtents[1]));
            if (dim < 3) {
                textExpectedExtentZ.setText("1");
            }
            else {
                textExpectedExtentZ.setText(Integer.toString(padExtents[2]));
            }
        	
        	if (Arrays.equals(padExtents, extents)) {
    			doPad = false;
    		} else {
    			doPad = true;
    		}
        	
        	if (doPad) {
        		textExpectedExtentX.setEnabled(true);
            	textExpectedExtentY.setEnabled(true);
            	textExpectedExtentZ.setEnabled(true);
        	} else {
        		textExpectedExtentX.setEnabled(false);
            	textExpectedExtentY.setEnabled(false);
            	textExpectedExtentZ.setEnabled(false);
        	}
        	        	
        } else if (command.equals("Subsample by 8")) {
        	denom = 8;
        	for(int i = 0; i < dim; i++) {
        		if(isDivisible(extents[i], denom)) {
        			padExtents[i] = extents[i];
        		} else {
        			padExtents[i] = makeExtentDivisibleByDenom(extents[i], denom);
        		}
        	}
        	
        	textExpectedExtentX.setText(Integer.toString(padExtents[0]));
        	textExpectedExtentY.setText(Integer.toString(padExtents[1]));
            if (dim < 3) {
                textExpectedExtentZ.setText("1");
            }
            else {
                textExpectedExtentZ.setText(Integer.toString(padExtents[2]));
            }
        	
        	if (Arrays.equals(padExtents, extents)) {
    			doPad = false;
    		} else {
    			doPad = true;
    		}
        	
        	if (doPad) {
        		textExpectedExtentX.setEnabled(true);
            	textExpectedExtentY.setEnabled(true);
            	textExpectedExtentZ.setEnabled(true);
        	} else {
        		textExpectedExtentX.setEnabled(false);
            	textExpectedExtentY.setEnabled(false);
            	textExpectedExtentZ.setEnabled(false);
        	}
        	        	
        }
    }

    /**
     * Method for catching end of algorithm events.
     *
     * @param  algo  the algorithm that is caught
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo instanceof AlgorithmSubsample) {

            if (algoSub.isCompleted()) {

                try {
                    new ViewJFrameImage(resultImage, null, userInterface.getNewFrameLocation());
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Subsample reports: out of memory; " + "unable to open a new frame");
                }

                insertScriptLine();
            }

            algoSub.finalize();
            algoSub = null;
        }
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
     * Respond to checkbox item events.
     *
     * @param  event  item event
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == processIndepBox) {
            lockZBox.setSelected(processIndepBox.isSelected());
            lockZBox.setEnabled(!processIndepBox.isSelected());

            if (processIndepBox.isSelected()) {

                if (voiCheckBox != null) {
                    voiCheckBox.setEnabled(false);
                    voiCheckBox.setSelected(false);
                }
            } else {

                if (voiCheckBox != null) {
                    voiCheckBox.setEnabled(true);
                }
            }
        }
        
    }

    /**
     * Accessor that tells whether VOIs are transformed.
     *
     * @param  doVOI  boolean
     */
    public void setDoVOI(boolean doVOI) {
        this.doVOI = doVOI;
    }

    /**
     * Accessor that sets whether slices are processed independently.
     *
     * @param  processIndep  DOCUMENT ME!
     */
    public void setProcessIndep(boolean processIndep) {
        this.processIndep = processIndep;
    }

    /**
     * Method for calling the Subsample algorithm.
     */
    protected void callAlgorithm() {
        setVisible(false);

        // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
        resultImage = new ModelImage(image.getType(), newExtents, image.getImageName() + "_subsample_" + denom);

        algoSub = new AlgorithmSubsample(image, resultImage, newExtents, padExtents, sigmas, processIndep, doVOI, xfrm, doPad);

        algoSub.addListener(this);

        createProgressBar(image.getImageName(), algoSub);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoSub.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoSub.run();
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        setProcessIndep(scriptParameters.doProcess3DAs25D());
        denom = scriptParameters.getParams().getInt("subsample_factor");
        setDoVOI(scriptParameters.getParams().getBoolean("do_transform_vois"));
        lockZ = scriptParameters.getParams().getBoolean("do_not_change_zdim");

        if (image.getNDims() == 2) {
            newExtents = new int[2];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            sigmas = new float[2];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
        } else if (image.getNDims() == 3) {
            newExtents = new int[3];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;

            if (lockZ) {
                newExtents[2] = image.getExtents()[2];
            } else {
                newExtents[2] = image.getExtents()[2] / denom;
            }

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        } else if (image.getNDims() == 4) {
            newExtents = new int[4];
            newExtents[0] = image.getExtents()[0] / denom;
            newExtents[1] = image.getExtents()[1] / denom;
            newExtents[2] = image.getExtents()[2] / denom;
            newExtents[3] = image.getExtents()[3];

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        }

        if (doVOI) {
            oXres = image.getFileInfo(0).getResolutions()[0] * denom;
            oYres = image.getFileInfo(0).getResolutions()[1] * denom;
            Sx = ((float) (newExtents[0]) * oXres) /
                     ((float) (image.getExtents()[0]) * image.getFileInfo(0).getResolutions()[0]);
            Sy = ((float) (newExtents[1]) * oYres) /
                     ((float) (image.getExtents()[1]) * image.getFileInfo(0).getResolutions()[1]);

            if (processIndep || (image.getNDims() == 2)) {
                xfrm = new TransMatrix(3);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy);
            } else {

                if (lockZ) {
                    oZres = image.getFileInfo(0).getResolutions()[2];
                    Sz = 1.0f;
                } else {
                    oZres = image.getFileInfo(0).getResolutions()[2] * denom;
                    Sz = ((float) (newExtents[2]) * oZres) /
                             ((float) (image.getExtents()[2]) * image.getFileInfo(0).getResolutions()[2]);
                }

                xfrm = new TransMatrix(4);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy, Sz);
            }
        } // if (doVOI)
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.storeProcess3DAs25D(processIndep);
        scriptParameters.getParams().put(ParameterFactory.newParameter("subsample_factor", denom));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_transform_vois", doVOI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_not_change_zdim", lockZ));
    }

    /**
     * Sets up the dialog window and makes it visible.
     */
    private void init() {

        setTitle("Subsample");

        ButtonGroup sampleSizeGroup = null;

        JPanel sampleSizePanel = new JPanel();

        sampleSizePanel.setLayout(new BoxLayout(sampleSizePanel, BoxLayout.Y_AXIS));
        sampleSizePanel.setBorder(buildTitledBorder("New Size"));
        sampleSizePanel.setForeground(Color.black);

        sampleSizeGroup = new ButtonGroup();

        by2Button = new JRadioButton("Subsample by 2", true);
        by2Button.addActionListener(this);
        by2Button.setFont(serif12);
        sampleSizeGroup.add(by2Button);
        sampleSizePanel.add(by2Button);

        by4Button = new JRadioButton("Subsample by 4", false);
        by4Button.addActionListener(this);
        by4Button.setFont(serif12);
        sampleSizeGroup.add(by4Button);
        sampleSizePanel.add(by4Button);

        by8Button = new JRadioButton("Subsample by 8", false);
        by8Button.addActionListener(this);
        by8Button.setFont(serif12);
        sampleSizeGroup.add(by8Button);
        sampleSizePanel.add(by8Button);

        if ((image.getVOIs() != null) && (!image.getVOIs().isEmpty()) && (image.getNDims() <= 3)) {
            voiCheckBox = new JCheckBox("Transform VOIs");
            voiCheckBox.setFont(serif12);
            voiCheckBox.setSelected(false);
            sampleSizePanel.add(voiCheckBox);
        }

        if (image.getNDims() == 3) {
            processIndepBox = new JCheckBox("Process each slice independently (2.5D)", false);
            processIndepBox.setFont(serif12);
            processIndepBox.addItemListener(this);
            sampleSizePanel.add(processIndepBox);

            lockZBox = new JCheckBox("Leave Z dimension unchanged", false);
            lockZBox.setFont(serif12);
            lockZBox.addItemListener(this);
            sampleSizePanel.add(lockZBox);
        }
        
        //Panel for Orginal Extents
		JPanel originalExtentsPanel = new JPanel(new GridBagLayout());

		originalExtentsPanel.setForeground(Color.black);
		originalExtentsPanel.setBorder(buildTitledBorder("Original Extents"));
        
        //Orginal Extent X
		JLabel labelOriginalExtentX = new JLabel("extent X: ");
		labelOriginalExtentX.setFont(serif12);

		textOriginalExtentX = new JTextField();
		textOriginalExtentX.setColumns(5);
		textOriginalExtentX.setMaximumSize(labelOriginalExtentX.getPreferredSize());
		textOriginalExtentX.setHorizontalAlignment(JTextField.RIGHT);
		textOriginalExtentX.setText(Integer.toString(extents[0]));
		textOriginalExtentX.setFont(serif12);
		textOriginalExtentX.setEnabled(false);

		// Orginal Extent Y
		JLabel labelOriginalExtentY = new JLabel("extent Y: ");
		labelOriginalExtentY.setFont(serif12);

		textOriginalExtentY = new JTextField();
		textOriginalExtentY.setColumns(5);
		textOriginalExtentY.setMaximumSize(labelOriginalExtentY.getPreferredSize());
		textOriginalExtentY.setHorizontalAlignment(JTextField.RIGHT);
		textOriginalExtentY.setText(Integer.toString(extents[1]));
		textOriginalExtentY.setFont(serif12);
		textOriginalExtentY.setEnabled(false);

		// Orginal Extent Z
		JLabel labelOriginalExtentZ = new JLabel("extent Z: ");
		labelOriginalExtentZ.setFont(serif12);

		textOriginalExtentZ = new JTextField();
		textOriginalExtentZ.setColumns(5);
		textOriginalExtentZ.setMaximumSize(labelOriginalExtentZ.getPreferredSize());
		textOriginalExtentZ.setHorizontalAlignment(JTextField.RIGHT);
		if (dim >= 3) {
			textOriginalExtentZ.setText(Integer.toString(extents[2]));	
		}
        else {
            textOriginalExtentZ.setText("1");
        }
        textOriginalExtentZ.setFont(serif12);
		textOriginalExtentZ.setEnabled(false);

		// Panel for Expected Extents
		JPanel expectedExtentsPanel = new JPanel(new GridBagLayout());

		expectedExtentsPanel.setForeground(Color.black);
		expectedExtentsPanel.setBorder(buildTitledBorder("Pad Extents"));

		// Expected Extent X
		JLabel labelExpectedExtentX = new JLabel("extent X: ");
		labelExpectedExtentX.setFont(serif12);

		textExpectedExtentX = new JTextField();
		textExpectedExtentX.setColumns(5);
		textExpectedExtentX.setMaximumSize(labelExpectedExtentX.getPreferredSize());
		textExpectedExtentX.setHorizontalAlignment(JTextField.RIGHT);
		textExpectedExtentX.setText(Integer.toString(padExtents[0]));
		textExpectedExtentX.setFont(serif12);
		if (!doPad) {
			textExpectedExtentX.setEnabled(false);
		}

		// Expected Extent Y
		JLabel labelExpectedExtentY = new JLabel("extent Y: ");
		labelExpectedExtentY.setFont(serif12);

		textExpectedExtentY = new JTextField();
		textExpectedExtentY.setColumns(5);
		textExpectedExtentY.setMaximumSize(labelExpectedExtentY.getPreferredSize());
		textExpectedExtentY.setHorizontalAlignment(JTextField.RIGHT);
		textExpectedExtentY.setText(Integer.toString(padExtents[1]));
		textExpectedExtentY.setFont(serif12);
		if (!doPad) {
			textExpectedExtentY.setEnabled(false);
		}

		// Expected Extent Z
		JLabel labelExpectedExtentZ = new JLabel("extent Z: ");
		labelExpectedExtentZ.setFont(serif12);

		textExpectedExtentZ = new JTextField();
		textExpectedExtentZ.setColumns(5);
		textExpectedExtentZ.setMaximumSize(labelExpectedExtentZ.getPreferredSize());
		textExpectedExtentZ.setHorizontalAlignment(JTextField.RIGHT);
        if (dim < 3) {
            textExpectedExtentZ.setText("1");
        }
        else {
		    textExpectedExtentZ.setText(Integer.toString(padExtents[2]));
        }
		textExpectedExtentZ.setFont(serif12);
		if (!doPad || dim < 3)  {
			textExpectedExtentZ.setEnabled(false);
		}
		
		GridBagConstraints gbc1 = new GridBagConstraints();
		
		gbc1.anchor = GridBagConstraints.WEST;
		gbc1.gridheight = 1;
		gbc1.gridwidth = 1;
		gbc1.insets = new Insets(3, 3, 3, 3);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentX, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentX, gbc1);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentY, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentY, gbc1);

		gbc1.gridy++;
		gbc1.gridx = 0;
		originalExtentsPanel.add(labelOriginalExtentZ, gbc1);
		gbc1.gridx = 1;
		originalExtentsPanel.add(textOriginalExtentZ, gbc1);

		gbc1.gridx = 0;
		gbc1.weightx = 1.0;
		gbc1.weighty = 1.0;
		gbc1.fill = GridBagConstraints.VERTICAL;

		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.anchor = GridBagConstraints.WEST;
		gbc2.gridheight = 1;
		gbc2.gridwidth = 1;
		gbc2.insets = new Insets(3, 3, 3, 3);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentX, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentX, gbc2);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentY, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentY, gbc2);

		gbc2.gridy++;
		gbc2.gridx = 2;
		expectedExtentsPanel.add(labelExpectedExtentZ, gbc2);
		gbc2.gridx = 3;
		expectedExtentsPanel.add(textExpectedExtentZ, gbc2);

		gbc2.gridx = 1;
		gbc2.weightx = 1.0;
		gbc2.weighty = 1.0;
		gbc2.fill = GridBagConstraints.VERTICAL;
        
		JPanel mainPanel = new JPanel(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridy = 0;
		
		mainPanel.add(sampleSizePanel, gbc);

		JPanel padPanel = new JPanel(new GridBagLayout());

		padPanel.setForeground(Color.black);
		padPanel.setBorder(buildTitledBorder("Pad Image"));

		gbc.gridy = 1;
		mainPanel.add(padPanel, gbc);

		padPanel.add(originalExtentsPanel, gbc1);
		padPanel.add(expectedExtentsPanel, gbc2);

		getContentPane().add(mainPanel);
		getContentPane().add(buildButtons(), BorderLayout.SOUTH);

		pack();
		setVisible(true);
		setResizable(false);
        
        
        

        //JPanel buttonPanel = new JPanel(new FlowLayout());

        // Make & set the OK (remove) and Cancel buttons--place outside the border
        /*
         * buildOKButton(); OKButton.setText("OK"); OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
         * buttonPanel.add(OKButton);
         *
         * buildCancelButton(); cancelButton.setPreferredSize(MipavUtil.defaultButtonSize); buttonPanel.add(cancelButton);
         */
        //buttonPanel.add(buildButtons());

        //JPanel panel = new JPanel(new BorderLayout());

        //panel.add(sampleSizePanel); // put the main panel into the center of the dialog
        //panel.add(buttonPanel, BorderLayout.SOUTH);
        //panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        //getContentPane().add(panel);
        //pack();
        //setResizable(false);
        //setVisible(true);
    }

    /**
     * Sets the variables based on the user's dialog input.
     *
     * @return  was everything ok (unnecessary because there is no possible illegal user input)
     */
    private boolean setVariables() {

        if (by4Button.isSelected()) {
            denom = 4;
            for(int i = 0; i < dim; i++) {
            	if(!isDivisible(padExtents[i], denom)) {
            		padExtents[i] = makeExtentDivisibleByDenom(padExtents[i], denom);
            	}
            }
        } else if (by8Button.isSelected()) {
            denom = 8;
            for(int i = 0; i < dim; i++) {
            	if(!isDivisible(padExtents[i], denom)) {
            		padExtents[i] = makeExtentDivisibleByDenom(padExtents[i], denom);
            	}
            }
        }

        if (processIndepBox != null) {
            processIndep = processIndepBox.isSelected();
        }

        if (lockZBox != null) {
            lockZ = lockZBox.isSelected();
        }

        if (image.getNDims() == 2) {
            newExtents = new int[2];
            //newExtents[0] = image.getExtents()[0] / denom;
            //newExtents[1] = image.getExtents()[1] / denom;
            
            newExtents[0] = padExtents[0] / denom;
            newExtents[1] = padExtents[1] / denom;

            sigmas = new float[2];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
        } else if (image.getNDims() == 3) {
            newExtents = new int[3];
            //newExtents[0] = image.getExtents()[0] / denom;
            //newExtents[1] = image.getExtents()[1] / denom;
            
            newExtents[0] = padExtents[0] / denom;
            newExtents[1] = padExtents[1] / denom;
            newExtents[2] = padExtents[2] / denom;

            if (lockZ) {
                newExtents[2] = image.getExtents()[2];
            } else {
                //newExtents[2] = image.getExtents()[2] / denom;
                newExtents[2] = padExtents[2] / denom;
            }

            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        } else if (image.getNDims() == 4) {
            newExtents = new int[4];
            //newExtents[0] = image.getExtents()[0] / denom;
            //newExtents[1] = image.getExtents()[1] / denom;
            //newExtents[2] = image.getExtents()[2] / denom;
            
            newExtents[0] = padExtents[0] / denom;
            newExtents[1] = padExtents[1] / denom;
            newExtents[2] = padExtents[2] / denom;
            newExtents[3] = image.getExtents()[3];
            
            sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * (image.getFileInfo(0).getResolutions()[0] / image.getFileInfo(0).getResolutions()[2]);
        }

        if (voiCheckBox != null) {
            doVOI = voiCheckBox.isSelected();
        } // if (voiCheckBox != null)

        if (doVOI) {
            oXres = image.getFileInfo(0).getResolutions()[0] * (float) (image.getExtents()[0]) /
                        (float) (newExtents[0]);
            oYres = image.getFileInfo(0).getResolutions()[1] * (float) (image.getExtents()[1]) /
                        (float) (newExtents[1]);

            // Sx = ( (float) (newExtents[0]) * oXres) /
            // ( (float) (image.getExtents()[0]) * image.getFileInfo(0).getResolutions()[0]);
            Sx = 1.0f;

            // Sy = ( (float) (newExtents[1]) * oYres) /
            // ( (float) (image.getExtents()[1]) * image.getFileInfo(0).getResolutions()[1]);
            Sy = 1.0f;

            if (processIndep || (image.getNDims() == 2)) {
                xfrm = new TransMatrix(3);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy);
            } else {

                if (lockZ) {
                    oZres = image.getFileInfo(0).getResolutions()[2];
                    Sz = 1.0f;
                } else {
                    oZres = image.getFileInfo(0).getResolutions()[2] * (float) (image.getExtents()[2]) /
                                (float) (newExtents[2]);

                    // Sz = ( (float) (newExtents[2]) * oZres) /
                    // ( (float) (image.getExtents()[2]) * image.getFileInfo(0).getResolutions()[2]);
                    Sz = 1.0f;
                }

                xfrm = new TransMatrix(4);
                xfrm.identity();
                xfrm.setZoom(Sx, Sy, Sz);
            }
        } // if (doVOI)

        return true;
    }
    
    /**
	 * Check if the extent value is divisible by 2, 4 or 8.
	 * 
	 * @param dimValue	dimension value
	 * 
	 * @return isPower2 true if dimValue is divisible 2, 4 or 8 false otherwise.
	 */

	private boolean isDivisible(int dimValue, int scale) {
		
		boolean flag = false;
		
		if ((dimValue % scale) == 0) {
			flag = true;
		}
		
		return flag;
	}
	
	/**
	 * Calculate the nearest extent value divisible by 2, 4 or 8
	 * 
	 * @param dimValue     extent value
	 * 
	 * @return newDimValue extent value divisible by 2, 4 or 8.
	 */
	
	private int makeExtentDivisibleByDenom (int dimValue, int scale) {
		
		int newDimValue = dimValue;
		int rem = 0;
		if (scale == 2) {
			newDimValue = dimValue + 1;
		} else {
			rem = dimValue % scale;
			newDimValue = dimValue + (scale - rem);
		}
		
		return newDimValue;
	}
	
    
}
