package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;

public class JDialogProstateSaveFeatures2D extends JDialogBase
		implements AlgorithmInterface {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = 2408406330754526954L;

	/** Red channel. */
	private static final int RED_OFFSET = 1;

	/** Green channel. */
	private static final int GREEN_OFFSET = 2;

	/** Blue channel. */
	private static final int BLUE_OFFSET = 3;

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------
	// image intensity
		private boolean imageIntensityFilter = false;
		
		// Coherence Enhancing Diffusion Panel 
	    private boolean coherenceEnhancingDiffusionFilter = false;
	    
	    // Anisotropic Diffusion
	    private boolean regisotropicDiffusionFilter = false;
	    
	    // Inhomogeneity N3 correction.
	    private boolean IHN3CorrectionFilter = false;
	    
	    // mode
	    private boolean modeFilter = false;
	    
	    // mean
	    private boolean meanFilter = false;
	    
	    // median
	    private boolean medianFilter = false;
	    
	    private boolean invertFilter = false;
	    
	    private boolean haralickFilter = true;
	    
	    private boolean gaborFilter = false;
	    
	    private boolean hurstFilter = true;
	    
	    private boolean waveletFilter = false;
	    
	    private boolean gaussianFilter = false;
	    
	    private boolean gmFilter = false;
	    
	    private int numberFeatures = 0;

		private int tracingSliceNumber =6;
		
		private int haralickFeatureNumber = 4;
		
	
		private boolean distanceFilter = false;

		private ModelImage resultImage;
		private ModelImage classificationImage;
		
		
	private JTextField textSavedFileName;

	private JButton buttonSavedFileName;

	private String savedFileDirAbs;
	private String savedFileName;

	private int haralickImagesNumber = 0;
	
	private int imageOriginNumber = 0; 

    private ModelImage image;	
    private boolean testSample;
    private int waveletFeatureNumber = 1; 
    
    private AlgorithmProstateFeatures2D textureAlgo;

    private JPanel savedFilePanel;
    
    // ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Empty constructor needed for dynamic instantiation.
	 */
	public JDialogProstateSaveFeatures2D() {
	}

	/**
	 * Creates a new JDialogHaralickTexture object.
	 * 
	 * @param theParentFrame
	 *            Parent frame.
	 * @param im
	 *            Source image.
	 */
	public JDialogProstateSaveFeatures2D(Frame theParentFrame, ModelImage im, boolean testSample) {
		super(theParentFrame, false);
		image = im;
		this.testSample = testSample;
		init();
		calculateNumberFeatures();
		setVisible(true);
	}

	private void calculateNumberFeatures() {
		if ( imageIntensityFilter == true ) {
			numberFeatures++;
		}
		if ( coherenceEnhancingDiffusionFilter == true ) {
			numberFeatures++;
		}
	    if ( regisotropicDiffusionFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( IHN3CorrectionFilter ==  true ) { 
	    	numberFeatures++;
	    }
	    if ( modeFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( meanFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( medianFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( invertFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( gaborFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( hurstFilter == true ) {
	    	numberFeatures++;
	    }
	    
	    if ( waveletFilter == true ) {
	    	numberFeatures += waveletFeatureNumber;
	    }
	    
	    if ( gaussianFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( gmFilter == true ) {
	    	numberFeatures++;
	    }
	    if ( haralickFilter == true ) {
	    	numberFeatures += haralickFeatureNumber;
	    }
	    
	}
	
	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 * 
	 * @param event
	 *            Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

		if (command.equals("OK")) {
				callAlgorithm();
				setVisible(false);
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			//MipavUtil.showHelp("Haral1001");
			MipavUtil.showWebHelp("Filters_(Spatial):_Haralick_Texture");
		} else if (command.equalsIgnoreCase("saveFileBrowse")) {
			JFileChooser chooser = new JFileChooser();

			chooser.setDialogTitle("Choose Saved Features File Name");
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				savedFileDirAbs = chooser.getSelectedFile().getAbsolutePath();
				savedFileName = chooser.getSelectedFile().getName();
				// FileIO fileIO = new FileIO();
				// fileIO.setQuiet(true);
				System.err.println("savedFileDir = " + savedFileDirAbs);
				System.err.println("savedFileName = " + savedFileName);
				textSavedFileName.setText(savedFileDirAbs);

			}
		} 
	}

	// ************************************************************************
	// ************************** Algorithm Events ****************************
	// ************************************************************************

	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithm when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		int i;
	}

	public void saveFeatureSpaceValue(ModelImage resultImage, ModelImage classificationImage ) {
		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];
		int sliceSize = xDim * yDim;
		int i, z, k;
		int classify;
		int classNumber;
		int index = 0;
		float value;
		int zDim;
		int xIndex, yIndex;
		int numImages = numberFeatures;


		float[] resultBuffer = new float[sliceSize];
		float[] resultBufferClass = new float[sliceSize];

		Vector[] features = new Vector[numImages];

		for (i = 0; i < numImages; i++) {
			features[i] = new Vector();
		}

		if (image.getNDims() == 2) {
			zDim = 1;
		} else {
			zDim = image.getExtents()[2];
		}

		// boolean test = false;
		for (i = 0; i < numImages; i++) {
			// test = false;
			for (z = 0; z < zDim; z++) {
				try {
					// import and export ??????????????
					resultImage.exportData((i) * zDim * sliceSize + z
							* sliceSize, sliceSize, resultBuffer);
					classificationImage.exportData((i) * zDim * sliceSize + z
							* sliceSize, sliceSize, resultBufferClass);

					if (i == 0) {
						classificationImage.exportData((i + imageOriginNumber) * zDim * sliceSize + z * sliceSize, sliceSize, resultBufferClass);
					}

					for (k = 0; k < sliceSize; k++) {
						classify = (int) resultBufferClass[k];
						value = resultBuffer[k];
						index = k;
						// if ( classify == 1 /* && test == false */ ) {
						// System.err.println("classify = " + classify + " index
						// = " + index + " value = " + value);
						// if ( classify == 1 ) {
							features[i].add(new Feature(classify, index, value));
						// }
						// test = true;
						// }
					}

				} catch (IOException error) {
					MipavUtil
							.displayError("Temp: JDialogProstateSaveFeatures: IOException on destImage["
									+ i
									+ "].exportData(0,resultBuffer["
									+ i
									+ "],false)");

					return;
				}
			} // for (i = 0; i < resultNumber; i++)
		}

		// Save feature space into a file
		// savedFileDirAbs
		try {
			savedFileDirAbs = textSavedFileName.getText();
			
			File file = new File(savedFileDirAbs);
			PrintWriter output = new PrintWriter(file);
			Feature feature, featureTemp;
			int size, j;
			size = features[1].size();
			System.err.println("imageOriginNumber = " + imageOriginNumber);
			boolean printClassify = false;
			for (j = 0; j < size; j++) {
				printClassify = false;
				for (i = 0; i < features.length; i++) {
					feature = (Feature) features[i].get(j);
					classify = feature.classify;
					value = feature.value;
					index = feature.index;
					
					
					
					if (i == 0) {
						featureTemp = (Feature) features[i + imageOriginNumber].get(j);
						classify = featureTemp.classify;
					}
					if (printClassify == false) {
						if ( testSample == true ) {
							output.print(classify);
						} else {
							if (classify == 1) {
								output.print("+1");
							} else {
								output.print("-1");
							}
						}
						printClassify = true;
					}
					
					if ( printClassify == false ) {
	         			  output.print(classify);
	         			  printClassify = true;
	         		   }
					
					output.print(" " + (i+1) + ":" + value);
					
					
				}
				xIndex = index % yDim;
				yIndex = index / yDim;
				
				output.println();
			}
			output.close();
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	
		
	/**
	 * Once all the necessary variables are set, call the Gaussian Haralick
	 * feature algorithm.
	 */
	protected void callAlgorithm() {
		int i, j, k, index = 0;
		int resultNumber = 1;

		
		try {
			
				String[] name = new String[resultNumber];

				name[0] = makeImageName(image.getImageName(), "_Original");
				int[] newExtents = new int[3];
				newExtents[0] = image.getExtents()[0];
				newExtents[1] = image.getExtents()[1];
				newExtents[2] = numberFeatures;
				
				resultImage = new ModelImage(ModelStorageBase.FLOAT, newExtents, name[0]);
				classificationImage = new ModelImage(ModelStorageBase.FLOAT, newExtents, name[0]);		

				textureAlgo = new AlgorithmProstateFeatures2D(resultImage,
						classificationImage, image, imageIntensityFilter,
						coherenceEnhancingDiffusionFilter, 
						regisotropicDiffusionFilter,
						IHN3CorrectionFilter,
						modeFilter,
						meanFilter,
	                    medianFilter,
	                    invertFilter,
	                    haralickFilter,
	                    gaborFilter,
	                    hurstFilter, 
	                    waveletFilter,
	                    gaussianFilter,
	                    gmFilter,
						numberFeatures
						);
	
				textureAlgo.addListener(this);
				// createProgressBar(cropImage.getImageName(), textureAlgo);
	
				textureAlgo.run();
			
				// new ViewJFrameImage(resultImage);
				// new ViewJFrameImage(classificationImage);
				saveFeatureSpaceValue(resultImage, classificationImage); 
		
		
		} catch (OutOfMemoryError x) {
			System.gc();
			MipavUtil
					.displayError("Dialog Haralick Texture: unable to allocate enough memory");

			return;
		}

	}

	
	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	 */
	private void init() {

		setForeground(Color.black);

		setTitle("Prostate Features");
		
		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildSavedFilePanel();
		
		mainPanel.add(savedFilePanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);

		// setVisible( true );

		System.gc();
	}
	
	private void buildSavedFilePanel() {
		savedFilePanel = new JPanel();
		savedFilePanel.setBorder(buildTitledBorder("Saved Features File"));
		savedFilePanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel labelSavedFileName = WidgetFactory.buildLabel("File Name");
		textSavedFileName = WidgetFactory.buildTextField("");
		textSavedFileName.setColumns(10);

		buttonSavedFileName = new JButton("Browse");
		buttonSavedFileName.addActionListener(this);
		buttonSavedFileName.setActionCommand("saveFileBrowse");
		buttonSavedFileName.setMinimumSize(MipavUtil.defaultButtonSize);
		buttonSavedFileName.setPreferredSize(MipavUtil.defaultButtonSize);
		buttonSavedFileName.setFont(serif12B);

		gbc.gridx = 0;
		gbc.gridy = 0;
		savedFilePanel.add(labelSavedFileName, gbc);
		gbc.gridx = 1;
		savedFilePanel.add(textSavedFileName, gbc);
		gbc.gridx = 2;
		savedFilePanel.add(buttonSavedFileName, gbc);

	}

	
	

}


