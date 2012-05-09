package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Mathematics.Vector4f;


public class JPanelDTIPreprocessing extends JPanel implements AlgorithmInterface, ActionListener, ItemListener {

	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Use serialVersionUID for interoperability. */
	private static final long serialVersionUID = -4309868934393418962L;

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------

	private DTIPipeline pipeline;
	/** The B0 image from the original 4D DWI image series, extracted from the DWI volume: */
	private ModelImage imageB0 = null;
	/** The T2 image that the B0 image is registered to. This may be the original T2 if it matched the extents/resultions of the other image
	 * Or it is the resampled T2 image */
	private ModelImage imageT2 = null;
	/** After the B0 image is registered to the T2 image the B0 sub-volume is replaced in the original DWI image, 
	 * creating a new 4D DWI image: */
	private ModelImage imageUpdatedDWI = null;
	/** The transformation matrix describing the transformation from registering B0 to T2 image. 
	 * If that step in the process is skipped, this defaults to the identity matrix: */
	private TransMatrix b0toStructMatrix = new TransMatrix(4);
	/** File name for the B0 to T2 transformation matrix (needed to populate the next panel in the DTIPipeline) */
	private String b0MatrixFileName;

	// These are never initialized, yet they are used in init?
	private Font serif12;
	private Font serif12B;

	/** user-input for the B0 sub-volume number in the 4D DWI image */
	public JTextField refImageNumText;

	/** DOCUMENT ME! */
	private JComboBox comboBoxDOF;

	/** Progress bar that will listen to a dialog's algorithm (and reflect current progress)*/
	protected ViewJProgressBar progressBar;

	/** DOCUMENT ME! */
	private JComboBox comboBoxInterp;

	/** DOCUMENT ME! */
	private JComboBox comboBoxCostFunct;

	/** DOCUMENT ME! */
	private JCheckBox transformDWICheckbox;

	/** DOCUMENT ME! */
	public JComboBox matrixComboBox;

	/** DOCUMENT ME! */
	public JTextField matrixDirText;

	private JButton OKButton;

	private JPanel mainPanel;

	/** DOCUMENT ME! */
	private AlgorithmRegOAR3D reg3 = null;

	/** DOCUMENT ME! */
	private AlgorithmRegOAR35D reg35 = null;

	/** DOCUMENT ME! */
	private int cost, interp, DOF;

	/** DOCUMENT ME! */
	private int costT2, interpT2, DOFT2;

	private float rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
	rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

	private boolean maxOfMinResol = true, doSubsample = true, doMultiThread = true, fastMode = false;

	private boolean doGraph = false;

	private int maxIterations = 2, numMinima = 3;

	int registerTo = 3;

	public JCheckBox transformMatDWICheckbox;

	public JLabel transformB0label;

	public JCheckBox transformB0MatCheckbox;

	public JCheckBox transformB0Checkbox;

	public JLabel blanklabel;

	private JCheckBox skipPreCheckbox;

	public JCheckBox correctGradTransCheckbox;

	private JLabel labelDOF;

	private JLabel labelCost;

	private JLabel labelInternal;

	private JLabel labelInterp;

	public JPanel structOptPanel;

	/** output directory enables user to choose where to save files to. */
	private JTextField outputDir = new JTextField();
	/** action command when the user presses the browse button to select the output directory: */
	private String outputCommand = new String("browseOutput");

	public JPanelDTIPreprocessing(DTIPipeline pipeline) {
		super(new GridBagLayout());
		this.pipeline = pipeline;
		init();
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();

		// Run the entire within-volume 4D registration to the reference B0 image:
		if (command.equals("RUN OAR 3.5D")) {
			// First register the B0 volume to the structural T2 image:
			if (pipeline.T2Image != null) {
				// Extract the reference B0 image:
				imageB0 = B0extraction( pipeline.DWIImage, Integer.parseInt(refImageNumText.getText()) );
				if( imageB0!=null )
				{
					// Register B0 to T2 image:
					if(pipeline.T2Image.getExtents()!= imageB0.getExtents()){
						// Match the T2 image to the reference B0 image before the registration:
						imageT2 = resampleT2( pipeline.T2Image, imageB0 );                
						// callT2Algorithm runs the AlgorithmRegOAR3D in a separate thread. 
						// All further work must wait until the thread returns.
						// see algorithmPerformed
						callT2Algorithm(imageT2, imageB0);
					}
					else{             
						// T2 already matches, so use the one from the pipeline:
						imageT2 = pipeline.T2Image;
						// callT2Algorithm runs the AlgorithmRegOAR3D in a separate thread. 
						// All further work must wait until the thread returns.
						// see algorithmPerformed
						callT2Algorithm(imageT2, imageB0);
					}
				}
				else{
					MipavUtil.displayError("Error extracting B0 image");   
				}
			} 
			// There is no structural T2 image for the initial registration. Run the 4D registration:
			else {
				setVariablesForOAR35D();
				// callReg35Algorithm runs the AlgorithmRegOAR35D in a separate thread. 
				// All further work must wait until the thread returns.
				// see algorithmPerformed
				callReg35Algorithm( pipeline.DWIImage );
			}

		} 
		else if (command.equals("skipPre")) {
			if (skipPreCheckbox.isSelected()) {
				transformMatDWICheckbox.setEnabled(false);
				transformDWICheckbox.setEnabled(false);
				labelInternal.setEnabled(false);
				refImageNumText.setEnabled(false);              
				labelDOF.setEnabled(false); 
				comboBoxDOF.setEnabled(false);
				labelInterp.setEnabled(false);
				comboBoxInterp.setEnabled(false);
				labelCost.setEnabled(false);
				comboBoxCostFunct.setEnabled(false);
				OKButton.setEnabled(false);
				if (pipeline.T2Image != null){
					transformB0label.setEnabled(false);
					transformB0MatCheckbox.setEnabled(false);
					blanklabel.setEnabled(false);
					transformB0Checkbox.setEnabled(false);
				}
				pipeline.nextButton.setEnabled(true);
				pipeline.nextButton.setActionCommand("next2");
			}
			else{
				transformMatDWICheckbox.setEnabled(true);
				transformB0label.setEnabled(true);
				transformB0MatCheckbox.setEnabled(true);
				labelInternal.setEnabled(true);
				refImageNumText.setEnabled(true);              
				labelDOF.setEnabled(true);
				comboBoxDOF.setEnabled(true);
				labelInterp.setEnabled(true);
				comboBoxInterp.setEnabled(true);
				labelCost.setEnabled(true);
				comboBoxCostFunct.setEnabled(true);
				OKButton.setEnabled(true);

				pipeline.nextButton.setEnabled(false);
				pipeline.nextButton.setActionCommand("next2");
			}
		} 
		// The user pressed the button to choose a new output directory.
		// launch the directory chooser and set the value of the output directory text field:
		else if (command.equals(outputCommand) )
		{
			final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

			chooser.setDialogTitle("Choose dir");
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			final int returnValue = chooser.showOpenDialog(this);
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				outputDir.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);
			}			        	
		}
		// Show help:
		else if (command.equals("Help")) {
			MipavUtil.showHelp("OAR19076");
		}

	}
	
	/**
	 * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
	 * it has completed or failed to complete.
	 * 
	 * @param algorithm Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		final boolean pad = false;
		DecimalFormat nf;

		nf = new DecimalFormat();
		nf.setMaximumFractionDigits(4);
		nf.setMinimumFractionDigits(0);
		nf.setGroupingUsed(false);

		final DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
		dfs.setDecimalSeparator('.');
		nf.setDecimalFormatSymbols(dfs);

		// B0 to T2 Registration is complete. This section of code creates the transformed B0 image, 
		// saves it to file and displays it if the user chooses. The B0 data is exported from the new B0 image
		// and imported into a copy of the original DWI image. This works because the B0 image does not change
		// size during the registration to T2.
		if (algorithm instanceof AlgorithmRegOAR3D) {
			if (reg3.isCompleted()) {
				b0toStructMatrix = reg3.getTransform();
				
				// Create the transformed B0 image:
				final int xdimA = imageT2.getExtents()[0];
				final int ydimA = imageT2.getExtents()[1];
				final int zdimA = imageT2.getExtents()[2];
				final float xresA = imageT2.getFileInfo(0).getResolutions()[0];
				final float yresA = imageT2.getFileInfo(0).getResolutions()[1];
				final float zresA = imageT2.getFileInfo(0).getResolutions()[2];

				final String name = JDialogBase.makeImageName(imageB0.getImageName(), "_RegisteredB0toT2");

				AlgorithmTransform transform = new AlgorithmTransform(imageB0, b0toStructMatrix, 0, xresA, yresA, zresA, xdimA,
						ydimA, zdimA, true, false, pad);

				transform.setUpdateOriginFlag(true);
				transform.setFillValue(Float.valueOf("0.0"));
				transform.run();
				ModelImage resultB0toT2Image = transform.getTransformedImage();
				transform.finalize();

				resultB0toT2Image.calcMinMax();
				resultB0toT2Image.setImageName(name);
				resultB0toT2Image.setImageDirectory(outputDir.getText());
				// Save registered B0 to T2 result to the user-selected output directory (or default dir):
				ModelImage.saveImage( resultB0toT2Image, resultB0toT2Image.getImageName() + ".xml", outputDir.getText() );
				// Display registered B0 to T2 result if the user chooses:
				if (transformB0Checkbox.isSelected()) {
					if (resultB0toT2Image != null) {
						try {
							new ViewJFrameImage(resultB0toT2Image, null, new Dimension(610, 200));
						} catch (final OutOfMemoryError error) {
							MipavUtil.displayError("Out of memory: unable to open new frame");
						}
					}
				}

				// Create a new copy of the DWI image and insert the registered B0 image into the correct sub-volume:
				int xDim = pipeline.DWIImage.getExtents().length > 0 ? pipeline.DWIImage.getExtents()[0] : 1;
				int yDim = pipeline.DWIImage.getExtents().length > 1 ? pipeline.DWIImage.getExtents()[1] : 1;
				int zDim = pipeline.DWIImage.getExtents().length > 2 ? pipeline.DWIImage.getExtents()[2] : 1;
				int length = xDim*yDim*zDim;
				float[] buffer = new float[ length ];
				// Get the B0 sub-volume data:
				try {
					resultB0toT2Image.exportData( 0, buffer.length, buffer );
					// find the reference sub-volume number:
					int refVolumeNum = Integer.parseInt(refImageNumText.getText());
					// Clone the original DWI image:
					imageUpdatedDWI = (ModelImage)( pipeline.DWIImage.clone() );
					// Update the new image name:
					imageUpdatedDWI.setImageName( pipeline.DWIImage.getImageName() + "NewB0&DWIDataset" );
					// import the registered B0 sub-volume to replace the original B0 sub-volume:
					imageUpdatedDWI.importData( refVolumeNum * length, buffer, true );
				} catch (IOException e) {
					MipavUtil.displayError( "Sub-volume error on export/import: " + Integer.parseInt(refImageNumText.getText()) );
				}

				if (transform != null) {
					transform.disposeLocal();
					transform = null;
				}

				// Update ModelImage matrix:
				if (resultB0toT2Image != null) {
					resultB0toT2Image.getMatrixHolder().replaceMatrices(imageT2.getMatrixHolder().getMatrices());

					for (int i = 0; i < resultB0toT2Image.getExtents()[2]; i++) {
						resultB0toT2Image.getFileInfo(i).setOrigin(imageT2.getFileInfo(i).getOrigin());
					}
				}

				b0toStructMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
				// Save the B0 to T2 transform matrix to the output directory:
				b0MatrixFileName = new String( outputDir.getText() + imageB0.getImageName() + "_To_"
						+ imageT2.getImageName() + ".mtx" );
				if (transformB0MatCheckbox.isSelected()) {
					String message = "Using cost function, " + "Correlation ration";
					message += ", the cost is " + Double.toString(reg3.getAnswer()) + ".\n";
					message += "Some registration settings: \n";
					message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
					message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
					message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
					message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
					message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
					message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
					b0toStructMatrix.saveMatrix(b0MatrixFileName, message);
					Preferences.debug("Saved " + b0MatrixFileName + "\n", Preferences.DEBUG_FILEIO);
				}

			}

			if (reg3 != null) {
				reg3.disposeLocal();
				reg3 = null;
			}			
			// NOW start the 4D within-volume registration:
			setVariablesForOAR35D();
			callReg35Algorithm( imageUpdatedDWI );   
		}
		
		// The AlgorithmRegOAR35D has completed. This algorithm computes the within-volume 4D registration of the
		// sub-volumes to the reference B0 volume. The code below saves the transform matrices from the 4D registration (one per sub-volume)
		// Updates the gradient values of the DTIParameters for the new 4D image.
		// Saves the new 4D image to file and displays the result if the user has selected that option.
		if (algorithm instanceof AlgorithmRegOAR35D) {
			// Save the transform matrices array:
			TransMatrix[] arrayTransMatrix = reg35.getArrayTransMatrix2();
			String arrayMatrixFileName = new String( outputDir.getText() + pipeline.DWIImage.getImageName() + "TransMats" + ".mtx");
			if (transformMatDWICheckbox.isSelected()) {
				//createArrayTransMatrixTXT(); 
				saveTransformMatrix( arrayMatrixFileName, arrayTransMatrix );
			}

			ModelImage result35RegImage = reg35.getTransformedImage();
			if (result35RegImage != null) {
				result35RegImage.calcMinMax();
				result35RegImage.setImageName(pipeline.DWIImage.getImageName() + comboBoxDOF.getSelectedItem());
				result35RegImage.setDTIParameters( new DTIParameters( pipeline.DWIImage.getDTIParameters() ) );
				// Update the gradients in the new 4D image:
				if ( correctGradTransCheckbox.isSelected() && correctGradTransCheckbox.isEnabled() )
				{
					if ( (result35RegImage.getDTIParameters().getGradients() != null) && 
							(result35RegImage.getDTIParameters().getGradients().length == arrayTransMatrix.length) )
					{
						for ( int i = 0; i < arrayTransMatrix.length; i++ )
						{
							//System.err.println( arrayTransMatrix[i] );

							//System.err.print( result35RegImage.getDTIParameters().getGradients()[i][0] + " " + 
							//		result35RegImage.getDTIParameters().getGradients()[i][1] + " " + 
							//		result35RegImage.getDTIParameters().getGradients()[i][2] + "      --->     ");

							float[] grad = result35RegImage.getDTIParameters().getGradients()[i];
							Vector4f gradVec = new Vector4f( grad[0], grad[1], grad[2], 0 );
							Vector4f newGradVec = new Vector4f();
							arrayTransMatrix[i].Mult( gradVec, newGradVec );
							grad[0] = newGradVec.X;
							grad[1] = newGradVec.Y;
							grad[2] = newGradVec.Z;

							//System.err.println( result35RegImage.getDTIParameters().getGradients()[i][0] + " " + 
							//		result35RegImage.getDTIParameters().getGradients()[i][1] + " " + 
							//		result35RegImage.getDTIParameters().getGradients()[i][2] );
						}
					}
				}
				// Save the final result:
				result35RegImage.setImageDirectory(outputDir.getText());
				ModelImage.saveImage( result35RegImage, result35RegImage.getImageName() + ".xml", outputDir.getText() );

				// Display the transformed, motion-corrected within-volume 4D registered image if the user chooses:
				if (transformDWICheckbox.isSelected()) {
					try {
						new ViewJFrameImage(result35RegImage, null, new Dimension(610, 200));

					} catch (final OutOfMemoryError error) {
						MipavUtil.displayError("Out of memory: unable to open new frame");
					}
				}
				// Tell the parent DTIPipeline that the work is finished on this panel and update the pipeline variables:
				pipeline.nextButton.setEnabled(true);
				pipeline.nextButton.setActionCommand("next2");
				
				// The next step in the pipeline needs the following images:
				pipeline.finishPreProcessingPanel( result35RegImage, imageT2, 
						b0toStructMatrix, b0MatrixFileName, 
						arrayTransMatrix, arrayMatrixFileName );
			}		
		}
	}


	@Override
	public void itemStateChanged(ItemEvent arg0) {}



	/**
	 * This method creates the B-Value/Gradient file for DTI Tab
	 * 
	 * @return
    public void createArrayTransMatrixTXT() {

        try {
            File arrayMatFile = new File(matrixDirectory + pipeline.DWIImage.getImageName() + "TransMats" + ".mtx");
            FileOutputStream outputStream = new FileOutputStream(arrayMatFile);
            PrintStream printStream = new PrintStream(outputStream);
            String matrixString = "";

            for (int i = 0; i < arrayTransMatrix.length; i++) {
                printStream.print("TransMatrix" + " " + i + ":");
                printStream.println();
                for (int j = 0; j < 4; j++) {
                    matrixString = "\t" + Float.toString(arrayTransMatrix[i].Get(j, 0)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 1)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 2)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 3));
                    printStream.print(matrixString);
                    printStream.println();
                }
            }
            printStream.println();
            printStream.print("Using cost function, " + comboBoxCostFunct.getSelectedItem());
            printStream.print(", the cost is " + Double.toString(reg35.getAnswer()));
            printStream.println();
            printStream.print("Some registration settings: ");
            printStream.println();
            printStream.print("X Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a X coarse rate of " + 15.0 + " and X fine rate of " + 6.0);
            printStream.println();
            printStream.print("Y Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a Y coarse rate of " + 15.0 + " and Y fine rate of " + 6.0);
            printStream.println();
            printStream.print("Z Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a Z coarse rate of " + 15.0 + " and Z fine rate of " + 6.0);
            printStream.println();

        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of arrarTrans<atrix file failed....exiting algorithm \n",
                    Preferences.DEBUG_ALGORITHM);

        }
    }
	 */

	public void saveTransformMatrix(final String fileName, final TransMatrix[] matrix) {

		if (fileName == null) {
			return;
		}

		try {
			final File file = new File(fileName);
			final RandomAccessFile raFile = new RandomAccessFile(file, "rw");

			for ( int i = 0; i < matrix.length; i++ )
			{
				matrix[i].saveMatrix(raFile);
			}
			raFile.close();
		} catch (final IOException error) {
			MipavUtil.displayError("Matrix save error");

			return;
		}
	}

	/**
	 * Sets the default output directory for the images generated by this panel. 
	 * @param directory
	 */
	public void setOutputDirectory( String directory )
	{
		outputDir.setText( directory );
	}


	/**
	 * Calls the algorithm with the set-up parameters.
	 */
	protected void callReg35Algorithm( ModelImage dwiImage ) {
		cost = 1;
		float rotateBegin = (float) -30.0;
		float rotateEnd = (float) 30.0;
		float coarseRate = (float) 15.0;
		float fineRate = (float) 6.0;
		int refVolNum = Integer.parseInt(refImageNumText.getText());

		reg35 = new AlgorithmRegOAR35D(dwiImage, cost, DOF, interp, interp, registerTo, refVolNum, rotateBegin,
				rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode, maxIterations, numMinima);

		reg35.addListener(this);
		createProgressBar(dwiImage.getImageName(), reg35);
		reg35.run();
		// see algorithmPerformed for the results of the AlgorithmRegOAR35D
	}



	/**
	 * Calls the algorithm with the set-up parameters.
	 */
	protected void callT2Algorithm( ModelImage refT2Image, ModelImage matchB0Image ) {

		costT2 = 1;
		DOFT2 = 6;
		interpT2 = 0;
		float rotateBeginX = (float) -30.0;
		float rotateEndX = (float) 30.0;
		float coarseRateX = (float) 15.0;
		float fineRateX = (float) 6.0;
		float rotateBeginY = (float) -30.0;
		float rotateEndY = (float) 30.0;
		float coarseRateY = (float) 15.0;
		float fineRateY = (float) 6.0;
		float rotateBeginZ = (float) -30.0;
		float rotateEndZ = (float) 30.0;
		float coarseRateZ = (float) 15.0;
		float fineRateZ = (float) 6.0;

		reg3 = new AlgorithmRegOAR3D(refT2Image, matchB0Image, costT2, DOFT2, interpT2, rotateBeginX, rotateEndX,
				coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ,
				coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread, fastMode,
				maxIterations, numMinima);

		reg3.addListener(this);
		createProgressBar(refT2Image.getImageName(), reg3);
		reg3.run();
	}

	/**
	 * Creates the progress bar that will listen to an algorithm's progress changes
	 * @param title progress bar's title
	 * @param pListener algorithmbase that will notify progress updates to the pBar
	 */
	protected void createProgressBar(String title, AlgorithmBase pListener) {
		createProgressBar(title, " ...", pListener);
	}

	/**
	 * Creates the progress bar (should be created within JDialog's callAlgorithm method
	 * @param title progress bar's title
	 * @param msg the message to display on the progress bar (initial setting)
	 * @param pListener the algorithm that will register the progress bar as a listener
	 */
	protected void createProgressBar(String title, String msg, AlgorithmBase pListener) {
		progressBar = new ViewJProgressBar(title, msg, 0, 100, true);
		progressBar.setSeparateThread(false);
		pListener.addProgressChangeListener(progressBar);
		pListener.setProgressValues(0, 100);
	}


	/**
	 * Extracts the reference sub-volume from the input 4D image.
	 * @param image4D 4D image to extract the sub-volume from.
	 * @param refVolumeNum the reference sub-volume number.
	 * @return a new ModelImage containing the extracted sub-volume.
	 */
	private ModelImage B0extraction( ModelImage image4D, int refVolumeNum )
	{
		// Check that the input image is a 4D image:
		if ( image4D.getNDims() != 4 )
		{
			return null;
		}
		// Check that the reference volume number is a valid sub-volume.
		if ( (refVolumeNum < 0) || (refVolumeNum >= image4D.getExtents()[3]) )
		{
			return null;
		}
		int[] destB0Extents = new int[3];
		destB0Extents[0] = image4D.getExtents()[0];
		destB0Extents[1] = image4D.getExtents()[1];
		destB0Extents[2] = image4D.getExtents()[2];
		String resultB0String = image4D.getImageName() + "T=" + refImageNumText.getText();
		ModelImage resultB0Image = new ModelImage(image4D.getType(), destB0Extents, resultB0String);

		//B0 extraction from DWI dataset        
		if (resultB0Image != null)
		{
			System.err.println( refVolumeNum );
			AlgorithmSubset subsetAlgo = new AlgorithmSubset(image4D, resultB0Image, AlgorithmSubset.REMOVE_T, refVolumeNum);
			createProgressBar(image4D.getImageName(), subsetAlgo);
			subsetAlgo.run();

			if ( (subsetAlgo.isCompleted() == true) && (resultB0Image != null))
			{
				new ViewJFrameImage(resultB0Image);
				return resultB0Image;
			}
		}
		return null;
	}

	private JButton buildOKButton() {
		OKButton = new JButton("RUN OAR 3.5D");
		OKButton.addActionListener(this);
		OKButton.setFont(serif12B);
		return OKButton;
	}

	private TitledBorder buildTitledBorder(String title) {
		return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
				Color.black);
	}

	private TitledBorder highlightTitledBorder(String title){
		return new TitledBorder(new LineBorder( Color.black, 2), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
				Color.black);
	}

	/**
	 * Initializes the GUI components and displays the dialog.
	 */
	private void init() {
		setForeground(Color.black);



		structOptPanel = new JPanel();
		structOptPanel.setLayout(new GridBagLayout());
		structOptPanel.setBorder(buildTitledBorder("B0 to Structural Image OAR 3D Output Options"));
		transformB0Checkbox = new JCheckBox("Display Registered B0 to Structural Image");
		transformB0Checkbox.setFont(serif12);
		transformB0Checkbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		transformB0Checkbox.setForeground(Color.black);
		transformB0Checkbox.setSelected(true);
		transformB0Checkbox.addActionListener(this);
		transformB0Checkbox.setEnabled(false);

		transformB0MatCheckbox = new JCheckBox("Save Registered B0 to Structural Image Trans Matrix to directory");
		transformB0MatCheckbox.setFont(serif12);
		transformB0MatCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		transformB0MatCheckbox.setForeground(Color.black);
		transformB0MatCheckbox.setSelected(true);
		transformB0MatCheckbox.addActionListener(this);
		transformB0MatCheckbox.setEnabled(false);


		blanklabel = new JLabel("-------------------------------------------------------------------------------" +
				"----------------------------------------------------------------------------------------------------------" );

		blanklabel.setForeground(Color.black);
		blanklabel.setFont(serif12);
		blanklabel.setEnabled(false);

		transformB0label = new JLabel("Note: B0 to Structural Image OAR 3D rigid registration is performed " +
				"automatically when RUN OAR 3.5D button is selected");
		transformB0label.setForeground(Color.black);
		transformB0label.setFont(serif12);
		transformB0label.setEnabled(false);



		Insets insets = new Insets(0, 2, 0, 2);
		GridBagConstraints gbc = new GridBagConstraints();

		gbc.insets = insets;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.WEST;

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		structOptPanel.add(transformB0Checkbox, gbc);

		gbc.gridx = 0;
		gbc.gridy++;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		structOptPanel.add(transformB0MatCheckbox, gbc);

		gbc.gridx = 0;
		gbc.gridy++;
		gbc.weightx = 1;
		//gbc.fill = GridBagConstraints.HORIZONTAL;
		structOptPanel.add(blanklabel, gbc);

		gbc.gridx = 0;
		gbc.gridy++;
		gbc.weightx = 1;
		//gbc.fill = GridBagConstraints.HORIZONTAL;
		structOptPanel.add(transformB0label, gbc);




		JPanel optPanel = new JPanel();
		optPanel.setLayout(new GridBagLayout());
		optPanel.setBorder(buildTitledBorder("OAR 3.5D Input Options"));

		skipPreCheckbox = new JCheckBox("Skip Pre-Processing");
		skipPreCheckbox.setFont(serif12);
		skipPreCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		skipPreCheckbox.setForeground(Color.black);
		skipPreCheckbox.setActionCommand("skipPre");
		skipPreCheckbox.addActionListener(this);
		skipPreCheckbox.setSelected(false);


		labelInternal = new JLabel("Reference DWI Volume Number: ");
		labelInternal.setForeground(Color.black);
		labelInternal.setFont(serif12);

		refImageNumText = new JTextField("0", 2);
		refImageNumText.setEnabled(true);

		labelDOF = new JLabel("Degrees of freedom:");
		labelDOF.setForeground(Color.black);
		labelDOF.setFont(serif12);
		labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

		comboBoxDOF = new JComboBox();
		comboBoxDOF.setFont(MipavUtil.font12);
		comboBoxDOF.setBackground(Color.white);
		comboBoxDOF.setToolTipText("Degrees of freedom");
		comboBoxDOF.addItem("Motion Correction");
		comboBoxDOF.addItem("Motion Correction + Eddy Current");
		comboBoxDOF.setSelectedIndex(0);
		comboBoxDOF.addItemListener(this);

		labelInterp = new JLabel("Interpolation:");
		labelInterp.setForeground(Color.black);
		labelInterp.setFont(serif12);
		labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

		comboBoxInterp = new JComboBox();
		comboBoxInterp.setFont(MipavUtil.font12);
		comboBoxInterp.setBackground(Color.white);
		comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
		comboBoxInterp.addItem("Trilinear");
		comboBoxInterp.addItem("Bspline 3rd order");
		comboBoxInterp.addItem("Bspline 4th order");
		comboBoxInterp.addItem("Cubic Lagrangian");
		comboBoxInterp.addItem("Quintic Lagrangian");
		comboBoxInterp.addItem("Heptic Lagrangian");
		comboBoxInterp.addItem("Windowed sinc");
		comboBoxInterp.setSelectedIndex(0);
		comboBoxInterp.addItemListener(this);

		labelCost = new JLabel("Cost function:");
		labelCost.setForeground(Color.black);
		labelCost.setFont(serif12);
		labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

		comboBoxCostFunct = new JComboBox();
		comboBoxCostFunct.setFont(MipavUtil.font12);
		comboBoxCostFunct.setBackground(Color.white);
		comboBoxCostFunct.setAlignmentX(Component.LEFT_ALIGNMENT);
		comboBoxCostFunct.setToolTipText("Cost function");
		comboBoxCostFunct.addItem("Correlation ratio");
		comboBoxCostFunct.addItem("Least squares");
		comboBoxCostFunct.addItem("Normalized cross correlation");
		comboBoxCostFunct.addItem("Normalized mutual information");
		comboBoxCostFunct.setSelectedIndex(0);
		comboBoxCostFunct.addItemListener(this);

		gbc.insets = insets;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.WEST;

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		gbc.insets = new Insets(0, 2, 0, 2);
		gbc.fill = GridBagConstraints.REMAINDER;
		optPanel.add(skipPreCheckbox, gbc);

		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(0, 2, 0, 2);
		optPanel.add(labelInternal, gbc);

		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.weightx = 0.15;
		gbc.fill = GridBagConstraints.REMAINDER;
		optPanel.add(refImageNumText, gbc);

		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		optPanel.add(labelDOF, gbc);
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.weightx = 1;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		optPanel.add(comboBoxDOF, gbc);

		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		optPanel.add(labelInterp, gbc);
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.weightx = 1;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		optPanel.add(comboBoxInterp, gbc);

		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.weightx = 0;
		gbc.gridwidth = 1;
		optPanel.add(labelCost, gbc);
		gbc.gridx = 1;
		gbc.gridy = 4;
		gbc.weightx = 1;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		optPanel.add(comboBoxCostFunct, gbc);

		final JPanel outPanel = new JPanel();
		outPanel.setLayout(new GridBagLayout());
		outPanel.setBorder(buildTitledBorder("OAR 3.5D Output Options"));



		transformDWICheckbox = new JCheckBox("Display Transformed DWI Dataset Image");
		transformDWICheckbox.setFont(serif12);
		transformDWICheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		transformDWICheckbox.setForeground(Color.black);
		transformDWICheckbox.setSelected(true);
		transformDWICheckbox.addActionListener(this);

		transformMatDWICheckbox = new JCheckBox("Save Trans Matrices from Transformed DWI dataset to directory");
		transformMatDWICheckbox.setForeground(Color.black);
		transformMatDWICheckbox.setFont(serif12);
		transformMatDWICheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		transformMatDWICheckbox.setSelected(true);
		transformMatDWICheckbox.addActionListener(this);

		correctGradTransCheckbox = new JCheckBox("Correct Gradients after Transformation");
		correctGradTransCheckbox.setForeground(Color.black);
		correctGradTransCheckbox.setFont(serif12);
		correctGradTransCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
		correctGradTransCheckbox.setSelected(true);
		correctGradTransCheckbox.addActionListener(this);

		matrixComboBox = new JComboBox();
		matrixComboBox.setFont(serif12);
		matrixComboBox.setBackground(Color.white);
		matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);


		if (pipeline.T2Image != null) {
			matrixComboBox.addItem(pipeline.T2Image.getImageDirectory());
		}


		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.WEST;



		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		outPanel.add(transformDWICheckbox, gbc);

		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		outPanel.add(transformMatDWICheckbox, gbc);

		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		outPanel.add(correctGradTransCheckbox, gbc);



		buildOKButton();
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridBagLayout());
		buttonPanel.setBorder(JInterfaceBase.buildTitledBorder(""));
		gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;
		buttonPanel.add(OKButton, gbc);


		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = .5;
		gbc.weighty = 0;
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		mainPanel.add(optPanel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.weightx = .5;
		gbc.weighty = 0;
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		mainPanel.add(outPanel, gbc);
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.weightx = .5;
		gbc.weighty = 0;
		gbc.gridwidth = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		mainPanel.add(structOptPanel, gbc);


		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 1;
		gbc.weightx = 1;
		gbc.weighty = 1;
		mainPanel.add(buttonPanel, gbc);	

		JPanelEPIDistortionCorrection.buildLoadPanel( this, mainPanel, 
				new JLabel("Output dir:" ), outputDir, "Browse output directory", outputCommand );

		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.NONE;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel, gbc2);
		setVisible(true);

	}

	/**
	 * Resamples the input ModelImage to match the extents and resolutions of the second ModelImage.
	 * @param oldT2 input image that will be resampled
	 * @param matchB0 the image to match
	 * @return resampled version of the first input image.
	 */
	private ModelImage resampleT2( ModelImage oldT2, ModelImage matchB0 ) {

		//Parameters for algoTrans
		TransMatrix xfrm = new TransMatrix(4);
		xfrm.MakeIdentity();
		int[] units = matchB0.getUnitsOfMeasure();
		float fovX = oldT2.getResolutions(0)[0] * (oldT2.getExtents()[0] - 1);
		float fovY = oldT2.getResolutions(0)[1] * (oldT2.getExtents()[1] - 1);
		float fovZ = oldT2.getResolutions(0)[2] * (oldT2.getExtents()[2] - 1);
		int oXdim = Math.round(fovX / (matchB0.getResolutions(0)[0]) + 1);
		int oYdim = Math.round(fovY / (matchB0.getResolutions(0)[1]) + 1);
		int oZdim = Math.round(fovZ / (matchB0.getResolutions(0)[2]) + 1);

		//Sets T2 resolutions the same as the B0 volume
		AlgorithmTransform algoTrans = new AlgorithmTransform(oldT2, xfrm, 0, matchB0.getResolutions(0)[0], 
				matchB0.getResolutions(0)[1], matchB0.getResolutions(0)[2], oXdim, 
				oYdim, oZdim, units, false, false, false, false, null);
		algoTrans.addListener(this);

		algoTrans.run();


		int[] addCropXArr = new int[2];
		int[] addCropYArr = new int[2];
		int[] addCropZArr = new int[2];

		String resultT2String = oldT2.getImageName() + "Resample";
		// Match the extents of the new T2 image to the input matchB0 image:
		ModelImage matchT2 = new ModelImage(oldT2.getType(), matchB0.getExtents(), resultT2String);
		//Add or remove padding from T2 image
		float addCropX = (matchB0.getExtents()[0]-algoTrans.getTransformedImage().getExtents()[0])/2;
		float addCropY = (matchB0.getExtents()[1]-algoTrans.getTransformedImage().getExtents()[1])/2;
		float addCropZ = (matchB0.getExtents()[2]-algoTrans.getTransformedImage().getExtents()[2])/2;
		if (String.valueOf(addCropX/2).contains(".5")){
			addCropX = (float) (addCropX + 1.0);
			addCropXArr[0]= (int) (addCropX + .5); 
			addCropXArr[1]= (int) (addCropX - .5); 
		}
		else{
			addCropXArr[0]= (int) (addCropX); 
			addCropXArr[1]= (int) (addCropX); 
		}
		if (String.valueOf(addCropY/2).contains(".5")){
			addCropY = (float) (addCropY + 1.0);
			addCropYArr[0]= (int) (addCropY + .5); 
			addCropYArr[1]= (int) (addCropY - .5); 
		}
		else{
			addCropYArr[0]= (int) (addCropY); 
			addCropYArr[1]= (int) (addCropY); 
		}
		if (String.valueOf(addCropZ/2).contains(".5")){
			addCropZ = (float) (addCropZ + 1.0);
			addCropZArr[0]= (int) (addCropZ + .5); 
			addCropZArr[1]= (int) (addCropZ - .5); 
		}
		else{
			addCropZArr[0]= (int) (addCropZ); 
			addCropZArr[1]= (int) (addCropZ); 
		}

		AlgorithmAddMargins imageMarginsAlgo = new AlgorithmAddMargins(algoTrans.getTransformedImage(), matchT2, 
				addCropXArr, addCropYArr, addCropZArr);
		createProgressBar(oldT2.getImageName(), imageMarginsAlgo);
		imageMarginsAlgo.run();
		if((imageMarginsAlgo.isCompleted() == true) && matchT2!=null){
			new ViewJFrameImage(matchT2);
			matchT2.setImageDirectory(outputDir.getText());
			ModelImage.saveImage( matchT2, matchT2.getImageName() + ".xml", outputDir.getText() );
		}
		else
		{
			matchT2.disposeLocal();
			matchT2 = null;
			MipavUtil.displayError( "Unable to generated resampled image for " + oldT2.getImageName() + " to match " + matchB0.getImageName() );
		}
		// Clean up temporary image:
		algoTrans.getTransformedImage().disposeLocal(false);
		algoTrans.disposeLocal();
		algoTrans = null;
		imageMarginsAlgo = null;
		return matchT2;
	}

	/**
	 * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
	 * 
	 * @return <code>true</code> if the variables are properly set, <code>false</code> otherwise.
	 */
	private boolean setVariablesForOAR35D() {

		switch (comboBoxDOF.getSelectedIndex()) {

		case 0:
			DOF = 6;
			break;

		case 1:
			DOF = 12;
			break;

		default:
			DOF = 12;
			break;
		}

		switch (comboBoxInterp.getSelectedIndex()) {

		case 0:
			interp = AlgorithmTransform.TRILINEAR;
			break;

		case 1:
			interp = AlgorithmTransform.BSPLINE3;
			break;

		case 2:
			interp = AlgorithmTransform.BSPLINE4;
			break;

		case 3:
			interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
			break;

		case 4:
			interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
			break;

		case 5:
			interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
			break;

		case 6:
			interp = AlgorithmTransform.WSINC;
			break;

		default:
			interp = AlgorithmTransform.TRILINEAR;
			break;
		}

		switch (comboBoxCostFunct.getSelectedIndex()) {

		case 0:
			cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
			break;

		case 1:
			cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT;
			break;
			// case 2: cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT; break;

		case 2:
			cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT;
			break;

		case 3:
			cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT;
			break;

		default:
			cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
			break;
		}

		return true;
	}

}



