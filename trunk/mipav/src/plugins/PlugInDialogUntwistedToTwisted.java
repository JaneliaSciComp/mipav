
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
 ******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

 ******************************************************************
 ******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTPSpline;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistedToTwisted extends JDialogStandalonePlugin implements AlgorithmInterface
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5858343401705223389L;
//	private JRadioButton untwistedToTwisted;
//	private JRadioButton twistedToUntwisted;
//	private JRadioButton noseCentric;
//	private JRadioButton defaultCoords;
//	private ModelImage wormImage;

//	private String baseFileDir;
//	private JTextField  baseFileLocText;
//	private String baseFileName;
	private JTextField straightenedAnnotationText;
	private String straightenedAnnotationFile;
	private JTextField targetLocText;
	private String targetDir;
	

	private JTextField targetPointsText;
	private String targetPointsFile;
	private JTextField inputPointsText;
	private String inputPointsFile;
//	private JTextField  baseFileNameText;
//	private Vector<Integer> includeRange;
//	private JPanel inputsPanel;
//	private JTextField rangeFusionText;

	public PlugInDialogUntwistedToTwisted()
	{
		init();
		setVisible(true);
		addWindowListener(this);
	}

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 *
	 * @param  event  Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event)
	{
//		if ( event.getSource() == twistedToUntwisted && twistedToUntwisted.isSelected() )
//		{
//			defaultCoords.setSelected(true);
//			noseCentric.setSelected(false);
//			noseCentric.setEnabled(false);
//		}
//		if ( event.getSource() == untwistedToTwisted && untwistedToTwisted.isSelected() )
//		{
//			noseCentric.setEnabled(true);
//		}
		
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			if (setVariables()) {
				callAlgorithm();
				this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
			}
		} else if (command.equals("Script")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		}
	}

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}

	public void dispose()
	{
		super.dispose();
//		if ( wormImage != null )
//		{
//			wormImage.disposeLocal();
//			wormImage = null;
//		}		
	}

	public void saveSegmentationStatistics(final String dir, Vector<String> fileNamesList, Vector<Integer> numSegmented, Vector<Integer> numMatched )
	{

		final File fileDir = new File(dir);


		File file = new File(fileDir + File.separator + "segmentationSummary.csv");
		if (file.exists()) {
			file.delete();
			file = new File(fileDir + File.separator + "segmentationSummary.csv");
		}

		boolean noTest = false;
		int passCount = 0;
		int segCount = 0;
		try {
			for (int i = 0; i < fileNamesList.size(); i++)
			{
				if (numMatched.elementAt(i) >= 20)
				{
					passCount++;
				}
				if (numSegmented.elementAt(i) >= 20)
				{
					segCount++;
				}
				if (numMatched.elementAt(i) == -1)
				{
					noTest = true;
				}
			}
			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("Total Files" + "," + "Total Passed" + "\n");
			bw.write(fileNamesList.size() + "," + passCount + "\n");
			bw.write("file name" + "," + "number segmented" + "," + "number matched" + "," + "pass/fail" + "\n");
			for (int i = 0; i < fileNamesList.size(); i++) {
				bw.write(fileNamesList.elementAt(i) + "," + numSegmented.elementAt(i) + "," + numMatched.elementAt(i) + ","	+ (numMatched.elementAt(i) >= 20) + "," + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveSeamCellsTo");
			e.printStackTrace();
		}

	}

	@Override
	public void windowActivated(WindowEvent e) {}

	@Override
	public void windowClosed(WindowEvent e) {}

	public void windowClosing(final WindowEvent event)
	{
		if ( ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
				&& ViewUserInterface.getReference().isPlugInFrameVisible() )
		{
			System.exit(0);
		} else {
			dispose();
		}
	}

	@Override
	public void windowDeactivated(WindowEvent e) {}

	@Override
	public void windowDeiconified(WindowEvent e) {}

	@Override
	public void windowIconified(WindowEvent e) {}

	@Override
	public void windowOpened(WindowEvent e) {}

	private void callAlgorithm()
	{
		System.err.println( targetDir );
		System.err.println( straightenedAnnotationFile );
		
		if ( (targetDir.length() == 0) || (straightenedAnnotationFile.length() == 0) ) {
			return;
		}
		String targetDirName = targetDir.substring(targetDir.lastIndexOf( File.separator ) + 1, targetDir.length());
		System.err.println(targetDirName);

		String outputDirName = straightenedAnnotationFile.substring(0, straightenedAnnotationFile.lastIndexOf( File.separator ) + 1);
		String inputFileName = straightenedAnnotationFile.substring(straightenedAnnotationFile.lastIndexOf( File.separator ) + 1, straightenedAnnotationFile.lastIndexOf( '.' ));
		System.err.println(outputDirName);
		System.err.println(inputFileName);
		
		String toTwistedName = targetDir + File.separator + targetDirName + "_results" + File.separator + 
				"output_images" + File.separator + targetDirName + "_toTwisted.xml";
		File toTwistedFile = new File( toTwistedName );
		File annotationFile = new File( straightenedAnnotationFile );
		File targetFile = new File( targetPointsFile );
		File inputFile = new File( inputPointsFile );
		System.err.println(toTwistedName + " " + toTwistedFile.exists() );
		ModelImage toTwisted = null;
		if ( toTwistedFile.exists() && annotationFile.exists() && targetFile.exists() && inputFile.exists() )
		{
			System.err.println("opening image...");
			FileIO fileIO = new FileIO();
			toTwisted = fileIO.readImage(toTwistedName);  
			if ( toTwisted != null )
			{
				// read registration points and calculate thin-plate spline transform:
				VOI targetPts = LatticeModel.readAnnotationsCSV(targetPointsFile);
				VOI inputPts = LatticeModel.readAnnotationsCSV(inputPointsFile);
				int numTargetPts = targetPts.getCurves().size();
				int numInputPts = inputPts.getCurves().size();
				if ( numTargetPts != numInputPts ) {
					MipavUtil.displayError( "Number of registration target points and input points must match" );
					return;
				}
				double[] xSource = new double[ numTargetPts ]; 
				double[] ySource = new double[ numTargetPts ]; 
				double[] zSource = new double[ numTargetPts ]; 
				double[] xTarget = new double[ numTargetPts ]; 
				double[] yTarget = new double[ numTargetPts ]; 
				double[] zTarget = new double[ numTargetPts ]; 
				for ( int i = 0; i < numTargetPts; i++ ) {
					Vector3f sourcePt = inputPts.getCurves().elementAt(i).elementAt(0);
					Vector3f targetPt = targetPts.getCurves().elementAt(i).elementAt(0);
					xSource[i] = sourcePt.X;					ySource[i] = sourcePt.Y;					zSource[i] = sourcePt.Z;
					xTarget[i] = targetPt.X;					yTarget[i] = targetPt.Y;					zTarget[i] = targetPt.Z;
				}
				AlgorithmTPSpline spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTarget, yTarget, zTarget, 0.0f, toTwisted,
						toTwisted, true);

		        spline.setRunningInSeparateThread(false);
		        spline.run();

//				for ( int i = 0; i < numTargetPts; i++ ) {
//					Vector3f sourcePt = inputPts.getCurves().elementAt(i).elementAt(0);
//					Vector3f targetPt = targetPts.getCurves().elementAt(i).elementAt(0);
//					float[] transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
//					System.err.println( sourcePt + "   =>  " + targetPt + "    " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
//				}
		        
				
				int dimX = toTwisted.getExtents().length > 0 ? toTwisted.getExtents()[0] : 1;
				int dimY = toTwisted.getExtents().length > 1 ? toTwisted.getExtents()[1] : 1;
				int dimZ = toTwisted.getExtents().length > 2 ? toTwisted.getExtents()[2] : 1;
//				System.err.println("displaying image...");
//				new ViewJFrameImage(toTwisted);
				VOI annotations = LatticeModel.readAnnotationsCSV(straightenedAnnotationFile);
				if ( annotations != null )
				{
					if ( annotations.getCurves().size() > 0 )
					{
						Vector<String> failedList = new Vector<String>();
						VOI annotationVOI = new VOI( (short)0, inputFileName + "_" + targetDirName, VOI.ANNOTATION, 0 );
						for ( int j = 0; j < annotations.getCurves().size(); j++ )
						{
							VOIText text = (VOIText) annotations.getCurves().elementAt(j);
							Vector3f pos = text.elementAt(0);
							String name = text.getText();

							// do thin-plane spline registration on input:
							Vector3f transformedPt = new Vector3f(pos);
							if ( spline != null )
							{
								float[] temp = spline.getCorrespondingPoint(pos.X, pos.Y, pos.Z);
								transformedPt.X = temp[0];
								transformedPt.Y = temp[1];
								transformedPt.Z = temp[2];
							}
							
							if ( ((int)transformedPt.X >= 0) && ((int)transformedPt.X < dimX) && 
								 ((int)transformedPt.Y >= 0) && ((int)transformedPt.Y < dimY) &&
								 ((int)transformedPt.Z >= 0) && ((int)transformedPt.Z < dimZ)    )
							{
								float x = toTwisted.getFloatC( (int)transformedPt.X, (int)transformedPt.Y, (int)transformedPt.Z, 1 );
								float y = toTwisted.getFloatC( (int)transformedPt.X, (int)transformedPt.Y, (int)transformedPt.Z, 2 );
								float z = toTwisted.getFloatC( (int)transformedPt.X, (int)transformedPt.Y, (int)transformedPt.Z, 3 );
								Vector3f newPos = new Vector3f(x, y, z);

								if ( !newPos.equals( Vector3f.ZERO ) )
								{
									VOIText newText = new VOIText();
									newText.setText(name);
									newText.add(newPos);

									annotationVOI.getCurves().add(newText);
								}
								else
								{
									failedList.add(name);
								}
							}
							else
							{
								failedList.add(name);
							}
						}
						LatticeModel.saveAnnotationsAsCSV(outputDirName, inputFileName + "_" + targetDirName + "_retwist.csv", annotationVOI);
						String msg = "Retwisted " + annotationVOI.getCurves().size() + " out of " + annotations.getCurves().size() + " annotations" + "\n";
						for ( int j = 0; j < failedList.size(); j++ )
						{
							msg += failedList.elementAt(j) + " failed" + "\n";
						}
						MipavUtil.displayInfo( msg );
					}
				}
			}
			else
			{
				System.err.println("image open failed");
			}
		}
		
		
//		if ( includeRange != null )
//		{
//			System.err.println("Starting plugin" );
//			for ( int i = 0; i < includeRange.size(); i++ )
//			{				
//				// Build the full image name:
//				baseFileName = baseFileNameText.getText();
//				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
//				File imageFile = new File(baseFileDir + File.separator + fileName);
//
//				if ( imageFile.exists() )
//				{	
//					System.err.println( "   " + fileName );
//					FileIO fileIO = new FileIO();
//					if ( wormImage != null )
//					{
//						wormImage.disposeLocal(false);
//						wormImage = null;
//					}
//					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
//					WormData wormData = new WormData(wormImage);
//
////					String positionsFile = baseFileName + "_" + includeRange.elementAt(i) + "_positions.csv";
////					System.err.println( "   " + positionsFile );
////					File textFile = new File(baseFileDir + File.separator + "Positions" + File.separator + positionsFile);
////					if ( textFile.exists() )
////					{			
////						if ( twistedToUntwisted.isSelected() )
////						{
////							String latticeFile = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator +
////									baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 									
////									PlugInAlgorithmWormUntwisting.autoLatticeGenerationOutput + "1" + File.separator;
////							VOIVector latticeVector = new VOIVector();
////							PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, latticeFile, true, latticeVector, false);
////							if ( latticeVector.size() != 0 )
////							{
////								VOIVector vois = readMarkerPositions(positionsFile, textFile, null);
////								VOI annotations = vois.elementAt(0);
////
////								if ( annotations != null )
////								{
////									if ( annotations.getCurves().size() > 0 )
////									{
////										LatticeModel model = new LatticeModel(wormImage);
////										model.setSeamCellImage( wormData.readSeamSegmentation() );
////										model.setLattice(latticeVector.elementAt(0));
////										model.setMarkers(annotations);
////
////										model.interpolateLattice( false, false, false, true );	
////
////										VOI annotationsStraight = model.getAnnotationsStraight();
////
////
////										String distanceName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
////												baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
////												"output_images" + File.separator + 
////												baseFileName + "_" + includeRange.elementAt(i) + "_distanceMap.xml";
////										File distanceMapFile = new File(baseFileDir + File.separator + distanceName );
//////										System.err.println(distanceName + " " + distanceMapFile.exists() );											
////										ModelImage distanceMap = null;
////										if ( distanceMapFile.exists() )
////										{
////											distanceMap = fileIO.readImage(distanceName, baseFileDir + File.separator, false, null);
////										}
////										Vector<Integer> distance = null;
////										if ( distanceMap != null )
////										{
////											distance = new Vector<Integer>();
////											for ( int j = 0; j < annotationsStraight.getCurves().size(); j++ )
////											{
////												VOIText text = (VOIText) annotationsStraight.getCurves().elementAt(j);
////												Vector3f pos = text.elementAt(0);
////												int d = (distanceMap == null) ? 0 : distanceMap.getInt( (int)pos.X, (int)pos.Y, (int)pos.Z );
////												distance.add(d);
////											}
////											
////											distanceMap.disposeLocal(false);
////											distanceMap = null;
////										}
////
////										String fileBase = baseFileName + "_" + includeRange.elementAt(i) + "_positions_straight.csv";
////										saveMarkerPositions(baseFileDir + File.separator + "Positions" + File.separator + fileBase, annotationsStraight, distance);
////										model.dispose();
////										model = null;
////									}
////								}
////							}
////						}
////						else if ( untwistedToTwisted.isSelected() )
////						{
//							String toTwistedName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
//									baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
//									"output_images" + File.separator + 
//									baseFileName + "_" + includeRange.elementAt(i) + "_toTwisted.xml";
//							File toTwistedFile = new File(baseFileDir + File.separator + toTwistedName );
////							System.err.println(toTwistedName + " " + toTwistedFile.exists() );
//							ModelImage toTwisted = null;
//							if ( toTwistedFile.exists() )
//							{
//								toTwisted = fileIO.readImage(toTwistedName, baseFileDir + File.separator, false, null);  
//							}
//							String distanceName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
//									baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
//									"output_images" + File.separator + 
//									baseFileName + "_" + includeRange.elementAt(i) + "_distanceMap.xml";
//							File distanceMapFile = new File(baseFileDir + File.separator + distanceName );
////							System.err.println(distanceName + " " + distanceMapFile.exists() );
//							ModelImage distanceMap = null;
//							if ( distanceMapFile.exists() )
//							{
//								distanceMap = fileIO.readImage(distanceName, baseFileDir + File.separator, false, null);  
//							}
//							if ( toTwisted != null )
//							{
//								String positionsFile = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
//										baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + "tracked_annotations" + File.separator + 
//										"tracked_annotations.csv";
//								File textFile = new File(baseFileDir + File.separator + positionsFile);
//										
//								int[] imageExtents = new int[]{toTwisted.getExtents()[0], toTwisted.getExtents()[1]};
//								VOIVector vois = readMarkerPositions(positionsFile, textFile, imageExtents);
//								VOI annotations = vois.elementAt(0);
//
//								if ( annotations != null )
//								{
//									if ( annotations.getCurves().size() > 0 )
//									{
//
//										VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
//										Vector<Integer> distance = new Vector<Integer>();
//										for ( int j = 0; j < annotations.getCurves().size(); j++ )
//										{
//											VOIText text = (VOIText) annotations.getCurves().elementAt(j);
//											Vector3f pos = text.elementAt(0);
//											String name = text.getText();
//
//											float valid = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 0 );
////											if ( valid != 1 )
////											{
////												System.err.println( name + " invalid position" );
////											}
//											float x = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 1 );
//											float y = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 2 );
//											float z = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 3 );
//											Vector3f newPos = new Vector3f(x, y, z);
//
//											VOIText newText = new VOIText();
//											newText.setText(name);
//											newText.add(newPos);
//
//											annotationVOI.getCurves().add(newText);
//
//
//											int d = (distanceMap == null) ? 0 : distanceMap.getInt( (int)pos.X, (int)pos.Y, (int)pos.Z );
//											distance.add(d);
//										}
//										
//										String outputFile = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
//												baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + "tracked_annotations" + File.separator + 
//												"tracked_annotations_twisted.csv";
//										saveMarkerPositions(baseFileDir + File.separator + outputFile, annotationVOI, distance);
//										
//										toTwisted.disposeLocal(false);
//										toTwisted = null;
//										if ( distanceMap != null )
//										{
//											distanceMap.disposeLocal(false);
//											distanceMap = null;
//										}
//									}
//								}
//							}
////						}
////					}
//				}
//				else
//				{
//					MipavUtil.displayError( "Error in reading image file " + fileName );
//				}
//
//			}
//			System.err.println("Done plugin" );
//		}
	}     




	/**
	 * Initializes the panels for a non-integrated display. 
	 */
	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - Untwisted to Twisted Conversion");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		GuiBuilder gui = new GuiBuilder(this);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;



		JPanel inputsPanel = new JPanel(new GridBagLayout());
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input - Untwisted Space"));
		inputsPanel.setForeground(Color.black);
		
		straightenedAnnotationText = gui.buildFileField("Annotations to retwist (csv): ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(straightenedAnnotationText.getParent(), gbc);
		gbc.gridy++;


		
		JPanel regPanel = new JPanel(new GridBagLayout());
		regPanel.setBorder(JDialogBase.buildTitledBorder("Registration - Untwisted Space"));
		regPanel.setForeground(Color.black);

		targetPointsText = gui.buildFileField("Target points - register to this space (csv): ", "", false, JFileChooser.FILES_ONLY, this);
		regPanel.add(targetPointsText.getParent(), gbc);
		gbc.gridy++;

		inputPointsText = gui.buildFileField("Input points - transform this space to target (csv): ", "", false, JFileChooser.FILES_ONLY, this);
		regPanel.add(inputPointsText.getParent(), gbc);
		gbc.gridy++;



		JPanel dirPanel = new JPanel(new GridBagLayout());
		dirPanel.setBorder(JDialogBase.buildTitledBorder("Directory Information"));
		dirPanel.setForeground(Color.black);
		
		targetLocText = gui.buildFileField("Target directory: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		dirPanel.add(targetLocText.getParent(), gbc);
		gbc.gridy++;

//		baseFileNameText = gui.buildField("Base images name: ", "Decon");
//		inputsPanel.add(baseFileNameText.getParent(), gbc);
//		gbc.gridy++;
//
//		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", "             ");
//		inputsPanel.add(rangeFusionText.getParent(), gbc);
//		gbc.gridy++;

//		ButtonGroup group = new ButtonGroup();
//		untwistedToTwisted = gui.buildRadioButton( "untwisted->twisted", true);
//		untwistedToTwisted.addActionListener(this);
//		inputsPanel.add(untwistedToTwisted.getParent(), gbc);
//		group.add(untwistedToTwisted);
//		gbc.gridy++;
//		twistedToUntwisted = gui.buildRadioButton( "twisted->untwisted", false);
//		twistedToUntwisted.addActionListener(this);
//		inputsPanel.add(twistedToUntwisted.getParent(), gbc);
//		group.add(twistedToUntwisted);
//
//
//		gbc.gridy++;
//		group = new ButtonGroup();
//		defaultCoords = gui.buildRadioButton("default coordinates", true);
//		inputsPanel.add(defaultCoords.getParent(), gbc);
//		group.add(defaultCoords);
//		gbc.gridy++;
//		noseCentric = gui.buildRadioButton("nose centered", false);
//		inputsPanel.add(noseCentric.getParent(), gbc);
//		group.add(noseCentric);
//		gbc.gridy++;

		JPanel panel = new JPanel(new BorderLayout());
		panel.add(inputsPanel, BorderLayout.NORTH);
		panel.add(regPanel, BorderLayout.CENTER);
		panel.add(dirPanel, BorderLayout.SOUTH);
		
		getContentPane().add(panel, BorderLayout.NORTH);

		JPanel okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);


		pack();
		setResizable(true);

		System.gc();
	}



	private VOIVector readMarkerPositions( String fileName, File file, int[] imageExtents )
	{
		VOIVector vois = new VOIVector();
		VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
		vois.add(annotationVOI);
		FileReader fr;
		try {
			fr = new FileReader(file);
			BufferedReader br = new BufferedReader(fr);
			String line = br.readLine(); // first line is header
			line = br.readLine();
			while ( line != null )
			{
				Vector3f pos = new Vector3f();
				VOIText text = new VOIText();
				StringTokenizer st = new StringTokenizer(line, ",");
				if (st.hasMoreTokens()) {
					String name = st.nextToken();
					text.setText(name);
				}
				if (st.hasMoreTokens()) {
					pos.X = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					pos.Y = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					pos.Z = Float.valueOf(st.nextToken());
					Vector3f out = new Vector3f();
//					if ( noseCentric.isSelected() && (imageExtents != null) )
//					{
//						pos.X += imageExtents[0]/2;
//						pos.Y += imageExtents[1]/2;
//					}
					out.copy(pos);
					text.add(out);

					annotationVOI.getCurves().add(text);
					//					System.err.println( text.getText() + " " + text.elementAt(0) );
				}
				line = br.readLine();
			}

			fr.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return vois;
	}


	private void saveMarkerPositions( String fileName, VOI annotations, Vector<Integer> distanceList )
	{
		FileWriter fw;
		File file = new File(fileName);
		if ( file.exists() )
		{
			file.delete();
		}
		try {
			fw = new FileWriter(file);
			BufferedWriter bw = new BufferedWriter(fw);
			//			bw.write( "Name,x,y,z\n");
			bw.write( "Name,x,y,z,radius,spread\n");
			for ( int i = 0; i < annotations.getCurves().size(); i++ )
			{
				VOIText text = (VOIText) annotations.getCurves().elementAt(i);
				Vector3f pos = text.elementAt(0);
				Vector3f out = new Vector3f(pos);
				int distance = distanceList != null ? distanceList.elementAt(i) : 0;
				bw.write( text.getText() + "," + out.X + "," + out.Y + "," + out.Z + ",1," + distance + "\n" );
				//				bw.write( text.getText() + "," + out.X + "," + out.Y + "," + out.Z + "\n" );
				//				System.err.println( text.getText() + " " + out + "    " + spread);
			}

			bw.newLine();
			bw.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	/**
	 * This method could ensure everything in your dialog box has been set correctly
	 * 
	 * @return
	 */
	private boolean setVariables()
	{	    
		straightenedAnnotationFile = straightenedAnnotationText.getText();
		targetDir = targetLocText.getText();
		
		targetPointsFile = targetPointsText.getText();
		inputPointsFile = inputPointsText.getText();
		
//		baseFileDir = baseFileLocText.getText();
//		includeRange = new Vector<Integer>();
//		String rangeFusion = rangeFusionText.getText();
//		if(rangeFusion != null) {  
//			String[] ranges = rangeFusion.split("[,;]");
//			for(int i=0; i<ranges.length; i++) {
//				String[] subset = ranges[i].split("-");
//				int lowerBound = -1, bound = -1;
//				for(int j=0; j<subset.length; j++) {
//					try {
//						bound = Integer.valueOf(subset[j].trim());
//						if(lowerBound == -1) {
//							lowerBound = bound;
//							includeRange.add(lowerBound);
//						} 
//					} catch(NumberFormatException e) {
//						Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
//					}
//				}
//
//				for(int k=lowerBound+1; k<=bound; k++) {
//					includeRange.add(k);
//				}
//			}
//		}
//
//		if(includeRange.size() == 0) {
//			includeRange = null;
//		}

		return true;
	}
}
