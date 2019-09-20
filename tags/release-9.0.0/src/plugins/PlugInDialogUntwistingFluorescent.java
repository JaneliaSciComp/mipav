
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
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModelEM;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.ModelImageLargeFormat;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistingFluorescent extends JDialogStandalonePlugin implements AlgorithmInterface
{

	private static final long serialVersionUID = -8451902403280342311L;
	private JCheckBox segmentSeamCheck;
	private JCheckBox readSeamCheck;
	private JCheckBox segmentSeamResultsCheck;
	private JCheckBox straightenImageCheck;
	private JCheckBox straightenMarkersCheck;
	private JRadioButton voxelCoords;
	private JRadioButton micronCoords;
	private JRadioButton segmentSkinSurface;
	private JRadioButton segmentLattice;
	private ModelImage wormImage;
	private ModelImage nucleiImage;


	public PlugInDialogUntwistingFluorescent()
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


	private void callAlgorithm()
	{
		maxParallelAngle = -Float.MAX_VALUE;
		maxBendAngle = -Float.MAX_VALUE;
		min10thWidth = Float.MAX_VALUE;
		max10thWidth = -Float.MAX_VALUE;

		totalLengthMin = Float.MAX_VALUE;
		totalLengthMax = -Float.MAX_VALUE;
		
		minMidLength = Float.MAX_VALUE;
		maxMidLength = -Float.MAX_VALUE;

		minLength = Float.MAX_VALUE;
		maxLength = -Float.MAX_VALUE;
		minLengthDiff = Float.MAX_VALUE;
		maxLengthDiff = -Float.MAX_VALUE;
		
		minWidth = Float.MAX_VALUE;
		maxWidth = -Float.MAX_VALUE;
		minPAngle = Float.MAX_VALUE;
		maxPAngle = -Float.MAX_VALUE;
		minTAngle = Float.MAX_VALUE;
		maxTAngle = -Float.MAX_VALUE;
		
		
		if ( includeRange != null )
		{
			System.err.println("Starting plugin" );
			Vector<String> fileNamesList = null;
			Vector<Integer> numSegmented = null;
			Vector<Integer> numMatched = null;
			
			int latticeMatches = 0;
			int latticeCount = 0;
			
			for ( int i = 0; i < includeRange.size(); i++ )
			{				
				// Build the full image name:
				baseFileName = baseFileNameText.getText();
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File imageFile = new File(baseFileDir + File.separator + fileName);

				if ( imageFile.exists() )
				{	
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
					WormData wormData = new WormData(wormImage); 


//					wormData.segmentSkin();
					
					
					// build the nuclei image file name 
					String baseFileName2 = baseFileNameText.getText();
					String fileName2 = baseFileName2 + "_" + includeRange.elementAt(i) + ".tif";
					File imageFile2 = new File(baseFileDir2 + File.separator + fileName);
					if ( imageFile2.exists() )
					{			
						System.err.println( "   " + fileName );
						FileIO fileIO2 = new FileIO();
						nucleiImage = fileIO2.readImage(fileName2, baseFileDir2 + File.separator, false, null); 
//						wormData.setNucleiImage(nucleiImage);
					}
					
					
					
					
					
					String latticeFileName = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoLatticeGenerationOutput + "1" + File.separator;
					VOIVector latticeVector = new VOIVector();
					File latticeFile = new File(latticeFileName);
					if ( latticeFile.exists() )
					{
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, latticeFileName, true, latticeVector, false);
					}

					
					ModelImage seamImage = null;
					Vector<Vector3f> seamCells = null;
					VOIVector autoLattice = null;
					if ( readSeamCheck.isSelected() || segmentSeamCheck.isSelected() )
					{
						if ( readSeamCheck.isSelected() )
						{
							seamImage = wormData.readSeamSegmentation();
							seamCells = wormData.readSeamCells();
						}
						if ( (seamCells == null) || (seamImage == null) || segmentSeamCheck.isSelected() )
						{
							// Create output directory for seam cell segmentation:
							String resultsDir = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + "_results";
							File outputFileDir = new File(resultsDir);
							if ( !outputFileDir.exists() )
							{
								outputFileDir.mkdir();
							}					
							String outputDir = resultsDir + File.separator + "output_images" + File.separator;
							outputFileDir = new File(outputDir);
							if ( !outputFileDir.exists() )
							{
								outputFileDir.mkdir();
							}

							// Segmented blurred input image:
							wormData.segmentSeamCells(minRadius, maxRadius);
							seamImage = wormData.getSeamSegmentation();
							seamCells = wormData.getSeamCells();

							String seamCellDir = resultsDir + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + File.separator;
							outputFileDir = new File(seamCellDir);
							if ( !outputFileDir.exists() )
							{
								outputFileDir.mkdir();
							}
							LatticeModel.saveAnnotationsAsCSV(seamCellDir, "seamCellInfo.csv", wormData.getSeamAnnotations());
						}
						if ( seamCells != null)
						{
							int numFound = -1;
							if ( latticeVector.size() != 0 )
							{
								numFound = testSeamLattice(wormImage, wormData, seamCells, latticeVector.elementAt(0), maxRadius);

								if ( fileNamesList == null )
								{
									fileNamesList = new Vector<String>();
									numSegmented = new Vector<Integer>();
									numMatched = new Vector<Integer>();
								}
								fileNamesList.add( baseFileName + "_"  + includeRange.elementAt(i) );
								numSegmented.add( seamCells.size() );
								numMatched.add(numFound);
							}
							
//							wormData.testLattice();
						}
					}
					
					if ( buildLatticeCheck.isSelected() )
					{
						if ( (seamImage == null) || (seamCells == null) )
						{
							wormData.segmentSeamCells(minRadius, maxRadius);
						}
						if ( useSkinMarkerCheck.isSelected() )
						{
							wormData.segmentSkin();
						}
						wormData.generateLattice();
						latticeCount++;
						autoLattice = wormData.getAutoLattice();
						if ( latticeBuildTestResults.isSelected() && (latticeVector.size() != 0) )
						{
							if ( autoLattice != null )
							{
								boolean match = false;
								int matchIndex = -1;
								for ( int v = 0; v < autoLattice.size(); v++ )
								{
									boolean newMatch = compareLattices( autoLattice.elementAt(v), latticeVector.elementAt(0));	
									if ( !match && newMatch )
									{
										matchIndex = v;
									}
									match |= newMatch;
								}
								System.err.println( fileName + " lattice match " + match + "   " + matchIndex );
								if ( match )
								{
									latticeMatches++;
								}
							}
							else
							{
								System.err.println( fileName + " no lattice match" );
							}
						}
					}
					if ( straightenImageCheck.isSelected() || straightenMarkersCheck.isSelected() )
					{
						LatticeModel model = new LatticeModel(wormImage);
						boolean hasLattice = false;
						model.setSeamCellImage(seamImage);
						if ( latticeVector.size() != 0 )
						{
							model.setLattice(latticeVector.elementAt(0));
							hasLattice = true;
						}
						else if ( (autoLattice != null) && (autoLattice.size() > 0) )
						{
							model.setLattice(autoLattice.elementAt(0));		
							hasLattice = true;					
						}
						else
						{
							MipavUtil.displayError( "Error in reading lattice file " + latticeFile );
						}
						if ( hasLattice )
						{
							String nucleiFileName = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + "marker" + File.separator + "marker.csv";
							File nucleiFile = new File(nucleiFileName);
							if ( nucleiFile.exists() )
							{
								VOIVector vois = readMarkerPositions( nucleiFileName, nucleiFile );
								VOI nucleiVOIs = vois.elementAt(0);
								if ( (nucleiVOIs != null) && (nucleiVOIs.getCurves().size() > 0) )
								{
									model.setMarkers(nucleiVOIs);
								}
							}

							model.interpolateLattice( false, false, straightenImageCheck.isSelected(), false );
							ModelImage contourImage = null;
							contourImage = model.segmentLattice(wormImage, false, 0, segmentLattice.isSelected());
							
							if ( straightenMarkersCheck.isSelected() )
							{
								model.interpolateLattice( false, false, false, true );								
							}

							if ( wormImage != null )
							{
								wormImage.unregisterAllVOIs();
								wormImage.disposeLocal(false);
							}

							if ( nucleiImage != null )
							{					
								nucleiImage.setImageName(baseFileName2 + "_" + includeRange.elementAt(i) + ".tif");
								model.setImage(nucleiImage);
								model.interpolateLattice( false, false, straightenImageCheck.isSelected(), false );

								model.segmentLattice(nucleiImage, contourImage);
								model.dispose();
								model = null;

								if ( nucleiImage != null )
								{
									nucleiImage.disposeLocal(false);
								}
							}						

							if ( contourImage != null )
							{
								contourImage.disposeLocal(false);
							}
						}
					}


					if ( wormImage != null )
					{
						wormImage.disposeLocal(false);
					}
					if ( wormData != null )
					{
						wormData.dispose();
					}
				}
				else
				{
					MipavUtil.displayError( "Error in reading image file " + fileName );
				}

			}
			if ( fileNamesList != null )
			{
				saveSegmentationStatistics(baseFileDir, fileNamesList, numSegmented, numMatched );
			}
			System.err.println( "maxBendAngle     " + maxBendAngle );
			System.err.println( "totalLengthMin   " + totalLengthMin*WormData.VoxelSize );
			System.err.println( "totalLengthMax   " + totalLengthMax*WormData.VoxelSize );
			System.err.println( "minMidLength     " + minMidLength*WormData.VoxelSize  );
			System.err.println( "maxMidLength     " + maxMidLength*WormData.VoxelSize );
			System.err.println( "minLength        " + minLength*WormData.VoxelSize );
			System.err.println( "maxLength        " + maxLength*WormData.VoxelSize );
			System.err.println( "minLengthDiff    " + minLengthDiff*WormData.VoxelSize );
			System.err.println( "maxLengthDiff    " + maxLengthDiff*WormData.VoxelSize );
			System.err.println( "minWidth         " + minWidth*WormData.VoxelSize );
			System.err.println( "maxWidth         " + maxWidth*WormData.VoxelSize );
			System.err.println( "min10thWidth     " + min10thWidth*WormData.VoxelSize );
			System.err.println( "max10thWidth     " + max10thWidth*WormData.VoxelSize );
			System.err.println( "minPAngle        " + minPAngle );
			System.err.println( "maxPAngle        " + maxPAngle );
			System.err.println( "minTAngle        " + minTAngle );
			System.err.println( "maxTangle        " + maxTAngle );

			
			
			System.err.println("Done plugin" );
			if ( latticeBuildTestResults.isSelected() && (latticeCount > 0) )
			{
				MipavUtil.displayInfo( "Found " + latticeMatches + " out of " + latticeCount + " tested" );
			}
		}
	}

	public void dispose()
	{
		super.dispose();
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}		
		//		if ( maskImage != null )
		//		{
		//			maskImage.disposeLocal();
		//			maskImage = null;
		//		}		
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


	private String baseFileDir;
	private String baseFileDir2;
	private JTextField  baseFileLocText;
	private JTextField  baseFileLocText2;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private int imageIndex = 0;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;
	private JTextField rangeFusionText;
	private JTextField seamCellMinRadiusText;
	private JTextField seamCellMaxRadiusText;
	private JTextField segmentationPaddingText;
	private int paddingFactor;
	private int minRadius;
	private int maxRadius;
	private JCheckBox buildLatticeCheck;
	private JCheckBox useSkinMarkerCheck;
	private JCheckBox latticeBuildTestResults;

	/**
	 * Initializes the panels for a non-integrated display. 
	 */
	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - Fluorescent images - 1.0");
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



		inputsPanel = new JPanel(new GridBagLayout());
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		inputsPanel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Data directory (marker 1): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileLocText2 = gui.buildFileField("Data directory (marker 2): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText2.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", "             ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;


		JPanel algorithmsPanel = new JPanel(new BorderLayout());

		// seam segmentation panel:
		JPanel segmentationPanel = new JPanel(new GridBagLayout());
		segmentationPanel.setBorder(JDialogBase.buildTitledBorder("Seam Cell Segmentation Options"));
		segmentSeamCheck = gui.buildCheckBox( "Segment seam cells", false );
		segmentationPanel.add(segmentSeamCheck.getParent(), gbc);
		gbc.gridy++;
		readSeamCheck = gui.buildCheckBox( "Read seam cell from file", true );
		segmentationPanel.add(readSeamCheck.getParent(), gbc);
		gbc.gridy++;

		segmentSeamResultsCheck = gui.buildCheckBox( "Display test results", false );
		segmentationPanel.add(segmentSeamResultsCheck.getParent(), gbc);
		gbc.gridy++;

		seamCellMinRadiusText = gui.buildField("Seam cell min radius (voxels): ", "           8");
		segmentationPanel.add(seamCellMinRadiusText.getParent(), gbc);
		gbc.gridy++;

		seamCellMaxRadiusText = gui.buildField("Seam cell max radius (voxels): ", "           25");
		segmentationPanel.add(seamCellMaxRadiusText.getParent(), gbc);
		gbc.gridy++;
		algorithmsPanel.add(segmentationPanel, BorderLayout.NORTH );

		
		// lattice build panel:
		JPanel latticeBuildPanel = new JPanel(new GridBagLayout());
		latticeBuildPanel.setBorder(JDialogBase.buildTitledBorder("Lattice Build Options"));
		buildLatticeCheck = gui.buildCheckBox( "Automatically Build Lattice", false);
		latticeBuildPanel.add( buildLatticeCheck.getParent(), gbc);
		gbc.gridy++;
		useSkinMarkerCheck = gui.buildCheckBox( "Use skin marker segmentation", false);
		latticeBuildPanel.add( useSkinMarkerCheck.getParent(), gbc);
		gbc.gridy++;		
		latticeBuildTestResults = gui.buildCheckBox( "Display lattice test results", false );
		latticeBuildPanel.add( latticeBuildTestResults.getParent(), gbc);
		gbc.gridy++;
		algorithmsPanel.add(latticeBuildPanel, BorderLayout.CENTER );
		
		
		// straighten panel:
		JPanel algorithmPanel = new JPanel(new GridBagLayout());
		algorithmPanel.setBorder(JDialogBase.buildTitledBorder("Straightening Options"));
		straightenImageCheck = gui.buildCheckBox( "Straighten Image", false );
		algorithmPanel.add(straightenImageCheck.getParent(), gbc);
		gbc.gridy++;

		ButtonGroup group1 = new ButtonGroup();
		segmentSkinSurface = gui.buildRadioButton( "Segment Skin Surface Marker", true );
		algorithmPanel.add(segmentSkinSurface.getParent(), gbc);
		gbc.gridx++;
		group1.add(segmentSkinSurface);

		segmentLattice = gui.buildRadioButton( "Segment Lattice", false );
		algorithmPanel.add(segmentLattice.getParent(), gbc);
		group1.add(segmentLattice);
		gbc.gridy++;
		gbc.gridx = 0;

		segmentationPaddingText = gui.buildField("Segmentation padding (voxels): ", "           0");
		algorithmPanel.add(segmentationPaddingText.getParent(), gbc);
		gbc.gridy++;

		straightenMarkersCheck = gui.buildCheckBox( "Straighten Markers", false );
		algorithmPanel.add(straightenMarkersCheck.getParent(), gbc);
		gbc.gridx++;
		ButtonGroup group = new ButtonGroup();
		voxelCoords = gui.buildRadioButton( "voxels", false);
		algorithmPanel.add(voxelCoords.getParent(), gbc);
		group.add(voxelCoords);
		gbc.gridx++;
		micronCoords = gui.buildRadioButton( "microns", true);
		algorithmPanel.add(micronCoords.getParent(), gbc);
		group.add(micronCoords);

		algorithmsPanel.add(algorithmPanel, BorderLayout.SOUTH );

		gbc.gridx = 0;
		gbc.gridy++;


		getContentPane().add(inputsPanel, BorderLayout.NORTH);
		getContentPane().add(algorithmsPanel, BorderLayout.CENTER);

		JPanel okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);


		pack();
		setResizable(true);

		System.gc();
	}


	/**
	 * This method could ensure everything in your dialog box has been set correctly
	 * 
	 * @return
	 */
	private boolean setVariables()
	{	    
		try {
			paddingFactor = Integer.valueOf(segmentationPaddingText.getText().trim());
		} catch(NumberFormatException e) {
			paddingFactor = 10;
		}
		try {
			minRadius = Integer.valueOf(seamCellMinRadiusText.getText().trim());
		} catch(NumberFormatException e) {
			minRadius = 8;
		}
		try {
			maxRadius = Integer.valueOf(seamCellMaxRadiusText.getText().trim());
		} catch(NumberFormatException e) {
			maxRadius = 25;
		}
		baseFileDir = baseFileLocText.getText();
		baseFileDir2 = baseFileLocText2.getText();
		includeRange = new Vector<Integer>();
		String rangeFusion = rangeFusionText.getText();
		if(rangeFusion != null) {  
			String[] ranges = rangeFusion.split("[,;]");
			for(int i=0; i<ranges.length; i++) {
				String[] subset = ranges[i].split("-");
				int lowerBound = -1, bound = -1;
				for(int j=0; j<subset.length; j++) {
					try {
						bound = Integer.valueOf(subset[j].trim());
						if(lowerBound == -1) {
							lowerBound = bound;
							includeRange.add(lowerBound);
						} 
					} catch(NumberFormatException e) {
						Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
					}
				}

				for(int k=lowerBound+1; k<=bound; k++) {
					includeRange.add(k);
				}
			}
		}

		if(includeRange.size() == 0) {
			includeRange = null;
		}

		return true;
	}     




	private VOIVector readMarkerPositions( String fileName, File file )
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
				float radius;
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
					//					System.err.print( text.getText() + " " + pos + "    " );
					Vector3f out = new Vector3f();
					if ( micronCoords.isSelected() )
					{
						MipavCoordinateSystems.scannerToFile(pos, out, wormImage);
					}
					else
					{
						out.copy(pos);
					}
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

	private float maxParallelAngle = -Float.MAX_VALUE;
	private float maxBendAngle = -Float.MAX_VALUE;
	private float min10thWidth = Float.MAX_VALUE;
	private float max10thWidth = -Float.MAX_VALUE;

	private float totalLengthMin = Float.MAX_VALUE;
	private float totalLengthMax = -Float.MAX_VALUE;
	
	private float minMidLength = Float.MAX_VALUE;
	private float maxMidLength = -Float.MAX_VALUE;

	private float minLength = Float.MAX_VALUE;
	private float maxLength = -Float.MAX_VALUE;
	private float minLengthDiff = Float.MAX_VALUE;
	private float maxLengthDiff = -Float.MAX_VALUE;
	
	private float minWidth = Float.MAX_VALUE;
	private float maxWidth = -Float.MAX_VALUE;
	private float minPAngle = Float.MAX_VALUE;
	private float maxPAngle = -Float.MAX_VALUE;
	private float minTAngle = Float.MAX_VALUE;
	private float maxTAngle = -Float.MAX_VALUE;
	private int testSeamLattice( ModelImage image, WormData wormData, Vector<Vector3f> seam, VOI lattice, float minDist )
	{

		int count = 0;
		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		boolean[] latticePt = new boolean[left.size()];
		VOIContour newLeft = new VOIContour(false);
		VOIContour newRight = new VOIContour(false);
		boolean tenthFound = false;
		for ( int i = 0; i < left.size(); i++ )
		{
			float minLD = Float.MAX_VALUE;
			Vector3f closestL = new Vector3f();
			float minRD = Float.MAX_VALUE;
			Vector3f closestR = new Vector3f();
			int closestIndexL = -1;
			int closestIndexR = -1;
			for ( int j = 0; j < seam.size(); j++ )
			{
				float distL = left.elementAt(i).distance(seam.elementAt(j));
				if ( distL < minLD )
				{
					minLD = distL;
					closestL.copy(seam.elementAt(j));
					closestIndexL = j;
				}
				float distR = right.elementAt(i).distance(seam.elementAt(j));
				if ( distR < minRD )
				{
					minRD = distR;
					closestR.copy(seam.elementAt(j));
					closestIndexR = j;
				}
			}
//			System.err.println( "lattice match " + minLD + "  " + minRD );
			if ( (minLD < minDist) && (minRD < minDist) && !closestL.equals(closestR) && !newLeft.contains(closestL) && !newRight.contains(closestR) )
			{
				// match both left and right add 2:
				count += 2;
				newLeft.add(closestL);
				newRight.add(closestR);
				

				if ( i == (left.size()-1))
				{
					tenthFound = true;
				}
			}
		}
		
		if ( !tenthFound )
		{
			System.err.println( "lattice match " + count + "    " + tenthFound  );
			return 0;
		}
		else
		{
			System.err.println( "lattice match " + count + "    " + tenthFound + "   " + wormData.testLattice(newLeft, newRight, true) );			
		}
		
		for ( int i = 0; i < newLeft.size(); i++ )
		{
			int x = (int) newLeft.elementAt(i).X;
			int y = (int) newLeft.elementAt(i).Y;
			int z = (int) newLeft.elementAt(i).Z;
			float valueL = image.getFloat(x,y,z);
			x = (int) newRight.elementAt(i).X;
			y = (int) newRight.elementAt(i).Y;
			z = (int) newRight.elementAt(i).Z;
			float valueR = image.getFloat(x,y,z);
//			System.err.println( i + "   " + valueL + "   " + valueR );
		}
		

		float totalLengthL = 0;
		float totalLengthR = 0;
		float totalPAngle = 0;
		float totalBAngle = 0;
		for ( int i = 0; i < newLeft.size(); i++ )
		{
			if ( i > 0 )
			{
				Vector3f dirL = Vector3f.sub(newLeft.elementAt(i), newLeft.elementAt(i-1));
				Vector3f dirR = Vector3f.sub(newRight.elementAt(i), newRight.elementAt(i-1));
				float lengthL = dirL.normalize();
				float lengthR = dirR.normalize();
				float angle = dirL.angle(dirR);

				Vector3f dir1 = Vector3f.sub(newLeft.elementAt(i), newRight.elementAt(i));
				Vector3f dir2 = Vector3f.sub(newLeft.elementAt(i-1), newRight.elementAt(i-1));
				dir1.normalize();
				dir2.normalize();
				
				float width1 = newLeft.elementAt(i).distance(newRight.elementAt(i));
				float angleTwist = dir1.angle(dir2);

				Vector3f mid0 = Vector3f.add(newLeft.elementAt(i-1), newRight.elementAt(i-1));  mid0.scale(0.5f);
				Vector3f mid1 = Vector3f.add(newLeft.elementAt(i), newRight.elementAt(i));  mid1.scale(0.5f);
				float midLength = mid0.distance(mid1);
				if ( midLength < minMidLength )
					minMidLength = midLength;
				if ( midLength > maxMidLength )
					maxMidLength = midLength;
				
//				System.err.println( lengthL + "   " + lengthR + "   " + Math.abs(lengthL - lengthR) + "   " + angle + "        " + width1 + "    " + width2 + "    " + angleTwist);
				totalLengthL += lengthL;
				totalLengthR += lengthR;
				totalPAngle += angle;
				
				if ( lengthL < minLength )
					minLength = lengthL;
				if ( lengthR < minLength )
					minLength = lengthR;
				if( lengthL > maxLength )
					maxLength = lengthL;
				if ( lengthR > maxLength )
					maxLength = lengthR;
				
				float diff = Math.abs(lengthL - lengthR);
				if ( diff < minLengthDiff )
					minLengthDiff = diff;
				if ( diff > maxLengthDiff )
					maxLengthDiff = diff;
				
				if ( angle < minPAngle )
					minPAngle = angle;
				if ( angle > maxPAngle )
					maxPAngle = angle;
				
				if ( i < (newLeft.size() - 1) )
				{
					if ( width1 < minWidth )
						minWidth = width1;
					if ( width1 > maxWidth )
						maxWidth = width1;
				}
				else if ( i == (newLeft.size() -1 ) )
				{
					if ( width1 < min10thWidth )
						min10thWidth = width1;
					if ( width1 > max10thWidth )
					{
						max10thWidth = width1;			
					}
				}

				if ( angleTwist < minTAngle )
					minTAngle = angleTwist;
				if ( angleTwist > maxTAngle )
					maxTAngle = angleTwist;
				
			}
			if ( i > 1 )
			{
				Vector3f mid0 = Vector3f.add(newLeft.elementAt(i-2), newRight.elementAt(i-2));  mid0.scale(0.5f);
				Vector3f mid1 = Vector3f.add(newLeft.elementAt(i-1), newRight.elementAt(i-1));  mid1.scale(0.5f);
				Vector3f mid2 = Vector3f.add(newLeft.elementAt(i), newRight.elementAt(i));  mid2.scale(0.5f);
				Vector3f dir1 = Vector3f.sub(mid1, mid0);
				Vector3f dir2 = Vector3f.sub(mid2, mid1);
				dir1.normalize();
				dir2.normalize();
				float angle = dir1.angle(dir2);
				totalBAngle += angle;
				if ( angle > maxBendAngle )
				{
					maxBendAngle = angle;
				}
			}
		}
		if ( totalLengthL < totalLengthMin )
			totalLengthMin = totalLengthL;
		if ( totalLengthR < totalLengthMin )
			totalLengthMin = totalLengthR;
		
		if ( totalLengthL > totalLengthMax )
			totalLengthMax = totalLengthL;
		if ( totalLengthR > totalLengthMax )
			totalLengthMax = totalLengthR;

		if ( totalPAngle > maxParallelAngle )
		{
			maxParallelAngle = totalPAngle;
		}
		System.err.println("Max parallel error: " + totalPAngle);
		System.err.println("Max bend : " + totalBAngle);
//		System.err.println( minLength + "," + maxLength + "," + minLengthDiff + "," + maxLengthDiff + "," + minWidth + "," + maxWidth + "," + minPAngle + "," + maxPAngle + "," + minTAngle + "," + maxTAngle + "," + totalLengthL + "," + totalLengthR + "," + Math.abs(totalLengthL - totalLengthR) + "," + totalAngle );
////		System.err.println("");
////		System.err.println( totalLengthL + "     " + totalLengthR + "    " + Math.abs(totalLengthL - totalLengthR) + "    " + totalAngle );
//		System.err.println("");
//		System.err.println("");
		
		return count;
	}


	private VOI simplfyLattice(Vector<Vector3f> seam, VOI lattice, int minDist)
	{

		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		
		Vector3f[] leftLattice = new Vector3f[left.size()];
		Vector3f[] rightLattice = new Vector3f[left.size()];
		for ( int i = 0; i < seam.size(); i++ )
		{
			int found = -1;
			float closest = Float.MAX_VALUE;
			for ( int j = 0; j < left.size(); j++ )
			{
				float distL = left.elementAt(j).distance(seam.elementAt(i));
				if ( distL < closest )
					closest = distL;
				if ( distL <= minDist )
				{
					found = j;
					break;
				}
			}
			if ( found != -1 )
			{
				if ( leftLattice[found] == null )
				{
					leftLattice[found] = seam.elementAt(i);
				}
				else
				{
					if ( left.elementAt(found).distance(seam.elementAt(i)) < left.elementAt(found).distance(leftLattice[found]) )
					{
						leftLattice[found] = seam.elementAt(i);
					}
				}
			}
			
			
			found = -1;
			closest = Float.MAX_VALUE;
			for ( int j = 0; j < right.size(); j++ )
			{
				float distR = right.elementAt(j).distance(seam.elementAt(i));
				if ( distR < closest )
					closest = distR;
				if ( distR <= minDist )
				{
					found = j;
					break;
				}
			}
			if ( found != -1 )
			{
				if ( rightLattice[found] == null )
				{
					rightLattice[found] = seam.elementAt(i);
				}
				else
				{
					if ( right.elementAt(found).distance(seam.elementAt(i)) < right.elementAt(found).distance(rightLattice[found]) )
					{
						rightLattice[found] = seam.elementAt(i);
					}
				}
			}
		}
		
		VOIContour newLeft = new VOIContour(false);
		VOIContour newRight = new VOIContour(false);
		for ( int i = 0; i < leftLattice.length; i++ )
		{
			if ( (leftLattice[i] != null) && (rightLattice[i] != null) )
			{
				newLeft.add(leftLattice[i]);
				newRight.add(rightLattice[i]);
			}
		}
		VOI newLattice = new VOI((short)0, "lattice", VOI.POLYLINE, 0);
		newLattice.getCurves().add(newLeft);
		newLattice.getCurves().add(newRight);
		return newLattice;
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

		if ( segmentSeamResultsCheck.isSelected() )
		{
			if ( noTest )
			{
				MipavUtil.displayInfo( segCount + " out of " + fileNamesList.size() + " files segmented 20 or more seam cells" );
			}
			else
			{
				MipavUtil.displayInfo( passCount + " out of " + fileNamesList.size() + " matched lattice" );
			}
		}
	}

	private boolean compareLattices( VOI autoLattice, VOI lattice )
	{
		if ( autoLattice == null )
			return false;
		if ( autoLattice.getCurves().size() == 0 )
			return false;
		VOIContour autoLeft = (VOIContour)autoLattice.getCurves().elementAt(0);
		VOIContour autoRight = (VOIContour)autoLattice.getCurves().elementAt(1);
		
		VOIContour left = (VOIContour)lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour)lattice.getCurves().elementAt(1);
		
		boolean match1 = compareLattices(autoLeft, autoRight, left, right);
		boolean match2 = compareLattices(autoRight, autoLeft, left, right);
		return (match1 | match2);
	}
	
	private boolean compareLattices( VOIContour left1, VOIContour right1, VOIContour left, VOIContour right )
	{		
		int[][] matchingIndexes = new int[left1.size()][2];
		for ( int i = 0; i < left1.size(); i++ )
		{
			int minIndex = -1;
			float minDist = Float.MAX_VALUE;
//			System.err.println( "left " + left1.elementAt(i) );
			for ( int j = 0; j < left.size(); j++ )
			{
				float distance = left.elementAt(j).distance(left1.elementAt(i));
//				System.err.println( i + "   " + j + "   " + distance );
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = j;
				}
			}
			matchingIndexes[i][0] = minIndex;

			minIndex = -1;
			minDist = Float.MAX_VALUE;
//			System.err.println( "right " + right1.elementAt(i) );
			for ( int j = 0; j < right.size(); j++ )
			{
				float distance = right.elementAt(j).distance(right1.elementAt(i));
//				System.err.println( i + "   " + j + "   " + distance );
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = j;
				}
			}
			matchingIndexes[i][1] = minIndex;
		}

//		System.err.println( "New Lattice: " );
		boolean match = true;
		for ( int i = 0; i < matchingIndexes.length; i++ )
		{
//			System.err.println( matchingIndexes[i][0] + "   " + matchingIndexes[i][1] );
			if ( matchingIndexes[i][0] != matchingIndexes[i][1] )
			{
				match = false;
			}
			if ( i > 0 )
			{
				if ( (matchingIndexes[i][0] <= matchingIndexes[i-1][0]) || (matchingIndexes[i][1] <= matchingIndexes[i-1][1]) )
				{
					match = false;
				}
			}
		}
		return match;
	}
	
	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}
}
