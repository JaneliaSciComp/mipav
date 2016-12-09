
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
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModelEM;
import gov.nih.mipav.view.renderer.WildMagic.Render.ModelImageLargeFormat;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;

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
	private JCheckBox segmentSeamResultsCheck;
	private JCheckBox straightenImageCheck;
	private JCheckBox straightenMarkersCheck;
	private JRadioButton voxelCoords;
	private JRadioButton micronCoords;
	private JRadioButton segmentSkinSurface;
	private JRadioButton segmentLattice;
	private ModelImage wormImage;


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
		if ( includeRange != null )
		{
			System.err.println("Starting plugin" );
			Vector<String> fileNamesList = null;
			Vector<Integer> numSegmented = null;
			Vector<Integer> numMatched = null;
			for ( int i = 0; i < includeRange.size(); i++ )
			{				
				// Build the full image name:
				baseFileName = baseFileNameText.getText();
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File imageFile = new File(baseFileDir + File.separator + fileName);

				if ( imageFile.exists() )
				{	
					System.err.println( "   " + fileName );
					FileIO fileIO = new FileIO();
					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					if ( segmentSeamCheck.isSelected() )
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
						ModelImage blur = WormSegmentation.blur(wormImage, 3);
						Vector<Vector3f> seamCells = WormSegmentation.segmentSeam(blur, minRadius, maxRadius, outputDir);
						blur.disposeLocal(false);


						String latticeFile = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoLatticeGenerationOutput + "1" + File.separator;
						VOIVector latticeVector = new VOIVector();
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, latticeFile, true, latticeVector, false);

						int numFound = -1;
						if ( latticeVector.size() != 0 )
						{
							numFound = testSeamLattice(seamCells, latticeVector.elementAt(0), maxRadius);
						}


						short id = (short) wormImage.getVOIs().getUniqueID();
						VOI seamCellAnnotations = new VOI(id, "seam_cells", VOI.ANNOTATION, (float)Math.random());
						wormImage.registerVOI(seamCellAnnotations);
						for ( int j = 0; j < seamCells.size(); j++ )
						{
							VOIText text = new VOIText();
							text.setText("T__" + (j+1));
							text.add( new Vector3f(seamCells.elementAt(j)) );
							text.add( new Vector3f(seamCells.elementAt(j)) );
							text.setUseMarker(false);
							seamCellAnnotations.getCurves().add(text);
						}
						String seamCellDir = resultsDir + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + File.separator;
						outputFileDir = new File(seamCellDir);
						if ( !outputFileDir.exists() )
						{
							outputFileDir.mkdir();
						}
						LatticeModel.saveAllVOIsTo(seamCellDir, wormImage);
						LatticeModel.saveSeamCellsTo(seamCellDir, seamCells);

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
					if ( straightenImageCheck.isSelected() || straightenMarkersCheck.isSelected() )
					{
						String latticeFile = baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoLatticeGenerationOutput + "1" + File.separator;
						VOIVector latticeVector = new VOIVector();
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, latticeFile, true, latticeVector, false);

						if ( latticeVector.size() != 0 )
						{
							LatticeModel model = new LatticeModel(wormImage);
							model.setLattice(latticeVector.elementAt(0));

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
							if ( segmentSkinSurface.isSelected() )
							{
								contourImage = model.segmentSkin(wormImage, paddingFactor);
							}
							else if ( segmentLattice.isSelected() )
							{
								model.segmentLattice(wormImage, false);
							}
							if ( straightenMarkersCheck.isSelected() )
							{
								model.setSegmentSeamCells( true );
								model.interpolateLattice( false, false, false, true );								
							}
							model.dispose();
							model = null;

							if ( wormImage != null )
							{
								wormImage.disposeLocal(false);
							}


							// Build the full image name:
							baseFileName = baseFileNameText.getText();
							fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
							imageFile = new File(baseFileDir2 + File.separator + fileName);
							if ( imageFile.exists() )
							{					

								System.err.println( "   " + fileName );
								fileIO = new FileIO();
								wormImage = fileIO.readImage(fileName, baseFileDir2 + File.separator, false, null); 
								model = new LatticeModel(wormImage);
								model.setLattice(latticeVector.elementAt(0));
								model.setSegmentSeamCells( false );
								model.interpolateLattice( false, false, straightenImageCheck.isSelected(), false );

								if ( segmentSkinSurface.isSelected() )
								{
									contourImage = model.segmentSkin(wormImage, contourImage, paddingFactor);
								}
								else if ( segmentLattice.isSelected() )
								{
									model.segmentLattice(wormImage, false);
								}
								model.dispose();
								model = null;

								if ( wormImage != null )
								{
									wormImage.disposeLocal(false);
								}
							}						

							if ( contourImage != null )
							{
								contourImage.disposeLocal(false);
							}
						}
						else
						{
							MipavUtil.displayError( "Error in reading lattice file " + latticeFile );
						}
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
			System.err.println("Done plugin" );
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



		//		JPanel inputsPanel = new JPanel(new GridBagLayout());
		//		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		//		inputsPanel.setForeground(Color.black);
		//
		//		inputImageTF = gui.buildFileField("Worm image:  ", "", false, JFileChooser.FILES_ONLY, this);
		//		inputsPanel.add(inputImageTF.getParent(), gbc);
		//		gbc.gridy++;
		//
		//		latticeFileTF = gui.buildFileField("lattice directory:  ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		//		inputsPanel.add(latticeFileTF.getParent(), gbc);
		//		gbc.gridy++;
		//
		//		nucleiTF = gui.buildFileField("Marker info (spreadsheet csv file): ", "", false, JFileChooser.FILES_ONLY, this);
		//		inputsPanel.add(nucleiTF.getParent(), gbc);
		//		gbc.gridy++;
		//		gbc.gridx = 0;

		JPanel algorithmsPanel = new JPanel(new BorderLayout());

		JPanel segmentationPanel = new JPanel(new GridBagLayout());
		segmentationPanel.setBorder(JDialogBase.buildTitledBorder("Seam Cell Segmentation Options"));
		segmentSeamCheck = gui.buildCheckBox( "Segment seam cells", true );
		segmentationPanel.add(segmentSeamCheck.getParent(), gbc);
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


		JPanel algorithmPanel = new JPanel(new GridBagLayout());
		algorithmPanel.setBorder(JDialogBase.buildTitledBorder("Straightening Options"));
		straightenImageCheck = gui.buildCheckBox( "Straighten Image", true );
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

		segmentationPaddingText = gui.buildField("Segmentation padding (voxels): ", "           10");
		algorithmPanel.add(segmentationPaddingText.getParent(), gbc);
		gbc.gridy++;

		straightenMarkersCheck = gui.buildCheckBox( "Straighten Markers", true );
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


	private int testSeamLattice(Vector<Vector3f> seam, VOI lattice, int minDist)
	{

		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		int count = 0;
		for ( int i = 0; i < seam.size(); i++ )
		{
			boolean found = false;
			float closest = Float.MAX_VALUE;

			for ( int j = 0; j < left.size(); j++ )
			{
				float distL = left.elementAt(j).distance(seam.elementAt(i));
				float distR = right.elementAt(j).distance(seam.elementAt(i));
				if ( distL < closest )
					closest = distL;
				if ( distR < closest )
					closest = distR;
				if ( (distL <= minDist) || (distR <= minDist) )
				{
					found = true;
					break;
				}
			}
			if ( !found )
			{
				//				System.err.println( (i+1) + " " + closest );
				count++;
			}
		}
		int foundCount = seam.size() - count;
		if ( foundCount >= 20 )
		{
			System.err.println( "     PASS" );
		}
		if ( foundCount < 20 )
		{
			System.err.println( "     FAIL" );
			System.err.println( "     " + count + " out of " + seam.size() + " not found" );
		}
		return foundCount;
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

	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}
}
