
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
import gov.nih.mipav.model.structures.VOIContour;
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
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.VOIWormAnnotation;
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

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistedToTwisted extends JDialogStandalonePlugin implements AlgorithmInterface
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5858343401705223389L;
	private JTextField straightenedAnnotationText;
	private String straightenedAnnotationFile;
	private JTextField targetLocText;
	private String targetDir;


	private JTextField targetPointsText;
	private String targetPointsFile;
	private JTextField inputPointsText;
	private String inputPointsFile;

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
		if ( (targetDir.length() == 0) || (straightenedAnnotationFile.length() == 0) ) {
			return;
		}
		String targetDirName = targetDir.substring(targetDir.lastIndexOf( File.separator ) + 1, targetDir.length());
		System.err.println(targetDirName);

		String outputDirName = straightenedAnnotationFile.substring(0, straightenedAnnotationFile.lastIndexOf( File.separator ) + 1);
		String inputFileName = straightenedAnnotationFile.substring(straightenedAnnotationFile.lastIndexOf( File.separator ) + 1, straightenedAnnotationFile.lastIndexOf( '.' ));
		System.err.println(outputDirName);
		System.err.println(inputFileName);

		String parentDir = targetDir.substring(0, targetDir.lastIndexOf(targetDirName) );
		String imageName = parentDir + File.separator + targetDirName + ".tif";		
		File imageFile = new File( imageName );
		System.err.println(imageName + " " + imageFile.exists() );
		
		File annotationFile = new File( straightenedAnnotationFile );
		File targetFile = new File( targetPointsFile );
		File inputFile = new File( inputPointsFile );
		
		ModelImage image = null;
		if ( imageFile.exists() && annotationFile.exists() )
		{
			System.err.println("opening image...");
			FileIO fileIO = new FileIO();
			image = fileIO.readImage(imageName);  
			if ( image != null  )
			{
				// create wormData
				WormData wormData = new WormData(image);
				// read final lattice:
				VOI lattice = wormData.readFinalLattice();
				// create LatticeModel:
				LatticeModel latticeModel = new LatticeModel(image, lattice);
								
				
				
				VOI targetPts = null;
				VOI inputPts = null;
				if ( targetFile.exists() && inputFile.exists() )
				{
					// read registration points and calculate thin-plate spline transform:
					targetPts = LatticeModel.readAnnotationsCSV(targetPointsFile);
					inputPts = LatticeModel.readAnnotationsCSV(inputPointsFile);

					if ( targetPts != null && inputPts != null)
					{
						checkPointMatch(targetPts, inputPts);
						findPairs(image, targetPts, inputPts);
					}
				}

				VOI annotations = LatticeModel.readAnnotationsCSV(straightenedAnnotationFile);
				if ( annotations != null )
				{
					if ( annotations.getCurves().size() > 0 )
					{
//						Vector<String> failedList = new Vector<String>();
						VOI annotationsToRetwist = new VOI( (short)0, inputFileName + "_" + targetDirName, VOI.ANNOTATION, 0 );
						VOI annotationVOISpline = new VOI( (short)0, inputFileName + "_" + targetDirName + "_spline", VOI.ANNOTATION, 0 );
						for ( int i = 0; i < annotations.getCurves().size(); i++ )
						{
							VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
							Vector3f pos = text.elementAt(0);
							String name = text.getText();

							// do thin-plane spline registration on input:
							Vector3f transformedPt = new Vector3f(pos);
							if ( inputSplineL != null )
							{
								transformedPt = findSplineTime(pos);
							}
							VOIWormAnnotation newText = new VOIWormAnnotation();
							newText.setText(name);
							newText.add(transformedPt);
							newText.add(transformedPt);
							// save original position:
							annotationVOISpline.getCurves().add(newText);
							// retwist position:
							VOIWormAnnotation retwistText = new VOIWormAnnotation(newText);
							retwistText.retwist(true);
							annotationsToRetwist.getCurves().add( retwistText );
						}
						// set the annotations:
						latticeModel.setAnnotations(annotationsToRetwist);						
						// retwist:
						VOI retwistedAnnotations = latticeModel.retwistAnnotations(lattice);
						
						LatticeModel.saveAnnotationsAsCSV(outputDirName, inputFileName + "_" + targetDirName + "_spline.csv", annotationVOISpline);
						LatticeModel.saveAnnotationsAsCSV(outputDirName, inputFileName + "_" + targetDirName + "_retwist.csv", retwistedAnnotations);
						String msg = "Retwisted " + retwistedAnnotations.getCurves().size() + " out of " + annotations.getCurves().size() + " annotations" + "\n";
//						for ( int j = 0; j < failedList.size(); j++ )
//						{
//							msg += failedList.elementAt(j) + " failed" + "\n";
//						}
						MipavUtil.displayInfo( msg );
					}
				}
				image.disposeLocal(false);
				image = null;
			}
			else
			{
				MipavUtil.displayError( "Error reading file: " + 
						imageFile.getName() + " " +  annotationFile.getName() + " " + targetFile.getName() + " " + inputFile.getName() );
				System.err.println("image open failed");
			}
		}
		else if ( !imageFile.exists() )
		{
			MipavUtil.displayError( "Error reading file: " + imageFile.getName() );
		}
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
		regPanel.setBorder(JDialogBase.buildTitledBorder("Registration (optional) - Untwisted Space"));
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

	private void test(String fileName )
	{
		File file = new File(fileName);
		if ( !file.exists() ) return;
		String saveFile = fileName.substring(0, fileName.lastIndexOf(".csv") ) + "_test.csv";
		File outFile = new File(saveFile);
		if ( outFile.exists() )
		{
			outFile.delete();
		}
		FileReader fr;
		FileWriter fw;
		try {
			fr = new FileReader(file);
			fw = new FileWriter(outFile);
			BufferedReader br = new BufferedReader(fr);
			String line = br.readLine(); // first line is header
			StringTokenizer st = new StringTokenizer(line, ",");
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// name
				fw.write(',');
			}
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// visit type
				fw.write(',');
			}
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// GUID
				fw.write(',');
//				st.nextToken();
//				fw.write("ID");	// GUID -> ID
//				fw.write(',');
			}
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// Age
				fw.write(',');
			}
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// Diagnosis
				fw.write(',');
			}
			if (st.hasMoreTokens()) {
				fw.write(st.nextToken());	// Variable
				fw.write('\n');
			}
			line = br.readLine();
			Vector<String> guidList = new Vector<String>();
			while ( line != null )
			{
				st = new StringTokenizer(line, ",");
				if (st.hasMoreTokens()) {
					fw.write(st.nextToken());	// name
					fw.write(',');
				}
				if (st.hasMoreTokens()) {
					fw.write(st.nextToken());	// visit type
					fw.write(',');
				}
				if (st.hasMoreTokens()) {
//					fw.write(st.nextToken());	// GUID
//					fw.write(',');
					String guid = st.nextToken();
					
					if ( !guidList.contains(guid) )
					{
						guidList.add(guid);
					}
					int id = guidList.indexOf(guid);
					fw.write(""+ (id + 55));	// GUID -> ID
					fw.write(',');
				}
				if (st.hasMoreTokens()) {
//					fw.write(st.nextToken());	// Age
//					fw.write(',');
					String ageInYears = st.nextToken();
					int age = Float.valueOf(ageInYears).intValue() / 10;
					age *= 10;
					fw.write("" + age);	// Age in decades
					fw.write(',');
				}
				if (st.hasMoreTokens()) {
					fw.write(st.nextToken());	// Diagnosis
					fw.write(',');
				}
				if (st.hasMoreTokens()) {
					fw.write(st.nextToken());	// Variable
				}
				fw.write('\n');
				line = br.readLine();
			}

			fr.close();
			fw.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
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

		return true;
	}
	
	private Vector<AlgorithmTPSpline> piecewiseSplines = null;
	private VOI splineBoundsL = null;
	private VOI splineBoundsR = null;
	
	private String pairSet[] = new String[] {"H0", "H1", "H2", "H3", "V0", "V1", "V2", "V3", "V4", "V5", "V6", "T" };
	private VOIContour inputPositionsL;
	private VOIContour inputPositionsR;
	private VOIContour targetPositionsL;
	private VOIContour targetPositionsR;
	private float[] inputTimesL;
	private float[] inputTimesR;
	private float[] targetTimesL;
	private float[] targetTimesR;
	private NaturalSpline3 inputSplineL;
	private NaturalSpline3 inputSplineR;
	private NaturalSpline3 targetSplineL;
	private NaturalSpline3 targetSplineR;
	private void findPairs(ModelImage image, VOI targetPts, VOI inputPts)
	{
		VOI pairListTargetL = new VOI( (short)0, "temp", VOI.ANNOTATION, 0 );
		VOI pairListTargetR = new VOI( (short)0, "temp", VOI.ANNOTATION, 0 );
		VOI pairListInputL = new VOI( (short)0, "temp", VOI.ANNOTATION, 0 );
		VOI pairListInputR = new VOI( (short)0, "temp", VOI.ANNOTATION, 0 );
		for ( int i = 0; i < pairSet.length; i++ )
		{
			boolean foundLeft = false;
			for ( int j = 0; j < targetPts.getCurves().size(); j++ )
			{
				String name = ((VOIText)targetPts.getCurves().elementAt(j)).getText();
				if ( name.equals( pairSet[i] + "L") )
				{
					foundLeft = true;
					pairListTargetL.getCurves().add(new VOIText(((VOIText)targetPts.getCurves().elementAt(j))));
					break;
				}
			}
			if ( foundLeft )
			{
				foundLeft = false;
				for ( int j = 0; j < inputPts.getCurves().size(); j++ )
				{
					String name = ((VOIText)inputPts.getCurves().elementAt(j)).getText();
					if ( name.equals( pairSet[i] + "L") )
					{
						foundLeft = true;
						pairListInputL.getCurves().add(new VOIText(((VOIText)inputPts.getCurves().elementAt(j))));
						break;
					}
				}
				if ( !foundLeft )
				{
					pairListTargetL.getCurves().remove(pairListTargetL.getCurves().lastElement());
				}
				else 
				{
					boolean foundRight = false;
					for ( int j = 0; j < targetPts.getCurves().size(); j++ )
					{
						String name = ((VOIText)targetPts.getCurves().elementAt(j)).getText();
						if ( name.equals( pairSet[i] + "R") )
						{
							foundRight = true;
							pairListTargetR.getCurves().add(new VOIText(((VOIText)targetPts.getCurves().elementAt(j))));
							break;
						}
					}
					if ( foundRight )
					{
						foundRight = false;
						for ( int j = 0; j < inputPts.getCurves().size(); j++ )
						{
							String name = ((VOIText)inputPts.getCurves().elementAt(j)).getText();
							if ( name.equals( pairSet[i] + "R") )
							{
								foundRight = true;
								pairListInputR.getCurves().add(new VOIText(((VOIText)inputPts.getCurves().elementAt(j))));
								break;
							}
						}
						if ( !foundRight )
						{
							// left and right added to target, only left added to input, remove since right for input wasn't found:
							pairListTargetL.getCurves().remove(pairListTargetL.getCurves().lastElement());
							pairListTargetR.getCurves().remove(pairListTargetR.getCurves().lastElement());
							pairListInputL.getCurves().remove(pairListInputL.getCurves().lastElement());
						}
//						System.err.println( "Found " + pairSet[i] + "   " + pairListTarget.getCurves().size()  + "   " + pairListInput.getCurves().size() );
					}
				}
			}
		}
		
		if ( pairListTargetL.getCurves().size() + pairListTargetR.getCurves().size() < 4 )
		{
			MipavUtil.displayError( "Target points and input points must have at least 4 matches" );
			return;
		}


		VOIContour center = new VOIContour(false);	
		for ( int i = 0; i < pairListTargetL.getCurves().size(); i++ )
		{	
			Vector3f pt = new Vector3f(pairListTargetL.getCurves().elementAt(i).elementAt(0));
//			pt.add(pairListTargetR.getCurves().elementAt(i).elementAt(0));
//			pt.scale(0.5f);
//			pt.X = 0;
//			pt.Y = 0;
			center.add(pt);
		}
		float[] afTimeC = new float[center.size()];
		targetSplineL = smoothCurve(center, afTimeC);
		float length = targetSplineL.GetLength(0, 1);
		//		System.err.println( "Generate Curves " + length );
		int maxLength = (int)Math.ceil(length);
		float step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		targetPositionsL = new VOIContour(false);
		targetTimesL = new float[maxLength + 1];
		for (int i = 0; i <= maxLength; i++) {
			final float t = targetSplineL.GetTime(i*step);
			targetPositionsL.add(targetSplineL.GetPosition(t));
			targetTimesL[i] = t;
		}
		

		center = new VOIContour(false);	
		for ( int i = 0; i < pairListTargetR.getCurves().size(); i++ )
		{	
			Vector3f pt = new Vector3f(pairListTargetR.getCurves().elementAt(i).elementAt(0));
//			pt.add(pairListTargetR.getCurves().elementAt(i).elementAt(0));
//			pt.scale(0.5f);
//			pt.X = 0;
//			pt.Y = 0;
			center.add(pt);
		}
		afTimeC = new float[center.size()];
		targetSplineR = smoothCurve(center, afTimeC);
		length = targetSplineL.GetLength(0, 1);
		//		System.err.println( "Generate Curves " + length );
		maxLength = (int)Math.ceil(length);
		step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		targetPositionsR = new VOIContour(false);
		targetTimesR = new float[maxLength + 1];
		for (int i = 0; i <= maxLength; i++) {
			final float t = targetSplineR.GetTime(i*step);
			targetPositionsR.add(targetSplineR.GetPosition(t));
			targetTimesR[i] = t;
		}
		
		
		

		center = new VOIContour(false);	
		for ( int i = 0; i < pairListInputL.getCurves().size(); i++ )
		{	
			Vector3f pt = new Vector3f(pairListInputL.getCurves().elementAt(i).elementAt(0));
//			pt.add(pairListInputR.getCurves().elementAt(i).elementAt(0));
//			pt.scale(0.5f);
//			pt.X = 0;
//			pt.Y = 0;
			center.add(pt);
		}
		afTimeC = new float[center.size()];
		inputSplineL = smoothCurve(center, afTimeC);
		length = inputSplineL.GetLength(0, 1);
		//		System.err.println( "Generate Curves " + length );
		maxLength = (int)Math.ceil(length);
		step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		inputTimesL = new float[maxLength + 1];
		inputPositionsL = new VOIContour(false);
		for (int i = 0; i <= maxLength; i++) {
			final float t = inputSplineL.GetTime(i*step);
			inputPositionsL.add(inputSplineL.GetPosition(t));
			inputTimesL[i] = t;
		}
		
		
		

		center = new VOIContour(false);	
		for ( int i = 0; i < pairListInputR.getCurves().size(); i++ )
		{	
			Vector3f pt = new Vector3f(pairListInputR.getCurves().elementAt(i).elementAt(0));
//			pt.add(pairListInputR.getCurves().elementAt(i).elementAt(0));
//			pt.scale(0.5f);
//			pt.X = 0;
//			pt.Y = 0;
			center.add(pt);
		}
		afTimeC = new float[center.size()];
		inputSplineR = smoothCurve(center, afTimeC);
		length = inputSplineR.GetLength(0, 1);
		//		System.err.println( "Generate Curves " + length );
		maxLength = (int)Math.ceil(length);
		step = 1;
		if ( maxLength != length )
		{
			step = (length / maxLength);
		}
		inputTimesR = new float[maxLength + 1];
		inputPositionsR = new VOIContour(false);
		for (int i = 0; i <= maxLength; i++) {
			final float t = inputSplineR.GetTime(i*step);
			inputPositionsR.add(inputSplineR.GetPosition(t));
			inputTimesR[i] = t;
		}
		System.err.println( "Times " + targetTimesL.length + "   " + targetTimesR.length + "   " + inputTimesL.length + "   " + inputTimesR.length );
//		for ( int i = 0; i < inputTimes.length; i++ ) {
//			System.err.println(i + "    " + targetTimes[i] + "     " + inputTimes[i] );
//		}
		
//		System.err.println("1D Splines:");
//		for ( int i = 0; i < pairListInputL.getCurves().size(); i++ )
//		{	
//			VOIText leftI = (VOIText) pairListInputL.getCurves().elementAt(i);
//			VOIText rightI = (VOIText) pairListInputR.getCurves().elementAt(i);
//			VOIText leftT = (VOIText) pairListTargetL.getCurves().elementAt(i);
//			VOIText rightT = (VOIText) pairListTargetR.getCurves().elementAt(i);
//			
//			System.err.println(leftI.getText() + "  " + rightI.getText() );
//			float time = inputSpline.GetTime((leftI.elementAt(0).Z + rightI.elementAt(0).Z)/2f);
//			Vector3f targetP = targetSpline.GetPosition(time);
//			System.err.println( targetP.Z + "  " + (leftT.elementAt(0).Z + rightT.elementAt(0).Z)/2f);
//		}
		

		VOIText previousT[] = new VOIText[] {((VOIText)pairListTargetL.getCurves().elementAt(0)), ((VOIText)pairListTargetR.getCurves().elementAt(0))};
		VOIText previousI[] = new VOIText[] {((VOIText)pairListInputL.getCurves().elementAt(0)), ((VOIText)pairListInputR.getCurves().elementAt(0))};
		for ( int i = 0; i < pairListTargetL.getCurves().size() - 1; i++ )
		{
//			System.err.println( ((VOIText)pairListInput.getCurves().elementAt(i)).getText() + "   " +  ((VOIText)pairListInput.getCurves().elementAt(i)).getText());
			int numTargetPts = 4;
			double[] xSource = new double[ numTargetPts ]; 
			double[] ySource = new double[ numTargetPts ]; 
			double[] zSource = new double[ numTargetPts ]; 
			double[] xTarget = new double[ numTargetPts ]; 
			double[] yTarget = new double[ numTargetPts ]; 
			double[] zTarget = new double[ numTargetPts ]; 
			
			VOIText target1L = previousT[0];
			VOIText input1L = previousI[0];
			xSource[0] = input1L.elementAt(0).X;
			ySource[0] = input1L.elementAt(0).Y;
			zSource[0] = input1L.elementAt(0).Z;
			
			xTarget[0] = target1L.elementAt(0).X;
			yTarget[0] = target1L.elementAt(0).Y;
			zTarget[0] = target1L.elementAt(0).Z;
			
			VOIText target1R = previousT[1];
			VOIText input1R = previousI[1];
			xSource[1] = input1R.elementAt(0).X;
			ySource[1] = input1R.elementAt(0).Y;
			zSource[1] = input1R.elementAt(0).Z;
			
			xTarget[1] = target1R.elementAt(0).X;
			yTarget[1] = target1R.elementAt(0).Y;
			zTarget[1] = target1R.elementAt(0).Z;
			
			VOIText target2L = ((VOIText)pairListTargetL.getCurves().elementAt(i+1));
			VOIText input2L = ((VOIText)pairListInputL.getCurves().elementAt(i+1));
			xSource[2] = input2L.elementAt(0).X;
			ySource[2] = input2L.elementAt(0).Y;
			zSource[2] = input2L.elementAt(0).Z;
			
			xTarget[2] = target2L.elementAt(0).X;
			yTarget[2] = target2L.elementAt(0).Y;
			zTarget[2] = target2L.elementAt(0).Z;
			
			VOIText target2R = ((VOIText)pairListTargetR.getCurves().elementAt(i+1));
			VOIText input2R = ((VOIText)pairListInputR.getCurves().elementAt(i+1));
			xSource[3] = input2R.elementAt(0).X;
			ySource[3] = input2R.elementAt(0).Y;
			zSource[3] = input2R.elementAt(0).Z;
			
			xTarget[3] = target2R.elementAt(0).X;
			yTarget[3] = target2R.elementAt(0).Y;
			zTarget[3] = target2R.elementAt(0).Z;

			System.err.println("");
			System.err.println("");
			System.err.println("Set of Four: ");
			System.err.println(input1L.elementAt(0) + "  " + input1R.elementAt(0) + "  " + input2L.elementAt(0) + " " + input2R.elementAt(0) );
			System.err.println(target1L.elementAt(0) + "  " + target1R.elementAt(0) + "  " + target2L.elementAt(0) + " " + target2R.elementAt(0) );

			// create spline:
			AlgorithmTPSpline spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTarget, yTarget, zTarget, 0.0f, image,
					image, true);

			spline.setRunningInSeparateThread(false);
			boolean splineError = false;
//			System.err.println( "Creating spline " + i + "\n" + 
//					target1L.getText() + " " + input1L.getText() + "\n" + 
//					target1R.getText() + " " + input1R.getText() + "\n" +
//					target2L.getText() + " " + input2L.getText() + "\n" + 
//					target2R.getText() + " " + input2R.getText() + "\n" + "\n" );
			try {
			spline.run();
			} catch (Exception e) {
//				System.err.println( "Spline error: " + e.toString() );
				splineError = true;
			}
			if ( !splineError ) {
				System.err.println( "Bounds " + input1L.getText() + "   " + input1R.getText() );
				System.err.println( "Bounds " + input2L.getText() + "   " + input2R.getText() );
				previousT[0] = target2L;				previousT[1] = target2R;
				previousI[0] = input2L;					previousI[1] = input2R;
				
				if ( piecewiseSplines == null )
				{
					piecewiseSplines = new Vector<AlgorithmTPSpline>();
				}
				if ( splineBoundsL == null )
				{
					splineBoundsL =  new VOI( (short)0, "spline left", VOI.ANNOTATION, 0 );
				}
				if ( splineBoundsR == null )
				{
					splineBoundsR =  new VOI( (short)0, "spline right", VOI.ANNOTATION, 0 );
				}
				piecewiseSplines.add(spline);
				splineBoundsL.getCurves().add(input1L);
				splineBoundsR.getCurves().add(input1R);
				
				
				int testCount = 0;

				// test that target points warp to source points:
				Vector3f sourcePt = input1L.elementAt(0);
				Vector3f targetPt = target1L.elementAt(0);
				float[] transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
				System.err.println( "test: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
				if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
				{
					System.err.println( "error" );
					testCount++;
				}

				// test that target points warp to source points:
				sourcePt = input1R.elementAt(0);
				targetPt = target1R.elementAt(0);
				transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
				System.err.println( "test: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
				if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
				{
					System.err.println( "error" );
					testCount++;
				}

				// test that target points warp to source points:
				sourcePt = input2L.elementAt(0);
				targetPt = target2L.elementAt(0);
				transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
				System.err.println( "test: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
				if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
				{
					System.err.println( "error" );
					testCount++;
				}

				// test that target points warp to source points:
				sourcePt = input2R.elementAt(0);
				targetPt = target2R.elementAt(0);
				transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
				System.err.println( "test: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
				if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
				{
					System.err.println( "error" );
					testCount++;
				}
				System.err.println(testCount);
				
			}
			else if ( splineError && (i == (pairListTargetL.getCurves().size() - 2) ))
			{
				boolean splineFound = false;
				// failed on the last spline, try this last pair with previous pairs:
				for ( int j = i -1; j >= 0; j-- )
				{
					target1L = (VOIText)pairListTargetL.getCurves().elementAt(j);
					input1L = (VOIText)pairListInputL.getCurves().elementAt(j);
					xSource[0] = input1L.elementAt(0).X;
					ySource[0] = input1L.elementAt(0).Y;
					zSource[0] = input1L.elementAt(0).Z;

					xTarget[0] = target1L.elementAt(0).X;
					yTarget[0] = target1L.elementAt(0).Y;
					zTarget[0] = target1L.elementAt(0).Z;

					target1R = (VOIText)pairListTargetR.getCurves().elementAt(j);
					input1R = (VOIText)pairListInputR.getCurves().elementAt(j);
					xSource[1] = input1R.elementAt(0).X;
					ySource[1] = input1R.elementAt(0).Y;
					zSource[1] = input1R.elementAt(0).Z;

					xTarget[1] = target1R.elementAt(0).X;
					yTarget[1] = target1R.elementAt(0).Y;
					zTarget[1] = target1R.elementAt(0).Z;

					// create spline:
					spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTarget, yTarget, zTarget, 0.0f, image,
							image, true);

					spline.setRunningInSeparateThread(false);
					splineError = false;
//					System.err.println( "Creating spline " + i + "\n" + 
//							target1L.getText() + " " + input1L.getText() + "\n" + 
//							target1R.getText() + " " + input1R.getText() + "\n" +
//							target2L.getText() + " " + input2L.getText() + "\n" + 
//							target2R.getText() + " " + input2R.getText() + "\n" + "\n" );
					try {
						spline.run();
					} catch (Exception e) {
						System.err.println( "Spline error: " + e.toString() );
						splineError = true;
					}
					if ( !splineError )
					{
//						System.err.println( "Bounds " + input1L.getText() + "   " + input1R.getText() );
//						System.err.println( "Bounds " + input2L.getText() + "   " + input2R.getText() );
						splineFound = true;
						piecewiseSplines.add(spline);
						splineBoundsL.getCurves().add(input1L);
						splineBoundsR.getCurves().add(input1R);
						
						
						
						
						
						

						
						int testCount = 0;

						// test that target points warp to source points:
						Vector3f sourcePt = input1L.elementAt(0);
						Vector3f targetPt = target1L.elementAt(0);
						float[] transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
						System.err.println( "test2: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
						if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
						{
							System.err.println( "error" );
							testCount++;
						}

						// test that target points warp to source points:
						sourcePt = input1R.elementAt(0);
						targetPt = target1R.elementAt(0);
						transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
						System.err.println( "test2: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
						if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
						{
							System.err.println( "error" );
							testCount++;
						}

						// test that target points warp to source points:
						sourcePt = input2L.elementAt(0);
						targetPt = target2L.elementAt(0);
						transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
						System.err.println( "test2: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
						if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
						{
							System.err.println( "error" );
							testCount++;
						}

						// test that target points warp to source points:
						sourcePt = input2R.elementAt(0);
						targetPt = target2R.elementAt(0);
						transformedPt = spline.getCorrespondingPoint(sourcePt.X, sourcePt.Y, sourcePt.Z);
						System.err.println( "test2: " + sourcePt + "   =>  " + transformedPt[0] + " " + transformedPt[1] + " " + transformedPt[2] );
						if ( (Math.round(targetPt.X) != Math.round(transformedPt[0])) || (Math.round(targetPt.Y) != Math.round(transformedPt[1])) || (Math.round(targetPt.Z) != Math.round(transformedPt[2])) )
						{
							System.err.println( "error" );
							testCount++;
						}
						System.err.println(testCount);
						break;
					}
				}
			}
			
		}
		System.err.println("");
		System.err.println("");
		System.err.println("Done piecewise splines " + piecewiseSplines.size() );
		System.err.println("");
		System.err.println("");
	}
	
	private Vector3f findSplineTime(Vector3f pt) {
		Vector3f inputPt = new Vector3f();
		Vector3f targetPt;
		float zDistL = Float.MAX_VALUE;
		int indexL = -1;
		for (int i = 0; i < inputPositionsL.size(); i++) {
			float dist = Math.abs(pt.Z - inputPositionsL.elementAt(i).Z);
			if ( dist < zDistL )
			{
				zDistL = dist;
				indexL = i;
			}
		}
		float zDistR = Float.MAX_VALUE;
		int indexR = -1;
		for (int i = 0; i < inputPositionsR.size(); i++) {
			float dist = Math.abs(pt.Z - inputPositionsR.elementAt(i).Z);
			if ( dist < zDistR )
			{
				zDistR = dist;
				indexR = i;
			}
		}
		float time;
		float distL = inputPositionsL.elementAt(indexL).distance(pt);
		float distR = inputPositionsR.elementAt(indexR).distance(pt);
		if ( distL < distR )
		{
			inputPt.copy(inputPositionsL.elementAt(indexL));
			time = inputTimesL[indexL];
			targetPt = targetSplineL.GetPosition(time);
		}
		else
		{
			inputPt.copy(inputPositionsR.elementAt(indexR));
			time = inputTimesR[indexR];
			targetPt = targetSplineR.GetPosition(time);
		}

		float scaleX = targetPt.X / inputPt.X;
		float scaleY = targetPt.Y / inputPt.Y;
		float z = targetPt.Z;
		
		Vector3f transformedPt = new Vector3f(pt);
		transformedPt.X *= scaleX;
		transformedPt.Y *= scaleY;
		transformedPt.Z = z;
		transformedPt.X = Math.round(transformedPt.X);
		transformedPt.Y = Math.round(transformedPt.Y);
		transformedPt.Z = Math.round(transformedPt.Z);
		return transformedPt;
	}
	
	private NaturalSpline3 smoothCurve(final VOIContour curve, final float[] time) {
//		float totalDistance = 0;
//		for (int i = 0; i < curve.size() - 1; i++) {
//			totalDistance += curve.elementAt(i).distance(curve.elementAt(i + 1));
//		}
//
//		final Vector3f[] akPoints = new Vector3f[curve.size()];
//		float distance = 0;
//		for (int i = 0; i < curve.size(); i++) {
//			if (i > 0) {
//				distance += curve.elementAt(i).distance(curve.elementAt(i - 1));
//				time[i] = distance / totalDistance;
//				akPoints[i] = new Vector3f(curve.elementAt(i));
//			} else {
//				time[i] = 0;
//				akPoints[i] = new Vector3f(curve.elementAt(i));
//			}
//		}

		final Vector3f[] akPoints = new Vector3f[curve.size()];
		for (int i = 0; i < curve.size(); i++) {
			time[i] = i / (float)(curve.size() - 1);
			akPoints[i] = new Vector3f(curve.elementAt(i));
		}
		
		NaturalSpline3 spline = new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
		System.err.println("");
		System.err.println("");
		System.err.println("");
		System.err.println("Time");
		for (int i = 0; i < curve.size(); i++) {
			System.err.println(akPoints[i].Z + "   " + spline.GetPosition(time[i]) + "   " + time[i] );
		}
		return spline;
	}

	private void checkPointMatch(VOI targetPts, VOI inputPts)
	{
		float rightDistance = 0;
		float leftDistance = 0;
		float leftSwapDistance = 0;
		float rightSwapDistance = 0;
		for ( int i = 0; i < targetPts.getCurves().size(); i++ ) {
			
			String name = ((VOIText)targetPts.getCurves().elementAt(i)).getText();
			boolean isLeft = name.substring(name.length() - 1).equals("L");
			boolean isRight = name.substring(name.length() - 1).equals("R");
			Vector3f sourcePt = null;
			Vector3f targetPt = targetPts.getCurves().elementAt(i).elementAt(0);
			
			for ( int j = 0; j < inputPts.getCurves().size(); j++ )
			{
				String inputName = ((VOIText)inputPts.getCurves().elementAt(j)).getText();
				if ( inputName.equals( name ) )
				{
					sourcePt = inputPts.getCurves().elementAt(j).elementAt(0);
					break;
				}
			}
			if ( sourcePt != null && targetPt != null )
			{
				if ( isLeft )
				{
					leftDistance += sourcePt.distance(targetPt);
				}
				if ( isRight )
				{
					rightDistance += sourcePt.distance(targetPt);
				}
			}			
			
			String newName = name.substring(0, name.length()-1);
			boolean isLeftSwap = false;
			boolean isRightSwap = false;
			if ( isLeft )
			{
				newName = newName + "R";
				isLeftSwap = true;
			}
			if ( isRight )
			{
				newName = newName + "L";
				isRightSwap = true;
			}
			sourcePt = null;
			for ( int j = 0; j < inputPts.getCurves().size(); j++ )
			{
				// input point uses the swapped name:
				String inputName = ((VOIText)inputPts.getCurves().elementAt(j)).getText();
				if ( inputName.equals( newName ) )
				{
					sourcePt = inputPts.getCurves().elementAt(j).elementAt(0);
					break;
				}
			}
			if ( sourcePt != null && targetPt != null )
			{
				System.err.println( newName + "  vs " + name );
				if ( isLeftSwap )
				{
					leftSwapDistance += sourcePt.distance(targetPt);
				}
				if ( isRightSwap )
				{
					rightSwapDistance += sourcePt.distance(targetPt);
				}
			}			
		}
		
		System.err.println( leftDistance + "   " + rightDistance );
		System.err.println( leftSwapDistance + "   " + rightSwapDistance );
		if ( leftDistance > leftSwapDistance && rightDistance > rightSwapDistance )
		{
			MipavUtil.displayInfo( "Correspondence points seem to be swapped left-right... applying fix." );

			VOI temp = new VOI( (short)0, "temp", VOI.ANNOTATION, 0 );
			for ( int i = 0; i < inputPts.getCurves().size(); i++ ) {
				String name = ((VOIText)inputPts.getCurves().elementAt(i)).getText();
				boolean isLeft = name.substring(name.length() - 1).equals("L");
				boolean isRight = name.substring(name.length() - 1).equals("R");
				
				String newName = name.substring(0, name.length()-1);
				if ( isLeft )
				{
					newName = newName + "R";
				}
				if ( isRight )
				{
					newName = newName + "L";
				}
				if ( isLeft || isRight )
				{
					VOIText text = ((VOIText)inputPts.getCurves().elementAt(i));
					text.setText(newName);
					temp.getCurves().add(text);
				}
			}
			inputPts.getCurves().clear();
			inputPts.getCurves().addAll(temp.getCurves());
		}
	}
}
