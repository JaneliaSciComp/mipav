
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
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

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
import java.nio.file.Files;
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
	private JRadioButton untwistedToTwisted;
	private JRadioButton twistedToUntwisted;
	private JRadioButton voxelCoords;
	private JRadioButton micronCoords;
	private ModelImage wormImage;

	private String baseFileDir;
	private JTextField  baseFileLocText;
	private JTextField  inputFileText;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;
	private JTextField rangeFusionText;

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
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}		
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
		if ( includeRange != null )
		{
			System.err.println("Starting plugin" );
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
					

					if ( inputFileText.getText().length() > 0 )
					{
						File textFile = new File(inputFileText.getText());
						if ( textFile.exists() )
						{			
							VOIVector vois = readMarkerPositions(inputFileText.getText(), textFile);
							VOI annotations = vois.elementAt(0);
							if ( annotations != null )
							{
								if ( annotations.getCurves().size() > 0 )
								{
									if ( twistedToUntwisted.isSelected() )
									{
										String toStraightName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
												"output_images" + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_toStraight.xml";
										File toStraightFile = new File(baseFileDir + File.separator + toStraightName );
										System.err.println(toStraightName + " " + toStraightFile.exists() );
										
										ModelImage toTwisted = fileIO.readImage(toStraightName, baseFileDir + File.separator, false, null); 
										if ( toTwisted != null )
										{
											VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
											for ( int j = 0; j < annotations.getCurves().size(); j++ )
											{
												VOIText text = (VOIText) annotations.getCurves().elementAt(j);
												Vector3f pos = text.elementAt(0);
												String name = text.getText();

												float valid = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 0 );
												if ( valid != 1 )
												{
													System.err.println( name + " invalid position" );
												}
												float x = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 1 );
												float y = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 2 );
												float z = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 3 );
												Vector3f newPos = new Vector3f(x, y, z);
												
												VOIText newText = new VOIText();
												newText.setText(name);
												newText.add(newPos);

												annotationVOI.getCurves().add(newText);
											}
											String fileBase = inputFileText.getText().substring(0, inputFileText.getText().length() - 4);
											saveMarkerPositions( fileBase + "_straight.csv", annotationVOI, null );
										}
										else
										{
											MipavUtil.displayError( "Error in reading image file " + toStraightName );
										}
									}
									else if ( untwistedToTwisted.isSelected() )
									{
										String toTwistedName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
												"output_images" + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_toTwisted.xml";
										File toTwistedFile = new File(baseFileDir + File.separator + toTwistedName );
										System.err.println(toTwistedName + " " + toTwistedFile.exists() );
										ModelImage toTwisted = fileIO.readImage(toTwistedName, baseFileDir + File.separator, false, null);  
										
										String spreadName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator + 
												"output_images" + File.separator + 
												baseFileName + "_" + includeRange.elementAt(i) + "_spread.xml";
										File spreadFile = new File(baseFileDir + File.separator + spreadName );
										System.err.println(spreadName + " " + spreadFile.exists() );
										
										ModelImage spread = fileIO.readImage(spreadName, baseFileDir + File.separator, false, null);  
										
										if ( (toTwisted != null) && (spread != null) )
										{
											VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
											Vector<Integer> spreadAmount = new Vector<Integer>();
											for ( int j = 0; j < annotations.getCurves().size(); j++ )
											{
												VOIText text = (VOIText) annotations.getCurves().elementAt(j);
												Vector3f pos = text.elementAt(0);
												String name = text.getText();

												float valid = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 0 );
												if ( valid != 1 )
												{
													System.err.println( name + " invalid position" );
												}
												float x = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 1 );
												float y = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 2 );
												float z = toTwisted.getFloatC( (int)pos.X, (int)pos.Y, (int)pos.Z, 3 );
												Vector3f newPos = new Vector3f(x, y, z);
												
												VOIText newText = new VOIText();
												newText.setText(name);
												newText.add(newPos);

												annotationVOI.getCurves().add(newText);
												

												int s = spread.getInt( (int)x, (int)y, (int)z );
												spreadAmount.add(s);
											}
											String fileBase = inputFileText.getText().substring(0, inputFileText.getText().length() - 4);
											saveMarkerPositions( fileBase + "_twisted.csv", annotationVOI, spreadAmount );
										}
										else
										{
											MipavUtil.displayError( "Error in reading image file " + toTwistedName );
										}
									}
								}
							}
						}
					}
				}
				else
				{
					MipavUtil.displayError( "Error in reading image file " + fileName );
				}

			}
			System.err.println("Done plugin" );
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
		setTitle("Untwisting C.elegans - Twisted to Untwisted Conversion - 1.0");
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

		baseFileLocText = gui.buildFileField("Data directory: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", "             ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		inputFileText = gui.buildFileField("Input cordinate file:  ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(inputFileText.getParent(), gbc);
		gbc.gridy++;
		
		ButtonGroup group = new ButtonGroup();
		untwistedToTwisted = gui.buildRadioButton( "untwisted->twisted", true);
		inputsPanel.add(untwistedToTwisted.getParent(), gbc);
		group.add(untwistedToTwisted);
		gbc.gridy++;
		twistedToUntwisted = gui.buildRadioButton( "twisted->untwisted", false);
		twistedToUntwisted.setEnabled(false);
		inputsPanel.add(twistedToUntwisted.getParent(), gbc);
		group.add(twistedToUntwisted);
		

		gbc.gridy++;
		group = new ButtonGroup();
		voxelCoords = gui.buildRadioButton( "voxels", false);
		inputsPanel.add(voxelCoords.getParent(), gbc);
		group.add(voxelCoords);
		gbc.gridy++;
		micronCoords = gui.buildRadioButton( "microns", true);
		inputsPanel.add(micronCoords.getParent(), gbc);
		group.add(micronCoords);

		getContentPane().add(inputsPanel, BorderLayout.NORTH);

		JPanel okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);


		pack();
		setResizable(true);

		System.gc();
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
					System.err.println( text.getText() + " " + text.elementAt(0) );
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


	private void saveMarkerPositions( String fileName, VOI annotations, Vector<Integer> spreadAmount)
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
			bw.write( "Name,x,y,z\n");
//			bw.write( "Name,x,y,z,spread\n");
			for ( int i = 0; i < annotations.getCurves().size(); i++ )
			{
				VOIText text = (VOIText) annotations.getCurves().elementAt(i);
				Vector3f pos = text.elementAt(0);
				Vector3f out = new Vector3f();
				if ( micronCoords.isSelected() )
				{
					MipavCoordinateSystems.fileToScanner(pos, out, wormImage);
				}
				else
				{
					out.copy(pos);
				}
//				int spread = spreadAmount != null ? spreadAmount.elementAt(i) : -1;
//				bw.write( text.getText() + "," + out.X + "," + out.Y + "," + out.Z + "," + spread + "\n" );
				bw.write( text.getText() + "," + out.X + "," + out.Y + "," + out.Z + "\n" );
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
		baseFileDir = baseFileLocText.getText();
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
}
