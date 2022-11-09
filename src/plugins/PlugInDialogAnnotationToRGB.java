
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


public class PlugInDialogAnnotationToRGB extends JDialogStandalonePlugin implements AlgorithmInterface
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5858343401705223389L;
	private ModelImage wormImage;

	private String wormImageFileName;
	private String annotationFileName;
	private JTextField wormImageInput;
	private JTextField annotationInput;
	
	private String baseFileDir;
	private JTextField  baseFileLocText;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;
	private JTextField rangeFusionText;

	public PlugInDialogAnnotationToRGB()
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
	public void algorithmPerformed(AlgorithmBase algorithm) {}

	public void dispose()
	{
		super.dispose();
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
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
//		if ( includeRange != null )
//		{
//			System.err.println("Starting plugin" );
//			for ( int i = 0; i < includeRange.size(); i++ )
//			{				
				// Build the full image name:
//				baseFileName = baseFileNameText.getText();
//				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
//				File imageFile = new File(baseFileDir + File.separator + fileName);

				File imageFile = new File(wormImageFileName);
				if ( imageFile.exists() )
				{	
//					System.err.println( "   " + fileName );
					FileIO fileIO = new FileIO();
					if ( wormImage != null )
					{
						wormImage.disposeLocal(false);
						wormImage = null;
					}
					String name = wormImageFileName.substring( 1+ wormImageFileName.lastIndexOf(File.separator, wormImageFileName.length()));
					String dir = wormImageFileName.substring(0, 1+ wormImageFileName.lastIndexOf(File.separator));
					
					wormImage = fileIO.readImage(name, dir, false, null);  
					System.err.println( "Is color? " + wormImage.isColorImage() );

//					String positionsFile = baseFileName + "_" + includeRange.elementAt(i) + "_positions.csv";
//					System.err.println( "   " + positionsFile );
//					File textFile = new File(baseFileDir + File.separator + "Positions" + File.separator + positionsFile);
					
					File textFile = new File(annotationFileName);
					if ( textFile.exists() )
					{			
						VOIVector vois = readMarkerPositions(annotationFileName, textFile);
						VOI annotations = vois.elementAt(0);

						if ( annotations != null )
						{
							if ( annotations.getCurves().size() > 0 )
							{
								ModelImage annotationImage = new ModelImage( ModelStorageBase.ARGB, wormImage.getExtents(), 
										JDialogBase.makeImageName(wormImage.getImageName(), "_annotation_rgb"));
								JDialogBase.updateFileInfo(wormImage, annotationImage);
								int dimX = wormImage.getExtents().length > 0 ? wormImage.getExtents()[0] : 1;
								int dimY = wormImage.getExtents().length > 1 ? wormImage.getExtents()[1] : 1;
								int dimZ = wormImage.getExtents().length > 2 ? wormImage.getExtents()[2] : 1;
								float dataSize = dimX*dimY*dimZ;
								for ( int j = 0; j < dataSize; j++ )
								{
									if ( wormImage.isColorImage() )
									{
										annotationImage.set(j*4+1, 255 * (wormImage.getFloat(j*4+1)/wormImage.getMaxR()));
										annotationImage.set(j*4+2, 255 * (wormImage.getFloat(j*4+2)/wormImage.getMaxG()));
										annotationImage.set(j*4+3, 255 * (wormImage.getFloat(j*4+3)/wormImage.getMaxB()));
									}
									else
									{
										annotationImage.set(j*4+1, 255 * (wormImage.getFloat(j)/wormImage.getMax()));
									}
								}

								for ( int j = 0; j < annotations.getCurves().size(); j++ ) {
									VOIText text = (VOIText) annotations.getCurves().elementAt(j);
									Vector3f pos = text.elementAt(0);
									int xC = Math.round(pos.X);
									int yC = Math.round(pos.Y);
									int z = Math.round(pos.Z);
									Vector3f test = new Vector3f();
									for ( int x = Math.max( 0, xC - 3); x <= Math.min( dimX -1, xC + 3); x++ ) {
										for ( int y = Math.max( 0, yC - 3); y <= Math.min( dimY -1, yC + 3); y++ ) {
											test.set(x, y, z );
											if ( test.distance(pos) <= 3 )
											{
												annotationImage.setC( x, y, z, 3, 255);
											}
										}
									}
								}
								ModelImage.saveImage(annotationImage, annotationImage.getImageName() + ".tif", wormImage.getImageDirectory(), false);
								if ( annotationImage != null )
								{
									annotationImage.calcMinMax();
									new ViewJFrameImage(annotationImage);
//									annotationImage.disposeLocal(false);
//									annotationImage = null;
								}
							}
						}
					}
				}
				else
				{
					MipavUtil.displayError( "Error in reading image file " + wormImageFileName );
				}

//			}
//			System.err.println("Done plugin" );
//		}
		if ( wormImage != null )
		{
			wormImage.disposeLocal(false);
			wormImage = null;
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


		wormImageInput = gui.buildFileField("wormImage: ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(wormImageInput.getParent(), gbc);
		gbc.gridy++;
		annotationInput = gui.buildFileField("annotation file: ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(annotationInput.getParent(), gbc);
		gbc.gridy++;
		
//		baseFileLocText = gui.buildFileField("Data directory: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
//		inputsPanel.add(baseFileLocText.getParent(), gbc);
//		gbc.gridy++;
//
//		baseFileNameText = gui.buildField("Base images name: ", "Decon");
//		inputsPanel.add(baseFileNameText.getParent(), gbc);
//		gbc.gridy++;
//
//		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", "             ");
//		inputsPanel.add(rangeFusionText.getParent(), gbc);
//		gbc.gridy++;

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




	/**
	 * This method could ensure everything in your dialog box has been set correctly
	 * 
	 * @return
	 */
	private boolean setVariables()
	{	    
		wormImageFileName = wormImageInput.getText();
		annotationFileName = annotationInput.getText();
		
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
