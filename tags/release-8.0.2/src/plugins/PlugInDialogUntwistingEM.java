
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

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistingEM extends JFrame implements ActionListener,  WindowListener {

	private static final long serialVersionUID = -8451902403280342311L;
	private JTextField  inputImageTF;
	private JTextField  latticeFileTF;
//	private JTextField  maskImageTF;
//	private JTextField  modelImageTF;
	private JTextField  nucleiTF;
	private JTextField  latticeScaleTF;	
	private JTextField  nucleiScaleTF;
	private JTextField[]  resolutionsTF;
	private JTextField[]  outputResolutionsTF;	
	private JButton startButton;

	private ModelImageLargeFormat fullImage;
	private ModelImage wormImage;
//	private ModelImage wormModelImage;
//	private ModelImage maskImage;


	public PlugInDialogUntwistingEM()
	{
		init();
		setVisible(true);
		addWindowListener(this);
	}

	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();

		if (command.equals("start"))
		{			
			if ( wormImage != null ) {
				wormImage.disposeLocal();
				wormImage = null;
			}
			
			float scale = Float.valueOf(nucleiScaleTF.getText() );
			float latticeScale = Float.valueOf(latticeScaleTF.getText() );
			float resX = Float.valueOf( resolutionsTF[0].getText() );
			float resY = Float.valueOf( resolutionsTF[1].getText() );
			float resZ = Float.valueOf( resolutionsTF[2].getText() );
			float outputResX = Float.valueOf( outputResolutionsTF[0].getText() );
			float outputResZ = Float.valueOf( outputResolutionsTF[1].getText() );

			if ( inputImageTF.getText().length() > 0 )
			{
				File imageFile = new File(inputImageTF.getText());
				if ( imageFile.exists() )
				{
					String dirName = inputImageTF.getText().substring(inputImageTF.getText().lastIndexOf(File.separator) + 1);
			        
					boolean largeFormat = false;
					if ( imageFile.isDirectory() )
					{
						String[] list = imageFile.list();
						int min = Integer.MAX_VALUE;
						int max = -1;
						for ( int i = 0; i < list.length; i++ )
						{
							int value = ModelImageLargeFormat.getIndex(list[i]);
							if ( value < min )
							{
								min = value;
							}
							if ( value > max )
							{
								max = value;
							}
						}
						int average = (min + max)/2;
						for ( int i = 0; i < list.length; i++ )
						{
							int value = ModelImageLargeFormat.getIndex(list[i]);
							if ( value == average )
							{
//								System.err.println( inputImageTF.getText() + " " + list[i] + " " + value );									

								FileIO fileIO = new FileIO();
								wormImage = fileIO.readImage(list[i], inputImageTF.getText() + File.separator, false, null); 
								break;
							}
						}
						if ( wormImage != null )
						{
							int[] extents2D = wormImage.getExtents();
							if ( wormImage.getType() == ModelStorageBase.ARGB )
							{	
								fullImage = new ModelImageLargeFormat( ModelImageLargeFormat.ARGB, new int[]{ extents2D[0], extents2D[1], list.length}, inputImageTF.getText(), true );
							}
							else
							{
								fullImage = new ModelImageLargeFormat( new int[]{ extents2D[0], extents2D[1], list.length}, inputImageTF.getText(), true );
							}
							fullImage.setResolutions(resX, resY, resZ);
							wormImage.disposeLocal(false);
							wormImage = null;
						}
						else
						{
							MipavUtil.displayError( "Error in reading image file " + inputImageTF.getText() + File.separator + average );
						}
						largeFormat = true;
					}
//					if ( (fullImage != null) && (latticeFileTF.getText().length() == 0) )
//					{
////						System.err.println( "masking errors" );
////						LatticeModelEM.maskErrors(fullImage);
//						System.err.println("reslice");
//						fullImage.reslize();
//					}

					VOIVector latticeVector = new VOIVector();
					String latticeFile = latticeFileTF.getText() + File.separator;
					if ( (latticeFileTF.getText().length() > 0) && (fullImage != null) )
					{
						int[] extents = new int[3];
						extents[0] = (int) (fullImage.getExtents()[0] / latticeScale);
						extents[1] = (int) (fullImage.getExtents()[1] / latticeScale);
						extents[2] = fullImage.getExtents()[2];
						ModelImage temp = new ModelImage(ModelStorageBase.INTEGER, extents, "temp" );
//						System.err.println( extents[0] + " " + extents[1] + " " + extents[2] );
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(temp, latticeFile, true, latticeVector, false);

//						VOI lattice = latticeVector.elementAt(0);
//						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//						for ( int i = 0; i < Math.min( left.size(), right.size() ); i++ )
//						{
//							Vector3f pt = left.elementAt(i);
//							pt.Z *= (1050f/157f);
//							
//							if ( pt.Z >= 1050 )
//							{
//								System.err.println( i + " " + pt );
//							}
//							
//							pt = right.elementAt(i);
//							pt.Z *= (1050f/157f);
//							
//							if ( pt.Z >= 1050 )
//							{
//								System.err.println( i + " " + pt );
//							}
//						}
//						
//						temp.registerVOI( lattice );
//						LatticeModel.saveAllVOIsTo( latticeFile + "temp" + File.separator, temp );
						temp.disposeLocal();
						temp = null;
//						return;
					}
					
					if ( latticeVector.size() != 0 )
					{
						LatticeModelEM model = null;
						if ( (fullImage != null) && largeFormat )
						{
							model = new LatticeModelEM(fullImage);
						}
						if ( model != null )
						{
							model.setLattice(latticeVector.elementAt(0), latticeScale);

							if ( nucleiTF.getText().length() > 0 )
							{
								File nucleiFile = new File(nucleiTF.getText());
								if ( nucleiFile.exists() )
								{
									String fileName = nucleiTF.getText().substring(nucleiTF.getText().lastIndexOf(File.separator) + 1);
									VOIVector vois = readNucleiPositions( fileName, nucleiFile );
									VOI nucleiVOIs = vois.elementAt(0);
									if ( (nucleiVOIs != null) && (nucleiVOIs.getCurves().size() > 0) )
									{
										model.setNucleiMarkers(nucleiVOIs, scale);
//										System.err.println("done");
									}
								}
							}
							System.err.println("Starting straightening" );
							model.interpolateLattice( outputResX, outputResX, outputResZ );
							if ( fullImage != null )
							{
								fullImage.disposeLocal(false);
								System.gc();
							}
							model.dispose();
							model = null;
							System.err.println("Done straightening" );
						}
						else
						{
							MipavUtil.displayError( "Error in reading image folder " + inputImageTF.getText() );
						}
					}
					else
					{
						MipavUtil.displayError( "Error in reading lattice file " + latticeFileTF.getText() );
					}
				}
				else
				{
					MipavUtil.displayError( "Error in reading image file " + inputImageTF.getText() );
				}
			}
			else
			{
				MipavUtil.displayError( "Must set image file." );
			}
		}
		if (command.equals("close"))
		{			
			setVisible(false);
			if ( ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
					&& ViewUserInterface.getReference().isPlugInFrameVisible() )
			{
				System.exit(0);
			} else {
				dispose();
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



	/**
	 * Initializes the panels for a non-integrated display. 
	 */
	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - EM images - 1.0");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		JDialogStandalonePlugin dialogGUI = new JDialogStandalonePlugin();
		GuiBuilder gui = new GuiBuilder(dialogGUI);

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
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		inputsPanel.setForeground(Color.black);

		inputImageTF = gui.buildFileField("Worm image:  ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(inputImageTF.getParent(), gbc);
		gbc.gridy++;

		latticeFileTF = gui.buildFileField("lattice directory:  ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(latticeFileTF.getParent(), gbc);
		gbc.gridy++;

		nucleiTF = gui.buildFileField("Nuclei info: ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(nucleiTF.getParent(), gbc);
		gbc.gridy++;

		nucleiScaleTF = gui.buildDecimalField("Nuclei rescale factor: ", 0.5 );
		inputsPanel.add(nucleiScaleTF.getParent(), gbc);
		gbc.gridy++;

		latticeScaleTF = gui.buildDecimalField("Lattice rescale factor: ", 8 );
		inputsPanel.add(latticeScaleTF.getParent(), gbc);
		gbc.gridy++;

		resolutionsTF = new JTextField[3];

		resolutionsTF[0] = gui.buildDecimalField("Input image resolutions x: ", 8 );
		inputsPanel.add(resolutionsTF[0].getParent(), gbc);
//		gbc.gridx++;
		gbc.gridy++;
		resolutionsTF[1] = gui.buildDecimalField("Input image resolutions y: ", 8 );
		inputsPanel.add(resolutionsTF[1].getParent(), gbc);
//		gbc.gridx++;
		gbc.gridy++;
		resolutionsTF[2] = gui.buildDecimalField("Input image resolutions z: ", 30 );
		inputsPanel.add(resolutionsTF[2].getParent(), gbc);
		gbc.gridx = 0;
		gbc.gridy++;
		
		outputResolutionsTF = new JTextField[2];
		outputResolutionsTF[0] = gui.buildDecimalField("output slice (x,y) resolution: ", 8 );
		inputsPanel.add(outputResolutionsTF[0].getParent(), gbc);
//		gbc.gridx++;		
		gbc.gridy++;
		outputResolutionsTF[1] = gui.buildDecimalField("output slice (z) resolution: ", 30 );
		inputsPanel.add(outputResolutionsTF[1].getParent(), gbc);
		gbc.gridx = 0;
		gbc.gridy++;
		
		JPanel buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		JButton closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());
		//		panel.add(buttonPanel, gbc);

		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);

		setLocation(0, 0);
		pack();
		setResizable(true);
	}

	private VOIVector readNucleiPositions( String fileName, File file )
	{
		VOIVector vois = new VOIVector();
		VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
//		VOI circleVOI = new VOI( (short)1, fileName, VOI.POLYLINE, 0 );
		vois.add(annotationVOI);
//		vois.add(circleVOI);
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
				StringTokenizer st = new StringTokenizer(line, "\t");
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
				}
				if (st.hasMoreTokens()) {
					radius = Float.valueOf(st.nextToken());
					if ( !pos.isEqual(Vector3f.ZERO ) )
					{
						text.add(pos);
						Vector3f r = new Vector3f(pos);
						r.X += 2*radius;
						text.add(r);
					}
//					circleVOI.getCurves().add(makeEllipse2D(text.elementAt(0), radius) );
				}
				annotationVOI.getCurves().add(text);
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

	private VOIContour makeEllipse2D( Vector3f center, float radius ) {
		VOIContour ellipse = new VOIContour(true);
		int numPts = 32;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (radius * c), Vector3f.UNIT_X);
			final Vector3f pos2 = Vector3f.scale((float) (radius * s), Vector3f.UNIT_Y);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
		return ellipse;
	}
}
