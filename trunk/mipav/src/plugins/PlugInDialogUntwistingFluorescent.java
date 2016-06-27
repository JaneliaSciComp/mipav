
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
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistingFluorescent extends JFrame implements ActionListener,  WindowListener {

	private static final long serialVersionUID = -8451902403280342311L;
	private JTextField  inputImageTF;
	private JTextField  latticeFileTF;
	private JTextField  nucleiTF;
	private JCheckBox straightenImageCheck;
	private JCheckBox straightenMarkersCheck;
	private JCheckBox segmentSkinSurfaceCheck;
	private JButton startButton;

	private ModelImage wormImage;


	public PlugInDialogUntwistingFluorescent()
	{
		init();
		setVisible(true);
		addWindowListener(this);
	}

	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		
		if (command.equals("start"))
		{			
			if ( wormImage != null ) {
				wormImage.disposeLocal();
				wormImage = null;
			}
			
			if ( inputImageTF.getText().length() > 0 )
			{
				File imageFile = new File(inputImageTF.getText());
				if ( imageFile.exists() )
				{					
					FileIO fileIO = new FileIO();
					wormImage = fileIO.readImage(inputImageTF.getText());
					
					
					VOIVector latticeVector = new VOIVector();
					String latticeFile = latticeFileTF.getText() + File.separator;
					if ( (latticeFileTF.getText().length() > 0) )
					{
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, latticeFile, true, latticeVector, false);
					}
					
					if ( latticeVector.size() != 0 )
					{
						LatticeModel model = new LatticeModel(wormImage);
						
						if ( model != null )
						{
							model.setLattice(latticeVector.elementAt(0));

							if ( nucleiTF.getText().length() > 0 )
							{
								File nucleiFile = new File(nucleiTF.getText());
								if ( nucleiFile.exists() )
								{
									String fileName = nucleiTF.getText().substring(nucleiTF.getText().lastIndexOf(File.separator) + 1);
									VOIVector vois = readMarkerPositions( fileName, nucleiFile );
									VOI nucleiVOIs = vois.elementAt(0);
									if ( (nucleiVOIs != null) && (nucleiVOIs.getCurves().size() > 0) )
									{
										model.setMarkers(nucleiVOIs);
									}
								}
							}
							System.err.println("Starting straightening" );
							model.interpolateLattice( false, false, straightenImageCheck.isSelected(), false );
							if ( segmentSkinSurfaceCheck.isSelected() )
							{
								model.segmentSkin(wormImage, false);
							}
							if ( straightenMarkersCheck.isSelected() )
							{
								model.interpolateLattice( false, false, false, true );								
							}
							
							model.dispose();
							model = null;
							if ( wormImage != null )
							{
								wormImage.disposeLocal(false);
							}
							System.err.println("Done straightening" );
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
		setTitle("Untwisting C.elegans - Fluorescent images - 1.0");
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

		inputImageTF = gui.buildFileField("Worm image:  ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(inputImageTF.getParent(), gbc);
		gbc.gridy++;

		latticeFileTF = gui.buildFileField("lattice directory:  ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(latticeFileTF.getParent(), gbc);
		gbc.gridy++;

		nucleiTF = gui.buildFileField("Marker info (spreadsheet csv file): ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(nucleiTF.getParent(), gbc);
		gbc.gridy++;
		gbc.gridx = 0;
		
		straightenImageCheck = gui.buildCheckBox( "Straighten Image", true );
		inputsPanel.add(straightenImageCheck.getParent(), gbc);
		gbc.gridy++;
		
		straightenMarkersCheck = gui.buildCheckBox( "Straighten Markers", true );
		inputsPanel.add(straightenMarkersCheck.getParent(), gbc);
		gbc.gridy++;
		
		segmentSkinSurfaceCheck = gui.buildCheckBox( "Segment Skin Surface Marker", true );
		inputsPanel.add(segmentSkinSurfaceCheck.getParent(), gbc);
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
					MipavCoordinateSystems.scannerToFile(pos, out, wormImage);
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
}
