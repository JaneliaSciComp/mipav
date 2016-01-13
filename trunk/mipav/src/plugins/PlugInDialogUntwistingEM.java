
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
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModelEM;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogUntwistingEM extends JFrame implements ActionListener,  WindowListener {

	private static final long serialVersionUID = -8451902403280342311L;
	private JTextField  inputImageTF;
	private JTextField  nucleiTF;
	private JTextField  scaleTF;	
	private JButton startButton;

	private ModelImage wormImage;
	private ModelImage maskImage;
	
	
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
			if ( maskImage != null ) {
				maskImage.disposeLocal();
				maskImage = null;
			}

//			System.err.println( inputImageTF.getText() );
//			System.err.println( nucleiTF.getText() );
//			System.err.println( scaleTF.getText() );
			int scale = Integer.valueOf(scaleTF.getText() );
			
			if ( inputImageTF.getText().length() > 0 )
			{
				File imageFile = new File(inputImageTF.getText());
				if ( imageFile.exists() )
				{
					String fileName = inputImageTF.getText().substring(inputImageTF.getText().lastIndexOf(File.separator) + 1);
					String fileDir = inputImageTF.getText().substring(0, inputImageTF.getText().lastIndexOf(File.separator) + 1);
//					System.err.println( fileDir );
//					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					wormImage = fileIO.readImage(fileName, fileDir, false, null); 
					wormImage.calcMinMax();

					String baseName = JDialogBase.makeImageName( wormImage.getImageName(), "" );
					fileName = JDialogBase.makeImageName( wormImage.getImageName(), "_mask_image.tif" );
					imageFile = new File(fileDir + fileName);
					if ( imageFile.exists() )
					{
						if ( maskImage != null )
						{
							maskImage.disposeLocal();
							maskImage = null;
						}
//						System.err.println( fileName );
						maskImage = fileIO.readImage(fileName, fileDir, false, null);
						maskImage.calcMinMax();
					}
					
					VOIVector latticeVector = new VOIVector();
					fileName = baseName + File.separator + PlugInAlgorithmWormUntwisting.editLatticeOutput;
					String voiDir = new String(fileDir + fileName + File.separator);
					PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImage, voiDir, true, latticeVector, false);
					if ( latticeVector.size() != 0 )
					{
						LatticeModelEM model = new LatticeModelEM(wormImage);
						model.setLattice(latticeVector.elementAt(0));
						model.setMaskImage(maskImage);
						
						if ( nucleiTF.getText().length() > 0 )
						{
							File nucleiFile = new File(nucleiTF.getText());
							if ( nucleiFile.exists() )
							{
								fileName = nucleiTF.getText().substring(nucleiTF.getText().lastIndexOf(File.separator) + 1);
								fileDir = nucleiTF.getText().substring(0, nucleiTF.getText().lastIndexOf(File.separator) + 1);
//								System.err.println( fileDir );
//								System.err.println( fileName );
								VOIVector vois = readNucleiPositions( fileName, nucleiFile, scale );
								VOI nucleiVOIs = vois.elementAt(0);
								if ( (nucleiVOIs != null) && (nucleiVOIs.getCurves().size() > 0) )
								{
									model.setNucleiMarkers(nucleiVOIs);
//									wormImage.setVOIs(vois);
								}
							}
						}
//						new ViewJFrameImage((ModelImage)wormImage.clone());
						model.interpolateLattice( false );
						System.err.println("Done straightening" );
						model.dispose();
						model = null;
					}
				}
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
		if ( maskImage != null )
		{
			maskImage.disposeLocal();
			maskImage = null;
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

		inputImageTF = gui.buildFileField("Worm image:  ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(inputImageTF.getParent(), gbc);
		gbc.gridy++;

		nucleiTF = gui.buildFileField("Nuclei info: ", "", false, JFileChooser.FILES_ONLY, this);
		inputsPanel.add(nucleiTF.getParent(), gbc);
		gbc.gridy++;

		scaleTF = gui.buildIntegerField("rescale factor: ", 1 );
		inputsPanel.add(scaleTF.getParent(), gbc);
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
	
	private VOIVector readNucleiPositions( String fileName, File file, float scale )
	{
		VOIVector vois = new VOIVector();
		VOI annotationVOI = new VOI( (short)0, fileName, VOI.ANNOTATION, 0 );
		VOI circleVOI = new VOI( (short)1, fileName, VOI.POLYLINE, 0 );
		vois.add(annotationVOI);
		vois.add(circleVOI);
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
					pos.X = Float.valueOf(st.nextToken())/scale;
				}
				if (st.hasMoreTokens()) {
					pos.Y = Float.valueOf(st.nextToken())/scale;
				}
				if (st.hasMoreTokens()) {
					pos.Z = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					radius = Float.valueOf(st.nextToken())/scale;
					if ( !pos.isEqual(Vector3f.ZERO ) )
					{
						text.add(pos);
						Vector3f r = new Vector3f(pos);
						r.X += radius;
						text.add(r);
					}
					circleVOI.getCurves().add(makeEllipse2D(text.elementAt(0), radius) );
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
