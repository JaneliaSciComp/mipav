
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
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.JPanelVolumeOpacity;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationKMeans;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationLoG;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationWindowing;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

public class PlugInDialogVolumeRender extends JFrame implements ActionListener, AlgorithmInterface {

	private JButton startButton;
	private JButton nextButton;
	private JButton closeButton;
	private JTextField  baseFileLocText;
	private JTextField  baseFileNameText;
	private JTextField rangeFusionText;
	private String baseFileDir;
	private Vector<Integer> includeRange;
	private ModelImage wormImage;
	private JPanel gpuPanel;
	private VolumeTriPlanarRender volumeRenderer;
	private VolumeImage volumeImage;
	private VOILatticeManagerInterface voiManager;
	private VOIVector annotations;
	private int imageIndex = 0;
	private JDialogStandalonePlugin dialogGUI;

	public PlugInDialogVolumeRender() {}

	public PlugInDialogVolumeRender(boolean modal)
	{
		init();
		setVisible(true);
	}

	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 *
	 * @param  event  Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		if (command.equals("start"))
		{
			if (setVariables()) {
				callAlgorithm();
			}
		}
		else if ( command.equals("next") )
		{
//			saveSeamCells();
			imageIndex++;
			callAlgorithm();
		}
		else if (command.equals("close"))
		{
//			saveSeamCells();
			setVisible(false);
			dispose();
		}
		if ( includeRange != null )
		{
			imageIndex = Math.min( includeRange.size() - 1, imageIndex );
			imageIndex = Math.max( 0, imageIndex );
		}
	} 

	public void algorithmPerformed(AlgorithmBase algorithm)
	{        
		float min = (float) wormImage.getMin();
		float max = (float) wormImage.getMax();
		TransferFunction kTransfer = new TransferFunction();
		kTransfer.removeAll();
		kTransfer.addPoint(min, 255);
		kTransfer.addPoint((min + ((max - min) / 3.0f)), 255 * 0.67f);
		kTransfer.addPoint((min + ((max - min) * 2.0f / 3.0f)), 255 * 0.333f);
		kTransfer.addPoint(max, 0);
		volumeRenderer.getVolumeImage().UpdateImages(kTransfer, 0, null);

		voiManager = new VOILatticeManagerInterface( null, volumeImage.GetImage(), null, 0, true, null );
		volumeRenderer.setVOILatticeManager(voiManager);
		if ( annotations != null )
		{
			voiManager.setAnnotations(annotations);


			for (int i = 0; i < annotations.elementAt(0).getCurves().size(); i++)
			{
				final VOIText text = (VOIText) annotations.elementAt(0).getCurves().elementAt(i);
				text.createVolumeVOI( volumeImage, volumeRenderer.getTranslate() );    			
			}
		}

		volumeRenderer.displayVolumeSlices(false);
		volumeRenderer.displayVolumeRaycast(true);
		volumeRenderer.displayVOIs(true);
		volumeRenderer.setVolumeBlend(.8f);
		volumeRenderer.setABBlend(.8f);
		volumeRenderer.initLUT();
		voiManager.editAnnotations();
	}

	/**
	 * Once all the necessary variables are set, call the kidney segmentation algorithm
	 */
	protected void callAlgorithm()
	{
		if ( includeRange != null )
		{			
			if ( imageIndex < includeRange.size() )
			{
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					setTitle("Volume Renderer " + fileName );
					FileIO fileIO = new FileIO();
					if(wormImage != null) {
						wormImage.disposeLocal();
						wormImage = null;
					}
					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
					wormImage.calcMinMax();      

					if ( annotations != null )
					{
						annotations.clear();
						annotations = null;
					}
					if ( annotations == null )
					{
						annotations = new VOIVector();
					}

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + "seam_cells_checked";
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);

					if ( annotations.size() == 0 )
					{

						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + "seam_cells";
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);

						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + "annotations";    
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);
							if ( annotations.size() == 0 )
							{
								fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + "annotation";    
								voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
								PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);
							}
						}
					}
					//					System.err.println( annotations.elementAt(0).getCurves().size() );
					if ( voiManager != null )
					{
						voiManager.setImage(wormImage);
						voiManager.setAnnotations(annotations);
						voiManager.editAnnotations();
					}
				}
				if ( volumeImage == null )
				{
					volumeImage = new VolumeImage(false, wormImage, "", null, 0);
				}
				else
				{
					volumeImage.UpdateData(wormImage);
				}
				if ( volumeRenderer == null )
				{
					volumeRenderer = new VolumeTriPlanarRender(volumeImage);
					volumeRenderer.addConfiguredListener(this);
					gpuPanel.add(volumeRenderer.GetCanvas(), BorderLayout.CENTER);
					gpuPanel.setVisible(true);
					pack();				
				}
				else
				{
					volumeRenderer.initLUT();
				}
			}
		}
	}

	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Volume Renderer");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		dialogGUI = new JDialogStandalonePlugin();
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

		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		panel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Directory containing input images: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		panel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		panel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		panel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		JPanel buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		nextButton = gui.buildButton("next");
		nextButton.addActionListener(this);
		buttonPanel.add( nextButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());
		panel.add(buttonPanel, gbc);

		gpuPanel = new JPanel(new BorderLayout());
		setLocation(100, 100);

		final int imagePanelWidth = (int) (Toolkit.getDefaultToolkit().getScreenSize().width * 0.5f);
		final int imagePanelHeight = (int) (Toolkit.getDefaultToolkit().getScreenSize().height * 0.5f);

		gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
		gpuPanel.setMinimumSize(new Dimension(250, 250));
		gpuPanel.setBorder(JDialogBase.buildTitledBorder("Volume Display"));


		dialogGUI.getContentPane().add(panel, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(gpuPanel, BorderLayout.CENTER);

		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);

		pack();
		setResizable(true);

		//        gpuPanel.setVisible(true);
	}

	private boolean setVariables()
	{	    
		baseFileDir = baseFileLocText.getText();
		includeRange = new Vector<Integer>();
		String rangeFusion = rangeFusionText.getText();
		if( rangeFusion != null )
		{  
			String[] ranges = rangeFusion.split("[,;]");
			for( int i = 0; i < ranges.length; i++ )
			{
				String[] subset = ranges[i].split("-");
				int lowerBound = -1, bound = -1;
				for( int j = 0; j < subset.length; j++ )
				{
					try {
						bound = Integer.valueOf(subset[j].trim());
						if( lowerBound == -1 )
						{
							lowerBound = bound;
							includeRange.add(lowerBound);
						} 
					} catch(NumberFormatException e) {
						Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
					}
				}

				for( int k = lowerBound + 1; k <= bound; k++ )
				{
					includeRange.add(k);
				}
			}
		}

		if( includeRange.size() == 0 ) 
		{
			includeRange = null;
		}
		imageIndex = 0;
		return true;
	}

	private void saveSeamCells()
	{
		if ( wormImage == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}

		String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + "seam_cells_checked";  
		String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
		WormSegmentation.saveAllVOIsTo(voiDir, wormImage);
	}
}
