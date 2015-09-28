
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
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

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
	private VOIVector[] potentialLattices;
	private int imageIndex = 0;
	private JDialogStandalonePlugin dialogGUI;
	private JPanel inputsPanel;
	private JPanel buttonPanel;
	private JPanel latticeSelectionPanel;
	private JPanel algorithmsPanel;
	private JPanel editPanel;
	private JPanel choicePanel;
	private boolean editEnabled = false;
	private boolean batchProcess = false;
	private JRadioButton[] latticeChoices;
	private boolean latticeSelection = true;
	
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
		Object source = event.getSource();
		if (command.equals("start"))
		{
			if (setVariables()) 
			{
				if ( batchProcess )
				{
					if ( segmentSeamCells.isSelected() )
					{
						PlugInDialogWormLatticeStraighten.segmentSeamCells( includeRange, baseFileDir, baseFileNameText.getText() );
					}
					if ( buildLattice.isSelected() )
					{
						PlugInDialogWormLatticeStraighten.buildLattice( includeRange, baseFileDir, baseFileNameText.getText());
					}
					if ( latticeStraighten.isSelected() )
					{
						PlugInDialogWormLatticeStraighten.latticeStraighten( includeRange,  baseFileDir, baseFileNameText.getText() );
					}
					if ( calcMaxProjection.isSelected() )
					{
						PlugInDialogWormLatticeStraighten.createMaximumProjectionAVI( includeRange,  baseFileDir, baseFileNameText.getText() );
					}
					setVisible(false);
				}
				else if ( editEnabled )
				{
					if ( editSeamCells.isSelected() )
					{
						openSeamCells();
					}
					else if ( editLattice.isSelected() )
					{
						openLattice();
					}
					startButton.setText("back");
					startButton.setActionCommand("back");
					startButton.setEnabled(false);
					nextButton.setVisible(true);
					inputsPanel.setVisible(false);
					dialogGUI.getContentPane().remove(inputsPanel);
					dialogGUI.getContentPane().remove(choicePanel);
					dialogGUI.getContentPane().add(gpuPanel, BorderLayout.CENTER );
					if ( editLattice.isSelected() && latticeSelection )
					{
						System.err.println( "add lattice selection" );
						buttonPanel.add( latticeSelectionPanel );
					}
					pack();
				}
			}
		}
		else if ( command.equals("next") )
		{
			save();
			imageIndex++;

			if ( editSeamCells.isSelected() )
			{
				openSeamCells();
			}
			else if ( editLattice.isSelected() )
			{
				openLattice();
				latticeSelectionPanel.setVisible( latticeSelection );
			}
		}
		else if ( command.equals("back") )
		{
			save();
			imageIndex--;
			if ( editSeamCells.isSelected() )
			{
				openSeamCells();
			}
			else if ( editLattice.isSelected() )
			{
				openLattice();
				latticeSelectionPanel.setVisible( latticeSelection );
			}
		}
		else if (command.equals("close"))
		{
			if ( editEnabled )
			{
				save();
			}
			setVisible(false);
			dispose();
		}
		if ( includeRange != null )
		{
			imageIndex = Math.min( includeRange.size() - 1, imageIndex );
			imageIndex = Math.max( 0, imageIndex );
			
			startButton.setEnabled( imageIndex > 0 );
			nextButton.setEnabled( imageIndex < (includeRange.size() - 1));
		}
		latticeStraighten.setEnabled(none.isSelected());
		buildLattice.setEnabled(none.isSelected());
		segmentSeamCells.setEnabled(none.isSelected());
		calcMaxProjection.setEnabled(none.isSelected());
		if ( (source == latticeStraighten) || (source == buildLattice) || (source == segmentSeamCells) || (source == calcMaxProjection) )
		{
			if ( ((JCheckBox)source).isSelected() )
			{
				editSeamCells.setEnabled(false);
				editLattice.setEnabled(false);
				none.setSelected(true);
			}
			else if ( !latticeStraighten.isSelected() && !buildLattice.isSelected() && !segmentSeamCells.isSelected() && !calcMaxProjection.isSelected() )
			{
				editSeamCells.setEnabled(true);
				editLattice.setEnabled(true);
			}
		}
		if ( latticeChoices != null )
		{
			for ( int i = 0; i < latticeChoices.length; i++ )
			{
				if ( (source == latticeChoices[i]) && (latticeChoices[i].isSelected()) )
				{
					if ( voiManager != null )
					{
//						System.err.println( "Switching lattices to " + i );
						voiManager.setLattice(potentialLattices[i]);
					}
				}
			}
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
		if ( editSeamCells.isSelected() )
		{
			if ( annotations != null )
			{
				if ( annotations.size() > 0 )
				{
					voiManager.setAnnotations(annotations);


					for (int i = 0; i < annotations.elementAt(0).getCurves().size(); i++)
					{
						final VOIText text = (VOIText) annotations.elementAt(0).getCurves().elementAt(i);
						text.createVolumeVOI( volumeImage, volumeRenderer.getTranslate() );    			
					}
				}
			}
			voiManager.editAnnotations();
		}
		else if ( editLattice.isSelected() )
		{
			if ( potentialLattices != null )
			{
				if ( potentialLattices[0] != null )
				{
					voiManager.setLattice(potentialLattices[0]);
				}
			}
			voiManager.editLattice();
		}

		volumeRenderer.displayVolumeSlices(false);
		volumeRenderer.displayVolumeRaycast(true);
		volumeRenderer.displayVOIs(true);
		volumeRenderer.setVolumeBlend(.8f);
		volumeRenderer.setABBlend(.8f);
		volumeRenderer.initLUT();
	}

	protected void openSeamCells()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					gpuPanel.setBorder(JDialogBase.buildTitledBorder(fileName));
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

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editSeamCellOutput;
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);

					if ( annotations.size() == 0 )
					{
						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.autoSeamCellSegmentationOutput;
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);
					}
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

	protected void openLattice()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					gpuPanel.setBorder(JDialogBase.buildTitledBorder(fileName));
					FileIO fileIO = new FileIO();
					if(wormImage != null) {
						wormImage.disposeLocal();
						wormImage = null;
					}
					wormImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
					wormImage.calcMinMax();
					
					if ( potentialLattices != null )
					{
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							potentialLattices[i].clear();
						}
					}
					if ( potentialLattices == null )
					{
						potentialLattices = new VOIVector[5];
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							potentialLattices[i] = new VOIVector();
						}
					}

					latticeSelection = false;
					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editLatticeOutput;
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, potentialLattices[0], false);

					if ( potentialLattices[0].size() == 0 )
					{
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.autoLatticeGenerationOutput + i;
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, potentialLattices[i], false);
						}
						if ( potentialLattices[0].size() != 0 )
						{
							latticeSelection = true;
						}
					}
					if ( voiManager != null )
					{
						voiManager.setImage(wormImage);
						voiManager.setLattice(potentialLattices[0]);
						voiManager.editLattice();
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

	private JCheckBox latticeStraighten;
	private JCheckBox buildLattice;
	private JCheckBox segmentSeamCells;
	private JCheckBox calcMaxProjection;
	
	private JPanel makeAlgorithmsPanel(GuiBuilder gui)
	{
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Batch Algorithms"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		segmentSeamCells = gui.buildCheckBox("segment seam cells", false );
		segmentSeamCells.addActionListener(this);
		panel.add(segmentSeamCells.getParent(), gbc);
		gbc.gridy++;
		
		gbc.gridx = 0;
		buildLattice = gui.buildCheckBox("generate lattices", false );
		buildLattice.addActionListener(this);
		panel.add(buildLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		latticeStraighten = gui.buildCheckBox("straighten", false );
		latticeStraighten.addActionListener(this);
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		calcMaxProjection = gui.buildCheckBox("generate maximum intensity projection animation", false );
		calcMaxProjection.addActionListener(this);
		panel.add(calcMaxProjection.getParent(), gbc);
		gbc.gridy++;
		
		return panel;
	}
	
	private JRadioButton editSeamCells;
	private JRadioButton editLattice;
	private JRadioButton none;
	
	private JPanel makeEditPanel(GuiBuilder gui)
	{
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Build / Edit"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		editSeamCells = gui.buildRadioButton("edit seam cells", false );
		editSeamCells.addActionListener(this);
		editSeamCells.setActionCommand("editSeamCells");
		panel.add(editSeamCells.getParent(), gbc);
		gbc.gridy++;
		
		gbc.gridx = 0;
		editLattice = gui.buildRadioButton("edit lattice", false );
		editLattice.addActionListener(this);
		editLattice.setActionCommand("editLattice");
		panel.add(editLattice.getParent(), gbc);
		gbc.gridy++;
		
		gbc.gridx = 0;
		none = gui.buildRadioButton("none", true );
		none.addActionListener(this);
		none.setActionCommand("none");
		panel.add(none.getParent(), gbc);
		

        ButtonGroup group = new ButtonGroup();
        group.add(editSeamCells);
        group.add(editLattice);
        group.add(none);
		
        ButtonGroup latticeGroup = new ButtonGroup();        
        latticeSelectionPanel = new JPanel();
        latticeChoices = new JRadioButton[5];
        for ( int i = 0; i < latticeChoices.length; i++ )
        {
        	latticeChoices[i] = gui.buildRadioButton("lattice_" + i, i==0 );
        	latticeChoices[i].addActionListener(this);
        	latticeGroup.add(latticeChoices[i]);
        	latticeSelectionPanel.add(latticeChoices[i]);
        }
        
		return panel;
	}
	
	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - lattice - 1.0");
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

		inputsPanel = new JPanel(new GridBagLayout());
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		inputsPanel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Directory containing input images: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		nextButton = gui.buildButton("next");
		nextButton.addActionListener(this);
		nextButton.setVisible(false);
		buttonPanel.add( nextButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());
//		panel.add(buttonPanel, gbc);

		gpuPanel = new JPanel(new BorderLayout());
		setLocation(100, 100);

		final int imagePanelWidth = (int) (Toolkit.getDefaultToolkit().getScreenSize().width * 0.5f);
		final int imagePanelHeight = (int) (Toolkit.getDefaultToolkit().getScreenSize().height * 0.5f);

		gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
		gpuPanel.setMinimumSize(new Dimension(250, 250));
		gpuPanel.setBorder(JDialogBase.buildTitledBorder("Volume Display"));
		
		algorithmsPanel = makeAlgorithmsPanel(gui);
		editPanel = makeEditPanel(gui);
		
		choicePanel = new JPanel( new GridLayout(1,2) );
		choicePanel.add(algorithmsPanel);
		choicePanel.add(editPanel);
		
		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(choicePanel, BorderLayout.CENTER);
//		dialogGUI.getContentPane().add(algorithmsPanel, BorderLayout.CENTER);
//		dialogGUI.getContentPane().add(editPanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);

		pack();
		setResizable(true);

		//        gpuPanel.setVisible(true);
	}
	
	private void save()
	{
		if ( editSeamCells.isSelected() )
		{
			saveSeamCells();
		}
		else if ( editLattice.isSelected() )
		{
			saveLattice();
		}
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

		String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editSeamCellOutput;  
		String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
		WormSegmentation.saveAllVOIsTo(voiDir, wormImage);
	}

	private void saveLattice()
	{
		if ( wormImage == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
		voiManager.saveLattice( baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(imageIndex) + File.separator, 
				PlugInDialogWormLatticeStraighten.editLatticeOutput );
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
		
		editEnabled = (editSeamCells.isSelected() || editLattice.isSelected());
		batchProcess = (latticeStraighten.isSelected() || buildLattice.isSelected() || segmentSeamCells.isSelected() || calcMaxProjection.isSelected() );
		return (includeRange != null);
	}

}
