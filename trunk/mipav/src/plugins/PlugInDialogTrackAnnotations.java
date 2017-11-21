
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
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcatMult3Dto4D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.JPanelVolumeOpacity;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogTrackAnnotations extends JFrame implements ActionListener, AlgorithmInterface, WindowListener, AnnotationListener, TableModelListener, ListSelectionListener {

	private static final long serialVersionUID = -9056581285643263551L;

	private Vector<String> annotationNames;
	private JButton backButton;
	private String baseFileDir;
	private String baseFileDir2;
	private JTextField  baseFileLocText;
	private JTextField  baseFileLocText2;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private JPanel buttonPanel;
	private JPanel annotationPanel;
	private JButton closeButton;
	private JButton doneButton;
	private int imageIndex = 0;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;	

	private JButton nextButton;

	private JTextField rangeFusionText;
	private JCheckBox useHyperstack;
	private JButton startButton;

	private ModelImage imageA;
	private ModelImage imageB;
	private ModelImage[] imageStackA = null;
	private ModelImage[] imageStackB = null;
	private ModelLUT[] lutStackA;
	private ModelLUT[] lutStackB;
	private int maxRangeIndex = -1;
	private WormData wormData;
	private VolumeTriPlanarInterface triVolume;
	private VOI savedAnnotations = null;

	public PlugInDialogTrackAnnotations()
	{
		init(true);
		setVisible(true);
		addWindowListener(this);
	}


	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();
		if (command.equals("start"))
		{
			setVariables();
			if ( includeRange == null )
			{
				MipavUtil.displayError( "Please specify a range of images." );
				return;
			}
			startButton.setEnabled(false);
			openStraightened(true);
		}
		else if ( command.equals("next") )
		{
			//			voiManager.clear3DSelection();
			save();
			initDisplayAnnotationsPanel();
			imageIndex++;

			openStraightened(true);
		}
		else if ( command.equals("back") )
		{
			//			voiManager.clear3DSelection();
			save();
			imageIndex--;
			openStraightened(true);
		}
		else if ( command.equals("displayAll") )
		{
			VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
			if ( (annotations != null) ) {
				if ( annotations.getCurves().size() > 0 ) {
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						VOIText text = (VOIText) annotations.getCurves().elementAt(i);
						text.display(true);
					}
				}
			}
			displayLabel.setSelected(true);
		}
		else if ( command.equals("displayNone") )
		{
			VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
			if ( (annotations != null) ) {
				if ( annotations.getCurves().size() > 0 ) {
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						VOIText text = (VOIText) annotations.getCurves().elementAt(i);
						text.display(false);
					}
				}
			}
			displayLabel.setSelected(false);
		}
		else if ( source == displayLabel )
		{	
			if ( triVolume != null && triVolume.getVOIManager() != null )
			{
				VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
				
				int row = annotationTable.getSelectedRow();
				System.err.println( "selected row = " + row );
		        
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIText text = (VOIText) annotations.getCurves().elementAt(row);
						text.display(((JCheckBox)source).isSelected());
					}
				}
			}
		}
		// Closes the editing:
		else if (command.equals("done"))
		{			
			//			if ( voiManager != null )
			//			{
			//				voiManager.clear3DSelection();
			//			}
			save();
		}
		if (command.equals("close") || command.equals("done"))
		{			
			if ( triVolume != null )
			{
				triVolume.close();
				triVolume.disposeLocal(true);
				triVolume.dispose();
			}
			setVisible(false);
			if ( ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
					&& ViewUserInterface.getReference().isPlugInFrameVisible() )
			{
				System.exit(0);
			} else {
				dispose();
			}
		}


		if ( includeRange != null )
		{
			imageIndex = Math.min( includeRange.size() - 1, imageIndex );
			imageIndex = Math.max( 0, imageIndex );
			if ( nextButton != null )
			{
				nextButton.setEnabled( imageIndex < (includeRange.size() - 1));
			}
			if ( backButton != null )
			{
				backButton.setEnabled( imageIndex > 0 );
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
		if ( annotationNames != null )
		{
			annotationNames.clear();
		}
		if ( includeRange != null )
		{
			includeRange.clear();
			includeRange = null;
		}
		//		if ( triVolume != null )
		//		{
		//			triVolume.disposeLocal(false);
		//			triVolume = null;
		//		}
		//		if ( volumeImage != null )
		//		{
		//			volumeImage.dispose();
		//			volumeImage = null;
		//		}
		//		if ( voiManager != null )
		//		{
		//			voiManager.disposeLocal(false);
		//			voiManager = null;
		//		}
		
		if ( imageStackA != null )
		{
			for ( int i = 0; i < imageStackA.length; i++ )
			{
				if ( imageStackA[i] != null )
				{
					imageStackA[i].disposeLocal();
					imageStackA[i] = null;
				}
				if ( imageStackB[i] != null )
				{
					imageStackB[i].disposeLocal();
					imageStackB[i] = null;
				}
			}
		}
		if ( imageA != null )
		{
			imageA.disposeLocal();
			imageA = null;
		}
		if ( imageB != null )
		{
			imageB.disposeLocal();
			imageB = null;
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

	protected ModelImage openImage( File imageFile, String fileName )
	{
		FileIO fileIO = new FileIO();
		ModelImage image = fileIO.readImage(fileName, imageFile.getParent() + File.separator, false, null); 
		image.calcMinMax();     

		float[] res = image.getResolutions(0);
		res[0] = res[2]; 
		res[1] = res[2]; 
		int[] units = image.getUnitsOfMeasure();
		units[0] = units[2];
		units[1] = units[2];
		FileInfoBase[] fileInfo = image.getFileInfo();
		for ( int i = 0; i < fileInfo.length; i++ )
		{
			fileInfo[i].setResolutions(res);
			fileInfo[0].setUnitsOfMeasure(units);
		}
		
		return image;
	}

	/**
	 * Opens the current image for viewing. If this is the fist image the volume renderer is created and initialized.
	 * Updates the volume renderer and the histogram / LUT and opacity panels with the new image.
	 * @param imageFile image FIle to open
	 * @param fileName file name
	 * @return true if the file exists.
	 */
	protected boolean openImages( File imageFile, File imageFile2, String fileName, boolean startViewer )
	{
		if ( imageFile.exists() )
		{
			int[] previousExtents = null;
			FileIO fileIO = new FileIO();
			if ( imageA != null ) {
				previousExtents = new int[]{imageA.getExtents()[0], imageA.getExtents()[1], imageA.getExtents()[2]};
				imageA.disposeLocal();
				imageA = null;
			}
			if ( imageB != null )
			{
				imageB.disposeLocal();
				imageB = null;
			}
			imageA = fileIO.readImage(fileName, imageFile.getParent() + File.separator, false, null); 
			imageA.calcMinMax();     

			float[] res = imageA.getResolutions(0);
			res[0] = res[2]; 
			res[1] = res[2]; 
			int[] units = imageA.getUnitsOfMeasure();
			units[0] = units[2];
			units[1] = units[2];
			FileInfoBase[] fileInfo = imageA.getFileInfo();
			for ( int i = 0; i < fileInfo.length; i++ )
			{
				fileInfo[i].setResolutions(res);
				fileInfo[0].setUnitsOfMeasure(units);
			}
			if ( (imageFile2 != null) && imageFile2.exists() )
			{
				imageB = fileIO.readImage(fileName, imageFile2.getParent() + File.separator, false, null); 
				imageB.calcMinMax();     
				res = imageB.getResolutions(0);
				res[0] = res[2]; 
				res[1] = res[2]; 
				units = imageB.getUnitsOfMeasure();
				units[0] = units[2];
				units[1] = units[2];
				fileInfo = imageB.getFileInfo();
				for ( int i = 0; i < fileInfo.length; i++ )
				{
					fileInfo[i].setResolutions(res);
					fileInfo[0].setUnitsOfMeasure(units);
				}
			}
			if ( startViewer ) {
				if ( triVolume == null )
				{
					triVolume = new VolumeTriPlanarInterface(imageA, imageB, false);
					triVolume.addConfiguredListener(this);
					triVolume.addWindowListener(this);
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );
					setVisible(false);
				}
				else {

					boolean updateRenderer = (imageA.getExtents()[0] != previousExtents[0]) || 
							(imageA.getExtents()[1] != previousExtents[1]) ||
							(imageA.getExtents()[2] != previousExtents[2]);

					triVolume.setImage(imageA, imageB, null, null, updateRenderer);
					triVolume.getVolumeGPU().resetAxisX();
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );
				}
			}
			return true;
		}
		return false;
	}



	/**
	 * Opens the current volume for viewing including the straightened annotations and lattice.
	 */
	protected void openStraightened(boolean readAnnotations)
	{
		if ( includeRange != null )
		{
			if ( useHyperstack.isSelected() )
			{
				if ( imageStackA == null )
				{
					imageStackA = new ModelImage[includeRange.size()];
					imageStackB = new ModelImage[includeRange.size()];
					lutStackA = new ModelLUT[includeRange.size()];
					lutStackB = new ModelLUT[includeRange.size()];
					double maxRange = -1;
					
					ViewJProgressBar progressBar = new ViewJProgressBar("Opening HyperStack...",
			                "Opening HyperStack...", 0, includeRange.size(), false, null, null);
			        MipavUtil.centerOnScreen(progressBar);
			        progressBar.setVisible(true);
			        progressBar.updateValueImmed(0);
			        
					for ( int i = 0; i < includeRange.size(); i++ )
					{		
						String imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
						String subDirName = baseFileName + "_" + includeRange.elementAt(i) + File.separator;
						String subDirNameResults = baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator;
						File voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
						File voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);

						if ( voiFile.exists() )
						{
							imageStackA[i] = openImage(voiFile, imageName);
						}
						else {
							imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight_masked.tif";
							voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);				
							if ( voiFile.exists() )
							{
								imageStackA[i] = openImage(voiFile, imageName);
							}
						}

						if ( voiFile2.exists() )
						{
							imageStackB[i] = openImage(voiFile2, imageName);
						}
						else {
							imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight_masked.tif";
							voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
							if ( voiFile2.exists() )
							{
								imageStackB[i] = openImage(voiFile2, imageName);
							}
						}
						
						if ( imageStackA[i] != null ) {
							double range = imageStackA[i].getMax() - imageStackA[i].getMin();
							System.err.println("histo range: " + i + " " + range );
							if ( range > maxRange ) {
								maxRange = range;
								maxRangeIndex = i;
							}
						}
				        progressBar.updateValueImmed(i);
					}

			        progressBar.setVisible(false);
			        progressBar.dispose();
			        progressBar = null;

					imageA = imageStackA[imageIndex];
					imageB = imageStackB[imageIndex];		


					triVolume = new VolumeTriPlanarInterface(imageA, imageB, false);
					triVolume.addConfiguredListener(this);
					triVolume.addWindowListener(this);
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );
					setVisible(false);								
				}
				else {					
					int[] previousExtents = new int[]{imageA.getExtents()[0], imageA.getExtents()[1], imageA.getExtents()[2]};

					imageA = imageStackA[imageIndex];
					imageB = imageStackB[imageIndex];
					
					
					boolean updateRenderer = (imageA.getExtents()[0] != previousExtents[0]) || 
							(imageA.getExtents()[1] != previousExtents[1]) ||
							(imageA.getExtents()[2] != previousExtents[2]);

					triVolume.setImage(imageA, imageB, lutStackA[imageIndex], lutStackB[imageIndex], updateRenderer);
					triVolume.getVolumeGPU().resetAxisX();
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );
					

					lutStackA[imageIndex] = triVolume.getVolumeImageA().GetLUT();
					lutStackB[imageIndex] = triVolume.getVolumeImageB().GetLUT();
				}
			}			
			else if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight.tif";
				String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
				String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
				File voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
				File voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
				if ( !openImages( voiFile, voiFile2, imageName, true ) )
				{	
					imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight_masked.tif";
					voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
					voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
					openImages( voiFile, voiFile2, imageName, true );
				}
			}
		}
		if ( imageA != null )
		{
			String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
			String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
	
			wormData = new WormData(imageA);
			wormData.openStraightLattice();


			if ( readAnnotations ) {
				// try opening any existing annotations from file				
				VOI annotations = LatticeModel.readAnnotationsCSV(baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator +
						"tracked_annotations.csv");
				if ( annotations != null )
				{
					savedAnnotations = annotations;
				}
			}			
			
			if ( triVolume != null && triVolume.getVOIManager() != null )
			{
				((VOILatticeManagerInterface)triVolume.getVOIManager()).deleteAnnotations();
				if ( savedAnnotations != null ) {
					if ( savedAnnotations.getCurves().size() > 0 ) {
						for ( int i = 0; i < savedAnnotations.getCurves().size(); i++ ) {
							short id = (short) imageA.getVOIs().getUniqueID();
							int colorID = 0;
							VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
							newTextVOI.getCurves().add(savedAnnotations.getCurves().elementAt(i));
							//											System.err.println( "add annotation " + ((VOIText)annotations.getCurves().elementAt(j)).getText() );
							((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotation( newTextVOI );
						}
					}
				}
			}
		}		
	}



	/**
	 * User-interface initialization. If the UI is integrated all panels are displayed in one window.
	 * Otherwise the UI is divided into volume display and separate UI panels.
	 * @param integrated
	 */
	private void init(boolean integrated)
	{

		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Annotation Tracking");
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

		rangeFusionText = gui.buildField("Range of images (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		useHyperstack = gui.buildCheckBox( "Load images as hyperstack", true);
		inputsPanel.add(useHyperstack.getParent(), gbc);
		gbc.gridx++;

		buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());

		//		JPanel optionsPanel = makeOptionsPanel(gui);


		JPanel panel1 = new JPanel(new BorderLayout());
		panel1.add(inputsPanel, BorderLayout.NORTH);
		//		panel1.add(optionsPanel, BorderLayout.SOUTH);

		JPanel panel2 = new JPanel(new BorderLayout());
		panel2.add(buttonPanel, BorderLayout.SOUTH);

		dialogGUI.getContentPane().add(panel1, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(panel2, BorderLayout.SOUTH);

		//		tabbedPane = new JTabbedPane();
		//		tabbedPane.setVisible(false);

		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.add( dialogGUI.getContentPane(), BorderLayout.NORTH );
		//		leftPanel.add( tabbedPane, BorderLayout.SOUTH );

		//		volumePanel.setVisible(false);
		JPanel integratedPanel = new JPanel( new BorderLayout() );
		integratedPanel.add( leftPanel, BorderLayout.WEST );
		getContentPane().add(integratedPanel, BorderLayout.CENTER);

		setLocation(0, 0);
		pack();
		setResizable(true);
	}

	private JCheckBox displayLabel;
//	private JList surfaceList;
	private void initDisplayAnnotationsPanel( )
	{
		
		if ( annotationPanel == null )
		{
			JDialogStandalonePlugin dialogGUI = new JDialogStandalonePlugin();
			GuiBuilder gui = new GuiBuilder(dialogGUI);
			
			// Scroll panel that hold the control panel layout in order to use JScrollPane
			annotationPanel = new JPanel();
			annotationPanel.setLayout(new BorderLayout());

			JScrollPane scroller = new JScrollPane(annotationPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

			JPanel mainPanel = new JPanel(new BorderLayout());
			JPanel labelPanel = new JPanel();
			displayLabel = new JCheckBox("display", true);
			displayLabel.addActionListener(this);
			displayLabel.setActionCommand("displayLabel");

			labelPanel.add( new JLabel("Annotation: " ) );
			labelPanel.add(displayLabel);
			
			JButton displayAll = new JButton("Display all" );
			displayAll.addActionListener(this);
			displayAll.setActionCommand("displayAll");
			labelPanel.add( displayAll );
			
			JButton displayNone = new JButton("Display none" );
			displayNone.addActionListener(this);
			displayNone.setActionCommand("displayNone");
			labelPanel.add( displayNone );

			JPanel displayOptions = new JPanel(new BorderLayout());
			displayOptions.add( labelPanel, BorderLayout.NORTH );

			// list panel for surface filenames
			buildAnnotationTable();
			JScrollPane kScrollPane = new JScrollPane(annotationTable);
//			surfaceList = new JList(new DefaultListModel());
//			surfaceList.addListSelectionListener(this);
//			JScrollPane kScrollPane = new JScrollPane(surfaceList);
			JPanel scrollPanel = new JPanel();

			scrollPanel.setLayout(new BorderLayout());
			scrollPanel.add(kScrollPane);
			scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

			buttonPanel = new JPanel();
			backButton = gui.buildButton("back");
			backButton.addActionListener(this);
			backButton.setActionCommand("back");
			backButton.setVisible(true);
			backButton.setEnabled(false);
			buttonPanel.add( backButton );

			nextButton = gui.buildButton("next");
			nextButton.addActionListener(this);
			nextButton.setActionCommand("next");
			nextButton.setVisible(true);
			nextButton.setEnabled( imageIndex < (includeRange.size() - 1));
			buttonPanel.add( nextButton );

			doneButton = gui.buildButton("done");
			doneButton.addActionListener(this);
			doneButton.setActionCommand("done");
			doneButton.setVisible(true);
			doneButton.setEnabled(true);
			buttonPanel.add( doneButton );


			JPanel listPanel = new JPanel();
			listPanel.setLayout(new BorderLayout());
			listPanel.add(scrollPanel, BorderLayout.NORTH);
			listPanel.add(displayOptions, BorderLayout.CENTER);
			listPanel.add(buttonPanel, BorderLayout.SOUTH);
			listPanel.setBorder(JDialogBase.buildTitledBorder("Annotation list"));

			annotationPanel.add(listPanel, BorderLayout.NORTH);
			mainPanel.add(scroller, BorderLayout.CENTER);
			
			triVolume.insertTab( "Track Annotation", annotationPanel );
		}

		annotationTableModel.removeTableModelListener(this);
		int numRows = annotationTableModel.getRowCount();
		for ( int i = numRows -1; i >= 0; i-- ) {
			annotationTableModel.removeRow(i);
		}		
		int index = 0;
		if ( savedAnnotations != null ) {
			if ( savedAnnotations.getCurves().size() > 0 ) {
				for ( int i = 0; i < savedAnnotations.getCurves().size(); i++ ) {
					VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);
					annotationTableModel.addRow( new Object[]{text.getText(), text.elementAt(0).X, text.elementAt(0).Y, text.elementAt(0).Z } );
				}
			}
		}
		annotationTableModel.addTableModelListener(this);
	}

	
	private ListSelectionModel annotationList;
	private DefaultTableModel annotationTableModel;
	private JTable annotationTable;
	private void buildAnnotationTable() {
		if ( annotationTable == null )
		{
			annotationTableModel = new DefaultTableModel();
			annotationTableModel.addColumn("Name");
			annotationTableModel.addColumn("x");
			annotationTableModel.addColumn("y");
			annotationTableModel.addColumn("z");
			annotationTableModel.addTableModelListener(this);

			annotationTable = new JTable(annotationTableModel);
			annotationTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

			annotationTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			annotationTable.getColumn("Name").setMinWidth(100);
			annotationTable.getColumn("Name").setMaxWidth(100);
			annotationList = annotationTable.getSelectionModel();
			annotationList.addListSelectionListener(this);
		}
	}

	/**
	 * Saves seam cells, lattice, or annotations based on the current edit mode.
	 */
	private void save()
	{				
		savedAnnotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();

		if ( savedAnnotations != null ) {
			if ( savedAnnotations.getCurves().size() > 0 ) {
				for ( int i = savedAnnotations.getCurves().size() -1; i >= 0; i-- ) {
					VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);
					if ( !text.getVolumeVOI().GetDisplay() )
					{
						savedAnnotations.getCurves().remove(i);
					}
				}
			}
			saveAnnotations();
		}

	}

	/**
	 * Saves the annotations to the default edited file for the current image.
	 */
	private void saveAnnotations()
	{
		//		System.err.println("Saving annotations " );
		if ( imageA == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
		if ( wormData == null )
		{
			return;
		}
		if ( (triVolume != null) && (triVolume.getVOIManager() != null)  )
		{
			String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
			String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;

//			((VOILatticeManagerInterface)triVolume.getVOIManager()).saveAnnotations(baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator);
			((VOILatticeManagerInterface)triVolume.getVOIManager()).saveAnnotationsAsCSV(baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator,
					"tracked_annotations.csv");

			// first time try opening any existing annotations from file
//			if ( savedAnnotations.getCurves().size() > 0 )
//			{
//				VOIVector annotationList = new VOIVector();
//				LatticeModel.loadAllVOIsFrom(imageA, baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator, true, annotationList, false);
//				if ( annotationList.size() > 0 ) {
//					savedAnnotations = annotationList.elementAt(0);		
//					System.err.println("using annotation file " + baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator);
//				}
//				else
//				{
//					savedAnnotations = new VOI((short) 0, "annotationVOIs", VOI.ANNOTATION, -1.0f);
//				}
//			}
//			else
//			{
//				savedAnnotations = new VOI((short) 0, "annotationVOIs", VOI.ANNOTATION, -1.0f);
//			}
		}

		wormData.dispose();
	}

	/**
	 * Sets the include range list of file IDs when the user presses 'start'.
	 * @return true if there are files in the list to process.
	 */
	private boolean setVariables()
	{	    
		//		try {
		//			threshold = -1;
		//			if ( thresholdImageCheck.isSelected() )
		//			{
		//				threshold = Integer.valueOf(thresholdValue.getText().trim());
		//			}
		//		} catch(NumberFormatException e) {
		//			threshold = 75;
		//		}

		baseFileName = baseFileNameText.getText();
		baseFileDir = baseFileLocText.getText();
		baseFileDir2 = baseFileLocText2.getText();

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

		return (includeRange != null);
	}


	public void algorithmPerformed(AlgorithmBase algorithm) {

		triVolume.actionPerformed( new ActionEvent(this, 0, "HistoLUT") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "Opacity") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "Slices") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "VolumeRayCast") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "VOIToolbar") );
		triVolume.hideMenus();

		triVolume.getVolumeGPU().resetAxisX();
		triVolume.getVolumeGPU().displayVolumeSlices(false);
		triVolume.getVolumeGPU().displayVolumeRaycast(true);
		triVolume.getVolumeGPU().displayVOIs(true);
		triVolume.getVolumeGPU().setVolumeBlend(.8f);
		triVolume.getVolumeGPU().setABBlend(.5f);

		triVolume.getVolumeSlicesPanel().setDividerLocation( 0.75 );

		annotationNames = new Vector<String>();

		if ( (savedAnnotations != null) && (savedAnnotations.getCurves().size() > 0) ) {
			for ( int i = 0; i < savedAnnotations.getCurves().size(); i++ ) {
				VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);
				annotationNames.add( new String(text.getText()) );

				short id = (short) imageA.getVOIs().getUniqueID();
				int colorID = 0;
				VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
				newTextVOI.getCurves().add(text);
				//						System.err.println( "add annotation " + ((VOIText)annotations.getCurves().elementAt(j)).getText() );
				((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotation( newTextVOI );
			}
		}
		initDisplayAnnotationsPanel();

		((VOILatticeManagerInterface)triVolume.getVOIManager()).editAnnotations(false);
		((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotationListener(this);
		
		if ( useHyperstack.isSelected() )
		{
			lutStackA[imageIndex] = triVolume.getVolumeImageA().GetLUT();
			lutStackB[imageIndex] = triVolume.getVolumeImageB().GetLUT();
		}
	}


//	public void valueChanged(ListSelectionEvent e) {
//		if ( e.getSource() == surfaceList )
//		{
//	        int[] selected = surfaceList.getSelectedIndices();
//	        if ( selected != null )
//	        {
//	        	if ( selected.length > 0 )
//	        	{
//	                DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
//	                String name = (String)kList.elementAt( selected[0] );
//
//	                boolean isChecked = false;
//	    			if ( triVolume != null && triVolume.getVOIManager() != null )
//	    			{
//	    				VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
//
//	    				if ( annotations != null ) {
//	    					if ( annotations.getCurves().size() > 0 ) {
//	    						for ( int i = 0; i < annotations.getCurves().size(); i++ )
//	    						{
//	    							VOIText text = (VOIText) annotations.getCurves().elementAt(i);
//	    							if ( text.getText().equals(name) ) {
//	    								isChecked = text.getVolumeVOI().GetDisplay();
//	    								break;
//	    							}
//	    						}
//	    					}
//	    				}
//	    			}
//	    			
//	        		displayLabel.setSelected(isChecked);
//	        	}
//	        }
//		}
//	}


	public void annotationChanged() {

		if ( triVolume != null && triVolume.getVOIManager() != null )
		{
			VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
			annotationTableModel.removeTableModelListener(this);
			int numRows = annotationTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				annotationTableModel.removeRow(i);
			}		
			if ( annotations != null ) {
				if ( annotations.getCurves().size() > 0 ) {
					for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
						VOIText text = (VOIText) annotations.getCurves().elementAt(i);
						annotationTableModel.addRow( new Object[]{text.getText(), text.elementAt(0).X, text.elementAt(0).Y, text.elementAt(0).Z } );
					}
				}
			}
			annotationTableModel.addTableModelListener(this);
		}		
	}


	public void tableChanged(TableModelEvent e) {
//		System.err.println( e.getColumn() + " " + e.getFirstRow() + " " + e.getLastRow() );
		
		int column = e.getColumn();
		boolean isChecked = false;
		if ( triVolume != null && triVolume.getVOIManager() != null )
		{
			int row = e.getFirstRow();
			VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
			if ( (row >= 0) && (row < annotations.getCurves().size()) )
			{
				VOIText text = (VOIText) annotations.getCurves().elementAt(row);
				if ( column == 0 )
				{
					text.setText( annotationTableModel.getValueAt(row, column).toString() );
					text.updateText();
				}
				else
				{
					float value = Float.valueOf(annotationTableModel.getValueAt(row, column).toString());
					if ( value >= 0 ) {
						if ( column == 1 ) {
							text.elementAt(0).X = value;
							text.elementAt(1).X = value;
						}
						else if ( column == 2 ) {
							text.elementAt(0).Y = value;
							text.elementAt(1).Y = value;
						}
						else if ( column == 3 ) {
							text.elementAt(0).Z = value;
							text.elementAt(1).Z = value;
						}
					}
//					System.err.println( text.getText() + " " + text.elementAt(0) + "  " + value);
				}
//				System.err.println( text.getText() + " " + text.elementAt(0));
				text.update();
				isChecked = text.getVolumeVOI().GetDisplay();
			}
		}
		displayLabel.setSelected(isChecked);
	}


	public void valueChanged(ListSelectionEvent e) {
		if ( e.getSource() == annotationList && e.getValueIsAdjusting() )
			return;
		
		VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();

		int row = annotationTable.getSelectedRow();
		boolean isChecked = false;
		if ( (annotations != null) && (row >= 0) ) {
			if ( row < annotations.getCurves().size() ) {
				VOIText text = (VOIText) annotations.getCurves().elementAt(row);
				isChecked = text.getVolumeVOI().GetDisplay();
			}
		}
		displayLabel.setSelected(isChecked);
	}


}
