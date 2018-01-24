
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
import static org.jocl.CL.*;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.OpenCLInfo;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.jocl.Sizeof;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogTrackAnnotations extends JFrame implements ActionListener, AlgorithmInterface, WindowListener, AnnotationListener, TableModelListener, ListSelectionListener, KeyListener {

	private static final long serialVersionUID = -9056581285643263551L;

	// User-Interface inputs:
	// two image base directories:
	private String baseFileDir;
	private String baseFileDir2;
	private JTextField  baseFileLocText;
	private JTextField  baseFileLocText2;
	// One base name:
	private String baseFileName;
	private JTextField  baseFileNameText;
	
	// UI Buttons:
	private JPanel buttonPanel;
	private JButton backButton;
	private JButton closeButton;
	private JButton doneButton;
	private JButton startButton;
	private JButton nextButton;
	// range of images
	private int imageIndex = 0;
	private Vector<Integer> includeRange;
	private JTextField rangeFusionText;
	private JPanel inputsPanel;	
	// loads all images at once into a hyper stack for faster switching between images:
	private JCheckBox useHyperstack;
	// load resliced and rotated images
	private JCheckBox loadReslice;
	private JCheckBox loadRotated;
	// load any straightened annotations
	private JCheckBox loadStraightenedAnnotations;

	// on 'start' the images are loaded and the VolumeTriPlanarInterface is created:
	private VolumeTriPlanarInterface triVolume;
	// annotation panel displayed in the VolumeTriPlanarInterface:
	private JPanel annotationPanel;
	// turns on/off displaying individual annotations
	private JCheckBox displayLabel;
	// table user-interface for editing the positions of the annotations:
	private ListSelectionModel annotationList;
	private JTable annotationTable;
	private DefaultTableModel annotationTableModel;
	
	// Images
	private ModelImage imageA;
	private ModelImage imageB;
	// hyperstack arrays:
	private ModelImage[] imageStackA = null;
	private int[][] originalExtentsStack = null;
	private int[] originalExtents = null;
	private ModelImage[] imageStackB = null;
	// luts saved between back/next buttons for hyperstacks:
	private ModelLUT[] lutStackA;
	private ModelLUT[] lutStackB;
	private Vector<Boolean> straightLoaded;
	// saved annotations to initialize the next image in the sequence:
	private VOI savedAnnotations = null;
	
	
	/**
	 * Creates the plugin for labeling annotations for a series of straightened images.
	 * Reads positions from a csv file and saves any changes. The user can modify the positions
	 * by direct manipulation with the mouse in the 3D or 2D windows, or by changing positions in
	 * the table listing the annotations.
	 */
	public PlugInDialogTrackAnnotations()
	{
		init();
		setVisible(true);
		addWindowListener(this);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
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
			// save current annotations and open the next image:
			save();
			imageIndex++;
			openStraightened(true);
		}
		else if ( command.equals("back") )
		{
			// save current annotations and open the previous image:
			save();
			imageIndex--;
			openStraightened(true);
		}
		else if ( command.equals("displayAll") )
		{
			// display all annotations in the list:
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
			// display none of the annotations in the list:
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
			// find the selected annotation and turn it's display on/off:
			if ( triVolume != null && triVolume.getVOIManager() != null )
			{
				VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
				// selected row:
				int row = annotationTable.getSelectedRow();		        
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIText text = (VOIText) annotations.getCurves().elementAt(row);
						text.display(((JCheckBox)source).isSelected());
					}
				}
			}
		}
		else if (command.equals("done"))
		{			
			// save
			save();
		}
		if (command.equals("close") || command.equals("done"))
		{			
			// close volume display:
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

		// check range of images and enable or disable the next and back buttons:
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
		
		// close the plugin display:
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

	/* (non-Javadoc)
	 * Called when the volume renderer is initialized and visible.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// Add user-interface panels
		triVolume.actionPerformed( new ActionEvent(this, 0, "HistoLUT") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "Opacity") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "Slices") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "VolumeRayCast") );
		triVolume.actionPerformed( new ActionEvent(this, 0, "VOIToolbar") );
		// hide unused menus
		triVolume.hideMenus();

		// setup the display:
		if ( !loadRotated.isSelected() ) {
			triVolume.getVolumeGPU().resetAxisX();
		}
		triVolume.getVolumeGPU().displayVolumeSlices(false);
		triVolume.getVolumeGPU().displayVolumeRaycast(true);
		triVolume.getVolumeGPU().displayVOIs(true);
		triVolume.getVolumeGPU().setVolumeBlend(.8f);
		triVolume.getVolumeGPU().setABBlend(.5f);
		triVolume.getVolumeSlicesPanel().setDividerLocation( 0.75 );

		// load saved annotations
		if ( (savedAnnotations != null) && (savedAnnotations.getCurves().size() > 0) ) {
			for ( int i = 0; i < savedAnnotations.getCurves().size(); i++ ) {
				VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);

				short id = (short) imageA.getVOIs().getUniqueID();
				int colorID = 0;
				VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
				newTextVOI.getCurves().add(text);
				//						System.err.println( "add annotation " + ((VOIText)annotations.getCurves().elementAt(j)).getText() );
				((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotation( newTextVOI );
			}
		}
		// initialize the display panel for editing / displaying annotations:
		initDisplayAnnotationsPanel();
		((VOILatticeManagerInterface)triVolume.getVOIManager()).editAnnotations(false);
		((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotationListener(this);
		
		// save the luts for hyperstacks:
		if ( useHyperstack.isSelected() )
		{
			lutStackA[imageIndex] = triVolume.getVolumeImageA().GetLUT();
			lutStackB[imageIndex] = triVolume.getVolumeImageB().GetLUT();
		}
	}

	/* (non-Javadoc)
	 * Called from the LatticeModel when any annotations are changed.
	 * Updates the annotation table with the current annotations.
	 */
	public void annotationChanged() {

		if ( triVolume != null && triVolume.getVOIManager() != null )
		{
			// get current annotations and update table:
			VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
			// remove table listener durning updates:
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
			// restore table listener:
			annotationTableModel.addTableModelListener(this);
		}		
	}

	/* (non-Javadoc)
	 * @see java.awt.Window#dispose()
	 */
	public void dispose()
	{
		super.dispose();
		if ( includeRange != null )
		{
			includeRange.clear();
			includeRange = null;
		}
		
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

	/* (non-Javadoc)
	 * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
	 */
	public void tableChanged(TableModelEvent e) {
		System.err.println("tableChanged");
		// Track updates to the table and update the corresponding annotation.
		// The user can change the annotation name and position (x,y,z) with table edits.
		// Does not currently check type.
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
				}
				text.update();
				isChecked = text.getVolumeVOI().GetDisplay();
			}
		}
		displayLabel.setSelected(isChecked);
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(ListSelectionEvent e) {
		if ( e.getSource() == annotationList && e.getValueIsAdjusting() )
			return;
		
		// Updates the displayLabel checkbox based on which row of the table is current:
		VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();

		int row = annotationTable.getSelectedRow();
		boolean isChecked = true;
		if ( (annotations != null) && (row >= 0) ) {
			if ( row < annotations.getCurves().size() ) {
				VOIText text = (VOIText) annotations.getCurves().elementAt(row);
				if ( text.getVolumeVOI() != null )
				{
					isChecked = text.getVolumeVOI().GetDisplay();
				}
			}
		}
		displayLabel.setSelected(isChecked);
	}

	@Override
	public void windowActivated(WindowEvent e) {}
	@Override
	public void windowClosed(WindowEvent e) {}

	/* (non-Javadoc)
	 * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
	 */
	public void windowClosing(final WindowEvent event)
	{
		// Catch the windowClosing event when the VolumeTriPlanarInterface is closed
		// so all images in the hyperstack can be disposed and memory freed.
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
	 * Creates the table that displays the annotation information.
	 * The user can edit the annotations directly in the table.
	 */
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
			annotationTable.addKeyListener(this);

			annotationTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			annotationTable.getColumn("Name").setMinWidth(100);
			annotationTable.getColumn("Name").setMaxWidth(100);
			annotationList = annotationTable.getSelectionModel();
			annotationList.addListSelectionListener(this);
		}
	}
	
	/**
	 * User-interface initialization.
	 */
	private void init()	{

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

		// Data directories:
		inputsPanel = new JPanel(new GridBagLayout());
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		inputsPanel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Data directory (marker 1): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileLocText2 = gui.buildFileField("Data directory (marker 2): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText2.getParent(), gbc);
		gbc.gridy++;

		// base file name:
		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		// range of images to load:
		rangeFusionText = gui.buildField("Range of images (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		// loads all images at once if selected:
		useHyperstack = gui.buildCheckBox( "Load images as hyperstack", true);
		inputsPanel.add(useHyperstack.getParent(), gbc);
		gbc.gridy++;

		// resliced and rotated images:
		loadReslice = gui.buildCheckBox( "Load resliced images", true);
		inputsPanel.add(loadReslice.getParent(), gbc);
		gbc.gridx++;

		// resliced and rotated images:
		loadRotated = gui.buildCheckBox( "Load rotated images", true);
		inputsPanel.add(loadRotated.getParent(), gbc);		
		gbc.gridx = 0;
		gbc.gridy++;

		// resliced and rotated images:
//		loadStraightenedAnnotations = gui.buildCheckBox( "Load straightened annotations", true);
//		inputsPanel.add(loadStraightenedAnnotations.getParent(), gbc);
		

		// button panels:
		buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());


		JPanel panel1 = new JPanel(new BorderLayout());
		panel1.add(inputsPanel, BorderLayout.NORTH);

		JPanel panel2 = new JPanel(new BorderLayout());
		panel2.add(buttonPanel, BorderLayout.SOUTH);

		dialogGUI.getContentPane().add(panel1, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(panel2, BorderLayout.SOUTH);

		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.add( dialogGUI.getContentPane(), BorderLayout.NORTH );

		JPanel integratedPanel = new JPanel( new BorderLayout() );
		integratedPanel.add( leftPanel, BorderLayout.WEST );
		getContentPane().add(integratedPanel, BorderLayout.CENTER);

		setLocation(0, 0);
		pack();
		setResizable(true);
	}

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	private void initDisplayAnnotationsPanel( )
	{		
		if ( annotationPanel == null )
		{
			JDialogStandalonePlugin dialogGUI = new JDialogStandalonePlugin();
			GuiBuilder gui = new GuiBuilder(dialogGUI);
			
			annotationPanel = new JPanel();
			annotationPanel.setLayout(new BorderLayout());

			// Scroll panel that hold the control panel layout in order to use JScrollPane
			JScrollPane scroller = new JScrollPane(annotationPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

			JPanel mainPanel = new JPanel(new BorderLayout());
			JPanel labelPanel = new JPanel();
			// Display checkbox for displaying individual annotations:
			displayLabel = new JCheckBox("display", true);
			displayLabel.addActionListener(this);
			displayLabel.setActionCommand("displayLabel");

			labelPanel.add( new JLabel("Annotation: " ) );
			labelPanel.add(displayLabel);
			
			// Display all button:
			JButton displayAll = new JButton("Display all" );
			displayAll.addActionListener(this);
			displayAll.setActionCommand("displayAll");
			labelPanel.add( displayAll );
			
			// Display none button:
			JButton displayNone = new JButton("Display none" );
			displayNone.addActionListener(this);
			displayNone.setActionCommand("displayNone");
			labelPanel.add( displayNone );

			JPanel displayOptions = new JPanel(new BorderLayout());
			displayOptions.add( labelPanel, BorderLayout.NORTH );

			// build the annotation table for the list of annotations:
			buildAnnotationTable();
			// add annotation table to a scroll pane:
			JScrollPane kScrollPane = new JScrollPane(annotationTable);
			JPanel scrollPanel = new JPanel();
			scrollPanel.setLayout(new BorderLayout());
			scrollPanel.add(kScrollPane);
			scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

			// back button:
			buttonPanel = new JPanel();
			backButton = gui.buildButton("back");
			backButton.addActionListener(this);
			backButton.setActionCommand("back");
			backButton.setVisible(true);
			backButton.setEnabled(false);
			buttonPanel.add( backButton );

			// next button:
			nextButton = gui.buildButton("next");
			nextButton.addActionListener(this);
			nextButton.setActionCommand("next");
			nextButton.setVisible(true);
			nextButton.setEnabled( imageIndex < (includeRange.size() - 1));
			buttonPanel.add( nextButton );

			// done button:
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

		// Add the list of annotations to the table:
		annotationTableModel.removeTableModelListener(this);
		int numRows = annotationTableModel.getRowCount();
		for ( int i = numRows -1; i >= 0; i-- ) {
			annotationTableModel.removeRow(i);
		}		
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

	/**
	 * Opens an image (.tif) and resets the units of measure to the correct values:
	 * @param imageFile
	 * @param fileName
	 * @return
	 */
	private ModelImage openImage( File imageFile, String fileName )
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
	private boolean openImages( File imageFile, File imageFile2, String fileName, boolean startViewer )
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
					if ( !loadRotated.isSelected() ) {
						triVolume.getVolumeGPU().resetAxisX();
					}
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
	private void openStraightened(boolean readAnnotations)
	{
		if ( includeRange != null )
		{
			if ( useHyperstack.isSelected() )
			{
				// If images are opened as a hyperstack, open all images and save in an array:
				if ( imageStackA == null )
				{
					// count images to make sure all exist:
					for ( int i = includeRange.size() - 1; i >= 0; i-- )
					{	
						String imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
						if ( loadReslice.isSelected() ) {
							imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight_reslice.tif";
						}
						if ( loadRotated.isSelected() ) {
							imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight_reslice_rotate.tif";
						}
						String subDirName = baseFileName + "_" + includeRange.elementAt(i) + File.separator;
						String subDirNameResults = baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator;
						File voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);

						if ( !voiFile.exists() )
						{
							includeRange.remove(i);
						}
					}
					if ( includeRange.size() == 0 )
					{
						MipavUtil.displayError("No images available, check file path");
						return;
					}
					
					
					imageStackA = new ModelImage[includeRange.size()];
					originalExtentsStack = new int[includeRange.size()][];
					imageStackB = new ModelImage[includeRange.size()];
					lutStackA = new ModelLUT[includeRange.size()];
					lutStackB = new ModelLUT[includeRange.size()];
					
					ViewJProgressBar progressBar = new ViewJProgressBar("Opening HyperStack...",
			                "Opening HyperStack...", 0, includeRange.size(), false, null, null);
			        MipavUtil.centerOnScreen(progressBar);
			        progressBar.setVisible(true);
			        progressBar.updateValueImmed(0);
			        
					for ( int i = 0; i < includeRange.size(); i++ )
					{		
						String imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
						if ( loadReslice.isSelected() ) {
							imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight_reslice.tif";
						}
						if ( loadRotated.isSelected() ) {
							imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight_reslice_rotate.tif";
						}
						String subDirName = baseFileName + "_" + includeRange.elementAt(i) + File.separator;
						String subDirNameResults = baseFileName + "_" + includeRange.elementAt(i) + "_results" + File.separator;
						File voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
						File voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);

						long memoryInUse = 0;
						if ( i == 0 )
						{
							// check memory usage:
							System.gc();
							memoryInUse = MipavUtil.getUsedHeapMemory();
						}
						
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

						if ( loadReslice.isSelected() || loadRotated.isSelected() ) {
							// open original image and store original size:
							imageName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
							voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
							if ( voiFile.exists() )
							{
								ModelImage originalImage = openImage(voiFile, imageName);
								if ( originalImage != null ) {
									originalExtentsStack[i] = originalImage.getExtents().clone();
									originalImage.disposeLocal(false);
								}
							}
						}
						// Add memory check here:
						if ( i == 0 )
						{
							int sizeA = 0;
							int sizeB = 0;
							if ( imageStackA[i] != null ) {
								sizeA = imageStackA[i].getDataSize();
							}
							if ( imageStackB[i] != null ) {
								sizeB = imageStackB[i].getDataSize();
							}
							if ( !checkGPUMemory(sizeA, sizeB) )
							{
								MipavUtil.displayError("Image size too big to load on GPU.");
						        progressBar.setVisible(false);
						        progressBar.dispose();
						        progressBar = null;
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
								return;
							}

							System.gc();
							long memoryInUse2 = MipavUtil.getUsedHeapMemory();
							long imagesMemory = memoryInUse2 - memoryInUse;

					        final long totalMemory = MipavUtil.getMaxHeapMemory();
					        final long memoryFree = totalMemory-memoryInUse2;
					        System.err.println( "image memory use: " + (imagesMemory / 1048576) + "M" );
							if ( (imagesMemory * includeRange.size()) > memoryFree )
							{
								MipavUtil.displayError("Too many images, please load shorter sequence");
						        progressBar.setVisible(false);
						        progressBar.dispose();
						        progressBar = null;
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
								return;
							}
						}
				        progressBar.updateValueImmed(i);
					}

			        progressBar.setVisible(false);
			        progressBar.dispose();
			        progressBar = null;

					imageA = imageStackA[imageIndex];
					imageB = imageStackB[imageIndex];		
					originalExtents = originalExtentsStack[imageIndex];

					// create volume renderer and initialize:
					triVolume = new VolumeTriPlanarInterface(imageA, imageB, false);
					triVolume.addConfiguredListener(this);
					triVolume.addWindowListener(this);
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );
					setVisible(false);								
				}
				else {			
					// hyperstack already loaded, move to the next image:
					// check for changes in image dimensions:
					int[] previousExtents = new int[]{imageA.getExtents()[0], imageA.getExtents()[1], imageA.getExtents()[2]};

					imageA = imageStackA[imageIndex];
					imageB = imageStackB[imageIndex];
					originalExtents = originalExtentsStack[imageIndex];
					
					// recreate image on the GPU if the image dimensions change, if no change just reload with
					// the new image data:
					boolean updateRenderer = (imageA.getExtents()[0] != previousExtents[0]) || 
							(imageA.getExtents()[1] != previousExtents[1]) ||
							(imageA.getExtents()[2] != previousExtents[2]);

					// set the new image and LUTs:
					triVolume.setImage(imageA, imageB, lutStackA[imageIndex], lutStackB[imageIndex], updateRenderer);
					if ( !loadRotated.isSelected() ) {
						triVolume.getVolumeGPU().resetAxisX();
					}
					// set the title to match the new image name:
					triVolume.setTitle("Annotation Tracking " + imageA.getImageName() );					

					// save LUTs:
					lutStackA[imageIndex] = triVolume.getVolumeImageA().GetLUT();
					lutStackB[imageIndex] = triVolume.getVolumeImageB().GetLUT();
				}
			}			
			else if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				// load the new image directly from disk, no hyperstacks:
				String imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight.tif";
				if ( loadReslice.isSelected() ) {
					imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight_reslice.tif";
				}
				if ( loadRotated.isSelected() ) {
					imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight_reslice_rotate.tif";
				}
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
			// new image loaded:
			String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
			String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
	
			// remove all vois:
			imageA.unregisterAllVOIs();
			
			// load the straightened lattice:
			VOIVector lattice = LatticeModel.readLatticeCSV(baseFileDir + File.separator + subDirName + subDirNameResults + "straightened_lattice" + File.separator +
					"straightened_lattice.csv");
			for ( int j = 0; j < lattice.size(); j++ )
			{
				imageA.registerVOI(convertToLocal(lattice.elementAt(j)));
			}

			// load any existing annotations from the csv file:
			if ( readAnnotations ) {
				if ( savedAnnotations == null )
				{
					savedAnnotations = new VOI((short) 0, "annotation3d_", VOI.ANNOTATION, -1.0f);
				}
				// try opening any existing annotations from file				
				VOI annotations = LatticeModel.readAnnotationsCSV(baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator +
						"tracked_annotations.csv");
				if ( annotations != null )
				{
//					savedAnnotations = annotations;
					savedAnnotations = convertToLocal(annotations);
				}
//				annotations = LatticeModel.readAnnotationsCSV(baseFileDir2 + File.separator + subDirName + subDirNameResults + "tracked_annotations" + File.separator +
//						"tracked_annotations.csv");		
//				if ( annotations != null )
//				{
//					savedAnnotations.getCurves().addAll(annotations.getCurves());
//				}
//				if ( loadStraightenedAnnotations.isSelected() ) {
//					boolean loadStraight = true;
//					if ( straightLoaded.size() > imageIndex ) {
//						loadStraight = !straightLoaded.get(imageIndex);
//					}
//					if ( loadStraight ) {
//						if ( straightLoaded.size() == imageIndex ) {
//							straightLoaded.add(true);
//						}
//						annotations = LatticeModel.readAnnotationsCSV(baseFileDir + File.separator + subDirName + subDirNameResults + "straightened_annotations" + File.separator +
//								"straightened_annotations.csv");	
//						if ( annotations != null )
//						{
//							savedAnnotations.getCurves().addAll(annotations.getCurves());
//						}	
//						annotations = LatticeModel.readAnnotationsCSV(baseFileDir2 + File.separator + subDirName + subDirNameResults + "straightened_annotations" + File.separator +
//								"straightened_annotations.csv");	
//						if ( annotations != null )
//						{
//							savedAnnotations.getCurves().addAll(annotations.getCurves());
//						}
//					}
//				}
			}			
			
			// load the annotations into the viewer:
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
	 * Saves annotations. Stores the annotations locally to load into the next image.
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
						// If the annotation display is turned off do not save it or load into next image:
						savedAnnotations.getCurves().remove(i);
					}
				}
			}
			saveAnnotations();
		}

	}


	/**
	 * Saves the annotations to the default file for the current image in CSV format.
	 */
	private void saveAnnotations()
	{
		if ( imageA == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
//		VOI annotations1 = new VOI((short) 0, "tracked_annotations", VOI.ANNOTATION, -1.0f);
//		VOI annotations2 = new VOI((short) 0, "tracked_annotations", VOI.ANNOTATION, -1.0f);
//		String dir1 = new String();
//		String dir2 = new String();
//		String empty = new String();
//		for ( int i = 0; i < savedAnnotations.getCurves().size(); i++ ) {
//			if ( i == 0 )
//			{
//				VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);
//				if ( !text.getNote().equals(empty) )
//				{
//					dir1 = text.getNote();
//				}
//				annotations1.getCurves().add(savedAnnotations.getCurves().elementAt(i));
//			}
//			else
//			{
//				VOIText text = (VOIText) savedAnnotations.getCurves().elementAt(i);
//				if ( text.getNote().equals( dir1 ) || text.getNote().equals(empty) )
//				{
//					annotations1.getCurves().add(text);
//				}
//				else
//				{
//					if ( !text.getNote().equals(empty) )
//					{
//						dir2 = text.getNote();
//					}
//					annotations2.getCurves().add(text);
//				}
//			}
//		}
////		System.err.println( dir1 );
////		System.err.println( dir2 );
//
//		if ( !dir1.equals(empty) )
//		{
//			dir1 = dir1.substring(0, dir1.lastIndexOf(File.separator, dir1.length()) );
//		}
//		if ( !dir2.equals(empty) )
//		{
//			dir2 = dir2.substring(0, dir2.lastIndexOf(File.separator, dir2.length()) );
//		}
//		System.err.println( dir1 );
//		System.err.println( dir2 );
		
		// Save first set of annotations:
//		if ( annotations1.getCurves().size() > 0 )
//		{
//			VOI annotations1Original = convertToOriginal(annotations1);
//			// create default directory name if needed:
//			String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
//			String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
//			if ( dir1.equals("") )
//			{
//				dir1 = baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations";
//			}
//			LatticeModel.saveAnnotationsAsCSV(dir1 + File.separator, "tracked_annotations.csv", annotations1Original);
//			if ( loadReslice.isSelected() )
//			{
//				String name = "tracked_annotations_reslice.csv";
//				if ( loadRotated.isSelected() )
//				{
//					name = "tracked_annotations_reslice_rotate.csv";
//				}
//				LatticeModel.saveAnnotationsAsCSV(dir1 + File.separator, name, annotations1Original);
//			}
//		}
		
		// Save second set of annotations:
//		if ( annotations2.getCurves().size() > 0 )
//		{
//			VOI annotations2Original = convertToOriginal(annotations2);
//			// create default directory name if needed:
//			String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
//			String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
//			if ( dir2.equals("") )
//			{
//				dir2 = baseFileDir2 + File.separator + subDirName + subDirNameResults + "tracked_annotations";
//			}
//			LatticeModel.saveAnnotationsAsCSV(dir2 + File.separator, "tracked_annotations.csv", annotations2Original);
//			if ( loadReslice.isSelected() )
//			{
//				String name = "tracked_annotations_reslice.csv";
//				if ( loadRotated.isSelected() )
//				{
//					name = "tracked_annotations_reslice_rotate.csv";
//				}
//				LatticeModel.saveAnnotationsAsCSV(dir2 + File.separator, name, annotations2Original);
//			}
//		}


		VOI annotationsOriginal = convertToOriginal(savedAnnotations);
		// create default directory name if needed:
		String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
		String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
		String dir = baseFileDir + File.separator + subDirName + subDirNameResults + "tracked_annotations";
		LatticeModel.saveAnnotationsAsCSV(dir + File.separator, "tracked_annotations.csv", annotationsOriginal);
		if ( loadReslice.isSelected() )
		{
			String name = "tracked_annotations_reslice.csv";
			if ( loadRotated.isSelected() )
			{
				name = "tracked_annotations_reslice_rotate.csv";
			}
			LatticeModel.saveAnnotationsAsCSV(dir + File.separator, name, savedAnnotations);
		}
	}


	/**
	 * Sets the include range list of file IDs when the user presses 'start'.
	 * @return true if there are files in the list to process.
	 */
	private boolean setVariables()
	{	   
		straightLoaded = new Vector<Boolean>();
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
	
	private VOI convertToLocal( VOI voi ) {
		if ( !loadReslice.isSelected() && !loadRotated.isSelected() ) {
			return voi;
		}
		int shiftX = originalExtents[0]/2;
		int shiftY = originalExtents[1]/2;
		int xOffset = (imageA.getExtents()[0] - originalExtents[0])/2;
		int yOffset = (imageA.getExtents()[1] - originalExtents[1])/2;
		if ( loadRotated.isSelected() )
		{
			yOffset = (imageA.getExtents()[2] - originalExtents[1])/2;
		}
		
		VOI localVOI = new VOI(voi);
		for ( int i = 0; i < localVOI.getCurves().size(); i++ ) {
			VOIBase curveOrig = localVOI.getCurves().elementAt(i);
			if ( loadReslice.isSelected() ) {
				// add offset:
				for ( int j = 0; j < curveOrig.size(); j++ ) {
					curveOrig.elementAt(j).X -= shiftX;
					curveOrig.elementAt(j).Y -= shiftY;
					
					curveOrig.elementAt(j).X += xOffset;
					curveOrig.elementAt(j).Y += yOffset;
					
					curveOrig.elementAt(j).X += shiftX;
					curveOrig.elementAt(j).Y += shiftY;
				}
			}
			if ( loadRotated.isSelected() ) {
				// swap y and z coordinates:
				for ( int j = 0; j < curveOrig.size(); j++ ) {
					float temp = curveOrig.elementAt(j).Y;
					curveOrig.elementAt(j).Y = curveOrig.elementAt(j).Z;
					curveOrig.elementAt(j).Z = temp;
				}
			}
		}
		return localVOI;
	}
	
	private VOI convertToOriginal( VOI voi ) {
		if ( !loadReslice.isSelected() && !loadRotated.isSelected() ) {
			return voi;
		}	

		int shiftX = imageA.getExtents()[0]/2;
		int shiftY = imageA.getExtents()[1]/2;
		int xOffset = (imageA.getExtents()[0] - originalExtents[0])/2;
		int yOffset = (imageA.getExtents()[1] - originalExtents[1])/2;
		if ( loadRotated.isSelected() )
		{
			shiftY = imageA.getExtents()[2]/2;
			yOffset = (imageA.getExtents()[2] - originalExtents[1])/2;
		}
		
		VOI localVOI = new VOI(voi);
		for ( int i = 0; i < localVOI.getCurves().size(); i++ ) {
			VOIBase curveOrig = localVOI.getCurves().elementAt(i);
			if ( loadRotated.isSelected() ) {
				// swap y and z coordinates:
				for ( int j = 0; j < curveOrig.size(); j++ ) {
					float temp = curveOrig.elementAt(j).Y;
					curveOrig.elementAt(j).Y = curveOrig.elementAt(j).Z;
					curveOrig.elementAt(j).Z = temp;
				}
			}
			if ( loadReslice.isSelected() ) {
				// put back in original coordinates:
				for ( int j = 0; j < curveOrig.size(); j++ ) {
					curveOrig.elementAt(j).X -= shiftX;
					curveOrig.elementAt(j).Y -= shiftY;
					
					curveOrig.elementAt(j).X -= xOffset;
					curveOrig.elementAt(j).Y -= yOffset;
					
					curveOrig.elementAt(j).X += shiftX;
					curveOrig.elementAt(j).Y += shiftY;
				}
			}
		}
		return localVOI;
	}
	
	/**
	 * Checks image size and the available memory on the GPU.
	 * @param sizeA
	 * @param sizeB
	 * @return
	 */
	private boolean checkGPUMemory(int sizeA, int sizeB) {	
		long[] maxMemSizeArray = new long[2];
		
		OpenCLInfo.getMaxMemSize(CL_DEVICE_TYPE_GPU, maxMemSizeArray);
		long memoryUsed = sizeA * 4 + sizeB * 4;	
		long maxAllocSize = maxMemSizeArray[0];
		long totalMemSize = maxMemSizeArray[1];
		if ( (sizeA > (maxAllocSize / (Sizeof.cl_float))) || (sizeB > (maxAllocSize / (Sizeof.cl_float))) || (memoryUsed >= (totalMemSize / Sizeof.cl_float)) )
		{
			return false;
		}
		return true;
	}

	public void keyTyped(KeyEvent e) {
		if ( e.getKeyChar() == KeyEvent.VK_TAB ) {
			int row = annotationTable.getSelectedRow();
			int col = annotationTable.getSelectedColumn();
			if ( triVolume != null && triVolume.getVOIManager() != null )
			{
				if ( (row == 0)  && (col == 0) ) {
					
					VOIText text = new VOIText();
					text.setText("center" );
					int dimX = imageA.getExtents()[0];
					int dimY = imageA.getExtents()[1];
					int dimZ = imageA.getExtents()[2];
					text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );
					text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );
					
					short id = (short) imageA.getVOIs().getUniqueID();
					int colorID = 0;
					VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
					newTextVOI.getCurves().add(text);
					
					((VOILatticeManagerInterface)triVolume.getVOIManager()).clear3DSelection();
					((VOILatticeManagerInterface)triVolume.getVOIManager()).addAnnotation( newTextVOI );
					((VOILatticeManagerInterface)triVolume.getVOIManager()).clear3DSelection();
					int nRows = annotationTable.getRowCount();
					annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
				}
			}
		}
		if ( e.getKeyChar() == KeyEvent.VK_DELETE ) {
			int row = annotationTable.getSelectedRow();
			int col = annotationTable.getSelectedColumn();
			if ( col == 0 && row >= 0 )
			{
				TableCellEditor editor = annotationTable.getCellEditor();
				if ( editor != null )
					editor.stopCellEditing();
				VOI annotations = ((VOILatticeManagerInterface)triVolume.getVOIManager()).getAnnotations();
				annotations.getCurves().remove(row);
				annotationChanged();
				int nRows = annotationTable.getRowCount();
				if ( row < nRows ) {
					annotationTable.setRowSelectionInterval(row, row);
				}
				else {
					annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
				}
			}
		}
	}

	@Override
	public void keyPressed(KeyEvent e) {
	}

	@Override
	public void keyReleased(KeyEvent e) {}
}
