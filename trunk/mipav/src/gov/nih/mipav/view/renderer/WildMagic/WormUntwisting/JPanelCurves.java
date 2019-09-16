package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;


import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmMarchingCubes;

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
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelLights_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphSamples;
import gov.nih.mipav.view.renderer.flythroughview.ModelImage3DLayout;
import gov.nih.mipav.view.renderer.flythroughview.Skeleton3D;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class JPanelCurves extends JInterfaceBase implements ActionListener, TableModelListener, ListSelectionListener, KeyListener, MouseListener, CurveListener {

	private static final long serialVersionUID = -9056581285643263551L;

	private VolumeImage imageA;
	

	// on 'start' the images are loaded and the VolumeTriPlanarInterface is created:
	private VOILatticeManagerInterface voiManager;
	private VolumeTriPlanarRender volumeRenderer = null;
	// annotation panel displayed in the VolumeTriPlanarInterface:
	private JPanel curvesPanel;
	// turns on/off displaying individual annotations
	private JCheckBox displayLabel;
	// table user-interface for editing the positions of the annotations:
	private ListSelectionModel curvesList;
	private JTable curvesTable;
	private DefaultTableModel curveTableModel;
	// Original names:
	private Vector<String> curveList;
	
	private int displayChannel = 1;

	public JPanelCurves( VOILatticeManagerInterface voiInterface, VolumeTriPlanarRender renderer, VolumeImage imageA ) {
		voiManager = voiInterface;
		volumeRenderer = renderer;
		this.imageA = imageA;
		voiManager.addCurveListener(this);
	}
	
	public void dispose() {}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();
		if ( source == displayLabel )
		{	
			// find the selected annotation and turn it's display on/off:
			if ( voiManager != null )
			{
				// selected row:
				int row = curvesTable.getSelectedRow();		        
				if ( row >= 0 ) {
					String name = curveTableModel.getValueAt(row, 0).toString();
					voiManager.setCurveVisible(name, ((JCheckBox)source).isSelected());
				}
			}
		}
	}

	/* (non-Javadoc)
	 * Called from the LatticeModel when any curves are changed.
	 * Updates the curve table with the current list of curves.
	 */
	public void curveChanged() {
//		System.err.println("annotationChanged");

		if ( voiManager != null )
		{
			curveList = voiManager.getSplineCurves();
			
			curveTableModel.removeTableModelListener(this);
			curvesList.removeListSelectionListener(this);
			int numRows = curveTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				curveTableModel.removeRow(i);
			}		
			
			if ( curveList != null ) {
				if ( curveList.size() > 0 ) {
					for ( int i = 0; i < curveList.size(); i++ ) {						
						curveTableModel.addRow( new Object[]{curveList.elementAt(i) } );
					}
				}

			}
			// restore table listener:
			curveTableModel.addTableModelListener(this);
			curvesList.addListSelectionListener(this);
		}		
		curvesPanel.validate();
	}

	private boolean previewMode = false;
	public void setPreviewMode( boolean preview ) 
	{
		previewMode = preview;
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
	 */
	public void tableChanged(TableModelEvent e) {
		System.err.println("tableChanged");
		// Track updates to the table and update the corresponding curve.
		// The user can change the curve name with table edits.
		// Does not currently check type.
		int column = e.getColumn();
		boolean isChecked = false;
		if ( voiManager != null && (e.getSource() == curveTableModel) )
		{
			int row = e.getFirstRow();
			if ( row >= 0 )
			{
				if ( column == 0 )
				{
					String name = curveTableModel.getValueAt(row, 0).toString();
					isChecked = voiManager.isCurveSelected(name);
					voiManager.setCurveName( curveList.elementAt(row), curveTableModel.getValueAt(row, column).toString() );
					curveList.set(row, name);
				}
			}

			displayLabel.setSelected(isChecked);
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(ListSelectionEvent e) {

		if ( e.getSource() == curvesList && e.getValueIsAdjusting() )
			return;

		updateTableSelection(e);

		if ( e.getSource() == curvesList ) {
			if ( curvesTable.getRowCount() > 0 ) {
				// Updates the displayLabel checkbox based on which row of the table is current:
				int row = curvesTable.getSelectedRow();

				boolean isChecked = true;
				if ( row >= 0 ) {
					String name = curveTableModel.getValueAt(row, 0).toString();
					isChecked = voiManager.isCurveSelected(name);
				}
				displayLabel.setSelected(isChecked);
			}
		}

		imageA.GetImage().notifyImageDisplayListeners();
	}

	/**
	 * Creates the table that displays the annotation information.
	 * The user can edit the annotations directly in the table.
	 */
	private void buildCurveTable() {
		if ( curvesTable == null )
		{
			curveTableModel = new DefaultTableModel() {};
			
			curveTableModel.addColumn("Name");
//			curveTableModel.addColumn("color");
						
			curveTableModel.addTableModelListener(this);

			curvesTable = new JTable(curveTableModel);
			curvesTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			curvesTable.addKeyListener(this);
			curvesTable.addMouseListener(this);

			curvesTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
//			curvesTable.getColumn("Name").setMinWidth(100);
			curvesList = curvesTable.getSelectionModel();
			curvesList.addListSelectionListener(this);
		}
	}

	public JPanel getCurvesPanel() {
		return curvesPanel;
	}


    private static GridBagConstraints gbc = new GridBagConstraints();
    private static GridBagLayout gbLayout = new GridBagLayout();
    
    private static void initGB() {
    	gbc = new GridBagConstraints();
        gbLayout = new GridBagLayout();
    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	gbc.weightx = 1;
    	gbc.weighty = 1;
    	gbc.gridheight = 1;
    	gbc.gridwidth = 1;
    	gbc.anchor = GridBagConstraints.WEST;
    	gbc.fill = GridBagConstraints.HORIZONTAL;
    }
	
	/**
	 * The curve panel is added to the VolumeTriPlanarInterface for display.
	 */
	public JPanel initDisplayCurvesPanel( VOILatticeManagerInterface voiInterface, VolumeImage image, boolean latticeMarkers )
	{		
		voiManager = voiInterface;
		imageA = image;
		voiManager.addCurveListener(this);
		if ( curvesPanel == null )
		{			
			curvesPanel = new JPanel();
			curvesPanel.setLayout(new BorderLayout());
			
			initGB();
			JPanel displayPanel = new JPanel(new BorderLayout());

			initGB();
			JPanel labelPanel = new JPanel(gbLayout);
			// Display checkbox for displaying individual curves:
			displayLabel = new JCheckBox("display", true);
			displayLabel.addActionListener(this);
			displayLabel.setActionCommand("displayLabel");

			gbc.gridx = 0;			gbc.gridy = 0;
			labelPanel.add( new JLabel("Annotation: " ), gbc );
			gbc.gridx++;			gbc.gridy = 0;
			labelPanel.add(displayLabel, gbc);

			// Display all button:
			JButton displayAll = new JButton("Display all" );
			displayAll.addActionListener(this);
			displayAll.setActionCommand("displayAll");
			gbc.gridx++;			gbc.gridy = 0;
			labelPanel.add( displayAll, gbc );

			// Display none button:
			JButton displayNone = new JButton("Display none" );
			displayNone.addActionListener(this);
			displayNone.setActionCommand("displayNone");
			gbc.gridx++;			gbc.gridy = 0;
			labelPanel.add( displayNone, gbc );
			
			labelPanel.setBorder(JDialogBase.buildTitledBorder("Display"));

			// build the curve table for the list of curves:
			buildCurveTable();
			// add curve table to a scroll pane:
			JScrollPane kScrollPane = new JScrollPane(curvesTable);
			Dimension size = kScrollPane.getPreferredSize();
			//			System.err.println( size.width + " " + size.height );
			size.height /= 2;
			kScrollPane.setPreferredSize( size );
			kScrollPane.setBorder(JDialogBase.buildTitledBorder("Annotation list"));


			displayPanel.add(labelPanel, BorderLayout.SOUTH);
			
			curvesPanel.add(kScrollPane, BorderLayout.NORTH);
			curvesPanel.add(displayPanel, BorderLayout.SOUTH);
		}

		// Add the list of annotations to the table:
		curveChanged();

		return curvesPanel;
	}

	private boolean ctrlKey = false;
	public void keyTyped(KeyEvent e) {

		ctrlKey = e.isControlDown();

		System.err.println("keyTyped");
		if ( e.getKeyChar() == KeyEvent.VK_DELETE ) {
			System.err.println("delete");
			if ( (e.getSource() == curvesTable) ) {
				int row = curvesTable.getSelectedRow();
				int col = curvesTable.getSelectedColumn();
				if ( col == 0 && row >= 0 )
				{
					TableCellEditor editor = curvesTable.getCellEditor();
					if ( editor != null )
						editor.stopCellEditing();
					voiManager.deleteSelectedCurve();
					int nRows = curvesTable.getRowCount();
					if ( row < nRows ) {
						curvesTable.setRowSelectionInterval(row, row);
					}
					else if ( nRows > 0 ) {
						curvesTable.setRowSelectionInterval(nRows-1, nRows-1);
					}
				}
			}
		}
	}

	@Override
	public void keyPressed(KeyEvent e) {
		ctrlKey = e.isControlDown();
	}

	@Override
	public void keyReleased(KeyEvent e) {
		ctrlKey = e.isControlDown();
	}

	@Override
	public void mouseClicked(MouseEvent e) {}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mousePressed(MouseEvent e) {
		//		System.err.println("mousePressed " + ctrlKey );
		//		updateTableSelection();
	}

	@Override
	public void mouseReleased(MouseEvent e) {}

	private void updateTableSelection(ListSelectionEvent e) {

		if ( e.getSource() == curvesList ) {
			if ( curvesTable.getRowCount() > 0 ) {
				int[] rows = curvesTable.getSelectedRows();

				for ( int i = 0; i < curvesTable.getRowCount(); i++ ) {
					voiManager.setCurveSelected( curveTableModel.getValueAt(i, 0).toString(), false);
				}

				for ( int i = 0; i < rows.length; i++ ) {
					voiManager.setCurveSelected( curveTableModel.getValueAt(rows[i], 0).toString(), true);
				}
				imageA.GetImage().notifyImageDisplayListeners();
			}
		}
	}
	
}
