package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;


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
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;

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
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;


import WildMagic.LibFoundation.Mathematics.Vector3f;


public class JPanelLattice extends JInterfaceBase implements ActionListener, LatticeListener, TableModelListener, KeyListener, MouseListener {

	private static final long serialVersionUID = -9056581285643263551L;

	private ModelImage imageA;
	private WormData wormData;

	// on 'start' the images are loaded and the VolumeTriPlanarInterface is created:
	private VOILatticeManagerInterface voiManager;
	// annotation panel displayed in the VolumeTriPlanarInterface:
	private JSplitPane annotationPanel;
	// turns on/off displaying individual annotations
	private JCheckBox displaySeam;
	private JCheckBox displayLattice;
//	private JCheckBox displayGroupLabel;
	// table user-interface for editing the positions of the annotations:
	private ListSelectionModel annotationList;
	private JTable annotationTable;
	private DefaultTableModel annotationTableModel;

	// table user-interface for editing the positions of the annotations:
//	private ListSelectionModel annotationGroupList;
//	private JTable annotationGroupTable;
//	private DefaultTableModel annotationGroupTableModel;
//	private String selectedPrefix = null;
	

	public JPanelLattice( VOILatticeManagerInterface voiInterface, ModelImage image ) {
		voiManager = voiInterface;
		imageA = image;
		wormData = new WormData(imageA);
		voiManager.addLatticeListener(this);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		if ( command.equals("displayLabel") )
		{
			System.err.println("displayLabels");
		}
		else if ( command.equals("displayLattice") )
		{
			if ( voiManager != null )
			{
				voiManager.showLattice( displayLattice.isSelected() );
			}
		}
	}

	/* (non-Javadoc)
	 * Called from the LatticeModel when any annotations are changed.
	 * Updates the annotation table with the current annotations.
	 */
	public void latticeChanged() {
//		System.err.println("latticeChanged");

		if ( voiManager != null )
		{
			// get current annotations and update table:
			VOI lattice = voiManager.getLattice();
			// remove table listener during updates:
			annotationTableModel.removeTableModelListener(this);
			int numRows = annotationTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				annotationTableModel.removeRow(i);
			}		
			// remove table listener during updates:
//			annotationGroupTableModel.removeTableModelListener(this);
//			annotationGroupList.removeListSelectionListener(this);
//			numRows = annotationGroupTableModel.getRowCount();
//			for ( int i = numRows -1; i >= 0; i-- ) {
//				annotationGroupTableModel.removeRow(i);
//			}		
			if ( lattice != null ) {
				if ( lattice.getCurves().size() == 2 ) {
					VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
					VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
					if ( left.size() == right.size() ) {
						VOI seamLabels = wormData.segmentSeamFromLattice(voiManager.getLattice(), voiManager.getImage(), false);
						int seamCount = 0;
						for ( int i = 0; i < left.size(); i++ ) {
							VOIWormAnnotation leftMarker = (VOIWormAnnotation) seamLabels.getCurves().elementAt(seamCount++);
							VOIWormAnnotation rightMarker = (VOIWormAnnotation) seamLabels.getCurves().elementAt(seamCount++);

							annotationTableModel.addRow( new Object[]{leftMarker.getText(), leftMarker.elementAt(0).X, leftMarker.elementAt(0).Y, leftMarker.elementAt(0).Z,
									rightMarker.getText(), rightMarker.elementAt(0).X, rightMarker.elementAt(0).Y, rightMarker.elementAt(0).Z} );
						}					

						annotationList.clearSelection();
						Vector3f picked = voiManager.getLatticePickedPoint();
						if ( picked != null ) {
							for ( int i = 0; i < left.size(); i++ ) {
								if ( left.elementAt(i).equals(picked) || right.elementAt(i).equals(picked) ) {
									annotationList.addSelectionInterval(i, i);
								}
							}
						}
					}
				}

			}
			// restore table listener:
			annotationTableModel.addTableModelListener(this);
//			annotationGroupTableModel.addTableModelListener(this);
//			annotationGroupList.addListSelectionListener(this);
		}		
		annotationPanel.validate();
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
		// Track updates to the table and update the corresponding annotation.
		// The user can change the annotation name and position (x,y,z) with table edits.
		// Does not currently check type.
//		int column = e.getColumn();
//		boolean isChecked = false;
//		if ( voiManager != null && (e.getSource() == annotationTableModel) )
//		{
//			int row = e.getFirstRow();
//			VOI annotations = voiManager.getAnnotations();
//			if ( (row >= 0) && (row < annotations.getCurves().size()) )
//			{
//				VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(row);
//				if ( column == 0 )
//				{
//					text.setText( annotationTableModel.getValueAt(row, column).toString() );
//					text.updateText();
//				}
//				else if ( column < 4 )
//				{
//					float value = Float.valueOf(annotationTableModel.getValueAt(row, column).toString());
//					if ( value >= 0 ) {
//						if ( column == 1 ) {
//							text.elementAt(0).X = value;
//							text.elementAt(1).X = value;
//						}
//						else if ( column == 2 ) {
//							text.elementAt(0).Y = value;
//							text.elementAt(1).Y = value;
//						}
//						else if ( column == 3 ) {
//							text.elementAt(0).Z = value;
//							text.elementAt(1).Z = value;
//						}
//						text.retwist(previewMode);
//					}
//				}
//				else
//				{
//					try {
//						int value = Integer.valueOf(annotationTableModel.getValueAt(row, column).toString());
//						text.setLatticeSegment(value);
//					} catch ( java.lang.NumberFormatException error ) {
//						text.setLatticeSegment(-1);
//					}
//				}
//				text.update();
//				if ( text.getVolumeVOI() != null ) {
//					isChecked = text.getVolumeVOI().GetDisplay();
//				}
//			}
//
//			displayLabel.setSelected(isChecked);
//		}
////		if ( voiManager != null && (e.getSource() == annotationGroupTableModel) )
////		{
////			int row = annotationGroupTable.getSelectedRow();
////			int segment = -1;
////			String newPrefix = "";
////			boolean nameChanged = false;
////			boolean segmentChanged = false;
////			if ( column == 0 ) {
////				// new name:
////				if ( annotationGroupTableModel.getValueAt(row, 0) != null ) {
////					newPrefix = new String ( annotationGroupTableModel.getValueAt(row, 0).toString() );
////					if ( selectedPrefix.equals("new row") ) 
////					{
////						selectedPrefix = new String(newPrefix);
////						voiManager.setAnnotationPrefix( selectedPrefix );
////					}
////					else 
////					{
////						nameChanged = true;
////					}
////				}
////			}
////			else if ( column == 1 ) {
////				// change the segment:
////				try {
////					segment = Integer.valueOf(annotationGroupTableModel.getValueAt(row, column).toString());
////				} catch ( java.lang.NumberFormatException error ) {
////					// value erased:
////				}
////				segmentChanged = true;
////			}
////			if ( nameChanged || segmentChanged ) 
////			{
////				VOI annotations = voiManager.getAnnotations();
////				for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
////					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
////					String prefix = getPrefix(text.getText());
////					if ( segmentChanged && prefix.equals(selectedPrefix) ) {
////						text.setLatticeSegment(segment);
////					}
////					if ( nameChanged && prefix.equals(selectedPrefix) ) {
////						text.setText( newPrefix + getPostfix(text.getText() ) );
////						text.updateText();
////					}
////				}
////				if ( nameChanged ) {
////					selectedPrefix = new String(newPrefix);
////					voiManager.setAnnotationPrefix( selectedPrefix );
////				}
////				annotationChanged();
////			}
////		}
	}
//
//	/* (non-Javadoc)
//	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
//	 */
//	public void valueChanged(ListSelectionEvent e) {
//
//		if ( e.getSource() == annotationList && e.getValueIsAdjusting() )
//			return;
//
//		if ( e.getSource() == annotationList ) {
//			if ( annotationTable.getRowCount() > 0 ) {
//				// Updates the displayLabel checkbox based on which row of the table is current:
//				VOI annotations = voiManager.getAnnotations();
//				int row = annotationTable.getSelectedRow();
//
//				boolean isChecked = true;
//				if ( (annotations != null) && (row >= 0) ) {
//					if ( row < annotations.getCurves().size() ) {
//						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(row);
//						if ( text.getVolumeVOI() != null )
//						{
//							isChecked = text.getVolumeVOI().GetDisplay();
//						}
//					}
//				}
//				displayLabel.setSelected(isChecked);
//			}
//		}
////		else if ( e.getSource() == annotationGroupList ) {
////			if ( annotationGroupTable.getRowCount() > 0 ) {
////				int row = annotationGroupTable.getSelectedRow();
////				if ( row != -1 ) {
////					if ( annotationGroupTableModel.getValueAt(row, 0) != null ) {
////						selectedPrefix = new String ( annotationGroupTableModel.getValueAt(row, 0).toString() );
////						//						System.err.println("valueChanged " + row + "  " + selectedPrefix );
////						voiManager.setAnnotationPrefix( selectedPrefix );
////						boolean displayAll = true;
////						VOI annotations = voiManager.getAnnotations();
////						for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
////							VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
////							String prefix = getPrefix(text.getText());
////							if ( prefix.equals(selectedPrefix) ) {
////								if ( text.getVolumeVOI() != null )
////								{
////									displayAll &= text.getVolumeVOI().GetDisplay();
////									if ( !displayAll ) {
////										break;
////									}
////								}
////							}
////						}
////						displayGroupLabel.setSelected(displayAll);						
////					}
////					else {
////						selectedPrefix = "new row";
////					}
////				}
////			}
////		}
//
//		imageA.notifyImageDisplayListeners();
//	}

	/**
	 * Creates the table that displays the annotation information.
	 * The user can edit the annotations directly in the table.
	 */
	private void buildAnnotationTable() {
		if ( annotationTable == null )
		{
			annotationTableModel = new DefaultTableModel();
			annotationTableModel.addColumn("Left");
			annotationTableModel.addColumn("x");
			annotationTableModel.addColumn("y");
			annotationTableModel.addColumn("z");
			annotationTableModel.addColumn("Right");
			annotationTableModel.addColumn("x");
			annotationTableModel.addColumn("y");
			annotationTableModel.addColumn("z");
			
			annotationTableModel.addTableModelListener(this);

			annotationTable = new JTable(annotationTableModel);
			annotationTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			annotationTable.addKeyListener(this);
			annotationTable.addMouseListener(this);

			annotationTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
//			annotationTable.getColumn("Name").setMinWidth(100);
//			annotationTable.getColumn("Name").setMaxWidth(100);
			annotationList = annotationTable.getSelectionModel();


//			annotationGroupTableModel = new DefaultTableModel();
//			annotationGroupTableModel.addColumn("Group Name");
//			annotationGroupTableModel.addColumn("lattice segment");
//			annotationGroupTableModel.addTableModelListener(this);
//
//			annotationGroupTable = new JTable(annotationGroupTableModel);
//			annotationGroupTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
//			annotationGroupTable.addKeyListener(this);
//
//			annotationGroupTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
//			//			annotationGroupTable.getColumn("Group Name").setMinWidth(100);
//			//			annotationGroupTable.getColumn("Group Name").setMaxWidth(100);
//			//			annotationGroupTable.getColumn("lattice segment").setMinWidth(100);
//			//			annotationGroupTable.getColumn("lattice segment").setMaxWidth(100);
//			annotationGroupList = annotationGroupTable.getSelectionModel();
//			annotationGroupList.addListSelectionListener(this);
		}
	}

	public JSplitPane getAnnotationsPanel() {
		return annotationPanel;
	}

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	public JSplitPane initDisplayAnnotationsPanel( VOILatticeManagerInterface voiInterface, ModelImage image )
	{		
		voiManager = voiInterface;
		imageA = image;
		voiManager.addLatticeListener(this);
		if ( annotationPanel == null )
		{			
			GridBagLayout gbc = new GridBagLayout();
			GridBagConstraints gbcC = new GridBagConstraints();

			JPanel labelPanel = new JPanel(gbc);
			gbcC.gridx = 0;			gbcC.gridy = 0;
//			 Display checkbox for displaying individual annotations:
			displaySeam = new JCheckBox("Display Labels", false);
			displaySeam.addActionListener(this);
			displaySeam.setActionCommand("displayLabel");
			labelPanel.add( displaySeam, gbcC );
			
			displayLattice = new JCheckBox("Display Lattice", true);
			displayLattice.addActionListener(this);
			displayLattice.setActionCommand("displayLattice");
			gbcC.gridy++;
			labelPanel.add( displayLattice, gbcC );

			labelPanel.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Display"));

			// build the annotation table for the list of annotations:
			buildAnnotationTable();
			// add annotation table to a scroll pane:
			JScrollPane kScrollPane = new JScrollPane(annotationTable);
			Dimension size = kScrollPane.getPreferredSize();
			//			System.err.println( size.width + " " + size.height );
			size.height /= 2;
			kScrollPane.setPreferredSize( size );
			kScrollPane.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Lattice"));

	
			annotationPanel = new JSplitPane( JSplitPane.VERTICAL_SPLIT, kScrollPane, labelPanel );
			annotationPanel.setOneTouchExpandable(true);
			annotationPanel.setDividerSize(6);
			annotationPanel.setContinuousLayout(true);
			annotationPanel.setResizeWeight(0.5);
			annotationPanel.setDividerLocation(0.5);
		}

		// Add the list of annotations to the table:
		latticeChanged();

		return annotationPanel;
	}

	private boolean ctrlKey = false;
	public void keyTyped(KeyEvent e) {

//		ctrlKey = e.isControlDown();
//
//		System.err.println("keyTyped");
//		if ( e.getKeyChar() == KeyEvent.VK_TAB ) {
//			if ( voiManager != null )
//			{
//				if ( (e.getSource() == annotationTable) ) {
//					// add a new annotation by tabbing:
//					int row = annotationTable.getSelectedRow();
//					int col = annotationTable.getSelectedColumn();
//					if ( (row == 0)  && (col == 0) ) {
//
//						VOIWormAnnotation text = new VOIWormAnnotation();
//						text.setText("center" );
//						int dimX = imageA.getExtents()[0];
//						int dimY = imageA.getExtents()[1];
//						int dimZ = imageA.getExtents()[2];
//						text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );
//						text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );
//
//						short id = (short) imageA.getVOIs().getUniqueID();
//						int colorID = 0;
//						VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
//						newTextVOI.getCurves().add(text);
//
//						voiManager.clear3DSelection();
//						voiManager.addAnnotation( newTextVOI );
//						voiManager.clear3DSelection();
//						int nRows = annotationTable.getRowCount();
//						annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
//					}
//				}
////				else if ( e.getSource() == annotationGroupTable ) {
////					int row = annotationGroupTable.getSelectedRow();
////					int col = annotationGroupTable.getSelectedColumn();
////					if ( (row == 0)  && (col == 0) ) {
////						annotationGroupTableModel.removeTableModelListener(this);
////						annotationGroupTableModel.addRow( new Object[]{  } );
////						annotationGroupTableModel.addTableModelListener(this);
////
////						int nRows = annotationGroupTable.getRowCount();
////						annotationGroupTable.setRowSelectionInterval(nRows-1, nRows-1);
////					}	
////				}
//			}
//		}
//		else if ( e.getKeyChar() == KeyEvent.VK_DELETE ) {
//			System.err.println("delete");
//			if ( (e.getSource() == annotationTable) ) {
//				int row = annotationTable.getSelectedRow();
//				int col = annotationTable.getSelectedColumn();
//				if ( col == 0 && row >= 0 )
//				{
//					TableCellEditor editor = annotationTable.getCellEditor();
//					if ( editor != null )
//						editor.stopCellEditing();
//					voiManager.deleteSelectedPoint();
//					int nRows = annotationTable.getRowCount();
//					if ( row < nRows ) {
//						annotationTable.setRowSelectionInterval(row, row);
//					}
//					else if ( nRows > 0 ) {
//						annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
//					}
//				}
//			}
////			else if ( (e.getSource() == annotationGroupTable) ) {
////				int row = annotationGroupTable.getSelectedRow();
////				int col = annotationGroupTable.getSelectedColumn();
////				if ( col == 0 && row >= 0 )
////				{
////					TableCellEditor editor = annotationGroupTable.getCellEditor();
////					if ( editor != null )
////						editor.stopCellEditing();
////
////					voiManager.deleteSelectedPoint();
////					int nRows = annotationGroupTable.getRowCount();
////					if ( row < nRows ) {
////						annotationGroupTable.setRowSelectionInterval(row, row);
////					}
////					else if ( nRows > 0 ) {
////						annotationGroupTable.setRowSelectionInterval(nRows-1, nRows-1);
////					}
////				}
////			}
//		}
	}

	@Override
	public void keyPressed(KeyEvent e) {
		ctrlKey = e.isControlDown();
	}

	@Override
	public void keyReleased(KeyEvent e) {
		ctrlKey = e.isControlDown();
	}

	public static String getPrefix(String name) {
		String prefix = new String();
		for ( int j = 0; j < name.length(); j++ ) {
			if ( Character.isLetter(name.charAt(j) ) ) {
				prefix += name.charAt(j);
			}
			else {
				break;
			}
		}
		return prefix;
	}

	public static String getPostfix(String name) {
		String prefix = new String();
		for ( int j = 0; j < name.length(); j++ ) {
			if ( Character.isLetter(name.charAt(j) ) ) {
				prefix += name.charAt(j);
			}
			else {
				break;
			}
		}
		//		System.err.println( name + "  " + prefix + "   " + (name.indexOf(prefix) + prefix.length()));
		return name.substring( name.indexOf(prefix) + prefix.length() );
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

}
