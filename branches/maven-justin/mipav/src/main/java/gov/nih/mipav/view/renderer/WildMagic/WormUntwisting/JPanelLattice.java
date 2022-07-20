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
import gov.nih.mipav.model.structures.VOIVector;
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

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
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


public class JPanelLattice extends JInterfaceBase implements ActionListener, LatticeListener, TableModelListener, KeyListener, MouseListener, ListSelectionListener, ChangeListener {

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
	
	// latice - based clipping:
	private JCheckBox viewPoint;
	private JSlider latticePosition;
	private JCheckBox clipLattice;
	private JSlider clipDistance;
	
	private JRadioButton splineModel;
	private JRadioButton ellipseModel;
	private JCheckBox ellipseCross;
	private JSlider ellipseDiameter;
	


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
			if ( voiManager != null )
			{
				voiManager.showLatticeLabels( displaySeam.isSelected() );
			}
		}
		else if ( command.equals("displayLattice") )
		{
			if ( voiManager != null )
			{
				voiManager.showLattice( displayLattice.isSelected() );
			}
		}
		else if ( command.equals("clipLattice") ) {
			int value = clipDistance.getValue();
			setLatticeClip( clipLattice.isSelected(), value );
		}
		else if ( command.equals("splineModel") ) {
			if ( voiManager != null )
			{
				float percentage = ellipseDiameter.getValue() / 100.f;
				voiManager.updateCrossSection( splineModel.isSelected(), ellipseCross.isSelected(), percentage );
			}
		}
		else if ( command.equals("ellipseCross") ) {
			if ( voiManager != null )
			{
				float percentage = ellipseDiameter.getValue() / 100.f;
				voiManager.updateCrossSection( splineModel.isSelected(), ellipseCross.isSelected(), percentage );
			}
		}
	}

	/* (non-Javadoc)
	 * Called from the LatticeModel when any annotations are changed.
	 * Updates the annotation table with the current annotations.
	 */
	public void latticeChanged() {
		if ( voiManager != null )
		{
			// get current annotations and update table:
			VOIVector lattice = voiManager.getLattice();
			
			// remove table listener during updates:
			annotationTableModel.removeTableModelListener(this);
			annotationList.removeListSelectionListener(this);
			int numRows = annotationTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				annotationTableModel.removeRow(i);
			}		

			if ( lattice != null ) {
				if ( lattice.size() >= 2 ) {
					VOI left = lattice.elementAt(0);
					VOI right = lattice.elementAt(1);

					// get picked point and index:
					Vector3f picked = voiManager.getLatticePickedPoint();
					int pickedIndex = -1;
					if ( picked != null ) {
						for ( int i = 0; i < left.getCurves().size(); i++ ) {
							if ( left.getCurves().elementAt(i).elementAt(0).equals(picked) || right.getCurves().elementAt(i).elementAt(0).equals(picked) ) {
								pickedIndex = i;
								break;
							}
						}
					}
					//					System.err.println("latticeChanged " + pickedIndex + " " + voiManager.isShift() );


					if ( left.getCurves().size() == right.getCurves().size() ) {
						for ( int i = 0; i < left.getCurves().size(); i++ ) {
							VOIWormAnnotation leftMarker = (VOIWormAnnotation) left.getCurves().elementAt(i);
							VOIWormAnnotation rightMarker = (VOIWormAnnotation) right.getCurves().elementAt(i);

							annotationTableModel.addRow( new Object[]{leftMarker.getText(), leftMarker.elementAt(0).X, leftMarker.elementAt(0).Y, leftMarker.elementAt(0).Z,
									rightMarker.getText(), rightMarker.elementAt(0).X, rightMarker.elementAt(0).Y, rightMarker.elementAt(0).Z} );
						}					

						annotationList.clearSelection();
						if ( pickedIndex != -1 ) {
							annotationList.addSelectionInterval(pickedIndex, pickedIndex);
						}
					}
				}

			}
			// restore table listener:
			annotationTableModel.addTableModelListener(this);
			annotationList.addListSelectionListener(this);
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
		//		System.err.println("tableChanged");
		// Track updates to the table and update the corresponding annotation.
		// The user can change the annotation name and position (x,y,z) with table edits.
		// Does not currently check type.
		boolean leftChanged = false;
		boolean rightChanged = false;
		if ( voiManager != null && (e.getSource() == annotationTableModel) )
		{
			int row = annotationTable.getSelectedRow();
			int column = annotationTable.getSelectedColumn();
			//			System.err.println(row + "  " + column );

			String newName = null;
			VOIVector lattice = voiManager.getLattice();
			if ( column < 4 ) {
				VOI left = lattice.elementAt(0);
				if ( (row >= 0) && (row < left.getCurves().size()) )
				{
					VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(row);
					VOIWormAnnotation newText = new VOIWormAnnotation(text);
					boolean changed = false;
					if ( column == 0 ) {
						if ( newText != null ) {
							leftChanged = true;
							newName = new String( annotationTable.getValueAt(row, column).toString() );

							if ( newName.contains("H") || newName.contains("V") || newName.contains("T")  || newName.contains("Q") ) {
								newText.setSeamCell(true);
							}
							else {
								newText.setSeamCell(false);
							}
							newText.setText( newName );
							newText.updateText();
							newText.update();
							newText.retwist(previewMode);
							changed = true;
						}
					}
					else 
					{
						float value = Float.valueOf(annotationTable.getValueAt(row, column).toString());
						if ( value >= 0 ) {
							if ( column == 1 ) {
								newText.elementAt(0).X = value;
							}
							else if ( column == 2 ) {
								newText.elementAt(0).Y = value;
							}
							else if ( column == 3 ) {
								newText.elementAt(0).Z = value;
							}
							newText.retwist(previewMode);
							changed = true;
						}
					}
					if ( changed ) {
						voiManager.updateLattice(true, text, newText);
					}
				}
			}
			else {
				VOI right = lattice.elementAt(1);
				if ( (row >= 0) && (row < right.getCurves().size()) )
				{
					VOIWormAnnotation text = (VOIWormAnnotation) right.getCurves().elementAt(row);
					VOIWormAnnotation newText = new VOIWormAnnotation(text);
					boolean changed = false;
					if ( column == 4 ) {
						if ( newText != null ) {
							rightChanged = true;
							newName = new String( annotationTable.getValueAt(row, column).toString() );
							if ( newName.contains("H") || newName.contains("V") || newName.contains("T")  || newName.contains("Q") ) {
								newText.setSeamCell(true);
							}
							else {
								newText.setSeamCell(false);
							}

							newText.setText( newName );
							newText.updateText();
							newText.update();
							newText.retwist(previewMode);
							changed = true;
						}
					}
					else 
					{
						float value = Float.valueOf(annotationTable.getValueAt(row, column).toString());
						if ( value >= 0 ) {
							if ( column == 5 ) {
								newText.elementAt(0).X = value;
							}
							else if ( column == 6 ) {
								newText.elementAt(0).Y = value;
							}
							else if ( column == 7 ) {
								newText.elementAt(0).Z = value;
							}
							newText.retwist(previewMode);
							changed = true;
						}
					}
					if ( changed ) {
						voiManager.updateLattice(false, text, newText);
					}
				}
			}
		}
		if ( leftChanged || rightChanged ) {
			renameLattice(leftChanged, rightChanged);
		}
	}

	public void valueChanged(ListSelectionEvent e) {
		if ( e.getSource() == annotationList && e.getValueIsAdjusting() )
			return;

		updateTableSelection(e);
		imageA.notifyImageDisplayListeners();
	}


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
			annotationTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			annotationTable.addKeyListener(this);
			annotationTable.addMouseListener(this);

			annotationTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			annotationList = annotationTable.getSelectionModel();
			annotationList.addListSelectionListener(this);
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
			labelPanel.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Display"));
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

			gbcC.gridx = 0;			gbcC.gridy = 0;
			JPanel clipPanel = new JPanel(gbc);
			clipPanel.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Lattice clipping"));

			gbcC.gridx = 0;
			clipLattice = new JCheckBox("clip", false);
			clipLattice.addActionListener(this);
			clipLattice.setActionCommand("clipLattice");
			clipPanel.add( clipLattice, gbcC );
			gbcC.gridx++;
			int max = Math.max(0, voiManager.getLatticeCurveLength() -1);
			clipDistance = new JSlider(0, max, 0);
			clipDistance.addChangeListener(this);
			clipPanel.add( clipDistance, gbcC );
			gbcC.gridy++;
			

			gbcC.gridx = 0;			gbcC.gridy = 0;
			JPanel modelPanel = new JPanel(gbc);
			modelPanel.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Model"));

			gbcC.gridx = 0;
			JLabel modelLable = new JLabel("Model: ");
			modelPanel.add( modelLable, gbcC );
			gbcC.gridx++;
			splineModel = new JRadioButton("spline", true);
			splineModel.addActionListener(this);
			splineModel.setActionCommand("splineModel");
			modelPanel.add( splineModel, gbcC );
			gbcC.gridx++;
			ellipseModel = new JRadioButton("ellipse", false);
			ellipseModel.addActionListener(this);
			ellipseModel.setActionCommand("splineModel");
			modelPanel.add( ellipseModel, gbcC );
			ButtonGroup g = new ButtonGroup();
			g.add(splineModel);
			g.add(ellipseModel);

			gbcC.gridx = 0;
			gbcC.gridy++;
			ellipseCross = new JCheckBox("cross-section", false);
			ellipseCross.addActionListener(this);
			ellipseCross.setActionCommand("ellipseCross");
			modelPanel.add( ellipseCross, gbcC );
			gbcC.gridx++;			ellipseDiameter = new JSlider(0, 200, 100);
			ellipseDiameter.addChangeListener(this);
			modelPanel.add( ellipseDiameter, gbcC );

			JPanel panels = new JPanel(new BorderLayout());
			panels.add(labelPanel, BorderLayout.NORTH);
			panels.add(clipPanel, BorderLayout.CENTER);
			panels.add(modelPanel, BorderLayout.SOUTH);

			// build the annotation table for the list of annotations:
			buildAnnotationTable();
			// add annotation table to a scroll pane:
			JScrollPane kScrollPane = new JScrollPane(annotationTable);
			Dimension size = kScrollPane.getPreferredSize();
			//			System.err.println( size.width + " " + size.height );
			size.height /= 2;
			kScrollPane.setPreferredSize( size );
			kScrollPane.setBorder(JDialogBase.buildTitledBorder(imageA.getImageName() + " Lattice"));


			annotationPanel = new JSplitPane( JSplitPane.VERTICAL_SPLIT, kScrollPane, panels );
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


	private void updateTableSelection(ListSelectionEvent e) {

		if ( e.getSource() == annotationList ) {
			if ( annotationTable.getRowCount() > 0 ) {
				int row = annotationTable.getSelectedRow();
				int column = annotationTable.getSelectedColumn();

				VOIVector lattice = voiManager.getLattice();
				VOI left = lattice.elementAt(0);
				VOI right = lattice.elementAt(1);
				for ( int i = 0; i < left.getCurves().size(); i++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(i);
					text.setSelected( false );
					text.updateSelected( imageA );
				}
				for ( int i = 0; i < right.getCurves().size(); i++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) right.getCurves().elementAt(i);
					text.setSelected( false );
					text.updateSelected( imageA );
				}

				if ( column < 4 ) {
					VOIWormAnnotation text = (VOIWormAnnotation) left.getCurves().elementAt(row);

					text.setSelected( true );
					text.updateSelected( imageA );
				}
				else {
					VOIWormAnnotation text = (VOIWormAnnotation) right.getCurves().elementAt(row);

					text.setSelected( true );
					text.updateSelected( imageA );
				}
				imageA.notifyImageDisplayListeners();
			}
		}
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		Object source = e.getSource();
//		if ( source == latticePosition ) {
//			int value = latticePosition.getValue();
//			setLatticeClip( clipLattice.isSelected(), value );
//		}
		if ( source == clipDistance ) {
			if ( clipDistance.getMaximum() == 0 ) {
				clipDistance.setMaximum(voiManager.getLatticeCurveLength() - 1);
			}
			int value = clipDistance.getValue();
			voiManager.setLatticeClip(clipLattice.isSelected(), value);
		}
		else if ( source == ellipseDiameter ) {
			if ( voiManager != null )
			{
				float percentage = ellipseDiameter.getValue() / 100.f;
				voiManager.updateCrossSection( splineModel.isSelected(), ellipseCross.isSelected(), percentage );
			}
		}
	}
	
	private void setLatticeClip(boolean clip, int position) {
		voiManager.setLatticeClip(clip, position);
	}
	
	private void renameLattice(boolean leftChanged, boolean rightChanged) {

		VOIVector lattice = voiManager.getLattice();
		VOI left = lattice.elementAt(0);
		VOI right = lattice.elementAt(1);
		

		int totalSeamCount = 0;
		int seamCount = 0;
		int nonSeamCount = 0;

		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(i);
			if ( leftAnnotation.isSeamCell() ) {
				totalSeamCount++;
			}
		}
		for ( int i = 0; i < left.getCurves().size(); i++ ) {
			VOIWormAnnotation leftAnnotation = ((VOIWormAnnotation)left.getCurves().elementAt(i));
			VOIWormAnnotation rightAnnotation = ((VOIWormAnnotation)right.getCurves().elementAt(i));
			if ( leftAnnotation.isSeamCell() ) {
				seamCount++;
			}
			else {
				nonSeamCount++;
			}
			String name = "";
			if ( totalSeamCount <= 10 ) {
				// H0-H1-H2-V1-V2-V3-V4-V5-V6-T 
				if ( leftAnnotation.isSeamCell() ) {
					name = seamCount < 4 ? ("H" + (seamCount-1)) : (seamCount <= 9) ? ("V" + (seamCount - 3)) : "T";
				}
				else {
					name = "a" + (nonSeamCount-1);
				}
			}
			else if ( totalSeamCount > 10 ) {
				// H0-H1-H2-V1-V2-V3-V4-Q-V5-V6-T 
				if ( leftAnnotation.isSeamCell() ) {
					name = seamCount < 4 ? ("H" + (seamCount-1)) : (seamCount < 8) ? ("V" + (seamCount - 3)) : (seamCount == 8) ? "Q" : (seamCount < 11) ? ("V" + (seamCount - 4)) : "T";
				}
				else {
					name = "a" + (nonSeamCount-1);
				}
			}
//			System.err.println(i + "   " + seamCount + "   " + name );
			leftAnnotation.setText( name + "L" );
			leftAnnotation.updateText();
			leftAnnotation.update();

			rightAnnotation.setText( name + "R" );
			rightAnnotation.updateText();
			rightAnnotation.update();
		}
		latticeChanged();
		
//		
//		
//		boolean qValue = false;
//		int totalSeamCount = 0;
//		for ( int i = 0; i < left.getCurves().size(); i++ ) {
//			VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(i);
//			if ( leftAnnotation.isSeamCell() ) {
//				totalSeamCount++;
//			}
//		}
//
//		int aCount = -1;
//		int aCountPrev = -1;
//		int aRename = -1;
//		int aRenameStartValue = -1;
//		int hCount = -1;
//		int hCountPrev = -1;
//		int hRename = -1;
//		int hRenameStartValue = -1;
//		int vCount = 0;
//		int vCountPrev = 0;
//		int vRename = -1;
//		int vRenameStartValue = -1;
//		boolean changed = false;
//		// check if lattice is already named, if so return as is:
//		for ( int i = 0; i < left.getCurves().size(); i++ ) {
//			VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(i);
//			String leftText = leftAnnotation.getText();
//			VOIWormAnnotation rightAnnotation = (VOIWormAnnotation) right.getCurves().elementAt(i);
//			String rightText = rightAnnotation.getText();
//			
//			// make sure the pairs match names:
//			String testLeft = leftText.replace("L", "");
//			String testRight = rightText.replace("R", "");
//			if ( !testLeft.equals(testRight) ) {
//				if ( leftChanged ) {
//					// match right to left:
//					testRight = leftText.replace("L", "R");
//					rightAnnotation.setText(testRight);
//					rightAnnotation.updateText();
//					rightAnnotation.update();
//					rightAnnotation.retwist(previewMode);
//					changed = true;
//				}
//				else if ( rightChanged ) {
//					// match left to right:
//					testLeft = rightText.replace("R", "L");
//					leftAnnotation.setText(testLeft);
//					leftAnnotation.updateText();
//					leftAnnotation.update();
//					leftAnnotation.retwist(previewMode);
//					changed = true;
//				}
//			}
//		}
//			
//			// get the number from the names:
//			leftText = leftAnnotation.getText();
//			int value = 0;
//			for ( int j = 0; j < leftText.length(); j++ ) {
//				if ( Character.isDigit(leftText.charAt(j)) ) {
//					value += 10 * value + Integer.parseInt(leftText.substring(j, j+1));
//				}
//			}
////			System.err.println( leftText + "   " + value );
//			if ( leftText.startsWith("a") ) {
//				aCountPrev = aCount;
//				aCount = value;
//				if ( (aCount - aCountPrev) != 1 ) {
//					// rename a starting at index i:
//					aRename = i;
//					aRenameStartValue = aCountPrev + 1;
//				}
//			}
//			else if ( leftText.startsWith("H") ) {
//				hCountPrev = hCount;
//				hCount = value;
//				if ( (hCount - hCountPrev) != 1 ) {
//					// rename a starting at index i:
//					hRename = i;
//					hRenameStartValue = hCountPrev + 1;
//				}
//			}
//			else if ( leftText.startsWith("V") ) {
//				vCountPrev = vCount;
//				vCount = value;
//				if ( (vCount - vCountPrev) != 1 ) {
//					// rename a starting at index i:
//					vRename = i;
//					vRenameStartValue = vCountPrev + 1;
//				}
//			}
//		}
//		if ( (aRename != -1) || (aRename != -1) || (vRename != -1) ) {
//
//			for ( int i = 0; i < left.getCurves().size(); i++ ) {
//				VOIWormAnnotation leftAnnotation = (VOIWormAnnotation) left.getCurves().elementAt(i);
//				String leftText = leftAnnotation.getText();
//				VOIWormAnnotation rightAnnotation = (VOIWormAnnotation) right.getCurves().elementAt(i);
//				String rightText = rightAnnotation.getText();
//				if ( (aRename != -1) && (i >= aRename) && leftText.startsWith("a") ) {
//					leftAnnotation.setText("a" + aRenameStartValue + "L");
//					leftAnnotation.updateText();
//					leftAnnotation.update();
//					leftAnnotation.retwist(previewMode);
//					
//					rightAnnotation.setText("a" + aRenameStartValue + "R");
//					rightAnnotation.updateText();
//					rightAnnotation.update();
//					rightAnnotation.retwist(previewMode);
//					
//					aRenameStartValue++;
//					changed = true;
//				}
//				if ( (hRename != -1) && (i >= hRename) && leftText.startsWith("H") ) {
//
//					leftAnnotation.setText("H" + hRenameStartValue + "L" );
//					leftAnnotation.updateText();
//					leftAnnotation.update();
//					leftAnnotation.retwist(previewMode);
//					
//					rightAnnotation.setText("H" + hRenameStartValue + "R" );
//					rightAnnotation.updateText();
//					rightAnnotation.update();
//					rightAnnotation.retwist(previewMode);
//					
//					hRenameStartValue++;
//					changed = true;
//				}
//				if ( (vRename != -1) && (i >= vRename) && leftText.startsWith("V") ) {
//
//					String nameL = "";
//					String nameR = "";
//					if ( (totalSeamCount <= 10) || qValue ) {
//						// H0-H1-H2-V1-V2-V3-V4-V5-V6-T 
//						nameL = "V" + vRenameStartValue + "L";
//						nameR = "V" + vRenameStartValue + "R";
//						vRenameStartValue++;
//					}
//					else if ( (totalSeamCount > 10) && !qValue && (vRenameStartValue == 5) ) {
//						// H0-H1-H2-V1-V2-V3-V4-Q-V5-V6-T 
//						nameL = "Q" + "L";
//						nameR = "Q" + "R";
//						qValue = true;
//					}
//					
//					leftAnnotation.setText(nameL);
//					leftAnnotation.updateText();
//					leftAnnotation.update();
//					leftAnnotation.retwist(previewMode);
//					
//					rightAnnotation.setText(nameR);
//					rightAnnotation.updateText();
//					rightAnnotation.update();
//					rightAnnotation.retwist(previewMode);
//					
//					changed = true;
//				}
//			}
//		}
		
//		if ( changed ) latticeChanged();
	}

}
