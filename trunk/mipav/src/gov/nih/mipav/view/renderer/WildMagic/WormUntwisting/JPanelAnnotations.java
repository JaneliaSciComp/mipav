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
import gov.nih.mipav.model.structures.VOIText;
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
import java.awt.Rectangle;
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
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableRowSorter;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class JPanelAnnotations extends JInterfaceBase implements ActionListener, AnnotationListener, TableModelListener, ListSelectionListener, KeyListener, ChangeListener, MouseListener, DocumentListener {

	private static final long serialVersionUID = -9056581285643263551L;

	private VolumeImage imageA;
	

	// on 'start' the images are loaded and the VolumeTriPlanarInterface is created:
	private VOILatticeManagerInterface voiManager;
	private VolumeTriPlanarRender volumeRenderer = null;
	// annotation panel displayed in the VolumeTriPlanarInterface:
	private JSplitPane annotationPanel;
	// turns on/off displaying individual annotations
	private JCheckBox volumeClip;
	private JSlider volumeRadius;
	private JCheckBox displayLabel;
	private JCheckBox displayGroupLabel;
	// table user-interface for editing the positions of the annotations:
	private ListSelectionModel annotationList;
	private JTable annotationTable;
	private DefaultTableModel annotationTableModel;
	private boolean useLatticeMarkers = false;

	// table user-interface for editing the positions of the annotations:
	private ListSelectionModel annotationGroupList;
	private JTable annotationGroupTable;
	private DefaultTableModel annotationGroupTableModel;
	private String selectedPrefix = null;
	
	private int displayChannel = 1;
	private JTextField thresholdMin;
	private JTextField thresholdMax;
	private ModelImage mask;
	private TriMesh mesh;
	private SurfaceState surfacState;
	private VOI segmentationCurve;
	private VOIContour segmentationContour;
	private JCheckBox displayMidline;
	private JCheckBox displaySurface;
	private JButton createCurve;
	
	private JTextField searchField;
	private Dimension searchFieldSize;
	
	private JPanelAnnotations sharedAnnotationPanel = null;

	public JPanelAnnotations( VOILatticeManagerInterface voiInterface, VolumeTriPlanarRender renderer, VolumeImage imageA ) {
		voiManager = voiInterface;
		volumeRenderer = renderer;
		this.imageA = imageA;
		voiManager.addAnnotationListener(this);
	}
	
	public void dispose() 
	{
		if ( mask != null ) {
			mask.disposeLocal(false);
			mask = null;
		}
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();
		if ( command.equals("displayAll") )
		{
			// display all annotations in the list:
			VOI annotations = voiManager.getAnnotations();
			if ( (annotations != null) ) {
				if ( annotations.getCurves().size() > 0 ) {
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						text.display(true);
					}
				}
			}
			displayLabel.setSelected(true);
		}
		else if ( command.equals("displayNone") )
		{
			// display none of the annotations in the list:
			VOI annotations = voiManager.getAnnotations();
			if ( (annotations != null) ) {
				if ( annotations.getCurves().size() > 0 ) {
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						text.display(false);
					}
				}
			}
			displayLabel.setSelected(false);
		}
		else if ( command.equals("openAnnotations" ) ) {

            // get the voi directory
            String fileName = null;
            String directory = null;
            String voiDir = null;

            final JFileChooser chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            }

            if (fileName != null) {
                voiDir = new String(directory + fileName);
//                System.err.println("Opening csv file " + voiDir );
                VOI annotations = LatticeModel.readAnnotationsCSV(voiDir);
                if ( annotations != null && annotations.getCurves().size() > 0 ) {
                	for ( int i = 0; i < annotations.getCurves().size(); i++) {

						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						text.retwist(previewMode);
                	}
                	voiManager.setAnnotations(annotations);
                }
            }
		}
		else if ( command.equals("thresholdSegmentation") ) {

			VOI annotations = voiManager.getAnnotations();
			if ( (annotations != null) ) {
				if ( annotations.getCurves().size() > 0 ) {
					Vector<Vector3f> seedList = new Vector<Vector3f>();
					float maxValue = -Float.MAX_VALUE;
					float minValue =  Float.MAX_VALUE;
					for ( int i = 0; i < annotations.getCurves().size(); i++ )
					{
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						if ( text.isSelected() ) {
							// read positions and values and set up segmentation:
							Vector3f pos = text.elementAt(0);
							seedList.add(new Vector3f(pos));

							float value = imageA.GetImage().getFloat( (int)pos.X, (int)pos.Y, (int)pos.Z );
							if ( value > maxValue ) maxValue = value;
							if ( value < minValue ) minValue = value;						
						}
					}
					if ( seedList.size() == 0 ) {
						MipavUtil.displayInfo( "No annotations selected, please select annotations for segmentation." );
						return;
					}
					BitSet visited = new BitSet(imageA.GetImage().getVolumeSize());
					if ( mask != null ) {
						mask.disposeLocal(false);
						mask = null;
					}
					mask = new ModelImage(ModelStorageBase.FLOAT, imageA.GetImage().getExtents(), JDialogBase.makeImageName(imageA.GetImage().getImageName(),  "_segmentation") );
					JDialogBase.updateFileInfo(imageA.GetImage(), mask);

					float minThreshold = minValue;
					try {
						minThreshold = Float.valueOf(thresholdMin.getText().trim());
					} catch(NumberFormatException e) {}
					float maxThreshold = (float)imageA.GetImage().getMax();
					try {
						maxThreshold = Float.valueOf(thresholdMax.getText().trim());
					} catch(NumberFormatException e) {}
					
					int count = WormSegmentation.fill(imageA.GetImage(), minThreshold, maxThreshold, seedList, visited, mask);

					
//					System.err.println("Segmentation results " + imageA.GetImage().getImageName() + "  " + count );

					// remove previous segmentation:
			        if ( segmentationCurve != null ) {
						imageA.GetImage().unregisterVOI(segmentationCurve);
						segmentationCurve = null;
			        }
			        
					volumeRenderer.removeSurface("nerveRing");
					volumeRenderer.displaySurface(false);	
		    		if ( mesh != null ) {
		    			mesh = null;
		    		}
					
					if ( count > 0 ) {

				        // Setup layout of 3D image for mapping sample coordinates
				        // to real coordinates.
				        int[] aiExtents = mask.getExtents();

				        ModelImage3DLayout kVolumeLayout = new ModelImage3DLayout(aiExtents[0], aiExtents[1], aiExtents[2],1,1,1,0,0, 0);

				        // Perform the skeletonization of the input image.
				        // Extract the centerline curve.
				        Skeleton3D kSkeleton = new Skeleton3D(mask, kVolumeLayout);

				        FlyPathGraphSamples kFlyPathGraphSamples = kSkeleton.getPathGraph(1, 1);

				        FlyPathGraphCurve kFlyPathGraphCurve = new FlyPathGraphCurve(kFlyPathGraphSamples, 0.07f, 2);
//				        System.err.println("Skelentonize " + kFlyPathGraphCurve.getNumBranches() );
				        if ( kFlyPathGraphCurve.getNumBranches() > 0 ) {
				        	Curve3f kCurve = kFlyPathGraphCurve.getCurvePosition(0);
				        	int numVertex = 100;
				        	segmentationCurve = new VOI( (short)0, "nerve_ring", VOI.CONTOUR, -1f);
				        	segmentationContour = new VOIContour(false);
				        	segmentationCurve.getCurves().add(segmentationContour);
				            float fStep = kCurve.GetTotalLength() / (numVertex - 1);
				            imageA.GetImage().registerVOI(segmentationCurve);
				            for ( int i = 0;i < numVertex; i++ ) {
				                float fDist = i * fStep;
				                float fTime = kCurve.GetTime(fDist, 100, 1e-02f);      
				                Vector3f kPoint = kCurve.GetPosition(fTime);
				                segmentationContour.add(new Vector3f(kPoint));
				            }
				            segmentationContour.update( new ColorRGBA(255,0,0, 1) );
				            displayMidline.setSelected(true);
				        }											
						
				    	if ( (Preferences.isGpuCompEnabled() && OpenCLAlgorithmBase.isOCLAvailable()) )
				    	{				    		
				    		OpenCLAlgorithmMarchingCubes cubesAlg = new OpenCLAlgorithmMarchingCubes(mask, 0, false,
				    				false, false, 0, null );
				    		cubesAlg.setRunningInSeparateThread(false);
				    		cubesAlg.run();
				    		mesh = cubesAlg.getMesh();
				    		if ( mesh != null ) {
//				    			System.err.println("extracted mesh " + mesh.VBuffer.GetVertexQuantity() );
				    			surfacState = new SurfaceState( new TriMesh(new VertexBuffer(mesh.VBuffer), new IndexBuffer(mesh.IBuffer)), "segmentation" );
				    			volumeRenderer.addSurface( surfacState, true );
				    			volumeRenderer.displaySurface(false);
				    			JPanelLights_WM lightsPanel = new JPanelLights_WM(volumeRenderer);
				    			lightsPanel.enableLight(0, true);
				    			lightsPanel.enableLight(1, true);
				    			volumeRenderer.updateLighting(lightsPanel.getAllLights());
				    		}
				    	}
					}
				}
			}
		}
		else if ( command.equals("deleteSegmentation") ) {
			// remove segmentation:
			volumeRenderer.removeSurface("segmentation");
			volumeRenderer.displaySurface(false);	

            imageA.GetImage().unregisterVOI(segmentationCurve);
            displayMidline.setSelected(false);
            displaySurface.setSelected(false);
		}
		else if ( command.equals("saveSegmentation") ) {
			if ( mask != null ) {
				LatticeModel.saveImage( imageA.GetImage(), mask, "segmentation", "" );
			}
			if ( mesh != null ) {
				LatticeModel.saveTriMesh( imageA.GetImage(), "segmentation", "_mesh", mesh );
			}
			// save the spline of the medial axis of the segmentation in .csv:
			if ( segmentationContour != null ) {
				LatticeModel.saveContourAsCSV( imageA.GetImage(), "segmentation", "_midLine", segmentationContour );
			}
		}
		else if ( command.equals("displayMidline") ) {
			if ( displayMidline.isSelected() ) {
				imageA.GetImage().registerVOI(segmentationCurve);
			}
			else {
				imageA.GetImage().unregisterVOI(segmentationCurve);
			}
		}
		else if ( command.equals("displaySurface") ) {
			volumeRenderer.displaySurface(displaySurface.isSelected());
		}
		else if ( source == displayLabel )
		{	
			// find the selected annotation and turn it's display on/off:
			if ( voiManager != null )
			{
				VOI annotations = voiManager.getAnnotations();
				// selected row:
				int row = annotationTable.getSelectedRow();		        
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(row);
						text.display(((JCheckBox)source).isSelected());
					}
				}
			}
		}
		else if ( source == displayGroupLabel )
		{	
			// find the selected annotation and turn it's display on/off:
			if ( (voiManager != null) && (selectedPrefix != null) )
			{
				//				System.err.println("Display Group " + selectedPrefix );
				VOI annotations = voiManager.getAnnotations();
				for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
					String prefix = getPrefix(text.getText());
					if ( prefix.equals(selectedPrefix) ) {
						text.display(((JCheckBox)source).isSelected());
					}
				}
			}
		}
		else if ( source == volumeClip )
		{
			boolean clip = volumeClip.isSelected();
			//			System.err.println("Clip annotation " + clip );
			// find the selected annotation and turn it's display on/off:
			if ( voiManager != null )
			{
				VOI annotations = voiManager.getAnnotations();
				// turn off clipping for all rows:
				for ( int i = 0; i < annotations.getCurves().size(); i++ ) 
				{
					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
					text.getVolumeVOI().setVolumeClip(false);					
				}
				// clip radius:
				float value = volumeRadius.getValue();
				// selected row:
				int row = annotationTable.getSelectedRow();		        
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(row);
						text.getVolumeVOI().setVolumeClip(clip);
						text.getVolumeVOI().setVolumeClipRadius(value);
					}
				}
			}
		}
		else if ( command.equals("createCurve") ) {

			VOI annotations = voiManager.getAnnotations();
			// find selected control points:
			Vector<VOIWormAnnotation> controlPoints = new Vector<VOIWormAnnotation>();
			for ( int i = 0; i < annotations.getCurves().size(); i++ ) 
			{
				VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
				if ( text.isSelected() ) {
					controlPoints.add(text);
				}
			}
			if ( controlPoints.size() > 0 ) {
				voiManager.addSplineControlPts(controlPoints);
			}
		}
		else if ( command.equals("match") ) {
			if ( sharedAnnotationPanel != null ) {
				VOIWormAnnotation text = sharedAnnotationPanel.getSelected();
				VOIWormAnnotation localText = getSelected();
				if ( text != null && localText != null ) {
					localText.setText( text.getText() );
					localText.updateText();
					annotationChanged();
				}
			}
		}
	}

	/* (non-Javadoc)
	 * Called from the LatticeModel when any annotations are changed.
	 * Updates the annotation table with the current annotations.
	 */
	public void annotationChanged() {
//		System.err.println("annotationChanged " + imageA.GetImage().getImageName() );

		if ( voiManager != null )
		{
			// get current annotations and update table:
			VOI annotations = voiManager.getAnnotations();
			
			// remove table listener during updates:
			annotationTableModel.removeTableModelListener(this);
			annotationList.removeListSelectionListener(this);
			int numRows = annotationTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				annotationTableModel.removeRow(i);
			}		
			// remove table listener during updates:
			annotationGroupTableModel.removeTableModelListener(this);
			annotationGroupList.removeListSelectionListener(this);
			numRows = annotationGroupTableModel.getRowCount();
			for ( int i = numRows -1; i >= 0; i-- ) {
				annotationGroupTableModel.removeRow(i);
			}		
			if ( annotations != null ) {
				if ( annotations.getCurves().size() > 0 ) {
					int[] segments = new int[annotations.getCurves().size()];
					Vector<String> names = new Vector<String>();
					for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						if ( text == null ) continue;
						
//						System.err.println( text.getText() );
						names.add(text.getText());
						Vector3f pos = text.elementAt(0);
						float value = imageA.GetImage().getFloat((int)pos.X, (int)pos.Y, (int)pos.Z);
						if ( imageA.GetImage().isColorImage() )
							value = imageA.GetImage().getFloatC((int)pos.X, (int)pos.Y, (int)pos.Z, displayChannel == 1 ? 1 : 2 );
						
						if ( useLatticeMarkers ) {
							int latticeSegment = text.getLatticeSegment();
							//							System.err.println(i + "  " + note);
							if ( latticeSegment != -1 ) {
								//								System.err.println(note + "   " + value );
								annotationTableModel.addRow( new Object[]{text.getText(), pos.X, pos.Y, pos.Z, latticeSegment, value } );
							}
							else {
								annotationTableModel.addRow( new Object[]{text.getText(), pos.X, pos.Y, pos.Z, "", value } );
								segments[i] = -1;
							}
						}
						else
						{
							annotationTableModel.addRow( new Object[]{text.getText(), pos.X, pos.Y, pos.Z, value } );
							segments[i] = -1;
						}
					}
					Vector<String> prefixList = new Vector<String>();
					String[] prefixes = new String[names.size()];
					for ( int i = 0; i < names.size(); i++ ) {
						String name = names.elementAt(i);
						String prefix = getPrefix(name);
						//						System.err.println( name + "  " + prefix + "  " + getPostfix(name) );
						prefixes[i] = prefix;
						if ( prefix.length() > 0 && !prefixList.contains(prefix) ) {
							prefixList.add(prefix);
							//							System.err.println(prefix);
						}
					}
					for ( int i = 0; i < prefixList.size(); i++ ) {
						String prefix = prefixList.elementAt(i);
						boolean segmentMatch = true;
						int segment = -1;
						for ( int j = 0; j < prefixes.length; j++ ) {
							if ( prefixes[j].equals(prefix) ) {
								if ( segments[j] == -1 ) {
									segmentMatch = false;
									break;
								}
								else if ( segment == -1 ) {
									segment = segments[j];
								}
								else if ( segment != segments[j] ) {
									segmentMatch = false;
									break;
								}
							}
						}
						if ( segmentMatch ) {
							annotationGroupTableModel.addRow( new Object[]{ prefix, segment } );
						}
						else {
							annotationGroupTableModel.addRow( new Object[]{ prefix } );
						}
					}
					//					System.err.println( selectedPrefix + "  " + prefixList.contains(selectedPrefix) );
					//					if ( !prefixList.contains(selectedPrefix) ) {
					//						annotationGroupTableModel.addRow( new Object[]{ selectedPrefix } );
					//					}
					

					annotationList.clearSelection();
					for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
						if ( text.isSelected() ) {
							int tableRow = getRow(annotationTable, text);
							if ( tableRow != -1 ) {
								annotationList.addSelectionInterval(tableRow, tableRow);
								annotationTable.scrollRectToVisible( new Rectangle(annotationTable.getCellRect(tableRow, 0, true)) );
							}
						}
					}
				}

			}
			// restore table listener:
			annotationTableModel.addTableModelListener(this);
			annotationList.addListSelectionListener(this);
			annotationGroupTableModel.addTableModelListener(this);
			annotationGroupList.addListSelectionListener(this);
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
		boolean isChecked = false;
		boolean isClipped = false;
		if ( voiManager != null && (e.getSource() == annotationTableModel) )
		{
			int row = annotationTable.getSelectedRow();
			int column = annotationTable.getSelectedColumn();
			System.err.println(row + "  " + column );

			String newName = null;
			VOI annotations = voiManager.getAnnotations();
			if ( (row >= 0) && (row < annotations.getCurves().size()) )
			{
				System.err.println("currentName " + currentName );
				VOIWormAnnotation text = getSelected(currentName, annotations);
				if ( text != null ) {
					if ( column == 0 )
					{
						newName = new String( annotationTable.getValueAt(row, column).toString() );
						text.setText( newName );
						text.updateText();
					}
					else if ( column < 4 )
					{
						float value = Float.valueOf(annotationTable.getValueAt(row, column).toString());
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
							text.retwist(previewMode);
						}
					}
					else if ( annotationTable.getColumnName(column).equals("segment") )
					{
						try {
							int value = Integer.valueOf(annotationTable.getValueAt(row, column).toString());
							text.setLatticeSegment(value);
						} catch ( java.lang.NumberFormatException error ) {
							text.setLatticeSegment(-1);
						}
					}
					text.update();
					if ( text.getVolumeVOI() != null ) {
						isChecked = text.getVolumeVOI().GetDisplay();
						isClipped = text.getVolumeVOI().GetClipped();
					}
					annotationChanged();
				}
			}

			displayLabel.setSelected(isChecked);
			volumeClip.setSelected(isClipped);
			

			currentAnnotationTableRow = -1;
			if ( newName != null ) {
				currentName = new String(newName);
			}
		}
		if ( voiManager != null && (e.getSource() == annotationGroupTableModel) )
		{
			int row = annotationGroupTable.getSelectedRow();
			int column = annotationGroupTable.getSelectedColumn();
			int segment = -1;
			String newPrefix = "";
			boolean nameChanged = false;
			boolean segmentChanged = false;
			if ( column == 0 ) {
				// new name:
				if ( annotationGroupTable.getValueAt(row, 0) != null ) {
					newPrefix = new String ( annotationGroupTable.getValueAt(row, 0).toString() );
					if ( selectedPrefix.equals("new row") ) 
					{
						selectedPrefix = new String(newPrefix);
						voiManager.setAnnotationPrefix( selectedPrefix );
					}
					else 
					{
						nameChanged = true;
					}
				}
			}
			else if ( column == 1 ) {
				// change the segment:
				try {
					segment = Integer.valueOf(annotationGroupTable.getValueAt(row, column).toString());
				} catch ( java.lang.NumberFormatException error ) {
					// value erased:
				}
				segmentChanged = true;
			}
			if ( nameChanged || segmentChanged ) 
			{
				VOI annotations = voiManager.getAnnotations();
				for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
					String prefix = getPrefix(text.getText());
					if ( segmentChanged && prefix.equals(selectedPrefix) ) {
						text.setLatticeSegment(segment);
					}
					if ( nameChanged && prefix.equals(selectedPrefix) ) {
						text.setText( newPrefix + getPostfix(text.getText() ) );
						text.updateText();
					}
				}
				if ( nameChanged ) {
					selectedPrefix = new String(newPrefix);
					voiManager.setAnnotationPrefix( selectedPrefix );
				}
				annotationChanged();
			}
		}
	}

	private int currentAnnotationTableRow = -1;
	private String currentName;

	/* (non-Javadoc)
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(ListSelectionEvent e) {

//		System.err.println("valueChanged");
		if ( e.getSource() == annotationList && e.getValueIsAdjusting() )
			return;

		updateTableSelection(e);

		if ( e.getSource() == annotationList ) {
			if ( annotationTable.getRowCount() > 0 ) {
				// Updates the displayLabel checkbox based on which row of the table is current:
				VOI annotations = voiManager.getAnnotations();
				int row = annotationTable.getSelectedRow();
				int column = annotationTable.getSelectedColumn();
//				System.err.println(row + "  " + column );
				if ( row != currentAnnotationTableRow ) {
					currentAnnotationTableRow = row;
//					String selectedName = "";
					VOIWormAnnotation text = getSelected(row, column, annotationTable, annotations);
					if ( text != null ) {
//						selectedName = new String(text.getText());
//						System.err.println( " selectedName " + selectedName );
						currentName = new String(text.getText());
					}
//					System.err.println( " current " + currentName );
				}

				boolean isChecked = true;
				boolean isClipped = true;
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIWormAnnotation text = getSelected(currentName, annotations);
						if ( text != null ) {
							if ( text.getVolumeVOI() != null )
							{
								isChecked = text.getVolumeVOI().GetDisplay();
								isClipped = text.getVolumeVOI().GetClipped();
							}
						}
					}
				}
				displayLabel.setSelected(isChecked);
				volumeClip.setSelected(isClipped);
			}
		}
		else if ( e.getSource() == annotationGroupList ) {
			if ( annotationGroupTable.getRowCount() > 0 ) {
				int row = annotationGroupTable.getSelectedRow();
				if ( row != -1 ) {
					if ( annotationGroupTable.getValueAt(row, 0) != null ) {
						selectedPrefix = new String ( annotationGroupTable.getValueAt(row, 0).toString() );
						//						System.err.println("valueChanged " + row + "  " + selectedPrefix );
						voiManager.setAnnotationPrefix( selectedPrefix );
						boolean displayAll = true;
						VOI annotations = voiManager.getAnnotations();
						for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
							VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
							String prefix = getPrefix(text.getText());
							if ( prefix.equals(selectedPrefix) ) {
								if ( text.getVolumeVOI() != null )
								{
									displayAll &= text.getVolumeVOI().GetDisplay();
									if ( !displayAll ) {
										break;
									}
								}
							}
						}
						displayGroupLabel.setSelected(displayAll);						
					}
					else {
						selectedPrefix = "new row";
					}
				}
			}
		}

		imageA.GetImage().notifyImageDisplayListeners();
	}

	/**
	 * Creates the table that displays the annotation information.
	 * The user can edit the annotations directly in the table.
	 */
	private void buildAnnotationTable(boolean latticeMarkers) {
		if ( annotationTable == null )
		{
			annotationTableModel = new DefaultTableModel() {

			    @Override
			    public boolean isCellEditable(int row, int column) {
			    	String columnName = getColumnName(column);
			    	if ( columnName.equals("value") ) return false;
			       return true;
			    }
			};
			
			annotationTableModel.addColumn("Name");
			annotationTableModel.addColumn("x");
			annotationTableModel.addColumn("y");
			annotationTableModel.addColumn("z");
			if ( latticeMarkers ) {
				annotationTableModel.addColumn("lattice segment");
				useLatticeMarkers = true;
			}
			annotationTableModel.addColumn("value");
			
			annotationTableModel.addTableModelListener(this);

			annotationTable = new JTable(annotationTableModel);
			annotationTable.setRowSorter(new TableRowSorter(annotationTableModel));
			annotationTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			annotationTable.addKeyListener(this);
			annotationTable.addMouseListener(this);

			annotationTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			annotationTable.getColumn("Name").setMinWidth(100);
			annotationTable.getColumn("Name").setMaxWidth(100);
			annotationList = annotationTable.getSelectionModel();
			annotationList.addListSelectionListener(this);


			annotationGroupTableModel = new DefaultTableModel();
			annotationGroupTableModel.addColumn("Group Name");
			annotationGroupTableModel.addColumn("lattice segment");
			annotationGroupTableModel.addTableModelListener(this);

			annotationGroupTable = new JTable(annotationGroupTableModel);
			annotationGroupTable.setRowSorter(new TableRowSorter(annotationGroupTableModel));
			annotationGroupTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			annotationGroupTable.addKeyListener(this);

			annotationGroupTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			//			annotationGroupTable.getColumn("Group Name").setMinWidth(100);
			//			annotationGroupTable.getColumn("Group Name").setMaxWidth(100);
			//			annotationGroupTable.getColumn("lattice segment").setMinWidth(100);
			//			annotationGroupTable.getColumn("lattice segment").setMaxWidth(100);
			annotationGroupList = annotationGroupTable.getSelectionModel();
			annotationGroupList.addListSelectionListener(this);
		}
	}

	public JSplitPane getAnnotationsPanel() {
		return annotationPanel;
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
//    	gbc.fill = GridBagConstraints.HORIZONTAL;
    }
	
	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	public JSplitPane initDisplayAnnotationsPanel( VOILatticeManagerInterface voiInterface, VolumeImage image, boolean latticeMarkers )
	{		
		return initDisplayAnnotationsPanel(voiInterface, image, latticeMarkers, false);
	}

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	public JSplitPane initDisplayAnnotationsPanel( VOILatticeManagerInterface voiInterface, VolumeImage image, boolean latticeMarkers, boolean matchImages )
	{		
		voiManager = voiInterface;
		imageA = image;
		voiManager.addAnnotationListener(this);
		if ( annotationPanel == null )
		{			
			initGB();
			JPanel thresholdPanel = new JPanel(gbLayout);
			thresholdPanel.setBorder(JDialogBase.buildTitledBorder("Threshold Segmentation"));

			thresholdPanel.add( new JLabel("min value:"), gbc );
			thresholdMin = new JTextField( String.valueOf(imageA.GetImage().getMax()/2) );
			gbc.gridx++;
			thresholdPanel.add( thresholdMin, gbc );
			
			gbc.gridx = 0;
			gbc.gridy++;
			thresholdPanel.add( new JLabel("max value:"), gbc );
			thresholdMax = new JTextField( String.valueOf(imageA.GetImage().getMax()) );
			gbc.gridx++;
			thresholdPanel.add( thresholdMax, gbc );
			
			// Segment button:
			gbc.gridx = 0;
			gbc.gridy++;
			JButton segment = new JButton("threshold segmentation" );
			segment.addActionListener(this);
			segment.setActionCommand("thresholdSegmentation");
			segment.setPreferredSize(MipavUtil.defaultButtonSize);
			thresholdPanel.add( segment, gbc );
			gbc.gridx++;
			JButton deleteSegmentation = new JButton("remove" );
			deleteSegmentation.addActionListener(this);
			deleteSegmentation.setActionCommand("deleteSegmentation");
			deleteSegmentation.setPreferredSize(MipavUtil.defaultButtonSize);
			thresholdPanel.add( deleteSegmentation, gbc );
			gbc.gridx++;
			JButton saveSegmentation = new JButton("save" );
			saveSegmentation.addActionListener(this);
			saveSegmentation.setActionCommand("saveSegmentation");
			saveSegmentation.setPreferredSize(MipavUtil.defaultButtonSize);
			thresholdPanel.add( saveSegmentation, gbc );

			gbc.gridx = 0;
			gbc.gridy++;
			displayMidline = new JCheckBox("Display Midline", false);
			displayMidline.addActionListener(this);
			displayMidline.setActionCommand("displayMidline");
			thresholdPanel.add( displayMidline, gbc );
			gbc.gridx++;
			displaySurface = new JCheckBox("Display Segmentation Surface", false);
			displaySurface.addActionListener(this);
			displaySurface.setActionCommand("displaySurface");
			thresholdPanel.add( displaySurface, gbc );

			initGB();
			JPanel labelPanel = new JPanel(gbLayout);
			// Display checkbox for displaying individual annotations:
			displayLabel = new JCheckBox("display", true);
			displayLabel.addActionListener(this);
			displayLabel.setActionCommand("displayLabel");

			// Display checkbox for displaying individual annotations:
			displayGroupLabel = new JCheckBox("display group", true);
			displayGroupLabel.addActionListener(this);
			displayGroupLabel.setActionCommand("displayGroupLabel");

			gbc.gridx = 0;			gbc.gridy = 0;
			labelPanel.add( new JLabel("Annotation: " ), gbc );
			gbc.gridx++;			gbc.gridy = 0;
			labelPanel.add(displayLabel, gbc);

			gbc.gridx++;			gbc.gridy = 0;
			labelPanel.add(displayGroupLabel, gbc);

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

			// Open CSV button:
			JButton openAnnotations = new JButton("Open annotation .csv" );
			openAnnotations.addActionListener(this);
			openAnnotations.setActionCommand("openAnnotations");
			gbc.gridx = 0;			gbc.gridy++;
			labelPanel.add( openAnnotations, gbc );

			// volume clip checkbox for clipping around individual annotations:
			volumeClip = new JCheckBox("volume clip", true);
			volumeClip.setSelected(false);
			volumeClip.addActionListener(this);
			volumeClip.setActionCommand("volumeClip");
			gbc.gridx = 1;			gbc.gridy++;
			labelPanel.add( volumeClip, gbc );

			volumeRadius = new JSlider(0, 70, 30);
			volumeRadius.addChangeListener(this);
			gbc.gridx++;
			labelPanel.add( volumeRadius, gbc );
			
			// Create curve button:
			createCurve = new JButton("curve" );
			createCurve.setEnabled(false);
			createCurve.addActionListener(this);
			createCurve.setActionCommand("createCurve");
			gbc.gridx = 0;			gbc.gridy++;
			labelPanel.add( createCurve, gbc );
			
			labelPanel.setBorder(JDialogBase.buildTitledBorder("Display and Clipping"));

			// build the annotation table for the list of annotations:
			buildAnnotationTable(latticeMarkers);
			// add annotation table to a scroll pane:
			JPanel panel = new JPanel( new GridBagLayout() );
			initGB();
			// create search bar:
			searchField = new JTextField("Search annotations");
			searchField.setFont(MipavUtil.font12I);
			searchField.getDocument().addDocumentListener(this);
			searchField.addKeyListener(this);
			searchField.addMouseListener(this);	        
			panel.add(searchField, gbc);
			
			
			gbc.gridy++;
			panel.add( annotationTable.getTableHeader(), gbc );
			gbc.gridy++;
			panel.add( annotationTable, gbc );
			gbc.gridy++;
			if ( matchImages ) {
				JButton match = new JButton("match" );
				match.addActionListener(this);
				match.setActionCommand("match");
				match.setPreferredSize(MipavUtil.defaultButtonSize);
				match.setSize(MipavUtil.defaultButtonSize);
				match.setMaximumSize(MipavUtil.defaultButtonSize);
				panel.add(match, gbc );
			}
//			JScrollPane kScrollPane = new JScrollPane(annotationTable);
			JScrollPane kScrollPane = new JScrollPane(panel);
			Dimension size = kScrollPane.getPreferredSize();
			//			System.err.println( size.width + " " + size.height );
			size.height /= 2;
			kScrollPane.setPreferredSize( size );
			kScrollPane.setBorder(JDialogBase.buildTitledBorder( imageA.GetImage().getImageName() + " Annotation list"));


			// add annotation table to a scroll pane:
			JScrollPane kScrollPaneGroup = new JScrollPane(annotationGroupTable);
			size = kScrollPaneGroup.getPreferredSize();
			//			System.err.println( size.width + " " + size.height );
			size.height /= 2;
			kScrollPaneGroup.setPreferredSize( size );
			kScrollPaneGroup.setBorder(JDialogBase.buildTitledBorder( imageA.GetImage().getImageName() + " Group list"));

			JSplitPane displayPanel = new JSplitPane( JSplitPane.VERTICAL_SPLIT, thresholdPanel, labelPanel );
			displayPanel.setOneTouchExpandable(true);
			displayPanel.setDividerSize(6);
			displayPanel.setContinuousLayout(true);
			displayPanel.setResizeWeight(0.5);
			displayPanel.setDividerLocation(0.5);
			
			JSplitPane listPanel = new JSplitPane( JSplitPane.VERTICAL_SPLIT, kScrollPane, kScrollPaneGroup );
			listPanel.setOneTouchExpandable(true);
			listPanel.setDividerSize(6);
			listPanel.setContinuousLayout(true);
			listPanel.setResizeWeight(0.5);
			listPanel.setDividerLocation(0.5);
			

			
			annotationPanel = new JSplitPane( JSplitPane.VERTICAL_SPLIT, listPanel, displayPanel );
			annotationPanel.setOneTouchExpandable(true);
			annotationPanel.setDividerSize(6);
			annotationPanel.setContinuousLayout(true);
			annotationPanel.setResizeWeight(0.5);
			annotationPanel.setDividerLocation(0.5);
		}

		// Add the list of annotations to the table:
		annotationChanged();

		return annotationPanel;
	}

	private boolean ctrlKey = false;
	private int searchIndex = 0;
	public void keyTyped(KeyEvent e) {
		ctrlKey = e.isControlDown();

//		System.err.println("keyTyped");
		if ( e.getKeyChar() == KeyEvent.VK_TAB ) {
			if ( voiManager != null )
			{
				if ( (e.getSource() == annotationTable) ) {
					// add a new annotation by tabbing:
					int row = annotationTable.getSelectedRow();
					int col = annotationTable.getSelectedColumn();
					if ( (row == 0)  && (col == 0) ) {

						VOIWormAnnotation text = new VOIWormAnnotation();
						text.setText("center" );
						int dimX = imageA.GetImage().getExtents()[0];
						int dimY = imageA.GetImage().getExtents()[1];
						int dimZ = imageA.GetImage().getExtents()[2];
						text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );
						text.add( new Vector3f( dimX/2, dimY/2, dimZ/2 ) );

						short id = (short) imageA.GetImage().getVOIs().getUniqueID();
						int colorID = 0;
						VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
						newTextVOI.getCurves().add(text);

						voiManager.clear3DSelection();
						voiManager.addAnnotation( newTextVOI );
						voiManager.clear3DSelection();
						int nRows = annotationTable.getRowCount();
						annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
					}
				}
				else if ( e.getSource() == annotationGroupTable ) {
					int row = annotationGroupTable.getSelectedRow();
					int col = annotationGroupTable.getSelectedColumn();
					if ( (row == 0)  && (col == 0) ) {
						annotationGroupTableModel.removeTableModelListener(this);
						annotationGroupTableModel.addRow( new Object[]{  } );
						annotationGroupTableModel.addTableModelListener(this);

						int nRows = annotationGroupTable.getRowCount();
						annotationGroupTable.setRowSelectionInterval(nRows-1, nRows-1);
					}	
				}
			}
		}
		else if ( e.getKeyChar() == KeyEvent.VK_DELETE ) {
//			System.err.println("delete");
			if ( (e.getSource() == annotationTable) ) {
				int row = annotationTable.getSelectedRow();
				int col = annotationTable.getSelectedColumn();
				if ( col == 0 && row >= 0 )
				{
					TableCellEditor editor = annotationTable.getCellEditor();
					if ( editor != null )
						editor.stopCellEditing();
					voiManager.deleteSelectedPoint();
					int nRows = annotationTable.getRowCount();
					if ( row < nRows ) {
						annotationTable.setRowSelectionInterval(row, row);
					}
					else if ( nRows > 0 ) {
						annotationTable.setRowSelectionInterval(nRows-1, nRows-1);
					}
				}
			}
			else if ( (e.getSource() == annotationGroupTable) ) {
				int row = annotationGroupTable.getSelectedRow();
				int col = annotationGroupTable.getSelectedColumn();
				if ( col == 0 && row >= 0 )
				{
					TableCellEditor editor = annotationGroupTable.getCellEditor();
					if ( editor != null )
						editor.stopCellEditing();

					voiManager.deleteSelectedPoint();
					int nRows = annotationGroupTable.getRowCount();
					if ( row < nRows ) {
						annotationGroupTable.setRowSelectionInterval(row, row);
					}
					else if ( nRows > 0 ) {
						annotationGroupTable.setRowSelectionInterval(nRows-1, nRows-1);
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

	public void stateChanged(ChangeEvent arg0) {
		Object source = arg0.getSource();
		if ( source == volumeRadius ) {
			float value = volumeRadius.getValue();
			if ( voiManager != null )
			{
				VOI annotations = voiManager.getAnnotations();
				// selected row:
				int row = annotationTable.getSelectedRow();		        
				if ( (annotations != null) && (row >= 0) ) {
					if ( row < annotations.getCurves().size() ) {
						VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(row);
						text.getVolumeVOI().setVolumeClipRadius(value);
					}
				}
			}
		}
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
		if ( e.getSource() == searchField ) {
			searchFieldSize = searchField.getSize();
			searchField.setMinimumSize(searchFieldSize);
			searchField.setPreferredSize(searchFieldSize);
			searchField.setText("");
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {}
	
	public void setSharedAnnotationPanel( JPanelAnnotations panel ) {
		sharedAnnotationPanel = panel;
	}
	
	public VOIWormAnnotation getSelected() {

		if ( annotationTable.getRowCount() > 0 ) {
			int[] rows = annotationTable.getSelectedRows();

			// Updates the displayLabel checkbox based on which row of the table is current:
			VOI annotations = voiManager.getAnnotations();

			for ( int i = 0; i < rows.length; i++ ) {
				VOIWormAnnotation text = getSelected(rows[i], 0, annotationTable, annotations );
				if ( text != null ) return text;
			}
		}
		return null;
	}

	private VOIWormAnnotation getSelected( String name, VOI annotations) {
		for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
			if ( text.getText().equals(name) ) {
				return text;
			}
		}
		return null;
	}

	private VOIWormAnnotation getSelected(int row, int column, JTable table, VOI annotations) {
		if ( row < 0 || column  < 0 ) return null;
		
		String name = new String ( table.getValueAt(row, column).toString() );
		for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
			VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
			if ( text.getText().equals(name) ) {
				return text;
			}
		}
		return null;
	}

	private int getRow(JTable table, VOIWormAnnotation text) {
		for ( int i = 0; i < table.getRowCount(); i++ ) {
			String name = new String ( table.getValueAt(i, 0).toString() );
			if ( text.getText().equals(name) ) {
				return i;
			}
		}
		return -1;
	}
	
	private void updateTableSelection(ListSelectionEvent e) {

		int numSelected = -1;
		if ( e.getSource() == annotationList ) {
			if ( annotationTable.getRowCount() > 0 ) {
				int[] rows = annotationTable.getSelectedRows();

				// Updates the displayLabel checkbox based on which row of the table is current:
				VOI annotations = voiManager.getAnnotations();
				
//				System.err.println( voiManager + " " + imageA.GetImage().getImageName() );

				for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
					text.setSelected( false );
					text.updateSelected( imageA.GetImage() );
				}

				for ( int i = 0; i < rows.length; i++ ) {
					VOIWormAnnotation text = getSelected(rows[i], 0, annotationTable, annotations ); //(VOIWormAnnotation) annotations.getCurves().elementAt(rows[i]);
					if ( text != null ) {
						text.setSelected( true );
						text.updateSelected( imageA.GetImage() );
						numSelected++;
					}
				}
				imageA.GetImage().notifyImageDisplayListeners();
			}
		}
		else if ( e.getSource() == annotationGroupList ) {
			int[] rows = annotationGroupTable.getSelectedRows();

			// Updates the displayLabel checkbox based on which row of the table is current:
			VOI annotations = voiManager.getAnnotations();

			for ( int i = 0; i < annotations.getCurves().size(); i++ ) {
				VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
				text.setSelected( false );
				text.updateSelected( imageA.GetImage() );
			}
			annotationList.removeListSelectionListener(this);
			annotationList.clearSelection();
			for ( int i = 0; i < rows.length; i++ ) {
				selectedPrefix = new String ( annotationGroupTable.getValueAt(rows[i], 0).toString() );

				for ( int j = 0; j < annotations.getCurves().size(); j++ ) {
					VOIWormAnnotation text = (VOIWormAnnotation) annotations.getCurves().elementAt(j);
					String prefix = getPrefix(text.getText());
					if ( prefix.equals(selectedPrefix) ) {
//						System.err.println("updateTableSelection " + prefix + " " + selectedPrefix );
						text.setSelected(true);
						text.updateSelected(imageA.GetImage());
						int tableRow = getRow(annotationTable, text);
						if ( tableRow != -1 ) {
							annotationList.addSelectionInterval(tableRow, tableRow);
							annotationTable.scrollRectToVisible( new Rectangle(annotationTable.getCellRect(tableRow, 0, true)) );
						}
						numSelected++;
					}
				}
			}
			annotationList.addListSelectionListener(this);
			imageA.GetImage().notifyImageDisplayListeners();
		}
		createCurve.setEnabled(numSelected > 1);
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
//		System.err.println("insertUpdate " + searchField.getText());
		searchText();
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
//		System.err.println("removeUpdate " + searchField.getText());
		searchText();
	}

	@Override
	public void changedUpdate(DocumentEvent e) {
//		System.err.println("changedUpdate " + searchField.getText());
		searchText();
	}
	
	private void searchText() {

		String searchText = searchField.getText();	
		searchText = searchField.getText();	
		System.err.println("keyTyped " + searchText + " list size = " + annotationTable.getRowCount());
		searchText = searchText.toLowerCase();
		searchText = searchText.trim();
		boolean found = false;
		for ( int i = 0; i < annotationTable.getRowCount(); i++ ) {
			String label = new String ( annotationTable.getValueAt(i, 0).toString() );
			label = label.trim();
			label = label.toLowerCase();
//			System.err.println( "   " + i + "  " + searchText + "   " + label.equals(searchText) + "   " + label );
			if ( label.equals(searchText) ) {
				found = true;
				annotationTable.setRowSelectionInterval(i, i);
				annotationTable.scrollRectToVisible( new Rectangle(annotationTable.getCellRect(i, 0, true)) );
				searchIndex = 0;
				break;
			}
		}
		if ( !found ) {
			for ( int i = searchIndex; i < annotationTable.getRowCount(); i++ ) {
				String label = new String ( annotationTable.getValueAt(i, 0).toString() );
				label = label.trim();
				label = label.toLowerCase();
				if ( label.startsWith(searchText) ) {
					found = true;
					annotationTable.setRowSelectionInterval(i, i);
					annotationTable.scrollRectToVisible( new Rectangle(annotationTable.getCellRect(i, 0, true)) );
					searchIndex = i+1;
					break;
				}
			}
			if ( !found ) {
				searchIndex = 0;
				for ( int i = searchIndex; i < annotationTable.getRowCount(); i++ ) {
					String label = new String ( annotationTable.getValueAt(i, 0).toString() );
					label = label.trim();
					label = label.toLowerCase();
					if ( label.startsWith(searchText) ) {
						found = true;
						annotationTable.setRowSelectionInterval(i, i);
						annotationTable.scrollRectToVisible( new Rectangle(annotationTable.getCellRect(i, 0, true)) );
						searchIndex = i+1;
						break;
					}
				}
			}
		}
	}
	
}
