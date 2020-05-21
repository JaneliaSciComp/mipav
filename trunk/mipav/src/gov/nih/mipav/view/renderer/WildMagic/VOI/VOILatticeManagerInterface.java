package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.CurveListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.VOIWormAnnotation;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.TriMesh;


/**

 *
 */
public class VOILatticeManagerInterface extends VOIManagerInterface
{

	private boolean doAnnotations = false;
	private boolean doAutomaticLabels = false;
	// enables editing or adding points in 3D
	private boolean mouseSelection3D = false;
	public static float VoxelSize =  0.1625f;


	private LatticeModel latticeModel;

	private JTextField defaultVoxelSize;

	private JDialog updateVoxelSize;

	//    private VOI pickedPoint = null;
	private boolean movingPickedPoint = false;

	/**
	 * Creates a VOIManagerInterface object.
	 * @param kParent the parent frame, must be a VOIManagerInterfaceListener
	 * @param kImageA imageA
	 * @param kImageB imageB
	 * @param iNViews number of views displayed in the parent.
	 * @param bGPU set to true if this VOIManagerInterface is part of the GPU-based Volume Renderer.
	 * @param kVOIGroup for ViewJFrameImage and ViewJFrameTriImage, so the VOI Toolbar can be part of a larger button group.
	 */
	public VOILatticeManagerInterface ( VOIManagerInterfaceListener kParent,
			ModelImage kImageA, ModelImage kImageB, int iNViews, boolean bGPU, ButtonGroup kVOIGroup )
	{
		super(kParent, kImageA, kImageB, iNViews, bGPU, kVOIGroup );
	}

	/* 
	 * Handles all VOI Action commands from the VOI toolbar and VOI Menu.
	 * @param event ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();

		if ( command.equals("AddAnnotations") ) {
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Annotations");
			doAnnotations = true;
		}
		else if ( command.equals("EditAnnotations") ) {
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Annotations");
			doAnnotations = true;
		}
		else if ( command.equals("OpenAnnotations") )
		{
			final JFileChooser chooser = new JFileChooser();

			if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
				chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
			} else {
				chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
			}

			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

			final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

			if (returnVal == JFileChooser.APPROVE_OPTION) {
				String fileName = chooser.getSelectedFile().getName();
				String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
				Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
				openAnnotations(directory, fileName);
			}
		} 
		else if ( command.equals("OpenNeurite") ) {
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

			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

			final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

			if (returnVal == JFileChooser.APPROVE_OPTION) {
				fileName = chooser.getSelectedFile().getName();
//				System.err.println( fileName );
				directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
				Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
			}

			if (fileName != null) {
				VOIVector annotations = new VOIVector();
				voiDir = new String(directory + fileName + File.separator);
				loadAllVOIsFrom(voiDir, false, annotations, true);

				if ( latticeModel != null )
				{
					saveVOIs("OpenNeurite");
					latticeModel.addNeurite( annotations.elementAt(0), fileName );
				}
				else
				{
					latticeModel = new LatticeModel( m_kImageA );
					latticeModel.addNeurite( annotations.elementAt(0), fileName );
				}
			}
		} 
		else if ( command.equals("OpenLattice") )
		{
			final JFileChooser chooser = new JFileChooser();

			if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
				chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
			} else {
				chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
			}

			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

			final int returnVal = chooser.showOpenDialog(m_kParent.getFrame());

			if (returnVal == JFileChooser.APPROVE_OPTION) {
				String fileName = chooser.getSelectedFile().getName();
				String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
				Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
				openLattice(directory, fileName);
			}
		} 
		else if ( command.equals("AddLattice") ) {
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Lattice");
			doAnnotations = false;
		} 
		else if ( command.equals("EditLattice") ) {
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Lattice");
			if ( latticeModel != null )
			{
				latticeModel.clearAddLeftRightMarkers();
			}
			doAnnotations = false;
		} 
		else if ( command.equals("SaveLattice") ) {
			if ( latticeModel != null )
			{
				latticeModel.saveLattice( );
			}
		} 
		else if ( command.equals("ShowExpandedModel") ) {
			if ( latticeModel != null )
			{
				latticeModel.showExpandedModel( );
			}
		} 
		else if ( command.equals("ShowModel") ) {
			if ( latticeModel != null )
			{
				latticeModel.showModel( );
			}
		}
		else if ( command.equals("StraightenLattice") ) {
			if ( latticeModel != null )
			{
				latticeModel.interpolateLattice( true, false, true, false );
				voiMenuBuilder.setMenuItemEnabled("Show Expanded Model", true);
			}
		} 
		else if ( command.equals("voxelSize") ) {
			setVoxelSize();
		} 
		else if ( command.equals("OKVoxelSize") ) {
			try {
				float value = Float.valueOf(defaultVoxelSize.getText());
				if ( value > 0 )
				{
					VoxelSize = Float.valueOf(defaultVoxelSize.getText());
					updateVoxelSize.setVisible(false);
					updateVoxelSize.dispose();
				}
				else
				{
					MipavUtil.displayError( "Enter a voxel size > 0" );
					defaultVoxelSize.requestFocus();
				}
			}
			catch ( java.lang.NumberFormatException e )
			{
				MipavUtil.displayError( "Enter a number > 0" );
				defaultVoxelSize.requestFocus();
			}
		} 
		else if ( command.equals("SegmentNerveRing") ) {
			mouseSelection3D = true;
		}
		else if ( command.equals("animateStraightening") )
		{
			System.err.println("Starting Worm Straightening Animation");
			animationStep = 1;
		}
		else if ( command.equals("recordStraightening") )
		{
			System.err.println("Starting Worm Straightening Recording");
			animationStep = 2;
		}
		else {
			super.actionPerformed(event);
		}

	}
	
	private int animationStep = -1;
	public int getAnimationStep()
	{
		return animationStep;
	}
	
	public void setAnimationStep(int i)
	{
		animationStep = -1;
	}

	public String getAnnotationPrefix()
	{
		if ( latticeModel != null )
		{
			return latticeModel.getAnnotationPrefix();
		}
		return "A";
	}

	public void setAnnotationPrefix(String s)
	{
		if ( latticeModel != null )
		{
			latticeModel.setAnnotationPrefix(s);
		}
	}
	
	public int getCurrentIndex()
	{
		if ( latticeModel != null )
		{
			return latticeModel.getCurrentIndex();
		}
		return 0;
	}
	
	public void colorAnnotations( boolean setColor )
	{
		if ( latticeModel != null ) {
			latticeModel.colorAnnotations(setColor);
		}
	}

	/**
	 * Adds an annotation listener to the latticeModel.
	 * @param listener
	 */
	public void addAnnotationListener( AnnotationListener listener ) {

		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		latticeModel.addAnnotationListener(listener);
	}
	
	public void addCurveListener( CurveListener listener ) {

		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		latticeModel.addCurveListener(listener);
	}
	
	public void addLatticeListener( LatticeListener listener ) {

		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		latticeModel.addLatticeListener(listener);
	}
	
	/**
	 * Enable editing annotations in either 3D or 2D windows with the mouse.
	 * @param automaticLabels if true the labels are created with numbers only (no leading 'A' for annotation).
	 */
	public void editAnnotations( boolean automaticLabels )
	{
		mouseSelection3D = true;
		doAnnotations = true;
		doAutomaticLabels = automaticLabels;
	}
	
	public boolean isEditAnnotations()
	{
		return doAnnotations;
	}
	
	/**
	 * Returns the automatic labels flag.
	 * @return
	 */
	public boolean doAutomaticLabels()
	{
		return doAutomaticLabels;
	}
	
	public void editLattice()
	{
		mouseSelection3D = true;
		if ( latticeModel != null )
		{
			latticeModel.clearAddLeftRightMarkers();
		}
		doAnnotations = false;
	}
	public void editClip() {
		mouseSelection3D = false;
	}
	
	public void openLattice( String directory, String fileName )
	{
		if (fileName != null)
		{
			VOIVector lattice = new VOIVector();
			String voiDir = new String(directory + fileName + File.separator);
			loadAllVOIsFrom(voiDir, false, lattice, false);
			setLattice(lattice);
		}
	}

	public void saveLattice(String directory, String fileName)
	{
		if ( latticeModel != null )
		{
			latticeModel.saveLattice( directory, fileName );
		}
	}

	public void openNeuriteCurves() {

		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		latticeModel.openNeuriteCurves();
	}
	
	public void saveNeuriteCurves( )
	{
		if ( latticeModel != null )
		{
			latticeModel.saveNeuriteCurves( );
		}
	}
	
	public void setLattice( VOIVector lattice )
	{
		boolean saveL = true;
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
			saveL = false;
		}
		if ( saveL )
		{
			saveVOIs("loadLattice");
		}
		latticeModel.setLattice( lattice );	
//		latticeModel.expandLattice();
	}

	
	/**
	 * Untwists the worm image quickly for the preview mode - without saving any images or statistics
	 * @return untwisted image.
	 */
	public ModelImage untwistTest()
	{
		if ( latticeModel == null ) return null;
		return latticeModel.untwistTest();
	}
	
	public void openAnnotations( String directory, String fileName )
	{
		if (fileName != null)
		{
			VOIVector annotations = new VOIVector();
			String voiDir = new String(directory + fileName + File.separator);
			loadAllVOIsFrom(voiDir, false, annotations, true);

			setAnnotations(annotations);
		}
	}

	public void addAnnotations( VOIVector annotations )
	{
		VOI newAnnotationVOI = annotations.size() > 0 ? annotations.elementAt(0).getCurves() != null ? annotations.elementAt(0) : null : null;

		boolean saveA = true;
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
			saveA = false;
		}
		if ( saveA )
		{
			saveVOIs("addAnnotations");
		}
		latticeModel.addAnnotations( newAnnotationVOI );
	}
	
	public void setAnnotations( VOIVector annotations )
	{
		VOI newAnnotationVOI = annotations.size() > 0 ? annotations.elementAt(0).getCurves() != null ? annotations.elementAt(0) : null : null;

		boolean saveA = true;
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
			saveA = false;
		}
		if ( saveA )
		{
			saveVOIs("loadAnnotations");
		}
		latticeModel.setAnnotations( newAnnotationVOI );
	}
	
	public void setAnnotations( VOI annotations )
	{
		boolean saveA = true;
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
			saveA = false;
		}
		if ( saveA )
		{
			saveVOIs("loadAnnotations");
		}
		latticeModel.setAnnotations( annotations );
	}

	public void add3DMarker( VOI textVOI, boolean automaticLabel, boolean multiSelect )
	{
		add3DMarker(textVOI, automaticLabel, multiSelect, false );
	}

	private boolean isShiftSelected = false;
	public boolean isShift() { return isShiftSelected; }
	public void setShiftDown( boolean isShift ) { isShiftSelected = isShift; }
	
	public void add3DMarker( VOI textVOI, boolean automaticLabel, boolean multiSelect, boolean isShift )
	{
		isShiftSelected = isShift;
		if ( doAnnotations )
		{
			textVOI.setActive(automaticLabel);
			if ( !automaticLabel )
			{
				textVOI.setActive(false);
				System.err.println("selectAnnotation JDialogAnnotation" );
				new JDialogAnnotation(m_kImageA, textVOI, 0, true, true);
				if ( !textVOI.isActive() )
				{
					return;
				}
			}
			addAnnotation(textVOI, multiSelect);
		}
		else
		{
			addLeftRightMarker(textVOI, isShift);
		}
	}

	public void clear3DSelection()
	{
		if ( latticeModel != null )
		{
			latticeModel.clear3DSelection();
		}
		if ( movingPickedPoint )
		{
			movingPickedPoint = false;
		}
	}
	
	public void flipLattice() {
		if ( latticeModel != null ) {
			latticeModel.flipLattice();
		}
	}

	public boolean is3DSelectionEnabled()
	{
		return mouseSelection3D;
	}

	public boolean select3DMarker( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean rightMouse, boolean multiSelect, boolean isShift )
	{
		if ( doAnnotations )
		{
			return selectAnnotations(startPt, endPt, pt, rightMouse, multiSelect);
		}
		return selectLattice(startPt, endPt, pt, isShift);
	}

	public boolean modify3DMarker( Vector3f startPt, Vector3f endPt, Vector3f pt )
	{
		if ( doAnnotations )
		{
			return modifyAnnotations(pt);
		}
		return modifyLattice(startPt, endPt, pt);
	}
	
	public void deleteSelectedPoint()
	{
		if ( latticeModel != null )
		{
			if ( latticeModel.hasPicked() )
			{
				saveVOIs("deleteSelectedPoint");
				latticeModel.deleteSelectedPoint(doAnnotations);
			}
		}		
	}
	
	public boolean hasSelectedPoint()
	{
		if ( latticeModel != null )
		{
			return latticeModel.hasPicked();
		}		
		return false;
	}
	
	public Vector<VOIWormAnnotation> getPickedAnnotation() {
		if ( latticeModel != null )
		{
			return latticeModel.getPickedAnnotation();
		}
		return null;
	}

	public void moveSelectedPoint( Vector3f direction )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("moveSelectedPoint");
			}
			latticeModel.moveSelectedPoint(direction, doAnnotations);
		}
	}

	public void updateSelectedPoint( Color color )
	{
		if ( latticeModel != null )
		{
			latticeModel.updateSelectedPoint(color);
			updateDisplay();
		}
	}
	
	/**
	 * Set the new imageA and imageB for creating VOIs.
	 * @param imageA
	 * @param imageB
	 */
	public void setImage( ModelImage imageA, ModelImage imageB )
	{
		m_kImageA = imageA;
		m_kImageB = imageB;
		if ( latticeModel != null )
		{
			latticeModel.setImage(imageA);
		}
	}
	
	public ModelImage getImage()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getImage();
	}
	
	public void updateManager( int index, int orientation )
	{
		m_kVOIManagers.elementAt(index).setImage(m_kImageA, orientation);
	}
    
    protected void initVOIManagers(int iNViews) {
        m_kVOIManagers = new Vector<VOIManager>();
        for ( int i = 0; i < iNViews; i++ )
        {
            m_kVOIManagers.add(new VOILatticeManager(this));
            ((VOILatticeManager)m_kVOIManagers.lastElement()).setInterface(this);
        }
        if ( (popup != null) && (popupPt != null) ) {
        	for ( int i = 0; i < iNViews; i++ )
        	{
        		m_kVOIManagers.elementAt(i).setPopupVOI(popup);
        		m_kVOIManagers.elementAt(i).setPopupPt(popupPt);
        	}
        }
    }
    

	protected void redoVOIs()
	{
		movingPickedPoint = false;
		if ( m_kRedoList.isEmpty() )
		{
			return;
		}
		m_kUndoList.add( getVOIState() );
		setVOIState( m_kRedoList.remove( m_kRedoList.size() - 1) );
		if ( latticeModel != null )
		{
			latticeModel.redo();
		}

		if ( imageStatList != null )
		{
			imageStatList.refreshVOIList(getActiveImage().getVOIs());
		}
		if (m_kVOIDialog != null) {
			m_kVOIDialog.updateVOIPanel(m_kCurrentVOIGroup, getActiveImage() );
		}
		updateDisplay();
	}

	protected void undoVOIs()
	{
		movingPickedPoint = false;
		if ( m_kUndoList.size() <= 0 )
		{
			return;
		}
		m_kRedoList.add( getVOIState() );
		setVOIState( m_kUndoList.remove( m_kUndoList.size() - 1) );
		if ( latticeModel != null )
		{
			latticeModel.undo();
		}

		if ( imageStatList != null )
		{
			imageStatList.refreshVOIList(getActiveImage().getVOIs());
		}
		if (m_kVOIDialog != null) {
			m_kVOIDialog.updateVOIPanel(m_kCurrentVOIGroup, getActiveImage() );
		}
		updateDisplay();
	}

	public void deleteAnnotations() {
		if ( latticeModel != null )
		{
			latticeModel.deleteAnnotations();
		}
	}

	public void showModel( boolean display ) {
		if ( latticeModel != null )
		{
			latticeModel.showModel(display);
		}		
	}
	
	public boolean isModelDisplayed() {

		if ( latticeModel != null )
		{
			return latticeModel.isModelDisplayed();
		}		
		return false;
	}
	
	public void showLattice( boolean display ) {
		if ( latticeModel != null )
		{
			latticeModel.showLattice(display);
		}		
	}
	
	public void showLatticeLabels( boolean display ) {
		if ( latticeModel != null )
		{
			latticeModel.showLatticeLabels(display);
		}		
	}

	public void setLatticeClip(boolean clip, int position) {
		if ( latticeModel != null )
		{
//			latticeModel.showMarker(position);
			float extent = latticeModel.getDiameter(position);
//			latticeModel.getCenter().getVolumeVOI().setVolumeClip(clip);
			renderer.setLatticeClip(clip, latticeModel.getCenter(position), latticeModel.getBasisVectors(position), new float[] {extent,extent,10});
		}		
	}

	
	public TriMesh generateTriMesh( int stepsize ) {
		if ( latticeModel != null )
		{
			return latticeModel.generateTriMesh( true, stepsize);
		}		
		return null;
	}
	
	public void setPaddingFactor( int padding ) {
		if ( latticeModel != null )
		{
			latticeModel.setPaddingFactor(padding);
		}		
	}
	
	public void addSplineControlPts( Vector<VOIWormAnnotation> controlPts ) {
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		saveVOIs("addCurve");
		latticeModel.addSplineControlPts(controlPts);
	}
	
	/**
	 * Add an annotation to the latticeModel.
	 * @param textVOI new annotation.
	 */
	public void addAnnotation( VOI textVOI, boolean multiSelect )
	{       
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		saveVOIs("addAnnotation");
		latticeModel.addAnnotation(textVOI, multiSelect);
	}
	
	/**
	 * Add an annotation to the latticeModel.
	 * @param textVOI new annotation.
	 */
	public void addAnnotation( VOI textVOI )
	{       
		addAnnotation(textVOI, false);
	}
	
	public void displayAnnotation( String name, boolean display )
	{
		if ( latticeModel == null )
		{
			return;
		}
		latticeModel.displayAnnotation(name, display);
	}
	
	/**
	 * Returns list of annotations from lattice model.
	 * @return latticeModel annotations.
	 */
	public VOI getAnnotations()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getAnnotations();		
	}
	
	public VOI getAnnotationsStraight()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getAnnotationsStraight();		
	}
	
	public Vector<String> getSplineCurves()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getSplineCurves();		
	}

	public void setCurveVisible(String name, boolean visible)
	{
		if ( latticeModel != null )
		{
			latticeModel.setCurveVisible(name, visible);
		}		
	}
	
	public void setCurveName(String oldName, String newName)
	{
		if ( latticeModel != null )
		{
			latticeModel.setCurveName(oldName, newName);
		}
	}
	
	public void setCurveSelected(String name, boolean selected)
	{
		if ( latticeModel != null )
		{
			latticeModel.setCurveSelected(name, selected);
		}
	}
	
	public boolean isCurveSelected(String name)
	{
		if ( latticeModel != null )
		{
			return latticeModel.isCurveSelected(name);
		}
		return false;
	}
	
	public void deleteSelectedCurve()
	{
		if ( latticeModel != null )
		{
			latticeModel.deleteSelectedCurve();
		}
	}
	
	public Vector3f getLatticePickedPoint()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getPicked();		
	}
	
	public VOIVector getLattice()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getLattice();		
	}
	

	public int getLatticeCurveLength() {
		if ( latticeModel != null )
		{
			return latticeModel.getLatticeCurveLength();
		}
		return 0;
	}
	
	public VOIVector getLatticeStraight()
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.getLatticeStraight();		
	}

	public boolean isPreview()
	{
		if ( latticeModel == null )
		{
			return false;
		}
		return latticeModel.isPreview( );
	}

	
	public void setPreviewMode( boolean preview, VOIVector lattice, VOI annotations )
	{
		clearUndoRedo();
		latticeModel.setPreviewMode( preview, lattice, annotations );
		saveVOIs("loadAnnotations");
	}
	
	public VOI retwistAnnotations(VOIVector lattice) 
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.retwistAnnotations(lattice);	
	}
	
	public VOIVector retwistLattice(VOIVector lattice) 
	{
		if ( latticeModel == null )
		{
			return null;
		}
		return latticeModel.retwistLattice(lattice);	
	}
	
	private void addLeftRightMarker( VOI textVOI, boolean isShift )
	{       
		if ( latticeModel == null )
		{
			latticeModel = new LatticeModel( m_kImageA );
		}
		if ( latticeModel.getPicked() != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("moveLeftRightMarker");
			}
			latticeModel.modifyLeftRightMarker( textVOI.getCurves().elementAt(0).elementAt(0) );
		}
		else
		{
			Vector3f pt = latticeModel.getPicked( textVOI.getCurves().elementAt(0).elementAt(0), false );
			if ( pt == null )
			{
				saveVOIs("addLeftRightMarker");
				latticeModel.addLeftRightMarker( textVOI.getCurves().elementAt(0).elementAt(0), isShift );
			}
		}
	}
	
    public void addVOI( VOIBase kNew, boolean bQuickLUT, boolean bUpdate, boolean isFinished )
    {
    	if ( doAnnotations && kNew.getType() == VOI.ANNOTATION )
    	{
			short id = (short) m_kImageA.getVOIs().getUniqueID();
			int colorID = 0;
			VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
			newTextVOI.getCurves().add(kNew);

			if ( latticeModel != null )
			{
				latticeModel.clear3DSelection();
			}
    		addAnnotation( newTextVOI );
    	}
    	else
    	{
    		super.addVOI(kNew, bQuickLUT, bUpdate, isFinished );
    	}
    }

	private boolean modifyLattice( Vector3f startPt, Vector3f endPt, Vector3f pt )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("modifyLattice");
			}
			return latticeModel.modifyLattice(startPt, endPt, pt);
		}
		return false;
	}

	private boolean selectLattice( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean isShift )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("selectLattice");
			}
			return latticeModel.selectLattice(startPt, endPt, pt, isShift);
		}
		return false;
	}

	private boolean modifyAnnotations( Vector3f pt )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("modifyAnnotations");
			}
			return latticeModel.modifyAnnotation(pt);
		}
		return false;
	}

	private boolean selectAnnotations( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean rightMouse, boolean multiSelect )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("selectAnnotations");
			}
			return latticeModel.selectAnnotation(startPt, endPt, pt, rightMouse, multiSelect);
		}
		return false;
	}

	public void updateAnnotation( VOIWormAnnotation annotation )
	{
		if ( latticeModel != null )
		{
			saveVOIs("updateAnnotation");
			latticeModel.updateAnnotation(annotation);
		}
	}

	public void updateAnnotation( VOIText annotation )
	{
		if ( latticeModel != null )
		{
			saveVOIs("updateAnnotation");
			latticeModel.updateAnnotation(new VOIWormAnnotation(annotation));
		}
	}

	private void setVoxelSize()
	{
		JButton OK = new JButton( "OK" );
		OK.setActionCommand("OKVoxelSize");
		OK.addActionListener(this);
		defaultVoxelSize = new JTextField( "" + VoxelSize );
		defaultVoxelSize.addActionListener(this);
		JPanel panel = new JPanel( new GridLayout(1, 3) );
		panel.add( new JLabel( "Current Voxel Size" ) );
		panel.add( defaultVoxelSize );
		panel.add( new JLabel("um") );

		updateVoxelSize = new JDialog();
		updateVoxelSize.getContentPane().setLayout(new BorderLayout());
		updateVoxelSize.setModalityType( JDialog.ModalityType.APPLICATION_MODAL);    	
		updateVoxelSize.getContentPane().add( panel, BorderLayout.NORTH );
		updateVoxelSize.getContentPane().add( OK, BorderLayout.SOUTH );
		updateVoxelSize.pack();
		updateVoxelSize.setResizable(false);

		MipavUtil.centerOnScreen(updateVoxelSize);
		updateVoxelSize.setVisible(true);
	}

	
//	private void testSegmentation()
//	{
//        long time = System.currentTimeMillis();
//		Random r = new Random(time);
//		System.err.println( Math.random() );
//		
//		ModelImage test = new ModelImage( ModelStorageBase.ARGB_FLOAT, m_kImageA.getExtents(), "testNuclei" );
//		final int dimX = m_kImageA.getExtents().length > 0 ? m_kImageA.getExtents()[0] : 1;
//		final int dimY = m_kImageA.getExtents().length > 1 ? m_kImageA.getExtents()[1] : 1;
//		final int dimZ = m_kImageA.getExtents().length > 2 ? m_kImageA.getExtents()[2] : 1;
//		
//		int[] radiiCounts = new int[12];
//		int count = (int) (Math.random() * 100 + 400);
//		Vector<Vector3f> nuclei = new Vector<Vector3f>();
//		Vector<Integer> radii = new Vector<Integer>();
//		while ( nuclei.size() < count )
//		{
//			int exp = (int) (1 + 10*Math.random());
//			int plusMinus = (int) Math.pow( -1, exp );
//			float shift = (float) (5 * plusMinus * Math.random());
//			Integer radius = new Integer( (int) Math.round(10 + shift) );
//			float x = (float) Math.random() * (dimX - radius * 4) + radius * 2;
//			float y = (float) Math.random() * (dimY - radius * 4) + radius * 2;
//			float z = (float) Math.random() * (dimZ - radius * 4) + radius * 2;
//			Vector3f pt = new Vector3f( Math.round(x), Math.round(y), Math.round(z) );
//			boolean found = false;
//			for ( int i = 0; i < nuclei.size(); i++ )
//			{
//				if ( nuclei.elementAt(i).distance(pt) < 2*radius )
//				{
//					found = true;
//					break;
//				}
//			}
//			if ( !found )
//			{
//				nuclei.add(pt);
//				radii.add(radius);
////				System.err.println( nuclei.size() + " " + pt + " " + radius );
//				radiiCounts[radius-5]++;
//			}
////			System.err.println( nuclei.size() );
//		}
//		
//		int minValue = 100;
//		int maxValue = 255;
//		Vector3f newPt = new Vector3f();
//    	for ( int z = 0; z < dimZ; z++ )
//    	{
//    		for ( int y = 0; y < dimY; y++ )
//    		{
//    			for ( int x = 0; x < dimX; x++ )
//    			{
//    				newPt.set(x,y,z);
//    				for ( int i = 0; i < nuclei.size(); i++ )
//    				{
//    					Integer radius = radii.elementAt(i);
//    					if ( newPt.distance(nuclei.elementAt(i)) <= radius )
//    					{
//    						int currentValue = (int) Math.min( 255, Math.max(0, (minValue + (maxValue - minValue) * Math.random()) ));
//    						test.setC(x,  y, z, 0, 255);
//    						test.setC(x,  y, z, 1, currentValue);
//    						test.setC(x,  y, z, 2, currentValue);
//    						test.setC(x,  y, z, 3, currentValue);
//    						break;
//    					}
//    				}
//    			}
//    		}
////			System.err.println( z );
//    	}
//    	test.calcMinMax();
////    	new ViewJFrameImage(test);
//
//		System.err.println( "seeded " + count + " nuclei" );
//    	for ( int i = 0; i < radiiCounts.length; i++ )
//    	{
//    		System.err.println( (i+5) + " " + radiiCounts[i] );
//    	}
////		Vector<Vector3f> nuclei = WormSegmentationWindowing.nucleiSegmentation(m_kImageA, 150, 10, 20);
//    	VOI foundNuclei = WormSegmentationWindowing.findMarkers(test, minValue -1, maxValue, 5, 15 );
////		Vector<Vector3f> foundNuclei = WormSegmentationKMeans.segmentAll1(test, 100f, 255, 500);
//		if ( foundNuclei != null )
//		{
//			Vector3f pt = new Vector3f();
////			for ( int i = 0; i < foundNuclei.size(); i++ )
////			{
////				pt.copy(foundNuclei.elementAt(i));
////				if ( test.getFloatC( (int)pt.X, (int)pt.Y, (int)pt.Z, 1 ) < 255 )
////					System.err.println( i + " zero center value " +  test.getFloatC( (int)pt.X, (int)pt.Y, (int)pt.Z, 1 ) );
////			}
//			
//	    	for ( int z = 0; z < dimZ; z++ )
//	    	{
//	    		for ( int y = 0; y < dimY; y++ )
//	    		{
//	    			for ( int x = 0; x < dimX; x++ )
//	    			{
//	    				pt.set(x,y,z);
//	    				boolean found = false;
//	    				for ( int i = 0; i < foundNuclei.getCurves().size() && !found; i++ )
//	    				{
//	    					VOIContour contour = (VOIContour) foundNuclei.getCurves().elementAt(i);
//	    					for ( int j = 0; j < contour.size(); j++ )
//	    					{
//	    						if ( contour.elementAt(j).distance(pt) < 2 )
//	    						{
//	    							test.setC(x,  y, z, 1, 255);
//	    							test.setC(x,  y, z, 2, 0);
//	    							test.setC(x,  y, z, 3, 0);
//	    							found = true;
//	    							break;
//	    						}
//	    					}
//	    				}
//	    			}
//	    		}
////				System.err.println( z );
//	    	}	    	
//	    	count = 0;
//			for ( int i = 0; i < foundNuclei.getCurves().size(); i++ )
//			{
//				count += foundNuclei.getCurves().elementAt(i).size();
//			}
//	    	System.err.println( "Found " + count + " nuclei " );
//		}
//		else
//		{
//	    	System.err.println( "Found 0 nuclei " );				
//		}
//    	test.calcMinMax();
//    	new ViewJFrameImage(test);
//	}
	
}
