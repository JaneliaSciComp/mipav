package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.AnnotationListener;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormSegmentation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.BitSet;
import java.util.Random;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**

 *
 */
public class VOILatticeManagerInterface extends VOIManagerInterface
{

	private boolean doAnnotations = false;
	private boolean doAutomaticLabels = false;
	private boolean mouse3D = false;
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
			mouse3D = voiMenuBuilder.isMenuItemSelected("Add Annotations");
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Annotations");
			doAnnotations = true;
		}
		else if ( command.equals("EditAnnotations") ) {
			mouse3D = voiMenuBuilder.isMenuItemSelected("Add Annotations");
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
		else if ( command.equals("SaveAnnotations") ) {
			if ( latticeModel != null )
			{
				latticeModel.saveAnnotations( );
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
			mouse3D = voiMenuBuilder.isMenuItemSelected("Add Lattice Points");
			mouseSelection3D = voiMenuBuilder.isMenuItemSelected("Edit Lattice");
			doAnnotations = false;
		} 
		else if ( command.equals("EditLattice") ) {
			mouse3D = voiMenuBuilder.isMenuItemSelected("Add Lattice Points");
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
		else if ( command.equals("TestLattice") ) {
			WormData wormData = new WormData( m_kImageA );
			wormData.testLattice( latticeModel.getLeft(), latticeModel.getRight(), true );
		} else if ( command.equals("StraightenLattice") ) {
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
	
	/**
	 * Enable editing annotations in either 3D or 2D windows with the mouse.
	 * @param automaticLabels if true the labels are created with numbers only (no leading 'A' for annotation).
	 */
	public void editAnnotations( boolean automaticLabels )
	{
		mouse3D = false;
		mouseSelection3D = true;
		doAnnotations = true;
		doAutomaticLabels = automaticLabels;
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
		mouse3D = false;
		mouseSelection3D = true;
		if ( latticeModel != null )
		{
			latticeModel.clearAddLeftRightMarkers();
		}
		doAnnotations = false;
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
	
	public void setLattice( VOIVector lattice )
	{
		VOI newLattice = lattice.size() > 0 ? lattice.elementAt(0).getCurves() != null ? lattice.elementAt(0) : null : null;
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
		latticeModel.setLattice( newLattice );	
//		latticeModel.expandLattice();
	}
	
	public void untwistTest()
	{
		if ( latticeModel == null ) return;
		latticeModel.untwistTest();
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

	public void add3DMarker( VOI textVOI, boolean automaticLabel )
	{
		if ( doAnnotations )
		{
			textVOI.setActive(automaticLabel);
			if ( !automaticLabel )
			{
				textVOI.setActive(false);
				new JDialogAnnotation(m_kImageA, textVOI, 0, true, true);
				if ( !textVOI.isActive() )
				{
					return;
				}
			}
			addAnnotation(textVOI);
		}
		else
		{
			addLeftRightMarker(textVOI);
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

	public boolean is3DMouseEnabled()
	{
		return mouse3D;
	}
	public boolean is3DSelectionEnabled()
	{
		return mouseSelection3D;
	}

	public boolean modify3DMarker( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean rightMouse )
	{
		if ( doAnnotations )
		{
			return modifyAnnotations(startPt, endPt, pt, rightMouse);
		}
		return modifyLattice(startPt, endPt, pt);
	}
	
	public void deleteSelectedPoint()
	{
		if ( latticeModel != null )
		{
			if ( latticeModel.getPicked() != null )
			{
				saveVOIs("deleteSelectedPoint");
				latticeModel.deleteSelectedPoint(doAnnotations);
			}
		}		
	}
	
	public Vector3f getSelectedPoint()
	{
		if ( latticeModel != null )
		{
			return latticeModel.getPicked();
		}		
		return null;
	}
	
	public VOIText getPickedAnnotation() {
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
	
//	public void saveAnnotations( final String voiDir ) {
//		if ( latticeModel != null )
//		{
//			latticeModel.saveAnnotations(voiDir);
//		}
//	}
	
	public void showModel( boolean display ) {
		if ( latticeModel != null )
		{
			latticeModel.showModel(display);
		}		
	}
	
	/**
	 * Add an annotation to the latticeModel.
	 * @param textVOI new annotation.
	 */
	public void addAnnotation( VOI textVOI )
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
				saveVOIs("moveAnnotation");
			}
			latticeModel.setPicked( textVOI.getCurves().elementAt(0).elementAt(0), true );
		}
		Vector3f pt = latticeModel.getPicked( textVOI.getCurves().elementAt(0).elementAt(0), true );
		if ( pt == null )
		{
			saveVOIs("addAnnotation");
			latticeModel.addAnnotation(textVOI);
		}
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

	private void addLeftRightMarker( VOI textVOI )
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
			latticeModel.setPicked( textVOI.getCurves().elementAt(0).elementAt(0), false );
		}
		else
		{
			Vector3f pt = latticeModel.getPicked( textVOI.getCurves().elementAt(0).elementAt(0), false );
			if ( pt == null )
			{
				saveVOIs("addLeftRightMarker");
				latticeModel.addLeftRightMarker( textVOI.getCurves().elementAt(0).elementAt(0) );
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

	private boolean modifyAnnotations( Vector3f startPt, Vector3f endPt, Vector3f pt, boolean rightMouse )
	{
		if ( latticeModel != null )
		{
			if ( !movingPickedPoint )
			{
				movingPickedPoint = true;
				saveVOIs("modifyAnnotations");
			}
			return latticeModel.modifyAnnotation(startPt, endPt, pt, rightMouse);
		}
		return false;
	}

	public void updateAnnotation( VOIText annotation )
	{
		if ( latticeModel != null )
		{
			saveVOIs("updateAnnotation");
			latticeModel.updateAnnotation(annotation);
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
