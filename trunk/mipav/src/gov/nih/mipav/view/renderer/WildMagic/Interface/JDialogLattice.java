package gov.nih.mipav.view.renderer.WildMagic.Interface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.Surfaces.TubeSurface;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

public class JDialogLattice extends JDialogBase {

	private ModelImage image;    
	private JTextField[][] pairFields;
	private JPanel okCancelPanel;    
	private VOI lattice = null;
	private Vector<VOI> markerList = null;
	private int numPoints = 0;
	private int numPairs = 0;
	private Vector3f[] left_right_markers;
	private JButton saveButton;
	private VOIVector fullLattice = null;
	private VOIManagerInterface parent;
	
	public JDialogLattice( ModelImage image, VOIManagerInterface parent )
	{
		this.image = image;
		this.parent = parent;
		init();
		setVisible(true);
	}

    public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		Object source = event.getSource();

		if (command.equals("OK")) {
			setVariables();
			if ( fullLattice != null )
			{
				parent.setLattice(fullLattice);
			}
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if (command.equals("Cancel")) {
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if ( source == saveButton ) {
			saveLattice();
		} else {
			setVariables();
		}
    }
    
    private void saveLattice()
    {
        final JFileChooser chooser = new JFileChooser();

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        final int returnVal = chooser.showSaveDialog(null);

        String fileName = null, directory = null, voiDir;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }

        if (fileName != null) {
            voiDir = new String(directory + fileName + File.separator);   
            
            VOIVector backUpVOIs = image.getVOIsCopy();

			image.resetVOIs();
			image.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( lattice.getCurves().elementAt(0).elementAt(j) );
				mainAxis.add( lattice.getCurves().elementAt(1).elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
			
			saveAllVOIsTo( voiDir, image );    
            
			image.restoreVOIs(backUpVOIs);
        }

    }
    
    private void saveAllVOIsTo(final String voiDir, ModelImage image) {
        try {
            ViewVOIVector VOIs = image.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            	String[] list = voiFileDir.list();
            	for ( int i = 0; i < list.length; i++ )
            	{
            		File lrFile = new File( voiDir + list[i] );
            		lrFile.delete();
            	}
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                	FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
                	fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                }
                else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(),true);             	
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()
    
    
    private void setVariables()
    {
    	int count = 0;
    	int[][] pairs = new int[numPairs][2];
    	for ( int i = 0; i < numPairs; i++ )
    	{
    		pairs[i][0] = -1;
    		pairs[i][1] = -1;
    		if ( (pairFields[i][0].getText().length() > 0) && (pairFields[i][1].getText().length() > 0) )
    		{
    			if ( testParameter( pairFields[i][0].getText(), 0, numPoints ) );
    			{
    				pairs[i][0] = Integer.valueOf(pairFields[i][0].getText()).intValue();
    				count++;
    			}
    			if ( testParameter( pairFields[i][1].getText(), 0, numPoints ) );
    			{
    				pairs[i][1] = Integer.valueOf(pairFields[i][1].getText()).intValue();
    				count++;
    			}
    		}
    	}    	
    	if ( count > 0 )
    	{   		
    		if ( fullLattice == null )
    		{
    			fullLattice = new VOIVector();
    		}
    		else
    		{
    			fullLattice.clear();
    		}
    		if ( lattice == null )
    		{
    			short id = (short) image.getVOIs().getUniqueID();
    			lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
    			VOIContour leftSide = new VOIContour( false );
    			VOIContour rightSide = new VOIContour( false );
    			lattice.getCurves().add(leftSide);		
    			lattice.getCurves().add(rightSide);
    		}
    		else
    		{
    			image.unregisterVOI(lattice);
    		}
    		VOIContour leftSide = (VOIContour) lattice.getCurves().elementAt(0);
    		VOIContour rightSide = (VOIContour) lattice.getCurves().elementAt(1);
    		leftSide.clear();
    		rightSide.clear();
    		for ( int i = 0; i < numPairs; i++ )
    		{
    			if ( (pairs[i][0] == -1) || (pairs[i][1] == -1) )
    			{
    				break;
    			}
    			
    			leftSide.add( left_right_markers[ pairs[i][0] ] );
    			rightSide.add( left_right_markers[ pairs[i][1] ] );
    		}
    		
    		if ( leftSide.size() > 0 )
    		{
    			image.registerVOI(lattice);
    			fullLattice.add(lattice);
    			
    			lattice.setColor( new Color( 0, 0, 255) );
    			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
    			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
    			lattice.getCurves().elementAt(0).setClosed(false);
    			lattice.getCurves().elementAt(1).setClosed(false);
    			
    			if ( markerList == null )
    			{
    				markerList = new Vector<VOI>();
    			}
    			else
    			{
    				for ( int j = 0; j < markerList.size(); j++ )
    				{
    					image.unregisterVOI(markerList.elementAt(j) );
    				}
    				markerList.clear();
    			}
    			for ( int j = 0; j < leftSide.size(); j++ )
    			{
    				short id = (short) image.getVOIs().getUniqueID();
    				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
    				VOIContour mainAxis = new VOIContour(false); 		    		    		
    				mainAxis.add( leftSide.elementAt(j) );
    				mainAxis.add( rightSide.elementAt(j) );
    				marker.getCurves().add(mainAxis);
    				marker.setColor( new Color( 255, 255, 0) );
    				mainAxis.update( new ColorRGBA(1,1,0,1));
    				if ( j == 0 )
    				{
    					marker.setColor( new Color( 0, 255, 0) );
    					mainAxis.update( new ColorRGBA(0,1,0,1));
    				}
    				image.registerVOI( marker );
    				markerList.add(marker);
    			}
    		}
    	}
    }
	
	private void init( )
	{
		setResizable(true);
		setForeground(Color.black);
		setTitle("Build 3D Lattice");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		GuiBuilder gui = new GuiBuilder(this);
		
		numPoints = 0;
		VOIVector markers = image.getVOIs();
		for ( int i = 0; i < markers.size(); i++ )
		{
			VOI currentVOI = markers.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				numPoints++;
			}
		}
		if ( numPoints == 0 )
		{
			return;
		}
		left_right_markers = new Vector3f[numPoints];
		for ( int i = 0; i < markers.size(); i++ )
		{
			VOI currentVOI = markers.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				VOIText textVOI = (VOIText) currentVOI.getCurves().elementAt(0);
	    		Vector3f pt = textVOI.elementAt(0);
	    		String markerID = textVOI.getText();
	    		int id = Integer.valueOf( markerID.substring( 3, markerID.length() ) );				
				
//				System.err.println( markerID + " " + id );
				if ( (id >= 0) && (id < numPoints) )
				{
					left_right_markers[ id ] = pt;
				}
			}
		}
		
		numPairs = numPoints/2;

		JPanel panel = new JPanel(new GridLayout(numPairs, 3));
		panel.setForeground(Color.black);
		
		
		pairFields = new JTextField[numPairs][2];
		for ( int i = 0; i < numPairs; i++ )
		{
			panel.add( new JLabel( "Enter pair: " ) );
			pairFields[i][0] = new JTextField(5);
			pairFields[i][0].addActionListener(this);
			pairFields[i][0].addFocusListener(this);
			panel.add(pairFields[i][0]);
			pairFields[i][1] = new JTextField(5);
			pairFields[i][1].addFocusListener(this);
			panel.add(pairFields[i][1]);
		}
		saveButton = new JButton("Save Lattice");
		saveButton.addActionListener(this);
		
		getContentPane().add(panel, BorderLayout.NORTH);
		getContentPane().add( saveButton, BorderLayout.CENTER );
		okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

		pack();
		setResizable(true);

		System.gc();

	}
	
    public void focusGained(FocusEvent event) { }

    public void focusLost(FocusEvent event) 
    { 
    	setVariables();
    }
    
    
    
    

    
	public static TubeSurface interpolateLattice( ModelImage image, VOI lattice, boolean showModel )
	{
		// Assume image is isotropic (square voxels).
		if ( lattice.getCurves().size() != 2 )
		{
			return null;
		}
		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		if ( left.size() != right.size() )
		{
			return null;
		}
		VOIContour center = new VOIContour(false);
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
			centerPt.scale(0.5f);
			center.add(centerPt);
		}

    	float[] afTimeC = new float[center.size()];
		NaturalSpline3 centerSpline = smoothCurve(image, center, afTimeC);
		NaturalSpline3 leftSpline = smoothCurve2(image, left, afTimeC);
		NaturalSpline3 rightSpline = smoothCurve2(image, right, afTimeC);

		
		Vector<Vector3f> centerPositions = new Vector<Vector3f>();
		Vector<Vector3f> centerTangents = new Vector<Vector3f>();
		Vector<Float> wormDiameters = new Vector<Float>();
		Vector<Vector3f> rightVectors = new Vector<Vector3f>();
		Vector<Vector3f> upVectors = new Vector<Vector3f>();
		
		float length = centerSpline.GetLength(0, 1);
		int extent = 0;
		float[] allTimes = new float[(int) (Math.ceil(length))];
		float minCurve = Float.MAX_VALUE;
		float maxCurve = -Float.MAX_VALUE;
		for ( int i = 0; i < length; i++ )
		{
			float t = centerSpline.GetTime(i);
			allTimes[i] = t;
			centerPositions.add(centerSpline.GetPosition(t));
			centerTangents.add( centerSpline.GetFirstDerivative(t) );
			Vector3f leftPt = leftSpline.GetPosition(t);
			Vector3f rightPt = rightSpline.GetPosition(t);
			
			Vector3f rightDir = Vector3f.sub( rightPt, leftPt );		
			float diameter = rightDir.normalize();
			diameter /= 2f;
			diameter += 6;
			if ( diameter > extent )
			{
				extent = (int) Math.ceil(diameter);
			}			
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);
			
			centerTangents.elementAt(i).normalize();
			Vector3f upDir = Vector3f.cross( rightDir, centerTangents.elementAt(i) );
			upDir.normalize();
			upVectors.add(upDir);
//			if ( i > 0 )
//			{
//				System.err.println( i + "   " + centerPositions.elementAt(i).distance(centerPositions.elementAt(i-1)));
//			}
			float curve = centerSpline.GetSecondDerivative(t).length();
        	if ( curve < minCurve )
        	{
        		minCurve = curve;
        	}
        	if ( curve > maxCurve )
        	{
        		maxCurve = curve;
        	}

		}		
		extent += 10;

		int[] latticeSlice = new int[afTimeC.length];
		float[] closestTimes = new float[afTimeC.length];
		float[] leftDistances = new float[afTimeC.length];
		float[] rightDistances = new float[afTimeC.length];
		for ( int i = 0; i < afTimeC.length; i++ )
		{
			float minDif = Float.MAX_VALUE;
			for ( int j = 0; j < allTimes.length; j++ )
			{
				float dif = Math.abs(allTimes[j] - afTimeC[i]);
				if ( dif < minDif )
				{
					minDif = dif;
					latticeSlice[i] = j;
					closestTimes[i] = allTimes[j];
				}
			}
			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
			{
				float curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += centerSpline.GetPosition(allTimes[j]).distance( centerSpline.GetPosition(allTimes[j-1]) );
				}
//				System.err.println( i + "   " + curveDistance);
//				System.err.println( i + "   " + (latticeSlice[i] - latticeSlice[i-1]) );


				curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += leftSpline.GetPosition(allTimes[j]).distance( leftSpline.GetPosition(allTimes[j-1]) );
				}
				leftDistances[i] = curveDistance;

				curveDistance = 0;
				for ( int j = latticeSlice[i-1]+1; j <= latticeSlice[i]; j++ )
				{
					curveDistance += rightSpline.GetPosition(allTimes[j]).distance( rightSpline.GetPosition(allTimes[j-1]) );
				}
				rightDistances[i] = curveDistance;
			}
		}

		saveLatticeStatistics(image, length, left, right, leftDistances, rightDistances, "_before");
		
		Vector<Ellipsoid3f> ellipseBounds = new Vector<Ellipsoid3f>();
		float distance = 0;
		short sID = (short)(image.getVOIs().getUniqueID());
		VOI samplingPlanes = new VOI(sID, "samplingPlanes");
		VOI wormContours = new VOI(sID, "wormContours");
		for ( int i = 0; i < centerPositions.size(); i++ )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        Vector3f rkDVector = centerTangents.elementAt(i);
	        
			Vector3f[] output = new Vector3f[4];
	        Vector3f rightV = Vector3f.scale( extent, rkRVector );
	        Vector3f upV = Vector3f.scale( extent, rkUVector );
	        output[0] = Vector3f.add( Vector3f.neg(rightV), Vector3f.neg(upV) );
	        output[1] = Vector3f.add( rightV, Vector3f.neg(upV) );
	        output[2] = Vector3f.add( rightV, upV );
	        output[3] = Vector3f.add( Vector3f.neg(rightV), upV );
	        for ( int j = 0; j < 4; j++ )
	        {
	        	output[j].add(rkEye);
	        }
	        
	        if ( i > 0 )
	        {
	        	distance += centerPositions.elementAt(i).distance( centerPositions.elementAt(i-1) );
	        }
	        if ( (showModel && ((i%30) == 0)) || !showModel )
	        {
	        	float curve = centerSpline.GetSecondDerivative(allTimes[i]).length();
	        	float scale = (curve - minCurve)/(maxCurve - minCurve);
		        VOIContour ellipse = new VOIContour(true);
//		        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), averageDiameter, distance, length, ellipse );
		        Ellipsoid3f ellipsoid = makeEllipse( rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse );
		        ellipseBounds.add( ellipsoid );
	        	wormContours.importCurve(ellipse);
	        }
	        
			VOIContour kBox = new VOIContour(true);
			for ( int j = 0; j < 4; j++ )
			{
				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
			}
//			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(1) ) + " " + kBox.elementAt(2).distance( kBox.elementAt(3) ) );
//			System.err.println( kBox.elementAt(0).distance( kBox.elementAt(3) ) + " " + kBox.elementAt(1).distance( kBox.elementAt(2) ) );
			kBox.update( new ColorRGBA(0,0,1,1) );		
//	        if ( (i%40) == 0 )
	        {	
	        	samplingPlanes.importCurve(kBox);
	        }
		}
		VOIContour centerLine = new VOIContour(false);
		centerLine.addAll( centerPositions );
		sID = (short)(image.getVOIs().getUniqueID());
		VOI samplingPoints = new VOI(sID, "samplingPlanes");
		samplingPoints.getCurves().add(centerLine);
		image.registerVOI(samplingPoints);
		if ( showModel )
		{
			image.registerVOI(wormContours);
		}
		if ( !showModel )
		{
			straighten(image, samplingPlanes, ellipseBounds, 2*extent, latticeSlice, false, false );
		}
		
		return null; //createTube( image, center, extent);
	}

	public static Ellipsoid3f makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameterA, float averageDiameter, 
			float distance, float totalDistance, VOIContour ellipse  )
	{
		int numPts = 32;
		double[] adCos = new double[32];
		double[] adSin = new double[32];
		for ( int i = 0; i < numPts; i++ )
		{
			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
		}
		float diameterB = (averageDiameter*averageDiameter)/diameterA;
		if ( (distance < .25*totalDistance) || (distance > .75*totalDistance) )
		{
			diameterB = diameterA;
		}
		for ( int i = 0; i < numPts; i++ )
		{
			Vector3f pos1 = Vector3f.scale((float) (diameterA * adCos[i]), right);
			Vector3f pos2 = Vector3f.scale((float) (diameterB * adSin[i]), up);
			Vector3f pos = Vector3f.add(pos1,pos2);
			pos.add(center);
			ellipse.addElement( pos );
		}
		float[] extents = new float[]{diameterA, diameterB, diameterB };
		Vector3f[] axes = new Vector3f[]{right, up, Vector3f.cross(right,up) };
		return new Ellipsoid3f( center, axes, extents );
	}

	public static Ellipsoid3f makeEllipse( Vector3f right, Vector3f up, Vector3f center, float diameterA, float scale, VOIContour ellipse  )
	{
		int numPts = 32;
		double[] adCos = new double[32];
		double[] adSin = new double[32];
		for ( int i = 0; i < numPts; i++ )
		{
			adCos[i] = Math.cos( Math.PI * 2.0 * i/numPts );
			adSin[i] = Math.sin( Math.PI * 2.0 * i/numPts);
		}
		float diameterB = diameterA/2f + (1-scale) * diameterA/2f;
		for ( int i = 0; i < numPts; i++ )
		{
			Vector3f pos1 = Vector3f.scale((float) (diameterA * adCos[i]), right);
			Vector3f pos2 = Vector3f.scale((float) (diameterB * adSin[i]), up);
			Vector3f pos = Vector3f.add(pos1,pos2);
			pos.add(center);
			ellipse.addElement( pos );
		}
		float[] extents = new float[]{diameterA, diameterB, diameterB };
		Vector3f[] axes = new Vector3f[]{right, up, Vector3f.cross(right,up) };
		return new Ellipsoid3f( center, axes, extents );
	}
	
	public static NaturalSpline3 smoothCurve( ModelImage image, VOIContour curve, float[] time )
    {
    	float totalDistance = 0;
    	for ( int i = 0; i < curve.size()-1; i++ )
    	{
    		totalDistance += curve.elementAt(i).distance(curve.elementAt(i+1));
    	}
    	
    	Vector3f[] akPoints = new Vector3f[curve.size()];
    	float distance = 0;
    	for ( int i = 0; i < curve.size(); i++ )
    	{
    		if ( i > 0 )
    		{
    			distance += curve.elementAt(i).distance( curve.elementAt(i-1) );
    			time[i] = distance / totalDistance;
    			akPoints[i] = new Vector3f(curve.elementAt(i));
    		}
    		else
    		{    			
    			time[i] = 0;
    			akPoints[i] = new Vector3f(curve.elementAt(i));
    		}
    	}
    	
    	return new NaturalSpline3( NaturalSpline3.BoundaryType.BT_FREE, curve.size()-1, time, akPoints );
    }
	
	public static NaturalSpline3 smoothCurve2( ModelImage image, VOIContour curve, float[] time )
    {
    	Vector3f[] akPoints = new Vector3f[curve.size()];
    	for ( int i = 0; i < curve.size(); i++ )
    	{
    		akPoints[i] = new Vector3f(curve.elementAt(i));
    	}
    	
    	return new NaturalSpline3( NaturalSpline3.BoundaryType.BT_FREE, curve.size()-1, time, akPoints );
    }
    
    public static void straighten( ModelImage image, VOI samplingPlanes, Vector<Ellipsoid3f> ellipseBounds,
    		int diameter, int[] latticeSlice,
    		boolean fillData, boolean displayMask )
    {
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		int colorFactor = image.isColorImage() ? 4 : 1;
		int[] resultExtents = new int[]{diameter, diameter, samplingPlanes.getCurves().size()};
		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 

		BitSet duplicateMask = new BitSet( dimX * dimY * dimZ );

    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName.replaceAll("_clone", "" );
    	}
		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straigntened");
		JDialogBase.updateFileInfo( image, resultImage );
		resultImage.setResolutions( new float[]{1,1,1});
		Vector3f lpsOrigin = new Vector3f();
		for( int i = 0; i < samplingPlanes.getCurves().size(); i++ )
		{
//			float diameterInterp = samplingDiameters.elementAt(i);
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
	        Vector3f[] corners = new Vector3f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j] = kBox.elementAt(j);
	        }
			if ( fillData && (i > 0) )
			{
				System.arraycopy(values[i-1], 0, values[i], 0, values[i].length);
			}
			try {
				image.exportDiagonal( duplicateMask, 0, i, resultExtents, corners, ellipseBounds.elementAt(i), !fillData, values[i], true);

				if ( i == 0 )
				{
					MipavCoordinateSystems.fileToScanner( corners[0], lpsOrigin, image );
				}

				resultImage.importData(i*values[i].length, values[i], false);
			} catch(IOException e) {
				e.printStackTrace();
			}
		}

		if ( displayMask )
		{
			ModelImage duplicateMaskImage = new ModelImage( ModelStorageBase.BOOLEAN, image.getExtents(), imageName + "_mask" );
			try {
				duplicateMaskImage.importData( 0, duplicateMask, true );
				new ViewJFrameImage(duplicateMaskImage);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		

		float[] leftDistances = new float[latticeSlice.length];
		float[] rightDistances = new float[latticeSlice.length];
		short id = (short) image.getVOIs().getUniqueID();
		VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
		VOIContour leftSide = new VOIContour( false );
		VOIContour rightSide = new VOIContour( false );
		lattice.getCurves().add(leftSide);		
		lattice.getCurves().add(rightSide);
		Vector3f dir = new Vector3f(1,0,0);
		for ( int i = 0; i < latticeSlice.length; i++ )
		{
			Ellipsoid3f ellipsoid = ellipseBounds.elementAt( latticeSlice[i] );
//			Vector3f center = ellipsoid.Center;
			float width = ellipsoid.Extent[0] - 6;
//			Vector3f dir = ellipsoid.Axis[0];
			Vector3f center = new Vector3f(diameter/2,diameter/2,latticeSlice[i]);
			
			Vector3f leftPt = Vector3f.scale( -width, dir ); leftPt.add(center);
			leftSide.add(leftPt);
			
			Vector3f rightPt = Vector3f.scale(  width, dir ); rightPt.add(center);
			rightSide.add(rightPt);

			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if ( i > 0 )
			{
				leftDistances[i] = leftSide.elementAt(i).distance(leftSide.elementAt(i-1) );
				rightDistances[i] = rightSide.elementAt(i).distance(rightSide.elementAt(i-1) );
			}
		}

		resultImage.registerVOI(lattice);

		lattice.setColor( new Color( 0, 0, 255) );
		lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
		lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
		lattice.getCurves().elementAt(0).setClosed(false);
		lattice.getCurves().elementAt(1).setClosed(false);
		for ( int j = 0; j < leftSide.size(); j++ )
		{
			id = (short) image.getVOIs().getUniqueID();
			VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
			VOIContour mainAxis = new VOIContour(false); 		    		    		
			mainAxis.add( leftSide.elementAt(j) );
			mainAxis.add( rightSide.elementAt(j) );
			marker.getCurves().add(mainAxis);
			marker.setColor( new Color( 255, 255, 0) );
			mainAxis.update( new ColorRGBA(1,1,0,1));
			if ( j == 0 )
			{
				marker.setColor( new Color( 0, 255, 0) );
				mainAxis.update( new ColorRGBA(0,1,0,1));
			}
			resultImage.registerVOI( marker );
		}

		saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");
		
		
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);  	
    }
    
    
	/**
	 * Generate the tube streamline from the given polyline.
	 * @param kTract  polyline of the medial path.
	 * @return  kTube Tube surface generated. 
	 */
	public static TubeSurface createTube( ModelImage image, VOIContour kTract, float diameter ) {
		scale(image, kTract);

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		diameter /= Math.max( dimX, Math.max(dimY, dimZ));
		
		TubeSurface kTube;
		int iNumCtrlPoints = kTract.size();
		Vector3f[] akCtrlPoint = new Vector3f[iNumCtrlPoints];
		for ( int i = 0; i < iNumCtrlPoints; i++ ) {
			akCtrlPoint[i] = kTract.elementAt(i);
		}

		int iDegree = 2;
		BSplineCurve3f m_pkSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
				false,true);

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0,3);
		kAttr.SetCChannels(0,4);

		Vector2f kUVMin = new Vector2f(0.0f,0.0f);
		Vector2f kUVMax = new Vector2f(1.0f,1.0f);
		kTube = new TubeSurface(m_pkSpline, diameter, false,Vector3f.UNIT_Z,
				iNumCtrlPoints,8,kAttr,true,false,kUVMin,kUVMax);
//		kTube.Local.SetTranslate( m_kTranslate );
		kTube.SetName( "WormSurface" );
		return kTube;
	}
	
    public static void scale( ModelImage image, VOIContour kTract )
    {
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		
		final float fMaxX = (dimX - 1) * image.getFileInfo(0).getResolutions()[0];
		final float fMaxY = (dimY - 1) * image.getFileInfo(0).getResolutions()[1];
		final float fMaxZ = (dimZ - 1) * image.getFileInfo(0).getResolutions()[2];

		float m_fMax = fMaxX;
		if (fMaxY > m_fMax) {
			m_fMax = fMaxY;
		}
		if (fMaxZ > m_fMax) {
			m_fMax = fMaxZ;
		}
		float m_fX = fMaxX / m_fMax;
		float m_fY = fMaxY / m_fMax;
		float m_fZ = fMaxZ / m_fMax;
    	
        Vector3f kVolumeScale = new Vector3f(m_fX, m_fY, m_fZ);
        Vector3f kExtentsScale = new Vector3f(1f/(dimX - 1), 
                1f/(dimY - 1), 
                1f/(dimZ - 1)  );

        for ( int i = 0; i < kTract.size(); i++ )
        {
            Vector3f kPos = kTract.elementAt(i);
            kPos.mult(kExtentsScale);
            kPos.mult(kVolumeScale);
        }
    }
    
    public static void saveLatticeStatistics( ModelImage image, float length, VOIContour left, VOIContour right, 
    		float[] leftPairs, float[] rightPairs, String postFix )
    {
    	String imageName = image.getImageName();
    	if ( imageName.contains("_clone") )
    	{
    		imageName = imageName.replaceAll("_clone", "" );
    	}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator;
        File voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName( imageName, "") + File.separator +
    			"statistics" + File.separator;
        voiFileDir = new File(voiDir);
        if (voiFileDir.exists() && voiFileDir.isDirectory()) {
//        	String[] list = voiFileDir.list();
//        	for ( int i = 0; i < list.length; i++ )
//        	{
//        		File lrFile = new File( voiDir + list[i] );
//        		lrFile.delete();
//        	}
        } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
        } else { // voiFileDir does not exist
            voiFileDir.mkdir();
        }

        File file = new File(voiDir + "LatticeInfo" + postFix + ".txt");
        if ( file.exists() )
        {
        	file.delete();
        	file = new File(voiDir + "LatticeInfo" + postFix + ".txt");
        }


        try {

        	FileWriter fw = new FileWriter(file);
        	BufferedWriter bw = new BufferedWriter(fw);
        	bw.write( "Total Curve Length:" + "\t" + length + "\n" );
            bw.newLine();
//        	bw.write( "Diameter" + "\t" + "Left" + "\t"  + "Right" + "\t" + "\n" );
        	for ( int i = 0; i < leftPairs.length; i++ )
        	{
        		bw.write(i + "\t" + left.elementAt(i).distance(right.elementAt(i)) + "\t" + leftPairs[i] + "\t" + rightPairs[i] + "\n");
        				
        	}
//        	for ( int i = 0; i < leftPairs.length; i++ )
//        	{
//        		bw.write( "Left" + i + "->" + "Right" + i + "\t" + left.elementAt(i).distance(right.elementAt(i)) + "\n" );
//        	}
//            bw.newLine();
//        	for ( int i = 1; i < leftPairs.length; i++ )
//        	{
//        		bw.write( "Left" + (i-1) + "->" + "Left" + i + "\t" + leftPairs[i] + "\n" );
//        	}
//            bw.newLine();
//        	for ( int i = 1; i < leftPairs.length; i++ )
//        	{
//        		bw.write( "Right" + (i-1) + "->" + "Right" + i + "\t" + rightPairs[i] + "\n" );
//        	}
            bw.newLine();
        	bw.close();
        } catch (final Exception e) {
        	System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
        	e.printStackTrace();
        }
    }

}
